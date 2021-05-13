
#########################################################################################
#################### Results
#########################################################################################

print("Assign results and make winner and loser names")
# Include implict opinion
# df_scores7 <- AssignResults(df_scores6) 
# Exclude implict opinion
# df_scores7 <- AssignResults(df_scores6 %>% filter(Disputant_1 != "Implicit opinion", Disputant_2 != "Implicit opinion")) 
# Filter out higher number columns
# Trim down the columns to exclude the no longer needed _3, _4, _5 stuff
if (YesCullCullNonWinners == TRUE) {
  df_scores6b = CullNonWinners(df_scores6)   
} else {
  df_scores6b = df_scores6
}

df_scores7 <- AssignResults(
  df_scores6b %>% 
    filter(Disputant_1 != "Implicit opinion", Disputant_2 != "Implicit opinion") 
    ) %>% 
    select(DisputeID, SeqID, tabnum, Location, Disputant_1, Disputant_2, Winner, Result_1, Result_2,  Score_1, Score_2,
      #Strict_1, Strict_2, # No longer use these because they are inferior to the alt method which allows many different values
      Money_1, Money_2, AltStrict_1, AltStrict_2,
      Strict_Result_1, Strict_Result_2, Strict_Result_Score_1, Strict_Result_Score_2,
      Chapter, Chapter_Name_Hebrew, ReverseExcelTabName, ExcelTabName, ChapterRowKey, HebrewChapterLocation, 
      Ith_chapter_in_seder, PsuedoDate, EffectiveRows, tab, 
      Disputant_Hebrew_D1, Disputant_Hebrew_D1b, Disputant_Hebrew_D1c, Disputant_Hebrew_D2, Disputant_Hebrew_D2b, 
      Disputant_Hebrew_D2c, Is_Hebrew_Disputant_Name_In_Mishna_Text_1, Is_Hebrew_Disputant_Name_In_Mishna_Text_1_any, 
      Is_Hebrew_Disputant_Name_In_Mishna_Text_1b, Is_Hebrew_Disputant_Name_In_Mishna_Text_1c, 
      Is_Hebrew_Disputant_Name_In_Mishna_Text_2, Is_Hebrew_Disputant_Name_In_Mishna_Text_2_any, 
      Is_Hebrew_Disputant_Name_In_Mishna_Text_2b, Is_Hebrew_Disputant_Name_In_Mishna_Text_2c, FileSource, MishnahKey, 
      MishnaText, IsCopy, IsNaText, keep,  Money_Sum, MoneyQuestion, NumDisp, Owe_1, Owe_2, 
      Owe_Sum, OweQuestion,  Seder_Hebrew, Seder_Name, Seder_Number, Seder_Translate, 
      Strict_Sum, StrictQuestion, AltStrictQuestion, Loser, LoserName, WinnerName, BlankWinner,
      ChumraName, KulaName, BHBhighName, BHBlowName, MoneyName, ChayavhighName, ChayavlowName, OweName,
      Tie.Win, Tie.Strict, Tie.Money, Tie.Owe
)
df_scores7_strict <- df_scores7  %>% filter(AltStrictQuestion==1)
df_scores7_money  <- df_scores7  %>% filter(MoneyQuestion==1)
df_scores7_owe    <- df_scores7  %>% filter(OweQuestion==1)

#########################################################################################
#####################################Check if data are in good shape for David's Score DS 
#########################################################################################
print("Check data for issues before using David's score data ")
tmp8 <- metaseqcheck(df_scores7, df_scores7_strict, df_scores7_money, df_scores7_owe)
df_scores8          <- tmp8$df_output_win
df_scores8_strict   <- tmp8$df_output_chumra
df_scores8_money    <- tmp8$df_output_money 
df_scores8_owe      <- tmp8$df_output_owe 


#########################################################################################
#####################################Make cross-validated datasets
#########################################################################################

print("Make cross-validated datasets")
numberfolds = 5
df_scores9        <- AssignKfold(df_scores8, numberfolds)
df_scores9_strict <- AssignKfold(df_scores8_strict, numberfolds)
df_scores9_money  <- AssignKfold(df_scores8_money, numberfolds)
df_scores9_owe    <- AssignKfold(df_scores8_owe, numberfolds)


# Give me lots of room for additional data processing later 
df_scores100        <- df_scores9
df_scores100_strict <- df_scores9_strict
df_scores100_money  <- df_scores9_money
df_scores100_owe    <- df_scores9_owe

##################################################################################################################
################################### Estimate ELO assuming order in data is order in Elo
##################################################################################################################
# Calculate the Elo scores for the regular disputes and also who is chumra 
# elo.run(score(points.Home, points.Visit or) ~ team.Home + team.Visitor,data = tournament, k = 20)
# The maximum possible adjustment per game, called the K-factor, was set at K = 16 for masters and K = 32 for weaker players. 
# Initial score is 1500 (observed, documentation is unclear and controlled by initial.elos=1500)
# Also calculate the David's Score, which is order free

print("Calculate Elo scores (Win, Chumra, Money, and Owe) in Talmudic order")
# elo_1 <- elo.run(score(Score_1, Score_2) ~ Disputant_1 + Disputant_2, data = df_scores100, k = 32)
# # Display Scores
# final.elos(elo_1)
DefaultElo = 1500
elo_1b <- elo.seq(df_scores100$WinnerName, df_scores100$LoserName, df_scores100$PsuedoDate, df_scores100$Tie.Win, startvalue=DefaultElo, k=32, runcheck=FALSE)
extract_elo(elo_1b)
dom_mat_wins <- creatematrix(elo_1b)
# By default corrected for number of interactions in a dyad (prop="Dij"), otherwise the raw proportion (prop="Pij")
#ds_1b <- DS(dom_mat_wins, prop = "Dij") #calculate proportions corrected for chance 
ds_1b <- DS(dom_mat_wins, prop = "Pij") #raw proportions
colnames(ds_1b) <- c("Name", "DS.win", "normDS.win")
# Tmp1 = as.data.frame(extract_elo(elo_1b))
# Tmp2 = as.data.frame(final.elos(elo_1))
# Tmp1$Name = row.names(Tmp1)
# Tmp2$Name = row.names(Tmp2)
# Tmp3 = left_join(Tmp1, Tmp2, by="Name")
# colnames(Tmp3) <- c("Alternate Win Elo Score", "Name", "Current Win Elo Score")

# cor(Tmp3$"Alternate Win Elo Score", Tmp3$"Current Win Elo Score")
# cor.test(Tmp3$"Alternate Win Elo Score", Tmp3$"Current Win Elo Score", method = "spearman")


#elo_2 <- elo.run(score(Strict_1, Strict_2) ~ Disputant_1 + Disputant_2, data = df_scores100 %>% filter(StrictQuestion==1), k = 32)
#final.elos(elo_2)
# elo_2b <- elo.run(score(AltStrict_1, AltStrict_2) ~ Disputant_1 + Disputant_2, data = df_scores100 %>% filter(AltStrictQuestion==1), k = 32)
# # Display Scores
# final.elos(elo_2b)

elo_2c <- elo.seq(df_scores100_strict$ChumraName, df_scores100_strict$KulaName, df_scores100_strict$PsuedoDate, df_scores100_strict$Tie.Strict, startvalue=DefaultElo, k=32, runcheck=FALSE)
extract_elo(elo_2c)
dom_mat_strict <- creatematrix(elo_2c)

# By default corrected for number of interactions in a dyad (prop="Dij"), otherwise the raw proportion (prop="Pij")
# ds_2c <- DS(dom_mat_strict, prop = "Dij")
ds_2c <- DS(dom_mat_strict, prop = "Pij")
colnames(ds_2c) <- c("Name", "DS.strict", "normDS.strict")

# elo_3 <- elo.run(score(Money_1, Money_2) ~ Disputant_1 + Disputant_2, data = df_scores100 %>% filter(MoneyQuestion==1), k = 32)
# # Display Scores
# final.elos(elo_3)
elo_3b <- elo.seq(df_scores100_money$BHBhighName, df_scores100_money$BHBlowName, df_scores100_money$PsuedoDate, df_scores100_money$Tie.Money, startvalue=DefaultElo, k=32, runcheck=FALSE)

dom_mat_money <- creatematrix(elo_3b)
# By default corrected for number of interactions in a dyad (prop="Dij"), otherwise the raw proportion (prop="Pij")
# ds_3b <- DS(dom_mat_money, prop = "Dij")
ds_3b <- DS(dom_mat_money, prop = "Pij")
colnames(ds_3b) <- c("Name", "DS.money", "normDS.money")


# elo_4 <- elo.run(score(Owe_1, Owe_2) ~ Disputant_1 + Disputant_2, data = df_scores100 %>% filter(OweQuestion==1), k = 32)
# final.elos(elo_4)
elo_4b <- elo.seq(df_scores100_owe$ChayavhighName, df_scores100_owe$ChayavlowName, df_scores100_owe$PsuedoDate, df_scores100_owe$Tie.Owe, startvalue=DefaultElo, k=32, runcheck=FALSE)
dom_mat_owe <- creatematrix(elo_4b)
# ds_4b <- DS(dom_mat_owe, prop = "Dij")
ds_4b <- DS(dom_mat_owe, prop = "Pij")
colnames(ds_4b) <- c("Name", "DS.owe", "normDS.owe")

# Matrix by arguments
dom_mat_disputes <- as.data.frame(dom_mat_wins + t(dom_mat_wins))
dom_mat_disputes$TotalDisputes <- rowSums(dom_mat_disputes)
dom_mat_disputes$Name = row.names(dom_mat_disputes)

# Matrix by wins
dom_mat_wins2 <- as.data.frame(dom_mat_wins)
dom_mat_wins2$TotalDisputes <- rowSums(dom_mat_wins2)
dom_mat_wins2$Name = row.names(dom_mat_wins2)

# Matrix by strictness wins 
dom_mat_strict2 = as.data.frame(dom_mat_strict)
dom_mat_strict2$TotalDisputes <- rowSums(dom_mat_strict2)
dom_mat_strict2$Name = row.names(dom_mat_strict2)


# Matrix by strictness disputes 
dom_mat_strictdispute2 = as.data.frame(dom_mat_strict + t(dom_mat_strict))
dom_mat_strictdispute2$TotalDisputes <- rowSums(dom_mat_strictdispute2)
dom_mat_strictdispute2$Name = row.names(dom_mat_strictdispute2)

##################################################################################################################
################################### Make the Elo results into a nice data frame
##################################################################################################################

print("Combine the results with other estimates into a single results dataframe")
# This as.data.frame does the same as do.call. Is easier to read but required as column renaming step
# df_elo_1b = as.data.frame(extract_elo(elo_1b))
# df_elo_2c = as.data.frame(extract_elo(elo_2c))
# df_elo_3b = as.data.frame(extract_elo(elo_3b))
# df_elo_4b = as.data.frame(extract_elo(elo_4b))
# df_elo_1b$Name = row.names(df_elo_1b)
# df_elo_2c$Name = row.names(df_elo_2c)
# df_elo_4b$Name = row.names(df_elo_4b)
# df_elo_3b$Name = row.names(df_elo_3b)

# Old code
# df_results_1 <- do.call(rbind, Map(data.frame, EloScoreWins=final.elos(elo_1)))
# df_results_2b <- do.call(rbind, Map(data.frame, EloScoreChumra=final.elos(elo_2b)))
# df_results_3 <- do.call(rbind, Map(data.frame, EloScoreMoney=final.elos(elo_3)))
# df_results_4 <- do.call(rbind, Map(data.frame, EloScoreOwe=final.elos(elo_4)))


df_results_1 <- do.call(rbind, Map(data.frame,  EloScoreWins    = extract_elo(elo_1b)))
df_results_2c <- do.call(rbind, Map(data.frame, EloScoreChumra  = extract_elo(elo_2c)))
df_results_3 <- do.call(rbind, Map(data.frame,  EloScoreMoney   = extract_elo(elo_3b)))
df_results_4 <- do.call(rbind, Map(data.frame,  EloScoreOwe     = extract_elo(elo_4b)))

df_results_1$Name   <- row.names(df_results_1)
df_results_2c$Name  <- row.names(df_results_2c)
df_results_3$Name   <- row.names(df_results_3)
df_results_4$Name   <- row.names(df_results_4)

df_results_5 <- merge(x=df_results_1, y=df_results_2c, by="Name", all=TRUE)
df_results_5$EloScoreChumra[is.na(df_results_5$EloScoreChumra)] <- DefaultElo

df_counts = stack(df_scores100 %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()
# df_chumra_counts = stack(df_scores8 %>% filter(Result_1=="Chumra" | Result_1=="Kula") %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()
df_chumra_counts = stack(df_scores100 %>% filter(StrictQuestion==1) %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()


#df_money_counts  = stack(df_scores100 %>% filter(Result_1=="BHB 1" | Result_1=="BHB 0" | Result_1=="BHB 0.5") %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()
df_money_counts  = stack(df_scores100 %>% filter(MoneyQuestion==1) %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()

df_owe_counts  = stack(df_scores100 %>% filter(OweQuestion==1) %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()

# This doesn't work right, because it only counts the full name appearance, and in a split setting it won't match properly
#df_counts_without_splitting = stack(df_scores_bak3 %>% select(Disputant_1, Disputant_2, Disputant_3, Disputant_4, Disputant_5)) %>% group_by(values) %>% tally()
#df_counts_without_splitting$n[is.na(df_counts_without_splitting$values)] <- 0
# Better way is to count the number of unique SeqId where this appears
df_results_5$UniqueQuestions <- df_scores100 %>% 
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(mycount=length(unique(SeqID))) %>% 
  pull()

df_results_5 <- left_join(df_results_5, df_scores100 %>% 
  filter(MoneyQuestion==1) %>%   
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(UniqueMoneyQuestions=length(unique(SeqID))),
  by="Name" 
  ) %>% mutate(UniqueMoneyQuestions = replace_na(UniqueMoneyQuestions, 0))

df_results_5 <- left_join(df_results_5, df_scores100 %>% 
  filter(OweQuestion==1) %>%   
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(UniqueOweQuestions=length(unique(SeqID))),
  by="Name" 
  ) %>% mutate(UniqueOweQuestions = replace_na(UniqueOweQuestions, 0))


df_results_5 <- left_join(df_results_5, df_scores100 %>% 
  filter(StrictQuestion==1) %>%   
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(UniqueChumraQuestions=length(unique(SeqID))),
  by="Name" 
  ) %>% mutate(UniqueChumraQuestions = replace_na(UniqueChumraQuestions, 0))

df_WinRate_00 = WinRate(df_scores100)
df_WinRate_01 = df_WinRate_00 %>% select(
  Disputant, 
  CountMatches.Win, CountMatches.Strict, CountMatches.Money, CountMatches.Owe, #CountMatches.Win Note N is missing, already in the df
  Sum.Wins.Win, Sum.Wins.Strict, Sum.Wins.Money, Sum.Wins.Owe,
  Sum.Ties.Win, Sum.Ties.Strict, Sum.Ties.Money, Sum.Ties.Owe,
  Sum.Loss.Win, Sum.Loss.Strict, Sum.Loss.Money, Sum.Loss.Owe,
  WinRate, WinChumraRate, WinMoneyRate, WinOweRate, WinChumraExMonRate,
  WinRate_ignore_ties, WinChumraRate_ignore_ties, WinMoneyRate_ignore_ties, WinOweRate_ignore_ties, WinChumraExMonRate_ignore_ties 
  )

colnames(df_WinRate_01) = c(
  "Name",
  "n.disputes", "n.chumra", "n.money", "n.owe", # Note N is missing, already in the df
  "n.wins", "n.wins.chumra", "n.wins.money", "n.wins.owe",
  "n.ties", "n.ties.chumra", "n.ties.money", "n.ties.owe",
  "n.loss", "n.loss.chumra", "n.loss.money", "n.loss.owe",  
  "WinRate", "WinChumraRate", "WinMoneyRate", "WinOweRate", "WinChumraExMonRate",
  "WinRate_ignore_ties", "WinChumraRate_ignore_ties", "WinMoneyRate_ignore_ties", "WinOweRate_ignore_ties", "WinChumraExMonRate_ignore_ties" 
)



df_results_6 = df_results_5
#df_results_6 <- merge(x=df_results_5, y=df_counts, by.x="Name", by.y="values", all=TRUE)

# Elo Money
df_results_7 <- merge(x=df_results_6, y=df_results_3, by="Name", all=TRUE)
df_results_7$EloScoreMoney[is.na(df_results_7$EloScoreMoney)] <- DefaultElo

# Elo Owe
df_results_7 <- merge(x=df_results_7, y=df_results_4, by="Name", all=TRUE)
df_results_7$EloScoreOwe[is.na(df_results_7$EloScoreOwe)] <- DefaultElo



df_results_9 = left_join(df_results_7, df_WinRate_01, by="Name")

Big05List =  (df_results_9 %>% top_n(5,  n.disputes) %>% select(Name))[,1]
Big10List =  (df_results_9 %>% top_n(10, n.disputes) %>% select(Name))[,1]
Big15List =  (df_results_9 %>% top_n(15, n.disputes) %>% select(Name))[,1]
Big20List =  (df_results_9 %>% top_n(20, n.disputes) %>% select(Name))[,1]
Big25List =  (df_results_9 %>% top_n(25, n.disputes) %>% select(Name))[,1]


FieldList25 = Big25List
FieldList25[26] <- "TotalDisputesAmongBig25"
FieldList25[27] <- "TotalDisputes"
FieldList25[28] <- "Name"
# Reverse order
FieldList25 <- rev(FieldList25)

dom_mat_disputes$TotalDisputesAmongBig25 <- dom_mat_disputes %>% select(Big25List) %>% rowSums()
# dom_mat_disputes2 <- dom_mat_disputes %>% select(all_of(FieldList25))
dom_mat_disputes2 <- dom_mat_disputes %>% select(any_of(FieldList25))
# Wins or Losses (no ties, undirected)
dom_mat_disputes3 <- dom_mat_disputes2 %>% filter(row.names(.) %in%  Big25List)


dom_mat_disputes3 <- dom_mat_disputes2 %>% filter(row.names(.) %in%  Big25List)


# Wins only (directed)
dom_mat_wins2$TotalDisputesAmongBig25 <- dom_mat_wins2 %>% select(Big25List) %>% rowSums() 
dom_mat_wins3 <- dom_mat_wins2 %>% select(any_of(FieldList25))
dom_mat_wins4 <- dom_mat_wins3 %>% filter(row.names(.) %in%  Big25List)

dom_mat_strict3 <- dom_mat_strict2 %>% select(any_of(FieldList25))  %>% filter(row.names(.) %in%  Big25List)
dom_mat_strict4 <- dom_mat_strict3 %>% filter(row.names(.) %in%  Big25List)



df_results_9$n.chumra.exmon = df_results_9$n.chumra - df_results_9$n.money
df_results_9$n.wins.chumra.exmon = df_results_9$n.wins.chumra - df_results_9$n.loss.money


df_results_9$Top10Rabbi = df_results_9$Name %in% Big10List
df_results_9$Top15Rabbi = df_results_9$Name %in% Big15List
df_results_9$Top20Rabbi = df_results_9$Name %in% Big20List
df_results_9$Top25Rabbi = df_results_9$Name %in% Big25List

df_results_10 = left_join(df_results_9 , ds_1b, by="Name")
df_results_10 = left_join(df_results_10, ds_2c, by="Name")
df_results_10 = left_join(df_results_10, ds_3b, by="Name")
df_results_10 = left_join(df_results_10, ds_4b, by="Name")

df_results_10$DS.owe[is.na(df_results_10$DS.owe)]       <- 0.0
df_results_10$DS.strict[is.na(df_results_10$DS.strict)] <- 0.0
df_results_10$DS.money[is.na(df_results_10$DS.money)]   <- 0.0
df_results_10$DS.owe[is.na(df_results_10$DS.owe)]       <- 0.0

df_results_100 <- df_results_10

# Make a ten by ten of the rabbis in the most debates
df_scores100_topten = df_scores100 %>% 
  filter(
    (LoserName  %in% Big10List) &  
    (WinnerName %in% Big10List)
  )

df_scores100_top25 = df_scores100 %>% 
  filter(
    (LoserName  %in% Big25List) &  
    (WinnerName %in% Big25List)
  )

elo_1_topten <- elo.seq(df_scores100_topten$WinnerName, df_scores100_topten$LoserName, df_scores100_topten$PsuedoDate, df_scores100_topten$Tie.Win, startvalue=1500, k=32, runcheck=FALSE)
dom_mat_topten_wins <- creatematrix(elo_1_topten)

elo_1_top25 <- elo.seq(df_scores100_top25$WinnerName, df_scores100_top25$LoserName, df_scores100_top25$PsuedoDate, df_scores100_top25$Tie.Win, startvalue=1500, k=32, runcheck=FALSE)
dom_mat_top25_wins <- creatematrix(elo_1_top25)


