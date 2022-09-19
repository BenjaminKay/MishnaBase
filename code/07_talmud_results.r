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
      AltStrict_1, AltStrict_2, #Money_1, Money_2,
      Strict_Result_1, Strict_Result_2, Strict_Result_Score_1, Strict_Result_Score_2,
      AltMoney_1, AltMoney_2, AltOwe_1, AltOwe_2,
      Owe_Result_Score_1, Owe_Result_Score_2, 
      Money_Result_Score_1, Money_Result_Score_2,
      Chapter, Chapter_Name_Hebrew, Tractate_Name_Hebrew, ReverseExcelTabName, ExcelTabName, ChapterRowKey, HebrewChapterLocation, 
      Ith_chapter_in_seder, PsuedoDate, EffectiveRows, tab, 
      Disputant_Hebrew_D1, Disputant_Hebrew_D1b, Disputant_Hebrew_D1c, Disputant_Hebrew_D2, Disputant_Hebrew_D2b, 
      Disputant_Hebrew_D2c, Is_Hebrew_Disputant_Name_In_Mishna_Text_1, Is_Hebrew_Disputant_Name_In_Mishna_Text_1_any, 
      Is_Hebrew_Disputant_Name_In_Mishna_Text_1b, Is_Hebrew_Disputant_Name_In_Mishna_Text_1c, 
      Is_Hebrew_Disputant_Name_In_Mishna_Text_2, Is_Hebrew_Disputant_Name_In_Mishna_Text_2_any, 
      Is_Hebrew_Disputant_Name_In_Mishna_Text_2b, Is_Hebrew_Disputant_Name_In_Mishna_Text_2c, FileSource, MishnahKey, 
      MishnaText, IsCopy, IsNaText, keep,  Money_Sum, MoneyQuestion, NumDisp, #Owe_1, Owe_2, 
      Owe_Sum, OweQuestion,  Seder_Hebrew, Seder_Name, Seder_Number, Seder_Translate, 
      Strict_Sum, StrictQuestion, AltStrictQuestion, Loser, LoserName, WinnerName, BlankWinner,
      ChumraName, KulaName, BHBhighName, BHBlowName, MoneyName, ChayavhighName, ChayavlowName, OweName,
      Tie.Win, Tie.Strict, Tie.Money, Tie.Owe,
      IsInsideCopy, IsCopy, IsOutsideCopy
)
# They seem to be working
# Test if new owe makes sense (higher wins)
#df_scores7 %>% filter(OweQuestion == 1, (Owe_Result_Score_1 %ni% c("0.0", "1.0"))|(Owe_Result_Score_2 %ni% c("0.0", "1.0"))) %>% select(Owe_1, AltOwe_1, Result_1, Owe_Result_Score_1, Owe_2, AltOwe_2, Result_2, Owe_Result_Score_2) %>% head(20)
df_scores7 %>% filter(OweQuestion == 1, (Owe_Result_Score_1 %ni% c("0.0", "1.0"))|(Owe_Result_Score_2 %ni% c("0.0", "1.0"))) %>% select(AltOwe_1, Result_1, Owe_Result_Score_1, AltOwe_2, Result_2, Owe_Result_Score_2) %>% head(20)
# Test if new money makes sense / BHB (lower wins)
# df_scores7 %>% filter(MoneyQuestion == 1, (Money_Result_Score_1 %ni% c("0.0", "1.0"))|(Money_Result_Score_2 %ni% c("0.0", "1.0"))) %>% select(Money_1, AltMoney_1, Result_1, Money_Result_Score_1, Money_2, AltMoney_2, Result_2, Money_Result_Score_2) %>% head(20)
df_scores7 %>% filter(MoneyQuestion == 1, (Money_Result_Score_1 %ni% c("0.0", "1.0"))|(Money_Result_Score_2 %ni% c("0.0", "1.0"))) %>% select(AltMoney_1, Result_1, Money_Result_Score_1,  AltMoney_2, Result_2, Money_Result_Score_2) %>% head(20)
#df_scores7$Tractate_Name_Hebrew = df_scores7$Chapter_Name_Hebrew 

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

# These will drop all duplicate arguments (useful for mishnah level analysis)
df_scores_no_copies_100        <- df_scores9        %>% filter(!IsCopy)
df_scores_no_copies_100_strict <- df_scores9_strict %>% filter(!IsCopy)
df_scores_no_copies_100_money  <- df_scores9_money  %>% filter(!IsCopy)
df_scores_no_copies_100_owe    <- df_scores9_owe    %>% filter(!IsCopy)


# These will drop duplicate arguments that are copied inside a seder (useful for seder level analysis)
df_scores_in_copies_100        <- df_scores9        %>% filter(!IsInsideCopy)
df_scores_in_copies_100_strict <- df_scores9_strict %>% filter(!IsInsideCopy)
df_scores_in_copies_100_money  <- df_scores9_money  %>% filter(!IsInsideCopy)
df_scores_in_copies_100_owe    <- df_scores9_owe    %>% filter(!IsInsideCopy)


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
# elo_1b <- elo.seq(df_scores100$WinnerName, df_scores100$LoserName, df_scores100$PsuedoDate, df_scores100$Tie.Win, startvalue=DefaultElo, k=32, runcheck=FALSE)
elo_1b <- elo.seq(df_scores_no_copies_100$WinnerName, df_scores_no_copies_100$LoserName, df_scores_no_copies_100$PsuedoDate, df_scores_no_copies_100$Tie.Win, startvalue=DefaultElo, k=32, runcheck=FALSE)

extract_elo(elo_1b)
dom_mat_wins <- creatematrix(elo_1b)


# Notice hos there is no ties vector, because disputes isn't wins.
# elo_dispute_links <- elo.seq(df_scores100$WinnerName, df_scores100$LoserName, df_scores100$PsuedoDate, draw= NULL, startvalue=DefaultElo, k=32, runcheck=FALSE)
elo_dispute_links <- elo.seq(df_scores_no_copies_100$WinnerName, df_scores_no_copies_100$LoserName, df_scores_no_copies_100$PsuedoDate, draw= NULL, startvalue=DefaultElo, k=32, runcheck=FALSE)
extract_elo(elo_dispute_links)
dom_mat_elo_dispute_links <- creatematrix(elo_dispute_links)

# By default corrected for number of interactions in a dyad (prop="Dij"), otherwise the raw proportion (prop="Pij")
#ds_1b <- DS(dom_mat_wins, prop = "Dij") #calculate proportions corrected for chance 
ds_1b <- DS(dom_mat_wins, prop = "Pij") #raw proportions
colnames(ds_1b) <- c("Name", "DS.win", "normDS.win")


elo_2c <- elo.seq(df_scores_in_copies_100_strict$ChumraName, df_scores_in_copies_100_strict$KulaName, df_scores_in_copies_100_strict$PsuedoDate, df_scores_in_copies_100_strict$Tie.Strict, startvalue=DefaultElo, k=32, runcheck=FALSE)
extract_elo(elo_2c)
dom_mat_strict <- creatematrix(elo_2c)

# By default corrected for number of interactions in a dyad (prop="Dij"), otherwise the raw proportion (prop="Pij")
# ds_2c <- DS(dom_mat_strict, prop = "Dij")
ds_2c <- DS(dom_mat_strict, prop = "Pij")
colnames(ds_2c) <- c("Name", "DS.strict", "normDS.strict")

# elo_3 <- elo.run(score(Money_1, Money_2) ~ Disputant_1 + Disputant_2, data = df_scores100 %>% filter(MoneyQuestion==1), k = 32)
# # Display Scores
# final.elos(elo_3)
elo_3b <- elo.seq(df_scores_no_copies_100_money$BHBhighName, df_scores_no_copies_100_money$BHBlowName, df_scores_no_copies_100_money$PsuedoDate, df_scores_no_copies_100_money$Tie.Money, startvalue=DefaultElo, k=32, runcheck=FALSE)

dom_mat_money <- creatematrix(elo_3b)
# By default corrected for number of interactions in a dyad (prop="Dij"), otherwise the raw proportion (prop="Pij")
# ds_3b <- DS(dom_mat_money, prop = "Dij")
ds_3b <- DS(dom_mat_money, prop = "Pij")
colnames(ds_3b) <- c("Name", "DS.money", "normDS.money")


# elo_4 <- elo.run(score(Owe_1, Owe_2) ~ Disputant_1 + Disputant_2, data = df_scores100 %>% filter(OweQuestion==1), k = 32)
# final.elos(elo_4)
elo_4b <- elo.seq(df_scores_no_copies_100_owe$ChayavhighName, df_scores_no_copies_100_owe$ChayavlowName, df_scores_no_copies_100_owe$PsuedoDate, df_scores_no_copies_100_owe$Tie.Owe, startvalue=DefaultElo, k=32, runcheck=FALSE)
dom_mat_owe <- creatematrix(elo_4b)
# ds_4b <- DS(dom_mat_owe, prop = "Dij")
ds_4b <- DS(dom_mat_owe, prop = "Pij")
colnames(ds_4b) <- c("Name", "DS.owe", "normDS.owe")

# Matrix by arguments
dom_mat_disputes <- as.data.frame(dom_mat_wins + t(dom_mat_wins))
dom_mat_disputes$TotalDisputes <- rowSums(dom_mat_disputes)
dom_mat_disputes$Name = row.names(dom_mat_disputes)


# The other way drops ties, this incluses "tie.win" results 
dom_mat_disputes_alt <- as.data.frame(dom_mat_elo_dispute_links + t(dom_mat_elo_dispute_links))
dom_mat_disputes_alt$TotalDisputes <- rowSums(dom_mat_disputes_alt)
dom_mat_disputes_alt$Name = row.names(dom_mat_disputes_alt)


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

df_counts         = stack(df_scores_no_copies_100 %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()
df_chumra_counts  = stack(df_scores_no_copies_100 %>% filter(StrictQuestion==1) %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()
df_money_counts   = stack(df_scores_no_copies_100 %>% filter(MoneyQuestion==1) %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()
df_owe_counts     = stack(df_scores_no_copies_100 %>% filter(OweQuestion==1) %>% select(Disputant_1, Disputant_2)) %>% group_by(values) %>% tally()

# This doesn't work right, because it only counts the full name appearance, and in a split setting it won't match properly
#df_counts_without_splitting = stack(df_scores_bak3 %>% select(Disputant_1, Disputant_2, Disputant_3, Disputant_4, Disputant_5)) %>% group_by(values) %>% tally()
#df_counts_without_splitting$n[is.na(df_counts_without_splitting$values)] <- 0
# Better way is to count the number of unique SeqId where this appears
#df_results_5$UniqueQuestions <- df_scores100 %>% 
df_results_5$UniqueQuestions <- df_scores_no_copies_100 %>% 
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(mycount=length(unique(SeqID))) %>% 
  pull()

#df_results_5 <- left_join(df_results_5, df_scores100 %>% 
df_results_5 <- left_join(df_results_5, df_scores_no_copies_100 %>% 
  filter(MoneyQuestion==1) %>%   
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(UniqueMoneyQuestions=length(unique(SeqID))),
  by="Name" 
  ) %>% mutate(UniqueMoneyQuestions = replace_na(UniqueMoneyQuestions, 0))

#df_results_5 <- left_join(df_results_5, df_scores100 %>% 
df_results_5 <- left_join(df_results_5, df_scores_no_copies_100 %>% 
  filter(OweQuestion==1) %>%   
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(UniqueOweQuestions=length(unique(SeqID))),
  by="Name" 
  ) %>% mutate(UniqueOweQuestions = replace_na(UniqueOweQuestions, 0))


#df_results_5 <- left_join(df_results_5, df_scores100 %>% 
df_results_5 <- left_join(df_results_5, df_scores_no_copies_100 %>% 
  filter(StrictQuestion==1) %>%   
  select(Disputant_1, Disputant_2, SeqID) %>% 
  gather(Disputant, Name, -SeqID) %>% 
  group_by(Name) %>% 
  summarize(UniqueChumraQuestions=length(unique(SeqID))),
  by="Name" 
  ) %>% mutate(UniqueChumraQuestions = replace_na(UniqueChumraQuestions, 0))

# df_WinRate_00 = WinRate(df_scores100)
# Now fixed to handle AltOwe and AltMoney
df_WinRate_00 = WinRate(df_scores_no_copies_100)

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

# Big05List =  (df_results_9 %>% top_n(5,  n.disputes) %>% select(Name))[,1]
# Big10List =  (df_results_9 %>% top_n(10, n.disputes) %>% select(Name))[,1]
# Big15List =  (df_results_9 %>% top_n(15, n.disputes) %>% select(Name))[,1]
# Big20List =  (df_results_9 %>% top_n(20, n.disputes) %>% select(Name))[,1]
# Big25List =  (df_results_9 %>% top_n(25, n.disputes) %>% select(Name))[,1]
# Big30List =  (df_results_9 %>% top_n(30, n.disputes) %>% select(Name))[,1]

NumberNames = length(unique(df_results_9$Name))

Big05List =  (df_results_9 %>% top_n(min(NumberNames, 5 ), n.disputes) %>% select(Name))[,1]
Big10List =  (df_results_9 %>% top_n(min(NumberNames, 10), n.disputes) %>% select(Name))[,1]
Big12List =  (df_results_9 %>% top_n(min(NumberNames, 12), n.disputes) %>% select(Name))[,1]
Big15List =  (df_results_9 %>% top_n(min(NumberNames, 15), n.disputes) %>% select(Name))[,1]
Big20List =  (df_results_9 %>% top_n(min(NumberNames, 20), n.disputes) %>% select(Name))[,1]
Big25List =  (df_results_9 %>% top_n(min(NumberNames, 25), n.disputes) %>% select(Name))[,1]
Big30List =  (df_results_9 %>% top_n(min(NumberNames, 30), n.disputes) %>% select(Name))[,1]

# df_results_9 %>% select(Name, n.disputes) %>% arrange(desc(n.disputes)) %>% top_n(26, n.disputes) %>% write.excel()
# Number of arguments, not number of disputes. 
# df_Seder_Dispute %>% filter(!IsCopy, Disputant=="Shamai") %>% select(Disputant, SeqID)
# As number of arguments
# df_Seder_Dispute %>% filter(!IsCopy, Disputant=="Shamai") %>% summarize(n.args = length(unique(SeqID))) 
# df_Seder_Dispute %>% filter(!IsCopy, Disputant=="Rabbi Dosa ben Harkinas") %>% summarize(n.args = length(unique(SeqID))) 







FieldList25 = Big25List
# FieldList25[26] <- "TotalDisputesAmongBig25"
# FieldList25[27] <- "TotalDisputes"
# FieldList25[28] <- "Name"

FieldList25[length(Big25List) + 1] <- "TotalDisputesAmongBig25"
FieldList25[length(Big25List) + 2] <- "TotalDisputes"
FieldList25[length(Big25List) + 3] <- "Name"



# Reverse order
FieldList25 <- rev(FieldList25)

# Top 30 by contains top 25 by arguments
FieldList30 = Big30List
# FieldList30[32] <- "TotalDisputesAmongBig30"
# FieldList30[33] <- "TotalDisputes"
# FieldList30[34] <- "Name"

FieldList30[length(Big30List) + 1] <- "TotalDisputesAmongBig30"
FieldList30[length(Big30List) + 2] <- "TotalDisputes"
FieldList30[length(Big30List) + 3] <- "Name"

# Reverse order
FieldList30 <- rev(FieldList30)


# Other Rabbi Lists
Table2List = c("Rabbi Meir", "Rabbi Yossi", "Rabbi Yehuda", "Rabbi Shimon")

Table5List = c("Shimon ben Azai", "Rabbi Dosa ben Harkinas", "Tanna Kama", "Rabbi Meir", "Chachamim", "Rabbi Yehuda", "Rabbi Shimon", "Rabbi Yossi", "Average of Rabbis Shimon, Yehuda, and Yossi")

Table7List = c("Tanna Kama", "Rabbi Yehuda", "Chachamim", "Rabbi Meir", "Rabbi Yossi", "Rabbi Shimon", "Rabbi Eliezer", "Rabbi Akiva", "Beit Shamai", "Beit Hillel", "Rabbi Yehoshua", "Rabbi Shimon ben Gamliel", "Rabbi Elazar", "Rabbi Gamliel")

Table8List = c("Tanna Kama", "Rabbi Yehuda", "Chachamim", "Rabbi Meir", "Rabbi Yossi", "Rabbi Shimon", "Rabbi Eliezer", "Rabbi Akiva", "Beit Shamai", "Beit Hillel", "Rabbi Yehoshua", "Rabbi Shimon ben Gamliel", "Rabbi Elazar", "Rabbi Gamliel")

#   “Tanna Kama Usha” and “Chachamim Usha” refer to these constructs in their arguments with Rabbis Elazar, Meir, Shimon, Shimon ben Gamliel, Yehuda, and Yossi.
Table9UshaList    = c("Rabbi Elazar", "Rabbi Meir", "Rabbi Shimon", "Rabbi Shimon ben Gamliel", "Rabbi Yehuda", "Rabbi Yossi") 

#   “Tanna Kama Yavneh” and “Chachamim Yavneh” refer to these constructs in their arguments with Rabbis Akiva, Elazar ben Azarya, Eliezer, Gamliel, Tarfon, and Yehoshua.
Table9YavnehList  = c("Rabbi Akiva", "Rabbi Elazar ben Azarya",  "Rabbi Eliezer", "Rabbi Gamliel", "Rabbi Tarfon", "Rabbi Yehoshua")


##############################################################################################################################################################################################
##########################################################        Make Tables 8 and 9                                            #############################################################
##############################################################################################################################################################################################


#df_Seder_Dispute_Strict = df_scores100 %>% 
# Need two versions of this, one with any copies, one without inside copies (handle later in summary stats)
df_Seder_Dispute_Strict = df_scores100 %>% 
  filter(StrictQuestion==1) %>%  
  mutate(OtherAltStrict_1 = AltStrict_2, OtherAltStrict_2 = AltStrict_1, OtherDisputant_1=Disputant_2, OtherDisputant_2=Disputant_1) %>% 
  select(
    Disputant_1, Disputant_2, 
    SeqID, Seder_Name, DisputeID,
    AltStrict_1, AltStrict_2,  
    OtherAltStrict_1, OtherAltStrict_2, 
    OtherDisputant_1, OtherDisputant_2,
    #Money_1, Money_2,
    AltMoney_1, AltMoney_2,
    #Owe_1, Owe_2,
    AltOwe_1, AltOwe_2,
    StrictQuestion, MoneyQuestion, OweQuestion,
    IsCopy, IsInsideCopy
    ) %>% 
  pivot_longer(
    !c(Seder_Name, SeqID, DisputeID, StrictQuestion, MoneyQuestion, OweQuestion, IsCopy, IsInsideCopy), 
    names_to = c(".value", "DisputantNumber"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) 
df_Seder_Dispute_Strict = rename_column(df_Seder_Dispute_Strict, "AltStrict", "IsStricter")
df_Seder_Dispute_Strict = rename_column(df_Seder_Dispute_Strict, "OtherAltStrict", "OtherIsStricter")


df_Seder_Dispute_Strict$IsUsha    = df_Seder_Dispute_Strict$OtherDisputant %in% Table9UshaList
df_Seder_Dispute_Strict$IsYavneh  = df_Seder_Dispute_Strict$OtherDisputant %in% Table9YavnehList
df_Seder_Dispute_Strict$UshaYavneh = ifelse(df_Seder_Dispute_Strict$IsUsha, "Usha", ifelse(df_Seder_Dispute_Strict$IsYavneh, "Yavneh", "Neither"))

df_Seder_Dispute_Strict %>% head()





df_Dispute_Strict_Sum = df_Seder_Dispute_Strict %>% filter(!IsCopy) %>%
  group_by(Disputant) %>% 
  summarize(
    Sum.Wins.Strict       = sum(IsStricter[IsStricter >  OtherIsStricter], na.rm=TRUE),
    Sum.Ties.Strict       = sum(IsStricter[IsStricter == OtherIsStricter], na.rm=TRUE),
    CountMatches.Strict   = n()
  ) %>% mutate(
    Sum.Loss.Strict       = CountMatches.Strict - Sum.Wins.Strict - Sum.Ties.Strict

  ) %>% mutate(
    WinChumraRate         = (Sum.Wins.Strict  + 0.5 * Sum.Ties.Strict)  / CountMatches.Strict,
    WinChumraRate_ignore_ties = Sum.Wins.Strict / (CountMatches.Strict - Sum.Ties.Strict)
  )


df_Seder_Dispute_Strict_Sum = df_Seder_Dispute_Strict %>% filter(!IsInsideCopy) %>%
  arrange(Seder_Name, Disputant)  %>% 
  group_by(Disputant, Seder_Name) %>% 
  summarize(
    Sum.Wins.Strict       = sum(IsStricter[IsStricter >  OtherIsStricter], na.rm=TRUE),
    Sum.Ties.Strict       = sum(IsStricter[IsStricter == OtherIsStricter], na.rm=TRUE),
    CountMatches.Strict   = n()
  ) %>% mutate(
    Sum.Loss.Strict       = CountMatches.Strict - Sum.Wins.Strict - Sum.Ties.Strict

  ) %>% mutate(
    WinChumraRate         = (Sum.Wins.Strict  + 0.5 * Sum.Ties.Strict)  / CountMatches.Strict, # this is what we want as discussed with Dani on 1/12/22
    WinChumraRate_ignore_ties = Sum.Wins.Strict / (CountMatches.Strict - Sum.Ties.Strict)
  )

df_Dispute_Strict_Sum_trim = df_Dispute_Strict_Sum %>% select(Disputant, WinChumraRate)
df_Dispute_Strict_Sum_trim = rename_column(df_Dispute_Strict_Sum_trim, "WinChumraRate", "AllMishnah")
df_Seder_Dispute_Strict_Sum_trim = df_Seder_Dispute_Strict_Sum %>% select(Disputant, Seder_Name, WinChumraRate)

# nm1 = c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim")
# Table8 = df_Seder_Dispute_Strict_Sum_trim %>% ungroup() %>% 
#   arrange(Seder_Name) %>%
#   pivot_wider(
#     names_from  = Seder_Name,
#     values_from = WinChumraRate
#   ) %>% mutate(
#      sigma_seders=rowSds(as.matrix(.[, c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim")]), na.rm = TRUE) #https://stackoverflow.com/questions/29581612/r-dplyr-mutate-calculating-standard-deviation-for-each-row, uses library(matrixStats)
#   ) %>% left_join(df_Dispute_Strict_Sum_trim, by="Disputant") %>% 
#   filter(
#    Disputant %in% Table8List
#   )


Table8 = df_Seder_Dispute_Strict_Sum_trim %>% ungroup() %>% 
  arrange(Seder_Name) %>%
  pivot_wider(
    names_from  = Seder_Name,
    values_from = WinChumraRate
  ) %>% left_join(df_Dispute_Strict_Sum_trim, by="Disputant") %>% 
  filter(
   Disputant %in% Table8List
  )

if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  Table8 = Table8 %>% mutate(
     sigma_seders=rowSds(as.matrix(.[, c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim")]), na.rm = TRUE) #https://stackoverflow.com/questions/29581612/r-dplyr-mutate-calculating-standard-deviation-for-each-row, uses library(matrixStats)
  )
}




# https://stackoverflow.com/questions/27613310/rounding-selected-columns-of-data-table-in-r
# Table8_Round2 = Table8 %>% mutate_each(funs(round(., 2)), Tohoroth, Kodashim, Moed, Nezikin, Zeraim, Naschim, sigma_seders, AllMishnah)
# Avoids depricated mutate_each and funs
round2 <- function(x) round(x, 2)

if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  Table8_Round2 = Table8 %>% 
    mutate_at(
      c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim", "sigma_seders", "AllMishnah"), 
      round2
      )
} else {
  Table8_Round2 = Table8
}


#View(Table8_Round2 )
write.excel(Table8_Round2)

# df_Dispute_Strict_Sum2 = df_Seder_Dispute_Strict %>% 
df_Dispute_Strict_Sum2 = df_Seder_Dispute_Strict %>% filter(!IsCopy) %>%
  filter(UshaYavneh !="Neither", Disputant %in% c("Tanna Kama","Chachamim")) %>% 
  group_by(Disputant, UshaYavneh) %>% 
  summarize(
    Sum.Wins.Strict       = sum(IsStricter[IsStricter >  OtherIsStricter], na.rm=TRUE),
    Sum.Ties.Strict       = sum(IsStricter[IsStricter == OtherIsStricter], na.rm=TRUE),
    CountMatches.Strict   = n()
  ) %>% mutate(
    Sum.Loss.Strict       = CountMatches.Strict - Sum.Wins.Strict - Sum.Ties.Strict

  ) %>% mutate(
    WinChumraRate         = (Sum.Wins.Strict  + 0.5 * Sum.Ties.Strict)  / CountMatches.Strict,
    WinChumraRate_ignore_ties = Sum.Wins.Strict / (CountMatches.Strict - Sum.Ties.Strict)
  )

# df_Seder_Dispute_Strict_Sum2 = df_Seder_Dispute_Strict %>%
df_Seder_Dispute_Strict_Sum2 = df_Seder_Dispute_Strict %>% filter(!IsInsideCopy) %>%
  arrange(Seder_Name, Disputant)  %>% 
  filter(UshaYavneh !="Neither", Disputant %in% c("Tanna Kama","Chachamim")) %>% 
  group_by(Disputant, UshaYavneh, Seder_Name) %>% 
  summarize(
    Sum.Wins.Strict       = sum(IsStricter[IsStricter >  OtherIsStricter], na.rm=TRUE),
    Sum.Ties.Strict       = sum(IsStricter[IsStricter == OtherIsStricter], na.rm=TRUE),
    CountMatches.Strict   = n()
  ) %>% mutate(
    Sum.Loss.Strict       = CountMatches.Strict - Sum.Wins.Strict - Sum.Ties.Strict

  ) %>% mutate(
    WinChumraRate         = (Sum.Wins.Strict  + 0.5 * Sum.Ties.Strict)  / CountMatches.Strict, # this is what we want as discussed with Dani on 1/12/22
    WinChumraRate_ignore_ties = Sum.Wins.Strict / (CountMatches.Strict - Sum.Ties.Strict)
  )

# Win.Rate.StrictDisputes                  =  (Sum.Wins.StrictDisputes                  + 0.5 * Sum.Ties.StrictDisputes)                 / Count.StrictDisputes,

df_Dispute_Strict_Sum2_trim = df_Dispute_Strict_Sum2 %>% select(Disputant, WinChumraRate)
df_Dispute_Strict_Sum2_trim = rename_column(df_Dispute_Strict_Sum2_trim, "WinChumraRate", "AllMishnah")
df_Seder_Dispute_Strict_Sum2_trim = df_Seder_Dispute_Strict_Sum2 %>% select(Disputant, Seder_Name, WinChumraRate)


# Table9 = df_Seder_Dispute_Strict_Sum2_trim %>% ungroup()  %>% 
#   arrange(Seder_Name) %>% 
#   pivot_wider(
#     names_from  = Seder_Name,
#     values_from = WinChumraRate
#   ) %>% mutate(
#      sigma_seders=rowSds(as.matrix(.[, c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim")]), na.rm = TRUE) #https://stackoverflow.com/questions/29581612/r-dplyr-mutate-calculating-standard-deviation-for-each-row, uses library(matrixStats)
#   ) %>% left_join(df_Dispute_Strict_Sum_trim, by="Disputant") 


Table9 = df_Seder_Dispute_Strict_Sum2_trim %>% ungroup()  %>% 
  arrange(Seder_Name) %>% 
  pivot_wider(
    names_from  = Seder_Name,
    values_from = WinChumraRate
# ) %>% left_join(df_Dispute_Strict_Sum_trim, by="Disputant") 
  ) %>% left_join(df_Dispute_Strict_Sum2 %>% select(Disputant, WinChumraRate) , by="Disputant") 
Table9 = rename_column(Table9, "WinChumraRate", "AllMishnah")


if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  Table9 = Table9 %>% mutate(
       sigma_seders=rowSds(as.matrix(.[, c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim")]), na.rm = TRUE) #https://stackoverflow.com/questions/29581612/r-dplyr-mutate-calculating-standard-deviation-for-each-row, uses library(matrixStats)
    ) 
}

# https://stackoverflow.com/questions/27613310/rounding-selected-columns-of-data-table-in-r
# Table9_Round2 = Table9 %>% mutate_each(funs(round(., 2)), Tohoroth, Kodashim, Moed, Nezikin, Zeraim, Naschim, sigma_seders, AllMishnah)
# Avoids depricated mutate_each and funs
# Table9_Round2 = Table9 %>% 
#   mutate_at(
#     c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim", "sigma_seders", "AllMishnah"), 
#     round2
#     )


# https://stackoverflow.com/questions/27613310/rounding-selected-columns-of-data-table-in-r
# Table8_Round2 = Table8 %>% mutate_each(funs(round(., 2)), Tohoroth, Kodashim, Moed, Nezikin, Zeraim, Naschim, sigma_seders, AllMishnah)
# Avoids depricated mutate_each and funs
round2 <- function(x) round(x, 2)

if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  Table9_Round2 = Table9 %>% 
    mutate_at(
      c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim", "sigma_seders", "AllMishnah"), 
      round2
      )
} else {
  Table9_Round2 = Table9 
}







write.excel(Table9_Round2)


##############################################################################################################################################################################################
##########################################################        Make Table 1                                                   #############################################################
##############################################################################################################################################################################################
df_Seder_Dispute = df_scores100 %>% 
  mutate(
    OtherAltStrict_1 = AltStrict_2, OtherAltStrict_2 = AltStrict_1, 
    OtherDisputant_1 =Disputant_2, OtherDisputant_2=Disputant_1,
    # OtherMoney_1 = Money_2, OtherMoney_2 = Money_1,
    # OtherOwe_1 = Owe_2, OtherOwe_2 = Owe_1
    OtherAltMoney_1 = AltMoney_2, OtherAltMoney_2 = AltMoney_1,
    OtherAltOwe_1   = AltOwe_2, OtherAltOwe_2 = AltOwe_1

    ) %>% 
  select(
    Disputant_1, Disputant_2, 
    SeqID, Seder_Name, DisputeID,
    AltStrict_1, AltStrict_2,  
    OtherAltStrict_1, OtherAltStrict_2, 
    OtherDisputant_1, OtherDisputant_2,
    # Money_1, Money_2,
    # OtherMoney_1, OtherMoney_2,
    # Owe_1, Owe_2,
    # OtherOwe_1, OtherOwe_2,
    AltMoney_1, AltMoney_2,
    OtherAltMoney_1, OtherAltMoney_2,
    AltOwe_1, AltOwe_2,
    OtherAltOwe_1, OtherAltOwe_2,    
    StrictQuestion, MoneyQuestion, OweQuestion,
    IsCopy, IsInsideCopy
    ) %>% 
  pivot_longer(
    !c(Seder_Name, SeqID, DisputeID, StrictQuestion, MoneyQuestion, OweQuestion, IsCopy, IsInsideCopy), 
    names_to = c(".value", "DisputantNumber"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) 


# How many arguments involve the top 12?
df_in_top12 = df_Seder_Dispute
df_in_top12$Disputant_is_top12      = df_in_top12$Disputant      %in% Big12List
df_in_top12$OtherDisputant_is_top12 = df_in_top12$OtherDisputant %in% Big12List
df_in_top12$BothTop12 = df_in_top12$Disputant_is_top12 & df_in_top12$OtherDisputant_is_top12
df_in_top12$EitherTop12 = df_in_top12$Disputant_is_top12 | df_in_top12$OtherDisputant_is_top12

df_in_top12 = df_in_top12 %>% group_by(SeqID) %>% mutate(MinBothTop12 = min(BothTop12))
df_in_top12  %>% select(SeqID, Disputant, OtherDisputant, Disputant_is_top12, OtherDisputant_is_top12, BothTop12, MinBothTop12) %>% head()


df_in_top12 %>% filter(!IsCopy) %>% group_by(MinBothTop12) %>% summarize(
  CountOfUniqueArguments = length(unique(SeqID ))
  ) %>% ungroup() %>% mutate(
  TotalUniqueArguments = sum(CountOfUniqueArguments)
  ) %>% mutate(
  ShareAsPct = 100 * (CountOfUniqueArguments / TotalUniqueArguments)
  )

df_in_top12 %>% filter(!IsCopy) %>% group_by(EitherTop12) %>% summarize(
  CountOfUniqueArguments = length(unique(SeqID ))
  ) %>% ungroup() %>% mutate(
  TotalUniqueArguments = sum(CountOfUniqueArguments)
  ) %>% mutate(
  ShareAsPct = 100 * (CountOfUniqueArguments / TotalUniqueArguments)
  )





df_Seder_Dispute_2way = df_scores100 %>% filter(EffectiveRows == 1) %>% 
  mutate(
    OtherAltStrict_1 = AltStrict_2, OtherAltStrict_2 = AltStrict_1, 
    OtherDisputant_1 =Disputant_2, OtherDisputant_2=Disputant_1,
    # OtherMoney_1 = Money_2, OtherMoney_2 = Money_1,
    # OtherOwe_1 = Owe_2, OtherOwe_2 = Owe_1
    OtherAltMoney_1 = AltMoney_2, OtherAltMoney_2 = AltMoney_1,
    OtherAltOwe_1   = AltOwe_2, OtherAltOwe_2 = AltOwe_1

    ) %>% 
  select(
    Disputant_1, Disputant_2, 
    SeqID, Seder_Name, DisputeID,
    AltStrict_1, AltStrict_2,  
    OtherAltStrict_1, OtherAltStrict_2, 
    OtherDisputant_1, OtherDisputant_2,
    # Money_1, Money_2,
    # OtherMoney_1, OtherMoney_2,
    # Owe_1, Owe_2,
    # OtherOwe_1, OtherOwe_2,
    AltMoney_1, AltMoney_2,
    OtherAltMoney_1, OtherAltMoney_2,
    AltOwe_1, AltOwe_2,
    OtherAltOwe_1, OtherAltOwe_2,    
    StrictQuestion, MoneyQuestion, OweQuestion,
    IsCopy, IsInsideCopy
    ) %>% 
  pivot_longer(
    !c(Seder_Name, SeqID, DisputeID, StrictQuestion, MoneyQuestion, OweQuestion, IsCopy, IsInsideCopy), 
    names_to = c(".value", "DisputantNumber"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) 


df_in_top12_2way = df_Seder_Dispute_2way
df_in_top12_2way$Disputant_is_top12      = df_in_top12_2way$Disputant      %in% Big12List
df_in_top12_2way$OtherDisputant_is_top12 = df_in_top12_2way$OtherDisputant %in% Big12List
df_in_top12_2way$BothTop12 = df_in_top12_2way$Disputant_is_top12 & df_in_top12_2way$OtherDisputant_is_top12
df_in_top12_2way$EitherTop12 = df_in_top12_2way$Disputant_is_top12 | df_in_top12_2way$OtherDisputant_is_top12

df_in_top12_2way = df_in_top12_2way %>% group_by(SeqID) %>% mutate(MinBothTop12 = min(BothTop12))


df_in_top12_2way %>% filter(!IsCopy) %>% group_by(MinBothTop12) %>% summarize(
  CountOfUniqueArguments = length(unique(SeqID ))
  ) %>% ungroup() %>% mutate(
  TotalUniqueArguments = sum(CountOfUniqueArguments)
  ) %>% mutate(
  ShareAsPct = 100 * (CountOfUniqueArguments / TotalUniqueArguments)
  )





df_Seder_Dispute = rename_column(df_Seder_Dispute, "AltStrict", "IsStricter")
df_Seder_Dispute = rename_column(df_Seder_Dispute, "OtherAltStrict", "OtherIsStricter")

df_Seder_Dispute$IsUsha    = df_Seder_Dispute$OtherDisputant %in% Table9UshaList
df_Seder_Dispute$IsYavneh  = df_Seder_Dispute$OtherDisputant %in% Table9YavnehList
df_Seder_Dispute$UshaYavneh = ifelse(df_Seder_Dispute$IsUsha, "Usha", ifelse(df_Seder_Dispute$IsYavneh, "Yavneh", "Neither"))

df_Seder_Dispute$IsInIsTable2List   = (df_Seder_Dispute$Disputant %in% Table2List)

df_Seder_Dispute$OtherKamaChachamim   = ifelse(df_Seder_Dispute$OtherDisputant == "Tanna Kama", "Tanna Kama",
                                        ifelse(df_Seder_Dispute$OtherDisputant == "Chachamim", "Chachamim", "Neither"))

df_Seder_Dispute$OtherTable2List = ifelse(df_Seder_Dispute$OtherDisputant %in% Table2List, df_Seder_Dispute$OtherDisputant, "Other")
# Percentage of Each Rabbi's arguments with Tanna Kama and Chachmim 

df_Seder_Dispute$DisputantTable2List = ifelse(df_Seder_Dispute$Disputant %in% Table2List, df_Seder_Dispute$Disputant, "Other")
# Percentage of Each Rabbi's arguments with Tanna Kama and Chachmim 


df_Seder_Dispute$DisputantKamaChachamim   = ifelse(df_Seder_Dispute$Disputant %in% c("Tanna Kama", "Chachamim"), df_Seder_Dispute$Disputant, "Neither")


# TotalArguments = length(unique(df_Seder_Dispute$SeqID ))
TotalArguments = df_Seder_Dispute %>% filter(!IsCopy) %>% summarize(
    TotalUniqueArguments_AllMishnah = length(unique(SeqID ))
  ) %>% pull()

# Table1_Round0 = df_Seder_Dispute %>%
Table1_Round0 = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  group_by(Disputant) %>%
  summarize(TotalUniqueArguments = length(unique(SeqID))) %>%
  arrange(desc(TotalUniqueArguments)) %>% 
  mutate(PercentageOfAllArguments = round(100*TotalUniqueArguments / TotalArguments,0))

write.excel(Table1_Round0)

##############################################################################################################################################################################################
##########################################################        Make Tables 2A, 2B, and 2C                                        ##########################################################
##############################################################################################################################################################################################

TotalArgumentsWithTable2List    = df_Seder_Dispute %>% filter(IsInIsTable2List) %>% 
  summarize(
    TotalArguments = length(unique(SeqID ))
  ) %>% pull(TotalArguments)


# Table2A_Round2 = df_Seder_Dispute %>% 
Table2A_Round2 = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  #  filter(IsInIsTable2List, OtherKamaChachamim != "Neither") %>%
  group_by(Disputant, OtherKamaChachamim) %>%
  summarize(TotalUniqueArguments = length(unique(SeqID))) %>%
  arrange(desc(TotalUniqueArguments)) %>% 
  #  mutate(PercentageOfTotalArguments = round(100*TotalUniqueArguments / TotalArgumentsWithTable2List,0))  %>%
  pivot_wider(
    names_from  = OtherKamaChachamim,
  #    values_from = c(TotalUniqueArguments, PercentageOfTotalArguments)
    values_from = TotalUniqueArguments
  ) %>% smart_df_fillna() %>%
  # left_join(df_Seder_Dispute  %>%
  left_join(df_Seder_Dispute  %>% filter(!IsCopy) %>% 
    group_by(Disputant) %>%
    summarize(TotalUniqueArguments = length(unique(SeqID))),
    by = "Disputant" 
    ) %>% 
  mutate(
    PctWithChachamim = round(Chachamim    / TotalUniqueArguments * 100, 2),
    PctWithTannaKama  = round(`Tanna Kama`  / TotalUniqueArguments * 100, 2)
  ) %>% 
  rename(ArgumentsW_TannaKama = 'Tanna Kama') %>% 
  rename(ArgumentsW_Chachamim = Chachamim) %>% 
  rename(ArgumentsW_Other = Neither) %>%     
  rename(ArgumentsW_Any = TotalUniqueArguments) %>% 
  filter(Disputant %in% Table2List) %>%
  arrange(desc(ArgumentsW_Any))

write.excel(Table2A_Round2)

# Table2B_Round2 = df_Seder_Dispute %>%
Table2B_Round2 = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  #  filter(IsInIsTable2List, OtherKamaChachamim != "Neither") %>%
  group_by(Disputant, OtherTable2List) %>%
  summarize(TotalUniqueArguments = length(unique(SeqID))) %>%
  arrange(desc(TotalUniqueArguments)) %>% 
  #  mutate(PercentageOfTotalArguments = round(100*TotalUniqueArguments / TotalArgumentsWithTable2List,0))  %>%
  pivot_wider(
    names_from  = OtherTable2List,
  #    values_from = c(TotalUniqueArguments, PercentageOfTotalArguments)
    values_from = TotalUniqueArguments
  ) %>% smart_df_fillna() %>%
  # left_join(df_Seder_Dispute  %>% 
  left_join(df_Seder_Dispute  %>% filter(!IsCopy) %>% 
    group_by(Disputant) %>%
    summarize(TotalUniqueArguments = length(unique(SeqID))),
    by = "Disputant" 
    ) %>% 
  mutate(
    PctWith_RabbiYehuda    = round(`Rabbi Yehuda`  / TotalUniqueArguments * 100, 2),
    PctWith_RabbiMeir      = round(`Rabbi Meir`    / TotalUniqueArguments * 100, 2),
    PctWith_RabbiYossi      = round(`Rabbi Yossi`    / TotalUniqueArguments * 100, 2),        
    PctWith_RabbiShimon    = round(`Rabbi Shimon`  / TotalUniqueArguments * 100, 2)
  ) %>% 
  rename(ArgumentsW_RabbiYehuda = 'Rabbi Yehuda') %>% 
  rename(ArgumentsW_RabbiMeir = 'Rabbi Meir' ) %>% 
  rename(ArgumentsW_RabbiYossi = 'Rabbi Yossi' ) %>%     
  rename(ArgumentsW_RabbiShimon = 'Rabbi Shimon' ) %>%     
  rename(ArgumentsW_Other = Other ) %>%       
  rename(ArgumentsW_Any = TotalUniqueArguments) %>% 
  filter(Disputant %in% c("Tanna Kama", "Chachamim")) %>%
  arrange(desc(ArgumentsW_Any)) %>% 
  pivot_longer(
    -Disputant , 
    names_to = c(".value", "OtherDisputant"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )  %>% pivot_wider(
    names_from  = Disputant,
  #    values_from = c(TotalUniqueArguments, PercentageOfTotalArguments)
    values_from = c(ArgumentsW, PctWith)
  ) %>% filter(
  OtherDisputant %ni% c("Other", "Any")
  )

write.excel(Table2B_Round2)  


# Table2C_Round2 = df_Seder_Dispute %>% 
Table2C_Round2 = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  #  filter(IsInIsTable2List, OtherKamaChachamim != "Neither") %>%
  group_by(DisputantTable2List, OtherTable2List) %>%
  summarize(TotalUniqueArguments = length(unique(SeqID))) %>%
  arrange(desc(TotalUniqueArguments)) %>% 
  #  mutate(PercentageOfTotalArguments = round(100*TotalUniqueArguments / TotalArgumentsWithTable2List,0))  %>%
  pivot_wider(
    names_from  = OtherTable2List,
  #    values_from = c(TotalUniqueArguments, PercentageOfTotalArguments)
    values_from = TotalUniqueArguments
  ) %>% smart_df_fillna() %>%
  # left_join(df_Seder_Dispute  %>%
  left_join(df_Seder_Dispute  %>% filter(!IsCopy) %>% 
    group_by(DisputantTable2List) %>%
    summarize(TotalUniqueArguments = length(unique(SeqID))),
    by = "DisputantTable2List" 
    ) %>% 
  mutate(
    PctWith_Other          = round(Other           / TotalUniqueArguments * 100, 2),
    PctWith_RabbiYehuda    = round(`Rabbi Yehuda`  / TotalUniqueArguments * 100, 2),
    PctWith_RabbiMeir      = round(`Rabbi Meir`    / TotalUniqueArguments * 100, 2),
    PctWith_RabbiYossi      = round(`Rabbi Yossi`    / TotalUniqueArguments * 100, 2),        
    PctWith_RabbiShimon    = round(`Rabbi Shimon`  / TotalUniqueArguments * 100, 2)
  )  %>% 
  rename(ArgumentsW_RabbiYehuda = 'Rabbi Yehuda') %>% 
  rename(ArgumentsW_RabbiMeir = 'Rabbi Meir' ) %>% 
  rename(ArgumentsW_RabbiYossi = 'Rabbi Yossi' ) %>%     
  rename(ArgumentsW_RabbiShimon = 'Rabbi Shimon' ) %>%     
  rename(ArgumentsW_Other = Other ) %>%       
  rename(ArgumentsW_Any = TotalUniqueArguments) %>% 
  rename(Disputant = DisputantTable2List) %>%   
  #  filter(Disputant %in% Table1and2List) %>%
  arrange(desc(ArgumentsW_Any))

write.excel(Table2C_Round2)

##############################################################################################################################################################################################
##########################################################        Make Table 6                                                      ##########################################################
##############################################################################################################################################################################################

df_scores100$TractateChapter = paste0(df_scores100$tabnum, "_", df_scores100$Chapter)
# Table6 = df_scores100  %>% 
Table6 = df_scores100  %>% filter(!IsInsideCopy) %>% 
  arrange(Seder_Name) %>%
  group_by(Seder_Name) %>%
  summarize(
    TotalUniqueArguments = length(unique(SeqID))
  ) 

Table6b = df_scores100  %>% filter(!IsCopy) %>% 
  summarize(
    TotalUniqueArguments = length(unique(SeqID))
  ) 
# Old way usesinside copies
# Table6[nrow(Table6) + 1,] = list("AllMishnah", sum(Table6$TotalUniqueArguments))
# New version excludes inside and outside copies for total 
Table6[nrow(Table6) + 1,] = list("AllMishnah", Table6b$TotalUniqueArguments)




# 2/9
# One small change: for Table 6’s count: if I’m understanding correctly, you are having the code count how many chapters have arguments. But I think a chapter with no arguments is still relevant for this metric and should bring down the average. As such, you should use the total number of chapters. That is:
# Zeraim: 74 (some lists have 75, but the 4th chapter of Bikkurim doesn't count)
# Moed: 88 chapters
# Naschim: 71 chapters
# Nezikin: 73 chapters (some lists have 74, but the 6th chapter of Avot doesn't count)
# Kodashim: 91 chapters
# Tohoroth: 126 chapters  

# Counting way
# # Table6_SederChapters = df_scores100  %>% 
# Table6_SederChapters = df_scores100  %>% filter(!IsInsideCopy) %>% #although this shouldn't matter for this statistic
#   arrange(Seder_Name) %>%
#   group_by(Seder_Name, ) %>%
#   summarize(
#     SederChapters = length(unique(TractateChapter))
#   ) 

Table6_SederChapters <- read.table(header=TRUE, text="
Seder_Name  SederChapters
Zeraim         74         
Moed           88         
Naschim        71
Nezikin        73
Kodashim       91
Tohoroth       126
")

Table6_SederChapters[nrow(Table6_SederChapters) + 1,] = list("AllMishnah",sum(Table6_SederChapters$SederChapters))

Table6 = left_join(Table6, Table6_SederChapters, by="Seder_Name")
Table6$AvgNumArgChapter = Table6$TotalUniqueArguments / Table6$SederChapters

Table6_2 = Table6 %>%
  pivot_wider(
    names_from  = Seder_Name,
    values_from = c(TotalUniqueArguments, SederChapters, AvgNumArgChapter)
  ) 

# Table6_3 = Table6_2 %>% 
#     pivot_longer(
#     everything(), 
#     names_to = c("Variable", ".value"), 
#     names_sep = "_", 
#     values_drop_na = TRUE
#   )  %>% 
#   mutate_at(
#     c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim", "AllMishnah"), 
#     round2
#     )

Table6_3 = Table6_2 %>% 
    pivot_longer(
    everything(), 
    names_to = c("Variable", ".value"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )  


if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  Table6_3 = Table6_3 %>% 
    mutate_at(
      c("Tohoroth", "Kodashim", "Moed", "Nezikin", "Zeraim", "Naschim", "AllMishnah"), 
      round2
      )
}


write.excel(Table6_3)

##############################################################################################################################################################################################


##############################################################################################################################################################################################
##########################################################        Make Table 4                                                      ##########################################################
##############################################################################################################################################################################################

# Two way arguments only
# Table4 <- df_scores100  %>% 
# Table4_2_way <- df_scores100  %>% filter(!IsCopy) %>% 
#   filter(EffectiveRows == 1, Disputant_1 %in% Table2List, Disputant_2 %in% Table2List) %>% # two way arguments only
#   group_by(Disputant_1, Disputant_2)  %>%
#   mutate(Disputant_2 = paste0(Disputant_2, "_2nd") )  %>%
#   summarize( Tally = n()) %>%
#   pivot_wider(
#     names_from  = Disputant_2,
#     values_from = Tally
#   ) %>%
#   smart_df_fillna()  %>%       
#   rename(Disputant_1st = Disputant_1)

# if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
#   Table4_2_way = Table4_2_way %>% select(
#     "Disputant_1st", "Rabbi Meir_2nd", "Rabbi Shimon_2nd", "Rabbi Yehuda_2nd", "Rabbi Yossi_2nd"
#     )

#   # Dani manually estimated the number of multi-way arguments and it resulted in this data
#   # See 220206_table 4 arguments?
#   Table4_n_way = Table4_2_way
#   Table4_n_way[, 2:5] = 0
#   Table4_n_way$"Rabbi Meir_2nd"   = c(0, 1, 4, 1)
#   Table4_n_way$"Rabbi Shimon_2nd" = c(15, 0, 6, 3)
#   Table4_n_way$"Rabbi Yehuda_2nd" = c(27, 2, 0, 1)
#   Table4_n_way$"Rabbi Yossi_2nd"   = c(18, 1, 4, 0)
#   Table4_n_way = Table4_n_way %>% select(Disputant_1st, "Rabbi Meir_2nd", "Rabbi Shimon_2nd", "Rabbi Yehuda_2nd", "Rabbi Yossi_2nd")

#   Table4 = Table4_2_way
#   Table4[, 2:5] = 0
#   Table4$"Rabbi Meir_2nd"   = Table4_n_way$"Rabbi Meir_2nd"   + Table4_2_way$"Rabbi Meir_2nd"  
#   Table4$"Rabbi Shimon_2nd" = Table4_n_way$"Rabbi Shimon_2nd" + Table4_2_way$"Rabbi Shimon_2nd"
#   Table4$"Rabbi Yehuda_2nd" = Table4_n_way$"Rabbi Yehuda_2nd" + Table4_2_way$"Rabbi Yehuda_2nd"
#   Table4$"Rabbi Yossi_2nd"   = Table4_n_way$"Rabbi Yossi_2nd"   + Table4_2_way$"Rabbi Yossi_2nd"  
#   write.excel(Table4)
# }

# New version uses imported data using hand coded file df_tbl4_nway_arguments (source of df_tbl4_nway_arguments)
Table4_2_way_entries = df_scores100  %>% filter(!IsCopy) %>% 
  filter(EffectiveRows == 1, Disputant_1 %in% Table2List, Disputant_2 %in% Table2List) %>% # two way arguments only
  group_by(Disputant_1, Disputant_2)  %>%
  mutate(Disputant_2 = paste0(Disputant_2, "_2nd") ) %>% select(
    Disputant_1, Disputant_2, SeqID, Seder_Name, tabnum, Location
    )

Table4_n_way_entries = df_tbl4_nway_arguments %>% filter(Count == 1) %>%
  mutate(Disputant_2 = paste0(Disputant_2, "_2nd") ) %>% select(
    Disputant_1, Disputant_2, SeqID, Seder_Name, tabnum, Location
    )
Table4_n_way_entries$tabnum = as.character(Table4_n_way_entries$tabnum)
Table4_n_way_entries$Location = as.character(Table4_n_way_entries$Location)


Table4_all_entries = rbind(Table4_2_way_entries, Table4_n_way_entries)

Table4 <- Table4_all_entries  %>%
  summarize( Tally = n()) %>%
  pivot_wider(
    names_from  = Disputant_2,
    values_from = Tally
  ) %>%
  smart_df_fillna()  %>%       
  rename(Disputant_1st = Disputant_1)

write.excel(Table4)
# # for_dani

# df_tbl4_2_way_disputes = df_scores100  %>% 
#                       filter(!IsCopy) %>% 
#                       filter(EffectiveRows == 1, Disputant_1 %in% Table2List, Disputant_2 %in% Table2List) %>%
#                       select(Disputant_1, Disputant_2, SeqID, Seder_Name, tabnum, Location)



# write.excel(df_tbl4_2_way_disputes )


# df_tbl4_n_way_disputes = df_scores100  %>% 
#                       filter(!IsCopy) %>% 
#                       filter(EffectiveRows > 1, Disputant_1 %in% Table2List, Disputant_2 %in% Table2List) %>%
#                       select(Disputant_1, Disputant_2, SeqID, Seder_Name, tabnum, Location)

# write.excel(df_tbl4_n_way_disputes)



# df_check_money = df_scores100 %>% filter(
#                       DisputeID %in% sample(
#                         unique(
#                           df_scores100 %>% filter(MoneyQuestion == 1) %>% select(DisputeID) %>% pull()
#                           ), 
#                         3, replace=FALSE)
#                     ) %>% select(
#                             MoneyQuestion, DisputeID, SeqID, 
#                             Disputant_1, Disputant_2, Result_1, Result_2,
#                             #Money_1, Money_2
#                             AltMoney_1, AltMoney_2
#                             )
# write.excel(df_check_money )


# df_check_strict = df_scores100 %>% filter(
#                       DisputeID %in% sample(
#                         unique(
#                           df_scores100 %>% filter(StrictQuestion == 1) %>% select(DisputeID) %>% pull()
#                           ), 
#                         3, replace=FALSE)
#                     ) %>% select(
#                             StrictQuestion, DisputeID, SeqID, 
#                             Disputant_1, Disputant_2, Result_1, Result_2,
#                             AltStrict_1 , AltStrict_2
#                             )
# write.excel(df_check_strict )



# df_check_owe = df_scores100 %>% filter(
#                       DisputeID %in% sample(
#                         unique(
#                           df_scores100 %>% filter(OweQuestion == 1) %>% select(DisputeID) %>% pull()
#                           ), 
#                         3, replace=FALSE)
#                     ) %>% select(
#                             OweQuestion, DisputeID, SeqID, 
#                             Disputant_1, Disputant_2, Result_1, Result_2,
#                             #Owe_1 , Owe_2
#                             AltOwe_1 , AltOwe_2
#                             )
# write.excel(df_check_owe )

# df_10_random_rows = df_tbl4_n_way_arguments[sample(nrow(df_tbl4_n_way_disputes), 10, replace=FALSE), ]
# df_10_random_rows = df_10_random_rows %>% left_join(
#   df_scores0 %>% select(Disputant_1, tabnum, Location),
#   by=c("tabnum"="tabnum", "Location"="Location")
#   )
# write.excel(df_10_random_rows)

# df_10_random_arguments = df_tbl4_n_way_disputes %>% filter(
#   SeqID %in% sample(unique(df_tbl4_n_way_disputes$SeqID), 10, replace=FALSE)
#   )
# df_10_random_arguments = df_10_random_arguments %>% left_join(
#   df_scores1 %>% select(Disputant_1, SeqID),
#   by=c("SeqID"="SeqID")
#   )
# write.excel(df_10_random_arguments)

# For manual checking
# df_tbl4_n_way_disputes_w_seqid = df_tbl4_n_way_disputes %>% left_join(
#   df_scores1 %>% select(Disputant_1, SeqID),
#   by=c("SeqID"="SeqID")
#   ) 
# df_tbl4_n_way_disputes_w_seqid %>% write.excel()


# df_scores100 %>% filter(SeqID == 536)
# df_scores0 %>% filter(tabnum == 40, Location == "1:5")




# Output Table4_2_way
#   Disputant_1st Rabbi Meir_2nd Rabbi Shimon_2nd Rabbi Yehuda_2nd Rabbi Yossi_2nd
# 1    Rabbi Meir              0               11               31             27
# 2  Rabbi Shimon              3                0                3              3
# 3  Rabbi Yehuda             18                9                0             19
# 4    Rabbi Yossi              1                3                6              0

# 220115_tables_for_paper output, table 4 tab
# Disputant_1st Rabbi Shimon_2nd  Rabbi Yehuda_2nd  Rabbi Yossi_2nd  Rabbi Meir_2nd
# Rabbi Meir    11                31                27              0
# Rabbi Shimon  0                 3                 3               3
# Rabbi Yehuda  9                 0                 19              18
# Rabbi Yossi    3                 6                 0               1



# Output table 4_2_way in 1/13 email
# Disputant_1st   Rabbi Shimon_2nd  Rabbi Yehuda_2nd  Rabbi Yossi_2nd  Rabbi Meir_2nd
# Rabbi Meir      15                34                34              0
# Rabbi Shimon    0                 6                 3               3
# Rabbi Yehuda    15                0                 23              21
# Rabbi Yossi      7                 6                 0               2


# Output Table 4
#   Disputant_1st Rabbi Meir_2nd Rabbi Shimon_2nd Rabbi Yehuda_2nd Rabbi Yossi_2nd
# 1    Rabbi Meir              0               26               58             45
# 2  Rabbi Shimon              4                0                5              4
# 3  Rabbi Yehuda             22               15                0             23
# 4    Rabbi Yossi              2                6                7              0

# From email on 1/15, doesn't seem to match. 
# Total               Rabbi Meir later  Rabbi Shimon later  Rabbi Yehuda later  Rabbi Yossi later
# Rabbi Meir 1st        0                 30                 61                 52
# Rabbi Shimon 1st      4                 0                  8                  4
# Rabbi Yehuda 1st      25                21                 0                  27
# Rabbi Yossi 1st       3                 10                 7                  0
# write.excel(Table4)

####################################################################################################################################################################################

##############################################################################################################################################################################################
##########################################################        Make Table 5  and figure 4                                        ##########################################################
##############################################################################################################################################################################################

# Tanna Arguments that can be coded for strictness  
# Arguments that can be coded for charity 
# Arguments that can be coded for civil payments  
# Strict positions  
# Positions requiring charity 
# Positions requiring civil payments

# Table5_0 = df_Seder_Dispute %>% 
Table5_0 = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  group_by(Disputant) %>% 
  summarize(
    Count.StrictArguments                  =  length(unique(SeqID[StrictQuestion > 0 ])),
    Count.CharityBHBMoneyArguments         =  length(unique(SeqID[MoneyQuestion > 0  ])),
    Count.CivilPaymentsChayavOweArguments  =  length(unique(SeqID[OweQuestion > 0    ])),
    Count.StrictDisputes                   =  length(unique(DisputeID[StrictQuestion > 0 ])),
    Count.CharityBHBMoneyDisputes          =  length(unique(DisputeID[MoneyQuestion > 0  ])),
    Count.CivilPaymentsChayavOweDisputes   =  length(unique(DisputeID[OweQuestion > 0    ])),
    Sum.Wins.StrictDisputes                   =  sum(IsStricter[IsStricter  >  OtherIsStricter],  na.rm=TRUE),
    Sum.Ties.StrictDisputes                   =  sum(IsStricter[IsStricter  ==  OtherIsStricter],  na.rm=TRUE),
    # Sum.Wins.CharityBHBMoneyDisputes          =  sum(Money[Money            >  OtherMoney],       na.rm=TRUE),
    # Sum.Wins.CivilPaymentsChayavOweDisputes   =  sum(Owe[Owe                >  OtherOwe],         na.rm=TRUE),
    # Sum.Ties.CharityBHBMoneyDisputes          =  sum(Money[Money            ==  OtherMoney],       na.rm=TRUE),
    # Sum.Ties.CivilPaymentsChayavOweDisputes   =  sum(Owe[Owe                ==  OtherOwe],         na.rm=TRUE),    
    Sum.Wins.CharityBHBMoneyDisputes          =  sum(AltMoney[AltMoney            >  OtherAltMoney],       na.rm=TRUE),
    Sum.Wins.CivilPaymentsChayavOweDisputes   =  sum(AltOwe[AltOwe                >  OtherAltOwe],         na.rm=TRUE),
    #Sum.Ties.CharityBHBMoneyDisputes          =  sum(AltMoney[AltMoney            ==  OtherAltMoney],       na.rm=TRUE),
    Sum.Ties.CharityBHBMoneyDisputes          = length(AltMoney[(AltMoney == OtherAltMoney) & (!is.na(AltMoney)) & (!is.na(OtherAltMoney))]),   
    #Sum.Ties.CivilPaymentsChayavOweDisputes   =  sum(AltOwe[AltOwe                ==  OtherAltOwe],         na.rm=TRUE),      
    Sum.Ties.CivilPaymentsChayavOweDisputes   = length(AltOwe[(AltOwe == OtherAltOwe) & (!is.na(AltOwe)) & (!is.na(OtherAltOwe))])

  ) %>%
  mutate(
    Win.Rate.StrictDisputes                  =  (Sum.Wins.StrictDisputes                  + 0.5 * Sum.Ties.StrictDisputes)                 / Count.StrictDisputes,
    Win.Rate.CharityBHBMoneyDisputes         =  (Sum.Wins.CharityBHBMoneyDisputes         + 0.5 * Sum.Ties.CharityBHBMoneyDisputes)        / Count.CharityBHBMoneyDisputes,
    Win.Rate.CivilPaymentsChayavOweDisputes  =  (Sum.Wins.CivilPaymentsChayavOweDisputes  + 0.5 * Sum.Ties.CivilPaymentsChayavOweDisputes) / Count.CivilPaymentsChayavOweDisputes        
  ) %>%
  mutate(
    Win.Pct.StrictDisputes                  = round(100 * Win.Rate.StrictDisputes, 0), 
    Win.Pct.CharityBHBMoneyDisputes         = round(100 * Win.Rate.CharityBHBMoneyDisputes, 0),
    Win.Pct.CivilPaymentsChayavOweDisputes  = round(100 * Win.Rate.CivilPaymentsChayavOweDisputes, 0) 
  ) %>% 
  filter(
    Disputant %in% Table5List
  )
MiniList = c("Rabbi Yossi", "Rabbi Yehuda", "Rabbi Shimon")
# Make the prettier table (for later export)
Table5_Round0 = Table5_0 %>% select(
    Disputant, 
    Count.StrictArguments, Count.CharityBHBMoneyArguments, Count.CivilPaymentsChayavOweArguments,
    Count.StrictDisputes, Count.CharityBHBMoneyDisputes, Count.CivilPaymentsChayavOweDisputes,
    Win.Pct.StrictDisputes, Win.Pct.CharityBHBMoneyDisputes, Win.Pct.CivilPaymentsChayavOweDisputes 
  )

# Note that average and totals are not well defined when the people in it argue with each other. 
# if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
#   Table5_Round0[length(Table5List), 1] = "Average of Rabbis Shimon, Yehuda, and Yossi"
#   Table5_Round0[length(Table5List), 2:7] = NA
#   Table5_Round0[length(Table5List), 8:10] = round((Table5_Round0[4, 5:7] + Table5_Round0[5, 5:7] + Table5_Round0[6, 5:7]) / 3, 0)
# }

# Manual sort order
#https://stackoverflow.com/questions/9391910/sort-a-data-frame-manually-using-non-numeric-column
# make levels into the sort order
Table5_Round0$Tanna = factor(Table5_Round0$Disputant, levels=Table5List)
# Re-order the data.frame
Table5_Round0 <- Table5_Round0[order(Table5_Round0$Tanna),] %>% select(
    Tanna, 
    Count.StrictArguments, Count.CharityBHBMoneyArguments, Count.CivilPaymentsChayavOweArguments,
    Count.StrictDisputes, Count.CharityBHBMoneyDisputes, Count.CivilPaymentsChayavOweDisputes,    
    Win.Pct.StrictDisputes, Win.Pct.CharityBHBMoneyDisputes, Win.Pct.CivilPaymentsChayavOweDisputes 
  )

write.excel(Table5_Round0)


# Also create Figure 4 Figure 4: Standard error bars for strictness (Source: Authors’ calculations) 
# https://en.wikipedia.org/wiki/Bernoulli_distribution

Figure4List = c("Rabbi Shimon", "Rabbi Yossi", "Chachamim", "Rabbi Yehuda", "Tanna Kama", "Rabbi Meir")
Figure4_0 = Table5_0 %>% select(Disputant, Win.Rate.StrictDisputes, Count.StrictDisputes) %>% mutate(
  StdErr.Win.Rate.StrictDisputes = sqrt((1-Win.Rate.StrictDisputes) * Win.Rate.StrictDisputes / Count.StrictDisputes)# sqrt( (p * (1-p))/n ) 
  ) %>% filter(Disputant %in% Figure4List)

Figure4_0 <- Figure4_0 %>% mutate(
  mu = Win.Rate.StrictDisputes,
  se = StdErr.Win.Rate.StrictDisputes
  ) %>% mutate(
  mu_plus2sigma  = mu + 2.0 * se,
  mu_minus2sigma = mu - 2.0 * se
  ) %>%
  # https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
  mutate(Tanna = fct_reorder(Disputant, mu, .fun='median'))

# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
p4 <- ggplot(Figure4_0, aes(x=Tanna, y=mu)) + 
  geom_point(color="black") +
  geom_errorbar(aes(ymin=mu_minus2sigma, ymax=mu_plus2sigma), width=.2) +
  ylab("Probability of Being Strict (Disputes)") + 
  theme(text = element_text(size = 20))   # https://statisticsglobe.com/change-font-size-of-ggplot2-plot-in-r-axis-text-main-title-legend
print(p4)

ggsave(paste0(OutputProjectPathStr, OutputFileRoot, "figure_4.png"), width = 16, height = 9, device='png', dpi=300)


##############################################################################################################################################################################################

##############################################################################################################################################################################################
##########################################################        Make Table 7                                                      ##########################################################
##############################################################################################################################################################################################

Tb7_TotalArguments = df_Seder_Dispute %>% filter(!IsCopy) %>% summarize(
  TotalUniqueArguments_AllMishnah = length(unique(SeqID ))
  ) %>% pull()

Table7_ArgumentCounts = df_Seder_Dispute %>% filter(!IsInsideCopy) %>% 
  group_by(Seder_Name) %>%
  summarize(TotalUniqueArguments = length(unique(SeqID))) %>%
  pivot_wider(
    names_from  = "Seder_Name",
    names_prefix = "TotalUniqueArguments_",
    values_from = "TotalUniqueArguments"    
    )
Table7_ArgumentCounts$TotalUniqueArguments_AllMishnah = Tb7_TotalArguments
# Pivot wider and combine with total 

Table7_AllMishnah = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  group_by(Disputant) %>%
  summarize(TotalUniqueArgumentsRabbi_AllMishnah = length(unique(SeqID))) #%>%

Table7_BySeder = df_Seder_Dispute %>% filter(!IsInsideCopy) %>% 
  group_by(Disputant, Seder_Name) %>%
  summarize(TotalUniqueArgumentsRabbi = length(unique(SeqID))) %>%
  pivot_wider(
    names_from  = "Seder_Name",
    names_prefix = "TotalUniqueArgumentsRabbi_",
    values_from = "TotalUniqueArgumentsRabbi"    
    )



Table7_RabbiCounts = Table7_AllMishnah %>% 
  left_join(Table7_BySeder, by="Disputant") %>% 
  arrange(desc(TotalUniqueArgumentsRabbi_AllMishnah)) %>% 
  smart_df_fillna() 

if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  Table7_RabbiCounts$TotalUniqueArguments_Kodashim   = Table7_ArgumentCounts$TotalUniqueArguments_Kodashim   
  Table7_RabbiCounts$TotalUniqueArguments_Moed       = Table7_ArgumentCounts$TotalUniqueArguments_Moed       
  Table7_RabbiCounts$TotalUniqueArguments_Naschim    = Table7_ArgumentCounts$TotalUniqueArguments_Naschim    
  Table7_RabbiCounts$TotalUniqueArguments_Nezikin    = Table7_ArgumentCounts$TotalUniqueArguments_Nezikin    
  Table7_RabbiCounts$TotalUniqueArguments_Tohoroth   = Table7_ArgumentCounts$TotalUniqueArguments_Tohoroth   
  Table7_RabbiCounts$TotalUniqueArguments_Zeraim     = Table7_ArgumentCounts$TotalUniqueArguments_Zeraim     
  Table7_RabbiCounts$TotalUniqueArguments_AllMishnah = Table7_ArgumentCounts$TotalUniqueArguments_AllMishnah 


  Table7_RabbiCounts = Table7_RabbiCounts %>% mutate(
      PercentageOfAllArguments_Kodashim   = TotalUniqueArgumentsRabbi_Kodashim   / TotalUniqueArguments_Kodashim   ,
      PercentageOfAllArguments_Moed       = TotalUniqueArgumentsRabbi_Moed       / TotalUniqueArguments_Moed       ,
      PercentageOfAllArguments_Naschim    = TotalUniqueArgumentsRabbi_Naschim    / TotalUniqueArguments_Naschim    ,
      PercentageOfAllArguments_Nezikin    = TotalUniqueArgumentsRabbi_Nezikin    / TotalUniqueArguments_Nezikin    ,
      PercentageOfAllArguments_Tohoroth   = TotalUniqueArgumentsRabbi_Tohoroth   / TotalUniqueArguments_Tohoroth   ,
      PercentageOfAllArguments_Zeraim     = TotalUniqueArgumentsRabbi_Zeraim     / TotalUniqueArguments_Zeraim     ,
      PercentageOfAllArguments_AllMishnah = TotalUniqueArgumentsRabbi_AllMishnah / TotalUniqueArguments_AllMishnah 
    )

  Table7_draft = Table7_RabbiCounts %>% select(
      Disputant,
      PercentageOfAllArguments_AllMishnah,
      PercentageOfAllArguments_Zeraim    ,    
      PercentageOfAllArguments_Moed      ,
      PercentageOfAllArguments_Naschim   ,
      PercentageOfAllArguments_Nezikin   ,
      PercentageOfAllArguments_Kodashim  ,
      PercentageOfAllArguments_Tohoroth  ,
    )
  Table7_round2  = Table7_draft %>% 
    mutate_at(
      c("PercentageOfAllArguments_AllMishnah", "PercentageOfAllArguments_Zeraim", "PercentageOfAllArguments_Moed", "PercentageOfAllArguments_Naschim", "PercentageOfAllArguments_Nezikin", "PercentageOfAllArguments_Kodashim", "PercentageOfAllArguments_Tohoroth"), 
      round2
      ) %>% filter(Disputant %in% Table7List)
}
####################################################################################################################################################################################


##############################################################################################################################################################################################
##########################################################        Make Table B                                                      ##########################################################
##############################################################################################################################################################################################
# THis is table 5 with everyone in top 30


TableB_0 = df_Seder_Dispute %>% filter(!IsCopy) %>% 
  group_by(Disputant) %>% 
  summarize(
    Count.Arguments                        =  length(unique(SeqID)),
    Count.StrictArguments                  =  length(unique(SeqID[StrictQuestion > 0 ])),
    Count.CharityBHBMoneyArguments         =  length(unique(SeqID[MoneyQuestion > 0  ])),
    Count.CivilPaymentsChayavOweArguments  =  length(unique(SeqID[OweQuestion > 0    ])),
    Count.StrictDisputes                   =  length(unique(DisputeID[StrictQuestion > 0 ])),
    Count.CharityBHBMoneyDisputes          =  length(unique(DisputeID[MoneyQuestion > 0  ])),
    Count.CivilPaymentsChayavOweDisputes   =  length(unique(DisputeID[OweQuestion > 0    ])),
    Sum.Wins.StrictDisputes                   =  sum(IsStricter[IsStricter  >  OtherIsStricter],  na.rm=TRUE),
    Sum.Wins.CharityBHBMoneyDisputes          =  sum(AltMoney[AltMoney            >  OtherAltMoney],        na.rm=TRUE),
    Sum.Wins.CivilPaymentsChayavOweDisputes   =  sum(AltOwe[AltOwe                >  OtherAltOwe],          na.rm=TRUE),
    Sum.Ties.StrictDisputes                   =  sum(IsStricter[IsStricter  ==  OtherIsStricter],           na.rm=TRUE),
    #Sum.Ties.CharityBHBMoneyDisputes          =  sum(AltMoney[AltMoney            ==  OtherAltMoney],       na.rm=TRUE),
    Sum.Ties.CharityBHBMoneyDisputes          = length(AltMoney[(AltMoney == OtherAltMoney) & (!is.na(AltMoney)) & (!is.na(OtherAltMoney))]),
    #Sum.Ties.CivilPaymentsChayavOweDisputes   =  sum(AltOwe[AltOwe                ==  OtherAltOwe],         na.rm=TRUE), 
    Sum.Ties.CivilPaymentsChayavOweDisputes   = length(AltOwe[(AltOwe == OtherAltOwe) & (!is.na(AltOwe)) & (!is.na(OtherAltOwe))]),   
  ) %>%
  mutate(
    Win.Rate.StrictDisputes                  =  (Sum.Wins.StrictDisputes                  + 0.5 * Sum.Ties.StrictDisputes)                 / Count.StrictDisputes,
    Win.Rate.CharityBHBMoneyDisputes         =  (Sum.Wins.CharityBHBMoneyDisputes         + 0.5 * Sum.Ties.CharityBHBMoneyDisputes)        / Count.CharityBHBMoneyDisputes,
    Win.Rate.CivilPaymentsChayavOweDisputes  =  (Sum.Wins.CivilPaymentsChayavOweDisputes  + 0.5 * Sum.Ties.CivilPaymentsChayavOweDisputes) / Count.CivilPaymentsChayavOweDisputes        
#        WinMoneyRate                        = (Sum.Wins.Money   + 0.5 * Sum.Ties.Money)   / CountMatches.Money,
  ) %>%
  mutate(
    Win.Pct.StrictDisputes                  = round(100 * Win.Rate.StrictDisputes, 0), 
    Win.Pct.CharityBHBMoneyDisputes         = round(100 * Win.Rate.CharityBHBMoneyDisputes, 0),
    Win.Pct.CivilPaymentsChayavOweDisputes  = round(100 * Win.Rate.CivilPaymentsChayavOweDisputes, 0) 
  ) %>% 
  filter(
    Disputant %in% Big30List
  )

# Make the prettier table (for later export)
TableB_Round0 = TableB_0 %>% select(
    Disputant, Count.Arguments,
    Count.StrictArguments, Count.CharityBHBMoneyArguments, Count.CivilPaymentsChayavOweArguments,
    Count.StrictDisputes, Count.CharityBHBMoneyDisputes, Count.CivilPaymentsChayavOweDisputes,
    Win.Pct.StrictDisputes, Win.Pct.CharityBHBMoneyDisputes, Win.Pct.CivilPaymentsChayavOweDisputes 
  ) %>% arrange(desc(Count.Arguments))

write.excel(Table5_Round0)
####################################################################################################################################################################################




##############################################################################################################################################################################################
##########################################################        Write Tables to XLSX                                                      ##########################################################
##############################################################################################################################################################################################

wb_tables <- createWorkbook(
  creator = "Benjamin Kay",
  title = "Tables For Kazhdan and Kay (2022)",
  subject = "Mishnahbase project"
)

## Add 3 worksheets
addWorksheet(wb_tables, "Table 1")
addWorksheet(wb_tables, "Table 2A")
addWorksheet(wb_tables, "Table 2B")
addWorksheet(wb_tables, "Table 2C")
addWorksheet(wb_tables, "Table 3")
addWorksheet(wb_tables, "Table 4")
addWorksheet(wb_tables, "Table 5")
addWorksheet(wb_tables, "Table 6")
addWorksheet(wb_tables, "Table 7")
addWorksheet(wb_tables, "Table 8")
addWorksheet(wb_tables, "Table 9")
addWorksheet(wb_tables, "Table B")

writeData(wb_tables, sheet = "Table 1",  x = Table1_Round0)
writeData(wb_tables, sheet = "Table 2A", x = Table2A_Round2)
writeData(wb_tables, sheet = "Table 2B", x = Table2B_Round2)
writeData(wb_tables, sheet = "Table 2C", x = Table2C_Round2)
# writeData(wb_tables, sheet = "Table 3",  x = Table1) # Can't do this

writeData(wb_tables, sheet = "Table 5",  x = Table5_Round0) # To do Charity=BHB; civil payments=Chayav.
writeData(wb_tables, sheet = "Table 6",  x = Table6_3)
writeData(wb_tables, sheet = "Table 8",  x = Table8_Round2)
writeData(wb_tables, sheet = "Table 9",  x = Table9_Round2)
writeData(wb_tables, sheet = "Table B",  x = TableB_Round0)


if (length(unique(df_Seder_Dispute_Strict$Seder_Name) ) == 6) {
  writeData(wb_tables, sheet = "Table 4",  x = Table4)
  writeData(wb_tables, sheet = "Table 7",  x = Table7_round2) # To do Table 7: Percentage of Seder’s arguments among major Tannaim (Source: Authors’ calculations)  
}
saveWorkbook(wb_tables, paste0(OutputProjectPathStr, OutputFileRoot,   "tables_for_paper.xlsx"), overwrite = TRUE)

##############################################################################################################################################################################################
##################################     Return to Calculating summary stats 
################################################################################################################

dom_mat_disputes$TotalDisputesAmongBig30 <- dom_mat_disputes %>% select(Big30List) %>% rowSums()
# dom_mat_disputes2 <- dom_mat_disputes %>% select(all_of())



dom_mat_disputes2 <- dom_mat_disputes %>% select(any_of(FieldList30))
# Wins or Losses (no ties, undirected)
dom_mat_disputes3 <- dom_mat_disputes2 %>% filter(row.names(.) %in%  Big30List)
dom_mat_disputes3 <- dom_mat_disputes2 %>% filter(row.names(.) %in%  Big30List)

dom_mat_disputes_alt$TotalDisputesAmongBig30 <- dom_mat_disputes_alt %>% select(Big30List) %>% rowSums()
# dom_mat_disputes_alt2 <- dom_mat_disputes_alt %>% select(all_of())
dom_mat_disputes_alt2 <- dom_mat_disputes_alt %>% select(any_of(FieldList30))
# Wins or Losses (no ties, undirected)
dom_mat_disputes_alt3 <- dom_mat_disputes_alt2 %>% filter(row.names(.) %in%  Big30List)
dom_mat_disputes_alt3 <- dom_mat_disputes_alt2 %>% filter(row.names(.) %in%  Big30List)


dom_mat_disputes2 %>% filter(row.names(.) %in%  Table2List) %>% select(any_of(Table2List))
dom_mat_disputes_alt2 %>% filter(row.names(.) %in%  Table2List) %>% select(any_of(Table2List))


# Wins only (directed)
dom_mat_wins2$TotalDisputesAmongBig30 <- dom_mat_wins2 %>% select(Big30List) %>% rowSums() 
dom_mat_wins3 <- dom_mat_wins2 %>% select(any_of(FieldList30))
dom_mat_wins4 <- dom_mat_wins3 %>% filter(row.names(.) %in%  Big30List)

dom_mat_strict3 <- dom_mat_strict2 %>% select(any_of(FieldList30))  %>% filter(row.names(.) %in%  Big30List)
dom_mat_strict4 <- dom_mat_strict3 %>% filter(row.names(.) %in%  Big30List)

df_results_9$n.chumra.exmon = df_results_9$n.chumra - df_results_9$n.money
df_results_9$n.wins.chumra.exmon = df_results_9$n.wins.chumra - df_results_9$n.loss.money

df_results_9$Top10Rabbi = df_results_9$Name %in% Big10List
df_results_9$Top15Rabbi = df_results_9$Name %in% Big15List
df_results_9$Top20Rabbi = df_results_9$Name %in% Big20List
df_results_9$Top25Rabbi = df_results_9$Name %in% Big25List
df_results_9$Top30Rabbi = df_results_9$Name %in% Big30List

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

df_scores100_top30 = df_scores100 %>% 
  filter(
    (LoserName  %in% Big30List) &  
    (WinnerName %in% Big30List)
  )

elo_1_topten <- elo.seq(df_scores100_topten$WinnerName, df_scores100_topten$LoserName, df_scores100_topten$PsuedoDate, df_scores100_topten$Tie.Win, startvalue=1500, k=32, runcheck=FALSE)
dom_mat_topten_wins <- creatematrix(elo_1_topten)

elo_1_top25 <- elo.seq(df_scores100_top25$WinnerName, df_scores100_top25$LoserName, df_scores100_top25$PsuedoDate, df_scores100_top25$Tie.Win, startvalue=1500, k=32, runcheck=FALSE)
dom_mat_top25_wins <- creatematrix(elo_1_top25)

elo_1_top30 <- elo.seq(df_scores100_top30$WinnerName, df_scores100_top30$LoserName, df_scores100_top30$PsuedoDate, df_scores100_top30$Tie.Win, startvalue=1500, k=32, runcheck=FALSE)
dom_mat_top30_wins <- creatematrix(elo_1_top30)