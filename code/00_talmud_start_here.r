# source("C:/Users/Benjamin Kay/OneDrive/Documents/talmud_elo/220916 Data Check for GitHub and GlitchMe/00_talmud_start_here.r")
setwd("C:/Users/Benjamin Kay/OneDrive/Documents/talmud_elo/")
ProjectPathStr = "C:/Users/Benjamin Kay/OneDrive/Documents/talmud_elo/220916 Data Check for GitHub and GlitchMe/"
OutputProjectPathStr  = paste0(ProjectPathStr, "220916 Output/")
InputProjectPathStr   = paste0(ProjectPathStr, "inputs/")


  SourceFile = "Arguments in the Mishna 2022_02_06.xlsx"
# SourceFile = "220209_Test_document_1.xlsx"
# SourceFile = "Test document v6.xlsx"
  
RabbiNameListFile = "Full rabbi list English Hebrew Crosswalk v2 DK.xlsx"
SederFile = "talmud_chapter_info standard seder names DK.xlsx"
MishnaCorpusFile = "mishnah_v3.xlsx"
#table_4_nway_coded_argumentsFile  = "220221_table 4 arguments.xlsx"


OutputFileRoot = "220916_"

print("Load libraries and paths")
library("conflicted")
library("tidyverse") #includes stringr and ggplot2
library("openxlsx")

# Dates
library("zoo")
library("lubridate")

#Network graph functions
library('igraph')
library("data.table")
#Elo functions
# library("elo") # Package has been removed # https://cran.r-project.org/web/packages/elo/elo.pdf
library("EloRating")
# Alternative to david's score percolation and conductance https://www.biorxiv.org/content/10.1101/692384v1.full
library("Perc")
#Randomized elo ratings
library("aniDom")
library("stargazer")
library("ascii")
library("MASS") # Ordered probit with polr
library("glmnet") # For ridge and lasso
library("glmnetUtils") # Allows glmnet to take a formula instead of a matrix argument


library(matrixStats) # for rowSds
#library(EloChoice)

#########################################################################################
####################################  Conflicted Library functions 
#########################################################################################
# statnet.common::order
order   <- base::order
filter  <- dplyr::filter
lag     <- dplyr::lag  
select  <- dplyr::select # MASS messes up
first   <- dplyr::first
cv.glmnet <- glmnet::cv.glmnet

# Should we eliminate arguments that don't have a winner?
# Generally not, so set to FALSE.
YesCullCullNonWinners = FALSE 

#########################################################################################
####################################  Functions
#########################################################################################
source(paste0(ProjectPathStr, "02_talmud_functions.r"))

#########################################################################################
################################### Load data file & Concatenate all the tabs
#########################################################################################

source(paste0(ProjectPathStr, "04_talmud_load_data.r"))


#########################################################################################
################################### Generate and store results
#########################################################################################
source(paste0(ProjectPathStr, "06_talmud_scores.r"))


source(paste0(ProjectPathStr, "07_talmud_results.r"))

# df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreWins, WinRate, DS.win) 
# df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreChumra, WinChumraRate, DS.strict) 
# df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreMoney, WinMoneyRate, DS.money) 
# df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreOwe, WinOweRate, DS.owe) 

#######################################################################################
# Brandes Claims
#######################################################################################
source(paste0(ProjectPathStr, "08_talmud_Brandes.r"))

#######################################################################################
# In-Sample Measure Comparison
#######################################################################################
#source(paste0(ProjectPathStr, "09_talmud_insample.r"))

######################################################################################
################################### Make a network and draw the graph
######################################################################################
#source(paste0(ProjectPathStr, "12_talmud_network_graph.r"))

######################################################################################
################################### Cross-validate 
######################################################################################

###########################################################################################
##################             Dispute Summary Stats  #####################################
###########################################################################################
print("Dispute summary statistics")
df_scores100 %>% group_by(Seder_Name) %>% summarize(
    Arguments = n(), 
    NumSeferWithArguments = n_distinct(ExcelTabName)
  ) %>% mutate(
    ArgumentDensity = Arguments / NumSeferWithArguments
  )

#########################################################################################
# Table on performance for Dani
#########################################################################################
print("Table on performance for Dani")


Top30PrettyTableUnique = df_results_9 %>% filter(Top30Rabbi==TRUE) %>% 
  select(
    Name, 
    UniqueQuestions, 
    UniqueChumraQuestions, 
    UniqueMoneyQuestions, 
    UniqueOweQuestions, 
    WinRate, 
    WinChumraRate, 
    WinMoneyRate,
    WinOweRate,
    WinChumraExMonRate, # here
    WinRate_ignore_ties, 
    WinChumraRate_ignore_ties, 
    WinMoneyRate_ignore_ties,
    WinOweRate_ignore_ties,
    WinChumraExMonRate_ignore_ties
    ) %>%
  mutate(
    WinRate = round(WinRate, 2),
    WinChumraRate = round(WinChumraRate, 2),
    WinMoneyRate = round(WinMoneyRate, 2),    
    WinOweRate = round(WinOweRate, 2),
    WinChumraExMonRate = round(WinChumraExMonRate, 2),
    WinRate_ignore_ties = round(WinRate_ignore_ties, 2),
    WinChumraRate_ignore_ties = round(WinChumraRate_ignore_ties, 2),
    WinMoneyRate_ignore_ties = round(WinMoneyRate_ignore_ties, 2),    
    WinOweRate_ignore_ties = round(WinOweRate_ignore_ties, 2),
    WinChumraExMonRate_ignore_ties = round(WinChumraExMonRate_ignore_ties, 2),
  )
colnames(Top30PrettyTableUnique) = c(
  "Tanna", #1
  "All Arguments (Unique Count)", "Chumra Arguments (Unique Count)","BHB Arguments (Unique Count)", "Chayav Arguments (Unique Count)",
  "All Arguments (Win Rate)", "Chumra Arguments (Win Rate)", "BHB Arguments (Win Rate)", "Chayav Arguments (Win Rate)", "Chumra ex BHB Arguments (Win Rate)",
  "All Arguments (Win Rate, No Ties)", "Chumra Arguments (Win Rate, No Ties)", "BHB Arguments (Win Rate, No Ties)", "Chayav Arguments (Win Rate, No Ties)", "Chumra ex BHB Arguments (Win Rate, No Ties)"  
  )


cor(df_results_9 %>% filter(
    is.finite(WinChumraExMonRate_ignore_ties), !is.na(WinChumraExMonRate_ignore_ties),  
    is.finite(WinMoneyRate_ignore_ties), !is.na(WinMoneyRate_ignore_ties)   
  ) %>%  select(
    WinChumraExMonRate_ignore_ties, 
    WinMoneyRate_ignore_ties
    )
  )

cor(df_results_9 %>% filter(
    is.finite(WinChumraExMonRate), !is.na(WinChumraExMonRate),  
    is.finite(WinMoneyRate), !is.na(WinMoneyRate)   
  ) %>%  select(
    WinChumraExMonRate, 
    WinMoneyRate
    )
  )



df_probs = df_results_9 %>% filter(
  Name %in% c("Tanna Kama", "Rabbi Meir", "Chachamim", "Rabbi Yehuda", "Rabbi Shimon", "Rabbi Yossi")
  ) %>% mutate(
    p_hat_chumra = n.wins.chumra/ (n.wins.chumra + n.loss.chumra),
    n_hat_chumra = n.wins.chumra + n.loss.chumra
  ) %>% mutate(
    var_p_hat_chumra =  p_hat_chumra * (1-p_hat_chumra) / n_hat_chumra
  ) %>% mutate(
    SE_p_hat_chumra =  sqrt(var_p_hat_chumra)
  ) %>% select(Name, p_hat_chumra, n_hat_chumra, SE_p_hat_chumra) %>%  mutate(Name = fct_reorder(Name, p_hat_chumra)) 
df_probs$UpperP = df_probs$p_hat_chumra + 2 * df_probs$SE_p_hat_chumra
df_probs$LowerP = df_probs$p_hat_chumra - 2 * df_probs$SE_p_hat_chumra


ggplot(df_probs, aes(x=Name, y=p_hat_chumra)) + 
    geom_errorbar(aes(ymin=LowerP, ymax=UpperP), width=.1) +
    geom_line() +
    geom_point() +
    ylab("Probability of Being Strict") +
    xlab("Tanna") +
    theme(legend.position = "none") + theme(text = element_text(size = 18)) 



#########################################################################################
# Write output files
#########################################################################################
print("Write Output Files")
# Matrix of connections data
# Wins or Losses (no ties, undirected)
#write.csv(dom_mat_disputes3, "210411_Top30LinkMatrix.csv", row.names = FALSE)

# This _is not_ generally what you want because it _excludes_ ties
# write.csv(dom_mat_disputes3, paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "Top30LinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 

# This _is_ generally what you want because it _includes_ ties
#write.csv(dom_mat_disputes_alt3, paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "Top30LinkMatrixWTies_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 
write.csv(dom_mat_disputes_alt3, paste0(OutputProjectPathStr, "Top30LinkMatrixWTies_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 


# Matrix of wins data (excludes ties and losses (directed))
#write.csv(dom_mat_wins4, "210411_Top30WinLinkMatrix.csv", row.names = FALSE)
#write.csv(dom_mat_wins4, paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "Top30WinLinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 
write.csv(dom_mat_wins4, paste0(OutputProjectPathStr, "Top30WinLinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 

# Matrix of strictness wins (excludes ties and losses (directed))
#write.csv(dom_mat_strict4, "210411_Top30StrictWinLinkMatrix.csv", row.names = FALSE)
#write.csv(dom_mat_strict4, paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "Top30StrictWinLinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 
write.csv(dom_mat_strict4, paste0(OutputProjectPathStr, "Top30StrictWinLinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 


#write.csv(Top30PrettyTableUnique, paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "Top30PrettyTableUnique_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 
write.csv(Top30PrettyTableUnique, paste0(OutputProjectPathStr,  "Top30PrettyTableUnique_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 

# write.csv(Top30PrettyTable,       paste0(SourceFile, "_", OutputFileRoot, "Top30PrettyTableNotUnique_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 

# Dispute perforamnce data data, which is individual disputes 
# (recall that df_results holds the actual rating data)
write.xlsx(df_scores6e, paste0(OutputProjectPathStr, OutputFileRoot,  "df_scores6e.xlsx"))
write.xlsx(df_scores7, paste0(OutputProjectPathStr, OutputFileRoot,   "df_scores7.xlsx")) 

# write_utf8_csv(df_scores7 %>% select(-MishnaText) , paste0(SourceFile, "_", OutputFileRoot, "arguments.csv")) 
# write_utf8_csv(
#   df_scores7 %>% 
#     select(
#       Disputant_1, Disputant_2,
#       Winner, Result_1, Result_2, Score_1, Score_2, Money_1, Money_2, Strict_Result_1, Strict_Result_2,       Strict_Result_Score_1, Strict_Result_Score_2,   
#       StrictQuestion, MoneyQuestion, OweQuestion,             
#       Chapter_Name_Hebrew, Chapter, ChapterRowKey, HebrewChapterLocation, Ith_chapter_in_seder, EffectiveRows, tab,
#       Disputant_Hebrew_D1 , Disputant_Hebrew_D1b, Disputant_Hebrew_D1c, Disputant_Hebrew_D2, Disputant_Hebrew_D2b, Disputant_Hebrew_D2c,
#       FileSource, MishnahKey, Seder_Hebrew, Seder_Name, Seder_Number, Seder_Translate
#       ), 
#   paste0(SourceFile, "_", OutputFileRoot, "arguments.csv")) 

# An argument contains many disputes. A dispute involves exactly 2 people (one or more potentially anonymous).

Table_For_Arguments_dot_CSV =   df_scores2_with_effective_rows %>% 
    select(
      SeqID, Location, EffectiveRows, tabnum, Chapter, tab, tabnum,
      Seder_Hebrew, Seder_Name, Seder_Number, Seder_Translate,
      Tractate_Name_Hebrew, Chapter, Ith_chapter_in_seder, 
      Disputant_1, Result_1, Score_1, Strict_1, Money_1, Owe_1,  
      Disputant_2, Result_2, Score_2, Strict_2, Money_2, Owe_2,   
      Disputant_3, Result_3, Score_3, Strict_3, Money_3, Owe_3,   
      Disputant_4, Result_4, Score_4, Strict_4, Money_4, Owe_4,   
      Disputant_5, Result_5, Score_5, Strict_5, Money_5, Owe_5,   
      Disputant_6, Result_6, Score_6, Strict_6, Money_6, Owe_6,   
      Winner, 
      Comment, 
      IsCopy, keep, NumDisp,
      StrictQuestion, AltStrictQuestion, OweQuestion, MoneyQuestion, 
      Strict_Sum, Money_Sum, Owe_Sum  
      )

# Once not exporting everything in quotes, need to fix the location commas to semi colins or error generated
Table_For_Arguments_dot_CSV$Location = sapply(Table_For_Arguments_dot_CSV$Location, gsub, pattern=",", replacement=";")
# Ditto for comments
Table_For_Arguments_dot_CSV$Comment = sapply(Table_For_Arguments_dot_CSV$Comment, gsub, pattern=",", replacement=";")
write_utf8_csv_noquotes(Table_For_Arguments_dot_CSV, paste0(OutputProjectPathStr, "arguments.csv")) 

# An argument contains many disputes. A dispute involves exactly 2 people (one or more potentially anonymous).
# Appendix A details how one multi-way “argument” can involve several two-way “disputes”.
"
Table for GitHub (Arguments arguments.csv Data Dictionary) Generated abo above in Table_For_Arguments_dot_CSV

| Field                | Description                                                                                                   |
|----------------------|---------------------------------------------------------------------------------------------------------------|
| SeqID                | Unique argument ID.                                                                                           |
| Location             | Combination of chapter and line number(s) within that chapter where the dispute occurs.                       |
| EffectiveRows        | The number of disputes contained within the argument. All rabbis involved in a dispute are paired up with all possible other rabbis who   are involved in the dispute but take a different position.|
| tab                  | The Hebrew name (and number of order) of the 63 masechtot in which the argument is appears.|
| tabnum               | Which of the 63 masechtot / books of the Talmud where the argument appears in.                                |
| Seder_Hebrew         | Name of seder of the Talmud in which the argument appears, written in Hebrew.                                 |
| Seder_Name           | Name of seder of the Talmud in which the argument appears, Hebrew name in English transliteration.            |
| Seder_Number         | Which of the six seders of the Talmud the argument appears in.                                                |
| Seder_Translate      | English translation of which of the six seders of the Talmud the argument appears in.                         |
| Tractate_Name_Hebrew | Which of the 63 masechtot / books of the Talmud where the argument appears in, name in Hebrew.                |
| Chapter              | Which chapter number of the masechtot where the argument appears.                                             |
| Ith_chapter_in_seder | Which chapter number of the seder where the argument appears.                                                 |
| Disputant_N          | The rabbi(s) offering the nth position in the argument.                                                       |
| Result_N             | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher   number is stricter), BHB 1.0 / 0.0, Chayav 1.0 / Patur 0.0, and a few less common topics have separate scores.|
| Score_N              | Disputant(s) N wins the argument according to Maimonides (1)   or loses (0).                                  |
| Strict_N             | Are disputant(s) N stricter (1 if yes, 0 if no or not a   strictness argument).                               |
| Money_N              | Does disputant(s) N say they owe money (1 if yes, 0 if no or not a money owing argument).                     |
| Owe_N                | Does disputant(s) N say they owe (1 if yes, 0 if no or not a owe argument).                                   |
| Winner               | Which disputant(s), by name, won the argument according to Maimonides.                                      |
| Comment              | Miscellaneous notes on the dispute not fitting into other variables.                                          |
| IsCopy               | Is this the second or later appearance of a dispute in the Mishnah? Always false because duplicate arguments are now filtered out. First appearance only.|
| keep                 | Argument meets data quality checks (database only contains values where this is TRUE).                        |
| NumDisp              | Number of distinct disputant positions contained within this   argument. This is not the combinations of disputes or a count of all possible   combinations of disputants. Also see field EffectiveRows.|
| AltStrictQuestion    | Is this a question about strictness (1 if yes, 0 if no)?                                                      |
| OweQuestion          | Is this a question about owing (1 if yes, 0 if no)?                                                           |
| MoneyQuestion        | Is this a question about owing money (1 if yes, 0 if no)?                                                     |
| Owe_Sum              | Number of argument positions taking an owe position. Either 0 (not a owing argument) or 1 (an owing argument).|
| Strict_Sum           | Number of argument positions taking the strictest position. Generally 0 (not a strictness argument) or 1 (one position is strictest), but it can be more than 1 if there is a multi-way argument and more than one Tana takes the strictest view. |
| Money_Sum            | Number of argument positions taking an money owing position. Generally 0 (not a money owing argument) or 1 (a money owing argument), but it can be more than 1 if there is a multi-way argument and more than one Tana takes the strictest view.  |


"

# Due to excel, Hebrew, and comma related complications, it was better to export in XLSX and then in 
# Excel convert (save as) UTF-8 CSV



# df_input$SeqID is the argument number
# #Write arguments file, one row per argument (a topic)
# write.xlsx(
#   df_scores2 %>% 
#     select(-Chapter_Name_Hebrew, -PsuedoDate), 
#   paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "arguments.xlsx")) 




#Write disputes file, one row per dispute (a pair of positions)
# DisputeID  is the dispute number
# write.xlsx(
#   df_scores7 %>% 
#     select(
#       DisputeID, SeqID, Disputant_1, Disputant_2,
#       Winner, Result_1, Result_2, Score_1, Score_2, 
#       AltMoney_1, AltMoney_2, Strict_Result_1, Strict_Result_2,
#       AltOwe_1, AltOwe_2,       
#       Strict_Result_Score_1, Strict_Result_Score_2,   
#       StrictQuestion, MoneyQuestion, OweQuestion,             
#       Tractate_Name_Hebrew, Chapter, ChapterRowKey, Location, HebrewChapterLocation, Ith_chapter_in_seder, EffectiveRows, tab, tabnum, 
#       Disputant_Hebrew_D1, Disputant_Hebrew_D1b, Disputant_Hebrew_D1c, Disputant_Hebrew_D2, Disputant_Hebrew_D2b, Disputant_Hebrew_D2c,
#       FileSource, MishnahKey, Seder_Hebrew, Seder_Name, Seder_Number, Seder_Translate
#       ), 
#   paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "disputes.xlsx")) 

# An argument contains many disputes. A dispute involves exactly 2 people (one or more potentially anonymous).
Table_For_Disputes_dot_CSV =   df_scores7 %>% 
    select(
  SeqID, DisputeID, Location,  EffectiveRows, tab, tabnum, Seder_Hebrew, Seder_Name, Seder_Number, Seder_Translate, Tractate_Name_Hebrew, Chapter, HebrewChapterLocation, Ith_chapter_in_seder, 
  Disputant_1, Disputant_Hebrew_D1, Disputant_Hebrew_D1b, Disputant_Hebrew_D1c,
  Disputant_2, Disputant_Hebrew_D2, Disputant_Hebrew_D2b, Disputant_Hebrew_D2c,
  StrictQuestion, MoneyQuestion, OweQuestion, Winner,
  Result_1, Score_1, AltOwe_1, AltMoney_1, AltStrict_1,
  Result_2, Score_2, AltOwe_2, AltMoney_2, AltStrict_2,
  Owe_Result_Score_1, Money_Result_Score_1, Strict_Result_1, Strict_Result_Score_1, 
  Owe_Result_Score_2, Money_Result_Score_2, Strict_Result_2, Strict_Result_Score_2, 
      )

# Once not exporting everything in quotes, need to fix the location commas to semi colins or error generated
Table_For_Disputes_dot_CSV$Location = sapply(Table_For_Disputes_dot_CSV$Location, gsub, pattern=",", replacement=";")

write_utf8_csv_noquotes(Table_For_Disputes_dot_CSV, paste0(OutputProjectPathStr, "disputes.csv")) 
  # An argument contains many disputes. A dispute involves exactly 2 people (one or more potentially anonymous).
"
Table for GitHub (Disputes (disputes.csv) Data Dictionary) as defined above in Table_For_Disputes_dot_CSV

| Field | Description |
|---|---|
| SeqID                | Unique argument ID.                                                                                           |
| DisputeID | A unique dispute ID. A single argument can contain multiple disputes. |
| Location | Combination of chapter and line number(s) within that chapter   where the dispute occurs.  |
| EffectiveRows | The number of disputes contained within the argument. All rabbis involved in a dispute are paired up with all possible other rabbis who are involved in the dispute but take a different position. |
| tab | The Hebrew name (and number of order) of the 63 masechtot in which the argument is appears. |
| tabnum | The number of the masechtot where the argument appears in. |
| Seder_Hebrew | Name of seder of the Talmud in which the argument appears, written in Hebrew. |
| Seder_Name | Name of seder of the Talmud in which the argument appears, Hebrew name in English transliteration. |
| Seder_Number | Which of the six seders of the Talmud the argument appears in. |
| Seder_Translate | English translation of which of the six seders of the Talmud   the argument appears in. |
| Tractate_Name_Hebrew | Which of the 63 masechtot / books of the Talmud where the   argument appears in, name in Hebrew. |
| Chapter | Which chapter number of the masechtot where the argument appears. |
| HebrewChapterLocation | Using Hebrew numbering, what is the chapter location of the dispute. |
| Ith_chapter_in_seder | Which chapter number of the seder where the argument appears. |
| Disputant_N | The rabbi(s) offering the nth position in the argument. |
| Disputant_Hebrew_DNX | Disputant number N's name in Hebrew, possibly in one of X   different versions. |
| OweQuestion | Is this a question about owing (1 if yes, 0 if no)? |
| MoneyQuestion | Is this a question about owing money (1 if yes, 0 if no)? |
| StrictQuestion | Is this a question about strictness (1 if yes, 0 if no)? |
| Winner | Which disputant(s), by name, won the argument according to Maimonides. |
| Result_N | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher number is stricter), BHB 1.0 / 0.0, Chayav 1.0 / Patur 0.0, and a few less common topics have separate scores. |
| Score_N | Disputant(s) N wins the argument according to Maimonides (1) or loses (0). |
| AltOwe_N | Does disputant(s) N say they owe money (1 if yes, 0 if no or not a money owing argument). |
| AltMoney_N | Does disputant(s) N say they owe money (1 if yes, 0 if no or not a money owing argument). |
| AltStrict_N | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher number is stricter), BHB 1.0 / 0.0 (lower is stricter).  |
|Owe_Result_Score_N     | Numerical portion of Result_N but only for 'Owe' related arguments.|
|Money_Result_Score_N   | Numerical portion of Result_N but only for 'Money' related arguments.|
|Strict_Result_Score_N  | Numerical portion of Result_N but only for 'Strictness' related arguments. See AltStrict_N definition for using this number.|
"



# write.xlsx(df_scores7 %>% 
#   filter(
#     Disputant_1 == "Rabbi Yossi" | Disputant_2 == "Rabbi Yossi" | 
#     Disputant_1 == "Rabb Yossi" | Disputant_2 == "Rabb Yossi", 
#     Result_1 == "Chumra 0.5" | Result_2 == "Chumra 0.5" |  
#     Result_1 == "Chayav 0.5" | Result_2 == "Chayav 0.5"| 
#     Result_1 == "Chumra 0.66"| Result_2 == "Chumra 0.66"| 
#     Result_1 == "Chumra 0.33"| Result_2 == "Chumra 0.33"|  
#     Result_1 == "BHB 0.5" | Result_2 == "BHB 0.5"
#     ), 
#   paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "Yossi.xlsx")
#   )



# Write copies file for Dani

#write.csv(df_scores_with_seder_info_copies_only, paste0(OutputProjectPathStr, SourceFile, "_", OutputFileRoot, "ListOfCopiedArguments", ".csv"), row.names = FALSE) 


# Save files for plotting
print("Saving the data")
# save(net_rabbi_undir_mink, l3b, ResultListWin, ResultListChumra, ResultListMoney, ResultListOwe, Big20List, Big15List, df_winrate_ciplot, 
#   df_elo_combined1_rnd_tidy, df_elo_combined2b_rnd_tidy, df_elo_combined3_rnd_tidy, df_elo_combined4_rnd_tidy, 
#   df_results_100, df_scores100, 
#   file="200922b_results.RData"
#   )
#net_rabbi_undir_mink, l3b
save( Big20List, Big15List,   
  df_results_100, df_scores100, 
  file=paste0(OutputProjectPathStr, OutputFileRoot, "results.RData") 
  )
print("The end")



