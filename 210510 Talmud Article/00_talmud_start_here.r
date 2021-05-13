# source("C:/Users/yakne/OneDrive/Documents/talmud_elo/210510 Talmud Article/00_talmud_start_here.r")
setwd("C:/Users/yakne/OneDrive/Documents/talmud_elo/")
ProjectPathStr = "C:/Users/yakne/OneDrive/Documents/talmud_elo/210510 Talmud Article/"
SourceFile = "Arguments in the Mishna 2021_03_31.xlsx"
OutputFileRoot = "210510_"

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
library("elo") # https://cran.r-project.org/web/packages/elo/elo.pdf
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

YesCullCullNonWinners = FALSE 
source(paste0(ProjectPathStr, "07_talmud_results.r"))

df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreWins, WinRate, DS.win) 
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreChumra, WinChumraRate, DS.strict) 
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreMoney, WinMoneyRate, DS.money) 
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(Name, Top25Rabbi, EloScoreOwe, WinOweRate, DS.owe) 

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
# Name of Tana
# Number of arguments that are categorized as Chumra/Kula (strict)
# Number of arguments that are categorized as BHB 0/BHB 1. (money)
# Number of arguments that are categorized as Chayav/Patur. (owe)
# Percentage of arguments of 2 where the result is Chumra
# Percentage of arguments of 3 where the result is BHB 0
# Percentage of arguments of 4 where the result is Chayav   
# Top25PrettyTable = df_results_9 %>% filter(Top25Rabbi==TRUE) %>% 
#     select(
#     Name, 
#     UniqueQuestions, 
#     UniqueChumraQuestions, 
#     UniqueMoneyQuestions, 
#     UniqueOweQuestions, 
#     WinRate, 
#     WinChumraRate, 
#     WinMoneyRate,
#     WinOweRate,
#     WinRate_ignore_ties, 
#     WinChumraRate_ignore_ties, 
#     WinMoneyRate_ignore_ties,
#     WinOweRate_ignore_ties
#     ) %>%
#   mutate(
#     WinRate = round(WinRate, 2),
#     WinChumraRate = round(WinChumraRate, 2),
#     WinMoneyRate = round(WinMoneyRate, 2),    
#     WinOweRate = round(WinOweRate, 2),
#     WinRate_ignore_ties = round(WinRate_ignore_ties, 2),
#     WinChumraRate_ignore_ties = round(WinChumraRate_ignore_ties, 2),
#     WinMoneyRate_ignore_ties = round(WinMoneyRate_ignore_ties, 2),    
#     WinOweRate_ignore_ties = round(WinOweRate_ignore_ties, 2)       
#   )
# colnames(Top25PrettyTable) = c(
#   "Tana", 
#   "All Arguments (Count)", "Chumra Arguments (Count)", "BHB Arguments (Count)", "Chayav Arguments (Count)",   
#   "All Arguments (Win Rate)", "Chumra Arguments (Win Rate)", "BHB Arguments (Win Rate)", "Chayav Arguments (Win Rate)",
#   "All Arguments (Win Rate, No Ties)", "Chumra Arguments (Win Rate, No Ties)", "BHB Arguments (Win Rate, No Ties)", "Chayav Arguments (Win Rate, No Ties)"
#   )

Top25PrettyTableUnique = df_results_9 %>% filter(Top25Rabbi==TRUE) %>% 
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
    WinChumraExMonRate,
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
colnames(Top25PrettyTableUnique) = c(
  "Tana", #1
  "All Arguments (Unique Count)", "Chumra Arguments (Unique Count)","BHB Arguments (Unique Count)", "Chayav Arguments (Unique Count)",
  "All Arguments (Win Rate)", "Chumra Arguments (Win Rate)", "BHB Arguments (Win Rate)", "Chayav Arguments (Win Rate)", "Chumra ex BHB Arguments (Win Rate)",
  "All Arguments (Win Rate, No Ties)", "Chumra Arguments (Win Rate, No Ties)", "BHB Arguments (Win Rate, No Ties)", "Chayav Arguments (Win Rate, No Ties)", "Chumra ex BHB Arguments (Win Rate, No Ties)"  
  )
# write.excel(Top25PrettyTableUnique)


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

#########################################################################################
# Write output files
#########################################################################################
print("Write Output Files")
# Matrix of connections data
# Wins or Losses (no ties, undirected)
#write.csv(dom_mat_disputes3, "210411_Top25LinkMatrix.csv", row.names = FALSE)
write.csv(dom_mat_disputes3, paste0(SourceFile, "_", OutputFileRoot, "Top25LinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 


# Matrix of wins data (excludes ties and losses (directed))
#write.csv(dom_mat_wins4, "210411_Top25WinLinkMatrix.csv", row.names = FALSE)
write.csv(dom_mat_wins4, paste0(SourceFile, "_", OutputFileRoot, "Top25WinLinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 


# Matrix of strictness wins (excludes ties and losses (directed))
#write.csv(dom_mat_strict4, "210411_Top25StrictWinLinkMatrix.csv", row.names = FALSE)
write.csv(dom_mat_strict4, paste0(SourceFile, "_", OutputFileRoot, "Top25StrictWinLinkMatrix_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 


write.csv(Top25PrettyTableUnique, paste0(SourceFile, "_", OutputFileRoot, "Top25PrettyTableUnique_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 
# write.csv(Top25PrettyTable,       paste0(SourceFile, "_", OutputFileRoot, "Top25PrettyTableNotUnique_Cull", YesCullCullNonWinners,".csv"), row.names = FALSE) 

# Dispute perforamnce data data, which is individual disputes 
# (recall that df_results holds the actual rating data)
write.xlsx(df_scores6e, paste0(OutputFileRoot,  "df_scores6e.xlsx"))
write.xlsx(df_scores7, paste0(OutputFileRoot,   "df_scores7.xlsx")) 


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
  file=paste0(OutputFileRoot, "results.RData") 
  )
print("The end")

