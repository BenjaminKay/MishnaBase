######################################################################################
################################### How do the methods compare in-sample?
######################################################################################

######################################################################################
################################### All rabbis, basic correlations 
######################################################################################

# David's score and Elo give very similar measures, win rate is very different 
df_results_100 %>% select(EloScoreWins, WinRate, DS.win) %>%  cor(use="complete.obs")
# David's score and Elo give very similar measures, win rate is very different 
df_results_100 %>% select(EloScoreChumra, WinChumraRate, DS.strict) %>%  cor(use="complete.obs")
# All measures about the same
df_results_100 %>% select(EloScoreMoney, WinMoneyRate, DS.money) %>%  cor(use="complete.obs")
# All measures about the same
df_results_100 %>% select(EloScoreOwe, WinOweRate, DS.owe) %>%  cor(use="complete.obs")

######################################################################################
################################### Top 25 rabbis, basic correlations 
######################################################################################

# All measures about the same
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(EloScoreWins, WinRate, DS.win) %>%  cor(use="complete.obs")
# All measures about the same
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(EloScoreChumra, WinChumraRate, DS.strict) %>%  cor(use="complete.obs")
# All measures about the same
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(EloScoreMoney, WinMoneyRate, DS.money) %>%  cor(use="complete.obs")
# All measures about the same
df_results_100 %>% filter(Top25Rabbi == TRUE) %>% select(EloScoreOwe, WinOweRate, DS.owe) %>%  cor(use="complete.obs")

######################################################################################
################################### Does order matter for Elo?
######################################################################################
print("Random reordering of elo to test dependency on order (slow)")

SampleNumbers = 200
tmp_resample <-EloScoreResampleResults(df_scores100, SampleNumbers)
df_elo_combined1_rnd_tidy  = tmp_resample$df_win
df_elo_combined2b_rnd_tidy = tmp_resample$df_chumra
df_elo_combined3_rnd_tidy  = tmp_resample$df_money
df_elo_combined4_rnd_tidy  = tmp_resample$df_owe

df_elo_combined1_rnd_comp <- df_elo_combined1_rnd_tidy %>% group_by(Name) %>% summarize(median_elo.win=median(EloScoreWins, na.rm=TRUE), mean_elo=mean(EloScoreWins, na.rm=TRUE))
df_elo_combined1_rnd_comp  <- left_join(df_elo_combined1_rnd_comp,  df_results_100 %>% select(Name, Top25Rabbi, n.disputes, n.wins, EloScoreWins, WinRate, DS.win, n.disputes), by=c("Name"))
df_elo_combined1_rnd_comp %>% select(median_elo.win, DS.win, EloScoreWins, WinRate) %>% cor(use="complete.obs")


df_elo_combined2b_rnd_comp <- df_elo_combined2b_rnd_tidy %>% group_by(Name) %>% summarize(median_elo.strict=median(EloScoreChumra, na.rm=TRUE), mean_elo=mean(EloScoreChumra, na.rm=TRUE))
df_elo_combined2b_rnd_comp  <- left_join(df_elo_combined2b_rnd_comp, df_results_100 %>% select(Name, EloScoreChumra, WinChumraRate, DS.strict, n.disputes, n.chumra), by=c("Name"))
df_elo_combined2b_rnd_comp %>% select(median_elo.strict, DS.strict, EloScoreChumra, WinChumraRate) %>% cor(use="complete.obs")


df_elo_combined3_rnd_comp <- df_elo_combined3_rnd_tidy %>% group_by(Name) %>% summarize(median_elo.money=median(EloScoreMoney, na.rm=TRUE), mean_elo=mean(EloScoreMoney, na.rm=TRUE))
df_elo_combined3_rnd_comp  <- left_join(df_elo_combined3_rnd_comp,  df_results_100 %>% select(Name, EloScoreMoney, WinMoneyRate, DS.money, n.disputes, n.money), by=c("Name"))
df_elo_combined3_rnd_comp %>% select(median_elo.money, DS.money, EloScoreMoney, WinMoneyRate) %>% cor(use="complete.obs")


df_elo_combined4_rnd_comp <- df_elo_combined4_rnd_tidy %>% group_by(Name) %>% summarize(median_elo.owe=median(EloScoreOwe, na.rm=TRUE), mean_elo=mean(EloScoreOwe, na.rm=TRUE))
df_elo_combined4_rnd_comp  <- left_join(df_elo_combined4_rnd_comp,  df_results_100 %>% select(Name, EloScoreOwe, WinOweRate, DS.owe, n.disputes, n.owe), by=c("Name"))
df_elo_combined4_rnd_comp %>% select(median_elo.owe, DS.owe, EloScoreOwe, WinOweRate) %>% cor(use="complete.obs")

# mem use
#object.size(df_elo_combined1_rnd_tidy)



######################################################################################
################################### Agresti-Coull CI Intervals
######################################################################################
print("Use Agresti-Coull method to construct standard errors for the win rate")
# Agresti–Coull interval
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti%E2%80%93Coull_interval
# Simple way is n~=n+4, x~=x+2, p=x~ / n~
# Normal approximation interval is p +- z score sqrt (p * (1-p)/n)
df_elo_combined1_rnd_comp <- df_elo_combined1_rnd_comp %>% mutate(	
  	p_win_AC_simp = (n.disputes*WinRate + 2)/(n.disputes+4),
  	sd_win_AC_simp = sqrt(p_win_AC_simp * (1-p_win_AC_simp) / n.disputes),
  	sd_win_simp = sqrt(WinRate * (1-WinRate) / n.disputes)  	
  ) %>% mutate(Name = fct_reorder(Name, p_win_AC_simp)) 

df_winrate_ciplot <- df_elo_combined1_rnd_comp  %>% 
	select(Name, Top25Rabbi, p_win_AC_simp , sd_win_AC_simp, sd_win_simp, WinRate) %>% 
	mutate(
  		U= pmin(p_win_AC_simp + 1.95*sd_win_AC_simp, 1),
  		L= pmax(p_win_AC_simp - 1.95*sd_win_AC_simp,0),
  		U2= pmin(WinRate + 1.95*sd_win_simp, 1),
  		L2= pmax(WinRate - 1.95*sd_win_simp, 0)  		
  		)  %>% arrange(p_win_AC_simp) 

df_winrate_ciplot %>% 
    ggplot(aes(x = Name, y = p_win_AC_simp)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = U, ymin = L)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_y_continuous(limits = c(0.0, 1.0)) +
    labs(x = "Name", y = "Win Rate", Title= "Agresti-Coull CI") 

df_winrate_ciplot %>% filter(Top25Rabbi) %>% 
    ggplot(aes(x = Name, y = p_win_AC_simp)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = U, ymin = L)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_y_continuous(limits = c(0.0, 1.0)) +
    labs(x = "Name", y = "Win Rate", title= "Agresti-Coull CI (Top 25)") 

# Non Agresti COull plots
df_winrate_ciplot %>% 
    ggplot(aes(x = Name, y = WinRate)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = U2, ymin = L2)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_y_continuous(limits = c(0.0, 1.0)) +
    labs(x = "Name", y = "Win Rate", Title= "Binomial CI") 

df_winrate_ciplot %>% filter(Top25Rabbi) %>%  
    ggplot(aes(x = Name, y = WinRate)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymax = U2, ymin = L2)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_y_continuous(limits = c(0.0, 1.0)) +
    labs(x = "Name", y = "Win Rate", Title= "Binomial CI (Top 25)") 

# df_results_100 %>% group_by(Top10Rabbi) %>% summarize(disputes_n=sum(n.disputes))
# df_results_100 %>% group_by(Top15Rabbi) %>% summarize(disputes_n=sum(n.disputes))


##################################################################################################################
################################### In Sample Prediction Accuracy
##################################################################################################################
print("Calculate in sample prediction accuracy")
# ResultListWin     <- PredictResults(df_elo_combined1_rnd_comp, df_scores100, 10, "WinnerName")

# ResultListChumra  <- PredictResults(df_elo_combined2b_rnd_comp, df_scores100, 10, "ChumraName")

# ResultListMoney   <- PredictResults(df_elo_combined3_rnd_comp, df_scores100, 10, "MoneyName")

# ResultListOwe     <- PredictResults(df_elo_combined4_rnd_comp, df_scores100, 10, "OweName")

ListOfWinMeasures = c("Score", "AltStrict", "Money", "Owe")
ListOfDomMeasures_Win = c("WinRate", "DS.win", "EloScoreWins", "n.disputes")
TolWin = c(.03, 4, 16, 5) # TolWin = c(0, 0, 0, 0)
i  = 1
print("part 1")
for (dommeasure in ListOfDomMeasures_Win) {   
  if (i==1) {
      df_eval.win = NewPredictResultsFullSample(df_results_100, df_scores100, dommeasure, "Score", TolWin[i]) 
      df_eval.win_by_rabbi = ForecastAccuracyByRabbi(df_eval.win, paste0(dommeasure, "_PredScore"))
      df_eval.win_excl = NewPredictResultsFullSample(df_results_100, df_scores100 %>% filter(Score_1 !=Score_2) , dommeasure, "Score", TolWin[i])
    } else {
      df_eval.win = left_join(
        df_eval.win,
        NewPredictResultsFullSample(df_results_100, df_scores100, dommeasure, "Score", TolWin[i]) %>% select(1,6),
        by="DisputeID"
        )
      df_eval.win_by_rabbi = left_join(
        df_eval.win_by_rabbi,
        ForecastAccuracyByRabbi(df_eval.win, paste0(dommeasure, "_PredScore")),
        by="Disputant"
        )
      df_eval.win_excl = left_join(
        df_eval.win_excl,
        NewPredictResultsFullSample(df_results_100, df_scores100 %>% filter(Score_1 !=Score_2) , dommeasure, "Score", TolWin[i]) %>% select(1,6),
        by="DisputeID"
        )      
    }
  i = i + 1    
}




# df_disagree_win = df_eval.win %>% filter(
#   !((WinRate_PredAcc == DS.win_PredAcc) & (WinRate_PredAcc == EloScoreWins_PredAcc) &(WinRate_PredAcc == n.disputes_PredAcc)))


# df_eval.win = NewPredictResultsFullSample(df_results_100, df_scores100, "EloScoreWins", "Score", 0) 


# df_eval.strict = NewPredictResultsFullSample(df_results_100, df_scores100, "WinChumraRate", "AltStrict", 0) 
# df_eval.money = NewPredictResultsFullSample(df_results_100, df_scores100, "WinMoneyRate", "Money", 0) 
# df_eval.owe = NewPredictResultsFullSample(df_results_100, df_scores100, "WinOweRate", "Owe", 0) 

df_winscores = df_scores100 %>% select(Disputant_1, Disputant_2, Score_1, Score_2)
df_winscores = left_join(df_winscores, df_results_100, by=c("Disputant_1"="Name") )
df_winscores = left_join(df_winscores, df_results_100, by=c("Disputant_2"="Name"), suffix=c(".1", ".2") )

df_winscores$MultinomialScore = as.factor(df_winscores$Score_1 - df_winscores$Score_2)
df_winscores$BinomialScore = as.factor(df_winscores$Score_1)



df_winscores$DS.win.diff = scale(df_winscores$DS.win.1 - df_winscores$DS.win.2)
df_winscores$WinRate.diff = scale(df_winscores$WinRate.1 - df_winscores$WinRate.2)
df_winscores$n.disputes.diff = scale(df_winscores$n.disputes.1 - df_winscores$n.disputes.2)
df_winscores$EloScoreWins.diff = scale(df_winscores$EloScoreWins.1 - df_winscores$EloScoreWins.2)
df_winscores$n.disputes.log.diff = log(df_winscores$n.disputes.1) - log( df_winscores$n.disputes.2)

df_winscores$StdAvgMeasure.diff = 0.25 + (
  df_winscores$DS.win.diff +
  df_winscores$WinRate.diff +
  df_winscores$n.disputes.diff 
  )

df_winscores$StdAvgMeasure.diff
df_winscores_excl = df_winscores %>% filter(Score_1 !=Score_2) 

m <- polr(MultinomialScore ~ DS.win.diff + WinRate.diff + n.disputes.diff + EloScoreWins.diff, data = df_winscores, Hess=TRUE)
summary(m)

m2 <- polr(MultinomialScore ~  StdAvgMeasure.diff, data = df_winscores, Hess=TRUE)
summary(m2)

m3 <- polr(MultinomialScore ~  WinRate.diff, data = df_winscores, Hess=TRUE)
summary(m3)

m4 <- polr(MultinomialScore ~  WinRate.diff + DS.win.diff, data = df_winscores, Hess=TRUE)
summary(m4)

m5 <- polr(MultinomialScore ~  EloScoreWins.diff + DS.win.diff, data = df_winscores, Hess=TRUE)
summary(m5)


mb <- polr(MultinomialScore ~ DS.win.diff + WinRate.diff + n.disputes.diff + EloScoreWins.diff, data = df_winscores_excl, Hess=TRUE)
summary(mb)

mb2 <- polr(MultinomialScore ~  StdAvgMeasure.diff, data = df_winscores_excl, Hess=TRUE)
summary(mb2)

mb3 <- polr(MultinomialScore ~  WinRate.diff, data = df_winscores_excl, Hess=TRUE)
summary(mb3)
mb3_pred = predict(mb3, df_winscores_excl)
table(mb3_pred , df_winscores_excl$MultinomialScore)



mb4 <- polr(MultinomialScore ~  WinRate.diff + DS.win.diff, data = df_winscores_excl, Hess=TRUE)
summary(mb4)

mb5 <- polr(MultinomialScore ~  EloScoreWins.diff + DS.win.diff, data = df_winscores_excl, Hess=TRUE)
summary(mb5)

mb6 <- polr(MultinomialScore ~  DS.win.diff + WinRate.diff + EloScoreWins.diff, data = df_winscores_excl, Hess=TRUE)
summary(mb6)
mb6_pred = predict(mb6, df_winscores_excl)
table(mb6_pred, df_winscores_excl$MultinomialScore)



mc3 <- glm(BinomialScore ~ WinRate.diff, family = binomial(link = "probit"), data = df_winscores_excl)
summary(mc3)
mc3_pred = (predict(mc3, df_winscores_excl) >= 0.5) * 1
table(mc3_pred , df_winscores_excl$BinomialScore)



mc6 <- glm(BinomialScore ~  DS.win.diff + WinRate.diff + EloScoreWins.diff, family = binomial(link = "probit"), data = df_winscores_excl)
summary(mc6)
mc6_pred = (predict(mc6, df_winscores_excl) >= 0.5) * 1
table(mc6_pred, df_winscores_excl$BinomialScore)



mc7 <- glm(BinomialScore ~  DS.win.diff * WinRate.diff * EloScoreWins.diff * n.disputes.log.diff, family = binomial(link = "probit"), data = df_winscores_excl)
summary(mc7)
mc7_pred = (predict(mc7, df_winscores_excl) >= 0.5) * 1
table(mc7_pred, df_winscores_excl$BinomialScore)


lambdas <- 10^seq(2, -3, by = -.1)
# Alpha  = 0 is ridge
# cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
# optimal_lambda <- cv_ridge$lambda.min
# optimal_lambda

# Setting alpha = 1 implements lasso regression
lasso_reg <- glmnetUtils::cv.glmnet(BinomialScore ~  DS.win.diff * WinRate.diff * EloScoreWins.diff, 
  data = df_winscores_excl, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5, family="binomial")

small.lambda.betas <- coef(lasso_reg, s = "lambda.min")
#small.lambda.betas <- plot(lasso_reg, s = "lambda.min")
