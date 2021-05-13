#########################################################################################
####################################  Functions
#########################################################################################
print("Load user functions")
`%ni%` <- Negate(`%in%`) 

# Like excel functions of same name
left = function(text, num_char) {
  substr(text, 1, num_char)
}
 
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
 
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  #http://datacornering.com/how-to-copy-the-r-data-frame-to-excel-and-customize-your-startup/
  write.table(x,"clipboard-20000",sep="\t",row.names=row.names,col.names=col.names,...)
}

rename_column <- function(df, old_name_str, new_name_str){
    colnames(df)[colnames(df)==old_name_str] <- new_name_str
    return(df)}  

smart_df_fillna <- function(df, PrintDetails = FALSE) {
  # Fill in each field in a DF based on the data type
  #as.data.frame is just in case, because sometimes the dlypr constructs have the wrong type
  df_new <- as.data.frame(df)
  for (i in names(df_new)) {
    classtmp <- class(df_new[ ,i])[1]
    if (classtmp == "character" ) {
      #print(c(i, classtmp))
      df_new[ ,i][is.na(df_new[ ,i])] <- "Missing"
    } else if ( classtmp == "numeric" || classtmp == "integer") {
      #print(c(i, classtmp))
      df_new[ ,i][is.na(df_new[ ,i])] <- 0
    } else if (classtmp == "logical") {
      #print(c(i, classtmp))
      df_new[ ,i][is.na(df_new[ ,i])] <- FALSE
    } else if ( classtmp == "POSIXct" || classtmp == "POSIXt") {
      #print(c(i, classtmp))
      df_new[ ,i][is.na(df_new[ ,i])] <- anytime("01/01/1901")
    }
    if (PrintDetails) {
      print(c(i, classtmp))
    }
  }
  return(df_new)
}

make0s = function(inputstr, maxlen){
    #make0s("ham")
    tmplength   = str_length(inputstr)
    zerostr     = paste(replicate(maxlen-tmplength, "0"), collapse="")
    return(paste0(zerostr, inputstr))}

make0s2 = function(inputstr){
    return(make0s(inputstr, 2))}

make0s3 =  function(inputstr){
    return(make0s(inputstr, 3))}

make0s5 =  function(inputstr){
    return(make0s(inputstr, 5))}

PrintIthRowLong = function(df_input, ii){
    #print the ith row of a dataframe for all fields along with field names
    if  (is.data.table(df_input)){       
        for (ij in colnames(df_input)) {
            # Remove leading brackets in print by using cat instead of print
            # print(df_input[ii, ij])
            # ..ij allows data frame reference by string instead of assuming ij is the field we want
            cat(paste(ij, ": ", df_input[ii, ..ij], "\n"))
        }       
    } else if  (is.data.frame(df_input)) {       
        for (ij in colnames(df_input)) {           
            # Remove leading brackets in print by using cat instead of print
            # print(df_input[ii, ij])           
            cat(paste(ij, ": ", df_input[ii, ij], "\n"))
        }   
    } else {
        print("Not a data table or frame")
    }
   }

IsNumeric<- function(x){
  # https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
  return(!is.na(as.numeric(x)))
}

# Summary Stats. This "get" setup lets you pass the variable as a field to dpylr

EdaTable <- function(df, fieldasstr){
  #print(fieldasstr)
  df_out <- df[fieldasstr] %>% summarize(
    Field = fieldasstr,
    Pct01 = quantile(get(fieldasstr), probs=0.01, na.rm=TRUE),
    Pct05 = quantile(get(fieldasstr), probs=0.05, na.rm=TRUE),
    Pct10 = quantile(get(fieldasstr), probs=0.10, na.rm=TRUE),
    Pct25 = quantile(get(fieldasstr), probs=0.25, na.rm=TRUE),
    Pct50 = quantile(get(fieldasstr), probs=0.50, na.rm=TRUE),
    Pct75 = quantile(get(fieldasstr), probs=0.75, na.rm=TRUE),
    Pct90 = quantile(get(fieldasstr), probs=0.90, na.rm=TRUE),
    Pct95 = quantile(get(fieldasstr), probs=0.95, na.rm=TRUE),
    Pct99 = quantile(get(fieldasstr), probs=0.99, na.rm=TRUE),
    Mean  = mean(get(fieldasstr), na.rm=TRUE),
    Count = n(),
    NonNA =  sum(!is.na(get(fieldasstr))),
    StdDev= sd(get(fieldasstr), na.rm=TRUE),
    )
  return(df_out) }

# Take a talmud address and parse to a list of addresses
ParseRefList <- function(input_key){
  input_parts <- str_split(input_key, ", ", simplify = TRUE)
  n = dim(input_parts)[2]
  i = 1
  output_keys = list()
  for (input_part in input_parts){
    if (grepl("-", input_part)){
      input_part_parts = str_split(input_part, "-", simplify = TRUE)
      m = dim(input_part_parts)[2]
      EndNumber = as.numeric(input_part_parts[2])
      StartNumber = as.numeric(str_split(input_part_parts[1], ":", simplify = TRUE)[2] )
      Chapter = as.numeric(str_split(input_part_parts[1], ":", simplify = TRUE)[1] )
      # output_keys = c(output_keys, )
      #print(paste(Chapter, StartNumber, EndNumber))
      tmpkeys = sprintf(paste(Chapter, ":%s", sep="" ), (StartNumber:EndNumber))
      #print(tmpkeys)
      output_keys[(i:(i+EndNumber-StartNumber))] = tmpkeys
      i = i + (EndNumber - StartNumber + 1)
    } else {
      output_keys[i] = input_part 
      i = i+1
    }
  }
  #output_keys <- strsplit(input_key, ";")
  return(output_keys)
}

# Given a df of the mishna, a tab, and a tab name and input keys, match the mishna to the other data frame
PullMishnaLines <- function(df_mishna_input, tab, input_keys){
  # inputkeys = ParseRefList("10:2-6, 5:6-9")
  # tabtmp = "כלאיים4"
  # input_keys = ParseRefList("2:10")
  # input_keys = ParseRefList("2:10-11, 5:4-8")
  # df_mishna_input = df_mishna1
  # df_out_tmp <- PullMishnaLines(df_mishna_input, tabtmp, input_keys)  
  df_tmp = as.data.frame(as.matrix(input_keys))
  colnames(df_tmp) = "NumberChapterLocation"
  df_tmp$ExcelTabName = tab
  df_tmp$NumberChapterLocation= as.character(df_tmp$NumberChapterLocation)
  df_return = left_join(df_tmp, df_mishna_input, by=c("ExcelTabName", "NumberChapterLocation"))
  return(df_return)
}

# Assign as evenly as possible all data to one of k groups
AssignKfold <- function(df_input, k_folds){
  n_obs = dim(df_input)[1]
  #k_folds = 5
  k_fold_assignments = 1:n_obs %% k_folds + 1
  k_fold_assignments = sample(k_fold_assignments)
  df_input$k_fold <- k_fold_assignments
  return(df_input)
}

KSampleResults <- function  (df_scores_input, numberfolds, reps, df_old_results) {
  df_tmp____win <- data.frame("Name"=unique(df_old_results$Name))
  df_tmp____win$permutation = "EloScoreWins" 
  df_tmp____win_tmp <- df_tmp____win

  for (k in 2:reps){
    df_tmp____win_tmp$permutation = paste("EloScoreWins", k, sep="") 
    df_tmp____win = rbind(df_tmp____win, df_tmp____win_tmp)
  }

  for (j in 1:numberfolds){
    # df_scores_tmp_rnd_tmp <- df_scores_input[sample(nrow(df_scores_input)*samplefrac, replace=FALSE),]
    # Recall fold n excludes the n block of data and includes the remainder
    df_scores_tmp_rnd_tmp  <- df_scores_input %>% filter(k_fold!=j)
    #print(paste("Size of source data:", dim(df_scores_tmp_rnd_tmp)), digits=1)
    ListReturn = EloScoreResampleResults(df_scores_tmp_rnd_tmp, reps)
    #print(head(df_scores_input$Name))
    #print(head(ListReturn$df_win$Name))
    #print(paste("j:", j))
    #print(paste("Size of df_win:", dim(ListReturn$df_win)), digits=1)
    if (j==1) {
      df_tmp____win <- left_join(df_tmp____win, ListReturn$df_win,   by=c("Name", "permutation"), suffix = c("", as.character(j)))
      #df_tmp_chumra <- ListReturn$df_chumra
      #df_tmp__money <- ListReturn$df_money
    } else {
      colnames(ListReturn$df_win) = c("Name", "permutation", paste("EloScoreWins", j, sep=""), paste("n", j, sep=""))
      df_tmp____win <-  left_join(df_tmp____win, ListReturn$df_win,    by=c("Name", "permutation"))
      #df_tmp_chumra <- left_join(df_tmp_chumra, ListReturn$df_chumra, by="Name", suffix = c(as.character(j-1), as.character(j)))
      #df_tmp__money <- left_join(df_tmp__money, ListReturn$df_money,  by="Name", suffix = c(as.character(j-1), as.character(j)))
    }

    #print("Size of combined file:")
    #print(dim(df_tmp____win))
  }
  # return(list("df_win"=df_tmp____win, "df_chumra"=df_tmp_chumra, "df_money"=df_tmp__money))
  return(df_tmp____win)
}
# df_elo_combined1_rnd_tmp_tidy  <- df_elo_combined1_rnd_tmp  %>% gather(permutation, EloScoreWins, -Name)

# This function determines the question type (money / owe / strict)
# It also assigns 1 / 0 for winning 
AssignScores <- function(df_input){
  df_input$SeqID <- seq.int(nrow(df_input)) # This is an argument number (so sharred among multi-way arguments)
  df_input$PsuedoDate = mdy("1/1/1000") + df_input$SeqID #Note that this date won't look good in excel
  df_input$keep = TRUE

  # If your name is the same name as the winner, then you get one point, otherwise zero.
  df_input$Score_1 <- (df_input$Disputant_1 == df_input$Winner & !is.na(df_input$Disputant_1)) * 1
  df_input$Score_2 <- (df_input$Disputant_2 == df_input$Winner & !is.na(df_input$Disputant_2)) * 1
  df_input$Score_3 <- (df_input$Disputant_3 == df_input$Winner & !is.na(df_input$Disputant_3)) * 1
  df_input$Score_4 <- (df_input$Disputant_4 == df_input$Winner & !is.na(df_input$Disputant_4)) * 1    
  df_input$Score_5 <- (df_input$Disputant_5 == df_input$Winner & !is.na(df_input$Disputant_5)) * 1           
  df_input$Score_6 <- (df_input$Disputant_6 == df_input$Winner & !is.na(df_input$Disputant_6)) * 1           

  # If your opinion is chumra you get one point otherwise zero 
  # New way, BHB or Chumra ( BHB 1 is treated as Kula and BHB 0 is treated as Chumra.)
  df_input$Strict_1 <- ((df_input$Result_1 == "Chumra 1.0") | (df_input$Result_1 == "BHB 0.0")) * 1
  df_input$Strict_2 <- ((df_input$Result_2 == "Chumra 1.0") | (df_input$Result_2 == "BHB 0.0")) * 1
  df_input$Strict_3 <- ((df_input$Result_3 == "Chumra 1.0") | (df_input$Result_3 == "BHB 0.0")) * 1
  df_input$Strict_4 <- ((df_input$Result_4 == "Chumra 1.0") | (df_input$Result_4 == "BHB 0.0")) * 1
  df_input$Strict_5 <- ((df_input$Result_5 == "Chumra 1.0") | (df_input$Result_5 == "BHB 0.0")) * 1
  df_input$Strict_6 <- ((df_input$Result_6 == "Chumra 1.0") | (df_input$Result_6 == "BHB 0.0")) * 1

  # Landlord money issues (BHB)
  df_input$Money_1 <- (df_input$Result_1 == "BHB 1.0") * 1
  df_input$Money_2 <- (df_input$Result_2 == "BHB 1.0") * 1
  df_input$Money_3 <- (df_input$Result_3 == "BHB 1.0") * 1    
  df_input$Money_4 <- (df_input$Result_4 == "BHB 1.0") * 1
  df_input$Money_5 <- (df_input$Result_5 == "BHB 1.0") * 1      
  df_input$Money_6 <- (df_input$Result_6 == "BHB 1.0") * 1   

  # Does owe money (high) / Chayav
  # Don't owe money (low) / Patur      
  df_input$Owe_1 <- (df_input$Result_1 == "Chayav 1.0") * 1
  df_input$Owe_2 <- (df_input$Result_2 == "Chayav 1.0") * 1
  df_input$Owe_3 <- (df_input$Result_3 == "Chayav 1.0") * 1    
  df_input$Owe_4 <- (df_input$Result_4 == "Chayav 1.0") * 1
  df_input$Owe_5 <- (df_input$Result_5 == "Chayav 1.0") * 1
  df_input$Owe_6 <- (df_input$Result_6 == "Chayav 1.0") * 1

  df_input$Strict_1[is.na(df_input$Strict_1)] <- 0
  df_input$Strict_2[is.na(df_input$Strict_2)] <- 0
  df_input$Strict_3[is.na(df_input$Strict_3)] <- 0
  df_input$Strict_4[is.na(df_input$Strict_4)] <- 0
  df_input$Strict_5[is.na(df_input$Strict_5)] <- 0
  df_input$Strict_6[is.na(df_input$Strict_6)] <- 0

  df_input$Money_1[is.na(df_input$Money_1)] <- 0
  df_input$Money_2[is.na(df_input$Money_2)] <- 0
  df_input$Money_3[is.na(df_input$Money_3)] <- 0
  df_input$Money_4[is.na(df_input$Money_4)] <- 0
  df_input$Money_5[is.na(df_input$Money_5)] <- 0
  df_input$Money_6[is.na(df_input$Money_6)] <- 0

  df_input$Owe_1[is.na(df_input$Owe_1)] <- 0
  df_input$Owe_2[is.na(df_input$Owe_2)] <- 0
  df_input$Owe_3[is.na(df_input$Owe_3)] <- 0
  df_input$Owe_4[is.na(df_input$Owe_4)] <- 0
  df_input$Owe_5[is.na(df_input$Owe_5)] <- 0  
  df_input$Owe_6[is.na(df_input$Owe_6)] <- 0  

  # For error checking
  df_input <- df_input %>% 
    mutate(Strict_Sum = Strict_1 + Strict_2 + Strict_3 + Strict_4 + Strict_5 + Strict_6) 
  df_input <- df_input %>% 
    mutate(Money_Sum = Money_1 + Money_2 + Money_3 + Money_4 + Money_5 + Money_6) 
  df_input <- df_input %>% 
    mutate(Owe_Sum = Owe_1 + Owe_2 + Owe_3 + Owe_4 + Owe_5 + Owe_6) 
  # df_input$StrictQuestion <- ifelse(grepl("Chumra",df_input$Result_1), 1,
  #                                    ifelse(grepl("Kula",df_input$Result_1), 1, 0))
  df_input <- df_input %>% mutate(StrictQuestion = if_else(Strict_Sum>=1, 1, 0))
  df_input$AltStrictQuestion  <- df_input$StrictQuestion
  # df_input$OweQuestion <- ifelse(grepl("Chayav",df_input$Result_1), 1,
  #                                    ifelse(grepl("Patur",df_input$Result_1), 1, 0))
  df_input <- df_input %>% mutate(OweQuestion = if_else(Owe_Sum>=1, 1, 0))
  # df_input$MoneyQuestion <- ifelse(grepl("BHB",df_input$Result_1), 1, 0)    
  df_input <- df_input %>% mutate(MoneyQuestion = if_else(Money_Sum>=1, 1, 0))

  # Before, you could be coded as a zero even if it was all missing. Now this checks that there is at least one non-missing of that question type 
  # If not, overwrites to NA instead of zero. 
  # df_input <- df_input %>% mutate(Strict_Sum = Strict_1 + Strict_2 + Strict_3 + Strict_4 + Strict_5) %>% mutate_at(c("Strict_1", "Strict_2", "Strict_3", "Strict_4", "Strict_5"), )
  # https://stackoverflow.com/questions/55260462/mutate-multiple-variables-based-on-a-given-condition
  df_input <- df_input %>% 
      mutate_at(vars(Strict_1, Strict_2, Strict_3, Strict_4, Strict_5, Strict_6), list(~ ifelse(Strict_Sum < 1, NA, .)))
  df_input <- df_input %>% 
      mutate_at(vars(Money_1, Money_2, Money_3, Money_4, Money_5, Money_6), list(~ ifelse(Money_Sum <  1, NA, .)))
  df_input <- df_input %>% 
      mutate_at(vars(Owe_1, Owe_2, Owe_3, Owe_4, Owe_5, Owe_6), list(~ ifelse(Owe_Sum <  1, NA, .)))  
  # df_input <- df_input %>% select(-Strict_Sum, -Money_Sum, -Owe_Sum)                
  return(df_input)      
}

CountDisputants <- function(df_input){
  max_disputants = 6
  df_input$NumDisp <- max_disputants - as.numeric(rowSums(is.na(df_input %>% select(Disputant_1, Disputant_2, Disputant_3, Disputant_4, Disputant_5, Disputant_6) )))
  #df_input$NumDisp <- as.numeric(df_input$NumDisp)
  return(df_input)
}

FilterBadRows <- function(df_input){
  # Cull zero disputes
  df_input2 <- df_input %>% filter(NumDisp <= 6, NumDisp>=2)
  # Could add other stuff later
  df_output <- df_input2
  return(df_output)
}

# Ungroup names and create additional rows
UngroupNames <- function (df_input){
  # If an opinion is shared then we need to duplicate the row but put one name from the split in each row
  # This has to be done for each disputant column. 
  #This one does one
  for(i in 1:nrow(df_input)) {
      Disputant_1 = unlist(df_input[i,]$Disputant_1)
      if (grepl("/", Disputant_1, fixed=TRUE)) {
          name1 = (unlist(strsplit((Disputant_1), "/")))[1]
          name2 = (unlist(strsplit((Disputant_1), "/")))[2]

          df_input[nrow(df_input) + 1,] = df_input[i,]

          df_input[i,]$Disputant_1  <- name1
          df_input[nrow(df_input), ]$Disputant_1  <-name2      
      }
  }
  # This one does two 
  for(i in 1:nrow(df_input)) {
      # row <- df_input[i,]
      Disputant_2 = unlist(df_input[i,]$Disputant_2)
      if (grepl("/", Disputant_2, fixed=TRUE)) {
          name1 = (unlist(strsplit(Disputant_2, "/")))[1]
          name2 = (unlist(strsplit(Disputant_2, "/")))[2]

          df_input[nrow(df_input) + 1,] = df_input[i,]

          df_input[i,]$Disputant_2  <- name1
          df_input[nrow(df_input), ]$Disputant_2  <-name2        
      }
  }
  df_input$DisputeID <- seq.int(nrow(df_input)) # Not unique to an argument, unique to an argument-disputant1 - disputant 2 tripple 
  return(df_input)
}
# Makes all disputes into two dispute setups
HandleDuplicates <- function(df_input){
  # Copy the data structure but not the values of the input frame. 
  df_output = df_input[0, ]
  input__rows = nrow(df_input) 
  output_rows = 0
  for(ik in 1:input__rows) { # For each row
    # print(ik)
    # print(df_input$SeqID[ik]) 
    maxval = df_input$NumDisp[ik]
    if (!is.numeric(maxval)){
      maxval=2.0
    }
    replacerowscount = maxval * ((maxval) - 1) / 2
    # print(paste(!is.numeric(maxval), df_input$NumDisp[ik], deparse(maxval), class(maxval), ik))       
    # print(paste(ik, maxval, df_input$SeqID[ik]))
    
    if (maxval == 2){
      # If standard two way dispute, just copy the line over      
      df_output[output_rows + 1, ] = df_input[ik,]    
      output_rows = output_rows + 1
    } else if (maxval >  2) { # If number of disputants is three or more...
      # Make the number of required new rowsbased on argument pairs
      df_output[(output_rows+1): (output_rows + replacerowscount),] = df_input[ik,]

      il = 1
      for (ii in 1:(maxval-1)) { #need to loop through one time fewer than the number of disputes 
        for (ij in ii:maxval){
          if (ii!=ij){ # Make sure you don't compare a rabbi to themselves
              # Duplicate the fields as needed into just disputant one and two fields
              Disputant_Field1_Str <- paste("Disputant", ii, sep="_")
              Strict____Field1_Str <- paste("Strict",    ii, sep="_")
              Money_____Field1_Str <- paste("Money",     ii, sep="_")
              Score_____Field1_Str <- paste("Score",     ii, sep="_")

              Disputant_Field2_Str <- paste("Disputant", ij, sep="_")
              Strict____Field2_Str <- paste("Strict",    ij, sep="_")
              Money_____Field2_Str <- paste("Money",     ij, sep="_")
              Score_____Field2_Str <- paste("Score",     ij, sep="_")  

              df_output[output_rows + il, ]$Disputant_1  <- df_input[ik, Disputant_Field1_Str]
              df_output[output_rows + il, ]$Disputant_2  <- df_input[ik, Disputant_Field2_Str]

              df_output[output_rows + il, ]$Strict_1  <- df_input[ik, Strict____Field1_Str]
              df_output[output_rows + il, ]$Strict_2  <- df_input[ik, Strict____Field2_Str]

              df_output[output_rows + il, ]$Money_1  <- df_input[ik, Money_____Field1_Str]
              df_output[output_rows + il, ]$Money_2  <- df_input[ik, Money_____Field2_Str]

              df_output[output_rows + il, ]$Score_1  <- df_input[ik, Score_____Field1_Str]
              df_output[output_rows + il, ]$Score_2  <- df_input[ik, Score_____Field2_Str]

              df_output[output_rows + il, ]$Disputant_1  <- df_input[ik, Disputant_Field1_Str]
              df_output[output_rows + il, ]$Disputant_2  <- df_input[ik, Disputant_Field2_Str] 

              il = il + 1            
          }
        }
      }
      output_rows = output_rows + replacerowscount  
    }
  }
  #Clean up the rest
  return(df_output)
}


Clean_Results <- function (df_input, input_column_str){
  # df_tmp <- Clean_Results(head(df_scores), "Result_1")
  #df_input[paste(input_column_str, "bak", sep="_")] <- df_input[input_column_str]
  
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "[Ww]oman [0-9]*.*[0-9]", "")
  df_input[input_column_str] <-  str_replace_all(df_input[,input_column_str], "[Ss]lave [0-9]*.*[0-9]", "")
  df_input[input_column_str] <-  str_replace_all(df_input[,input_column_str], "=", "")
  df_input[input_column_str] <-  str_replace_all(df_input[,input_column_str], "\\?", "")
  # Clean up pure number results
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^\\s[0-9]*.*[0-9]$", "")
  # Report everything as decimals
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "\\s[0]$", " 0.0")
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "\\s[1]$", " 1.0")
  
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^Chumra$", "Chumra 1.0")
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^Kula$", "Kula 0.0")
  
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^Chayav$", "Chayav 1.0")
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^Patur$", "Patur 0.0")
  
  
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^BHB 0$", "BHB 0.0")
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^BHB 1$", "BHB 1.0")
  
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^Land owner$", "Land owner 1.0")
  df_input[input_column_str] <-  str_replace(df_input[,input_column_str], "^Renter$", "Renter 0.0")
  
  #Blank results into NA's
  df_input[input_column_str] <-   replace_na(df_input[, input_column_str], "")
  return(df_input)
}

CullNonWinners <- function(df_input){
  # THis version culls all the banks, which results in no winner
  #Use this only for when Dani requests.
  df_input$NameTest = !(is.na(df_input$Disputant_1) | is.na(df_input$Disputant_2) | is.na(df_input$Score_1))
  df_input2 = df_input[df_input$NameTest==TRUE, ]  
  return(df_input2 = df_input[df_input$NameTest==TRUE, ]  )
}
AssignResults <- function(df_input){
  df_input$Strict_Result_1 <- ""
  df_input$Strict_Result_2 <- ""

  df_input$Strict_Result_1[df_input$StrictQuestion==1] <- df_input %>% filter(StrictQuestion==1) %>% 
     mutate(Strict_Result_1n = Result_1) %>% pull()   
  df_input$Strict_Result_2[df_input$StrictQuestion==1] <- df_input %>% filter(StrictQuestion==1) %>% 
    mutate(Strict_Result_2n = Result_2) %>% pull()   

  # Notice that sapply with "[" can be used to extract either the first or second items in those lists so  this captures the numeric part based on the space
  df_input$Strict_Result_Score_1 <- sapply(strsplit(as.character(df_input$Strict_Result_1),' '), "[", 2)
  df_input$Strict_Result_Score_2 <- sapply(strsplit(as.character(df_input$Strict_Result_2),' '), "[", 2)

  # Old way
  # df_input$AltStrict_1 <- (df_input$Strict_Result_Score_1 >= df_input$Strict_Result_Score_2) * 1
  # df_input$AltStrict_2 <- (df_input$Strict_Result_Score_1 <= df_input$Strict_Result_Score_2) * 1

  # Now that BHB is in Chumra, and  BHB 1 is treated as Kula and BHB 0 is treated as Chumra, we need slightly different logic
  #currently there are some blank result entries causing a problem here (tab 13:8:2 and tab 2:3:6 for example)
  # df_input$AltStrict_1 <- ifelse(df_input$MoneyQuestion==1, 
  # (df_input$Strict_Result_Score_1 <= df_input$Strict_Result_Score_2) * 1,
  # (df_input$Strict_Result_Score_1 >= df_input$Strict_Result_Score_2) * 1
  # )
  # df_input$AltStrict_2 <- ifelse(df_input$MoneyQuestion==1, 
  # (df_input$Strict_Result_Score_1 >= df_input$Strict_Result_Score_2) * 1,
  # (df_input$Strict_Result_Score_1 <= df_input$Strict_Result_Score_2) * 1
  # )
  # Improved logic to handle blanks
  # To explain what's happening: In many two-way arguments there's no clear BHB 0 or BHB 1. 
  #The obvious cases are ones where the question doesn't relate to BHB, but it can happen elsewhere too. 
  #Say Rabbi X says you cannot light a fire within 50 feet of my field but Rabbi Y says that the distance 
  # depends on the wind conditions--which could be more than 50 feet but could be less. In those cases, I 
  #wouldn't code a BHB 0/BHB 1 score. In 3-way arguments, there can be a mix of positions. Rabbi V says I 
  #can light a fire so long as I'm 25 feet away, Rabbi X says I can light it if I'm 50 feet away, and Rabbi 
  # Y says it depends on circumstances. In those cases, I can say Rabbi V is BHB 1 relative to Rabbi X who 
  #is BHB 0, but I cannot say how either of them compare to Rabbi Y. So I would code it as Rabbi V = BHB 1; 
  #Rabbi X = BHB 0; Rabbi Y = [blank]
  # For our purposes, then, when we convert the three-way argument to a two-way argument, we should treat it 
  # one argument with a BHB 0/1 score (Rabbi V versus Rabbi X)  and two arguments with no BHB 0/1 score 
  # (Rabbi V versus Rabbi Y; Rabbi X versus Rabbi Y). So if one of the two position fields is blank then, 
  #for purposes of considering the pairwise arguments, they should both be treated as blank.
  df_input$AltStrict_1 <- ifelse(df_input$StrictQuestion==1, 
    ifelse((df_input$Strict_Result_1 != "") & (df_input$Strict_Result_2 != ""),
      ifelse(df_input$MoneyQuestion==1,
        (df_input$Strict_Result_Score_1 <= df_input$Strict_Result_Score_2) * 1,
        (df_input$Strict_Result_Score_1 >= df_input$Strict_Result_Score_2) * 1),
      NA), 
    NA)
  df_input$AltStrict_2 <- ifelse(df_input$StrictQuestion==1, 
    ifelse((df_input$Strict_Result_1 != "") & (df_input$Strict_Result_2 != ""),
      ifelse(df_input$MoneyQuestion==1,
        (df_input$Strict_Result_Score_1 >= df_input$Strict_Result_Score_2) * 1,
        (df_input$Strict_Result_Score_1 <= df_input$Strict_Result_Score_2) * 1),
      NA), 
    NA)  

  #Replace 
  # df_results_9$n.win[is.na(df_results_9$n.win)] <- 0
  # THis version culls all the banks, which results in no winner
  # df_input$NameTest = !(is.na(df_input$Disputant_1) | is.na(df_input$Disputant_2) | is.na(df_input$Score_1))
  # df_input2 = df_input[df_input$NameTest==TRUE, ]  
  # THis version includes the blanks, but it would be better to just force the blanks into ties and code a tie
  df_input$BlankWinner = (is.na(df_input$Score_1) | is.na(df_input$Score_2))
  df_input$Score_1 = ifelse(df_input$BlankWinner, 0, df_input$Score_1)
  df_input$Score_2 = ifelse(df_input$BlankWinner, 0, df_input$Score_2)
  df_input2 = df_input

  #Need to do BHB / Money logic for non 1 / 0 comparisons

  # Need to add logic here to make sure that ties are handled right
  # Need new vector, IsTie.Win, IsTie.Chumra, etc
  df_input2 = df_input2 %>% mutate(
    Loser  = case_when(
      Score_1==1 & Score_2==0 ~ Disputant_2,
      Score_1==0 & Score_2==1 ~ Disputant_1,
      Score_1==0 & Score_2==0 ~ Disputant_2 # Note however that we have to set Tie.Win = True
    ),
    Winner  = case_when(
      Score_1==1 & Score_2==0 ~ Disputant_1,
      Score_1==0 & Score_2==1 ~ Disputant_2,
      Score_1==0 & Score_2==0 ~ Disputant_1 # Note however that we have to set Tie.Win = True
    ),
    KulaName  = case_when(
      Strict_1==1 & Strict_2==0 ~ Disputant_2,
      Strict_1==0 & Strict_2==1 ~ Disputant_1,
      Strict_1==0 & Strict_2==0 ~ Disputant_2 # Note however that we have to set Tie.Win = True
    ),
    ChumraName  = case_when(
      Strict_1==1 & Strict_2==0 ~ Disputant_1,
      Strict_1==0 & Strict_2==1 ~ Disputant_2,
      Strict_1==0 & Strict_2==0 ~ Disputant_1 # Note however that we have to set Tie.Win = True
    ),   
    BHBlowName  = case_when(
      Money_1==1 & Money_2==0 ~ Disputant_2,
      Money_1==0 & Money_2==1 ~ Disputant_1,
      Money_1==0 & Money_2==0 ~ Disputant_2 # Note however that we have to set Tie.Win = True
    ),
    BHBhighName = case_when(
      Money_1==1 & Money_2==0 ~ Disputant_1,
      Money_1==0 & Money_2==1 ~ Disputant_2,
      Money_1==0 & Money_2==0 ~ Disputant_1 # Note however that we have to set Tie.Win = True
    ),  
    ChayavlowName  = case_when(
      Owe_1==1 & Owe_2==0 ~ Disputant_2,
      Owe_1==0 & Owe_2==1 ~ Disputant_1,
      Owe_1==0 & Owe_2==0 ~ Disputant_2 # Note however that we have to set Tie.Win = True
    ),
    ChayavhighName  = case_when(
      Owe_1==1 & Owe_2==0 ~ Disputant_1,
      Owe_1==0 & Owe_2==1 ~ Disputant_2,
      Owe_1==0 & Owe_2==0 ~ Disputant_1 # Note however that we have to set Tie.Win = True
    ),
    Tie.Win    = ifelse(Score_1==0  & Score_2==0  , TRUE, FALSE),
    Tie.Strict = ifelse(Strict_1==0 & Strict_2==0 , TRUE, FALSE),
    Tie.Money  = ifelse(Money_1==0  & Money_2==0  , TRUE, FALSE),
    Tie.Owe    = ifelse(Owe_1==0    & Owe_2==0    , TRUE, FALSE)
    )                   
  
  # df_input2$Loser      <- ifelse( $Score_1==1 & df_input2$Score_2==0, 
  #   df_input2$Disputant_2, 
  #   df_input2$Disputant_1
  #   )
  # #df_input2$Loser      <- ifelse(df_input2$Score_1==1, df_input2$Disputant_2, df_input2$Disputant_1)
  # df_input2$WinnerName <- ifelse(df_input2$Score_1==1, df_input2$Disputant_1, df_input2$Disputant_2)
  # df_input2$ChumraName <- ifelse(df_input2$Strict_1==1, df_input2$Disputant_1, df_input2$Disputant_2)
  # df_input2$KulaName   <- ifelse(df_input2$Strict_1==1, df_input2$Disputant_2, df_input2$Disputant_1)
  # df_input2$BHBhighName  <- ifelse(df_input2$Money_1==1, df_input2$Disputant_1, df_input2$Disputant_2)
  # df_input2$BHBlowName   <- ifelse(df_input2$Money_1==1, df_input2$Disputant_2, df_input2$Disputant_1)
  # df_input2$ChayavhighName  <- ifelse(df_input2$Owe_1==1, df_input2$Disputant_1, df_input2$Disputant_2)
  # df_input2$ChayavlowName   <- ifelse(df_input2$Owe_1==1, df_input2$Disputant_2, df_input2$Disputant_1)
  df_input2$LoserName   <- df_input2$Loser
  df_input2$WinnerName  <- df_input2$Winner
  df_input2$MoneyName  <- df_input2$BHBhighName
  df_input2$OweName    <- df_input2$ChayavhighName
  df_output <- df_input2
  #print(colnames(df_input2))
  return(df_output)
}

metaseqcheck <- function(df_input_win, df_input_chumra, df_input_money, df_input_owe){
  #Check if data are in good shape. presence=null is okay
  df_input_win    <- df_input_win[    order(df_input_win$PsuedoDate), ]
  df_input_chumra <- df_input_chumra[ order(df_input_chumra$PsuedoDate), ]
  df_input_money  <- df_input_money[  order(df_input_money$PsuedoDate), ]
  df_input_owe    <- df_input_owe[    order(df_input_owe$PsuedoDate), ]

  df_input_chumra = df_input_chumra %>% filter(!is.na(ChumraName), !is.na(KulaName)) 
  df_input_money = df_input_money %>% filter(!is.na(BHBhighName), !is.na(BHBlowName)) 
  # This seems to mess things up. Don't know why. 
  # df_input_chumra = df_input_chumra[df_input_chumra$ChumraName!=df_input_chumra$KulaName, ]  
  # df_input_owe = df_input_owe %>% filter(!is.na(ChayavhighName), !is.na(ChayavlowName))

  df_input_win    = df_input_win[df_input_win$WinnerName!=df_input_win$LoserName, ]
  df_input_money  = df_input_money[df_input_money$BHBhighName!=df_input_money$BHBlowName, ]
  df_input_owe    = df_input_owe[df_input_owe$ChayavhighName!=df_input_owe$ChayavlowName, ]

  seqcheck(df_input_win$WinnerName,     df_input_win$LoserName,     df_input_win$PsuedoDate,    df_input_win$Tie.Win)
  seqcheck(df_input_chumra$ChumraName,  df_input_chumra$KulaName,   df_input_chumra$PsuedoDate, df_input_win$Tie.Strict)  
  seqcheck(df_input_money$BHBhighName,  df_input_money$BHBlowName,  df_input_money$PsuedoDate,  df_input_win$Tie.Money)     
  seqcheck(df_input_owe$ChayavhighName, df_input_owe$ChayavlowName, df_input_owe$PsuedoDate,    df_input_win$Tie.Owe)

  return(list(
    "df_output_win"   =df_input_win, 
    "df_output_chumra"=df_input_chumra, 
    "df_output_money" =df_input_money,
    "df_output_owe"   =df_input_owe
    )
  )  
}

# This has to be done to use elo.seq which is a more accurate method 
#Old
# elo_1 <- elo.run(score(Score_1, Score_2) ~ Disputant_1 + Disputant_2, data = df_scores100, k = 32)
# # Display Scores
# final.elos(elo_1)
#New
# elo_1b <- elo.seq(df_scores100$WinnerName, df_scores100$LoserName, df_scores100$PsuedoDate, startvalue=1500, k=32, runcheck=FALSE)
# extract_elo(elo_1b)

EloScoreResampleResults <- function (df_scores_input, reps){
  #Either randomly reorder the results or do k-fold-cross validation
  # tmp_resample <- ResampleResults(df_scores2, 0.8, 10)
  for (j in 1:reps){
    # It is convinient if the first fold just returns the results for the input data
    if (j==1){
        df_scores_tmp_rnd_tmp <- df_scores_input
      } else {
        df_scores_tmp_rnd_tmp <- df_scores_input[sample(nrow(df_scores_input), replace=FALSE),]
      }
    
    elo_1_rnd_tmp  <- elo.run(score(Score_1, Score_2) ~ Disputant_1 + Disputant_2, data = df_scores_tmp_rnd_tmp, k = 32)
    elo_2b_rnd_tmp <- elo.run(score(AltStrict_1, AltStrict_2) ~ Disputant_1 + Disputant_2, data = df_scores_tmp_rnd_tmp %>% filter(AltStrictQuestion==1), k = 32)
    elo_3_rnd_tmp  <- elo.run(score(Money_1, Money_2) ~ Disputant_1 + Disputant_2, data = df_scores_tmp_rnd_tmp %>% filter(MoneyQuestion==1), k = 32)
    elo_4_rnd_tmp  <- elo.run(score(Owe_1, Owe_2) ~ Disputant_1 + Disputant_2, data = df_scores_tmp_rnd_tmp %>% filter(OweQuestion==1), k = 32)


    df_results_1_rnd_tmp  <- do.call(rbind, Map(data.frame, EloScoreWins=final.elos(elo_1_rnd_tmp)))
    df_results_2c_rnd_tmp <- do.call(rbind, Map(data.frame, EloScoreChumra=final.elos(elo_2b_rnd_tmp)))
    df_results_3_rnd_tmp  <- do.call(rbind, Map(data.frame, EloScoreMoney=final.elos(elo_3_rnd_tmp)))
    df_results_4_rnd_tmp  <- do.call(rbind, Map(data.frame, EloScoreOwe=final.elos(elo_4_rnd_tmp)))

    df_results_1_rnd_tmp$Name  <- row.names(df_results_1_rnd_tmp)
    df_results_2c_rnd_tmp$Name <- row.names(df_results_2c_rnd_tmp)
    df_results_3_rnd_tmp$Name  <- row.names(df_results_3_rnd_tmp)
    df_results_4_rnd_tmp$Name  <- row.names(df_results_4_rnd_tmp)
    
    rownames(df_results_1_rnd_tmp) <- c()
    rownames(df_results_2c_rnd_tmp) <- c()
    rownames(df_results_3_rnd_tmp) <- c()
    rownames(df_results_4_rnd_tmp) <- c()

    if (j == 1) {
      # Merge data according to row names
      df_elo_combined1_rnd_tmp  <- df_results_1_rnd_tmp
      df_elo_combined2b_rnd_tmp <- df_results_2c_rnd_tmp
      df_elo_combined3_rnd_tmp  <- df_results_3_rnd_tmp
      df_elo_combined4_rnd_tmp  <- df_results_4_rnd_tmp
    } else {
      df_elo_combined1_rnd_tmp  <- merge(df_elo_combined1_rnd_tmp,  df_results_1_rnd_tmp,  all=TRUE, by="Name", suffixes = c("", j))
      df_elo_combined2b_rnd_tmp <- merge(df_elo_combined2b_rnd_tmp, df_results_2c_rnd_tmp, all=TRUE, by="Name", suffixes = c("", j))
      df_elo_combined3_rnd_tmp  <- merge(df_elo_combined3_rnd_tmp,  df_results_3_rnd_tmp,  all=TRUE, by="Name", suffixes = c("", j))    
      df_elo_combined4_rnd_tmp  <- merge(df_elo_combined4_rnd_tmp,  df_results_4_rnd_tmp,  all=TRUE, by="Name", suffixes = c("", j))    
    }       
  }
  # Tidy up the data for plotting later

  df_elo_combined1_rnd_tmp_tidy  <- df_elo_combined1_rnd_tmp  %>% gather(permutation, EloScoreWins, -Name)
  df_elo_combined2b_rnd_tmp_tidy <- df_elo_combined2b_rnd_tmp %>% gather(permutation, EloScoreChumra, -Name)
  df_elo_combined3_rnd_tmp_tidy  <- df_elo_combined3_rnd_tmp  %>% gather(permutation, EloScoreMoney, -Name)
  df_elo_combined4_rnd_tmp_tidy  <- df_elo_combined4_rnd_tmp  %>% gather(permutation, EloScoreOwe, -Name)
  
  # df_elo_combined1_rnd_tmp_tidy  <- left_join(df_elo_combined1_rnd_tmp_tidy,  df_counts,        by=c("Name" = "values"))
  # df_elo_combined2b_rnd_tmp_tidy <- left_join(df_elo_combined2b_rnd_tmp_tidy, df_chumra_counts, by=c("Name" = "values"))
  # df_elo_combined3_rnd_tmp_tidy  <- left_join(df_elo_combined3_rnd_tmp_tidy,  df_money_counts,  by=c("Name" = "values"))
  # df_elo_combined4_rnd_tmp_tidy  <- left_join(df_elo_combined4_rnd_tmp_tidy,  df_owe_counts,    by=c("Name" = "values"))
  #print(colnames(df_elo_combined1_rnd_tmp_tidy))
  df_elo_combined1_rnd_tmp_tidy  <- df_elo_combined1_rnd_tmp_tidy[ order(df_elo_combined1_rnd_tmp_tidy$permutation), ]
  df_elo_combined2b_rnd_tmp_tidy <- df_elo_combined2b_rnd_tmp_tidy[order(df_elo_combined2b_rnd_tmp_tidy$permutation), ]
  df_elo_combined3_rnd_tmp_tidy  <- df_elo_combined3_rnd_tmp_tidy[ order(df_elo_combined3_rnd_tmp_tidy$permutation), ]
  df_elo_combined4_rnd_tmp_tidy  <- df_elo_combined4_rnd_tmp_tidy[ order(df_elo_combined4_rnd_tmp_tidy$permutation), ]
  return(list("df_win"=df_elo_combined1_rnd_tmp_tidy, 
    "df_chumra" = df_elo_combined2b_rnd_tmp_tidy, 
    "df_money"  = df_elo_combined3_rnd_tmp_tidy,
    "df_owe"    = df_elo_combined4_rnd_tmp_tidy)
  )
}

PredictResults <- function (df_elo_data, df_score_data, min_n, ResultType){
  # https://datascience.blog.wzb.eu/2016/09/27/dynamic-columnvariable-names-with-dplyr-using-standard-evaluation-functions/
  # Dynamic column/variable names with dplyr using Standard Evaluation functions
  # Not needed, can just use quoted versions

  # df_predictors_tmp____0 <- df_elo_data  %>% select(Name, n, median_elo, normDS, EloScoreWins,   WinRate)
  # df_predictors_tmp____0 <- df_elo_data  %>% select(field_list)
  if (ResultType=="WinnerName") {
        df_predictors_tmp____0 <- df_elo_data  %>% select(Name, n.disputes, median_elo.win, DS.win, EloScoreWins,    WinRate)
    } else if (ResultType=="ChumraName") {
        df_predictors_tmp____0 <- df_elo_data  %>% select(Name, n.disputes, median_elo.strict, DS.strict, EloScoreChumra,  WinChumraRate)
    } else if (ResultType=="MoneyName") {
        df_predictors_tmp____0 <- df_elo_data  %>% select(Name, n.disputes, median_elo.money, DS.money, EloScoreMoney,   WinMoneyRate)
    } else if (ResultType=="OweName") {
        df_predictors_tmp____0 <- df_elo_data  %>% select(Name, n.disputes, median_elo.owe, DS.owe, EloScoreOwe,     WinOweRate)
    }

    df_predictors_tmp____1 <- left_join(df_score_data, df_predictors_tmp____0, by=c("Disputant_1" = "Name"))
    df_predictors_tmp____1 <- left_join(df_predictors_tmp____1, df_predictors_tmp____0, by=c("Disputant_2" = "Name"), suffix = c(".1", ".2"))

    # print(CorrectName)
  if (ResultType=="WinnerName") {
    df_predictors_tmp____2  <- df_predictors_tmp____1 %>% mutate(
      median_elo_winner = ifelse(median_elo.win.1   < median_elo.win.2,   2, 1),
      talmud_elo_winner = ifelse(EloScoreWins.1 < EloScoreWins.2, 2, 1),
      davids_scr_winner = ifelse(DS.win.1       < DS.win.2,       2, 1),
      win___rate_winner = ifelse(WinRate.1      < WinRate.2,      2, 1),
      actual_____winner = ifelse(WinnerName    == Disputant_2,    2, 1)
      )
    df_predictors_tmp____3 <- df_predictors_tmp____2 %>% filter(!(is.na(median_elo_winner)), !(is.na(davids_scr_winner)))
  }
  if (ResultType=="ChumraName") {
    df_predictors_tmp____2  <- df_predictors_tmp____1 %>% mutate(
      median_elo_winner = ifelse(median_elo.strict.1     < median_elo.strict.2,   2, 1),
      talmud_elo_winner = ifelse(EloScoreChumra.1 < EloScoreChumra.2, 2, 1),
      davids_scr_winner = ifelse(DS.strict.1         < DS.strict.2,       2, 1),
      win___rate_winner = ifelse(WinChumraRate.1  < WinChumraRate.2,2, 1),
      actual_____winner = ifelse(ChumraName      == Disputant_2,    2, 1)
      )
    df_predictors_tmp____3 <- df_predictors_tmp____2 %>% filter(AltStrictQuestion==1, !(is.na(median_elo_winner)), !(is.na(davids_scr_winner)))
  } 
  if (ResultType=="MoneyName") {
    df_predictors_tmp____2  <- df_predictors_tmp____1 %>% mutate(
      median_elo_winner = ifelse(median_elo.money.1     < median_elo.money.2,   2, 1),
      talmud_elo_winner = ifelse(EloScoreMoney.1  < EloScoreMoney.2, 2, 1),
      davids_scr_winner = ifelse(DS.money.1         < DS.money.2,       2, 1),
      win___rate_winner = ifelse(WinMoneyRate.1   < WinMoneyRate.2, 2, 1),
      actual_____winner = ifelse(MoneyName       == Disputant_2,    2, 1)
      )
    df_predictors_tmp____3 <- df_predictors_tmp____2 %>% filter(MoneyQuestion==1, !(is.na(median_elo_winner)), !(is.na(davids_scr_winner)))

  }      

  if (ResultType=="OweName") {
    df_predictors_tmp____2  <- df_predictors_tmp____1 %>% mutate(
      median_elo_winner = ifelse(median_elo.owe.1     < median_elo.owe.2,   2, 1),
      talmud_elo_winner = ifelse(EloScoreOwe.1  < EloScoreOwe.2, 2, 1),
      davids_scr_winner = ifelse(DS.owe.1         < DS.owe.2,       2, 1),
      win___rate_winner = ifelse(WinOweRate.1   < WinOweRate.2, 2, 1),
      actual_____winner = ifelse(OweName       == Disputant_2,    2, 1)
      )
    df_predictors_tmp____3 <- df_predictors_tmp____2 %>% filter(OweQuestion==1, !(is.na(median_elo_winner)), !(is.na(davids_scr_winner)))

  } 

  n_trials_tmp        <- length( df_predictors_tmp____3$actual_____winner) 
  p_median_elo_winner <- sum(df_predictors_tmp____3$actual_____winner == df_predictors_tmp____3$median_elo_winner) / n_trials_tmp 
  p_talmud_elo_winner <- sum(df_predictors_tmp____3$actual_____winner == df_predictors_tmp____3$talmud_elo_winner) / n_trials_tmp 
  p_davids_scr_winner <- sum(df_predictors_tmp____3$actual_____winner == df_predictors_tmp____3$davids_scr_winner) / n_trials_tmp 
  p_win___rate_winner <- sum(df_predictors_tmp____3$actual_____winner == df_predictors_tmp____3$win___rate_winner) / n_trials_tmp 
  p_actual_____winner <- sum(df_predictors_tmp____3$actual_____winner == df_predictors_tmp____3$actual_____winner) / n_trials_tmp 
  df_compare_results_tmp <-data.frame(
    Method=c("Median Random Order Elo", "Talmud Order Elo", "David's Score", "Win Rate"),
    ProbCorrect=c(p_median_elo_winner, p_talmud_elo_winner, p_davids_scr_winner, p_win___rate_winner),
    StdErrP=c(
      p_median_elo_winner * (1-p_median_elo_winner) / n_trials_tmp,
      p_talmud_elo_winner * (1-p_talmud_elo_winner) / n_trials_tmp,
      p_davids_scr_winner * (1-p_davids_scr_winner) / n_trials_tmp,
      p_win___rate_winner * (1-p_win___rate_winner) / n_trials_tmp
      )
    )
  print("Full in-sample prediction accuracy")
  print(df_compare_results_tmp)
  print(n_trials_tmp)

  df_predictors_tmp____4 <- df_predictors_tmp____3 %>% filter(n.disputes.1>=min_n, n.disputes.2>=min_n)
  n_trials_tmp_2 <- length( df_predictors_tmp____4$actual_____winner) 
  p_median_elo_winner <- sum(df_predictors_tmp____4$actual_____winner == df_predictors_tmp____4$median_elo_winner) / n_trials_tmp_2 
  p_talmud_elo_winner <- sum(df_predictors_tmp____4$actual_____winner == df_predictors_tmp____4$talmud_elo_winner) / n_trials_tmp_2 
  p_davids_scr_winner <- sum(df_predictors_tmp____4$actual_____winner == df_predictors_tmp____4$davids_scr_winner) / n_trials_tmp_2 
  p_win___rate_winner <- sum(df_predictors_tmp____4$actual_____winner == df_predictors_tmp____4$win___rate_winner) / n_trials_tmp_2 
  p_actual_____winner <- sum(df_predictors_tmp____4$actual_____winner == df_predictors_tmp____4$actual_____winner) / n_trials_tmp_2 
  df_compare_results_tmp_2 <-data.frame(
    Method=c("Median Random Order Elo", "Talmud Order Elo", "David's Score", "Win Rate"),
    ProbCorrect=c(p_median_elo_winner, p_talmud_elo_winner, p_davids_scr_winner, p_win___rate_winner),
    StdErrP=c(
      p_median_elo_winner * (1-p_median_elo_winner) / n_trials_tmp_2,
      p_talmud_elo_winner * (1-p_talmud_elo_winner) / n_trials_tmp_2,
      p_davids_scr_winner * (1-p_davids_scr_winner) / n_trials_tmp_2,
      p_win___rate_winner * (1-p_win___rate_winner) / n_trials_tmp_2
      )
    )
  print("Minimum n-case in-sample prediction accuracy:")
  print(paste(min_n, ": cases"))
  print(df_compare_results_tmp_2)
  print(n_trials_tmp_2)

  return(list(
    "df_predictor"=df_predictors_tmp____3, 
    "df_comp_res"=df_compare_results_tmp, 
    "n_trials"=n_trials_tmp,
    "df_predictor_restricted"=df_predictors_tmp____4, 
    "df_comp_res_restricted"=df_compare_results_tmp_2, 
    "n_trials_restricted"=n_trials_tmp_2
    )
  )
}

BrandesResultsTwoWay <- function (df_scores, DispSupossedToWin, DispSupossedToLose){
  # Brandes
  df_bran_a = df_scores %>% 
      filter(Disputant_1==DispSupossedToWin & Disputant_2==DispSupossedToLose) %>% 
      select(SeqID, tabnum, Location, Winner, Disputant_1, Disputant_2, Score_1, Score_2) 
  df_bran_b = df_scores %>% 
      filter(Disputant_1==DispSupossedToLose & Disputant_2==DispSupossedToWin) %>% 
      select(SeqID, tabnum, Location, Winner, Disputant_1, Disputant_2, Score_1, Score_2) 
  df_bran = rbind(
    df_bran_a %>%
      transmute(SeqID=SeqID, Tana_1=Disputant_1, Tana_2=Disputant_2, Win=Score_1, Lose=Score_2), 
      df_bran_b %>%
      transmute(SeqID=SeqID, Tana_1=Disputant_2, Tana_2=Disputant_1, Win=Score_2, Lose=Score_1)
      )
  print(paste("When", DispSupossedToWin, "disputes with", DispSupossedToLose, "how often does", DispSupossedToWin, "win?"))
  return(df_bran %>% summarize(
    Wins    = sum(Win),
    Losses  = sum(Lose),
    Neither = n() - sum(Win) - sum(Losses),
    Matches = n(),
    WinPct  = round(100*(sum(Win) + 0.5 *(n() - sum(Win) - sum(Losses))) /n(), 0), # Win Rate as half ties + wins over total so always ties has a win rate of 0.5 
    WinPct_ignore_ties  = round(100*(sum(Win)) / (sum(Win) + sum(Losses)), 0), # Win Rate as wins over non-ties. Always ties is NA 
    ))
}

BrandesResultsAlwaysWin <- function (df_scores, DispSupossedToWin){
  df_bran_a = df_scores %>% 
      filter(Disputant_1==DispSupossedToWin) %>% 
      select(SeqID, tabnum, Location, Winner, Disputant_1, Disputant_2, Score_1, Score_2) 
  df_bran_b = df_scores %>% 
      filter( Disputant_2==DispSupossedToWin) %>% 
      select(SeqID, tabnum, Location, Winner, Disputant_1, Disputant_2, Score_1, Score_2) 
  df_bran = rbind(
    df_bran_a %>%
      transmute(SeqID=SeqID, Tana_1=Disputant_1, Tana_2=Disputant_2, Win=Score_1, Lose=Score_2), 
      df_bran_b %>%
      transmute(SeqID=SeqID, Tana_1=Disputant_2, Tana_2=Disputant_1, Win=Score_2, Lose=Score_1)
      )
  print(paste("When", DispSupossedToWin, "disputes", "how often does", DispSupossedToWin, "win?"))
  return(df_bran %>% summarize(
    Wins = sum(Win),
    Losses =  sum(Lose),
    Neither = n() - sum(Win) - sum(Losses),
    Matches = n(),
    WinPct= round(100*(sum(Win) + 0.5 *(n() - sum(Win) - sum(Losses))) /n(), 0), # Win Rate as half ties + wins over total so always ties has a win rate of 0.5 
    WinPct_ignore_ties  = round(100*(sum(Win)) / (sum(Win) + sum(Losses)), 0), # Win Rate as wins over non-ties. Always ties is NA 
    ))
}

WinRate <- function (df_scores){
  df_tmp_a = df_scores %>% 
      group_by(Disputant_1) %>% 
      select(SeqID, tabnum, Location, Winner, 
        Disputant_1, Score_1, AltStrict_1, Money_1, Owe_1,
        Disputant_2, Score_2, AltStrict_2, Money_2, Owe_2,
        StrictQuestion, MoneyQuestion, OweQuestion) %>%
      summarize(
        Disputant   = first(Disputant_1),
        Count.Win     = n(),
        Count.Strict  = sum(StrictQuestion, na.rm=TRUE),
        Count.Money   = sum(MoneyQuestion, na.rm=TRUE),
        Count.Owe     = sum(OweQuestion, na.rm=TRUE),
        Score         = sum(Score_1, na.rm=TRUE),     
        AltStrict     = sum(AltStrict_1, na.rm=TRUE), 
        Money         = sum(Money_1, na.rm=TRUE),     
        Owe           = sum(Owe_1, na.rm=TRUE),
        Score_B       = sum(Score_2, na.rm=TRUE),     
        AltStrict_B   = sum(AltStrict_2, na.rm=TRUE), 
        Money_B       = sum(Money_2, na.rm=TRUE),     
        Owe_B         = sum(Owe_2, na.rm=TRUE)                   
        ) %>% select(-Disputant_1)       
  df_tmp_b = df_scores %>% 
      group_by(Disputant_2) %>% 
      select(SeqID, tabnum, Location, Winner, 
        Disputant_1, Score_1, AltStrict_1, Money_1, Owe_1,
        Disputant_2, Score_2, AltStrict_2, Money_2, Owe_2,
        StrictQuestion, MoneyQuestion, OweQuestion) %>%
      summarize(
        Disputant     = first(Disputant_2),
        Count.Win     = n(),
        Count.Strict  = sum(StrictQuestion, na.rm=TRUE),
        Count.Money   = sum(MoneyQuestion, na.rm=TRUE),
        Count.Owe     = sum(OweQuestion, na.rm=TRUE),
        Score         = sum(Score_2, na.rm=TRUE),     
        AltStrict     = sum(AltStrict_2, na.rm=TRUE), 
        Money         = sum(Money_2, na.rm=TRUE),     
        Owe           = sum(Owe_2, na.rm=TRUE),
        Score_B       = sum(Score_1, na.rm=TRUE),     
        AltStrict_B   = sum(AltStrict_1, na.rm=TRUE), 
        Money_B       = sum(Money_1, na.rm=TRUE),     
        Owe_B         = sum(Owe_1, na.rm=TRUE)        
        ) %>% select(-Disputant_2)  
  df_tmp = rbind(df_tmp_a, df_tmp_b)
  

  df_tmp2 = df_tmp %>% group_by(Disputant) %>% summarize(
          CountMatches.Win      = sum(Count.Win),  
          CountMatches.Strict   = sum(Count.Strict),  
          CountMatches.Money    = sum(Count.Money),
          CountMatches.Owe      = sum(Count.Owe),                         
          Sum.Wins.Win      = sum(Score), 
          Sum.Wins.Strict   = sum(AltStrict, na.rm=TRUE),
          Sum.Wins.Money    = sum(Money, na.rm=TRUE),
          Sum.Wins.Owe      = sum(Owe, na.rm=TRUE),
          Sum.Loss.Win      = sum(Score_B), 
          Sum.Loss.Strict   = sum(AltStrict_B, na.rm=TRUE),
          Sum.Loss.Money    = sum(Money_B, na.rm=TRUE),
          Sum.Loss.Owe      = sum(Owe_B, na.rm=TRUE)               
    )  %>% mutate(
        Sum.Ties.Win      = CountMatches.Win    - Sum.Wins.Win    - Sum.Loss.Win, 
        Sum.Ties.Strict   = CountMatches.Strict - Sum.Wins.Strict - Sum.Loss.Strict, 
        Sum.Ties.Money    = CountMatches.Money  - Sum.Wins.Money  - Sum.Loss.Money,
        Sum.Ties.Owe      = CountMatches.Owe    - Sum.Wins.Owe    - Sum.Loss.Owe
    ) %>% mutate(
        WinRate             = (Sum.Wins.Win     + 0.5 * Sum.Ties.Win)     / CountMatches.Win,
        WinChumraRate       = (Sum.Wins.Strict  + 0.5 * Sum.Ties.Strict)  / CountMatches.Strict,
        WinMoneyRate        = (Sum.Wins.Money   + 0.5 * Sum.Ties.Money)   / CountMatches.Money,
        WinOweRate          = (Sum.Wins.Owe     + 0.5 * Sum.Ties.Owe)     / CountMatches.Owe,
        WinChumraExMonRate  = ((Sum.Wins.Strict - Sum.Loss.Money) + 0.5 * (Sum.Ties.Strict - Sum.Ties.Money))     / (CountMatches.Strict - CountMatches.Money),
        WinRate_ignore_ties             = Sum.Wins.Win    / (CountMatches.Win - Sum.Ties.Win), # Win Rate as wins over non-ties. Always ties is NA 
        WinChumraRate_ignore_ties       = Sum.Wins.Strict / (CountMatches.Strict - Sum.Ties.Strict), # Win Rate as wins over non-ties. Always ties is NA 
        WinMoneyRate_ignore_ties        = Sum.Wins.Money  / (CountMatches.Money - Sum.Ties.Money), # Win Rate as wins over non-ties. Always ties is NA 
        WinOweRate_ignore_ties          = Sum.Wins.Owe    / (CountMatches.Owe - Sum.Ties.Owe), # Win Rate as wins over non-ties. Always ties is NA  
        WinChumraExMonRate_ignore_ties  = (Sum.Wins.Strict - Sum.Loss.Money) / ((CountMatches.Strict - Sum.Ties.Strict) - (CountMatches.Money - Sum.Ties.Money))
    )

  # Replace NA with 0  
  df_tmp2$WinRate[is.na(df_tmp2$WinRate)]             <- 0.5
  df_tmp2$WinChumraRate[is.na(df_tmp2$WinChumraRate)] <- 0.5
  df_tmp2$WinMoneyRate[is.na(df_tmp2$WinMoneyRate)]   <- 0.5
  df_tmp2$WinOweRate[is.na(df_tmp2$WinOweRate)]       <- 0.5
  df_tmp2$WinChumraExMonRate[is.na(df_tmp2$WinChumraExMonRate)] <- 0.5

  df_tmp2$WinRate[!is.finite(df_tmp2$WinRate)]             <- 0.5
  df_tmp2$WinChumraRate[!is.finite(df_tmp2$WinChumraRate)] <- 0.5
  df_tmp2$WinMoneyRate[!is.finite(df_tmp2$WinMoneyRate)]   <- 0.5
  df_tmp2$WinOweRate[!is.finite(df_tmp2$WinOweRate)]       <- 0.5
  df_tmp2$WinChumraExMonRate[!is.finite(df_tmp2$WinChumraExMonRate)] <- 0.5
  return(df_tmp2)  
 
}

DisputesInvolving <- function (df_scores, TanaStr){
  # df_tmp = DisputesInvolving(df_scores100, "Tana Kama")
  # df_tmp = DisputesInvolving(df_scores100, "Rabbi Elazar ben Tzadok")
  # (0.5 * dim(df_tmp %>% filter(StrictQuestion==TRUE, AltStrict_1 == AltStrict_2))[1] +
  # dim(df_tmp %>% filter(StrictQuestion==TRUE, Disputant_1 == "Rabbi Elazar ben Tzadok", AltStrict_1 == 1))[1] +
  # dim(df_tmp %>% filter(StrictQuestion==TRUE, Disputant_2 == "Rabbi Elazar ben Tzadok", AltStrict_2 == 1))[1]) / 
  # dim(df_tmp  %>% filter(StrictQuestion==TRUE))[1]

  # (0.5 * dim(df_tmp %>% filter(MoneyQuestion==TRUE, Money_1 == Money_2))[1] +
  # dim(df_tmp %>% filter(MoneyQuestion==TRUE, Disputant_1 == "Rabbi Elazar ben Tzadok", Money_1 == 1))[1] +
  # dim(df_tmp %>% filter(MoneyQuestion==TRUE, Disputant_2 == "Rabbi Elazar ben Tzadok", Money_2 == 1))[1]) / 
  # dim(df_tmp  %>% filter(MoneyQuestion==TRUE))[1]


  # (0.5 * dim(df_tmp %>% filter(OweQuestion==TRUE, Owe_1 == Owe_1))[1] +
  # dim(df_tmp %>% filter(OweQuestion==TRUE, Disputant_1 == "Rabbi Elazar ben Tzadok", Owe_1 == 1))[1] +
  # dim(df_tmp %>% filter(OweQuestion==TRUE, Disputant_2 == "Rabbi Elazar ben Tzadok", Owe_2 == 1))[1]) / 
  # dim(df_tmp  %>% filter(OweQuestion==TRUE))[1]

  return(df_scores %>% filter(Disputant_1 == TanaStr | Disputant_2 == TanaStr))
}

# This shows that the dominance matrices strip out the ties
#1087 wins
# dim(df_scores100 %>% filter((Disputant_1 == "Tana Kama" & Score_1==1)| (Disputant_2 == "Tana Kama" & Score_2==1)))
#277 non wins
# dim(df_scores100 %>% filter((Disputant_1 == "Tana Kama" & Score_1==0)| (Disputant_2 == "Tana Kama" & Score_2==0)))
#76 ties which impliese that 201 losses
# dim(df_scores100 %>% filter((Disputant_1 == "Tana Kama" & Score_1==0 & Score_2==0)| (Disputant_2 == "Tana Kama" & Score_1==0 & Score_2==0)))

# dim(df_scores100 %>% filter((Disputant_1 == "Beit Hillel")| (Disputant_2 == "Beit Hillel")))
# dim(df_scores100 %>% filter((Disputant_1 == "Beit Hillel" & Score_1==1)| (Disputant_2 == "Beit Hillel" & Score_2==1)))
# dim(df_scores100 %>% filter((Disputant_1 == "Beit Hillel" & Score_1==0)| (Disputant_2 == "Beit Hillel" & Score_2==0)))
# dim(df_scores100 %>% filter((Disputant_1 == "Beit Hillel" & Score_1==0 & Score_2==0)| (Disputant_2 == "Beit Hillel" & Score_1==0 & Score_2==0)))
# # 249 disputs, 223 wins, 8 ties, and 18 losses, and 241 non ties


NewPredictResultsFullSample <- function (df_elo_data, df_score_data, DomMeasure, WinMeasure, Tol) {
  # df_elo_data = df_results_100 
  # df_score_data = df_scores100
  # DomMeasure = "WinChumraRate"
  DomMeasure1 = paste0(DomMeasure, "_1")
  DomMeasure2 = paste0(DomMeasure, "_2")  

  #WinMeasure = "AltStrict"
  WinMeasure1 = paste0(WinMeasure, "_1")
  WinMeasure2 = paste0(WinMeasure, "_2")  
  WinMeasurePred1 = paste0(WinMeasure, "_predicted", "_1")
  WinMeasurePred2 = paste0(WinMeasure, "_predicted",  "_2")

  DomMeasurePredAcc = paste0(DomMeasure, "_PredScore")
  test1 = df_score_data %>% select(
    DisputeID,
    Disputant_1, 
    Disputant_2,       
    paste0(WinMeasure, "_1"), 
    paste0(WinMeasure, "_2")
      )
  test2 = df_elo_data %>% select(Name, DomMeasure)
  df_elo_data2 = left_join(test1,         test2, by=c("Disputant_1"="Name"))
  df_elo_data2 = left_join(df_elo_data2,  test2, by=c("Disputant_2"="Name"), suffix=c("_1", "_2"))



  # df_elo_data2[WinMeasurePred1] = ifelse(
  #   df_elo_data2[[DomMeasure1]] > df_elo_data2[[DomMeasure2]], 1, 0)

  # df_elo_data2[WinMeasurePred2] = ifelse(
  #   df_elo_data2[[DomMeasure2]] > df_elo_data2[[DomMeasure1]], 1, 0)

  df_elo_data2[WinMeasurePred1] = ifelse(
    df_elo_data2[[DomMeasure1]] > (Tol + df_elo_data2[[DomMeasure2]]), 1, 0)

  df_elo_data2[WinMeasurePred2] = ifelse(
    df_elo_data2[[DomMeasure2]] > (Tol + df_elo_data2[[DomMeasure1]]), 1, 0)

  df_elo_data2[DomMeasurePredAcc] = (df_elo_data2[[WinMeasurePred2]] == df_elo_data2[[WinMeasure2]]) + 
    (df_elo_data2[[WinMeasurePred1]] == df_elo_data2[[WinMeasure1]])

  TotalPredictions = sum(!is.na(df_elo_data2[[WinMeasure1]])) + 
    sum(!is.na(df_elo_data2[[WinMeasure2]]))

  TotalCorrectPredict = sum(df_elo_data2[DomMeasurePredAcc], na.rm = TRUE)
  print(
    paste0(
      "Correctly Predicting: ", 
      TotalCorrectPredict, 
      " out of: ", 
      TotalPredictions, 
      " (", 
      DomMeasure, 
      ", ", 
      WinMeasure,
      ": ", 
      round(100 * TotalCorrectPredict/TotalPredictions,1),
      "%)"
      )
    )
  return(df_elo_data2 %>% select(DisputeID, Disputant_1, Disputant_2, WinMeasurePred1, WinMeasurePred2, DomMeasurePredAcc))
} 



ForecastAccuracyByRabbi <- function (df, PredictedResultsCol) {
  #ForecastAccuracyByRabbi(df_eval.win, "WinRate_PredAcc")
  # https://stackoverflow.com/questions/26724124/standard-evaluation-in-dplyr-summarise-a-variable-given-as-a-character-string
  # !!sym(PredictedResultsCol) this allows you to use a string in dpylr
  #df_eval.win = left_join(df_eval.win, df_scores100 %>% select(DisputeID, Disputant_1, Disputant_2), by="DisputeID")

  df_tmp_a = df %>% 
      group_by(Disputant_1) %>% 
      select(Disputant_1, !!sym(PredictedResultsCol)) %>%
      summarize(
        SumPredAcc    = sum(!!sym(PredictedResultsCol), na.rm=TRUE),                   
        CountPredAcc  = n()
        ) 
  df_tmp_b = df %>% 
      group_by(Disputant_2) %>% 
      select(Disputant_2, !!sym(PredictedResultsCol)) %>%
      summarize(
        SumPredAcc    = sum(!!sym(PredictedResultsCol), na.rm=TRUE),                     
        CountPredAcc  = n()
        ) 
  df_tmp = rbind(
    rename_column(df_tmp_a, "Disputant_1", "Disputant"),
    rename_column(df_tmp_b, "Disputant_2", "Disputant")
    ) %>% group_by(Disputant) %>% 
        summarize(
          MeanPredAcc    = sum(SumPredAcc, na.rm=TRUE) / ( 2 * sum(CountPredAcc, na.rm=TRUE))                   
          ) 
  colnames(df_tmp) = c("Disputant", paste0(PredictedResultsCol, "_Mean"))
  return(df_tmp)
}      

