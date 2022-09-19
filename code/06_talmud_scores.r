#########################################################################################
####################################  Assign scores
#########################################################################################
print("Asign scores")
df_scores1 <- AssignScores(df_scores0)

#########################################################################################
#################################### Handle disputants 3-5. This approach creates two new rows to compare them against each. 
#########################################################################################
print("Reorginize data to handle cases with more than 2 disputants")

df_scores2 <- CountDisputants(df_scores1)
df_scores2 <- FilterBadRows(df_scores2)

# Eulers' rule. Without repeats there are N * (N-1) /2 combinations 
# For example: A B, C means N = 3. AB BC AC are possibilities (order doesn't matter) 
# 3 * 2 / 1 = 3
# But if there are two people taking position 1 and one taking position 2, this isn't right
# A1B A2B are the only possibilities. 
# Old way does no count shared arguments properly
# df_scores2$EffectiveRows = df_scores2$NumDisp * ((df_scores2$NumDisp ) - 1) / 2
# New way, just count after the splitting (done later)
# df_scores5 = df_scores5 %>% group_by(SeqID) %>% mutate(EffectiveRows = length(unique(DisputeID ))) %>% ungroup()

# Is the first disputant gernerally stricter?
summary(df_scores2 %>% filter(NumDisp==2, Disputant_1 != "Tanna Kama", Disputant_2 != "Tanna Kama") %>% select(Strict_1))
summary(df_scores2 %>% filter(NumDisp==2, Disputant_1 != "Tanna Kama", Disputant_2 != "Tanna Kama") %>% select(Strict_2))


EstimatedNumberRowsNotCountingSplitArguments = sum(df_scores2$EffectiveRows)



df_seder_summary_no_repeats <- df_scores2 %>% group_by(Seder_Name) %>% summarize(CountArguments = n())
df_seder_summary_no_repeats <- df_seder_summary_no_repeats %>%  add_row(Seder_Name="Average", CountArguments = mean(df_seder_summary_no_repeats$CountArguments, rm.na=TRUE))
print(df_seder_summary_no_repeats)

df_chapter_summary_no_repeats <- df_scores2 %>% group_by(tab) %>% summarize(CountArguments = n())
df_chapter_summary_no_repeats <- df_chapter_summary_no_repeats %>%  add_row(tab="Average", CountArguments = mean(df_chapter_summary_no_repeats$CountArguments, rm.na=TRUE))
df_chapter_summary_no_repeats$CountArguments = as.integer(df_chapter_summary_no_repeats$CountArguments)

print(df_chapter_summary_no_repeats)

df_scores3 <- HandleDuplicates(df_scores2) 

# Make sure these are all numeric fields (this duplication process turns everything into strings)
df_scores3b <-  df_scores3 %>% mutate_at(c("Score_1", "Strict_1", "Money_1", "Owe_1", "Score_2", "Strict_2", "Money_2", "Owe_2"), as.numeric)



df_scores4 <- df_scores3b %>% filter(keep==TRUE)

Clean_Field_List <- c(
  "Disputant_3", "Disputant_4", "Disputant_5", "Disputant_6",
  "Result_3", "Result_4", "Result_5", "Result_6",
  "Score_3", "Score_4", "Score_5", "Score_6",
  "Strict_3", "Strict_4", "Strict_5", "Strict_6"
  ) 


df_scores4[df_scores4$keep == TRUE, Clean_Field_List] = NA

df_seder_summary <- df_scores4 %>% group_by(Seder_Name) %>% summarize(CountArguments = n())
df_seder_summary <- df_seder_summary %>%  add_row(Seder_Name="Average", CountArguments = mean(df_seder_summary$CountArguments, rm.na=TRUE))


df_chapter_summary <- df_scores4 %>% group_by(tab) %>% summarize(CountArguments = n())
df_chapter_summary <- df_chapter_summary %>%  add_row(tab="Average", CountArguments = mean(df_chapter_summary$CountArguments, rm.na=TRUE))
df_chapter_summary$CountArguments = as.integer(df_chapter_summary$CountArguments)
#########################################################################################
#################################### Split the names and make the rows for shared arguments
#########################################################################################
print("Reorginize data to handle cases with shared Arguments")

# Old way did not handle instances where more than 3 people took the position in a argument
# About 11 cases
# df_scores4 %>% mutate(SlashCount_1 = str_count(Disputant_1, "/"), SlashCount_2 = str_count(Disputant_2, "/")) %>%  summarize(MaxSlashCount_1 = max(SlashCount_1), MaxSlashCount_2 = max(SlashCount_2))
#df_scores5_old <- UngroupNames(df_scores4)
df_scores5 <- UngroupNames2(df_scores4)
df_scores5b = df_scores5 %>% group_by(SeqID) %>% mutate(EffectiveRows = length(unique(DisputeID ))) %>% ungroup()

df_scores6a = left_join(df_scores5b,  df_hebrew_rabbi_name_1 %>% select(English_Name, Hebrew_Name_1), by=c("Disputant_1"="English_Name"))
# df_scores6a$Disputant_Hebrew_D1 <- df_scores6a$Hebrew_Name_1                          
df_scores6a <- df_scores6a %>% rename(Disputant_Hebrew_D1 = Hebrew_Name_1)

df_scores6b = left_join(df_scores6a, df_hebrew_rabbi_name_1 %>% select(English_Name, Hebrew_Name_1), by=c("Disputant_2"="English_Name"))
#df_scores6b$Disputant_Hebrew_D2 <- df_scores6b$Hebrew_Name_1
df_scores6b <- df_scores6b %>% rename(Disputant_Hebrew_D2 = Hebrew_Name_1)

df_scores6b2 = left_join(df_scores6b, df_hebrew_rabbi_name_1 %>% select(English_Name, Hebrew_Name_2), by=c("Disputant_1"="English_Name"))
#df_scores6b2$Disputant_Hebrew_D1b <- df_scores6b2$Hebrew_Name_1
df_scores6b2 <- df_scores6b2 %>% rename(Disputant_Hebrew_D1b = Hebrew_Name_2)

df_scores6b3 = left_join(df_scores6b2, df_hebrew_rabbi_name_1 %>% select(English_Name, Hebrew_Name_2), by=c("Disputant_2"="English_Name"))
#df_scores6b3$Disputant_Hebrew_D2b <- df_scores6b3$Hebrew_Name_1
df_scores6b3 <- df_scores6b3 %>% rename(Disputant_Hebrew_D2b = Hebrew_Name_2)


df_scores6b4 = left_join(df_scores6b3, df_hebrew_rabbi_name_1 %>% select(English_Name, Hebrew_Name_3), by=c("Disputant_1"="English_Name"))
# df_scores6b4$Disputant_Hebrew_D1c <- df_scores6b4$Hebrew_Name_1
df_scores6b4 <- df_scores6b4 %>% rename(Disputant_Hebrew_D1c = Hebrew_Name_3)

df_scores6b5 = left_join(df_scores6b4, df_hebrew_rabbi_name_1 %>% select(English_Name, Hebrew_Name_3), by=c("Disputant_2"="English_Name"))
# df_scores6b5$Disputant_Hebrew_D2c <- df_scores6b5$Hebrew_Name_1
df_scores6b5 <- df_scores6b5 %>% rename(Disputant_Hebrew_D2c = Hebrew_Name_3)


# df_scores6b$Disputant_Hebrew_D1 <- df_scores6b$Hebrew_Name_1.x                          
#df_scores6b$Disputant_Hebrew_D2 <- df_scores6b$Hebrew_Name_1.y

# df_scores6c <- df_scores6b %>% select(-Hebrew_Name_1.x, -Hebrew_Name_1.y)
df_scores6c <- df_scores6b5
# View(df_scores6c %>% select(Disputant_1, Disputant_Hebrew_D1, Disputant_2, Disputant_Hebrew_D2))


df_scores6d = left_join(df_scores6c, df_mishna1, by=c("tab"="tab", "Location"="NumberChapterLocation"))


df_scores6e <- df_scores6d
df_scores6e$IsNaText = is.na(df_scores6e$MishnaText)

# Initialize the fields before filling them
df_scores6e$ExcelTabName            = ""
df_scores6e$MishnaText              = ""
df_scores6e$ReverseExcelTabName     = ""
df_scores6e$MishnahKey              = ""
df_scores6e$HebrewChapterLocation   = ""
df_scores6e$FileSource              = ""

# For every row in the data, put together the misha text associated with that argument. 
for (i in 1:dim(df_scores6e)[1]){
  if (is.na(df_scores6e[i, "MishnaText"])){
    tmptab = df_scores6e[i, "tab"]
    tmplocation = df_scores6e[i, "Location"]
    tmpdf = PullMishnaLines(df_mishna1, tmptab, ParseRefList(tmplocation))
    #print(paste(tmptab, tmplocation))
    df_scores6e[i, "MishnaText"] = paste(tmpdf$MishnaText, collapse = "...")  # Paste together the rows.
    df_scores6e[i, "ExcelTabName"] = tmpdf$ExcelTabName[1]  
    df_scores6e[i, "ReverseExcelTabName"] = tmpdf$ReverseExcelTabName[1] 
    df_scores6e[i, "MishnahKey"] =  paste(tmpdf$MishnahKey, collapse=", ") 
    df_scores6e[i, "HebrewChapterLocation"] = paste(tmpdf$HebrewChapterLocation, collapse="; ")
    df_scores6e[i, "FileSource"] = tmpdf$FileSource[1]  


    print(paste(i, tmptab,  tmplocation))
  }
}

# Row by row GREPL: https://stackoverflow.com/questions/45442349/how-to-use-grepl-in-every-row
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1 <- mapply(grepl, df_scores6e$Disputant_Hebrew_D1, df_scores6e$MishnaText) 
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2 <- mapply(grepl, df_scores6e$Disputant_Hebrew_D2, df_scores6e$MishnaText) 

df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1b <- mapply(grepl, df_scores6e$Disputant_Hebrew_D1b, df_scores6e$MishnaText) 
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2b <- mapply(grepl, df_scores6e$Disputant_Hebrew_D2b, df_scores6e$MishnaText) 

df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1c <- mapply(grepl, df_scores6e$Disputant_Hebrew_D1c, df_scores6e$MishnaText) 
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2c <- mapply(grepl, df_scores6e$Disputant_Hebrew_D2c, df_scores6e$MishnaText) 


# Replace NA with FALSE
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1 <- df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1 %>% replace_na(FALSE)
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2 <- df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2 %>% replace_na(FALSE)
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1b <- df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1b %>% replace_na(FALSE)
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2b  <- df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2b  %>% replace_na(FALSE)
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1c  <- df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1c  %>% replace_na(FALSE)
df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2c <- df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2c %>% replace_na(FALSE)


df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1_any <- 
  df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1 | 
  df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1b | 
  df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_1c

df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2_any <- 
  df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2 |
  df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2b | 
  df_scores6e$Is_Hebrew_Disputant_Name_In_Mishna_Text_2c



# View(df_scores6 %>% group_by(tab, is.na(MishnaText)) %>% tally())
# View(df_scores6 %>% filter(Disputant_1 == "Mishna Rishona") %>% select(Disputant_1, Disputant_2, tab, Location, MishnaText))

# View(df_scores6 %>% select(Disputant_1, Disputant_Hebrew_D1, Disputant_2, Disputant_Hebrew_D2, tab, Location, MishnaText))
# View(df_scores6 %>% group_by(tab, is.na(MishnaText)) %>% tally() %>% pivot_wider(names_from = "is.na(MishnaText)", values_from = n))

#View(df_scores6 %>% group_by(Disputant_1, is.na(Disputant_Hebrew_D1)) %>% tally() %>% pivot_wider(names_from = "is.na(Disputant_Hebrew_D1)", values_from = n))
# df_scores6 
# wb2 <- createWorkbook()
# ## Add worksheets
# addWorksheet(wb2, "Scores")
# writeData(wb2, "Scores", df_scores6, rowNames = TRUE)
# saveWorkbook(wb2, "scores_with_text.xlsx", overwrite = TRUE)
# df_mishna1 %>% filter(tab=="כלאים4", NumberChapterLocation=="4:9")
# df_mishna1 %>% filter(is.na("MishnaText"))
# readr::write_csv(df_scores6e, "df_scores6e.csv")
# CSV won't print unicode properly but xlsx will!

df_scores6 <- df_scores6e

# Get EffectiveRows from df_scores6 and merge back into df_scores2

df_effective_rows = df_scores6 %>% select(EffectiveRows, SeqID) %>% group_by(SeqID) %>% summarise(n_entries=n(), EffectiveRows = median(EffectiveRows), countunique = n_distinct(EffectiveRows))
df_scores2_with_effective_rows = df_scores2 %>% left_join(df_effective_rows %>% select(EffectiveRows, SeqID), by="SeqID")


print(df_scores6 %>% filter(!IsCopy) %>% dim())
print(df_scores6  %>% dim())