##################################################################################################################
################################### Load data file & Concatenate all the tabs
##################################################################################################################
print("Load data and combine all tabs")
sheetlist = getSheetNames(paste0(InputProjectPathStr, SourceFile))
i = length(sheetlist)
for (tab in sheetlist){
  print(tab)
  df_tmp = read.xlsx(xlsxFile=paste0(InputProjectPathStr, SourceFile), sheet=tab)
  # Actually concatenate the tabs
  df_tmp$tab <- tab
  # df_tmp$tabnum <- i # Old way counts digits
  df_tmp$tabnum <- keepdigits(tab) # New way reads off name
  if (i == length(sheetlist)) {
    df_scores  = df_tmp
  } else {
    df_scores = rbind(df_scores, df_tmp)
  }
  i = i - 1   
}  

#sheetlist = getSheetNames("Arguments in the Mishna 2020_5_18.xlsx")
# sheetlist = getSheetNames("Arguments in the Mishna 2020_5_18b.xlsx")
# sheetlist2 = getSheetNames("Arguments in the Mishna 2020_5_17.xlsx")
# i = length(sheetlist)
# for (tab in sheetlist){
#   if (sheetlist[i]!=sheetlist2[i]) {
#     print(paste("sheetlist1          ", sheetlist[i]))
#     print(paste("sheetlist2 (correct)", sheetlist2[i]))
#   }
#     i = i - 1   
# }

#df_seder_info = read.xlsx(xlsxFile="talmud_chapter_info.xlsx", sheet="info")

df_seder_info = read.xlsx(xlsxFile=paste0(InputProjectPathStr, SederFile), sheet="info")
df_seder_info$Tractate_Name_Hebrew = df_seder_info$Chapter_Name_Hebrew

df_scores_with_seder_info = merge(x=df_scores, y=df_seder_info, by.x="tabnum", by.y="Chapter_Key")


df_hebrew_rabbi_name_0 = read.xlsx(xlsxFile=paste0(InputProjectPathStr, RabbiNameListFile), sheet="Full Rabbi List")
df_hebrew_rabbi_name_1 = df_hebrew_rabbi_name_0 %>% filter(Filter.Out==FALSE) %>% select(-Filter.Out, -Order)

# This file is made with 200831_talmud_mishnah_search_v3.r
# contains ChapterRowKey  MishnaText  FileSource  Chapter ExcelTabNumber  ExcelTabName  ReverseExcelTabName MishnahKey  HebrewChapterLocation commaloc  NumberChapterLocation

df_mishna0 = read.xlsx(xlsxFile=paste0(InputProjectPathStr, MishnaCorpusFile), sheet="Mishnah")
df_mishna0$tab = paste(df_mishna0$Chapter, df_mishna0$ExcelTabNumber, sep="")
df_mishna1 = df_mishna0 %>% select(-"", -Chapter, -ExcelTabNumber)


# splits <- strsplit(dna, "")[[1]]
# reversed <- rev(splits)
# final_result <- paste(reversed, collapse = "")




  # ZERAIM. Contains eleven books or Masechtoth.
  # MOED. Contains twelve Books or Masechtoth.                                           
  # NASCHIM. Contains seven Books or Masechtoth. 
  # NEZIKIN. Contains ten Books or Masechtoth. Nezikin
  # KODASCHIM. Contains eleven Books or Masechtoth. Kodashim
  # TOHOROTH. Contains twelve Books or Masechtoth.


# Do any columns contain question marks?
df_scores_with_seder_info %>% filter_all(any_vars(grepl("\\?",.)))

# Old way included anything with a copy, but now "copy original" indicates it is the non-copy of first or primary appearance
#df_scores_with_seder_info$IsCopy        = grepl("copy", (df_scores_with_seder_info$Comment), ignore.case = TRUE)

df_scores_with_seder_info$IsInsideCopy  = grepl("copy inside", (df_scores_with_seder_info$Comment), ignore.case = TRUE)
df_scores_with_seder_info$IsOutsideCopy = grepl("copy outside", (df_scores_with_seder_info$Comment), ignore.case = TRUE)
df_scores_with_seder_info$IsCopy        = df_scores_with_seder_info$IsInsideCopy | df_scores_with_seder_info$IsOutsideCopy

df_scores_with_seder_info_copies_only = df_scores_with_seder_info %>% filter(IsCopy)


#########################################################################################
#################################### Clean up woman and slave results
#########################################################################################
print("Clean up entries to have standardized numbers and remove Women and Slave confounding data")
df_scores0 <- Clean_Results(df_scores_with_seder_info,  "Result_1")
df_scores0 <- Clean_Results(df_scores0, "Result_2")
df_scores0 <- Clean_Results(df_scores0, "Result_3")
df_scores0 <- Clean_Results(df_scores0, "Result_4")
df_scores0 <- Clean_Results(df_scores0, "Result_5")
df_scores0 <- Clean_Results(df_scores0, "Result_6")

# clean out the "<#>/y" into "<#>" and fill down to make chapters searchable
df_scores0$Chapter = as.numeric(dropalpha(droppunct(df_scores0$Chapter)))
df_scores0 = df_scores0 %>% fill("Chapter", .direction = "down")

print(df_scores_with_seder_info %>% filter(!IsCopy) %>% dim())
print(df_scores_with_seder_info  %>% dim())
print(df_scores0 %>% filter(!IsCopy) %>% dim())
print(df_scores0  %>% dim())


# Is it a copy? If so delete
# df_scores0 <- df_scores0 %>% filter(!IsCopy)

# The old way was to drop if it is a copy
# Is it a copy? If so delete
# df_scores0 <- df_scores0 %>% filter(!IsCopy)
# Now we will filter based on a copy if it is mishah-wide and otherwise filter out inside copies

df_tbl4_nway_arguments = read.xlsx(xlsxFile=paste0(InputProjectPathStr, table_4_nway_coded_argumentsFile), sheet="Effective Rows >1")
