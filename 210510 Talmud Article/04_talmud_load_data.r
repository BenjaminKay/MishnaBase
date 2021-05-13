##################################################################################################################
################################### Load data file & Concatenate all the tabs
##################################################################################################################
print("Load data and combine all tabs")
sheetlist = getSheetNames(SourceFile)
i = length(sheetlist)
for (tab in sheetlist){
  print(tab)
  df_tmp = read.xlsx(xlsxFile=SourceFile, sheet=tab)
  # Actually concatenate the tabs
  df_tmp$tab <- tab
  df_tmp$tabnum <- i
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

df_seder_info = read.xlsx(xlsxFile="talmud_chapter_info.xlsx", sheet="info")
df_scores_with_seder_info = merge(x=df_scores, y=df_seder_info, by.x="tabnum", by.y="Chapter_Key")


df_hebrew_rabbi_name_0 = read.xlsx(xlsxFile="Full rabbi list English Hebrew Crosswalk v2 DK.xlsx", sheet="Full Rabbi List")
df_hebrew_rabbi_name_1 = df_hebrew_rabbi_name_0 %>% filter(Filter.Out==FALSE) %>% select(-Filter.Out, -Order)


df_mishna0 = read.xlsx(xlsxFile="mishnah_v3.xlsx", sheet="Mishnah")
df_mishna0$tab = paste(df_mishna0$Chapter, df_mishna0$ExcelTabNumber, sep="")
df_mishna1 = df_mishna0 %>% select(-"", -Chapter, -ExcelTabNumber)


# splits <- strsplit(dna, "")[[1]]
# reversed <- rev(splits)
# final_result <- paste(reversed, collapse = "")




  # ZERAIM. Contains eleven books or Masechtoth.
  # MOED. Contains twelve Books or Masechtoth.                                           
  # NASCHIM. Contains seven Books or Masechtoth.
  # NEZIKIN. Contains ten Books or Masechtoth.
  # KODASCHIM. Contains eleven Books or Masechtoth.
  # TOHOROTH. Contains twelve Books or Masechtoth.


# Do any columns contain question marks?
df_scores_with_seder_info %>% filter_all(any_vars(grepl("\\?",.)))

df_scores_with_seder_info$IsCopy = grepl("copy", (df_scores_with_seder_info$Comment), ignore.case = TRUE)




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

# Is it a copy? If so delete
df_scores0 <- df_scores0 %>% filter(!IsCopy)