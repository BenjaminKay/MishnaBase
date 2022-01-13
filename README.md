# MishnaBase

## Citation: 
Kazhdan, Daniel and Benjamin S. Kay. "Unlocking Ancient Texts with New Tools: A Data-Centered Study of the Mishnah." Jewish Studies, an Internet Journal (forthcoming).

## Overview
This is the database of Mishnaic disputes and constituant disputes used in our Kazhdan and <a href="https://sites.google.com/site/benjaminkay/home">Kay</a>  (R&R at <a href="https://jewish-faculty.biu.ac.il/en/JSIJ"> JSIJ</a>) working paper.\* In addition to downloading the database here on <a href="https://github.com/BenjaminKay/MishnaBase/">github</a>, you can see the database in action: <a href="https://mishnabase.glitch.me/">here</a>. 

## Description of Data
Broadly speaking, our database is like the <a href="http://scdb.wustl.edu/">US Supreme Court database</a> and <a href="https://www.ruf.rice.edu/~pbrace/statecourt/">similar</a> <a href="http://artsandsciences.sc.edu/poli/juri/highcts.htm">databases</a>  of other apex courts. These databases are <a href="https://libguides.princeton.edu/c.php?g=916665&p=6607543">widely used</a> by legal scholars who wish to make claims about broad trends in judicial decisions. The Supreme Court database includes, inter alia, (a) information on which Supreme Court justices are involved in a decision, (b) the direction of the justices’ opinions (e.g., pro- or anti-defendant), and (c) which justices prevail. Our respective analogy for the Mishnah is to code (a) which Tannaim are involved in a decision, (b) the direction of the Tanna’s opinions (e.g., strict or lenient), and (c) which Tanma prevails.  We manually went through the Mishnah, coded every argument along the various parameters described above, and then used computers to collate the data to show who argues with whom and what attitudes they tended to take in these arguments. 

For a detailed discussion of the methodology with which we assembled this database, please see appendix A of our paper. 

\* - The views and opinions expressed in this article are those of the authors and do not reflect the views or opinions of their employers. 


## Data Dictionaries
### Arguments (<a href="https://github.com/BenjaminKay/MishnaBase/blob/main/arguments.csv">arguments.csv</a>) Data Dictionary


| Field                | Description                                                                                                                                                                                                                                             |
|----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Chapter              | Which chapter number of the masechtot where the argument   appears.                                                                                                                                                                                     |
| Ith_chapter_in_seder | Which chapter number of the seder where the argument appears.                                                                                                                                                                                           |
| Comment              | Miscellaneous notes on the dispute not fitting into other   variables.                                                                                                                                                                                  |
| Disputant_N          | The rabbi(s) offering the nth position in the argument.                                                                                                                                                                                                 |
| Score_N              | Disputant(s) N wins the argument according to Maimonides (1)   or loses (0).                                                                                                                                                                            |
| Winner               | Which disputant(s), by name, won the argument according to   Maimonides.                                                                                                                                                                                |
| Result_N             | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher   number is stricter), BHB 1.0 / 0.0, Chayav 1.0 / Patur 0.0, and a few less   common topics have separate scores.                                                                       |
| Seder_Hebrew         | Name of seder of the Talmud in which the argument appears,   written in Hebrew.                                                                                                                                                                         |
| Seder_Name           | Name of seder of the Talmud in which the argument appears,   Hebrew name in English transliteration.                                                                                                                                                    |
| Seder_Number         | Which of the six seders of the Talmud the argument appears in.                                                                                                                                                                                          |
| Seder_Translate      | English translation of which of the six seders of the Talmud   the argument appears in.                                                                                                                                                                 |
| SeqID                | Unique argument ID.                                                                                                                                                                                                                                     |
| tab                  | The Hebrew name (and number of order) of the 63 masechtot in   which the argument is appears.                                                                                                                                                           |
| tabnum               | Which of the 63 masechtot / books of the Talmud where the   argument appears in.                                                                                                                                                                        |
| Tractate_Name_Hebrew | Which of the 63 masechtot / books of the Talmud where the   argument appears in, name in Hebrew.                                                                                                                                                        |
| IsCopy               | Is this the second or later appearance of a dispute in the   Mishnah? Always false because duplicate arguments are now filtered out. First   appearance only.                                                                                           |
| NumDisp              | Number of distinct disputant positions contained within this   argument. This is not the combinations of disputes or a count of all possible   combinations of disputants. Also see field EffectiveRows.                                                |
| EffectiveRows        | The number of disputes contained within the argument. All   rabbis involved in a dispute are paired up with all possible other rabbis who   are involved in the dispute but take a different position.                                                  |
| Location             | Combination of chapter and line number(s) within that chapter   where the dispute occurs.                                                                                                                                                               |
| keep                 | Argument meets data quality checks (database only contains   values where this is TRUE).                                                                                                                                                                |
| OweQuestion          | Is this a question about owing (1 if yes, 0 if no)?                                                                                                                                                                                                     |
| StrictQuestion       | Is this a question about strictness (1 if yes, 0 if no)?                                                                                                                                                                                                |
| AltStrictQuestion    | Same as StrictQuestion                                                                                                                                                                                                                                  |
| MoneyQuestion        | Is this a question about owing money (1 if yes, 0 if no)?                                                                                                                                                                                               |
| Owe_N                | Does disputant(s) N say they owe (1 if yes, 0 if no or not a   owe argument).                                                                                                                                                                           |
| Strict_N             | Are disputant(s) N stricter (1 if yes, 0 if no or not a   strictness argument).                                                                                                                                                                         |
| Money_N              | Does disputant(s) N say they owe money (1 if yes, 0 if no or   not a money owing argument).                                                                                                                                                             |
| Owe_Sum              | Number of argument positions taking an owe position. Either 0   (not a owing argument) or 1 (an owing argument).                                                                                                                                        |
| Strict_Sum           | Number of argument positions taking the strictest position.   Generally 0 (not a strictness argument) or 1 (one position is strictest), but   it can be more than 1 if there is a multi-way argument and more than one Tana   takes the strictest view. |
| Money_Sum            | Number of argument positions taking an money owing position.   Generally 0 (not a money owing argument) or 1 (a money owing argument), but   it can be more than 1 if there is a multi-way argument and more than one Tana   takes the strictest view.  |

Created with <a href="https://www.tablesgenerator.com/markdown_tables">Tables Generator</a>


### Disputes (<a href="https://github.com/BenjaminKay/MishnaBase/blob/main/disputes.csv">disputes.csv</a>) Data Dictionary

| Field                 | Description                                                                                                                                                                                            |
|-----------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| DisputeID             | A unique dispute ID. A signle argument can contain multiple   disputes.                                                                                                                                |
| Location              | Combination of chapter and line number(s) within that chapter   where the dispute occurs.                                                                                                              |
| Disputant_Hebrew_DNX  | Disputant number N's name in Hebrew, possibly in one of X   different versions.                                                                                                                        |
| Score_N               | Disputant(s) N wins the argument according to Maimonides (1)   or loses (0).                                                                                                                           |
| Money_N               | Does disputant(s) N say they owe money (1 if yes, 0 if no or   not a money owing argument).                                                                                                            |
| Seder_Translate       | English translation of which of the six seders of the Talmud   the argument appears in.                                                                                                                |
| OweQuestion           | Is this a question about owing (1 if yes, 0 if no)?                                                                                                                                                    |
| MoneyQuestion         | Is this a question about owing money (1 if yes, 0 if no)?                                                                                                                                              |
| StrictQuestion        | Is this a question about strictness (1 if yes, 0 if no)?                                                                                                                                               |
| Seder_Name            | Name of seder of the Talmud in which the argument appears,   Hebrew name in English transliteration.                                                                                                   |
| Seder_Hebrew          | Name of seder of the Talmud in which the argument appears,   written in Hebrew.                                                                                                                        |
| Result_N              | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher   number is stricter), BHB 1.0 / 0.0, Chayav 1.0 / Patur 0.0, and a few less   common topics have separate scores.                      |
| tab                   | The Hebrew name (and number of order) of the 63 masechtot in   which the argument is appears.                                                                                                          |
| EffectiveRows         | The number of disputes contained within the argument. All   rabbis involved in a dispute are paired up with all possible other rabbis who   are involved in the dispute but take a different position. |
| Disputant_N           | The rabbi(s) offering the nth position in the argument.                                                                                                                                                |
| SeqID                 | Unique argument ID.                                                                                                                                                                                    |
| Chapter               | Which chapter number of the masechtot where the argument   appears.                                                                                                                                    |
| Ith_chapter_in_seder  | Which chapter number of the seder where the argument appears.                                                                                                                                          |
| Winner                | Which disputant(s), by name, won the argument according to   Maimonides.                                                                                                                               |
| Tractate_Name_Hebrew  | Which of the 63 masechtot / books of the Talmud where the   argument appears in, name in Hebrew.                                                                                                       |
| tabnum                | Which of the 63 masechtot / books of the Talmud where the   argument appears in.                                                                                                                       |
| Seder_Number          | Which of the six seders of the Talmud the argument appears in.                                                                                                                                         |
| ChapterRowKey         | Row in source text file for matching Mishnah text from the Maagar Sifrut ha-Kodesh (Mamre Institute)&dagger;.                                                                                                                   |
| FileSource            | Mishnah text source file  from the Maagar Sifrut ha-Kodesh (Mamre Institute)&dagger;.                                                                 |
| HebrewChapterLocation | Using Hebrew numbering, what is the chapter location of the dispute.                                                                                                                                 |
| MishnahKey            | Line number (entire Mishnah, not restarting by seder) sourced from the Maagar Sifrut ha-Kodesh (Mamre Institute)&dagger; and author calculations.
                                                                                    |
| Strict_Result_N       | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher number is stricter), BHB 1.0 / 0.0 (lower is stricter).                                                                               |
| Strict_Result_Score_N | Numeric portion of Strict_Result_N|

Created with <a href="https://www.tablesgenerator.com/markdown_tables">Tables Generator</a>


&dagger; - Digital text copy of the Mishnah used for error checking and other research purposes from the Mamre Institute's Maagar Sifrut ha-Kodesh, available at  https://kodesh.snunit.k12.il/b/h/h1.htm, 2009. 
