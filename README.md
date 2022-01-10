# MishnaBase
MishnaBase

This is the database of Mishnaic disputes and constituant disputes used in our Kazhdan and <a href="https://sites.google.com/site/benjaminkay/home">Kay</a>  (R&R at JSIJ) working paper.\* 

In addition to downloading the database here on <a href="https://github.com/BenjaminKay/MishnaBase/">github</a>, you can see the database in action: <a href="https://mishnabase.glitch.me/">here</a>. 


Broadly speaking, our database is like the US Supreme Court database  and similar databases of other apex courts. These databases are widely used by legal scholars who wish to make claims about broad trends in judicial decisions (Weinshall and Epstein 2020). The Supreme Court database includes, inter alia, (a) information on which Supreme Court justices are involved in a decision, (b) the direction of the justices’ opinions (e.g., pro- or anti-defendant), and (c) which justices prevail. Our respective analogy for the Mishnah is to code (a) which Tannaim are involved in a decision, (b) the direction of the Tanna’s opinions (e.g., strict or lenient), and (c) which Tanma prevails.  We manually went through the Mishnah, coded every argument along the various parameters described above, and then used computers to collate the data to show who argues with whom and what attitudes they tended to take in these arguments. 

For a detailed discussion of the methodology with which we assembled this database, please see appendix A of our paper. 

\* - The views and opinions expressed in this article are those of the authors and do not reflect the views or opinions of their employers. 


Arguments Data Dictionary

| Score_N              | Disputant(s) N wins the   argument according to Maimonides (1) or loses (0).                                                                                                                             |
|----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Winner               | Which disputant(s), by name, won the argument according to   Maimonides.                                                                                                                                 |
| Result_N             | Relative argument positions like Chumra 1.0 / Kula 0.0 (higher   number is stricter), BHB 1.0 / 0.0, Chayav 1.0 / Patur 0.0, and a few less   common topics have separate scores.                        |
| Seder_Hebrew         | Name of seder of the Talmud in which the argument appears,   written in Hebrew.                                                                                                                          |
| Seder_Name           | Name of seder of the Talmud in which the argument appears,   Hebrew name in English transliteration.                                                                                                     |
| Seder_Number         | Which of the six seders of the Talmud the argument appears in.                                                                                                                                           |
| Seder_Translate      | English translation of which of the six seders of the Talmud   the argument appears in.                                                                                                                  |
| SeqID                | Unique argument ID.                                                                                                                                                                                      |
| tab                  | Hebrew name (and number of order) of which of the 63 masechtot   / books of the Talmud where the argument is appears.                                                                                    |
| tabnum               | Which of the 63 masechtot / books of the Talmud where the   argument is appears.                                                                                                                         |
| Tractate_Name_Hebrew | Hebrew name of which of the 63 masechtot / books of the Talmud   where the argument is appears.                                                                                                          |
| IsCopy               | Is this the second or later appearance of a dispute in the   Talmud? Always false because duplicate arguments are now filtered out. First   appearance only.                                             |
| NumDisp              | Number of distinct disputant positions contained within this   argument. This is not the combinations of disputes or a count of all possible   combinations of disputants. Also see field EffectiveRows. |
| EffectiveRows        | The number of disputes contained within the argument. All   rabbis involved in a dispute are paired up with all possible other rabbis who   are involved in the dispute but take a different position.   |
| Location             | Combination of chapter and line number(s) within that chapter   where the dispute occurs.                                                                                                                |
| keep                 | Argument meets data quality checks (database only contains   values where this is TRUE).                                                                                                                 |
| OweQuestion          | Is this a question about owing (1 if yes, 0 if no)?                                                                                                                                                      |
| StrictQuestion       | Is this a question about strictness (1 if yes, 0 if no)?                                                                                                                                                 |
| AltStrictQuestion    | Is this a question about strictness (1 if yes, 0 if no)?                                                                                                                                                 |
| MoneyQuestion        | Is this a question about owing money (1 if yes, 0 if no)?                                                                                                                                                |
| Owe_N                | Does disputant(s) N say they owe (1 if yes, 0 if no or not a   owe argument).                                                                                                                            |
| Strict_N             | Are disputant(s) N stricter (1 if yes, 0 if no or not a   strictness argument).                                                                                                                          |
| Money_N              | Does disputant(s) N say they owe money (1 if yes, 0 if no or   not a money owing argument).                                                                                                              |
| Owe_Sum              | Number of argument positions taking an owe position. Either 0   (not a owing argument) or 1 (an owing argument).                                                                                         |
| Strict_Sum           | Number of argument positions taking the strictest position.   Generally 0 (not a strictness argument) or 1 (one position is strictest), but   maybe be 2 or 3.                                           |
| Money_Sum            | Number of argument positions taking an money owing position.   Either 0 (not a money owing argument) or 1 (a money owing argument).                                                                      |
