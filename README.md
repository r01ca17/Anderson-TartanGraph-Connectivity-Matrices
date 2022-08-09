# Anderson-TartanGraph-Connectivity-Matrices
The supplementary material for the manuscript by Anderson et al. Contains the raw data from in-silico experiments and the R code used to analyse data and produce the graphs presented in Anderson et al.

UPDATED: 09/08/2022

TARTANGRAPH_CODE zipped folder added as part of revisions made to Anderson et al. manuscript. Contains collated data produced to calculate a new response variable of TartanGraph connectivity as suggested by a reviewer.

The Updated R script enclosed demonstrates how raw data was collated into summary files calculating mean Traversal Probabilities under different treatments and how to produce the four figures in the revised Anderson et al. manuscript. 

Note: The raw data is NOT included due to size constraints. As such it is not possible at this time to run the data collation part of the code to convert RangeShifter outputs into collated data. 
Even so, it should still be demonstrable from the text of the R script how raw data was intially read into RStudio and compiled in order to create the three excel files in folder "Traversal Probability Results".

Using the files in "traversal Probability Results" is should be possible to run parts 2 onward in the R script to build the four figures that appear in Anderson et al (2022).






OUTDATED MATERIAL: Anderson_Supp_Material.zip

Zipped folder contains the following:

SMS_Inputs - TartanGraphs: The inputs for the RangeShifter dispersal model; the text files encoding the structure of stylised TartanGraph landscapes as well as a word file with pictures of said landscapes.

SMS_Outputs1_Edge_Number_Analysis: Raw data from the first experiment examining the effect of edge number on TartanGraph connectivity.

SMS_Outputs2_Edge_Width_Analysis: Raw data from the second and third experiemnts examining the effect of edge width on TartanGraph connectivity.

R Script_Anderson et al: The R code used to analyse raw data and produce Anderson et al figures. Directory set up in advance to access data in the two output folders described above.

Results - Connectivity parameters (Freq): Word file containing summary statistics for connectivity measures returned by the RangeShifter simulations.

R-Script_Anderson-et-al: Chrome HTML file, a compiled report for the R code included in this folder.
