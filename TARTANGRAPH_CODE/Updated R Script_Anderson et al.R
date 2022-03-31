#' **SUPPLEMENTARY MATERIAL - R CODE FOR ANDERSON ET AL.**

# Code for the data analysis and production of figures 
#  utilised in Anderson et al. 

# Written in R script. Originally run on RStudio (R version 3. 6. 3)

# CONTENTS
# PART 1: RAW DATA COLLATION
# PART 2: CODE FOR FIGURE 2
# PART 3: CODE FOR FIGURE 3
# PART 4: CODE FOR FIGURE 4 
# PART 5: CODE FOR FIGURE 5

# =================================================================

#' **PART 1 : RAW DATA COLLATION**

# The following code was used to quantify connectivity of 
#  TartanGraphs using individual-based data produced by the
#   RangeShifter dispersal modeling platform and compile the
#    results into the three csv files found in the "Dispersal 
#     Probability Results" folder.

# This code creates data sets that allow comparison of 
#  connectivity parameters with five independent variables: 
#   number of "connecting-edges", number of "transecting-edges", 
#    the width of either edge type and directional persistence.

# WARNING: THE OUTPUT FILES OF RANGESHIFTER DETAIL SPECIFIC TRAITS
#  OF ALL INDIVIDUALS FOR EVERY SIMULATED YEAR, HENCE THE FILES
#   ARE VERY LARGE AND RUNNING THE BELOW CODE REQUIRES ~1.5
#    HOURS TO COMPLETE.
#     RUNNING THE DATA COLLATION CODE IS NOT NECESSARY TO RUN THE CODE
#      FOR BUILDING THE FOUR FIGURES AS THEY USE THE ALREADY COLLATED
#       DATA SETS FOUND IN THE "Traversal Probability Results" FOLDER

# ====

# Set directory to examine the relevant RangeShifter outputs

setwd("C:/ ... /TARTANGRAPH_CODE/RangeShifter Outputs")

# The code that follows demonstrates how the raw data outputs of
#  RangeShifter simulations were collated and used to quantify,
#   for different experimental treatments, the inter-patch connectivity
#    of TartanGraph systems using three response variables:
#
#  1. Inter-patch Disperses: the number of individuals that dispersed
#      from natal habitat patches into novel patches.

#  2. Failed Disperses: the number of individuals that dispersed from
#      natal patches and expired mid-transfer in LLE corridors.

#  3. Traversal Probability: the likelihood of any individual, upon
#      leaving natal patches, undergoing inter-patch dispersal.

# Data collation code is divided into three sections for each Experimental
#  Set of TartanGraphs used to answer different aims of our investigation.
#   See Table 1 in Anderson et al. for more details about the treatments 
#    employed and hypotheses tested in each Experimental Sets.


### EXPERIMENTAL SET ONE ####

# The 25 TartanGraphs of Experimental Set One were designed to examine the
#  effect of LLE number and orientation on inter-patch Traversal Probability.

# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.1 - FIRST REPLICATES

# Read individual-based data of five TartanGraphs at D.P. 5.0
Res1<-read.table("Sim10150_Rep0_Inds.txt", header=TRUE)

# Exclude all individuals who have never left their natal patches
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]

# Exclude the first ten years of simulation, covers the initialization period
Res1<-subset(Res1, Year > 9)

# Add field detailing the number of connecting-edges in these TartanGraphs
Res1$CE<-3

# Repeat for all following individual-based data files
Res2<-read.table("Sim10250_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5

Res3<-read.table("Sim10350_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9

Res4<-read.table("Sim10450_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

Res5<-read.table("Sim10550_Rep0_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

# Combine all observations into a single file
ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

# Create a data frame matching the different patch IDs of the five 
#  TartanGraphs to the different numbers of transecting-edges they contained
TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))



# To begin quantifying the number of inter-patch disperses, first
#  exclude all individuals who did not find their way to novel 
#   patches at the end of the year they initiated dispersal
InterPatch<-ResT[!(ResT$PatchID==0),]

# Of successful inter-patch disperses, exclude settled individuals
#  who dispersed in a previous year
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]

# Match each remaining individual to the transecting-edge treatment
#  of their TartanGraph
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

# Quantify the number of individuals who underwent inter-patch
#  dispersal in different TartanGraphs based on relevant experimental
#   treatments
InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  



# To quantify the number of individuals that initiated patch-egress
#  but failed to locate novel habitat before mortality, subset individuals
#   recorded in no habitat patch at end of simulated year
Failures<-ResT[(ResT$PatchID==0),]                              

# Match each remaining individual to the transecting-edge treatment
#  of their TartanGraph 
Failures<-merge(Failures, TranEd, by="Natal_patch")

# Quantify the number of individuals who perished mid-transfer in 
#  different TartanGraphs based on relevant experimental treatments
FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         



# For convenience, add failed disperser data as anew field to inter-patch 
#  disperser data set
InterPaTable$Failed<-FailedTable$Freq  

# Calculate inter-patch Traversal Probability by dividing inter-patch
#  by the total number of individuals who left natal patches
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

# Compile the final result of this first section
Rep1<-InterPaTable

# This code is largely replicated for the following 27 sections. 
#  Changes from this intial format will be indicated.


# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.2 - SECOND REPLICATES

Res1<-read.table("Sim10150_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10250_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10350_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10450_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10550_Rep1_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


       
InterPatch<-ResT[!(ResT$PatchID==0),]              
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable



# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.3 - THIRD REPLICATES 

Res1<-read.table("Sim10150_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10250_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10350_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10450_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10550_Rep2_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]              
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable

# Once data for TartanGraphs across all three replicates is compiled,
#  combine their results in a single data set
Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)

# Calculate the mean Traversal Probability of each TartanGraph across
#  three replicate simulations
Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
# Calculate standard deviation of Traversal Probabilities
Data$SD<-apply(Data[1:25,9:11], 1, FUN=sd)
# Calculate standard error of triple replicates
Data$SE<-Data$SD/(sqrt(3))

# This is our final data set for the 25 TartanGraphs of Experiemntal Set
#  One under a Directional Persistence treatment of 5.0
DP5<-Data
# Note the value of Directional Persistence in these simulations
DP5$DP<-5





# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.1 - FIRST REPLICATE

Res1<-read.table("Sim10175_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10275_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10375_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10475_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10575_Rep0_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable


# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.2 - SECOND REPLICATE

Res1<-read.table("Sim10175_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10275_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10375_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10475_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10575_Rep1_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable



# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.3 - THIRD REPLICATE 

Res1<-read.table("Sim10175_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10275_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10375_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10475_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10575_Rep2_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable


Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:25,9:11], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP7<-Data
DP7$DP<-7

# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.1 - FIRST REPLICATE

Res1<-read.table("Sim10199_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10299_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10399_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10499_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10599_Rep0_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable


# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.2 - SECOND REPLICATE

Res1<-read.table("Sim10199_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10299_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10399_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10499_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10599_Rep1_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable



# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.3 - THIRD REPLICATE 

Res1<-read.table("Sim10199_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim10299_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim10399_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim10499_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17
Res5<-read.table("Sim10599_Rep2_Inds.txt", header=TRUE)
Res5<-Res5[(Res5$Natal_patch!=Res5$PatchID),]
Res5<-subset(Res5, Year > 9)
Res5$CE<-33

ResT<-rbind(Res1, Res2, Res3, Res4, Res5)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11),     
                   TE=c(0,0,3,3,7,7,15,15,31,31))


InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable


Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:25,9:11], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP10<-Data
DP10$DP<-10

# Combine the results of the preceding nine sections to produce final
#  results of all simulations conducted on Experimental Set One
ExpSet01Res<-rbind(DP5, DP7, DP10)

# In this final object can be found the results stored in the excel file
#  "ExpSet01.csv" in the "Traversal Probability Results" folder






### EXPERIMENTAL SET TWO ####

# The 48 TartanGraphs of Experimental Set Two were designed to examine the
#  effect of connecting-edge width on inter-patch Traversal Probability.

# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.1 - FIRST REPLICATES

# Read individual-based data of twelve TartanGraphs at D.P. 5.0
Res1<-read.table("Sim10650_Rep0_Inds.txt", header=TRUE)

# Exclude all individuals who have never left their natal patches
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]

# Exclude the first ten years of simulation, covers the initialization period
Res1<-subset(Res1, Year > 9)

# Add field detailing the number of transecting-edges in these TartanGraphs
Res1$TE<-0

Res2<-read.table("Sim10750_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10850_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10950_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

# Create a data frame matching the different patch IDs of the twelve 
#  TartanGraphs to the different numbers and widths (in cells) of 
#   connecting-edges they contained
ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable

# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.2 - SECOND REPLICATES

Res1<-read.table("Sim10650_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10750_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10850_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10950_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable

# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.3 - THIRD REPLICATES

Res1<-read.table("Sim10650_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10750_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10850_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10950_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable

Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Wd=Rep1$Wd, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:48,10:12], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP5<-Data
DP5$DP<-5

# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.1 - FIRST REPLICATES

Res1<-read.table("Sim10675_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10775_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10875_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10975_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable

# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.2 - SECOND REPLICATES

Res1<-read.table("Sim10675_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10775_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10875_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10975_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable

# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.3 - THIRD REPLICATES

Res1<-read.table("Sim10675_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10775_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10875_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10975_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable

Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Wd=Rep1$Wd, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:48,10:12], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP7<-Data
DP7$DP<-7

# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.1 - FIRST REPLICATES

Res1<-read.table("Sim10699_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10799_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10899_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10999_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable

# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.2 - SECOND REPLICATES

Res1<-read.table("Sim10699_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10799_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10899_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10999_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable

# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.3 - THIRD REPLICATES

Res1<-read.table("Sim10699_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$TE<-0
Res2<-read.table("Sim10799_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$TE<-3
Res3<-read.table("Sim10899_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$TE<-7
Res4<-read.table("Sim10999_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$TE<-15

ResT<-rbind(Res1, Res2, Res3, Res4)

ConE<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25),     
                 CE=c(3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17,3,3,5,5,9,9,17,17),
                 Width=c(2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, ConE, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, ConE, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable

Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Wd=Rep1$Wd, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:48,10:12], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP10<-Data
DP10$DP<-10

ExpSet02Res<-rbind(DP5, DP7, DP10)

# In this final object can be found the results stored in the excel file
#  "ExpSet02.csv" in the "Traversal Probability Results" folder







### EXPERIMENTAL SET THREE ####

# The 36 TartanGraphs of Experimental Set Three were designed to examine the
#  effect of transecting-edge width on inter-patch Traversal Probability.

# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.1 - FIRST REPLICATES

# Read individual-based data of nine TartanGraphs at D.P. 5.0
Res1<-read.table("Sim11050_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11150_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11250_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11350_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable


# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.2 - SECOND REPLICATES

Res1<-read.table("Sim11050_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11150_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11250_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11350_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable



# SECTION 1.0 - DIRECTIONAL PERSISTENCE 5.0

# SECTION 1.3 - THIRD REPLICATES 

Res1<-read.table("Sim11050_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11150_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11250_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11350_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable


Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Wd=Rep1$Wd, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:36,10:12], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP5<-Data
DP5$DP<-5

# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.1 - FIRST REPLICATES

Res1<-read.table("Sim11075_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11175_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11275_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11375_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable


# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.2 - SECOND REPLICATES

Res1<-read.table("Sim11075_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11175_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11275_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11375_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable



# SECTION 2.0 - DIRECTIONAL PERSISTENCE 7.5

# SECTION 2.3 - THIRD REPLICATES 

Res1<-read.table("Sim11075_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11175_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11275_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11375_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable


Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Wd=Rep1$Wd, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:36,10:12], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP7<-Data
DP7$DP<-7.5


# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.1 - FIRST REPLICATES

Res1<-read.table("Sim11099_Rep0_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11199_Rep0_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11299_Rep0_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11399_Rep0_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep1<-InterPaTable


# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.2 - SECOND REPLICATES

Res1<-read.table("Sim11099_Rep1_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11199_Rep1_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11299_Rep1_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11399_Rep1_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep2<-InterPaTable



# SECTION 3.0 - DIRECTIONAL PERSISTENCE 10.0

# SECTION 3.3 - THIRD REPLICATES 

Res1<-read.table("Sim11099_Rep2_Inds.txt", header=TRUE)
Res1<-Res1[(Res1$Natal_patch!=Res1$PatchID),]
Res1<-subset(Res1, Year > 9)
Res1$CE<-3
Res2<-read.table("Sim11199_Rep2_Inds.txt", header=TRUE)
Res2<-Res2[(Res2$Natal_patch!=Res2$PatchID),]
Res2<-subset(Res2, Year > 9)
Res2$CE<-5
Res3<-read.table("Sim11299_Rep2_Inds.txt", header=TRUE)
Res3<-Res3[(Res3$Natal_patch!=Res3$PatchID),]
Res3<-subset(Res3, Year > 9)
Res3$CE<-9
Res4<-read.table("Sim11399_Rep2_Inds.txt", header=TRUE)
Res4<-Res4[(Res4$Natal_patch!=Res4$PatchID),]
Res4<-subset(Res4, Year > 9)
Res4$CE<-17

ResT<-rbind(Res1, Res2, Res3, Res4)

TranEd<-data.frame(Natal_patch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),     
                  TE=c(3,3,7,7,15,15,3,3,7,7,15,15,3,3,7,7,15,15),
                  Width=c(2,2,2,2,2,2,4,4,4,4,4,4,6,6,6,6,6,6))

InterPatch<-ResT[!(ResT$PatchID==0),]               
InterPatch<-InterPatch[!(InterPatch$Nsteps==0),]
InterPatch<-merge(InterPatch, TranEd, by="Natal_patch")

InterPaTable<-data.frame(table(CE=InterPatch$CE, TE=InterPatch$TE, Wd=InterPatch$Width))  


Failures<-ResT[(ResT$PatchID==0),]                              

Failures<-merge(Failures, TranEd, by="Natal_patch")

FailedTable<-data.frame(table(CE=Failures$CE, TE=Failures$TE, Wd=Failures$Width))         


InterPaTable$Failed<-FailedTable$Freq             
InterPaTable$Prob<-InterPaTable$Freq/(InterPaTable$Failed+InterPaTable$Freq)     

Rep3<-InterPaTable


Data<-data.frame(CE=Rep1$CE, TE=Rep1$TE, Wd=Rep1$Wd, Inter-Pa1=Rep1$Freq, 
                 Inter-Pa2=Rep2$Freq, Inter-Pa3=Rep3$Freq, Failed1=Rep1$Failed, 
                 Failed2=Rep2$Failed, Failed4=Rep3$Failed, T.Prob1=Rep1$Prob, 
                 T.Prob2=Rep2$Prob, T.Prob3=Rep3$Prob)


Data$Mean.T.Prob<-(Data$T.Prob1+Data$T.Prob2+Data$T.Prob3)/3
Data$SD<-apply(Data[1:36,10:12], 1, FUN=sd)
Data$SE<-Data$SD/(sqrt(3))

DP10<-Data
DP10$DP<-10

ExpSet03Res<-rbind(DP5, DP7, DP10)

# In this final object can be found the results stored in the excel file
#  "ExpSet03.csv" in the "Traversal Probability Results" folder



# ================================================================

#' **PART 2: CODE FOR FIGURE 2**

# ================================================================

# Set directory to examine the collated results from the first Experimental Set

setwd("C:/ ... /TARTANGRAPH_CODE/Traversal Probability Results")


# Read results from Experimental Set one into environment
Data<-read.csv("ExpSet01.csv")


# Subset Traversal Probabilities at Directional Persistence of 7.5
Fig2<-subset(Data, DP==7.5)

# Create colours to distinguish points by connecting-edge number
cols<-c("red","orange","green","blue","purple")

# Make blank graph - Transecting-edge density by mean Traversal Probabilities
plot(Mean.T.Prob~TE, data = Fig2, xlab="Number of Transecting-edges", 
     ylab="Mean.T.Prob Traversal Probability", col="white", pch=16)

# Add dotted lines to distinguish different connecting-edge numbers
lines(Mean.T.Prob~TE, data=subset(Fig2, CE==3), lty="dotted", lwd=2, col="red")
lines(Mean.T.Prob~TE, data=subset(Fig2, CE==5), lty="dotted", lwd=2, col="orange")
lines(Mean.T.Prob~TE, data=subset(Fig2, CE==9), lty="dotted", lwd=2, col="green")
lines(Mean.T.Prob~TE, data=subset(Fig2, CE==17), lty="dotted", lwd=2, col="blue")
lines(Mean.T.Prob~TE, data=subset(Fig2, CE==33), lty="dotted", lwd=2, col="purple")

# Add arrows to indicate standard deviation between replicate simulations
arrows(x0=Fig2$TE, y0=Fig2$Mean.T.Prob-Fig2$SD, x1=Fig2$TE, y1=Fig2$Mean.T.Prob+Fig2$SD, 
       code=3, angle=90, length=0.15, lwd=2)

# Add points
points(Mean.T.Prob~TE, data = Fig2, col=cols, pch=16, cex=1.5)

# Add legend
legend(20, 0.007, legend=c("3","5","9","17","33"), cex=1,
       fill=c(cols), title = "Number of Connecting-edges")


# ================================================================

#' **PART 3: CODE FOR FIGURE 3**

# ================================================================

# Set directory to examine the collated results from the second Experimental Set

setwd("C:/ ... /TARTANGRAPH_CODE/Traversal Probability Results")

# Read results from Experimental Set one into environment
Data<-read.csv("ExpSet02.csv")


# Find the average Traversal Probabilities of TartanGraphs under each treatment 
#  of Directional Persistence and Connecting-edge width
Fig3<-aggregate(Data$Mean.T.Prob, by=list(Data$DP, Data$Width), FUN=mean)
names(Fig3)<-c("DP","Width","Mean.T.Prob")

# Find the standard deviation in Traversal Probabilities of TartanGraphs 
#  under each treatment of Directional Persistence and Connecting-edge width
sd<-aggregate(Data$Mean.T.Prob, by=list(Data$DP, Data$Width), FUN=sd)

sd$se<-sd$x/16 # Calculate standard error

Fig3$SE<-sd$se # Add standard error to data set to be used to make Figure 3


# Make empty plot - Connecting-edge width against mean Traversal Probabilities
plot(Mean.T.Prob~Width, data = Fig3, xlab="Width of Connecting-edges (no. cells)", 
     ylab="Mean Traversal Probability", col="white", ylim=c(0,0.0037),
     pch=16)

# Add lines to distinguish different Directional Persistence treatments
lines(Mean.T.Prob~Width, data=subset(Fig3, DP==5), lty="dotted", lwd=2, col="red")
lines(Mean.T.Prob~Width, data=subset(Fig3, DP==7.5), lty="dotted", lwd=2, col="orange")
lines(Mean.T.Prob~Width, data=subset(Fig3, DP==10), lty="dotted", lwd=2, col="blue")

# Add arrows to indicate standard error between TartanGraphs
arrows(x0=Fig3$Width, y0=Fig3$Mean.T.Prob-Fig3$SE, x1=Fig3$Width, 
       y1=Fig3$Mean.T.Prob+Fig3$SE, code=3, angle=90, length=0.15, lwd=2)

# Add points, differently coloured for each Directional Persistence treatment
points(Mean.T.Prob~Width, data=subset(Fig3, DP==5), pch=18, cex=1.5, col="red")
points(Mean.T.Prob~Width, data=subset(Fig3, DP==7.5), pch=18, cex=1.5, col="orange")
points(Mean.T.Prob~Width, data=subset(Fig3, DP==10), pch=18, cex=1.5, col="blue")

# Add legend
legend(2, 0.0037, legend=c("5.0","7.5","10.0"), fill=c("red","orange","blue"),
       cex = 1, title="Directional Persistence")



# ================================================================

#' **PART 4: CODE FOR FIGURE 4**

# ================================================================

# Set directory to examine the collated results from the second Experimental Set

setwd("C:/ ... /TARTANGRAPH_CODE/Traversal Probability Results")

# Read results from Experimental Set one into environment
Data<-read.csv("ExpSet02.csv")




# SECTION 1: LEFT-HAND PANEL

# Subset the twelve TartanGraphs with no transecting-edges and under 
#  Directional Persistence 7.5 treatment
TE0<-subset(Data, DP==7.5 & TE==0)

# Aggregate the mean Traversal Probabilities of TartanGraphs
Fig4L<-aggregate(TE0$Mean.T.Prob, by=list(TE0$CE, TE0$Width), FUN=mean)
names(Fig4L)<-c("CE","Width","Mean.T.Prob")

# Calculate standard deviation of Traversal Probability on each TartanGraph
#  across three replicate simulations
Fig4L.sd<-apply(TE0[1:12,11:13], 1, FUN=sd)

Fig4L$SD<-Fig4L.sd


# Jitter connecting-edge numbers to allow ploted poitns to be offset along
#  the figure's x-axis
Fig4L$CE<-jitter(Fig4L$CE, 1.2)

# Empty plot - connecting-edge number against mean Traversal Probabilities
plot(Mean.T.Prob~CE, data = Fig4L, xlab="Number of Connecting-edges", 
     ylab="Mean Traversal Probability", col="white", pch=16)

# Plot dotted lines to distinguish different connecting-edge width treatments
lines(Mean.T.Prob~CE, data=subset(Fig4L, Width==2), lty="dotted", col="red")
lines(Mean.T.Prob~CE, data=subset(Fig4L, Width==4), lty="dotted", col="orange")
lines(Mean.T.Prob~CE, data=subset(Fig4L, Width==6), lty="dotted", col="blue")

# Add error bars to indicate standard deviation
arrows(x0=Fig4L$CE, y0=Fig4L$Mean.T.Prob-Fig4L$SD, x1=Fig4L$CE, 
       y1=Fig4L$Mean.T.Prob+Fig4L$SD, code=3, angle=90, length=0.075)

# Plot points of different colours to distinguish different connecting-edge 
#  width treatments
points(Mean.T.Prob~CE, data=subset(Fig4L, Width==2), pch=16, cex=1.3, col="red")
points(Mean.T.Prob~CE, data=subset(Fig4L, Width==4), pch=16, cex=1.3, col="orange")
points(Mean.T.Prob~CE, data=subset(Fig4L, Width==6), pch=16, cex=1.3, col="blue")

# Add legend
legend(4, 0.005, legend=c("2 cells","4 cells","6 cells"), cex=1,
       fill=c("red","orange","blue"), title = "Connecting-edge Width")

# Below the same operations are carried out to create the right-hand panel of 
#  Figure 4. 



# SECTION TWO: RIGHT-HAND PANEL

# Subset the twelve TartanGraphs with 15 transecting-edges and under 
#  Directional Persistence 7.5 treatment 
TE15<-subset(Data, DP==7.5 & TE==15)

Fig4R<-aggregate(TE15$Mean, by=list(TE15$CE, TE15$Width), FUN=mean)
names(Fig4R)<-c("CE","Width","Mean.T.Prob")

Fig4R.sd<-apply(TE15[1:12,11:13], 1, FUN=sd)

Fig4R$SD<-Fig4R.sd

Fig4R$CE<-jitter(Fig4R$CE, 1.2)

plot(Mean.T.Prob~CE, data = Fig4R, xlab="Number of Connecting-edges", 
     ylab=" ", col="white", pch=16)

axis(4) # Add y-axis scale to the right side of the empty plot

lines(Mean.T.Prob~CE, data=subset(Fig4R, Width==2), lty="dotted", col="red")
lines(Mean.T.Prob~CE, data=subset(Fig4R, Width==4), lty="dotted", col="orange")
lines(Mean.T.Prob~CE, data=subset(Fig4R, Width==6), lty="dotted", col="blue")

arrows(x0=Fig4R$CE, y0=Fig4R$Mean.T.Prob-Fig4R$SD, x1=Fig4R$CE, 
       y1=Fig4R$Mean.T.Prob+Fig4R$SD, code=3, angle=90, length=0.075)

points(Mean.T.Prob~CE, data=subset(Fig4R, Width==2), pch=16, cex=1.3, col="red")
points(Mean.T.Prob~CE, data=subset(Fig4R, Width==4), pch=16, cex=1.3, col="orange")
points(Mean.T.Prob~CE, data=subset(Fig4R, Width==6), pch=16, cex=1.3, col="blue")



# ================================================================

#' **PART 5: CODE FOR FIGURE 5**

# ================================================================

# Set directory to examine the collated results from the third Experimental Set

setwd("C:/ ... /TARTANGRAPH_CODE/Traversal Probability Results")



# Read results from Experimental Set three into environment
Data<-read.csv("ExpSet03.csv")

# Subset data under Directional Persistence 7.5 treatment
DP75<-subset(Data, DP==7.5)

# Calculate the mean Traversal Probabilities of TartanGraphs sharing the
#  same number of transecting-edge numbers and widths
Fig5<-aggregate(DP75$Mean, by=list(DP75$TE, DP75$Width), FUN=mean)
names(Fig5)<-c("TE","Width","Mean.T.Prob")

# Find the standard deviation in Traversal Probabilities of TartanGraphs 
#  under each treatment of transecting-edge number and width
ExS3.sd<-aggregate(DP75$Mean,by=list(DP75$TE, DP75$Width), FUN=sd)

Fig5$SD<-ExS3.sd$x # Add standard deviation values

Fig5$SE<-Fig5$SD/4 # Calculate standard error for each group



# Empty plot - transecting-edge number against mean Traversal Probabilities
plot(Mean.T.Prob~TE, data = Fig5, xlab="Number of Transecting-edges", 
     ylab="Mean Traversal Probability", col="black", pch=16, ylim=c(0,0.0017))

# Add lines, differently coloured to distinguish transecting-edge treatments
lines(Mean.T.Prob~TE, data=subset(Fig5, Width==2), lty="dotted", lwd=2, col="red")
lines(Mean.T.Prob~TE, data=subset(Fig5, Width==4), lty="dotted", lwd=2, col="orange")
lines(Mean.T.Prob~TE, data=subset(Fig5, Width==6), lty="dotted", lwd=2, col="blue")

# Add error bars to indicate standard error
arrows(x0=Fig5$TE, y0=Fig5$Mean.T.Prob-Fig5$SE, x1=Fig5$TE, 
       y1=Fig5$Mean.T.Prob+Fig5$SE, 
       code=3, angle=90, length=0.15, lwd=2)

# Add points, differently coloured to distinguish transecting-edge treatments
points(Mean.T.Prob~TE, data=subset(Fig5, Width==2), pch=16, cex=1.5, col="red")
points(Mean.T.Prob~TE, data=subset(Fig5, Width==4), pch=16, cex=1.5, col="orange")
points(Mean.T.Prob~TE, data=subset(Fig5, Width==6), pch=16, cex=1.5, col="blue")


# Add legend
legend(12, 0.0016, legend=c("2 cells","4 cells","6 cells"), cex=1,
       fill=c("red","orange","blue"), title = "Transecting-edge Width")

# ================================================================
