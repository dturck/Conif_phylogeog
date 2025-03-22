setwd("/Users/danturck/Documents/Systematics Conifer/Working")

require(phytools)
require(ape)
require(TreeSim)
require(geiger)
require(picante)

#Read in tree

conif.tree<-read.tree("2018_North_America.tre")
conif.tree

plot(conif.tree)

#Read in data

bigcsv<-read.csv("Absolute_true_master_csv_For_supplement.csv",header=TRUE,row.names=1)

#Prepare Matrix 

All.matrix <- matrix(0, 32, nrow(bigcsv))
colnames(All.matrix) <- rownames(bigcsv)

#put column names into matrix

All.matrix[1,] <- bigcsv$NW_Coast
All.matrix[2,] <- bigcsv$Blues
All.matrix[3,] <- bigcsv$NW_Inland
All.matrix[4,] <- bigcsv$NW_general
All.matrix[5,] <- bigcsv$Klamath
All.matrix[6,] <- bigcsv$NW_Klamath
All.matrix[7,] <- bigcsv$CA_Klam
All.matrix[8,] <- bigcsv$CA_no_Klam
All.matrix[9,] <- bigcsv$Sierra
All.matrix[10,] <- bigcsv$Coastal_CA
All.matrix[11,] <- bigcsv$AltoBaja
All.matrix[12,] <- bigcsv$Basin
All.matrix[13,] <- bigcsv$Mogollon
All.matrix[14,] <- bigcsv$Madre_West
All.matrix[15,] <- bigcsv$Madre_East
All.matrix[16,] <- bigcsv$Trans_vol
All.matrix[17,] <- bigcsv$Madre_South_new
All.matrix[18,] <- bigcsv$SW_Jalisco
All.matrix[19,] <- bigcsv$Chiapas
All.matrix[20,] <- bigcsv$Mexico_only
All.matrix[21,] <- bigcsv$Mexico_with_Mog
All.matrix[22,] <- bigcsv$Gulf
All.matrix[23,] <- bigcsv$Appalachia
All.matrix[24,] <- bigcsv$Laurentian
All.matrix[25,] <- bigcsv$Boreal_W
All.matrix[26,] <- bigcsv$Boreal_E
All.matrix[27,] <- bigcsv$Total_Eastern
All.matrix[28,] <- bigcsv$Laur_E_W
All.matrix[29,] <- bigcsv$Laur_and_Boreal_E
All.matrix[30,] <- bigcsv$Carribean
All.matrix[31,] <- bigcsv$Laur_App
All.matrix[32,] <- bigcsv$Boreal_EW

#Calculate PD and Species Richness (SR)!

All.pd<-pd(All.matrix, conif.tree)

#get Results!

All.pd

#correlation test between PD and SR

cor.test(All.pd$PD, All.pd$SR)

plot(All.pd$PD, 
     All.pd$SR, 
     xlab = "Phylogenetic Diversity", ylab = "Species Richness", 
     pch = 16)
