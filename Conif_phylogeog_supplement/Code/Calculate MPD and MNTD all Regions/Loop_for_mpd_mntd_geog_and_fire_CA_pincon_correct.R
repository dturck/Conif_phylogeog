require(phytools)
require(ape)
require(TreeSim)
require(geiger)
require(picante)
library(phytools)

setwd("/Users/danturck/Documents/Systematics Conifer/Working")

conif.tree<-read.tree("2018_North_America.tre")
conif.tree

plotTree(conif.tree, fsize=0.3)
?plotTree

#Further down, this csv will change for Californian Regions and then this file will be reloaded after

bigcsv<-read.csv("Absolute_true_master_csv_For_supplement.csv",header=TRUE,row.names=1)


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)




NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast
NW_Coast.matrix[2,] <- bigcsv$NW_Coast

NW_Coast.matrix

length(rownames(bigcsv))
NW_Coast <- rownames(bigcsv)[bigcsv$NW_Coast==1]

NW_Coast

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

NW_Coast.tre <- keep.tip(conif.tree, NW_Coast)
plotTree(NW_Coast.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = NW_Coast.matrix, dis = cophenetic(NW_Coast.tre))
NW_Coast_geog_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(conif.tree))
NW_Coast_geog_mpd<-NW_Coast_geog_mpd[-1,]
Test_name<-("NW_Coast_geog_mpd")
NW_Coast_final_mpd_geog<-cbind(Test_name,NW_Coast_geog_mpd)


mntd(samp = NW_Coast.matrix, dis = cophenetic(NW_Coast.tre))
NW_Coast_geog_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(conif.tree))
NW_Coast_geog_mntd<-NW_Coast_geog_mntd[-1,]
Test_name<-("NW_Coast_geog_mntd")
NW_Coast_final_mntd_geog<-cbind(Test_name,NW_Coast_geog_mntd)



#for traits

#Bark

#Set up the new matrix

NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$Bark
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$Bark

NW_Coast_Bark_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Bark_mpd<-NW_Coast_Bark_mpd[-1,]
Test_name<-("NW_Coast_Bark_mpd")
NW_Coast_final_Bark_mpd<-cbind(Test_name,NW_Coast_Bark_mpd)


NW_Coast_Bark_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Bark_mntd<-NW_Coast_Bark_mntd[-1,]
Test_name<-("NW_Coast_Bark_mntd")
NW_Coast_final_Bark_mntd<-cbind(Test_name,NW_Coast_Bark_mntd)

#Serotiny


NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$Serotiny
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$Serotiny

NW_Coast_Serotiny_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Serotiny_mpd<-NW_Coast_Serotiny_mpd[-1,]
Test_name<-("NW_Coast_Serotiny_mpd")
NW_Coast_final_Serotiny_mpd<-cbind(Test_name,NW_Coast_Serotiny_mpd)


NW_Coast_Serotiny_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Serotiny_mntd<-NW_Coast_Serotiny_mntd[-1,]
Test_name<-("NW_Coast_Serotiny_mntd")
NW_Coast_final_Serotiny_mntd<-cbind(Test_name,NW_Coast_Serotiny_mntd)

#Grass

NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$Grass
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$Grass

NW_Coast_Grass_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Grass_mpd<-NW_Coast_Grass_mpd[-1,]
Test_name<-("NW_Coast_Grass_mpd")
NW_Coast_final_Grass_mpd<-cbind(Test_name,NW_Coast_Grass_mpd)

NW_Coast_Grass_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Grass_mntd<-NW_Coast_Grass_mntd[-1,]
Test_name<-("NW_Coast_Grass_mntd")
NW_Coast_final_Grass_mntd<-cbind(Test_name,NW_Coast_Grass_mntd)


#Resprout

NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$Resprout
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$Resprout

NW_Coast_Resprout_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Resprout_mpd<-NW_Coast_Resprout_mpd[-1,]
Test_name<-("NW_Coast_Resprout_mpd")
NW_Coast_final_Resprout_mpd<-cbind(Test_name,NW_Coast_Resprout_mpd)


NW_Coast_Resprout_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Resprout_mntd<-NW_Coast_Resprout_mntd[-1,]
Test_name<-("NW_Coast_Resprout_mntd")
NW_Coast_final_Resprout_mntd<-cbind(Test_name,NW_Coast_Resprout_mntd)

#More_than_one_adaptation

NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$More_than_one_adaptation
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$More_than_one_adaptation

NW_Coast_More_than_one_adaptation_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_More_than_one_adaptation_mpd<-NW_Coast_More_than_one_adaptation_mpd[-1,]
Test_name<-("NW_Coast_More_than_one_adaptation_mpd")
NW_Coast_final_More_than_one_adaptation_mpd<-cbind(Test_name,NW_Coast_More_than_one_adaptation_mpd)


NW_Coast_More_than_one_adaptation_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_More_than_one_adaptation_mntd<-NW_Coast_More_than_one_adaptation_mntd[-1,]
Test_name<-("NW_Coast_More_than_one_adaptation_mntd")
NW_Coast_final_More_than_one_adaptation_mntd<-cbind(Test_name,NW_Coast_More_than_one_adaptation_mntd)

#Pyrophillic

NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$Pyrophillic
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$Pyrophillic

NW_Coast_Pyrophillic_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Pyrophillic_mpd<-NW_Coast_Pyrophillic_mpd[-1,]
Test_name<-("NW_Coast_Pyrophillic_mpd")
NW_Coast_final_Pyrophillic_mpd<-cbind(Test_name,NW_Coast_Pyrophillic_mpd)


NW_Coast_Pyrophillic_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Pyrophillic_mntd<-NW_Coast_Pyrophillic_mntd[-1,]
Test_name<-("NW_Coast_Pyrophillic_mntd")
NW_Coast_final_Pyrophillic_mntd<-cbind(Test_name,NW_Coast_Pyrophillic_mntd)

#Pyrophobic

NW_Coast.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Coast.matrix) <- rownames(bigcsv)
NW_Coast.matrix[1,] <- bigcsv$NW_Coast & bigcsv$Pyrophobic
NW_Coast.matrix[2,] <- bigcsv$NW_Coast & bigcsv$Pyrophobic

NW_Coast_Pyrophobic_mpd<-ses.mpd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Pyrophobic_mpd<-NW_Coast_Pyrophobic_mpd[-1,]
Test_name<-("NW_Coast_Pyrophobic_mpd")
NW_Coast_final_Pyrophobic_mpd<-cbind(Test_name,NW_Coast_Pyrophobic_mpd)


NW_Coast_Pyrophobic_mntd<-ses.mntd(samp = NW_Coast.matrix, dis =  cophenetic(NW_Coast.tre))
NW_Coast_Pyrophobic_mntd<-NW_Coast_Pyrophobic_mntd[-1,]
Test_name<-("NW_Coast_Pyrophobic_mntd")
NW_Coast_final_Pyrophobic_mntd<-cbind(Test_name,NW_Coast_Pyrophobic_mntd)




#get final csv for NW_Coast mpd
NW_Coast_final_mpd_combined<-rbind(NW_Coast_final_mpd_geog,NW_Coast_final_Bark_mpd,NW_Coast_final_Serotiny_mpd,NW_Coast_final_Grass_mpd,NW_Coast_final_Resprout_mpd,NW_Coast_final_Pyrophillic_mpd,NW_Coast_final_More_than_one_adaptation_mpd,NW_Coast_final_Pyrophobic_mpd)

#write to csv

write.csv(NW_Coast_final_mpd_combined, "Outfile_NW_Coast_final_mpd_combined.csv")

#get final csv for NW_Coast mntd
NW_Coast_final_mntd_combined<-rbind(NW_Coast_final_mntd_geog,NW_Coast_final_Bark_mntd,NW_Coast_final_Serotiny_mntd,NW_Coast_final_Grass_mntd,NW_Coast_final_Resprout_mntd,NW_Coast_final_Pyrophillic_mntd,NW_Coast_final_More_than_one_adaptation_mntd,NW_Coast_final_Pyrophobic_mntd)

#write to csv

write.csv(NW_Coast_final_mntd_combined, "Outfile_NW_Coast_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues
Blues.matrix[2,] <- bigcsv$Blues

Blues.matrix

length(rownames(bigcsv))
Blues <- rownames(bigcsv)[bigcsv$Blues==1]

Blues

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Blues.tre <- keep.tip(conif.tree, Blues)
plotTree(Blues.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Blues.matrix, dis = cophenetic(Blues.tre))
Blues_geog_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(conif.tree))
Blues_geog_mpd<-Blues_geog_mpd[-1,]
Test_name<-("Blues_geog_mpd")
Blues_final_mpd_geog<-cbind(Test_name,Blues_geog_mpd)


mntd(samp = Blues.matrix, dis = cophenetic(Blues.tre))
Blues_geog_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(conif.tree))
Blues_geog_mntd<-Blues_geog_mntd[-1,]
Test_name<-("Blues_geog_mntd")
Blues_final_mntd_geog<-cbind(Test_name,Blues_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$Bark
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$Bark

Blues_Bark_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Bark_mpd<-Blues_Bark_mpd[-1,]
Test_name<-("Blues_Bark_mpd")
Blues_final_Bark_mpd<-cbind(Test_name,Blues_Bark_mpd)


Blues_Bark_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Bark_mntd<-Blues_Bark_mntd[-1,]
Test_name<-("Blues_Bark_mntd")
Blues_final_Bark_mntd<-cbind(Test_name,Blues_Bark_mntd)

#Serotiny


Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$Serotiny
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$Serotiny

Blues_Serotiny_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Serotiny_mpd<-Blues_Serotiny_mpd[-1,]
Test_name<-("Blues_Serotiny_mpd")
Blues_final_Serotiny_mpd<-cbind(Test_name,Blues_Serotiny_mpd)


Blues_Serotiny_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Serotiny_mntd<-Blues_Serotiny_mntd[-1,]
Test_name<-("Blues_Serotiny_mntd")
Blues_final_Serotiny_mntd<-cbind(Test_name,Blues_Serotiny_mntd)

#Grass

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$Grass
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$Grass

Blues_Grass_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Grass_mpd<-Blues_Grass_mpd[-1,]
Test_name<-("Blues_Grass_mpd")
Blues_final_Grass_mpd<-cbind(Test_name,Blues_Grass_mpd)

Blues_Grass_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Grass_mntd<-Blues_Grass_mntd[-1,]
Test_name<-("Blues_Grass_mntd")
Blues_final_Grass_mntd<-cbind(Test_name,Blues_Grass_mntd)


#Resprout

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$Resprout
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$Resprout

Blues_Resprout_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Resprout_mpd<-Blues_Resprout_mpd[-1,]
Test_name<-("Blues_Resprout_mpd")
Blues_final_Resprout_mpd<-cbind(Test_name,Blues_Resprout_mpd)


Blues_Resprout_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Resprout_mntd<-Blues_Resprout_mntd[-1,]
Test_name<-("Blues_Resprout_mntd")
Blues_final_Resprout_mntd<-cbind(Test_name,Blues_Resprout_mntd)

#More_than_one_adaptation

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$More_than_one_adaptation
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$More_than_one_adaptation

Blues_More_than_one_adaptation_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_More_than_one_adaptation_mpd<-Blues_More_than_one_adaptation_mpd[-1,]
Test_name<-("Blues_More_than_one_adaptation_mpd")
Blues_final_More_than_one_adaptation_mpd<-cbind(Test_name,Blues_More_than_one_adaptation_mpd)


Blues_More_than_one_adaptation_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_More_than_one_adaptation_mntd<-Blues_More_than_one_adaptation_mntd[-1,]
Test_name<-("Blues_More_than_one_adaptation_mntd")
Blues_final_More_than_one_adaptation_mntd<-cbind(Test_name,Blues_More_than_one_adaptation_mntd)

#Pyrophillic

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$Pyrophillic
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$Pyrophillic

Blues_Pyrophillic_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Pyrophillic_mpd<-Blues_Pyrophillic_mpd[-1,]
Test_name<-("Blues_Pyrophillic_mpd")
Blues_final_Pyrophillic_mpd<-cbind(Test_name,Blues_Pyrophillic_mpd)


Blues_Pyrophillic_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Pyrophillic_mntd<-Blues_Pyrophillic_mntd[-1,]
Test_name<-("Blues_Pyrophillic_mntd")
Blues_final_Pyrophillic_mntd<-cbind(Test_name,Blues_Pyrophillic_mntd)

#Pyrophobic

Blues.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Blues.matrix) <- rownames(bigcsv)
Blues.matrix[1,] <- bigcsv$Blues & bigcsv$Pyrophobic
Blues.matrix[2,] <- bigcsv$Blues & bigcsv$Pyrophobic

Blues_Pyrophobic_mpd<-ses.mpd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Pyrophobic_mpd<-Blues_Pyrophobic_mpd[-1,]
Test_name<-("Blues_Pyrophobic_mpd")
Blues_final_Pyrophobic_mpd<-cbind(Test_name,Blues_Pyrophobic_mpd)


Blues_Pyrophobic_mntd<-ses.mntd(samp = Blues.matrix, dis =  cophenetic(Blues.tre))
Blues_Pyrophobic_mntd<-Blues_Pyrophobic_mntd[-1,]
Test_name<-("Blues_Pyrophobic_mntd")
Blues_final_Pyrophobic_mntd<-cbind(Test_name,Blues_Pyrophobic_mntd)




#get final csv for Blues mpd
Blues_final_mpd_combined<-rbind(Blues_final_mpd_geog,Blues_final_Bark_mpd,Blues_final_Serotiny_mpd,Blues_final_Grass_mpd,Blues_final_Resprout_mpd,Blues_final_Pyrophillic_mpd,Blues_final_More_than_one_adaptation_mpd,Blues_final_Pyrophobic_mpd)

#write to csv

write.csv(Blues_final_mpd_combined, "Outfile_Blues_final_mpd_combined.csv")

#get final csv for Blues mntd
Blues_final_mntd_combined<-rbind(Blues_final_mntd_geog,Blues_final_Bark_mntd,Blues_final_Serotiny_mntd,Blues_final_Grass_mntd,Blues_final_Resprout_mntd,Blues_final_Pyrophillic_mntd,Blues_final_More_than_one_adaptation_mntd,Blues_final_Pyrophobic_mntd)

#write to csv

write.csv(Blues_final_mntd_combined, "Outfile_Blues_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland
NW_Inland.matrix[2,] <- bigcsv$NW_Inland

NW_Inland.matrix

length(rownames(bigcsv))
NW_Inland <- rownames(bigcsv)[bigcsv$NW_Inland==1]

NW_Inland

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

NW_Inland.tre <- keep.tip(conif.tree, NW_Inland)
plotTree(NW_Inland.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = NW_Inland.matrix, dis = cophenetic(NW_Inland.tre))
NW_Inland_geog_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(conif.tree))
NW_Inland_geog_mpd<-NW_Inland_geog_mpd[-1,]
Test_name<-("NW_Inland_geog_mpd")
NW_Inland_final_mpd_geog<-cbind(Test_name,NW_Inland_geog_mpd)


mntd(samp = NW_Inland.matrix, dis = cophenetic(NW_Inland.tre))
NW_Inland_geog_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(conif.tree))
NW_Inland_geog_mntd<-NW_Inland_geog_mntd[-1,]
Test_name<-("NW_Inland_geog_mntd")
NW_Inland_final_mntd_geog<-cbind(Test_name,NW_Inland_geog_mntd)



#for traits

#Bark

#Set up the new matrix

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$Bark
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$Bark

NW_Inland_Bark_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Bark_mpd<-NW_Inland_Bark_mpd[-1,]
Test_name<-("NW_Inland_Bark_mpd")
NW_Inland_final_Bark_mpd<-cbind(Test_name,NW_Inland_Bark_mpd)


NW_Inland_Bark_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Bark_mntd<-NW_Inland_Bark_mntd[-1,]
Test_name<-("NW_Inland_Bark_mntd")
NW_Inland_final_Bark_mntd<-cbind(Test_name,NW_Inland_Bark_mntd)

#Serotiny


NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$Serotiny
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$Serotiny

NW_Inland_Serotiny_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Serotiny_mpd<-NW_Inland_Serotiny_mpd[-1,]
Test_name<-("NW_Inland_Serotiny_mpd")
NW_Inland_final_Serotiny_mpd<-cbind(Test_name,NW_Inland_Serotiny_mpd)


NW_Inland_Serotiny_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Serotiny_mntd<-NW_Inland_Serotiny_mntd[-1,]
Test_name<-("NW_Inland_Serotiny_mntd")
NW_Inland_final_Serotiny_mntd<-cbind(Test_name,NW_Inland_Serotiny_mntd)

#Grass

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$Grass
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$Grass

NW_Inland_Grass_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Grass_mpd<-NW_Inland_Grass_mpd[-1,]
Test_name<-("NW_Inland_Grass_mpd")
NW_Inland_final_Grass_mpd<-cbind(Test_name,NW_Inland_Grass_mpd)

NW_Inland_Grass_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Grass_mntd<-NW_Inland_Grass_mntd[-1,]
Test_name<-("NW_Inland_Grass_mntd")
NW_Inland_final_Grass_mntd<-cbind(Test_name,NW_Inland_Grass_mntd)


#Resprout

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$Resprout
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$Resprout

NW_Inland_Resprout_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Resprout_mpd<-NW_Inland_Resprout_mpd[-1,]
Test_name<-("NW_Inland_Resprout_mpd")
NW_Inland_final_Resprout_mpd<-cbind(Test_name,NW_Inland_Resprout_mpd)


NW_Inland_Resprout_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Resprout_mntd<-NW_Inland_Resprout_mntd[-1,]
Test_name<-("NW_Inland_Resprout_mntd")
NW_Inland_final_Resprout_mntd<-cbind(Test_name,NW_Inland_Resprout_mntd)

#More_than_one_adaptation

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$More_than_one_adaptation
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$More_than_one_adaptation

NW_Inland_More_than_one_adaptation_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_More_than_one_adaptation_mpd<-NW_Inland_More_than_one_adaptation_mpd[-1,]
Test_name<-("NW_Inland_More_than_one_adaptation_mpd")
NW_Inland_final_More_than_one_adaptation_mpd<-cbind(Test_name,NW_Inland_More_than_one_adaptation_mpd)


NW_Inland_More_than_one_adaptation_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_More_than_one_adaptation_mntd<-NW_Inland_More_than_one_adaptation_mntd[-1,]
Test_name<-("NW_Inland_More_than_one_adaptation_mntd")
NW_Inland_final_More_than_one_adaptation_mntd<-cbind(Test_name,NW_Inland_More_than_one_adaptation_mntd)

#Pyrophillic

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$Pyrophillic
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$Pyrophillic

NW_Inland_Pyrophillic_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Pyrophillic_mpd<-NW_Inland_Pyrophillic_mpd[-1,]
Test_name<-("NW_Inland_Pyrophillic_mpd")
NW_Inland_final_Pyrophillic_mpd<-cbind(Test_name,NW_Inland_Pyrophillic_mpd)


NW_Inland_Pyrophillic_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Pyrophillic_mntd<-NW_Inland_Pyrophillic_mntd[-1,]
Test_name<-("NW_Inland_Pyrophillic_mntd")
NW_Inland_final_Pyrophillic_mntd<-cbind(Test_name,NW_Inland_Pyrophillic_mntd)

#Pyrophobic

NW_Inland.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Inland.matrix) <- rownames(bigcsv)
NW_Inland.matrix[1,] <- bigcsv$NW_Inland & bigcsv$Pyrophobic
NW_Inland.matrix[2,] <- bigcsv$NW_Inland & bigcsv$Pyrophobic

NW_Inland_Pyrophobic_mpd<-ses.mpd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Pyrophobic_mpd<-NW_Inland_Pyrophobic_mpd[-1,]
Test_name<-("NW_Inland_Pyrophobic_mpd")
NW_Inland_final_Pyrophobic_mpd<-cbind(Test_name,NW_Inland_Pyrophobic_mpd)


NW_Inland_Pyrophobic_mntd<-ses.mntd(samp = NW_Inland.matrix, dis =  cophenetic(NW_Inland.tre))
NW_Inland_Pyrophobic_mntd<-NW_Inland_Pyrophobic_mntd[-1,]
Test_name<-("NW_Inland_Pyrophobic_mntd")
NW_Inland_final_Pyrophobic_mntd<-cbind(Test_name,NW_Inland_Pyrophobic_mntd)




#get final csv for NW_Inland mpd
NW_Inland_final_mpd_combined<-rbind(NW_Inland_final_mpd_geog,NW_Inland_final_Bark_mpd,NW_Inland_final_Serotiny_mpd,NW_Inland_final_Grass_mpd,NW_Inland_final_Resprout_mpd,NW_Inland_final_Pyrophillic_mpd,NW_Inland_final_More_than_one_adaptation_mpd,NW_Inland_final_Pyrophobic_mpd)

#write to csv

write.csv(NW_Inland_final_mpd_combined, "Outfile_NW_Inland_final_mpd_combined.csv")

#get final csv for NW_Inland mntd
NW_Inland_final_mntd_combined<-rbind(NW_Inland_final_mntd_geog,NW_Inland_final_Bark_mntd,NW_Inland_final_Serotiny_mntd,NW_Inland_final_Grass_mntd,NW_Inland_final_Resprout_mntd,NW_Inland_final_Pyrophillic_mntd,NW_Inland_final_More_than_one_adaptation_mntd,NW_Inland_final_Pyrophobic_mntd)

#write to csv

write.csv(NW_Inland_final_mntd_combined, "Outfile_NW_Inland_final_mntd_combined.csv")




#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath
Klamath.matrix[2,] <- bigcsv$Klamath

Klamath.matrix

length(rownames(bigcsv))
Klamath <- rownames(bigcsv)[bigcsv$Klamath==1]

Klamath

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Klamath.tre <- keep.tip(conif.tree, Klamath)
plotTree(Klamath.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Klamath.matrix, dis = cophenetic(Klamath.tre))
Klamath_geog_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(conif.tree))
Klamath_geog_mpd<-Klamath_geog_mpd[-1,]
Test_name<-("Klamath_geog_mpd")
Klamath_final_mpd_geog<-cbind(Test_name,Klamath_geog_mpd)


mntd(samp = Klamath.matrix, dis = cophenetic(Klamath.tre))
Klamath_geog_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(conif.tree))
Klamath_geog_mntd<-Klamath_geog_mntd[-1,]
Test_name<-("Klamath_geog_mntd")
Klamath_final_mntd_geog<-cbind(Test_name,Klamath_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$Bark
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$Bark

Klamath_Bark_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Bark_mpd<-Klamath_Bark_mpd[-1,]
Test_name<-("Klamath_Bark_mpd")
Klamath_final_Bark_mpd<-cbind(Test_name,Klamath_Bark_mpd)


Klamath_Bark_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Bark_mntd<-Klamath_Bark_mntd[-1,]
Test_name<-("Klamath_Bark_mntd")
Klamath_final_Bark_mntd<-cbind(Test_name,Klamath_Bark_mntd)

#Serotiny


Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$Serotiny
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$Serotiny

Klamath_Serotiny_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Serotiny_mpd<-Klamath_Serotiny_mpd[-1,]
Test_name<-("Klamath_Serotiny_mpd")
Klamath_final_Serotiny_mpd<-cbind(Test_name,Klamath_Serotiny_mpd)


Klamath_Serotiny_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Serotiny_mntd<-Klamath_Serotiny_mntd[-1,]
Test_name<-("Klamath_Serotiny_mntd")
Klamath_final_Serotiny_mntd<-cbind(Test_name,Klamath_Serotiny_mntd)

#Grass

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$Grass
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$Grass

Klamath_Grass_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Grass_mpd<-Klamath_Grass_mpd[-1,]
Test_name<-("Klamath_Grass_mpd")
Klamath_final_Grass_mpd<-cbind(Test_name,Klamath_Grass_mpd)

Klamath_Grass_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Grass_mntd<-Klamath_Grass_mntd[-1,]
Test_name<-("Klamath_Grass_mntd")
Klamath_final_Grass_mntd<-cbind(Test_name,Klamath_Grass_mntd)


#Resprout

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$Resprout
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$Resprout

Klamath_Resprout_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Resprout_mpd<-Klamath_Resprout_mpd[-1,]
Test_name<-("Klamath_Resprout_mpd")
Klamath_final_Resprout_mpd<-cbind(Test_name,Klamath_Resprout_mpd)


Klamath_Resprout_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Resprout_mntd<-Klamath_Resprout_mntd[-1,]
Test_name<-("Klamath_Resprout_mntd")
Klamath_final_Resprout_mntd<-cbind(Test_name,Klamath_Resprout_mntd)

#More_than_one_adaptation

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$More_than_one_adaptation
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$More_than_one_adaptation

Klamath_More_than_one_adaptation_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_More_than_one_adaptation_mpd<-Klamath_More_than_one_adaptation_mpd[-1,]
Test_name<-("Klamath_More_than_one_adaptation_mpd")
Klamath_final_More_than_one_adaptation_mpd<-cbind(Test_name,Klamath_More_than_one_adaptation_mpd)


Klamath_More_than_one_adaptation_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_More_than_one_adaptation_mntd<-Klamath_More_than_one_adaptation_mntd[-1,]
Test_name<-("Klamath_More_than_one_adaptation_mntd")
Klamath_final_More_than_one_adaptation_mntd<-cbind(Test_name,Klamath_More_than_one_adaptation_mntd)

#Pyrophillic

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$Pyrophillic
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$Pyrophillic

Klamath_Pyrophillic_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Pyrophillic_mpd<-Klamath_Pyrophillic_mpd[-1,]
Test_name<-("Klamath_Pyrophillic_mpd")
Klamath_final_Pyrophillic_mpd<-cbind(Test_name,Klamath_Pyrophillic_mpd)


Klamath_Pyrophillic_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Pyrophillic_mntd<-Klamath_Pyrophillic_mntd[-1,]
Test_name<-("Klamath_Pyrophillic_mntd")
Klamath_final_Pyrophillic_mntd<-cbind(Test_name,Klamath_Pyrophillic_mntd)

#Pyrophobic

Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Klamath.matrix) <- rownames(bigcsv)
Klamath.matrix[1,] <- bigcsv$Klamath & bigcsv$Pyrophobic
Klamath.matrix[2,] <- bigcsv$Klamath & bigcsv$Pyrophobic

Klamath_Pyrophobic_mpd<-ses.mpd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Pyrophobic_mpd<-Klamath_Pyrophobic_mpd[-1,]
Test_name<-("Klamath_Pyrophobic_mpd")
Klamath_final_Pyrophobic_mpd<-cbind(Test_name,Klamath_Pyrophobic_mpd)


Klamath_Pyrophobic_mntd<-ses.mntd(samp = Klamath.matrix, dis =  cophenetic(Klamath.tre))
Klamath_Pyrophobic_mntd<-Klamath_Pyrophobic_mntd[-1,]
Test_name<-("Klamath_Pyrophobic_mntd")
Klamath_final_Pyrophobic_mntd<-cbind(Test_name,Klamath_Pyrophobic_mntd)




#get final csv for Klamath mpd
Klamath_final_mpd_combined<-rbind(Klamath_final_mpd_geog,Klamath_final_Bark_mpd,Klamath_final_Serotiny_mpd,Klamath_final_Grass_mpd,Klamath_final_Resprout_mpd,Klamath_final_Pyrophillic_mpd,Klamath_final_More_than_one_adaptation_mpd,Klamath_final_Pyrophobic_mpd)

#write to csv

write.csv(Klamath_final_mpd_combined, "Outfile_Klamath_final_mpd_combined.csv")

#get final csv for Klamath mntd
Klamath_final_mntd_combined<-rbind(Klamath_final_mntd_geog,Klamath_final_Bark_mntd,Klamath_final_Serotiny_mntd,Klamath_final_Grass_mntd,Klamath_final_Resprout_mntd,Klamath_final_Pyrophillic_mntd,Klamath_final_More_than_one_adaptation_mntd,Klamath_final_Pyrophobic_mntd)

#write to csv

write.csv(Klamath_final_mntd_combined, "Outfile_Klamath_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general
NW_general.matrix[2,] <- bigcsv$NW_general

NW_general.matrix

length(rownames(bigcsv))
NW_general <- rownames(bigcsv)[bigcsv$NW_general==1]

NW_general

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

NW_general.tre <- keep.tip(conif.tree, NW_general)
plotTree(NW_general.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = NW_general.matrix, dis = cophenetic(NW_general.tre))
NW_general_geog_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(conif.tree))
NW_general_geog_mpd<-NW_general_geog_mpd[-1,]
Test_name<-("NW_general_geog_mpd")
NW_general_final_mpd_geog<-cbind(Test_name,NW_general_geog_mpd)


mntd(samp = NW_general.matrix, dis = cophenetic(NW_general.tre))
NW_general_geog_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(conif.tree))
NW_general_geog_mntd<-NW_general_geog_mntd[-1,]
Test_name<-("NW_general_geog_mntd")
NW_general_final_mntd_geog<-cbind(Test_name,NW_general_geog_mntd)



#for traits

#Bark

#Set up the new matrix

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$Bark
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$Bark

NW_general_Bark_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Bark_mpd<-NW_general_Bark_mpd[-1,]
Test_name<-("NW_general_Bark_mpd")
NW_general_final_Bark_mpd<-cbind(Test_name,NW_general_Bark_mpd)


NW_general_Bark_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Bark_mntd<-NW_general_Bark_mntd[-1,]
Test_name<-("NW_general_Bark_mntd")
NW_general_final_Bark_mntd<-cbind(Test_name,NW_general_Bark_mntd)

#Serotiny


NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$Serotiny
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$Serotiny

NW_general_Serotiny_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Serotiny_mpd<-NW_general_Serotiny_mpd[-1,]
Test_name<-("NW_general_Serotiny_mpd")
NW_general_final_Serotiny_mpd<-cbind(Test_name,NW_general_Serotiny_mpd)


NW_general_Serotiny_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Serotiny_mntd<-NW_general_Serotiny_mntd[-1,]
Test_name<-("NW_general_Serotiny_mntd")
NW_general_final_Serotiny_mntd<-cbind(Test_name,NW_general_Serotiny_mntd)

#Grass

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$Grass
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$Grass

NW_general_Grass_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Grass_mpd<-NW_general_Grass_mpd[-1,]
Test_name<-("NW_general_Grass_mpd")
NW_general_final_Grass_mpd<-cbind(Test_name,NW_general_Grass_mpd)

NW_general_Grass_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Grass_mntd<-NW_general_Grass_mntd[-1,]
Test_name<-("NW_general_Grass_mntd")
NW_general_final_Grass_mntd<-cbind(Test_name,NW_general_Grass_mntd)


#Resprout

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$Resprout
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$Resprout

NW_general_Resprout_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Resprout_mpd<-NW_general_Resprout_mpd[-1,]
Test_name<-("NW_general_Resprout_mpd")
NW_general_final_Resprout_mpd<-cbind(Test_name,NW_general_Resprout_mpd)


NW_general_Resprout_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Resprout_mntd<-NW_general_Resprout_mntd[-1,]
Test_name<-("NW_general_Resprout_mntd")
NW_general_final_Resprout_mntd<-cbind(Test_name,NW_general_Resprout_mntd)

#More_than_one_adaptation

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$More_than_one_adaptation
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$More_than_one_adaptation

NW_general_More_than_one_adaptation_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_More_than_one_adaptation_mpd<-NW_general_More_than_one_adaptation_mpd[-1,]
Test_name<-("NW_general_More_than_one_adaptation_mpd")
NW_general_final_More_than_one_adaptation_mpd<-cbind(Test_name,NW_general_More_than_one_adaptation_mpd)


NW_general_More_than_one_adaptation_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_More_than_one_adaptation_mntd<-NW_general_More_than_one_adaptation_mntd[-1,]
Test_name<-("NW_general_More_than_one_adaptation_mntd")
NW_general_final_More_than_one_adaptation_mntd<-cbind(Test_name,NW_general_More_than_one_adaptation_mntd)

#Pyrophillic

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$Pyrophillic
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$Pyrophillic

NW_general_Pyrophillic_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Pyrophillic_mpd<-NW_general_Pyrophillic_mpd[-1,]
Test_name<-("NW_general_Pyrophillic_mpd")
NW_general_final_Pyrophillic_mpd<-cbind(Test_name,NW_general_Pyrophillic_mpd)


NW_general_Pyrophillic_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Pyrophillic_mntd<-NW_general_Pyrophillic_mntd[-1,]
Test_name<-("NW_general_Pyrophillic_mntd")
NW_general_final_Pyrophillic_mntd<-cbind(Test_name,NW_general_Pyrophillic_mntd)

#Pyrophobic

NW_general.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_general.matrix) <- rownames(bigcsv)
NW_general.matrix[1,] <- bigcsv$NW_general & bigcsv$Pyrophobic
NW_general.matrix[2,] <- bigcsv$NW_general & bigcsv$Pyrophobic

NW_general_Pyrophobic_mpd<-ses.mpd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Pyrophobic_mpd<-NW_general_Pyrophobic_mpd[-1,]
Test_name<-("NW_general_Pyrophobic_mpd")
NW_general_final_Pyrophobic_mpd<-cbind(Test_name,NW_general_Pyrophobic_mpd)


NW_general_Pyrophobic_mntd<-ses.mntd(samp = NW_general.matrix, dis =  cophenetic(NW_general.tre))
NW_general_Pyrophobic_mntd<-NW_general_Pyrophobic_mntd[-1,]
Test_name<-("NW_general_Pyrophobic_mntd")
NW_general_final_Pyrophobic_mntd<-cbind(Test_name,NW_general_Pyrophobic_mntd)




#get final csv for NW_general mpd
NW_general_final_mpd_combined<-rbind(NW_general_final_mpd_geog,NW_general_final_Bark_mpd,NW_general_final_Serotiny_mpd,NW_general_final_Grass_mpd,NW_general_final_Resprout_mpd,NW_general_final_Pyrophillic_mpd,NW_general_final_More_than_one_adaptation_mpd,NW_general_final_Pyrophobic_mpd)

#write to csv

write.csv(NW_general_final_mpd_combined, "Outfile_NW_general_final_mpd_combined.csv")

#get final csv for NW_general mntd
NW_general_final_mntd_combined<-rbind(NW_general_final_mntd_geog,NW_general_final_Bark_mntd,NW_general_final_Serotiny_mntd,NW_general_final_Grass_mntd,NW_general_final_Resprout_mntd,NW_general_final_Pyrophillic_mntd,NW_general_final_More_than_one_adaptation_mntd,NW_general_final_Pyrophobic_mntd)

#write to csv

write.csv(NW_general_final_mntd_combined, "Outfile_NW_general_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath

NW_Klamath.matrix

length(rownames(bigcsv))
NW_Klamath <- rownames(bigcsv)[bigcsv$NW_Klamath==1]

NW_Klamath

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

NW_Klamath.tre <- keep.tip(conif.tree, NW_Klamath)
plotTree(NW_Klamath.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = NW_Klamath.matrix, dis = cophenetic(NW_Klamath.tre))
NW_Klamath_geog_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(conif.tree))
NW_Klamath_geog_mpd<-NW_Klamath_geog_mpd[-1,]
Test_name<-("NW_Klamath_geog_mpd")
NW_Klamath_final_mpd_geog<-cbind(Test_name,NW_Klamath_geog_mpd)


mntd(samp = NW_Klamath.matrix, dis = cophenetic(NW_Klamath.tre))
NW_Klamath_geog_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(conif.tree))
NW_Klamath_geog_mntd<-NW_Klamath_geog_mntd[-1,]
Test_name<-("NW_Klamath_geog_mntd")
NW_Klamath_final_mntd_geog<-cbind(Test_name,NW_Klamath_geog_mntd)



#for traits

#Bark

#Set up the new matrix

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$Bark
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$Bark

NW_Klamath_Bark_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Bark_mpd<-NW_Klamath_Bark_mpd[-1,]
Test_name<-("NW_Klamath_Bark_mpd")
NW_Klamath_final_Bark_mpd<-cbind(Test_name,NW_Klamath_Bark_mpd)


NW_Klamath_Bark_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Bark_mntd<-NW_Klamath_Bark_mntd[-1,]
Test_name<-("NW_Klamath_Bark_mntd")
NW_Klamath_final_Bark_mntd<-cbind(Test_name,NW_Klamath_Bark_mntd)

#Serotiny


NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$Serotiny
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$Serotiny

NW_Klamath_Serotiny_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Serotiny_mpd<-NW_Klamath_Serotiny_mpd[-1,]
Test_name<-("NW_Klamath_Serotiny_mpd")
NW_Klamath_final_Serotiny_mpd<-cbind(Test_name,NW_Klamath_Serotiny_mpd)


NW_Klamath_Serotiny_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Serotiny_mntd<-NW_Klamath_Serotiny_mntd[-1,]
Test_name<-("NW_Klamath_Serotiny_mntd")
NW_Klamath_final_Serotiny_mntd<-cbind(Test_name,NW_Klamath_Serotiny_mntd)

#Grass

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$Grass
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$Grass

NW_Klamath_Grass_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Grass_mpd<-NW_Klamath_Grass_mpd[-1,]
Test_name<-("NW_Klamath_Grass_mpd")
NW_Klamath_final_Grass_mpd<-cbind(Test_name,NW_Klamath_Grass_mpd)

NW_Klamath_Grass_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Grass_mntd<-NW_Klamath_Grass_mntd[-1,]
Test_name<-("NW_Klamath_Grass_mntd")
NW_Klamath_final_Grass_mntd<-cbind(Test_name,NW_Klamath_Grass_mntd)


#Resprout

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$Resprout
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$Resprout

NW_Klamath_Resprout_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Resprout_mpd<-NW_Klamath_Resprout_mpd[-1,]
Test_name<-("NW_Klamath_Resprout_mpd")
NW_Klamath_final_Resprout_mpd<-cbind(Test_name,NW_Klamath_Resprout_mpd)


NW_Klamath_Resprout_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Resprout_mntd<-NW_Klamath_Resprout_mntd[-1,]
Test_name<-("NW_Klamath_Resprout_mntd")
NW_Klamath_final_Resprout_mntd<-cbind(Test_name,NW_Klamath_Resprout_mntd)

#More_than_one_adaptation

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$More_than_one_adaptation
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$More_than_one_adaptation

NW_Klamath_More_than_one_adaptation_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_More_than_one_adaptation_mpd<-NW_Klamath_More_than_one_adaptation_mpd[-1,]
Test_name<-("NW_Klamath_More_than_one_adaptation_mpd")
NW_Klamath_final_More_than_one_adaptation_mpd<-cbind(Test_name,NW_Klamath_More_than_one_adaptation_mpd)


NW_Klamath_More_than_one_adaptation_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_More_than_one_adaptation_mntd<-NW_Klamath_More_than_one_adaptation_mntd[-1,]
Test_name<-("NW_Klamath_More_than_one_adaptation_mntd")
NW_Klamath_final_More_than_one_adaptation_mntd<-cbind(Test_name,NW_Klamath_More_than_one_adaptation_mntd)

#Pyrophillic

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$Pyrophillic
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$Pyrophillic

NW_Klamath_Pyrophillic_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Pyrophillic_mpd<-NW_Klamath_Pyrophillic_mpd[-1,]
Test_name<-("NW_Klamath_Pyrophillic_mpd")
NW_Klamath_final_Pyrophillic_mpd<-cbind(Test_name,NW_Klamath_Pyrophillic_mpd)


NW_Klamath_Pyrophillic_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Pyrophillic_mntd<-NW_Klamath_Pyrophillic_mntd[-1,]
Test_name<-("NW_Klamath_Pyrophillic_mntd")
NW_Klamath_final_Pyrophillic_mntd<-cbind(Test_name,NW_Klamath_Pyrophillic_mntd)

#Pyrophobic

NW_Klamath.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(NW_Klamath.matrix) <- rownames(bigcsv)
NW_Klamath.matrix[1,] <- bigcsv$NW_Klamath & bigcsv$Pyrophobic
NW_Klamath.matrix[2,] <- bigcsv$NW_Klamath & bigcsv$Pyrophobic

NW_Klamath_Pyrophobic_mpd<-ses.mpd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Pyrophobic_mpd<-NW_Klamath_Pyrophobic_mpd[-1,]
Test_name<-("NW_Klamath_Pyrophobic_mpd")
NW_Klamath_final_Pyrophobic_mpd<-cbind(Test_name,NW_Klamath_Pyrophobic_mpd)


NW_Klamath_Pyrophobic_mntd<-ses.mntd(samp = NW_Klamath.matrix, dis =  cophenetic(NW_Klamath.tre))
NW_Klamath_Pyrophobic_mntd<-NW_Klamath_Pyrophobic_mntd[-1,]
Test_name<-("NW_Klamath_Pyrophobic_mntd")
NW_Klamath_final_Pyrophobic_mntd<-cbind(Test_name,NW_Klamath_Pyrophobic_mntd)




#get final csv for NW_Klamath mpd
NW_Klamath_final_mpd_combined<-rbind(NW_Klamath_final_mpd_geog,NW_Klamath_final_Bark_mpd,NW_Klamath_final_Serotiny_mpd,NW_Klamath_final_Grass_mpd,NW_Klamath_final_Resprout_mpd,NW_Klamath_final_Pyrophillic_mpd,NW_Klamath_final_More_than_one_adaptation_mpd,NW_Klamath_final_Pyrophobic_mpd)

#write to csv

write.csv(NW_Klamath_final_mpd_combined, "Outfile_NW_Klamath_final_mpd_combined.csv")

#get final csv for NW_Klamath mntd
NW_Klamath_final_mntd_combined<-rbind(NW_Klamath_final_mntd_geog,NW_Klamath_final_Bark_mntd,NW_Klamath_final_Serotiny_mntd,NW_Klamath_final_Grass_mntd,NW_Klamath_final_Resprout_mntd,NW_Klamath_final_Pyrophillic_mntd,NW_Klamath_final_More_than_one_adaptation_mntd,NW_Klamath_final_Pyrophobic_mntd)

#write to csv

write.csv(NW_Klamath_final_mntd_combined, "Outfile_NW_Klamath_final_mntd_combined.csv")




#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam
CA_Klam.matrix[2,] <- bigcsv$CA_Klam

CA_Klam.matrix

length(rownames(bigcsv))
CA_Klam <- rownames(bigcsv)[bigcsv$CA_Klam==1]

CA_Klam

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

CA_Klam.tre <- keep.tip(conif.tree, CA_Klam)
plotTree(CA_Klam.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = CA_Klam.matrix, dis = cophenetic(CA_Klam.tre))
CA_Klam_geog_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(conif.tree))
CA_Klam_geog_mpd<-CA_Klam_geog_mpd[-1,]
Test_name<-("CA_Klam_geog_mpd")
CA_Klam_final_mpd_geog<-cbind(Test_name,CA_Klam_geog_mpd)


mntd(samp = CA_Klam.matrix, dis = cophenetic(CA_Klam.tre))
CA_Klam_geog_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(conif.tree))
CA_Klam_geog_mntd<-CA_Klam_geog_mntd[-1,]
Test_name<-("CA_Klam_geog_mntd")
CA_Klam_final_mntd_geog<-cbind(Test_name,CA_Klam_geog_mntd)



#for traits

#Bark

#Set up the new matrix

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$Bark
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$Bark

CA_Klam_Bark_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Bark_mpd<-CA_Klam_Bark_mpd[-1,]
Test_name<-("CA_Klam_Bark_mpd")
CA_Klam_final_Bark_mpd<-cbind(Test_name,CA_Klam_Bark_mpd)


CA_Klam_Bark_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Bark_mntd<-CA_Klam_Bark_mntd[-1,]
Test_name<-("CA_Klam_Bark_mntd")
CA_Klam_final_Bark_mntd<-cbind(Test_name,CA_Klam_Bark_mntd)

#Serotiny


CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$Serotiny
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$Serotiny

CA_Klam_Serotiny_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Serotiny_mpd<-CA_Klam_Serotiny_mpd[-1,]
Test_name<-("CA_Klam_Serotiny_mpd")
CA_Klam_final_Serotiny_mpd<-cbind(Test_name,CA_Klam_Serotiny_mpd)


CA_Klam_Serotiny_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Serotiny_mntd<-CA_Klam_Serotiny_mntd[-1,]
Test_name<-("CA_Klam_Serotiny_mntd")
CA_Klam_final_Serotiny_mntd<-cbind(Test_name,CA_Klam_Serotiny_mntd)

#Grass

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$Grass
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$Grass

CA_Klam_Grass_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Grass_mpd<-CA_Klam_Grass_mpd[-1,]
Test_name<-("CA_Klam_Grass_mpd")
CA_Klam_final_Grass_mpd<-cbind(Test_name,CA_Klam_Grass_mpd)

CA_Klam_Grass_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Grass_mntd<-CA_Klam_Grass_mntd[-1,]
Test_name<-("CA_Klam_Grass_mntd")
CA_Klam_final_Grass_mntd<-cbind(Test_name,CA_Klam_Grass_mntd)


#Resprout

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$Resprout
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$Resprout

CA_Klam_Resprout_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Resprout_mpd<-CA_Klam_Resprout_mpd[-1,]
Test_name<-("CA_Klam_Resprout_mpd")
CA_Klam_final_Resprout_mpd<-cbind(Test_name,CA_Klam_Resprout_mpd)


CA_Klam_Resprout_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Resprout_mntd<-CA_Klam_Resprout_mntd[-1,]
Test_name<-("CA_Klam_Resprout_mntd")
CA_Klam_final_Resprout_mntd<-cbind(Test_name,CA_Klam_Resprout_mntd)

#More_than_one_adaptation

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$More_than_one_adaptation
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$More_than_one_adaptation

CA_Klam_More_than_one_adaptation_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_More_than_one_adaptation_mpd<-CA_Klam_More_than_one_adaptation_mpd[-1,]
Test_name<-("CA_Klam_More_than_one_adaptation_mpd")
CA_Klam_final_More_than_one_adaptation_mpd<-cbind(Test_name,CA_Klam_More_than_one_adaptation_mpd)


CA_Klam_More_than_one_adaptation_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_More_than_one_adaptation_mntd<-CA_Klam_More_than_one_adaptation_mntd[-1,]
Test_name<-("CA_Klam_More_than_one_adaptation_mntd")
CA_Klam_final_More_than_one_adaptation_mntd<-cbind(Test_name,CA_Klam_More_than_one_adaptation_mntd)

#Pyrophillic

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$Pyrophillic
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$Pyrophillic

CA_Klam_Pyrophillic_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Pyrophillic_mpd<-CA_Klam_Pyrophillic_mpd[-1,]
Test_name<-("CA_Klam_Pyrophillic_mpd")
CA_Klam_final_Pyrophillic_mpd<-cbind(Test_name,CA_Klam_Pyrophillic_mpd)


CA_Klam_Pyrophillic_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Pyrophillic_mntd<-CA_Klam_Pyrophillic_mntd[-1,]
Test_name<-("CA_Klam_Pyrophillic_mntd")
CA_Klam_final_Pyrophillic_mntd<-cbind(Test_name,CA_Klam_Pyrophillic_mntd)

#Pyrophobic

CA_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_Klam.matrix) <- rownames(bigcsv)
CA_Klam.matrix[1,] <- bigcsv$CA_Klam & bigcsv$Pyrophobic
CA_Klam.matrix[2,] <- bigcsv$CA_Klam & bigcsv$Pyrophobic

CA_Klam_Pyrophobic_mpd<-ses.mpd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Pyrophobic_mpd<-CA_Klam_Pyrophobic_mpd[-1,]
Test_name<-("CA_Klam_Pyrophobic_mpd")
CA_Klam_final_Pyrophobic_mpd<-cbind(Test_name,CA_Klam_Pyrophobic_mpd)


CA_Klam_Pyrophobic_mntd<-ses.mntd(samp = CA_Klam.matrix, dis =  cophenetic(CA_Klam.tre))
CA_Klam_Pyrophobic_mntd<-CA_Klam_Pyrophobic_mntd[-1,]
Test_name<-("CA_Klam_Pyrophobic_mntd")
CA_Klam_final_Pyrophobic_mntd<-cbind(Test_name,CA_Klam_Pyrophobic_mntd)




#get final csv for CA_Klam mpd
CA_Klam_final_mpd_combined<-rbind(CA_Klam_final_mpd_geog,CA_Klam_final_Bark_mpd,CA_Klam_final_Serotiny_mpd,CA_Klam_final_Grass_mpd,CA_Klam_final_Resprout_mpd,CA_Klam_final_Pyrophillic_mpd,CA_Klam_final_More_than_one_adaptation_mpd,CA_Klam_final_Pyrophobic_mpd)

#write to csv

write.csv(CA_Klam_final_mpd_combined, "Outfile_CA_Klam_final_mpd_combined.csv")

#get final csv for CA_Klam mntd
CA_Klam_final_mntd_combined<-rbind(CA_Klam_final_mntd_geog,CA_Klam_final_Bark_mntd,CA_Klam_final_Serotiny_mntd,CA_Klam_final_Grass_mntd,CA_Klam_final_Resprout_mntd,CA_Klam_final_Pyrophillic_mntd,CA_Klam_final_More_than_one_adaptation_mntd,CA_Klam_final_Pyrophobic_mntd)

#write to csv

write.csv(CA_Klam_final_mntd_combined, "Outfile_CA_Klam_final_mntd_combined.csv")





conif.tree<-read.tree("2018_North_America.tre")
conif.tree

plot(conif.tree)

#csv file change to account for Pinus contorta serotiny change

bigcsv<-read.csv("Pincon_Sierra_Absolute_true_master_csv_Feb_6_2024.csv",header=TRUE,row.names=1)



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)




CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam

CA_no_Klam.matrix

length(rownames(bigcsv))
CA_no_Klam <- rownames(bigcsv)[bigcsv$CA_no_Klam==1]

CA_no_Klam

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

CA_no_Klam.tre <- keep.tip(conif.tree, CA_no_Klam)
plotTree(CA_no_Klam.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = CA_no_Klam.matrix, dis = cophenetic(CA_no_Klam.tre))
CA_no_Klam_geog_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(conif.tree))
CA_no_Klam_geog_mpd<-CA_no_Klam_geog_mpd[-1,]
Test_name<-("CA_no_Klam_geog_mpd")
CA_no_Klam_final_mpd_geog<-cbind(Test_name,CA_no_Klam_geog_mpd)


mntd(samp = CA_no_Klam.matrix, dis = cophenetic(CA_no_Klam.tre))
CA_no_Klam_geog_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(conif.tree))
CA_no_Klam_geog_mntd<-CA_no_Klam_geog_mntd[-1,]
Test_name<-("CA_no_Klam_geog_mntd")
CA_no_Klam_final_mntd_geog<-cbind(Test_name,CA_no_Klam_geog_mntd)



#for traits

#Bark

#Set up the new matrix

CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$Bark
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$Bark

CA_no_Klam_Bark_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Bark_mpd<-CA_no_Klam_Bark_mpd[-1,]
Test_name<-("CA_no_Klam_Bark_mpd")
CA_no_Klam_final_Bark_mpd<-cbind(Test_name,CA_no_Klam_Bark_mpd)


CA_no_Klam_Bark_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Bark_mntd<-CA_no_Klam_Bark_mntd[-1,]
Test_name<-("CA_no_Klam_Bark_mntd")
CA_no_Klam_final_Bark_mntd<-cbind(Test_name,CA_no_Klam_Bark_mntd)

#Serotiny


CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$Serotiny
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$Serotiny

CA_no_Klam_Serotiny_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Serotiny_mpd<-CA_no_Klam_Serotiny_mpd[-1,]
Test_name<-("CA_no_Klam_Serotiny_mpd")
CA_no_Klam_final_Serotiny_mpd<-cbind(Test_name,CA_no_Klam_Serotiny_mpd)


CA_no_Klam_Serotiny_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Serotiny_mntd<-CA_no_Klam_Serotiny_mntd[-1,]
Test_name<-("CA_no_Klam_Serotiny_mntd")
CA_no_Klam_final_Serotiny_mntd<-cbind(Test_name,CA_no_Klam_Serotiny_mntd)

#Grass

CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$Grass
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$Grass

CA_no_Klam_Grass_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Grass_mpd<-CA_no_Klam_Grass_mpd[-1,]
Test_name<-("CA_no_Klam_Grass_mpd")
CA_no_Klam_final_Grass_mpd<-cbind(Test_name,CA_no_Klam_Grass_mpd)

CA_no_Klam_Grass_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Grass_mntd<-CA_no_Klam_Grass_mntd[-1,]
Test_name<-("CA_no_Klam_Grass_mntd")
CA_no_Klam_final_Grass_mntd<-cbind(Test_name,CA_no_Klam_Grass_mntd)


#Resprout

CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$Resprout
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$Resprout

CA_no_Klam_Resprout_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Resprout_mpd<-CA_no_Klam_Resprout_mpd[-1,]
Test_name<-("CA_no_Klam_Resprout_mpd")
CA_no_Klam_final_Resprout_mpd<-cbind(Test_name,CA_no_Klam_Resprout_mpd)


CA_no_Klam_Resprout_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Resprout_mntd<-CA_no_Klam_Resprout_mntd[-1,]
Test_name<-("CA_no_Klam_Resprout_mntd")
CA_no_Klam_final_Resprout_mntd<-cbind(Test_name,CA_no_Klam_Resprout_mntd)

#More_than_one_adaptation

CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$More_than_one_adaptation
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$More_than_one_adaptation

CA_no_Klam_More_than_one_adaptation_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_More_than_one_adaptation_mpd<-CA_no_Klam_More_than_one_adaptation_mpd[-1,]
Test_name<-("CA_no_Klam_More_than_one_adaptation_mpd")
CA_no_Klam_final_More_than_one_adaptation_mpd<-cbind(Test_name,CA_no_Klam_More_than_one_adaptation_mpd)


CA_no_Klam_More_than_one_adaptation_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_More_than_one_adaptation_mntd<-CA_no_Klam_More_than_one_adaptation_mntd[-1,]
Test_name<-("CA_no_Klam_More_than_one_adaptation_mntd")
CA_no_Klam_final_More_than_one_adaptation_mntd<-cbind(Test_name,CA_no_Klam_More_than_one_adaptation_mntd)

#Pyrophillic

CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$Pyrophillic
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$Pyrophillic

CA_no_Klam_Pyrophillic_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Pyrophillic_mpd<-CA_no_Klam_Pyrophillic_mpd[-1,]
Test_name<-("CA_no_Klam_Pyrophillic_mpd")
CA_no_Klam_final_Pyrophillic_mpd<-cbind(Test_name,CA_no_Klam_Pyrophillic_mpd)


CA_no_Klam_Pyrophillic_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Pyrophillic_mntd<-CA_no_Klam_Pyrophillic_mntd[-1,]
Test_name<-("CA_no_Klam_Pyrophillic_mntd")
CA_no_Klam_final_Pyrophillic_mntd<-cbind(Test_name,CA_no_Klam_Pyrophillic_mntd)

#Pyrophobic

CA_no_Klam.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(CA_no_Klam.matrix) <- rownames(bigcsv)
CA_no_Klam.matrix[1,] <- bigcsv$CA_no_Klam & bigcsv$Pyrophobic
CA_no_Klam.matrix[2,] <- bigcsv$CA_no_Klam & bigcsv$Pyrophobic

CA_no_Klam_Pyrophobic_mpd<-ses.mpd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Pyrophobic_mpd<-CA_no_Klam_Pyrophobic_mpd[-1,]
Test_name<-("CA_no_Klam_Pyrophobic_mpd")
CA_no_Klam_final_Pyrophobic_mpd<-cbind(Test_name,CA_no_Klam_Pyrophobic_mpd)


CA_no_Klam_Pyrophobic_mntd<-ses.mntd(samp = CA_no_Klam.matrix, dis =  cophenetic(CA_no_Klam.tre))
CA_no_Klam_Pyrophobic_mntd<-CA_no_Klam_Pyrophobic_mntd[-1,]
Test_name<-("CA_no_Klam_Pyrophobic_mntd")
CA_no_Klam_final_Pyrophobic_mntd<-cbind(Test_name,CA_no_Klam_Pyrophobic_mntd)




#get final csv for CA_no_Klam mpd
CA_no_Klam_final_mpd_combined<-rbind(CA_no_Klam_final_mpd_geog,CA_no_Klam_final_Bark_mpd,CA_no_Klam_final_Serotiny_mpd,CA_no_Klam_final_Grass_mpd,CA_no_Klam_final_Resprout_mpd,CA_no_Klam_final_Pyrophillic_mpd,CA_no_Klam_final_More_than_one_adaptation_mpd,CA_no_Klam_final_Pyrophobic_mpd)

#write to csv

write.csv(CA_no_Klam_final_mpd_combined, "Outfile_CA_no_Klam_final_mpd_combined.csv")

#get final csv for CA_no_Klam mntd
CA_no_Klam_final_mntd_combined<-rbind(CA_no_Klam_final_mntd_geog,CA_no_Klam_final_Bark_mntd,CA_no_Klam_final_Serotiny_mntd,CA_no_Klam_final_Grass_mntd,CA_no_Klam_final_Resprout_mntd,CA_no_Klam_final_Pyrophillic_mntd,CA_no_Klam_final_More_than_one_adaptation_mntd,CA_no_Klam_final_Pyrophobic_mntd)

#write to csv

write.csv(CA_no_Klam_final_mntd_combined, "Outfile_CA_no_Klam_final_mntd_combined.csv")




Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra
Sierra.matrix[2,] <- bigcsv$Sierra

Sierra.matrix

length(rownames(bigcsv))
Sierra <- rownames(bigcsv)[bigcsv$Sierra==1]

Sierra

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Sierra.tre <- keep.tip(conif.tree, Sierra)
plotTree(Sierra.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Sierra.matrix, dis = cophenetic(Sierra.tre))
Sierra_geog_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(conif.tree))
Sierra_geog_mpd<-Sierra_geog_mpd[-1,]
Test_name<-("Sierra_geog_mpd")
Sierra_final_mpd_geog<-cbind(Test_name,Sierra_geog_mpd)


mntd(samp = Sierra.matrix, dis = cophenetic(Sierra.tre))
Sierra_geog_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(conif.tree))
Sierra_geog_mntd<-Sierra_geog_mntd[-1,]
Test_name<-("Sierra_geog_mntd")
Sierra_final_mntd_geog<-cbind(Test_name,Sierra_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$Bark
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$Bark

Sierra_Bark_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Bark_mpd<-Sierra_Bark_mpd[-1,]
Test_name<-("Sierra_Bark_mpd")
Sierra_final_Bark_mpd<-cbind(Test_name,Sierra_Bark_mpd)


Sierra_Bark_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Bark_mntd<-Sierra_Bark_mntd[-1,]
Test_name<-("Sierra_Bark_mntd")
Sierra_final_Bark_mntd<-cbind(Test_name,Sierra_Bark_mntd)

#Serotiny


Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$Serotiny
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$Serotiny

Sierra_Serotiny_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Serotiny_mpd<-Sierra_Serotiny_mpd[-1,]
Test_name<-("Sierra_Serotiny_mpd")
Sierra_final_Serotiny_mpd<-cbind(Test_name,Sierra_Serotiny_mpd)


Sierra_Serotiny_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Serotiny_mntd<-Sierra_Serotiny_mntd[-1,]
Test_name<-("Sierra_Serotiny_mntd")
Sierra_final_Serotiny_mntd<-cbind(Test_name,Sierra_Serotiny_mntd)

#Grass

Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$Grass
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$Grass

Sierra_Grass_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Grass_mpd<-Sierra_Grass_mpd[-1,]
Test_name<-("Sierra_Grass_mpd")
Sierra_final_Grass_mpd<-cbind(Test_name,Sierra_Grass_mpd)

Sierra_Grass_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Grass_mntd<-Sierra_Grass_mntd[-1,]
Test_name<-("Sierra_Grass_mntd")
Sierra_final_Grass_mntd<-cbind(Test_name,Sierra_Grass_mntd)


#Resprout

Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$Resprout
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$Resprout

Sierra_Resprout_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Resprout_mpd<-Sierra_Resprout_mpd[-1,]
Test_name<-("Sierra_Resprout_mpd")
Sierra_final_Resprout_mpd<-cbind(Test_name,Sierra_Resprout_mpd)


Sierra_Resprout_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Resprout_mntd<-Sierra_Resprout_mntd[-1,]
Test_name<-("Sierra_Resprout_mntd")
Sierra_final_Resprout_mntd<-cbind(Test_name,Sierra_Resprout_mntd)

#More_than_one_adaptation

Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$More_than_one_adaptation
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$More_than_one_adaptation

Sierra_More_than_one_adaptation_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_More_than_one_adaptation_mpd<-Sierra_More_than_one_adaptation_mpd[-1,]
Test_name<-("Sierra_More_than_one_adaptation_mpd")
Sierra_final_More_than_one_adaptation_mpd<-cbind(Test_name,Sierra_More_than_one_adaptation_mpd)


Sierra_More_than_one_adaptation_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_More_than_one_adaptation_mntd<-Sierra_More_than_one_adaptation_mntd[-1,]
Test_name<-("Sierra_More_than_one_adaptation_mntd")
Sierra_final_More_than_one_adaptation_mntd<-cbind(Test_name,Sierra_More_than_one_adaptation_mntd)

#Pyrophillic

Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$Pyrophillic
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$Pyrophillic

Sierra_Pyrophillic_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Pyrophillic_mpd<-Sierra_Pyrophillic_mpd[-1,]
Test_name<-("Sierra_Pyrophillic_mpd")
Sierra_final_Pyrophillic_mpd<-cbind(Test_name,Sierra_Pyrophillic_mpd)


Sierra_Pyrophillic_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Pyrophillic_mntd<-Sierra_Pyrophillic_mntd[-1,]
Test_name<-("Sierra_Pyrophillic_mntd")
Sierra_final_Pyrophillic_mntd<-cbind(Test_name,Sierra_Pyrophillic_mntd)

#Pyrophobic

Sierra.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Sierra.matrix) <- rownames(bigcsv)
Sierra.matrix[1,] <- bigcsv$Sierra & bigcsv$Pyrophobic
Sierra.matrix[2,] <- bigcsv$Sierra & bigcsv$Pyrophobic

Sierra_Pyrophobic_mpd<-ses.mpd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Pyrophobic_mpd<-Sierra_Pyrophobic_mpd[-1,]
Test_name<-("Sierra_Pyrophobic_mpd")
Sierra_final_Pyrophobic_mpd<-cbind(Test_name,Sierra_Pyrophobic_mpd)


Sierra_Pyrophobic_mntd<-ses.mntd(samp = Sierra.matrix, dis =  cophenetic(Sierra.tre))
Sierra_Pyrophobic_mntd<-Sierra_Pyrophobic_mntd[-1,]
Test_name<-("Sierra_Pyrophobic_mntd")
Sierra_final_Pyrophobic_mntd<-cbind(Test_name,Sierra_Pyrophobic_mntd)




#get final csv for Sierra mpd
Sierra_final_mpd_combined<-rbind(Sierra_final_mpd_geog,Sierra_final_Bark_mpd,Sierra_final_Serotiny_mpd,Sierra_final_Grass_mpd,Sierra_final_Resprout_mpd,Sierra_final_Pyrophillic_mpd,Sierra_final_More_than_one_adaptation_mpd,Sierra_final_Pyrophobic_mpd)

#write to csv

write.csv(Sierra_final_mpd_combined, "Outfile_Sierra_final_mpd_combined.csv")

#get final csv for Sierra mntd
Sierra_final_mntd_combined<-rbind(Sierra_final_mntd_geog,Sierra_final_Bark_mntd,Sierra_final_Serotiny_mntd,Sierra_final_Grass_mntd,Sierra_final_Resprout_mntd,Sierra_final_Pyrophillic_mntd,Sierra_final_More_than_one_adaptation_mntd,Sierra_final_Pyrophobic_mntd)

#write to csv

write.csv(Sierra_final_mntd_combined, "Outfile_Sierra_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA

Coastal_CA.matrix

length(rownames(bigcsv))
Coastal_CA <- rownames(bigcsv)[bigcsv$Coastal_CA==1]

Coastal_CA

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Coastal_CA.tre <- keep.tip(conif.tree, Coastal_CA)
plotTree(Coastal_CA.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Coastal_CA.matrix, dis = cophenetic(Coastal_CA.tre))
Coastal_CA_geog_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(conif.tree))
Coastal_CA_geog_mpd<-Coastal_CA_geog_mpd[-1,]
Test_name<-("Coastal_CA_geog_mpd")
Coastal_CA_final_mpd_geog<-cbind(Test_name,Coastal_CA_geog_mpd)


mntd(samp = Coastal_CA.matrix, dis = cophenetic(Coastal_CA.tre))
Coastal_CA_geog_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(conif.tree))
Coastal_CA_geog_mntd<-Coastal_CA_geog_mntd[-1,]
Test_name<-("Coastal_CA_geog_mntd")
Coastal_CA_final_mntd_geog<-cbind(Test_name,Coastal_CA_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$Bark
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$Bark

Coastal_CA_Bark_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Bark_mpd<-Coastal_CA_Bark_mpd[-1,]
Test_name<-("Coastal_CA_Bark_mpd")
Coastal_CA_final_Bark_mpd<-cbind(Test_name,Coastal_CA_Bark_mpd)


Coastal_CA_Bark_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Bark_mntd<-Coastal_CA_Bark_mntd[-1,]
Test_name<-("Coastal_CA_Bark_mntd")
Coastal_CA_final_Bark_mntd<-cbind(Test_name,Coastal_CA_Bark_mntd)

#Serotiny


Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$Serotiny
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$Serotiny

Coastal_CA_Serotiny_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Serotiny_mpd<-Coastal_CA_Serotiny_mpd[-1,]
Test_name<-("Coastal_CA_Serotiny_mpd")
Coastal_CA_final_Serotiny_mpd<-cbind(Test_name,Coastal_CA_Serotiny_mpd)


Coastal_CA_Serotiny_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Serotiny_mntd<-Coastal_CA_Serotiny_mntd[-1,]
Test_name<-("Coastal_CA_Serotiny_mntd")
Coastal_CA_final_Serotiny_mntd<-cbind(Test_name,Coastal_CA_Serotiny_mntd)

#Grass

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$Grass
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$Grass

Coastal_CA_Grass_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Grass_mpd<-Coastal_CA_Grass_mpd[-1,]
Test_name<-("Coastal_CA_Grass_mpd")
Coastal_CA_final_Grass_mpd<-cbind(Test_name,Coastal_CA_Grass_mpd)

Coastal_CA_Grass_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Grass_mntd<-Coastal_CA_Grass_mntd[-1,]
Test_name<-("Coastal_CA_Grass_mntd")
Coastal_CA_final_Grass_mntd<-cbind(Test_name,Coastal_CA_Grass_mntd)


#Resprout

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$Resprout
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$Resprout

Coastal_CA_Resprout_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Resprout_mpd<-Coastal_CA_Resprout_mpd[-1,]
Test_name<-("Coastal_CA_Resprout_mpd")
Coastal_CA_final_Resprout_mpd<-cbind(Test_name,Coastal_CA_Resprout_mpd)


Coastal_CA_Resprout_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Resprout_mntd<-Coastal_CA_Resprout_mntd[-1,]
Test_name<-("Coastal_CA_Resprout_mntd")
Coastal_CA_final_Resprout_mntd<-cbind(Test_name,Coastal_CA_Resprout_mntd)

#More_than_one_adaptation

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$More_than_one_adaptation
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$More_than_one_adaptation

Coastal_CA_More_than_one_adaptation_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_More_than_one_adaptation_mpd<-Coastal_CA_More_than_one_adaptation_mpd[-1,]
Test_name<-("Coastal_CA_More_than_one_adaptation_mpd")
Coastal_CA_final_More_than_one_adaptation_mpd<-cbind(Test_name,Coastal_CA_More_than_one_adaptation_mpd)


Coastal_CA_More_than_one_adaptation_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_More_than_one_adaptation_mntd<-Coastal_CA_More_than_one_adaptation_mntd[-1,]
Test_name<-("Coastal_CA_More_than_one_adaptation_mntd")
Coastal_CA_final_More_than_one_adaptation_mntd<-cbind(Test_name,Coastal_CA_More_than_one_adaptation_mntd)

#Pyrophillic

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$Pyrophillic
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$Pyrophillic

Coastal_CA_Pyrophillic_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Pyrophillic_mpd<-Coastal_CA_Pyrophillic_mpd[-1,]
Test_name<-("Coastal_CA_Pyrophillic_mpd")
Coastal_CA_final_Pyrophillic_mpd<-cbind(Test_name,Coastal_CA_Pyrophillic_mpd)


Coastal_CA_Pyrophillic_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Pyrophillic_mntd<-Coastal_CA_Pyrophillic_mntd[-1,]
Test_name<-("Coastal_CA_Pyrophillic_mntd")
Coastal_CA_final_Pyrophillic_mntd<-cbind(Test_name,Coastal_CA_Pyrophillic_mntd)

#Pyrophobic

Coastal_CA.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Coastal_CA.matrix) <- rownames(bigcsv)
Coastal_CA.matrix[1,] <- bigcsv$Coastal_CA & bigcsv$Pyrophobic
Coastal_CA.matrix[2,] <- bigcsv$Coastal_CA & bigcsv$Pyrophobic

Coastal_CA_Pyrophobic_mpd<-ses.mpd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Pyrophobic_mpd<-Coastal_CA_Pyrophobic_mpd[-1,]
Test_name<-("Coastal_CA_Pyrophobic_mpd")
Coastal_CA_final_Pyrophobic_mpd<-cbind(Test_name,Coastal_CA_Pyrophobic_mpd)


Coastal_CA_Pyrophobic_mntd<-ses.mntd(samp = Coastal_CA.matrix, dis =  cophenetic(Coastal_CA.tre))
Coastal_CA_Pyrophobic_mntd<-Coastal_CA_Pyrophobic_mntd[-1,]
Test_name<-("Coastal_CA_Pyrophobic_mntd")
Coastal_CA_final_Pyrophobic_mntd<-cbind(Test_name,Coastal_CA_Pyrophobic_mntd)




#get final csv for Coastal_CA mpd
Coastal_CA_final_mpd_combined<-rbind(Coastal_CA_final_mpd_geog,Coastal_CA_final_Bark_mpd,Coastal_CA_final_Serotiny_mpd,Coastal_CA_final_Grass_mpd,Coastal_CA_final_Resprout_mpd,Coastal_CA_final_Pyrophillic_mpd,Coastal_CA_final_More_than_one_adaptation_mpd,Coastal_CA_final_Pyrophobic_mpd)

#write to csv

write.csv(Coastal_CA_final_mpd_combined, "Outfile_Coastal_CA_final_mpd_combined.csv")

#get final csv for Coastal_CA mntd
Coastal_CA_final_mntd_combined<-rbind(Coastal_CA_final_mntd_geog,Coastal_CA_final_Bark_mntd,Coastal_CA_final_Serotiny_mntd,Coastal_CA_final_Grass_mntd,Coastal_CA_final_Resprout_mntd,Coastal_CA_final_Pyrophillic_mntd,Coastal_CA_final_More_than_one_adaptation_mntd,Coastal_CA_final_Pyrophobic_mntd)

#write to csv

write.csv(Coastal_CA_final_mntd_combined, "Outfile_Coastal_CA_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja
AltoBaja.matrix[2,] <- bigcsv$AltoBaja

AltoBaja.matrix

length(rownames(bigcsv))
AltoBaja <- rownames(bigcsv)[bigcsv$AltoBaja==1]

AltoBaja

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

AltoBaja.tre <- keep.tip(conif.tree, AltoBaja)
plotTree(AltoBaja.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = AltoBaja.matrix, dis = cophenetic(AltoBaja.tre))
AltoBaja_geog_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(conif.tree))
AltoBaja_geog_mpd<-AltoBaja_geog_mpd[-1,]
Test_name<-("AltoBaja_geog_mpd")
AltoBaja_final_mpd_geog<-cbind(Test_name,AltoBaja_geog_mpd)


mntd(samp = AltoBaja.matrix, dis = cophenetic(AltoBaja.tre))
AltoBaja_geog_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(conif.tree))
AltoBaja_geog_mntd<-AltoBaja_geog_mntd[-1,]
Test_name<-("AltoBaja_geog_mntd")
AltoBaja_final_mntd_geog<-cbind(Test_name,AltoBaja_geog_mntd)



#for traits

#Bark

#Set up the new matrix

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$Bark
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$Bark

AltoBaja_Bark_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Bark_mpd<-AltoBaja_Bark_mpd[-1,]
Test_name<-("AltoBaja_Bark_mpd")
AltoBaja_final_Bark_mpd<-cbind(Test_name,AltoBaja_Bark_mpd)


AltoBaja_Bark_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Bark_mntd<-AltoBaja_Bark_mntd[-1,]
Test_name<-("AltoBaja_Bark_mntd")
AltoBaja_final_Bark_mntd<-cbind(Test_name,AltoBaja_Bark_mntd)

#Serotiny


AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$Serotiny
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$Serotiny

AltoBaja_Serotiny_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Serotiny_mpd<-AltoBaja_Serotiny_mpd[-1,]
Test_name<-("AltoBaja_Serotiny_mpd")
AltoBaja_final_Serotiny_mpd<-cbind(Test_name,AltoBaja_Serotiny_mpd)


AltoBaja_Serotiny_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Serotiny_mntd<-AltoBaja_Serotiny_mntd[-1,]
Test_name<-("AltoBaja_Serotiny_mntd")
AltoBaja_final_Serotiny_mntd<-cbind(Test_name,AltoBaja_Serotiny_mntd)

#Grass

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$Grass
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$Grass

AltoBaja_Grass_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Grass_mpd<-AltoBaja_Grass_mpd[-1,]
Test_name<-("AltoBaja_Grass_mpd")
AltoBaja_final_Grass_mpd<-cbind(Test_name,AltoBaja_Grass_mpd)

AltoBaja_Grass_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Grass_mntd<-AltoBaja_Grass_mntd[-1,]
Test_name<-("AltoBaja_Grass_mntd")
AltoBaja_final_Grass_mntd<-cbind(Test_name,AltoBaja_Grass_mntd)


#Resprout

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$Resprout
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$Resprout

AltoBaja_Resprout_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Resprout_mpd<-AltoBaja_Resprout_mpd[-1,]
Test_name<-("AltoBaja_Resprout_mpd")
AltoBaja_final_Resprout_mpd<-cbind(Test_name,AltoBaja_Resprout_mpd)


AltoBaja_Resprout_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Resprout_mntd<-AltoBaja_Resprout_mntd[-1,]
Test_name<-("AltoBaja_Resprout_mntd")
AltoBaja_final_Resprout_mntd<-cbind(Test_name,AltoBaja_Resprout_mntd)

#More_than_one_adaptation

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$More_than_one_adaptation
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$More_than_one_adaptation

AltoBaja_More_than_one_adaptation_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_More_than_one_adaptation_mpd<-AltoBaja_More_than_one_adaptation_mpd[-1,]
Test_name<-("AltoBaja_More_than_one_adaptation_mpd")
AltoBaja_final_More_than_one_adaptation_mpd<-cbind(Test_name,AltoBaja_More_than_one_adaptation_mpd)


AltoBaja_More_than_one_adaptation_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_More_than_one_adaptation_mntd<-AltoBaja_More_than_one_adaptation_mntd[-1,]
Test_name<-("AltoBaja_More_than_one_adaptation_mntd")
AltoBaja_final_More_than_one_adaptation_mntd<-cbind(Test_name,AltoBaja_More_than_one_adaptation_mntd)

#Pyrophillic

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$Pyrophillic
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$Pyrophillic

AltoBaja_Pyrophillic_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Pyrophillic_mpd<-AltoBaja_Pyrophillic_mpd[-1,]
Test_name<-("AltoBaja_Pyrophillic_mpd")
AltoBaja_final_Pyrophillic_mpd<-cbind(Test_name,AltoBaja_Pyrophillic_mpd)


AltoBaja_Pyrophillic_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Pyrophillic_mntd<-AltoBaja_Pyrophillic_mntd[-1,]
Test_name<-("AltoBaja_Pyrophillic_mntd")
AltoBaja_final_Pyrophillic_mntd<-cbind(Test_name,AltoBaja_Pyrophillic_mntd)

#Pyrophobic

AltoBaja.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(AltoBaja.matrix) <- rownames(bigcsv)
AltoBaja.matrix[1,] <- bigcsv$AltoBaja & bigcsv$Pyrophobic
AltoBaja.matrix[2,] <- bigcsv$AltoBaja & bigcsv$Pyrophobic

AltoBaja_Pyrophobic_mpd<-ses.mpd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Pyrophobic_mpd<-AltoBaja_Pyrophobic_mpd[-1,]
Test_name<-("AltoBaja_Pyrophobic_mpd")
AltoBaja_final_Pyrophobic_mpd<-cbind(Test_name,AltoBaja_Pyrophobic_mpd)


AltoBaja_Pyrophobic_mntd<-ses.mntd(samp = AltoBaja.matrix, dis =  cophenetic(AltoBaja.tre))
AltoBaja_Pyrophobic_mntd<-AltoBaja_Pyrophobic_mntd[-1,]
Test_name<-("AltoBaja_Pyrophobic_mntd")
AltoBaja_final_Pyrophobic_mntd<-cbind(Test_name,AltoBaja_Pyrophobic_mntd)




#get final csv for AltoBaja mpd
AltoBaja_final_mpd_combined<-rbind(AltoBaja_final_mpd_geog,AltoBaja_final_Bark_mpd,AltoBaja_final_Serotiny_mpd,AltoBaja_final_Grass_mpd,AltoBaja_final_Resprout_mpd,AltoBaja_final_Pyrophillic_mpd,AltoBaja_final_More_than_one_adaptation_mpd,AltoBaja_final_Pyrophobic_mpd)

#write to csv

write.csv(AltoBaja_final_mpd_combined, "Outfile_AltoBaja_final_mpd_combined.csv")

#get final csv for AltoBaja mntd
AltoBaja_final_mntd_combined<-rbind(AltoBaja_final_mntd_geog,AltoBaja_final_Bark_mntd,AltoBaja_final_Serotiny_mntd,AltoBaja_final_Grass_mntd,AltoBaja_final_Resprout_mntd,AltoBaja_final_Pyrophillic_mntd,AltoBaja_final_More_than_one_adaptation_mntd,AltoBaja_final_Pyrophobic_mntd)

#write to csv

write.csv(AltoBaja_final_mntd_combined, "Outfile_AltoBaja_final_mntd_combined.csv")


conif.tree<-read.tree("2018_North_America.tre")
conif.tree

plot(conif.tree)

#Changing back to serotinous Pinus contorta matrix

bigcsv<-read.csv("Absolute_true_master_csv_For_supplement.csv",header=TRUE,row.names=1)


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)



Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin
Basin.matrix[2,] <- bigcsv$Basin

Basin.matrix

length(rownames(bigcsv))
Basin <- rownames(bigcsv)[bigcsv$Basin==1]

Basin

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Basin.tre <- keep.tip(conif.tree, Basin)
plotTree(Basin.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Basin.matrix, dis = cophenetic(Basin.tre))
Basin_geog_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(conif.tree))
Basin_geog_mpd<-Basin_geog_mpd[-1,]
Test_name<-("Basin_geog_mpd")
Basin_final_mpd_geog<-cbind(Test_name,Basin_geog_mpd)


mntd(samp = Basin.matrix, dis = cophenetic(Basin.tre))
Basin_geog_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(conif.tree))
Basin_geog_mntd<-Basin_geog_mntd[-1,]
Test_name<-("Basin_geog_mntd")
Basin_final_mntd_geog<-cbind(Test_name,Basin_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$Bark
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$Bark

Basin_Bark_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Bark_mpd<-Basin_Bark_mpd[-1,]
Test_name<-("Basin_Bark_mpd")
Basin_final_Bark_mpd<-cbind(Test_name,Basin_Bark_mpd)


Basin_Bark_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Bark_mntd<-Basin_Bark_mntd[-1,]
Test_name<-("Basin_Bark_mntd")
Basin_final_Bark_mntd<-cbind(Test_name,Basin_Bark_mntd)

#Serotiny


Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$Serotiny
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$Serotiny

Basin_Serotiny_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Serotiny_mpd<-Basin_Serotiny_mpd[-1,]
Test_name<-("Basin_Serotiny_mpd")
Basin_final_Serotiny_mpd<-cbind(Test_name,Basin_Serotiny_mpd)


Basin_Serotiny_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Serotiny_mntd<-Basin_Serotiny_mntd[-1,]
Test_name<-("Basin_Serotiny_mntd")
Basin_final_Serotiny_mntd<-cbind(Test_name,Basin_Serotiny_mntd)

#Grass

Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$Grass
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$Grass

Basin_Grass_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Grass_mpd<-Basin_Grass_mpd[-1,]
Test_name<-("Basin_Grass_mpd")
Basin_final_Grass_mpd<-cbind(Test_name,Basin_Grass_mpd)

Basin_Grass_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Grass_mntd<-Basin_Grass_mntd[-1,]
Test_name<-("Basin_Grass_mntd")
Basin_final_Grass_mntd<-cbind(Test_name,Basin_Grass_mntd)


#Resprout

Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$Resprout
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$Resprout

Basin_Resprout_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Resprout_mpd<-Basin_Resprout_mpd[-1,]
Test_name<-("Basin_Resprout_mpd")
Basin_final_Resprout_mpd<-cbind(Test_name,Basin_Resprout_mpd)


Basin_Resprout_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Resprout_mntd<-Basin_Resprout_mntd[-1,]
Test_name<-("Basin_Resprout_mntd")
Basin_final_Resprout_mntd<-cbind(Test_name,Basin_Resprout_mntd)

#More_than_one_adaptation

Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$More_than_one_adaptation
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$More_than_one_adaptation

Basin_More_than_one_adaptation_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_More_than_one_adaptation_mpd<-Basin_More_than_one_adaptation_mpd[-1,]
Test_name<-("Basin_More_than_one_adaptation_mpd")
Basin_final_More_than_one_adaptation_mpd<-cbind(Test_name,Basin_More_than_one_adaptation_mpd)


Basin_More_than_one_adaptation_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_More_than_one_adaptation_mntd<-Basin_More_than_one_adaptation_mntd[-1,]
Test_name<-("Basin_More_than_one_adaptation_mntd")
Basin_final_More_than_one_adaptation_mntd<-cbind(Test_name,Basin_More_than_one_adaptation_mntd)

#Pyrophillic

Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$Pyrophillic
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$Pyrophillic

Basin_Pyrophillic_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Pyrophillic_mpd<-Basin_Pyrophillic_mpd[-1,]
Test_name<-("Basin_Pyrophillic_mpd")
Basin_final_Pyrophillic_mpd<-cbind(Test_name,Basin_Pyrophillic_mpd)


Basin_Pyrophillic_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Pyrophillic_mntd<-Basin_Pyrophillic_mntd[-1,]
Test_name<-("Basin_Pyrophillic_mntd")
Basin_final_Pyrophillic_mntd<-cbind(Test_name,Basin_Pyrophillic_mntd)

#Pyrophobic

Basin.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Basin.matrix) <- rownames(bigcsv)
Basin.matrix[1,] <- bigcsv$Basin & bigcsv$Pyrophobic
Basin.matrix[2,] <- bigcsv$Basin & bigcsv$Pyrophobic

Basin_Pyrophobic_mpd<-ses.mpd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Pyrophobic_mpd<-Basin_Pyrophobic_mpd[-1,]
Test_name<-("Basin_Pyrophobic_mpd")
Basin_final_Pyrophobic_mpd<-cbind(Test_name,Basin_Pyrophobic_mpd)


Basin_Pyrophobic_mntd<-ses.mntd(samp = Basin.matrix, dis =  cophenetic(Basin.tre))
Basin_Pyrophobic_mntd<-Basin_Pyrophobic_mntd[-1,]
Test_name<-("Basin_Pyrophobic_mntd")
Basin_final_Pyrophobic_mntd<-cbind(Test_name,Basin_Pyrophobic_mntd)




#get final csv for Basin mpd
Basin_final_mpd_combined<-rbind(Basin_final_mpd_geog,Basin_final_Bark_mpd,Basin_final_Serotiny_mpd,Basin_final_Grass_mpd,Basin_final_Resprout_mpd,Basin_final_Pyrophillic_mpd,Basin_final_More_than_one_adaptation_mpd,Basin_final_Pyrophobic_mpd)

#write to csv

write.csv(Basin_final_mpd_combined, "Outfile_Basin_final_mpd_combined.csv")

#get final csv for Basin mntd
Basin_final_mntd_combined<-rbind(Basin_final_mntd_geog,Basin_final_Bark_mntd,Basin_final_Serotiny_mntd,Basin_final_Grass_mntd,Basin_final_Resprout_mntd,Basin_final_Pyrophillic_mntd,Basin_final_More_than_one_adaptation_mntd,Basin_final_Pyrophobic_mntd)

#write to csv

write.csv(Basin_final_mntd_combined, "Outfile_Basin_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon
Mogollon.matrix[2,] <- bigcsv$Mogollon

Mogollon.matrix

length(rownames(bigcsv))
Mogollon <- rownames(bigcsv)[bigcsv$Mogollon==1]

Mogollon

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Mogollon.tre <- keep.tip(conif.tree, Mogollon)
plotTree(Mogollon.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Mogollon.matrix, dis = cophenetic(Mogollon.tre))
Mogollon_geog_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(conif.tree))
Mogollon_geog_mpd<-Mogollon_geog_mpd[-1,]
Test_name<-("Mogollon_geog_mpd")
Mogollon_final_mpd_geog<-cbind(Test_name,Mogollon_geog_mpd)


mntd(samp = Mogollon.matrix, dis = cophenetic(Mogollon.tre))
Mogollon_geog_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(conif.tree))
Mogollon_geog_mntd<-Mogollon_geog_mntd[-1,]
Test_name<-("Mogollon_geog_mntd")
Mogollon_final_mntd_geog<-cbind(Test_name,Mogollon_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$Bark
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$Bark

Mogollon_Bark_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Bark_mpd<-Mogollon_Bark_mpd[-1,]
Test_name<-("Mogollon_Bark_mpd")
Mogollon_final_Bark_mpd<-cbind(Test_name,Mogollon_Bark_mpd)


Mogollon_Bark_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Bark_mntd<-Mogollon_Bark_mntd[-1,]
Test_name<-("Mogollon_Bark_mntd")
Mogollon_final_Bark_mntd<-cbind(Test_name,Mogollon_Bark_mntd)

#Serotiny


Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$Serotiny
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$Serotiny

Mogollon_Serotiny_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Serotiny_mpd<-Mogollon_Serotiny_mpd[-1,]
Test_name<-("Mogollon_Serotiny_mpd")
Mogollon_final_Serotiny_mpd<-cbind(Test_name,Mogollon_Serotiny_mpd)


Mogollon_Serotiny_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Serotiny_mntd<-Mogollon_Serotiny_mntd[-1,]
Test_name<-("Mogollon_Serotiny_mntd")
Mogollon_final_Serotiny_mntd<-cbind(Test_name,Mogollon_Serotiny_mntd)

#Grass

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$Grass
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$Grass

Mogollon_Grass_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Grass_mpd<-Mogollon_Grass_mpd[-1,]
Test_name<-("Mogollon_Grass_mpd")
Mogollon_final_Grass_mpd<-cbind(Test_name,Mogollon_Grass_mpd)

Mogollon_Grass_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Grass_mntd<-Mogollon_Grass_mntd[-1,]
Test_name<-("Mogollon_Grass_mntd")
Mogollon_final_Grass_mntd<-cbind(Test_name,Mogollon_Grass_mntd)


#Resprout

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$Resprout
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$Resprout

Mogollon_Resprout_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Resprout_mpd<-Mogollon_Resprout_mpd[-1,]
Test_name<-("Mogollon_Resprout_mpd")
Mogollon_final_Resprout_mpd<-cbind(Test_name,Mogollon_Resprout_mpd)


Mogollon_Resprout_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Resprout_mntd<-Mogollon_Resprout_mntd[-1,]
Test_name<-("Mogollon_Resprout_mntd")
Mogollon_final_Resprout_mntd<-cbind(Test_name,Mogollon_Resprout_mntd)

#More_than_one_adaptation

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$More_than_one_adaptation
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$More_than_one_adaptation

Mogollon_More_than_one_adaptation_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_More_than_one_adaptation_mpd<-Mogollon_More_than_one_adaptation_mpd[-1,]
Test_name<-("Mogollon_More_than_one_adaptation_mpd")
Mogollon_final_More_than_one_adaptation_mpd<-cbind(Test_name,Mogollon_More_than_one_adaptation_mpd)


Mogollon_More_than_one_adaptation_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_More_than_one_adaptation_mntd<-Mogollon_More_than_one_adaptation_mntd[-1,]
Test_name<-("Mogollon_More_than_one_adaptation_mntd")
Mogollon_final_More_than_one_adaptation_mntd<-cbind(Test_name,Mogollon_More_than_one_adaptation_mntd)

#Pyrophillic

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$Pyrophillic
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$Pyrophillic

Mogollon_Pyrophillic_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Pyrophillic_mpd<-Mogollon_Pyrophillic_mpd[-1,]
Test_name<-("Mogollon_Pyrophillic_mpd")
Mogollon_final_Pyrophillic_mpd<-cbind(Test_name,Mogollon_Pyrophillic_mpd)


Mogollon_Pyrophillic_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Pyrophillic_mntd<-Mogollon_Pyrophillic_mntd[-1,]
Test_name<-("Mogollon_Pyrophillic_mntd")
Mogollon_final_Pyrophillic_mntd<-cbind(Test_name,Mogollon_Pyrophillic_mntd)

#Pyrophobic

Mogollon.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mogollon.matrix) <- rownames(bigcsv)
Mogollon.matrix[1,] <- bigcsv$Mogollon & bigcsv$Pyrophobic
Mogollon.matrix[2,] <- bigcsv$Mogollon & bigcsv$Pyrophobic

Mogollon_Pyrophobic_mpd<-ses.mpd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Pyrophobic_mpd<-Mogollon_Pyrophobic_mpd[-1,]
Test_name<-("Mogollon_Pyrophobic_mpd")
Mogollon_final_Pyrophobic_mpd<-cbind(Test_name,Mogollon_Pyrophobic_mpd)


Mogollon_Pyrophobic_mntd<-ses.mntd(samp = Mogollon.matrix, dis =  cophenetic(Mogollon.tre))
Mogollon_Pyrophobic_mntd<-Mogollon_Pyrophobic_mntd[-1,]
Test_name<-("Mogollon_Pyrophobic_mntd")
Mogollon_final_Pyrophobic_mntd<-cbind(Test_name,Mogollon_Pyrophobic_mntd)




#get final csv for Mogollon mpd
Mogollon_final_mpd_combined<-rbind(Mogollon_final_mpd_geog,Mogollon_final_Bark_mpd,Mogollon_final_Serotiny_mpd,Mogollon_final_Grass_mpd,Mogollon_final_Resprout_mpd,Mogollon_final_Pyrophillic_mpd,Mogollon_final_More_than_one_adaptation_mpd,Mogollon_final_Pyrophobic_mpd)

#write to csv

write.csv(Mogollon_final_mpd_combined, "Outfile_Mogollon_final_mpd_combined.csv")

#get final csv for Mogollon mntd
Mogollon_final_mntd_combined<-rbind(Mogollon_final_mntd_geog,Mogollon_final_Bark_mntd,Mogollon_final_Serotiny_mntd,Mogollon_final_Grass_mntd,Mogollon_final_Resprout_mntd,Mogollon_final_Pyrophillic_mntd,Mogollon_final_More_than_one_adaptation_mntd,Mogollon_final_Pyrophobic_mntd)

#write to csv

write.csv(Mogollon_final_mntd_combined, "Outfile_Mogollon_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West
Madre_West.matrix[2,] <- bigcsv$Madre_West

Madre_West.matrix

length(rownames(bigcsv))
Madre_West <- rownames(bigcsv)[bigcsv$Madre_West==1]

Madre_West

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Madre_West.tre <- keep.tip(conif.tree, Madre_West)
plotTree(Madre_West.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Madre_West.matrix, dis = cophenetic(Madre_West.tre))
Madre_West_geog_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(conif.tree))
Madre_West_geog_mpd<-Madre_West_geog_mpd[-1,]
Test_name<-("Madre_West_geog_mpd")
Madre_West_final_mpd_geog<-cbind(Test_name,Madre_West_geog_mpd)


mntd(samp = Madre_West.matrix, dis = cophenetic(Madre_West.tre))
Madre_West_geog_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(conif.tree))
Madre_West_geog_mntd<-Madre_West_geog_mntd[-1,]
Test_name<-("Madre_West_geog_mntd")
Madre_West_final_mntd_geog<-cbind(Test_name,Madre_West_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$Bark
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$Bark

Madre_West_Bark_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Bark_mpd<-Madre_West_Bark_mpd[-1,]
Test_name<-("Madre_West_Bark_mpd")
Madre_West_final_Bark_mpd<-cbind(Test_name,Madre_West_Bark_mpd)


Madre_West_Bark_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Bark_mntd<-Madre_West_Bark_mntd[-1,]
Test_name<-("Madre_West_Bark_mntd")
Madre_West_final_Bark_mntd<-cbind(Test_name,Madre_West_Bark_mntd)

#Serotiny


Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$Serotiny
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$Serotiny

Madre_West_Serotiny_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Serotiny_mpd<-Madre_West_Serotiny_mpd[-1,]
Test_name<-("Madre_West_Serotiny_mpd")
Madre_West_final_Serotiny_mpd<-cbind(Test_name,Madre_West_Serotiny_mpd)


Madre_West_Serotiny_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Serotiny_mntd<-Madre_West_Serotiny_mntd[-1,]
Test_name<-("Madre_West_Serotiny_mntd")
Madre_West_final_Serotiny_mntd<-cbind(Test_name,Madre_West_Serotiny_mntd)

#Grass

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$Grass
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$Grass

Madre_West_Grass_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Grass_mpd<-Madre_West_Grass_mpd[-1,]
Test_name<-("Madre_West_Grass_mpd")
Madre_West_final_Grass_mpd<-cbind(Test_name,Madre_West_Grass_mpd)

Madre_West_Grass_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Grass_mntd<-Madre_West_Grass_mntd[-1,]
Test_name<-("Madre_West_Grass_mntd")
Madre_West_final_Grass_mntd<-cbind(Test_name,Madre_West_Grass_mntd)


#Resprout

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$Resprout
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$Resprout

Madre_West_Resprout_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Resprout_mpd<-Madre_West_Resprout_mpd[-1,]
Test_name<-("Madre_West_Resprout_mpd")
Madre_West_final_Resprout_mpd<-cbind(Test_name,Madre_West_Resprout_mpd)


Madre_West_Resprout_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Resprout_mntd<-Madre_West_Resprout_mntd[-1,]
Test_name<-("Madre_West_Resprout_mntd")
Madre_West_final_Resprout_mntd<-cbind(Test_name,Madre_West_Resprout_mntd)

#More_than_one_adaptation

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$More_than_one_adaptation
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$More_than_one_adaptation

Madre_West_More_than_one_adaptation_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_More_than_one_adaptation_mpd<-Madre_West_More_than_one_adaptation_mpd[-1,]
Test_name<-("Madre_West_More_than_one_adaptation_mpd")
Madre_West_final_More_than_one_adaptation_mpd<-cbind(Test_name,Madre_West_More_than_one_adaptation_mpd)


Madre_West_More_than_one_adaptation_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_More_than_one_adaptation_mntd<-Madre_West_More_than_one_adaptation_mntd[-1,]
Test_name<-("Madre_West_More_than_one_adaptation_mntd")
Madre_West_final_More_than_one_adaptation_mntd<-cbind(Test_name,Madre_West_More_than_one_adaptation_mntd)

#Pyrophillic

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$Pyrophillic
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$Pyrophillic

Madre_West_Pyrophillic_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Pyrophillic_mpd<-Madre_West_Pyrophillic_mpd[-1,]
Test_name<-("Madre_West_Pyrophillic_mpd")
Madre_West_final_Pyrophillic_mpd<-cbind(Test_name,Madre_West_Pyrophillic_mpd)


Madre_West_Pyrophillic_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Pyrophillic_mntd<-Madre_West_Pyrophillic_mntd[-1,]
Test_name<-("Madre_West_Pyrophillic_mntd")
Madre_West_final_Pyrophillic_mntd<-cbind(Test_name,Madre_West_Pyrophillic_mntd)

#Pyrophobic

Madre_West.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_West.matrix) <- rownames(bigcsv)
Madre_West.matrix[1,] <- bigcsv$Madre_West & bigcsv$Pyrophobic
Madre_West.matrix[2,] <- bigcsv$Madre_West & bigcsv$Pyrophobic

Madre_West_Pyrophobic_mpd<-ses.mpd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Pyrophobic_mpd<-Madre_West_Pyrophobic_mpd[-1,]
Test_name<-("Madre_West_Pyrophobic_mpd")
Madre_West_final_Pyrophobic_mpd<-cbind(Test_name,Madre_West_Pyrophobic_mpd)


Madre_West_Pyrophobic_mntd<-ses.mntd(samp = Madre_West.matrix, dis =  cophenetic(Madre_West.tre))
Madre_West_Pyrophobic_mntd<-Madre_West_Pyrophobic_mntd[-1,]
Test_name<-("Madre_West_Pyrophobic_mntd")
Madre_West_final_Pyrophobic_mntd<-cbind(Test_name,Madre_West_Pyrophobic_mntd)




#get final csv for Madre_West mpd
Madre_West_final_mpd_combined<-rbind(Madre_West_final_mpd_geog,Madre_West_final_Bark_mpd,Madre_West_final_Serotiny_mpd,Madre_West_final_Grass_mpd,Madre_West_final_Resprout_mpd,Madre_West_final_Pyrophillic_mpd,Madre_West_final_More_than_one_adaptation_mpd,Madre_West_final_Pyrophobic_mpd)

#write to csv

write.csv(Madre_West_final_mpd_combined, "Outfile_Madre_West_final_mpd_combined.csv")

#get final csv for Madre_West mntd
Madre_West_final_mntd_combined<-rbind(Madre_West_final_mntd_geog,Madre_West_final_Bark_mntd,Madre_West_final_Serotiny_mntd,Madre_West_final_Grass_mntd,Madre_West_final_Resprout_mntd,Madre_West_final_Pyrophillic_mntd,Madre_West_final_More_than_one_adaptation_mntd,Madre_West_final_Pyrophobic_mntd)

#write to csv

write.csv(Madre_West_final_mntd_combined, "Outfile_Madre_West_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East
Madre_East.matrix[2,] <- bigcsv$Madre_East

Madre_East.matrix

length(rownames(bigcsv))
Madre_East <- rownames(bigcsv)[bigcsv$Madre_East==1]

Madre_East

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Madre_East.tre <- keep.tip(conif.tree, Madre_East)
plotTree(Madre_East.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Madre_East.matrix, dis = cophenetic(Madre_East.tre))
Madre_East_geog_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(conif.tree))
Madre_East_geog_mpd<-Madre_East_geog_mpd[-1,]
Test_name<-("Madre_East_geog_mpd")
Madre_East_final_mpd_geog<-cbind(Test_name,Madre_East_geog_mpd)


mntd(samp = Madre_East.matrix, dis = cophenetic(Madre_East.tre))
Madre_East_geog_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(conif.tree))
Madre_East_geog_mntd<-Madre_East_geog_mntd[-1,]
Test_name<-("Madre_East_geog_mntd")
Madre_East_final_mntd_geog<-cbind(Test_name,Madre_East_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$Bark
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$Bark

Madre_East_Bark_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Bark_mpd<-Madre_East_Bark_mpd[-1,]
Test_name<-("Madre_East_Bark_mpd")
Madre_East_final_Bark_mpd<-cbind(Test_name,Madre_East_Bark_mpd)


Madre_East_Bark_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Bark_mntd<-Madre_East_Bark_mntd[-1,]
Test_name<-("Madre_East_Bark_mntd")
Madre_East_final_Bark_mntd<-cbind(Test_name,Madre_East_Bark_mntd)

#Serotiny


Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$Serotiny
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$Serotiny

Madre_East_Serotiny_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Serotiny_mpd<-Madre_East_Serotiny_mpd[-1,]
Test_name<-("Madre_East_Serotiny_mpd")
Madre_East_final_Serotiny_mpd<-cbind(Test_name,Madre_East_Serotiny_mpd)


Madre_East_Serotiny_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Serotiny_mntd<-Madre_East_Serotiny_mntd[-1,]
Test_name<-("Madre_East_Serotiny_mntd")
Madre_East_final_Serotiny_mntd<-cbind(Test_name,Madre_East_Serotiny_mntd)

#Grass

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$Grass
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$Grass

Madre_East_Grass_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Grass_mpd<-Madre_East_Grass_mpd[-1,]
Test_name<-("Madre_East_Grass_mpd")
Madre_East_final_Grass_mpd<-cbind(Test_name,Madre_East_Grass_mpd)

Madre_East_Grass_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Grass_mntd<-Madre_East_Grass_mntd[-1,]
Test_name<-("Madre_East_Grass_mntd")
Madre_East_final_Grass_mntd<-cbind(Test_name,Madre_East_Grass_mntd)


#Resprout

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$Resprout
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$Resprout

Madre_East_Resprout_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Resprout_mpd<-Madre_East_Resprout_mpd[-1,]
Test_name<-("Madre_East_Resprout_mpd")
Madre_East_final_Resprout_mpd<-cbind(Test_name,Madre_East_Resprout_mpd)


Madre_East_Resprout_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Resprout_mntd<-Madre_East_Resprout_mntd[-1,]
Test_name<-("Madre_East_Resprout_mntd")
Madre_East_final_Resprout_mntd<-cbind(Test_name,Madre_East_Resprout_mntd)

#More_than_one_adaptation

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$More_than_one_adaptation
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$More_than_one_adaptation

Madre_East_More_than_one_adaptation_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_More_than_one_adaptation_mpd<-Madre_East_More_than_one_adaptation_mpd[-1,]
Test_name<-("Madre_East_More_than_one_adaptation_mpd")
Madre_East_final_More_than_one_adaptation_mpd<-cbind(Test_name,Madre_East_More_than_one_adaptation_mpd)


Madre_East_More_than_one_adaptation_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_More_than_one_adaptation_mntd<-Madre_East_More_than_one_adaptation_mntd[-1,]
Test_name<-("Madre_East_More_than_one_adaptation_mntd")
Madre_East_final_More_than_one_adaptation_mntd<-cbind(Test_name,Madre_East_More_than_one_adaptation_mntd)

#Pyrophillic

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$Pyrophillic
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$Pyrophillic

Madre_East_Pyrophillic_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Pyrophillic_mpd<-Madre_East_Pyrophillic_mpd[-1,]
Test_name<-("Madre_East_Pyrophillic_mpd")
Madre_East_final_Pyrophillic_mpd<-cbind(Test_name,Madre_East_Pyrophillic_mpd)


Madre_East_Pyrophillic_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Pyrophillic_mntd<-Madre_East_Pyrophillic_mntd[-1,]
Test_name<-("Madre_East_Pyrophillic_mntd")
Madre_East_final_Pyrophillic_mntd<-cbind(Test_name,Madre_East_Pyrophillic_mntd)

#Pyrophobic

Madre_East.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_East.matrix) <- rownames(bigcsv)
Madre_East.matrix[1,] <- bigcsv$Madre_East & bigcsv$Pyrophobic
Madre_East.matrix[2,] <- bigcsv$Madre_East & bigcsv$Pyrophobic

Madre_East_Pyrophobic_mpd<-ses.mpd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Pyrophobic_mpd<-Madre_East_Pyrophobic_mpd[-1,]
Test_name<-("Madre_East_Pyrophobic_mpd")
Madre_East_final_Pyrophobic_mpd<-cbind(Test_name,Madre_East_Pyrophobic_mpd)


Madre_East_Pyrophobic_mntd<-ses.mntd(samp = Madre_East.matrix, dis =  cophenetic(Madre_East.tre))
Madre_East_Pyrophobic_mntd<-Madre_East_Pyrophobic_mntd[-1,]
Test_name<-("Madre_East_Pyrophobic_mntd")
Madre_East_final_Pyrophobic_mntd<-cbind(Test_name,Madre_East_Pyrophobic_mntd)




#get final csv for Madre_East mpd
Madre_East_final_mpd_combined<-rbind(Madre_East_final_mpd_geog,Madre_East_final_Bark_mpd,Madre_East_final_Serotiny_mpd,Madre_East_final_Grass_mpd,Madre_East_final_Resprout_mpd,Madre_East_final_Pyrophillic_mpd,Madre_East_final_More_than_one_adaptation_mpd,Madre_East_final_Pyrophobic_mpd)

#write to csv

write.csv(Madre_East_final_mpd_combined, "Outfile_Madre_East_final_mpd_combined.csv")

#get final csv for Madre_East mntd
Madre_East_final_mntd_combined<-rbind(Madre_East_final_mntd_geog,Madre_East_final_Bark_mntd,Madre_East_final_Serotiny_mntd,Madre_East_final_Grass_mntd,Madre_East_final_Resprout_mntd,Madre_East_final_Pyrophillic_mntd,Madre_East_final_More_than_one_adaptation_mntd,Madre_East_final_Pyrophobic_mntd)

#write to csv

write.csv(Madre_East_final_mntd_combined, "Outfile_Madre_East_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol
Trans_vol.matrix[2,] <- bigcsv$Trans_vol

Trans_vol.matrix

length(rownames(bigcsv))
Trans_vol <- rownames(bigcsv)[bigcsv$Trans_vol==1]

Trans_vol

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Trans_vol.tre <- keep.tip(conif.tree, Trans_vol)
plotTree(Trans_vol.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Trans_vol.matrix, dis = cophenetic(Trans_vol.tre))
Trans_vol_geog_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(conif.tree))
Trans_vol_geog_mpd<-Trans_vol_geog_mpd[-1,]
Test_name<-("Trans_vol_geog_mpd")
Trans_vol_final_mpd_geog<-cbind(Test_name,Trans_vol_geog_mpd)


mntd(samp = Trans_vol.matrix, dis = cophenetic(Trans_vol.tre))
Trans_vol_geog_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(conif.tree))
Trans_vol_geog_mntd<-Trans_vol_geog_mntd[-1,]
Test_name<-("Trans_vol_geog_mntd")
Trans_vol_final_mntd_geog<-cbind(Test_name,Trans_vol_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$Bark
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$Bark

Trans_vol_Bark_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Bark_mpd<-Trans_vol_Bark_mpd[-1,]
Test_name<-("Trans_vol_Bark_mpd")
Trans_vol_final_Bark_mpd<-cbind(Test_name,Trans_vol_Bark_mpd)


Trans_vol_Bark_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Bark_mntd<-Trans_vol_Bark_mntd[-1,]
Test_name<-("Trans_vol_Bark_mntd")
Trans_vol_final_Bark_mntd<-cbind(Test_name,Trans_vol_Bark_mntd)

#Serotiny


Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$Serotiny
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$Serotiny

Trans_vol_Serotiny_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Serotiny_mpd<-Trans_vol_Serotiny_mpd[-1,]
Test_name<-("Trans_vol_Serotiny_mpd")
Trans_vol_final_Serotiny_mpd<-cbind(Test_name,Trans_vol_Serotiny_mpd)


Trans_vol_Serotiny_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Serotiny_mntd<-Trans_vol_Serotiny_mntd[-1,]
Test_name<-("Trans_vol_Serotiny_mntd")
Trans_vol_final_Serotiny_mntd<-cbind(Test_name,Trans_vol_Serotiny_mntd)

#Grass

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$Grass
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$Grass

Trans_vol_Grass_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Grass_mpd<-Trans_vol_Grass_mpd[-1,]
Test_name<-("Trans_vol_Grass_mpd")
Trans_vol_final_Grass_mpd<-cbind(Test_name,Trans_vol_Grass_mpd)

Trans_vol_Grass_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Grass_mntd<-Trans_vol_Grass_mntd[-1,]
Test_name<-("Trans_vol_Grass_mntd")
Trans_vol_final_Grass_mntd<-cbind(Test_name,Trans_vol_Grass_mntd)


#Resprout

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$Resprout
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$Resprout

Trans_vol_Resprout_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Resprout_mpd<-Trans_vol_Resprout_mpd[-1,]
Test_name<-("Trans_vol_Resprout_mpd")
Trans_vol_final_Resprout_mpd<-cbind(Test_name,Trans_vol_Resprout_mpd)


Trans_vol_Resprout_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Resprout_mntd<-Trans_vol_Resprout_mntd[-1,]
Test_name<-("Trans_vol_Resprout_mntd")
Trans_vol_final_Resprout_mntd<-cbind(Test_name,Trans_vol_Resprout_mntd)

#More_than_one_adaptation

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$More_than_one_adaptation
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$More_than_one_adaptation

Trans_vol_More_than_one_adaptation_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_More_than_one_adaptation_mpd<-Trans_vol_More_than_one_adaptation_mpd[-1,]
Test_name<-("Trans_vol_More_than_one_adaptation_mpd")
Trans_vol_final_More_than_one_adaptation_mpd<-cbind(Test_name,Trans_vol_More_than_one_adaptation_mpd)


Trans_vol_More_than_one_adaptation_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_More_than_one_adaptation_mntd<-Trans_vol_More_than_one_adaptation_mntd[-1,]
Test_name<-("Trans_vol_More_than_one_adaptation_mntd")
Trans_vol_final_More_than_one_adaptation_mntd<-cbind(Test_name,Trans_vol_More_than_one_adaptation_mntd)

#Pyrophillic

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$Pyrophillic
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$Pyrophillic

Trans_vol_Pyrophillic_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Pyrophillic_mpd<-Trans_vol_Pyrophillic_mpd[-1,]
Test_name<-("Trans_vol_Pyrophillic_mpd")
Trans_vol_final_Pyrophillic_mpd<-cbind(Test_name,Trans_vol_Pyrophillic_mpd)


Trans_vol_Pyrophillic_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Pyrophillic_mntd<-Trans_vol_Pyrophillic_mntd[-1,]
Test_name<-("Trans_vol_Pyrophillic_mntd")
Trans_vol_final_Pyrophillic_mntd<-cbind(Test_name,Trans_vol_Pyrophillic_mntd)

#Pyrophobic

Trans_vol.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Trans_vol.matrix) <- rownames(bigcsv)
Trans_vol.matrix[1,] <- bigcsv$Trans_vol & bigcsv$Pyrophobic
Trans_vol.matrix[2,] <- bigcsv$Trans_vol & bigcsv$Pyrophobic

Trans_vol_Pyrophobic_mpd<-ses.mpd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Pyrophobic_mpd<-Trans_vol_Pyrophobic_mpd[-1,]
Test_name<-("Trans_vol_Pyrophobic_mpd")
Trans_vol_final_Pyrophobic_mpd<-cbind(Test_name,Trans_vol_Pyrophobic_mpd)


Trans_vol_Pyrophobic_mntd<-ses.mntd(samp = Trans_vol.matrix, dis =  cophenetic(Trans_vol.tre))
Trans_vol_Pyrophobic_mntd<-Trans_vol_Pyrophobic_mntd[-1,]
Test_name<-("Trans_vol_Pyrophobic_mntd")
Trans_vol_final_Pyrophobic_mntd<-cbind(Test_name,Trans_vol_Pyrophobic_mntd)




#get final csv for Trans_vol mpd
Trans_vol_final_mpd_combined<-rbind(Trans_vol_final_mpd_geog,Trans_vol_final_Bark_mpd,Trans_vol_final_Serotiny_mpd,Trans_vol_final_Grass_mpd,Trans_vol_final_Resprout_mpd,Trans_vol_final_Pyrophillic_mpd,Trans_vol_final_More_than_one_adaptation_mpd,Trans_vol_final_Pyrophobic_mpd)

#write to csv

write.csv(Trans_vol_final_mpd_combined, "Outfile_Trans_vol_final_mpd_combined.csv")

#get final csv for Trans_vol mntd
Trans_vol_final_mntd_combined<-rbind(Trans_vol_final_mntd_geog,Trans_vol_final_Bark_mntd,Trans_vol_final_Serotiny_mntd,Trans_vol_final_Grass_mntd,Trans_vol_final_Resprout_mntd,Trans_vol_final_Pyrophillic_mntd,Trans_vol_final_More_than_one_adaptation_mntd,Trans_vol_final_Pyrophobic_mntd)

#write to csv

write.csv(Trans_vol_final_mntd_combined, "Outfile_Trans_vol_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new

Madre_South_new.matrix

length(rownames(bigcsv))
Madre_South_new <- rownames(bigcsv)[bigcsv$Madre_South_new==1]

Madre_South_new

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Madre_South_new.tre <- keep.tip(conif.tree, Madre_South_new)
plotTree(Madre_South_new.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Madre_South_new.matrix, dis = cophenetic(Madre_South_new.tre))
Madre_South_new_geog_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(conif.tree))
Madre_South_new_geog_mpd<-Madre_South_new_geog_mpd[-1,]
Test_name<-("Madre_South_new_geog_mpd")
Madre_South_new_final_mpd_geog<-cbind(Test_name,Madre_South_new_geog_mpd)


mntd(samp = Madre_South_new.matrix, dis = cophenetic(Madre_South_new.tre))
Madre_South_new_geog_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(conif.tree))
Madre_South_new_geog_mntd<-Madre_South_new_geog_mntd[-1,]
Test_name<-("Madre_South_new_geog_mntd")
Madre_South_new_final_mntd_geog<-cbind(Test_name,Madre_South_new_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$Bark
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$Bark

Madre_South_new_Bark_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Bark_mpd<-Madre_South_new_Bark_mpd[-1,]
Test_name<-("Madre_South_new_Bark_mpd")
Madre_South_new_final_Bark_mpd<-cbind(Test_name,Madre_South_new_Bark_mpd)


Madre_South_new_Bark_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Bark_mntd<-Madre_South_new_Bark_mntd[-1,]
Test_name<-("Madre_South_new_Bark_mntd")
Madre_South_new_final_Bark_mntd<-cbind(Test_name,Madre_South_new_Bark_mntd)

#Serotiny


Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$Serotiny
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$Serotiny

Madre_South_new_Serotiny_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Serotiny_mpd<-Madre_South_new_Serotiny_mpd[-1,]
Test_name<-("Madre_South_new_Serotiny_mpd")
Madre_South_new_final_Serotiny_mpd<-cbind(Test_name,Madre_South_new_Serotiny_mpd)


Madre_South_new_Serotiny_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Serotiny_mntd<-Madre_South_new_Serotiny_mntd[-1,]
Test_name<-("Madre_South_new_Serotiny_mntd")
Madre_South_new_final_Serotiny_mntd<-cbind(Test_name,Madre_South_new_Serotiny_mntd)

#Grass

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$Grass
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$Grass

Madre_South_new_Grass_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Grass_mpd<-Madre_South_new_Grass_mpd[-1,]
Test_name<-("Madre_South_new_Grass_mpd")
Madre_South_new_final_Grass_mpd<-cbind(Test_name,Madre_South_new_Grass_mpd)

Madre_South_new_Grass_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Grass_mntd<-Madre_South_new_Grass_mntd[-1,]
Test_name<-("Madre_South_new_Grass_mntd")
Madre_South_new_final_Grass_mntd<-cbind(Test_name,Madre_South_new_Grass_mntd)


#Resprout

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$Resprout
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$Resprout

Madre_South_new_Resprout_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Resprout_mpd<-Madre_South_new_Resprout_mpd[-1,]
Test_name<-("Madre_South_new_Resprout_mpd")
Madre_South_new_final_Resprout_mpd<-cbind(Test_name,Madre_South_new_Resprout_mpd)


Madre_South_new_Resprout_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Resprout_mntd<-Madre_South_new_Resprout_mntd[-1,]
Test_name<-("Madre_South_new_Resprout_mntd")
Madre_South_new_final_Resprout_mntd<-cbind(Test_name,Madre_South_new_Resprout_mntd)

#More_than_one_adaptation

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$More_than_one_adaptation
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$More_than_one_adaptation

Madre_South_new_More_than_one_adaptation_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_More_than_one_adaptation_mpd<-Madre_South_new_More_than_one_adaptation_mpd[-1,]
Test_name<-("Madre_South_new_More_than_one_adaptation_mpd")
Madre_South_new_final_More_than_one_adaptation_mpd<-cbind(Test_name,Madre_South_new_More_than_one_adaptation_mpd)


Madre_South_new_More_than_one_adaptation_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_More_than_one_adaptation_mntd<-Madre_South_new_More_than_one_adaptation_mntd[-1,]
Test_name<-("Madre_South_new_More_than_one_adaptation_mntd")
Madre_South_new_final_More_than_one_adaptation_mntd<-cbind(Test_name,Madre_South_new_More_than_one_adaptation_mntd)

#Pyrophillic

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$Pyrophillic
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$Pyrophillic

Madre_South_new_Pyrophillic_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Pyrophillic_mpd<-Madre_South_new_Pyrophillic_mpd[-1,]
Test_name<-("Madre_South_new_Pyrophillic_mpd")
Madre_South_new_final_Pyrophillic_mpd<-cbind(Test_name,Madre_South_new_Pyrophillic_mpd)


Madre_South_new_Pyrophillic_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Pyrophillic_mntd<-Madre_South_new_Pyrophillic_mntd[-1,]
Test_name<-("Madre_South_new_Pyrophillic_mntd")
Madre_South_new_final_Pyrophillic_mntd<-cbind(Test_name,Madre_South_new_Pyrophillic_mntd)

#Pyrophobic

Madre_South_new.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Madre_South_new.matrix) <- rownames(bigcsv)
Madre_South_new.matrix[1,] <- bigcsv$Madre_South_new & bigcsv$Pyrophobic
Madre_South_new.matrix[2,] <- bigcsv$Madre_South_new & bigcsv$Pyrophobic

Madre_South_new_Pyrophobic_mpd<-ses.mpd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Pyrophobic_mpd<-Madre_South_new_Pyrophobic_mpd[-1,]
Test_name<-("Madre_South_new_Pyrophobic_mpd")
Madre_South_new_final_Pyrophobic_mpd<-cbind(Test_name,Madre_South_new_Pyrophobic_mpd)


Madre_South_new_Pyrophobic_mntd<-ses.mntd(samp = Madre_South_new.matrix, dis =  cophenetic(Madre_South_new.tre))
Madre_South_new_Pyrophobic_mntd<-Madre_South_new_Pyrophobic_mntd[-1,]
Test_name<-("Madre_South_new_Pyrophobic_mntd")
Madre_South_new_final_Pyrophobic_mntd<-cbind(Test_name,Madre_South_new_Pyrophobic_mntd)




#get final csv for Madre_South_new mpd
Madre_South_new_final_mpd_combined<-rbind(Madre_South_new_final_mpd_geog,Madre_South_new_final_Bark_mpd,Madre_South_new_final_Serotiny_mpd,Madre_South_new_final_Grass_mpd,Madre_South_new_final_Resprout_mpd,Madre_South_new_final_Pyrophillic_mpd,Madre_South_new_final_More_than_one_adaptation_mpd,Madre_South_new_final_Pyrophobic_mpd)

#write to csv

write.csv(Madre_South_new_final_mpd_combined, "Outfile_Madre_South_new_final_mpd_combined.csv")

#get final csv for Madre_South_new mntd
Madre_South_new_final_mntd_combined<-rbind(Madre_South_new_final_mntd_geog,Madre_South_new_final_Bark_mntd,Madre_South_new_final_Serotiny_mntd,Madre_South_new_final_Grass_mntd,Madre_South_new_final_Resprout_mntd,Madre_South_new_final_Pyrophillic_mntd,Madre_South_new_final_More_than_one_adaptation_mntd,Madre_South_new_final_Pyrophobic_mntd)

#write to csv

write.csv(Madre_South_new_final_mntd_combined, "Outfile_Madre_South_new_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco

SW_Jalisco.matrix

length(rownames(bigcsv))
SW_Jalisco <- rownames(bigcsv)[bigcsv$SW_Jalisco==1]

SW_Jalisco

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

SW_Jalisco.tre <- keep.tip(conif.tree, SW_Jalisco)
plotTree(SW_Jalisco.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = SW_Jalisco.matrix, dis = cophenetic(SW_Jalisco.tre))
SW_Jalisco_geog_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(conif.tree))
SW_Jalisco_geog_mpd<-SW_Jalisco_geog_mpd[-1,]
Test_name<-("SW_Jalisco_geog_mpd")
SW_Jalisco_final_mpd_geog<-cbind(Test_name,SW_Jalisco_geog_mpd)


mntd(samp = SW_Jalisco.matrix, dis = cophenetic(SW_Jalisco.tre))
SW_Jalisco_geog_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(conif.tree))
SW_Jalisco_geog_mntd<-SW_Jalisco_geog_mntd[-1,]
Test_name<-("SW_Jalisco_geog_mntd")
SW_Jalisco_final_mntd_geog<-cbind(Test_name,SW_Jalisco_geog_mntd)



#for traits

#Bark

#Set up the new matrix

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$Bark
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$Bark

SW_Jalisco_Bark_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Bark_mpd<-SW_Jalisco_Bark_mpd[-1,]
Test_name<-("SW_Jalisco_Bark_mpd")
SW_Jalisco_final_Bark_mpd<-cbind(Test_name,SW_Jalisco_Bark_mpd)


SW_Jalisco_Bark_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Bark_mntd<-SW_Jalisco_Bark_mntd[-1,]
Test_name<-("SW_Jalisco_Bark_mntd")
SW_Jalisco_final_Bark_mntd<-cbind(Test_name,SW_Jalisco_Bark_mntd)

#Serotiny


SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$Serotiny
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$Serotiny

SW_Jalisco_Serotiny_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Serotiny_mpd<-SW_Jalisco_Serotiny_mpd[-1,]
Test_name<-("SW_Jalisco_Serotiny_mpd")
SW_Jalisco_final_Serotiny_mpd<-cbind(Test_name,SW_Jalisco_Serotiny_mpd)


SW_Jalisco_Serotiny_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Serotiny_mntd<-SW_Jalisco_Serotiny_mntd[-1,]
Test_name<-("SW_Jalisco_Serotiny_mntd")
SW_Jalisco_final_Serotiny_mntd<-cbind(Test_name,SW_Jalisco_Serotiny_mntd)

#Grass

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$Grass
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$Grass

SW_Jalisco_Grass_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Grass_mpd<-SW_Jalisco_Grass_mpd[-1,]
Test_name<-("SW_Jalisco_Grass_mpd")
SW_Jalisco_final_Grass_mpd<-cbind(Test_name,SW_Jalisco_Grass_mpd)

SW_Jalisco_Grass_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Grass_mntd<-SW_Jalisco_Grass_mntd[-1,]
Test_name<-("SW_Jalisco_Grass_mntd")
SW_Jalisco_final_Grass_mntd<-cbind(Test_name,SW_Jalisco_Grass_mntd)


#Resprout

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$Resprout
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$Resprout

SW_Jalisco_Resprout_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Resprout_mpd<-SW_Jalisco_Resprout_mpd[-1,]
Test_name<-("SW_Jalisco_Resprout_mpd")
SW_Jalisco_final_Resprout_mpd<-cbind(Test_name,SW_Jalisco_Resprout_mpd)


SW_Jalisco_Resprout_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Resprout_mntd<-SW_Jalisco_Resprout_mntd[-1,]
Test_name<-("SW_Jalisco_Resprout_mntd")
SW_Jalisco_final_Resprout_mntd<-cbind(Test_name,SW_Jalisco_Resprout_mntd)

#More_than_one_adaptation

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$More_than_one_adaptation
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$More_than_one_adaptation

SW_Jalisco_More_than_one_adaptation_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_More_than_one_adaptation_mpd<-SW_Jalisco_More_than_one_adaptation_mpd[-1,]
Test_name<-("SW_Jalisco_More_than_one_adaptation_mpd")
SW_Jalisco_final_More_than_one_adaptation_mpd<-cbind(Test_name,SW_Jalisco_More_than_one_adaptation_mpd)


SW_Jalisco_More_than_one_adaptation_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_More_than_one_adaptation_mntd<-SW_Jalisco_More_than_one_adaptation_mntd[-1,]
Test_name<-("SW_Jalisco_More_than_one_adaptation_mntd")
SW_Jalisco_final_More_than_one_adaptation_mntd<-cbind(Test_name,SW_Jalisco_More_than_one_adaptation_mntd)

#Pyrophillic

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$Pyrophillic
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$Pyrophillic

SW_Jalisco_Pyrophillic_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Pyrophillic_mpd<-SW_Jalisco_Pyrophillic_mpd[-1,]
Test_name<-("SW_Jalisco_Pyrophillic_mpd")
SW_Jalisco_final_Pyrophillic_mpd<-cbind(Test_name,SW_Jalisco_Pyrophillic_mpd)


SW_Jalisco_Pyrophillic_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Pyrophillic_mntd<-SW_Jalisco_Pyrophillic_mntd[-1,]
Test_name<-("SW_Jalisco_Pyrophillic_mntd")
SW_Jalisco_final_Pyrophillic_mntd<-cbind(Test_name,SW_Jalisco_Pyrophillic_mntd)

#Pyrophobic

SW_Jalisco.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(SW_Jalisco.matrix) <- rownames(bigcsv)
SW_Jalisco.matrix[1,] <- bigcsv$SW_Jalisco & bigcsv$Pyrophobic
SW_Jalisco.matrix[2,] <- bigcsv$SW_Jalisco & bigcsv$Pyrophobic

SW_Jalisco_Pyrophobic_mpd<-ses.mpd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Pyrophobic_mpd<-SW_Jalisco_Pyrophobic_mpd[-1,]
Test_name<-("SW_Jalisco_Pyrophobic_mpd")
SW_Jalisco_final_Pyrophobic_mpd<-cbind(Test_name,SW_Jalisco_Pyrophobic_mpd)


SW_Jalisco_Pyrophobic_mntd<-ses.mntd(samp = SW_Jalisco.matrix, dis =  cophenetic(SW_Jalisco.tre))
SW_Jalisco_Pyrophobic_mntd<-SW_Jalisco_Pyrophobic_mntd[-1,]
Test_name<-("SW_Jalisco_Pyrophobic_mntd")
SW_Jalisco_final_Pyrophobic_mntd<-cbind(Test_name,SW_Jalisco_Pyrophobic_mntd)




#get final csv for SW_Jalisco mpd
SW_Jalisco_final_mpd_combined<-rbind(SW_Jalisco_final_mpd_geog,SW_Jalisco_final_Bark_mpd,SW_Jalisco_final_Serotiny_mpd,SW_Jalisco_final_Grass_mpd,SW_Jalisco_final_Resprout_mpd,SW_Jalisco_final_Pyrophillic_mpd,SW_Jalisco_final_More_than_one_adaptation_mpd,SW_Jalisco_final_Pyrophobic_mpd)

#write to csv

write.csv(SW_Jalisco_final_mpd_combined, "Outfile_SW_Jalisco_final_mpd_combined_Sep_7_2024.csv")

#get final csv for SW_Jalisco mntd
SW_Jalisco_final_mntd_combined<-rbind(SW_Jalisco_final_mntd_geog,SW_Jalisco_final_Bark_mntd,SW_Jalisco_final_Serotiny_mntd,SW_Jalisco_final_Grass_mntd,SW_Jalisco_final_Resprout_mntd,SW_Jalisco_final_Pyrophillic_mntd,SW_Jalisco_final_More_than_one_adaptation_mntd,SW_Jalisco_final_Pyrophobic_mntd)

#write to csv

write.csv(SW_Jalisco_final_mntd_combined, "Outfile_SW_Jalisco_final_mntd_combined_Sep_7_2024.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas
Chiapas.matrix[2,] <- bigcsv$Chiapas

Chiapas.matrix

length(rownames(bigcsv))
Chiapas <- rownames(bigcsv)[bigcsv$Chiapas==1]

Chiapas

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Chiapas.tre <- keep.tip(conif.tree, Chiapas)
plotTree(Chiapas.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Chiapas.matrix, dis = cophenetic(Chiapas.tre))
Chiapas_geog_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(conif.tree))
Chiapas_geog_mpd<-Chiapas_geog_mpd[-1,]
Test_name<-("Chiapas_geog_mpd")
Chiapas_final_mpd_geog<-cbind(Test_name,Chiapas_geog_mpd)


mntd(samp = Chiapas.matrix, dis = cophenetic(Chiapas.tre))
Chiapas_geog_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(conif.tree))
Chiapas_geog_mntd<-Chiapas_geog_mntd[-1,]
Test_name<-("Chiapas_geog_mntd")
Chiapas_final_mntd_geog<-cbind(Test_name,Chiapas_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$Bark
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$Bark

Chiapas_Bark_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Bark_mpd<-Chiapas_Bark_mpd[-1,]
Test_name<-("Chiapas_Bark_mpd")
Chiapas_final_Bark_mpd<-cbind(Test_name,Chiapas_Bark_mpd)


Chiapas_Bark_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Bark_mntd<-Chiapas_Bark_mntd[-1,]
Test_name<-("Chiapas_Bark_mntd")
Chiapas_final_Bark_mntd<-cbind(Test_name,Chiapas_Bark_mntd)

#Serotiny


Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$Serotiny
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$Serotiny

Chiapas_Serotiny_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Serotiny_mpd<-Chiapas_Serotiny_mpd[-1,]
Test_name<-("Chiapas_Serotiny_mpd")
Chiapas_final_Serotiny_mpd<-cbind(Test_name,Chiapas_Serotiny_mpd)


Chiapas_Serotiny_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Serotiny_mntd<-Chiapas_Serotiny_mntd[-1,]
Test_name<-("Chiapas_Serotiny_mntd")
Chiapas_final_Serotiny_mntd<-cbind(Test_name,Chiapas_Serotiny_mntd)

#Grass

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$Grass
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$Grass

Chiapas_Grass_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Grass_mpd<-Chiapas_Grass_mpd[-1,]
Test_name<-("Chiapas_Grass_mpd")
Chiapas_final_Grass_mpd<-cbind(Test_name,Chiapas_Grass_mpd)

Chiapas_Grass_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Grass_mntd<-Chiapas_Grass_mntd[-1,]
Test_name<-("Chiapas_Grass_mntd")
Chiapas_final_Grass_mntd<-cbind(Test_name,Chiapas_Grass_mntd)


#Resprout

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$Resprout
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$Resprout

Chiapas_Resprout_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Resprout_mpd<-Chiapas_Resprout_mpd[-1,]
Test_name<-("Chiapas_Resprout_mpd")
Chiapas_final_Resprout_mpd<-cbind(Test_name,Chiapas_Resprout_mpd)


Chiapas_Resprout_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Resprout_mntd<-Chiapas_Resprout_mntd[-1,]
Test_name<-("Chiapas_Resprout_mntd")
Chiapas_final_Resprout_mntd<-cbind(Test_name,Chiapas_Resprout_mntd)

#More_than_one_adaptation

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$More_than_one_adaptation
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$More_than_one_adaptation

Chiapas_More_than_one_adaptation_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_More_than_one_adaptation_mpd<-Chiapas_More_than_one_adaptation_mpd[-1,]
Test_name<-("Chiapas_More_than_one_adaptation_mpd")
Chiapas_final_More_than_one_adaptation_mpd<-cbind(Test_name,Chiapas_More_than_one_adaptation_mpd)


Chiapas_More_than_one_adaptation_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_More_than_one_adaptation_mntd<-Chiapas_More_than_one_adaptation_mntd[-1,]
Test_name<-("Chiapas_More_than_one_adaptation_mntd")
Chiapas_final_More_than_one_adaptation_mntd<-cbind(Test_name,Chiapas_More_than_one_adaptation_mntd)

#Pyrophillic

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$Pyrophillic
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$Pyrophillic

Chiapas_Pyrophillic_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Pyrophillic_mpd<-Chiapas_Pyrophillic_mpd[-1,]
Test_name<-("Chiapas_Pyrophillic_mpd")
Chiapas_final_Pyrophillic_mpd<-cbind(Test_name,Chiapas_Pyrophillic_mpd)


Chiapas_Pyrophillic_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Pyrophillic_mntd<-Chiapas_Pyrophillic_mntd[-1,]
Test_name<-("Chiapas_Pyrophillic_mntd")
Chiapas_final_Pyrophillic_mntd<-cbind(Test_name,Chiapas_Pyrophillic_mntd)

#Pyrophobic

Chiapas.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Chiapas.matrix) <- rownames(bigcsv)
Chiapas.matrix[1,] <- bigcsv$Chiapas & bigcsv$Pyrophobic
Chiapas.matrix[2,] <- bigcsv$Chiapas & bigcsv$Pyrophobic

Chiapas_Pyrophobic_mpd<-ses.mpd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Pyrophobic_mpd<-Chiapas_Pyrophobic_mpd[-1,]
Test_name<-("Chiapas_Pyrophobic_mpd")
Chiapas_final_Pyrophobic_mpd<-cbind(Test_name,Chiapas_Pyrophobic_mpd)


Chiapas_Pyrophobic_mntd<-ses.mntd(samp = Chiapas.matrix, dis =  cophenetic(Chiapas.tre))
Chiapas_Pyrophobic_mntd<-Chiapas_Pyrophobic_mntd[-1,]
Test_name<-("Chiapas_Pyrophobic_mntd")
Chiapas_final_Pyrophobic_mntd<-cbind(Test_name,Chiapas_Pyrophobic_mntd)




#get final csv for Chiapas mpd
Chiapas_final_mpd_combined<-rbind(Chiapas_final_mpd_geog,Chiapas_final_Bark_mpd,Chiapas_final_Serotiny_mpd,Chiapas_final_Grass_mpd,Chiapas_final_Resprout_mpd,Chiapas_final_Pyrophillic_mpd,Chiapas_final_More_than_one_adaptation_mpd,Chiapas_final_Pyrophobic_mpd)

#write to csv

write.csv(Chiapas_final_mpd_combined, "Outfile_Chiapas_final_mpd_combined.csv")

#get final csv for Chiapas mntd
Chiapas_final_mntd_combined<-rbind(Chiapas_final_mntd_geog,Chiapas_final_Bark_mntd,Chiapas_final_Serotiny_mntd,Chiapas_final_Grass_mntd,Chiapas_final_Resprout_mntd,Chiapas_final_Pyrophillic_mntd,Chiapas_final_More_than_one_adaptation_mntd,Chiapas_final_Pyrophobic_mntd)

#write to csv

write.csv(Chiapas_final_mntd_combined, "Outfile_Chiapas_final_mntd_combined.csv")


#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only
Mexico_only.matrix[2,] <- bigcsv$Mexico_only

Mexico_only.matrix

length(rownames(bigcsv))
Mexico_only <- rownames(bigcsv)[bigcsv$Mexico_only==1]

Mexico_only

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Mexico_only.tre <- keep.tip(conif.tree, Mexico_only)
plotTree(Mexico_only.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Mexico_only.matrix, dis = cophenetic(Mexico_only.tre))
Mexico_only_geog_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(conif.tree))
Mexico_only_geog_mpd<-Mexico_only_geog_mpd[-1,]
Test_name<-("Mexico_only_geog_mpd")
Mexico_only_final_mpd_geog<-cbind(Test_name,Mexico_only_geog_mpd)


mntd(samp = Mexico_only.matrix, dis = cophenetic(Mexico_only.tre))
Mexico_only_geog_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(conif.tree))
Mexico_only_geog_mntd<-Mexico_only_geog_mntd[-1,]
Test_name<-("Mexico_only_geog_mntd")
Mexico_only_final_mntd_geog<-cbind(Test_name,Mexico_only_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$Bark
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$Bark

Mexico_only_Bark_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Bark_mpd<-Mexico_only_Bark_mpd[-1,]
Test_name<-("Mexico_only_Bark_mpd")
Mexico_only_final_Bark_mpd<-cbind(Test_name,Mexico_only_Bark_mpd)


Mexico_only_Bark_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Bark_mntd<-Mexico_only_Bark_mntd[-1,]
Test_name<-("Mexico_only_Bark_mntd")
Mexico_only_final_Bark_mntd<-cbind(Test_name,Mexico_only_Bark_mntd)

#Serotiny


Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$Serotiny
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$Serotiny

Mexico_only_Serotiny_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Serotiny_mpd<-Mexico_only_Serotiny_mpd[-1,]
Test_name<-("Mexico_only_Serotiny_mpd")
Mexico_only_final_Serotiny_mpd<-cbind(Test_name,Mexico_only_Serotiny_mpd)


Mexico_only_Serotiny_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Serotiny_mntd<-Mexico_only_Serotiny_mntd[-1,]
Test_name<-("Mexico_only_Serotiny_mntd")
Mexico_only_final_Serotiny_mntd<-cbind(Test_name,Mexico_only_Serotiny_mntd)

#Grass

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$Grass
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$Grass

Mexico_only_Grass_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Grass_mpd<-Mexico_only_Grass_mpd[-1,]
Test_name<-("Mexico_only_Grass_mpd")
Mexico_only_final_Grass_mpd<-cbind(Test_name,Mexico_only_Grass_mpd)

Mexico_only_Grass_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Grass_mntd<-Mexico_only_Grass_mntd[-1,]
Test_name<-("Mexico_only_Grass_mntd")
Mexico_only_final_Grass_mntd<-cbind(Test_name,Mexico_only_Grass_mntd)


#Resprout

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$Resprout
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$Resprout

Mexico_only_Resprout_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Resprout_mpd<-Mexico_only_Resprout_mpd[-1,]
Test_name<-("Mexico_only_Resprout_mpd")
Mexico_only_final_Resprout_mpd<-cbind(Test_name,Mexico_only_Resprout_mpd)


Mexico_only_Resprout_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Resprout_mntd<-Mexico_only_Resprout_mntd[-1,]
Test_name<-("Mexico_only_Resprout_mntd")
Mexico_only_final_Resprout_mntd<-cbind(Test_name,Mexico_only_Resprout_mntd)

#More_than_one_adaptation

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$More_than_one_adaptation
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$More_than_one_adaptation

Mexico_only_More_than_one_adaptation_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_More_than_one_adaptation_mpd<-Mexico_only_More_than_one_adaptation_mpd[-1,]
Test_name<-("Mexico_only_More_than_one_adaptation_mpd")
Mexico_only_final_More_than_one_adaptation_mpd<-cbind(Test_name,Mexico_only_More_than_one_adaptation_mpd)


Mexico_only_More_than_one_adaptation_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_More_than_one_adaptation_mntd<-Mexico_only_More_than_one_adaptation_mntd[-1,]
Test_name<-("Mexico_only_More_than_one_adaptation_mntd")
Mexico_only_final_More_than_one_adaptation_mntd<-cbind(Test_name,Mexico_only_More_than_one_adaptation_mntd)

#Pyrophillic

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$Pyrophillic
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$Pyrophillic

Mexico_only_Pyrophillic_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Pyrophillic_mpd<-Mexico_only_Pyrophillic_mpd[-1,]
Test_name<-("Mexico_only_Pyrophillic_mpd")
Mexico_only_final_Pyrophillic_mpd<-cbind(Test_name,Mexico_only_Pyrophillic_mpd)


Mexico_only_Pyrophillic_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Pyrophillic_mntd<-Mexico_only_Pyrophillic_mntd[-1,]
Test_name<-("Mexico_only_Pyrophillic_mntd")
Mexico_only_final_Pyrophillic_mntd<-cbind(Test_name,Mexico_only_Pyrophillic_mntd)

#Pyrophobic

Mexico_only.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_only.matrix) <- rownames(bigcsv)
Mexico_only.matrix[1,] <- bigcsv$Mexico_only & bigcsv$Pyrophobic
Mexico_only.matrix[2,] <- bigcsv$Mexico_only & bigcsv$Pyrophobic

Mexico_only_Pyrophobic_mpd<-ses.mpd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Pyrophobic_mpd<-Mexico_only_Pyrophobic_mpd[-1,]
Test_name<-("Mexico_only_Pyrophobic_mpd")
Mexico_only_final_Pyrophobic_mpd<-cbind(Test_name,Mexico_only_Pyrophobic_mpd)


Mexico_only_Pyrophobic_mntd<-ses.mntd(samp = Mexico_only.matrix, dis =  cophenetic(Mexico_only.tre))
Mexico_only_Pyrophobic_mntd<-Mexico_only_Pyrophobic_mntd[-1,]
Test_name<-("Mexico_only_Pyrophobic_mntd")
Mexico_only_final_Pyrophobic_mntd<-cbind(Test_name,Mexico_only_Pyrophobic_mntd)




#get final csv for Mexico_only mpd
Mexico_only_final_mpd_combined<-rbind(Mexico_only_final_mpd_geog,Mexico_only_final_Bark_mpd,Mexico_only_final_Serotiny_mpd,Mexico_only_final_Grass_mpd,Mexico_only_final_Resprout_mpd,Mexico_only_final_Pyrophillic_mpd,Mexico_only_final_More_than_one_adaptation_mpd,Mexico_only_final_Pyrophobic_mpd)

#write to csv

write.csv(Mexico_only_final_mpd_combined, "Outfile_Mexico_only_final_mpd_combined.csv")

#get final csv for Mexico_only mntd
Mexico_only_final_mntd_combined<-rbind(Mexico_only_final_mntd_geog,Mexico_only_final_Bark_mntd,Mexico_only_final_Serotiny_mntd,Mexico_only_final_Grass_mntd,Mexico_only_final_Resprout_mntd,Mexico_only_final_Pyrophillic_mntd,Mexico_only_final_More_than_one_adaptation_mntd,Mexico_only_final_Pyrophobic_mntd)

#write to csv

write.csv(Mexico_only_final_mntd_combined, "Outfile_Mexico_only_final_mntd_combined.csv")














#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog

Mexico_with_Mog.matrix

length(rownames(bigcsv))
Mexico_with_Mog <- rownames(bigcsv)[bigcsv$Mexico_with_Mog==1]

Mexico_with_Mog

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Mexico_with_Mog.tre <- keep.tip(conif.tree, Mexico_with_Mog)
plotTree(Mexico_with_Mog.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Mexico_with_Mog.matrix, dis = cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_geog_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(conif.tree))
Mexico_with_Mog_geog_mpd<-Mexico_with_Mog_geog_mpd[-1,]
Test_name<-("Mexico_with_Mog_geog_mpd")
Mexico_with_Mog_final_mpd_geog<-cbind(Test_name,Mexico_with_Mog_geog_mpd)


mntd(samp = Mexico_with_Mog.matrix, dis = cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_geog_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(conif.tree))
Mexico_with_Mog_geog_mntd<-Mexico_with_Mog_geog_mntd[-1,]
Test_name<-("Mexico_with_Mog_geog_mntd")
Mexico_with_Mog_final_mntd_geog<-cbind(Test_name,Mexico_with_Mog_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$Bark
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$Bark

Mexico_with_Mog_Bark_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Bark_mpd<-Mexico_with_Mog_Bark_mpd[-1,]
Test_name<-("Mexico_with_Mog_Bark_mpd")
Mexico_with_Mog_final_Bark_mpd<-cbind(Test_name,Mexico_with_Mog_Bark_mpd)


Mexico_with_Mog_Bark_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Bark_mntd<-Mexico_with_Mog_Bark_mntd[-1,]
Test_name<-("Mexico_with_Mog_Bark_mntd")
Mexico_with_Mog_final_Bark_mntd<-cbind(Test_name,Mexico_with_Mog_Bark_mntd)

#Serotiny


Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$Serotiny
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$Serotiny

Mexico_with_Mog_Serotiny_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Serotiny_mpd<-Mexico_with_Mog_Serotiny_mpd[-1,]
Test_name<-("Mexico_with_Mog_Serotiny_mpd")
Mexico_with_Mog_final_Serotiny_mpd<-cbind(Test_name,Mexico_with_Mog_Serotiny_mpd)


Mexico_with_Mog_Serotiny_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Serotiny_mntd<-Mexico_with_Mog_Serotiny_mntd[-1,]
Test_name<-("Mexico_with_Mog_Serotiny_mntd")
Mexico_with_Mog_final_Serotiny_mntd<-cbind(Test_name,Mexico_with_Mog_Serotiny_mntd)

#Grass

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$Grass
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$Grass

Mexico_with_Mog_Grass_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Grass_mpd<-Mexico_with_Mog_Grass_mpd[-1,]
Test_name<-("Mexico_with_Mog_Grass_mpd")
Mexico_with_Mog_final_Grass_mpd<-cbind(Test_name,Mexico_with_Mog_Grass_mpd)

Mexico_with_Mog_Grass_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Grass_mntd<-Mexico_with_Mog_Grass_mntd[-1,]
Test_name<-("Mexico_with_Mog_Grass_mntd")
Mexico_with_Mog_final_Grass_mntd<-cbind(Test_name,Mexico_with_Mog_Grass_mntd)


#Resprout

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$Resprout
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$Resprout

Mexico_with_Mog_Resprout_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Resprout_mpd<-Mexico_with_Mog_Resprout_mpd[-1,]
Test_name<-("Mexico_with_Mog_Resprout_mpd")
Mexico_with_Mog_final_Resprout_mpd<-cbind(Test_name,Mexico_with_Mog_Resprout_mpd)


Mexico_with_Mog_Resprout_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Resprout_mntd<-Mexico_with_Mog_Resprout_mntd[-1,]
Test_name<-("Mexico_with_Mog_Resprout_mntd")
Mexico_with_Mog_final_Resprout_mntd<-cbind(Test_name,Mexico_with_Mog_Resprout_mntd)

#More_than_one_adaptation

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$More_than_one_adaptation
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$More_than_one_adaptation

Mexico_with_Mog_More_than_one_adaptation_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_More_than_one_adaptation_mpd<-Mexico_with_Mog_More_than_one_adaptation_mpd[-1,]
Test_name<-("Mexico_with_Mog_More_than_one_adaptation_mpd")
Mexico_with_Mog_final_More_than_one_adaptation_mpd<-cbind(Test_name,Mexico_with_Mog_More_than_one_adaptation_mpd)


Mexico_with_Mog_More_than_one_adaptation_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_More_than_one_adaptation_mntd<-Mexico_with_Mog_More_than_one_adaptation_mntd[-1,]
Test_name<-("Mexico_with_Mog_More_than_one_adaptation_mntd")
Mexico_with_Mog_final_More_than_one_adaptation_mntd<-cbind(Test_name,Mexico_with_Mog_More_than_one_adaptation_mntd)

#Pyrophillic

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$Pyrophillic
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$Pyrophillic

Mexico_with_Mog_Pyrophillic_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Pyrophillic_mpd<-Mexico_with_Mog_Pyrophillic_mpd[-1,]
Test_name<-("Mexico_with_Mog_Pyrophillic_mpd")
Mexico_with_Mog_final_Pyrophillic_mpd<-cbind(Test_name,Mexico_with_Mog_Pyrophillic_mpd)


Mexico_with_Mog_Pyrophillic_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Pyrophillic_mntd<-Mexico_with_Mog_Pyrophillic_mntd[-1,]
Test_name<-("Mexico_with_Mog_Pyrophillic_mntd")
Mexico_with_Mog_final_Pyrophillic_mntd<-cbind(Test_name,Mexico_with_Mog_Pyrophillic_mntd)

#Pyrophobic

Mexico_with_Mog.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Mexico_with_Mog.matrix) <- rownames(bigcsv)
Mexico_with_Mog.matrix[1,] <- bigcsv$Mexico_with_Mog & bigcsv$Pyrophobic
Mexico_with_Mog.matrix[2,] <- bigcsv$Mexico_with_Mog & bigcsv$Pyrophobic

Mexico_with_Mog_Pyrophobic_mpd<-ses.mpd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Pyrophobic_mpd<-Mexico_with_Mog_Pyrophobic_mpd[-1,]
Test_name<-("Mexico_with_Mog_Pyrophobic_mpd")
Mexico_with_Mog_final_Pyrophobic_mpd<-cbind(Test_name,Mexico_with_Mog_Pyrophobic_mpd)


Mexico_with_Mog_Pyrophobic_mntd<-ses.mntd(samp = Mexico_with_Mog.matrix, dis =  cophenetic(Mexico_with_Mog.tre))
Mexico_with_Mog_Pyrophobic_mntd<-Mexico_with_Mog_Pyrophobic_mntd[-1,]
Test_name<-("Mexico_with_Mog_Pyrophobic_mntd")
Mexico_with_Mog_final_Pyrophobic_mntd<-cbind(Test_name,Mexico_with_Mog_Pyrophobic_mntd)




#get final csv for Mexico_with_Mog mpd
Mexico_with_Mog_final_mpd_combined<-rbind(Mexico_with_Mog_final_mpd_geog,Mexico_with_Mog_final_Bark_mpd,Mexico_with_Mog_final_Serotiny_mpd,Mexico_with_Mog_final_Grass_mpd,Mexico_with_Mog_final_Resprout_mpd,Mexico_with_Mog_final_Pyrophillic_mpd,Mexico_with_Mog_final_More_than_one_adaptation_mpd,Mexico_with_Mog_final_Pyrophobic_mpd)

#write to csv

write.csv(Mexico_with_Mog_final_mpd_combined, "Outfile_Mexico_with_Mog_final_mpd_combined.csv")

#get final csv for Mexico_with_Mog mntd
Mexico_with_Mog_final_mntd_combined<-rbind(Mexico_with_Mog_final_mntd_geog,Mexico_with_Mog_final_Bark_mntd,Mexico_with_Mog_final_Serotiny_mntd,Mexico_with_Mog_final_Grass_mntd,Mexico_with_Mog_final_Resprout_mntd,Mexico_with_Mog_final_Pyrophillic_mntd,Mexico_with_Mog_final_More_than_one_adaptation_mntd,Mexico_with_Mog_final_Pyrophobic_mntd)

#write to csv

write.csv(Mexico_with_Mog_final_mntd_combined, "Outfile_Mexico_with_Mog_final_mntd_combined.csv")




#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf
Gulf.matrix[2,] <- bigcsv$Gulf

Gulf.matrix

length(rownames(bigcsv))
Gulf <- rownames(bigcsv)[bigcsv$Gulf==1]

Gulf

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Gulf.tre <- keep.tip(conif.tree, Gulf)
plotTree(Gulf.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Gulf.matrix, dis = cophenetic(Gulf.tre))
Gulf_geog_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(conif.tree))
Gulf_geog_mpd<-Gulf_geog_mpd[-1,]
Test_name<-("Gulf_geog_mpd")
Gulf_final_mpd_geog<-cbind(Test_name,Gulf_geog_mpd)


mntd(samp = Gulf.matrix, dis = cophenetic(Gulf.tre))
Gulf_geog_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(conif.tree))
Gulf_geog_mntd<-Gulf_geog_mntd[-1,]
Test_name<-("Gulf_geog_mntd")
Gulf_final_mntd_geog<-cbind(Test_name,Gulf_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$Bark
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$Bark

Gulf_Bark_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Bark_mpd<-Gulf_Bark_mpd[-1,]
Test_name<-("Gulf_Bark_mpd")
Gulf_final_Bark_mpd<-cbind(Test_name,Gulf_Bark_mpd)


Gulf_Bark_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Bark_mntd<-Gulf_Bark_mntd[-1,]
Test_name<-("Gulf_Bark_mntd")
Gulf_final_Bark_mntd<-cbind(Test_name,Gulf_Bark_mntd)

#Serotiny


Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$Serotiny
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$Serotiny

Gulf_Serotiny_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Serotiny_mpd<-Gulf_Serotiny_mpd[-1,]
Test_name<-("Gulf_Serotiny_mpd")
Gulf_final_Serotiny_mpd<-cbind(Test_name,Gulf_Serotiny_mpd)


Gulf_Serotiny_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Serotiny_mntd<-Gulf_Serotiny_mntd[-1,]
Test_name<-("Gulf_Serotiny_mntd")
Gulf_final_Serotiny_mntd<-cbind(Test_name,Gulf_Serotiny_mntd)

#Grass

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$Grass
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$Grass

Gulf_Grass_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Grass_mpd<-Gulf_Grass_mpd[-1,]
Test_name<-("Gulf_Grass_mpd")
Gulf_final_Grass_mpd<-cbind(Test_name,Gulf_Grass_mpd)

Gulf_Grass_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Grass_mntd<-Gulf_Grass_mntd[-1,]
Test_name<-("Gulf_Grass_mntd")
Gulf_final_Grass_mntd<-cbind(Test_name,Gulf_Grass_mntd)


#Resprout

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$Resprout
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$Resprout

Gulf_Resprout_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Resprout_mpd<-Gulf_Resprout_mpd[-1,]
Test_name<-("Gulf_Resprout_mpd")
Gulf_final_Resprout_mpd<-cbind(Test_name,Gulf_Resprout_mpd)


Gulf_Resprout_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Resprout_mntd<-Gulf_Resprout_mntd[-1,]
Test_name<-("Gulf_Resprout_mntd")
Gulf_final_Resprout_mntd<-cbind(Test_name,Gulf_Resprout_mntd)

#More_than_one_adaptation

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$More_than_one_adaptation
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$More_than_one_adaptation

Gulf_More_than_one_adaptation_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_More_than_one_adaptation_mpd<-Gulf_More_than_one_adaptation_mpd[-1,]
Test_name<-("Gulf_More_than_one_adaptation_mpd")
Gulf_final_More_than_one_adaptation_mpd<-cbind(Test_name,Gulf_More_than_one_adaptation_mpd)


Gulf_More_than_one_adaptation_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_More_than_one_adaptation_mntd<-Gulf_More_than_one_adaptation_mntd[-1,]
Test_name<-("Gulf_More_than_one_adaptation_mntd")
Gulf_final_More_than_one_adaptation_mntd<-cbind(Test_name,Gulf_More_than_one_adaptation_mntd)

#Pyrophillic

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$Pyrophillic
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$Pyrophillic

Gulf_Pyrophillic_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Pyrophillic_mpd<-Gulf_Pyrophillic_mpd[-1,]
Test_name<-("Gulf_Pyrophillic_mpd")
Gulf_final_Pyrophillic_mpd<-cbind(Test_name,Gulf_Pyrophillic_mpd)


Gulf_Pyrophillic_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Pyrophillic_mntd<-Gulf_Pyrophillic_mntd[-1,]
Test_name<-("Gulf_Pyrophillic_mntd")
Gulf_final_Pyrophillic_mntd<-cbind(Test_name,Gulf_Pyrophillic_mntd)

#Pyrophobic

Gulf.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Gulf.matrix) <- rownames(bigcsv)
Gulf.matrix[1,] <- bigcsv$Gulf & bigcsv$Pyrophobic
Gulf.matrix[2,] <- bigcsv$Gulf & bigcsv$Pyrophobic

Gulf_Pyrophobic_mpd<-ses.mpd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Pyrophobic_mpd<-Gulf_Pyrophobic_mpd[-1,]
Test_name<-("Gulf_Pyrophobic_mpd")
Gulf_final_Pyrophobic_mpd<-cbind(Test_name,Gulf_Pyrophobic_mpd)


Gulf_Pyrophobic_mntd<-ses.mntd(samp = Gulf.matrix, dis =  cophenetic(Gulf.tre))
Gulf_Pyrophobic_mntd<-Gulf_Pyrophobic_mntd[-1,]
Test_name<-("Gulf_Pyrophobic_mntd")
Gulf_final_Pyrophobic_mntd<-cbind(Test_name,Gulf_Pyrophobic_mntd)




#get final csv for Gulf mpd
Gulf_final_mpd_combined<-rbind(Gulf_final_mpd_geog,Gulf_final_Bark_mpd,Gulf_final_Serotiny_mpd,Gulf_final_Grass_mpd,Gulf_final_Resprout_mpd,Gulf_final_Pyrophillic_mpd,Gulf_final_More_than_one_adaptation_mpd,Gulf_final_Pyrophobic_mpd)

#write to csv

write.csv(Gulf_final_mpd_combined, "Outfile_Gulf_final_mpd_combined.csv")

#get final csv for Gulf mntd
Gulf_final_mntd_combined<-rbind(Gulf_final_mntd_geog,Gulf_final_Bark_mntd,Gulf_final_Serotiny_mntd,Gulf_final_Grass_mntd,Gulf_final_Resprout_mntd,Gulf_final_Pyrophillic_mntd,Gulf_final_More_than_one_adaptation_mntd,Gulf_final_Pyrophobic_mntd)

#write to csv

write.csv(Gulf_final_mntd_combined, "Outfile_Gulf_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia
Appalachia.matrix[2,] <- bigcsv$Appalachia

Appalachia.matrix

length(rownames(bigcsv))
Appalachia <- rownames(bigcsv)[bigcsv$Appalachia==1]

Appalachia

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Appalachia.tre <- keep.tip(conif.tree, Appalachia)
plotTree(Appalachia.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Appalachia.matrix, dis = cophenetic(Appalachia.tre))
Appalachia_geog_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(conif.tree))
Appalachia_geog_mpd<-Appalachia_geog_mpd[-1,]
Test_name<-("Appalachia_geog_mpd")
Appalachia_final_mpd_geog<-cbind(Test_name,Appalachia_geog_mpd)


mntd(samp = Appalachia.matrix, dis = cophenetic(Appalachia.tre))
Appalachia_geog_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(conif.tree))
Appalachia_geog_mntd<-Appalachia_geog_mntd[-1,]
Test_name<-("Appalachia_geog_mntd")
Appalachia_final_mntd_geog<-cbind(Test_name,Appalachia_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$Bark
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$Bark

Appalachia_Bark_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Bark_mpd<-Appalachia_Bark_mpd[-1,]
Test_name<-("Appalachia_Bark_mpd")
Appalachia_final_Bark_mpd<-cbind(Test_name,Appalachia_Bark_mpd)


Appalachia_Bark_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Bark_mntd<-Appalachia_Bark_mntd[-1,]
Test_name<-("Appalachia_Bark_mntd")
Appalachia_final_Bark_mntd<-cbind(Test_name,Appalachia_Bark_mntd)

#Serotiny


Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$Serotiny
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$Serotiny

Appalachia_Serotiny_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Serotiny_mpd<-Appalachia_Serotiny_mpd[-1,]
Test_name<-("Appalachia_Serotiny_mpd")
Appalachia_final_Serotiny_mpd<-cbind(Test_name,Appalachia_Serotiny_mpd)


Appalachia_Serotiny_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Serotiny_mntd<-Appalachia_Serotiny_mntd[-1,]
Test_name<-("Appalachia_Serotiny_mntd")
Appalachia_final_Serotiny_mntd<-cbind(Test_name,Appalachia_Serotiny_mntd)

#Grass

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$Grass
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$Grass

Appalachia_Grass_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Grass_mpd<-Appalachia_Grass_mpd[-1,]
Test_name<-("Appalachia_Grass_mpd")
Appalachia_final_Grass_mpd<-cbind(Test_name,Appalachia_Grass_mpd)

Appalachia_Grass_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Grass_mntd<-Appalachia_Grass_mntd[-1,]
Test_name<-("Appalachia_Grass_mntd")
Appalachia_final_Grass_mntd<-cbind(Test_name,Appalachia_Grass_mntd)


#Resprout

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$Resprout
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$Resprout

Appalachia_Resprout_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Resprout_mpd<-Appalachia_Resprout_mpd[-1,]
Test_name<-("Appalachia_Resprout_mpd")
Appalachia_final_Resprout_mpd<-cbind(Test_name,Appalachia_Resprout_mpd)


Appalachia_Resprout_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Resprout_mntd<-Appalachia_Resprout_mntd[-1,]
Test_name<-("Appalachia_Resprout_mntd")
Appalachia_final_Resprout_mntd<-cbind(Test_name,Appalachia_Resprout_mntd)

#More_than_one_adaptation

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$More_than_one_adaptation
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$More_than_one_adaptation

Appalachia_More_than_one_adaptation_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_More_than_one_adaptation_mpd<-Appalachia_More_than_one_adaptation_mpd[-1,]
Test_name<-("Appalachia_More_than_one_adaptation_mpd")
Appalachia_final_More_than_one_adaptation_mpd<-cbind(Test_name,Appalachia_More_than_one_adaptation_mpd)


Appalachia_More_than_one_adaptation_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_More_than_one_adaptation_mntd<-Appalachia_More_than_one_adaptation_mntd[-1,]
Test_name<-("Appalachia_More_than_one_adaptation_mntd")
Appalachia_final_More_than_one_adaptation_mntd<-cbind(Test_name,Appalachia_More_than_one_adaptation_mntd)

#Pyrophillic

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$Pyrophillic
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$Pyrophillic

Appalachia_Pyrophillic_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Pyrophillic_mpd<-Appalachia_Pyrophillic_mpd[-1,]
Test_name<-("Appalachia_Pyrophillic_mpd")
Appalachia_final_Pyrophillic_mpd<-cbind(Test_name,Appalachia_Pyrophillic_mpd)


Appalachia_Pyrophillic_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Pyrophillic_mntd<-Appalachia_Pyrophillic_mntd[-1,]
Test_name<-("Appalachia_Pyrophillic_mntd")
Appalachia_final_Pyrophillic_mntd<-cbind(Test_name,Appalachia_Pyrophillic_mntd)

#Pyrophobic

Appalachia.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Appalachia.matrix) <- rownames(bigcsv)
Appalachia.matrix[1,] <- bigcsv$Appalachia & bigcsv$Pyrophobic
Appalachia.matrix[2,] <- bigcsv$Appalachia & bigcsv$Pyrophobic

Appalachia_Pyrophobic_mpd<-ses.mpd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Pyrophobic_mpd<-Appalachia_Pyrophobic_mpd[-1,]
Test_name<-("Appalachia_Pyrophobic_mpd")
Appalachia_final_Pyrophobic_mpd<-cbind(Test_name,Appalachia_Pyrophobic_mpd)


Appalachia_Pyrophobic_mntd<-ses.mntd(samp = Appalachia.matrix, dis =  cophenetic(Appalachia.tre))
Appalachia_Pyrophobic_mntd<-Appalachia_Pyrophobic_mntd[-1,]
Test_name<-("Appalachia_Pyrophobic_mntd")
Appalachia_final_Pyrophobic_mntd<-cbind(Test_name,Appalachia_Pyrophobic_mntd)




#get final csv for Appalachia mpd
Appalachia_final_mpd_combined<-rbind(Appalachia_final_mpd_geog,Appalachia_final_Bark_mpd,Appalachia_final_Serotiny_mpd,Appalachia_final_Grass_mpd,Appalachia_final_Resprout_mpd,Appalachia_final_Pyrophillic_mpd,Appalachia_final_More_than_one_adaptation_mpd,Appalachia_final_Pyrophobic_mpd)

#write to csv

write.csv(Appalachia_final_mpd_combined, "Outfile_Appalachia_final_mpd_combined.csv")

#get final csv for Appalachia mntd
Appalachia_final_mntd_combined<-rbind(Appalachia_final_mntd_geog,Appalachia_final_Bark_mntd,Appalachia_final_Serotiny_mntd,Appalachia_final_Grass_mntd,Appalachia_final_Resprout_mntd,Appalachia_final_Pyrophillic_mntd,Appalachia_final_More_than_one_adaptation_mntd,Appalachia_final_Pyrophobic_mntd)

#write to csv

write.csv(Appalachia_final_mntd_combined, "Outfile_Appalachia_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian
Laurentian.matrix[2,] <- bigcsv$Laurentian

Laurentian.matrix

length(rownames(bigcsv))
Laurentian <- rownames(bigcsv)[bigcsv$Laurentian==1]

Laurentian

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Laurentian.tre <- keep.tip(conif.tree, Laurentian)
plotTree(Laurentian.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Laurentian.matrix, dis = cophenetic(Laurentian.tre))
Laurentian_geog_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(conif.tree))
Laurentian_geog_mpd<-Laurentian_geog_mpd[-1,]
Test_name<-("Laurentian_geog_mpd")
Laurentian_final_mpd_geog<-cbind(Test_name,Laurentian_geog_mpd)


mntd(samp = Laurentian.matrix, dis = cophenetic(Laurentian.tre))
Laurentian_geog_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(conif.tree))
Laurentian_geog_mntd<-Laurentian_geog_mntd[-1,]
Test_name<-("Laurentian_geog_mntd")
Laurentian_final_mntd_geog<-cbind(Test_name,Laurentian_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$Bark
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$Bark

Laurentian_Bark_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Bark_mpd<-Laurentian_Bark_mpd[-1,]
Test_name<-("Laurentian_Bark_mpd")
Laurentian_final_Bark_mpd<-cbind(Test_name,Laurentian_Bark_mpd)


Laurentian_Bark_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Bark_mntd<-Laurentian_Bark_mntd[-1,]
Test_name<-("Laurentian_Bark_mntd")
Laurentian_final_Bark_mntd<-cbind(Test_name,Laurentian_Bark_mntd)

#Serotiny


Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$Serotiny
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$Serotiny

Laurentian_Serotiny_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Serotiny_mpd<-Laurentian_Serotiny_mpd[-1,]
Test_name<-("Laurentian_Serotiny_mpd")
Laurentian_final_Serotiny_mpd<-cbind(Test_name,Laurentian_Serotiny_mpd)


Laurentian_Serotiny_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Serotiny_mntd<-Laurentian_Serotiny_mntd[-1,]
Test_name<-("Laurentian_Serotiny_mntd")
Laurentian_final_Serotiny_mntd<-cbind(Test_name,Laurentian_Serotiny_mntd)

#Grass

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$Grass
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$Grass

Laurentian_Grass_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Grass_mpd<-Laurentian_Grass_mpd[-1,]
Test_name<-("Laurentian_Grass_mpd")
Laurentian_final_Grass_mpd<-cbind(Test_name,Laurentian_Grass_mpd)

Laurentian_Grass_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Grass_mntd<-Laurentian_Grass_mntd[-1,]
Test_name<-("Laurentian_Grass_mntd")
Laurentian_final_Grass_mntd<-cbind(Test_name,Laurentian_Grass_mntd)


#Resprout

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$Resprout
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$Resprout

Laurentian_Resprout_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Resprout_mpd<-Laurentian_Resprout_mpd[-1,]
Test_name<-("Laurentian_Resprout_mpd")
Laurentian_final_Resprout_mpd<-cbind(Test_name,Laurentian_Resprout_mpd)


Laurentian_Resprout_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Resprout_mntd<-Laurentian_Resprout_mntd[-1,]
Test_name<-("Laurentian_Resprout_mntd")
Laurentian_final_Resprout_mntd<-cbind(Test_name,Laurentian_Resprout_mntd)

#More_than_one_adaptation

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$More_than_one_adaptation
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$More_than_one_adaptation

Laurentian_More_than_one_adaptation_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_More_than_one_adaptation_mpd<-Laurentian_More_than_one_adaptation_mpd[-1,]
Test_name<-("Laurentian_More_than_one_adaptation_mpd")
Laurentian_final_More_than_one_adaptation_mpd<-cbind(Test_name,Laurentian_More_than_one_adaptation_mpd)


Laurentian_More_than_one_adaptation_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_More_than_one_adaptation_mntd<-Laurentian_More_than_one_adaptation_mntd[-1,]
Test_name<-("Laurentian_More_than_one_adaptation_mntd")
Laurentian_final_More_than_one_adaptation_mntd<-cbind(Test_name,Laurentian_More_than_one_adaptation_mntd)

#Pyrophillic

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$Pyrophillic
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$Pyrophillic

Laurentian_Pyrophillic_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Pyrophillic_mpd<-Laurentian_Pyrophillic_mpd[-1,]
Test_name<-("Laurentian_Pyrophillic_mpd")
Laurentian_final_Pyrophillic_mpd<-cbind(Test_name,Laurentian_Pyrophillic_mpd)


Laurentian_Pyrophillic_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Pyrophillic_mntd<-Laurentian_Pyrophillic_mntd[-1,]
Test_name<-("Laurentian_Pyrophillic_mntd")
Laurentian_final_Pyrophillic_mntd<-cbind(Test_name,Laurentian_Pyrophillic_mntd)

#Pyrophobic

Laurentian.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laurentian.matrix) <- rownames(bigcsv)
Laurentian.matrix[1,] <- bigcsv$Laurentian & bigcsv$Pyrophobic
Laurentian.matrix[2,] <- bigcsv$Laurentian & bigcsv$Pyrophobic

Laurentian_Pyrophobic_mpd<-ses.mpd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Pyrophobic_mpd<-Laurentian_Pyrophobic_mpd[-1,]
Test_name<-("Laurentian_Pyrophobic_mpd")
Laurentian_final_Pyrophobic_mpd<-cbind(Test_name,Laurentian_Pyrophobic_mpd)


Laurentian_Pyrophobic_mntd<-ses.mntd(samp = Laurentian.matrix, dis =  cophenetic(Laurentian.tre))
Laurentian_Pyrophobic_mntd<-Laurentian_Pyrophobic_mntd[-1,]
Test_name<-("Laurentian_Pyrophobic_mntd")
Laurentian_final_Pyrophobic_mntd<-cbind(Test_name,Laurentian_Pyrophobic_mntd)




#get final csv for Laurentian mpd
Laurentian_final_mpd_combined<-rbind(Laurentian_final_mpd_geog,Laurentian_final_Bark_mpd,Laurentian_final_Serotiny_mpd,Laurentian_final_Grass_mpd,Laurentian_final_Resprout_mpd,Laurentian_final_Pyrophillic_mpd,Laurentian_final_More_than_one_adaptation_mpd,Laurentian_final_Pyrophobic_mpd)

#write to csv

write.csv(Laurentian_final_mpd_combined, "Outfile_Laurentian_final_mpd_combined.csv")

#get final csv for Laurentian mntd
Laurentian_final_mntd_combined<-rbind(Laurentian_final_mntd_geog,Laurentian_final_Bark_mntd,Laurentian_final_Serotiny_mntd,Laurentian_final_Grass_mntd,Laurentian_final_Resprout_mntd,Laurentian_final_Pyrophillic_mntd,Laurentian_final_More_than_one_adaptation_mntd,Laurentian_final_Pyrophobic_mntd)

#write to csv

write.csv(Laurentian_final_mntd_combined, "Outfile_Laurentian_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W
Boreal_W.matrix[2,] <- bigcsv$Boreal_W

Boreal_W.matrix

length(rownames(bigcsv))
Boreal_W <- rownames(bigcsv)[bigcsv$Boreal_W==1]

Boreal_W

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Boreal_W.tre <- keep.tip(conif.tree, Boreal_W)
plotTree(Boreal_W.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Boreal_W.matrix, dis = cophenetic(Boreal_W.tre))
Boreal_W_geog_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(conif.tree))
Boreal_W_geog_mpd<-Boreal_W_geog_mpd[-1,]
Test_name<-("Boreal_W_geog_mpd")
Boreal_W_final_mpd_geog<-cbind(Test_name,Boreal_W_geog_mpd)


mntd(samp = Boreal_W.matrix, dis = cophenetic(Boreal_W.tre))
Boreal_W_geog_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(conif.tree))
Boreal_W_geog_mntd<-Boreal_W_geog_mntd[-1,]
Test_name<-("Boreal_W_geog_mntd")
Boreal_W_final_mntd_geog<-cbind(Test_name,Boreal_W_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$Bark
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$Bark

Boreal_W_Bark_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Bark_mpd<-Boreal_W_Bark_mpd[-1,]
Test_name<-("Boreal_W_Bark_mpd")
Boreal_W_final_Bark_mpd<-cbind(Test_name,Boreal_W_Bark_mpd)


Boreal_W_Bark_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Bark_mntd<-Boreal_W_Bark_mntd[-1,]
Test_name<-("Boreal_W_Bark_mntd")
Boreal_W_final_Bark_mntd<-cbind(Test_name,Boreal_W_Bark_mntd)

#Serotiny


Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$Serotiny
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$Serotiny

Boreal_W_Serotiny_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Serotiny_mpd<-Boreal_W_Serotiny_mpd[-1,]
Test_name<-("Boreal_W_Serotiny_mpd")
Boreal_W_final_Serotiny_mpd<-cbind(Test_name,Boreal_W_Serotiny_mpd)


Boreal_W_Serotiny_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Serotiny_mntd<-Boreal_W_Serotiny_mntd[-1,]
Test_name<-("Boreal_W_Serotiny_mntd")
Boreal_W_final_Serotiny_mntd<-cbind(Test_name,Boreal_W_Serotiny_mntd)

#Grass

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$Grass
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$Grass

Boreal_W_Grass_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Grass_mpd<-Boreal_W_Grass_mpd[-1,]
Test_name<-("Boreal_W_Grass_mpd")
Boreal_W_final_Grass_mpd<-cbind(Test_name,Boreal_W_Grass_mpd)

Boreal_W_Grass_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Grass_mntd<-Boreal_W_Grass_mntd[-1,]
Test_name<-("Boreal_W_Grass_mntd")
Boreal_W_final_Grass_mntd<-cbind(Test_name,Boreal_W_Grass_mntd)


#Resprout

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$Resprout
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$Resprout

Boreal_W_Resprout_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Resprout_mpd<-Boreal_W_Resprout_mpd[-1,]
Test_name<-("Boreal_W_Resprout_mpd")
Boreal_W_final_Resprout_mpd<-cbind(Test_name,Boreal_W_Resprout_mpd)


Boreal_W_Resprout_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Resprout_mntd<-Boreal_W_Resprout_mntd[-1,]
Test_name<-("Boreal_W_Resprout_mntd")
Boreal_W_final_Resprout_mntd<-cbind(Test_name,Boreal_W_Resprout_mntd)

#More_than_one_adaptation

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$More_than_one_adaptation
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$More_than_one_adaptation

Boreal_W_More_than_one_adaptation_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_More_than_one_adaptation_mpd<-Boreal_W_More_than_one_adaptation_mpd[-1,]
Test_name<-("Boreal_W_More_than_one_adaptation_mpd")
Boreal_W_final_More_than_one_adaptation_mpd<-cbind(Test_name,Boreal_W_More_than_one_adaptation_mpd)


Boreal_W_More_than_one_adaptation_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_More_than_one_adaptation_mntd<-Boreal_W_More_than_one_adaptation_mntd[-1,]
Test_name<-("Boreal_W_More_than_one_adaptation_mntd")
Boreal_W_final_More_than_one_adaptation_mntd<-cbind(Test_name,Boreal_W_More_than_one_adaptation_mntd)

#Pyrophillic

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$Pyrophillic
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$Pyrophillic

Boreal_W_Pyrophillic_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Pyrophillic_mpd<-Boreal_W_Pyrophillic_mpd[-1,]
Test_name<-("Boreal_W_Pyrophillic_mpd")
Boreal_W_final_Pyrophillic_mpd<-cbind(Test_name,Boreal_W_Pyrophillic_mpd)


Boreal_W_Pyrophillic_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Pyrophillic_mntd<-Boreal_W_Pyrophillic_mntd[-1,]
Test_name<-("Boreal_W_Pyrophillic_mntd")
Boreal_W_final_Pyrophillic_mntd<-cbind(Test_name,Boreal_W_Pyrophillic_mntd)

#Pyrophobic

Boreal_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_W.matrix) <- rownames(bigcsv)
Boreal_W.matrix[1,] <- bigcsv$Boreal_W & bigcsv$Pyrophobic
Boreal_W.matrix[2,] <- bigcsv$Boreal_W & bigcsv$Pyrophobic

Boreal_W_Pyrophobic_mpd<-ses.mpd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Pyrophobic_mpd<-Boreal_W_Pyrophobic_mpd[-1,]
Test_name<-("Boreal_W_Pyrophobic_mpd")
Boreal_W_final_Pyrophobic_mpd<-cbind(Test_name,Boreal_W_Pyrophobic_mpd)


Boreal_W_Pyrophobic_mntd<-ses.mntd(samp = Boreal_W.matrix, dis =  cophenetic(Boreal_W.tre))
Boreal_W_Pyrophobic_mntd<-Boreal_W_Pyrophobic_mntd[-1,]
Test_name<-("Boreal_W_Pyrophobic_mntd")
Boreal_W_final_Pyrophobic_mntd<-cbind(Test_name,Boreal_W_Pyrophobic_mntd)




#get final csv for Boreal_W mpd
Boreal_W_final_mpd_combined<-rbind(Boreal_W_final_mpd_geog,Boreal_W_final_Bark_mpd,Boreal_W_final_Serotiny_mpd,Boreal_W_final_Grass_mpd,Boreal_W_final_Resprout_mpd,Boreal_W_final_Pyrophillic_mpd,Boreal_W_final_More_than_one_adaptation_mpd,Boreal_W_final_Pyrophobic_mpd)

#write to csv

write.csv(Boreal_W_final_mpd_combined, "Outfile_Boreal_W_final_mpd_combined.csv")

#get final csv for Boreal_W mntd
Boreal_W_final_mntd_combined<-rbind(Boreal_W_final_mntd_geog,Boreal_W_final_Bark_mntd,Boreal_W_final_Serotiny_mntd,Boreal_W_final_Grass_mntd,Boreal_W_final_Resprout_mntd,Boreal_W_final_Pyrophillic_mntd,Boreal_W_final_More_than_one_adaptation_mntd,Boreal_W_final_Pyrophobic_mntd)

#write to csv

write.csv(Boreal_W_final_mntd_combined, "Outfile_Boreal_W_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E
Boreal_E.matrix[2,] <- bigcsv$Boreal_E

Boreal_E.matrix

length(rownames(bigcsv))
Boreal_E <- rownames(bigcsv)[bigcsv$Boreal_E==1]

Boreal_E

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Boreal_E.tre <- keep.tip(conif.tree, Boreal_E)
plotTree(Boreal_E.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Boreal_E.matrix, dis = cophenetic(Boreal_E.tre))
Boreal_E_geog_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(conif.tree))
Boreal_E_geog_mpd<-Boreal_E_geog_mpd[-1,]
Test_name<-("Boreal_E_geog_mpd")
Boreal_E_final_mpd_geog<-cbind(Test_name,Boreal_E_geog_mpd)


mntd(samp = Boreal_E.matrix, dis = cophenetic(Boreal_E.tre))
Boreal_E_geog_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(conif.tree))
Boreal_E_geog_mntd<-Boreal_E_geog_mntd[-1,]
Test_name<-("Boreal_E_geog_mntd")
Boreal_E_final_mntd_geog<-cbind(Test_name,Boreal_E_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$Bark
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$Bark

Boreal_E_Bark_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Bark_mpd<-Boreal_E_Bark_mpd[-1,]
Test_name<-("Boreal_E_Bark_mpd")
Boreal_E_final_Bark_mpd<-cbind(Test_name,Boreal_E_Bark_mpd)


Boreal_E_Bark_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Bark_mntd<-Boreal_E_Bark_mntd[-1,]
Test_name<-("Boreal_E_Bark_mntd")
Boreal_E_final_Bark_mntd<-cbind(Test_name,Boreal_E_Bark_mntd)

#Serotiny


Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$Serotiny
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$Serotiny

Boreal_E_Serotiny_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Serotiny_mpd<-Boreal_E_Serotiny_mpd[-1,]
Test_name<-("Boreal_E_Serotiny_mpd")
Boreal_E_final_Serotiny_mpd<-cbind(Test_name,Boreal_E_Serotiny_mpd)


Boreal_E_Serotiny_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Serotiny_mntd<-Boreal_E_Serotiny_mntd[-1,]
Test_name<-("Boreal_E_Serotiny_mntd")
Boreal_E_final_Serotiny_mntd<-cbind(Test_name,Boreal_E_Serotiny_mntd)

#Grass

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$Grass
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$Grass

Boreal_E_Grass_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Grass_mpd<-Boreal_E_Grass_mpd[-1,]
Test_name<-("Boreal_E_Grass_mpd")
Boreal_E_final_Grass_mpd<-cbind(Test_name,Boreal_E_Grass_mpd)

Boreal_E_Grass_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Grass_mntd<-Boreal_E_Grass_mntd[-1,]
Test_name<-("Boreal_E_Grass_mntd")
Boreal_E_final_Grass_mntd<-cbind(Test_name,Boreal_E_Grass_mntd)


#Resprout

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$Resprout
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$Resprout

Boreal_E_Resprout_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Resprout_mpd<-Boreal_E_Resprout_mpd[-1,]
Test_name<-("Boreal_E_Resprout_mpd")
Boreal_E_final_Resprout_mpd<-cbind(Test_name,Boreal_E_Resprout_mpd)


Boreal_E_Resprout_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Resprout_mntd<-Boreal_E_Resprout_mntd[-1,]
Test_name<-("Boreal_E_Resprout_mntd")
Boreal_E_final_Resprout_mntd<-cbind(Test_name,Boreal_E_Resprout_mntd)

#More_than_one_adaptation

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$More_than_one_adaptation
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$More_than_one_adaptation

Boreal_E_More_than_one_adaptation_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_More_than_one_adaptation_mpd<-Boreal_E_More_than_one_adaptation_mpd[-1,]
Test_name<-("Boreal_E_More_than_one_adaptation_mpd")
Boreal_E_final_More_than_one_adaptation_mpd<-cbind(Test_name,Boreal_E_More_than_one_adaptation_mpd)


Boreal_E_More_than_one_adaptation_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_More_than_one_adaptation_mntd<-Boreal_E_More_than_one_adaptation_mntd[-1,]
Test_name<-("Boreal_E_More_than_one_adaptation_mntd")
Boreal_E_final_More_than_one_adaptation_mntd<-cbind(Test_name,Boreal_E_More_than_one_adaptation_mntd)

#Pyrophillic

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$Pyrophillic
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$Pyrophillic

Boreal_E_Pyrophillic_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Pyrophillic_mpd<-Boreal_E_Pyrophillic_mpd[-1,]
Test_name<-("Boreal_E_Pyrophillic_mpd")
Boreal_E_final_Pyrophillic_mpd<-cbind(Test_name,Boreal_E_Pyrophillic_mpd)


Boreal_E_Pyrophillic_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Pyrophillic_mntd<-Boreal_E_Pyrophillic_mntd[-1,]
Test_name<-("Boreal_E_Pyrophillic_mntd")
Boreal_E_final_Pyrophillic_mntd<-cbind(Test_name,Boreal_E_Pyrophillic_mntd)

#Pyrophobic

Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_E.matrix) <- rownames(bigcsv)
Boreal_E.matrix[1,] <- bigcsv$Boreal_E & bigcsv$Pyrophobic
Boreal_E.matrix[2,] <- bigcsv$Boreal_E & bigcsv$Pyrophobic

Boreal_E_Pyrophobic_mpd<-ses.mpd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Pyrophobic_mpd<-Boreal_E_Pyrophobic_mpd[-1,]
Test_name<-("Boreal_E_Pyrophobic_mpd")
Boreal_E_final_Pyrophobic_mpd<-cbind(Test_name,Boreal_E_Pyrophobic_mpd)


Boreal_E_Pyrophobic_mntd<-ses.mntd(samp = Boreal_E.matrix, dis =  cophenetic(Boreal_E.tre))
Boreal_E_Pyrophobic_mntd<-Boreal_E_Pyrophobic_mntd[-1,]
Test_name<-("Boreal_E_Pyrophobic_mntd")
Boreal_E_final_Pyrophobic_mntd<-cbind(Test_name,Boreal_E_Pyrophobic_mntd)




#get final csv for Boreal_E mpd
Boreal_E_final_mpd_combined<-rbind(Boreal_E_final_mpd_geog,Boreal_E_final_Bark_mpd,Boreal_E_final_Serotiny_mpd,Boreal_E_final_Grass_mpd,Boreal_E_final_Resprout_mpd,Boreal_E_final_Pyrophillic_mpd,Boreal_E_final_More_than_one_adaptation_mpd,Boreal_E_final_Pyrophobic_mpd)

#write to csv

write.csv(Boreal_E_final_mpd_combined, "Outfile_Boreal_E_final_mpd_combined.csv")

#get final csv for Boreal_E mntd
Boreal_E_final_mntd_combined<-rbind(Boreal_E_final_mntd_geog,Boreal_E_final_Bark_mntd,Boreal_E_final_Serotiny_mntd,Boreal_E_final_Grass_mntd,Boreal_E_final_Resprout_mntd,Boreal_E_final_Pyrophillic_mntd,Boreal_E_final_More_than_one_adaptation_mntd,Boreal_E_final_Pyrophobic_mntd)

#write to csv

write.csv(Boreal_E_final_mntd_combined, "Outfile_Boreal_E_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern

Total_Eastern.matrix

length(rownames(bigcsv))
Total_Eastern <- rownames(bigcsv)[bigcsv$Total_Eastern==1]

Total_Eastern

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Total_Eastern.tre <- keep.tip(conif.tree, Total_Eastern)
plotTree(Total_Eastern.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Total_Eastern.matrix, dis = cophenetic(Total_Eastern.tre))
Total_Eastern_geog_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(conif.tree))
Total_Eastern_geog_mpd<-Total_Eastern_geog_mpd[-1,]
Test_name<-("Total_Eastern_geog_mpd")
Total_Eastern_final_mpd_geog<-cbind(Test_name,Total_Eastern_geog_mpd)


mntd(samp = Total_Eastern.matrix, dis = cophenetic(Total_Eastern.tre))
Total_Eastern_geog_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(conif.tree))
Total_Eastern_geog_mntd<-Total_Eastern_geog_mntd[-1,]
Test_name<-("Total_Eastern_geog_mntd")
Total_Eastern_final_mntd_geog<-cbind(Test_name,Total_Eastern_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$Bark
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$Bark

Total_Eastern_Bark_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Bark_mpd<-Total_Eastern_Bark_mpd[-1,]
Test_name<-("Total_Eastern_Bark_mpd")
Total_Eastern_final_Bark_mpd<-cbind(Test_name,Total_Eastern_Bark_mpd)


Total_Eastern_Bark_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Bark_mntd<-Total_Eastern_Bark_mntd[-1,]
Test_name<-("Total_Eastern_Bark_mntd")
Total_Eastern_final_Bark_mntd<-cbind(Test_name,Total_Eastern_Bark_mntd)

#Serotiny


Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$Serotiny
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$Serotiny

Total_Eastern_Serotiny_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Serotiny_mpd<-Total_Eastern_Serotiny_mpd[-1,]
Test_name<-("Total_Eastern_Serotiny_mpd")
Total_Eastern_final_Serotiny_mpd<-cbind(Test_name,Total_Eastern_Serotiny_mpd)


Total_Eastern_Serotiny_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Serotiny_mntd<-Total_Eastern_Serotiny_mntd[-1,]
Test_name<-("Total_Eastern_Serotiny_mntd")
Total_Eastern_final_Serotiny_mntd<-cbind(Test_name,Total_Eastern_Serotiny_mntd)

#Grass

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$Grass
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$Grass

Total_Eastern_Grass_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Grass_mpd<-Total_Eastern_Grass_mpd[-1,]
Test_name<-("Total_Eastern_Grass_mpd")
Total_Eastern_final_Grass_mpd<-cbind(Test_name,Total_Eastern_Grass_mpd)

Total_Eastern_Grass_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Grass_mntd<-Total_Eastern_Grass_mntd[-1,]
Test_name<-("Total_Eastern_Grass_mntd")
Total_Eastern_final_Grass_mntd<-cbind(Test_name,Total_Eastern_Grass_mntd)


#Resprout

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$Resprout
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$Resprout

Total_Eastern_Resprout_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Resprout_mpd<-Total_Eastern_Resprout_mpd[-1,]
Test_name<-("Total_Eastern_Resprout_mpd")
Total_Eastern_final_Resprout_mpd<-cbind(Test_name,Total_Eastern_Resprout_mpd)


Total_Eastern_Resprout_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Resprout_mntd<-Total_Eastern_Resprout_mntd[-1,]
Test_name<-("Total_Eastern_Resprout_mntd")
Total_Eastern_final_Resprout_mntd<-cbind(Test_name,Total_Eastern_Resprout_mntd)

#More_than_one_adaptation

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$More_than_one_adaptation
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$More_than_one_adaptation

Total_Eastern_More_than_one_adaptation_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_More_than_one_adaptation_mpd<-Total_Eastern_More_than_one_adaptation_mpd[-1,]
Test_name<-("Total_Eastern_More_than_one_adaptation_mpd")
Total_Eastern_final_More_than_one_adaptation_mpd<-cbind(Test_name,Total_Eastern_More_than_one_adaptation_mpd)


Total_Eastern_More_than_one_adaptation_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_More_than_one_adaptation_mntd<-Total_Eastern_More_than_one_adaptation_mntd[-1,]
Test_name<-("Total_Eastern_More_than_one_adaptation_mntd")
Total_Eastern_final_More_than_one_adaptation_mntd<-cbind(Test_name,Total_Eastern_More_than_one_adaptation_mntd)

#Pyrophillic

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$Pyrophillic
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$Pyrophillic

Total_Eastern_Pyrophillic_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Pyrophillic_mpd<-Total_Eastern_Pyrophillic_mpd[-1,]
Test_name<-("Total_Eastern_Pyrophillic_mpd")
Total_Eastern_final_Pyrophillic_mpd<-cbind(Test_name,Total_Eastern_Pyrophillic_mpd)


Total_Eastern_Pyrophillic_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Pyrophillic_mntd<-Total_Eastern_Pyrophillic_mntd[-1,]
Test_name<-("Total_Eastern_Pyrophillic_mntd")
Total_Eastern_final_Pyrophillic_mntd<-cbind(Test_name,Total_Eastern_Pyrophillic_mntd)

#Pyrophobic

Total_Eastern.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Total_Eastern.matrix) <- rownames(bigcsv)
Total_Eastern.matrix[1,] <- bigcsv$Total_Eastern & bigcsv$Pyrophobic
Total_Eastern.matrix[2,] <- bigcsv$Total_Eastern & bigcsv$Pyrophobic

Total_Eastern_Pyrophobic_mpd<-ses.mpd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Pyrophobic_mpd<-Total_Eastern_Pyrophobic_mpd[-1,]
Test_name<-("Total_Eastern_Pyrophobic_mpd")
Total_Eastern_final_Pyrophobic_mpd<-cbind(Test_name,Total_Eastern_Pyrophobic_mpd)


Total_Eastern_Pyrophobic_mntd<-ses.mntd(samp = Total_Eastern.matrix, dis =  cophenetic(Total_Eastern.tre))
Total_Eastern_Pyrophobic_mntd<-Total_Eastern_Pyrophobic_mntd[-1,]
Test_name<-("Total_Eastern_Pyrophobic_mntd")
Total_Eastern_final_Pyrophobic_mntd<-cbind(Test_name,Total_Eastern_Pyrophobic_mntd)




#get final csv for Total_Eastern mpd
Total_Eastern_final_mpd_combined<-rbind(Total_Eastern_final_mpd_geog,Total_Eastern_final_Bark_mpd,Total_Eastern_final_Serotiny_mpd,Total_Eastern_final_Grass_mpd,Total_Eastern_final_Resprout_mpd,Total_Eastern_final_Pyrophillic_mpd,Total_Eastern_final_More_than_one_adaptation_mpd,Total_Eastern_final_Pyrophobic_mpd)

#write to csv

write.csv(Total_Eastern_final_mpd_combined, "Outfile_Total_Eastern_final_mpd_combined.csv")

#get final csv for Total_Eastern mntd
Total_Eastern_final_mntd_combined<-rbind(Total_Eastern_final_mntd_geog,Total_Eastern_final_Bark_mntd,Total_Eastern_final_Serotiny_mntd,Total_Eastern_final_Grass_mntd,Total_Eastern_final_Resprout_mntd,Total_Eastern_final_Pyrophillic_mntd,Total_Eastern_final_More_than_one_adaptation_mntd,Total_Eastern_final_Pyrophobic_mntd)

#write to csv

write.csv(Total_Eastern_final_mntd_combined, "Outfile_Total_Eastern_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W

Laur_E_W.matrix

length(rownames(bigcsv))
Laur_E_W <- rownames(bigcsv)[bigcsv$Laur_E_W==1]

Laur_E_W

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Laur_E_W.tre <- keep.tip(conif.tree, Laur_E_W)
plotTree(Laur_E_W.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Laur_E_W.matrix, dis = cophenetic(Laur_E_W.tre))
Laur_E_W_geog_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(conif.tree))
Laur_E_W_geog_mpd<-Laur_E_W_geog_mpd[-1,]
Test_name<-("Laur_E_W_geog_mpd")
Laur_E_W_final_mpd_geog<-cbind(Test_name,Laur_E_W_geog_mpd)


mntd(samp = Laur_E_W.matrix, dis = cophenetic(Laur_E_W.tre))
Laur_E_W_geog_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(conif.tree))
Laur_E_W_geog_mntd<-Laur_E_W_geog_mntd[-1,]
Test_name<-("Laur_E_W_geog_mntd")
Laur_E_W_final_mntd_geog<-cbind(Test_name,Laur_E_W_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$Bark
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$Bark

Laur_E_W_Bark_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Bark_mpd<-Laur_E_W_Bark_mpd[-1,]
Test_name<-("Laur_E_W_Bark_mpd")
Laur_E_W_final_Bark_mpd<-cbind(Test_name,Laur_E_W_Bark_mpd)


Laur_E_W_Bark_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Bark_mntd<-Laur_E_W_Bark_mntd[-1,]
Test_name<-("Laur_E_W_Bark_mntd")
Laur_E_W_final_Bark_mntd<-cbind(Test_name,Laur_E_W_Bark_mntd)

#Serotiny


Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$Serotiny
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$Serotiny

Laur_E_W_Serotiny_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Serotiny_mpd<-Laur_E_W_Serotiny_mpd[-1,]
Test_name<-("Laur_E_W_Serotiny_mpd")
Laur_E_W_final_Serotiny_mpd<-cbind(Test_name,Laur_E_W_Serotiny_mpd)


Laur_E_W_Serotiny_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Serotiny_mntd<-Laur_E_W_Serotiny_mntd[-1,]
Test_name<-("Laur_E_W_Serotiny_mntd")
Laur_E_W_final_Serotiny_mntd<-cbind(Test_name,Laur_E_W_Serotiny_mntd)

#Grass

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$Grass
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$Grass

Laur_E_W_Grass_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Grass_mpd<-Laur_E_W_Grass_mpd[-1,]
Test_name<-("Laur_E_W_Grass_mpd")
Laur_E_W_final_Grass_mpd<-cbind(Test_name,Laur_E_W_Grass_mpd)

Laur_E_W_Grass_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Grass_mntd<-Laur_E_W_Grass_mntd[-1,]
Test_name<-("Laur_E_W_Grass_mntd")
Laur_E_W_final_Grass_mntd<-cbind(Test_name,Laur_E_W_Grass_mntd)


#Resprout

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$Resprout
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$Resprout

Laur_E_W_Resprout_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Resprout_mpd<-Laur_E_W_Resprout_mpd[-1,]
Test_name<-("Laur_E_W_Resprout_mpd")
Laur_E_W_final_Resprout_mpd<-cbind(Test_name,Laur_E_W_Resprout_mpd)


Laur_E_W_Resprout_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Resprout_mntd<-Laur_E_W_Resprout_mntd[-1,]
Test_name<-("Laur_E_W_Resprout_mntd")
Laur_E_W_final_Resprout_mntd<-cbind(Test_name,Laur_E_W_Resprout_mntd)

#More_than_one_adaptation

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$More_than_one_adaptation
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$More_than_one_adaptation

Laur_E_W_More_than_one_adaptation_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_More_than_one_adaptation_mpd<-Laur_E_W_More_than_one_adaptation_mpd[-1,]
Test_name<-("Laur_E_W_More_than_one_adaptation_mpd")
Laur_E_W_final_More_than_one_adaptation_mpd<-cbind(Test_name,Laur_E_W_More_than_one_adaptation_mpd)


Laur_E_W_More_than_one_adaptation_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_More_than_one_adaptation_mntd<-Laur_E_W_More_than_one_adaptation_mntd[-1,]
Test_name<-("Laur_E_W_More_than_one_adaptation_mntd")
Laur_E_W_final_More_than_one_adaptation_mntd<-cbind(Test_name,Laur_E_W_More_than_one_adaptation_mntd)

#Pyrophillic

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$Pyrophillic
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$Pyrophillic

Laur_E_W_Pyrophillic_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Pyrophillic_mpd<-Laur_E_W_Pyrophillic_mpd[-1,]
Test_name<-("Laur_E_W_Pyrophillic_mpd")
Laur_E_W_final_Pyrophillic_mpd<-cbind(Test_name,Laur_E_W_Pyrophillic_mpd)


Laur_E_W_Pyrophillic_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Pyrophillic_mntd<-Laur_E_W_Pyrophillic_mntd[-1,]
Test_name<-("Laur_E_W_Pyrophillic_mntd")
Laur_E_W_final_Pyrophillic_mntd<-cbind(Test_name,Laur_E_W_Pyrophillic_mntd)

#Pyrophobic

Laur_E_W.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_E_W.matrix) <- rownames(bigcsv)
Laur_E_W.matrix[1,] <- bigcsv$Laur_E_W & bigcsv$Pyrophobic
Laur_E_W.matrix[2,] <- bigcsv$Laur_E_W & bigcsv$Pyrophobic

Laur_E_W_Pyrophobic_mpd<-ses.mpd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Pyrophobic_mpd<-Laur_E_W_Pyrophobic_mpd[-1,]
Test_name<-("Laur_E_W_Pyrophobic_mpd")
Laur_E_W_final_Pyrophobic_mpd<-cbind(Test_name,Laur_E_W_Pyrophobic_mpd)


Laur_E_W_Pyrophobic_mntd<-ses.mntd(samp = Laur_E_W.matrix, dis =  cophenetic(Laur_E_W.tre))
Laur_E_W_Pyrophobic_mntd<-Laur_E_W_Pyrophobic_mntd[-1,]
Test_name<-("Laur_E_W_Pyrophobic_mntd")
Laur_E_W_final_Pyrophobic_mntd<-cbind(Test_name,Laur_E_W_Pyrophobic_mntd)




#get final csv for Laur_E_W mpd
Laur_E_W_final_mpd_combined<-rbind(Laur_E_W_final_mpd_geog,Laur_E_W_final_Bark_mpd,Laur_E_W_final_Serotiny_mpd,Laur_E_W_final_Grass_mpd,Laur_E_W_final_Resprout_mpd,Laur_E_W_final_Pyrophillic_mpd,Laur_E_W_final_More_than_one_adaptation_mpd,Laur_E_W_final_Pyrophobic_mpd)

#write to csv

write.csv(Laur_E_W_final_mpd_combined, "Outfile_Laur_E_W_final_mpd_combined.csv")

#get final csv for Laur_E_W mntd
Laur_E_W_final_mntd_combined<-rbind(Laur_E_W_final_mntd_geog,Laur_E_W_final_Bark_mntd,Laur_E_W_final_Serotiny_mntd,Laur_E_W_final_Grass_mntd,Laur_E_W_final_Resprout_mntd,Laur_E_W_final_Pyrophillic_mntd,Laur_E_W_final_More_than_one_adaptation_mntd,Laur_E_W_final_Pyrophobic_mntd)

#write to csv

write.csv(Laur_E_W_final_mntd_combined, "Outfile_Laur_E_W_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E

Laur_and_Boreal_E.matrix

length(rownames(bigcsv))
Laur_and_Boreal_E <- rownames(bigcsv)[bigcsv$Laur_and_Boreal_E==1]

Laur_and_Boreal_E

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Laur_and_Boreal_E.tre <- keep.tip(conif.tree, Laur_and_Boreal_E)
plotTree(Laur_and_Boreal_E.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Laur_and_Boreal_E.matrix, dis = cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_geog_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(conif.tree))
Laur_and_Boreal_E_geog_mpd<-Laur_and_Boreal_E_geog_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_geog_mpd")
Laur_and_Boreal_E_final_mpd_geog<-cbind(Test_name,Laur_and_Boreal_E_geog_mpd)


mntd(samp = Laur_and_Boreal_E.matrix, dis = cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_geog_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(conif.tree))
Laur_and_Boreal_E_geog_mntd<-Laur_and_Boreal_E_geog_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_geog_mntd")
Laur_and_Boreal_E_final_mntd_geog<-cbind(Test_name,Laur_and_Boreal_E_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Bark
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Bark

Laur_and_Boreal_E_Bark_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Bark_mpd<-Laur_and_Boreal_E_Bark_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_Bark_mpd")
Laur_and_Boreal_E_final_Bark_mpd<-cbind(Test_name,Laur_and_Boreal_E_Bark_mpd)


Laur_and_Boreal_E_Bark_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Bark_mntd<-Laur_and_Boreal_E_Bark_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_Bark_mntd")
Laur_and_Boreal_E_final_Bark_mntd<-cbind(Test_name,Laur_and_Boreal_E_Bark_mntd)

#Serotiny


Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Serotiny
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Serotiny

Laur_and_Boreal_E_Serotiny_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Serotiny_mpd<-Laur_and_Boreal_E_Serotiny_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_Serotiny_mpd")
Laur_and_Boreal_E_final_Serotiny_mpd<-cbind(Test_name,Laur_and_Boreal_E_Serotiny_mpd)


Laur_and_Boreal_E_Serotiny_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Serotiny_mntd<-Laur_and_Boreal_E_Serotiny_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_Serotiny_mntd")
Laur_and_Boreal_E_final_Serotiny_mntd<-cbind(Test_name,Laur_and_Boreal_E_Serotiny_mntd)

#Grass

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Grass
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Grass

Laur_and_Boreal_E_Grass_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Grass_mpd<-Laur_and_Boreal_E_Grass_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_Grass_mpd")
Laur_and_Boreal_E_final_Grass_mpd<-cbind(Test_name,Laur_and_Boreal_E_Grass_mpd)

Laur_and_Boreal_E_Grass_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Grass_mntd<-Laur_and_Boreal_E_Grass_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_Grass_mntd")
Laur_and_Boreal_E_final_Grass_mntd<-cbind(Test_name,Laur_and_Boreal_E_Grass_mntd)


#Resprout

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Resprout
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Resprout

Laur_and_Boreal_E_Resprout_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Resprout_mpd<-Laur_and_Boreal_E_Resprout_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_Resprout_mpd")
Laur_and_Boreal_E_final_Resprout_mpd<-cbind(Test_name,Laur_and_Boreal_E_Resprout_mpd)


Laur_and_Boreal_E_Resprout_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Resprout_mntd<-Laur_and_Boreal_E_Resprout_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_Resprout_mntd")
Laur_and_Boreal_E_final_Resprout_mntd<-cbind(Test_name,Laur_and_Boreal_E_Resprout_mntd)

#More_than_one_adaptation

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$More_than_one_adaptation
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$More_than_one_adaptation

Laur_and_Boreal_E_More_than_one_adaptation_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_More_than_one_adaptation_mpd<-Laur_and_Boreal_E_More_than_one_adaptation_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_More_than_one_adaptation_mpd")
Laur_and_Boreal_E_final_More_than_one_adaptation_mpd<-cbind(Test_name,Laur_and_Boreal_E_More_than_one_adaptation_mpd)


Laur_and_Boreal_E_More_than_one_adaptation_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_More_than_one_adaptation_mntd<-Laur_and_Boreal_E_More_than_one_adaptation_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_More_than_one_adaptation_mntd")
Laur_and_Boreal_E_final_More_than_one_adaptation_mntd<-cbind(Test_name,Laur_and_Boreal_E_More_than_one_adaptation_mntd)

#Pyrophillic

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Pyrophillic
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Pyrophillic

Laur_and_Boreal_E_Pyrophillic_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Pyrophillic_mpd<-Laur_and_Boreal_E_Pyrophillic_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_Pyrophillic_mpd")
Laur_and_Boreal_E_final_Pyrophillic_mpd<-cbind(Test_name,Laur_and_Boreal_E_Pyrophillic_mpd)


Laur_and_Boreal_E_Pyrophillic_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Pyrophillic_mntd<-Laur_and_Boreal_E_Pyrophillic_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_Pyrophillic_mntd")
Laur_and_Boreal_E_final_Pyrophillic_mntd<-cbind(Test_name,Laur_and_Boreal_E_Pyrophillic_mntd)

#Pyrophobic

Laur_and_Boreal_E.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_and_Boreal_E.matrix) <- rownames(bigcsv)
Laur_and_Boreal_E.matrix[1,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Pyrophobic
Laur_and_Boreal_E.matrix[2,] <- bigcsv$Laur_and_Boreal_E & bigcsv$Pyrophobic

Laur_and_Boreal_E_Pyrophobic_mpd<-ses.mpd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Pyrophobic_mpd<-Laur_and_Boreal_E_Pyrophobic_mpd[-1,]
Test_name<-("Laur_and_Boreal_E_Pyrophobic_mpd")
Laur_and_Boreal_E_final_Pyrophobic_mpd<-cbind(Test_name,Laur_and_Boreal_E_Pyrophobic_mpd)


Laur_and_Boreal_E_Pyrophobic_mntd<-ses.mntd(samp = Laur_and_Boreal_E.matrix, dis =  cophenetic(Laur_and_Boreal_E.tre))
Laur_and_Boreal_E_Pyrophobic_mntd<-Laur_and_Boreal_E_Pyrophobic_mntd[-1,]
Test_name<-("Laur_and_Boreal_E_Pyrophobic_mntd")
Laur_and_Boreal_E_final_Pyrophobic_mntd<-cbind(Test_name,Laur_and_Boreal_E_Pyrophobic_mntd)




#get final csv for Laur_and_Boreal_E mpd
Laur_and_Boreal_E_final_mpd_combined<-rbind(Laur_and_Boreal_E_final_mpd_geog,Laur_and_Boreal_E_final_Bark_mpd,Laur_and_Boreal_E_final_Serotiny_mpd,Laur_and_Boreal_E_final_Grass_mpd,Laur_and_Boreal_E_final_Resprout_mpd,Laur_and_Boreal_E_final_Pyrophillic_mpd,Laur_and_Boreal_E_final_More_than_one_adaptation_mpd,Laur_and_Boreal_E_final_Pyrophobic_mpd)

#write to csv

write.csv(Laur_and_Boreal_E_final_mpd_combined, "Outfile_Laur_and_Boreal_E_final_mpd_combined.csv")

#get final csv for Laur_and_Boreal_E mntd
Laur_and_Boreal_E_final_mntd_combined<-rbind(Laur_and_Boreal_E_final_mntd_geog,Laur_and_Boreal_E_final_Bark_mntd,Laur_and_Boreal_E_final_Serotiny_mntd,Laur_and_Boreal_E_final_Grass_mntd,Laur_and_Boreal_E_final_Resprout_mntd,Laur_and_Boreal_E_final_Pyrophillic_mntd,Laur_and_Boreal_E_final_More_than_one_adaptation_mntd,Laur_and_Boreal_E_final_Pyrophobic_mntd)

#write to csv

write.csv(Laur_and_Boreal_E_final_mntd_combined, "Outfile_Laur_and_Boreal_E_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean
Carribean.matrix[2,] <- bigcsv$Carribean

Carribean.matrix

length(rownames(bigcsv))
Carribean <- rownames(bigcsv)[bigcsv$Carribean==1]

Carribean

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Carribean.tre <- keep.tip(conif.tree, Carribean)
plotTree(Carribean.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Carribean.matrix, dis = cophenetic(Carribean.tre))
Carribean_geog_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(conif.tree))
Carribean_geog_mpd<-Carribean_geog_mpd[-1,]
Test_name<-("Carribean_geog_mpd")
Carribean_final_mpd_geog<-cbind(Test_name,Carribean_geog_mpd)


mntd(samp = Carribean.matrix, dis = cophenetic(Carribean.tre))
Carribean_geog_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(conif.tree))
Carribean_geog_mntd<-Carribean_geog_mntd[-1,]
Test_name<-("Carribean_geog_mntd")
Carribean_final_mntd_geog<-cbind(Test_name,Carribean_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$Bark
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$Bark

Carribean_Bark_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Bark_mpd<-Carribean_Bark_mpd[-1,]
Test_name<-("Carribean_Bark_mpd")
Carribean_final_Bark_mpd<-cbind(Test_name,Carribean_Bark_mpd)


Carribean_Bark_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Bark_mntd<-Carribean_Bark_mntd[-1,]
Test_name<-("Carribean_Bark_mntd")
Carribean_final_Bark_mntd<-cbind(Test_name,Carribean_Bark_mntd)

#Serotiny


Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$Serotiny
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$Serotiny

Carribean_Serotiny_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Serotiny_mpd<-Carribean_Serotiny_mpd[-1,]
Test_name<-("Carribean_Serotiny_mpd")
Carribean_final_Serotiny_mpd<-cbind(Test_name,Carribean_Serotiny_mpd)


Carribean_Serotiny_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Serotiny_mntd<-Carribean_Serotiny_mntd[-1,]
Test_name<-("Carribean_Serotiny_mntd")
Carribean_final_Serotiny_mntd<-cbind(Test_name,Carribean_Serotiny_mntd)

#Grass

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$Grass
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$Grass

Carribean_Grass_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Grass_mpd<-Carribean_Grass_mpd[-1,]
Test_name<-("Carribean_Grass_mpd")
Carribean_final_Grass_mpd<-cbind(Test_name,Carribean_Grass_mpd)

Carribean_Grass_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Grass_mntd<-Carribean_Grass_mntd[-1,]
Test_name<-("Carribean_Grass_mntd")
Carribean_final_Grass_mntd<-cbind(Test_name,Carribean_Grass_mntd)


#Resprout

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$Resprout
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$Resprout

Carribean_Resprout_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Resprout_mpd<-Carribean_Resprout_mpd[-1,]
Test_name<-("Carribean_Resprout_mpd")
Carribean_final_Resprout_mpd<-cbind(Test_name,Carribean_Resprout_mpd)


Carribean_Resprout_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Resprout_mntd<-Carribean_Resprout_mntd[-1,]
Test_name<-("Carribean_Resprout_mntd")
Carribean_final_Resprout_mntd<-cbind(Test_name,Carribean_Resprout_mntd)

#More_than_one_adaptation

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$More_than_one_adaptation
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$More_than_one_adaptation

Carribean_More_than_one_adaptation_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_More_than_one_adaptation_mpd<-Carribean_More_than_one_adaptation_mpd[-1,]
Test_name<-("Carribean_More_than_one_adaptation_mpd")
Carribean_final_More_than_one_adaptation_mpd<-cbind(Test_name,Carribean_More_than_one_adaptation_mpd)


Carribean_More_than_one_adaptation_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_More_than_one_adaptation_mntd<-Carribean_More_than_one_adaptation_mntd[-1,]
Test_name<-("Carribean_More_than_one_adaptation_mntd")
Carribean_final_More_than_one_adaptation_mntd<-cbind(Test_name,Carribean_More_than_one_adaptation_mntd)

#Pyrophillic

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$Pyrophillic
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$Pyrophillic

Carribean_Pyrophillic_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Pyrophillic_mpd<-Carribean_Pyrophillic_mpd[-1,]
Test_name<-("Carribean_Pyrophillic_mpd")
Carribean_final_Pyrophillic_mpd<-cbind(Test_name,Carribean_Pyrophillic_mpd)


Carribean_Pyrophillic_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Pyrophillic_mntd<-Carribean_Pyrophillic_mntd[-1,]
Test_name<-("Carribean_Pyrophillic_mntd")
Carribean_final_Pyrophillic_mntd<-cbind(Test_name,Carribean_Pyrophillic_mntd)

#Pyrophobic

Carribean.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Carribean.matrix) <- rownames(bigcsv)
Carribean.matrix[1,] <- bigcsv$Carribean & bigcsv$Pyrophobic
Carribean.matrix[2,] <- bigcsv$Carribean & bigcsv$Pyrophobic

Carribean_Pyrophobic_mpd<-ses.mpd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Pyrophobic_mpd<-Carribean_Pyrophobic_mpd[-1,]
Test_name<-("Carribean_Pyrophobic_mpd")
Carribean_final_Pyrophobic_mpd<-cbind(Test_name,Carribean_Pyrophobic_mpd)


Carribean_Pyrophobic_mntd<-ses.mntd(samp = Carribean.matrix, dis =  cophenetic(Carribean.tre))
Carribean_Pyrophobic_mntd<-Carribean_Pyrophobic_mntd[-1,]
Test_name<-("Carribean_Pyrophobic_mntd")
Carribean_final_Pyrophobic_mntd<-cbind(Test_name,Carribean_Pyrophobic_mntd)




#get final csv for Carribean mpd
Carribean_final_mpd_combined<-rbind(Carribean_final_mpd_geog,Carribean_final_Bark_mpd,Carribean_final_Serotiny_mpd,Carribean_final_Grass_mpd,Carribean_final_Resprout_mpd,Carribean_final_Pyrophillic_mpd,Carribean_final_More_than_one_adaptation_mpd,Carribean_final_Pyrophobic_mpd)

#write to csv

write.csv(Carribean_final_mpd_combined, "Outfile_Carribean_final_mpd_combined.csv")

#get final csv for Carribean mntd
Carribean_final_mntd_combined<-rbind(Carribean_final_mntd_geog,Carribean_final_Bark_mntd,Carribean_final_Serotiny_mntd,Carribean_final_Grass_mntd,Carribean_final_Resprout_mntd,Carribean_final_Pyrophillic_mntd,Carribean_final_More_than_one_adaptation_mntd,Carribean_final_Pyrophobic_mntd)

#write to csv

write.csv(Carribean_final_mntd_combined, "Outfile_Carribean_final_mntd_combined.csv")



#gives row names
rownames(bigcsv)

#gives col names
colnames(bigcsv)

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App
Laur_App.matrix[2,] <- bigcsv$Laur_App

Laur_App.matrix

length(rownames(bigcsv))
Laur_App <- rownames(bigcsv)[bigcsv$Laur_App==1]

Laur_App

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Laur_App.tre <- keep.tip(conif.tree, Laur_App)
plotTree(Laur_App.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Laur_App.matrix, dis = cophenetic(Laur_App.tre))
Laur_App_geog_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(conif.tree))
Laur_App_geog_mpd<-Laur_App_geog_mpd[-1,]
Test_name<-("Laur_App_geog_mpd")
Laur_App_final_mpd_geog<-cbind(Test_name,Laur_App_geog_mpd)


mntd(samp = Laur_App.matrix, dis = cophenetic(Laur_App.tre))
Laur_App_geog_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(conif.tree))
Laur_App_geog_mntd<-Laur_App_geog_mntd[-1,]
Test_name<-("Laur_App_geog_mntd")
Laur_App_final_mntd_geog<-cbind(Test_name,Laur_App_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$Bark
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$Bark

Laur_App_Bark_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Bark_mpd<-Laur_App_Bark_mpd[-1,]
Test_name<-("Laur_App_Bark_mpd")
Laur_App_final_Bark_mpd<-cbind(Test_name,Laur_App_Bark_mpd)


Laur_App_Bark_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Bark_mntd<-Laur_App_Bark_mntd[-1,]
Test_name<-("Laur_App_Bark_mntd")
Laur_App_final_Bark_mntd<-cbind(Test_name,Laur_App_Bark_mntd)

#Serotiny


Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$Serotiny
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$Serotiny

Laur_App_Serotiny_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Serotiny_mpd<-Laur_App_Serotiny_mpd[-1,]
Test_name<-("Laur_App_Serotiny_mpd")
Laur_App_final_Serotiny_mpd<-cbind(Test_name,Laur_App_Serotiny_mpd)


Laur_App_Serotiny_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Serotiny_mntd<-Laur_App_Serotiny_mntd[-1,]
Test_name<-("Laur_App_Serotiny_mntd")
Laur_App_final_Serotiny_mntd<-cbind(Test_name,Laur_App_Serotiny_mntd)

#Grass

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$Grass
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$Grass

Laur_App_Grass_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Grass_mpd<-Laur_App_Grass_mpd[-1,]
Test_name<-("Laur_App_Grass_mpd")
Laur_App_final_Grass_mpd<-cbind(Test_name,Laur_App_Grass_mpd)

Laur_App_Grass_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Grass_mntd<-Laur_App_Grass_mntd[-1,]
Test_name<-("Laur_App_Grass_mntd")
Laur_App_final_Grass_mntd<-cbind(Test_name,Laur_App_Grass_mntd)


#Resprout

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$Resprout
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$Resprout

Laur_App_Resprout_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Resprout_mpd<-Laur_App_Resprout_mpd[-1,]
Test_name<-("Laur_App_Resprout_mpd")
Laur_App_final_Resprout_mpd<-cbind(Test_name,Laur_App_Resprout_mpd)


Laur_App_Resprout_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Resprout_mntd<-Laur_App_Resprout_mntd[-1,]
Test_name<-("Laur_App_Resprout_mntd")
Laur_App_final_Resprout_mntd<-cbind(Test_name,Laur_App_Resprout_mntd)

#More_than_one_adaptation

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$More_than_one_adaptation
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$More_than_one_adaptation

Laur_App_More_than_one_adaptation_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_More_than_one_adaptation_mpd<-Laur_App_More_than_one_adaptation_mpd[-1,]
Test_name<-("Laur_App_More_than_one_adaptation_mpd")
Laur_App_final_More_than_one_adaptation_mpd<-cbind(Test_name,Laur_App_More_than_one_adaptation_mpd)


Laur_App_More_than_one_adaptation_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_More_than_one_adaptation_mntd<-Laur_App_More_than_one_adaptation_mntd[-1,]
Test_name<-("Laur_App_More_than_one_adaptation_mntd")
Laur_App_final_More_than_one_adaptation_mntd<-cbind(Test_name,Laur_App_More_than_one_adaptation_mntd)

#Pyrophillic

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$Pyrophillic
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$Pyrophillic

Laur_App_Pyrophillic_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Pyrophillic_mpd<-Laur_App_Pyrophillic_mpd[-1,]
Test_name<-("Laur_App_Pyrophillic_mpd")
Laur_App_final_Pyrophillic_mpd<-cbind(Test_name,Laur_App_Pyrophillic_mpd)


Laur_App_Pyrophillic_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Pyrophillic_mntd<-Laur_App_Pyrophillic_mntd[-1,]
Test_name<-("Laur_App_Pyrophillic_mntd")
Laur_App_final_Pyrophillic_mntd<-cbind(Test_name,Laur_App_Pyrophillic_mntd)

#Pyrophobic

Laur_App.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Laur_App.matrix) <- rownames(bigcsv)
Laur_App.matrix[1,] <- bigcsv$Laur_App & bigcsv$Pyrophobic
Laur_App.matrix[2,] <- bigcsv$Laur_App & bigcsv$Pyrophobic

Laur_App_Pyrophobic_mpd<-ses.mpd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Pyrophobic_mpd<-Laur_App_Pyrophobic_mpd[-1,]
Test_name<-("Laur_App_Pyrophobic_mpd")
Laur_App_final_Pyrophobic_mpd<-cbind(Test_name,Laur_App_Pyrophobic_mpd)


Laur_App_Pyrophobic_mntd<-ses.mntd(samp = Laur_App.matrix, dis =  cophenetic(Laur_App.tre))
Laur_App_Pyrophobic_mntd<-Laur_App_Pyrophobic_mntd[-1,]
Test_name<-("Laur_App_Pyrophobic_mntd")
Laur_App_final_Pyrophobic_mntd<-cbind(Test_name,Laur_App_Pyrophobic_mntd)




#get final csv for Laur_App mpd
Laur_App_final_mpd_combined<-rbind(Laur_App_final_mpd_geog,Laur_App_final_Bark_mpd,Laur_App_final_Serotiny_mpd,Laur_App_final_Grass_mpd,Laur_App_final_Resprout_mpd,Laur_App_final_Pyrophillic_mpd,Laur_App_final_More_than_one_adaptation_mpd,Laur_App_final_Pyrophobic_mpd)

#write to csv

write.csv(Laur_App_final_mpd_combined, "Outfile_Laur_App_final_mpd_combined.csv")

#get final csv for Laur_App mntd
Laur_App_final_mntd_combined<-rbind(Laur_App_final_mntd_geog,Laur_App_final_Bark_mntd,Laur_App_final_Serotiny_mntd,Laur_App_final_Grass_mntd,Laur_App_final_Resprout_mntd,Laur_App_final_Pyrophillic_mntd,Laur_App_final_More_than_one_adaptation_mntd,Laur_App_final_Pyrophobic_mntd)

#write to csv

write.csv(Laur_App_final_mntd_combined, "Outfile_Laur_App_final_mntd_combined.csv")

#Boreal EW

conif.tree<-read.tree("2018_North_America.tre")
conif.tree

plotTree(conif.tree, fsize=0.3)

bigcsv<-read.csv("Absolute_true_master_csv_For_supplement.csv",header=TRUE,row.names=1)

rownames(bigcsv)

#gives col names
colnames(bigcsv)

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW

Boreal_EW.matrix

length(rownames(bigcsv))
Boreal_EW <- rownames(bigcsv)[bigcsv$Boreal_EW==1]

Boreal_EW

keep.tip <- function(tree,tip) drop.tip(tree,setdiff(tree$tip.label,tip))

Boreal_EW.tre <- keep.tip(conif.tree, Boreal_EW)
plotTree(Boreal_EW.tre, fsize=0.5)
obj<-geo.legend()

mpd(samp = Boreal_EW.matrix, dis = cophenetic(Boreal_EW.tre))
Boreal_EW_geog_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(conif.tree))
Boreal_EW_geog_mpd<-Boreal_EW_geog_mpd[-1,]
Test_name<-("Boreal_EW_geog_mpd")
Boreal_EW_final_mpd_geog<-cbind(Test_name,Boreal_EW_geog_mpd)


mntd(samp = Boreal_EW.matrix, dis = cophenetic(Boreal_EW.tre))
Boreal_EW_geog_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(conif.tree))
Boreal_EW_geog_mntd<-Boreal_EW_geog_mntd[-1,]
Test_name<-("Boreal_EW_geog_mntd")
Boreal_EW_final_mntd_geog<-cbind(Test_name,Boreal_EW_geog_mntd)



#for traits

#Bark

#Set up the new matrix

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$Bark
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$Bark

Boreal_EW_Bark_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Bark_mpd<-Boreal_EW_Bark_mpd[-1,]
Test_name<-("Boreal_EW_Bark_mpd")
Boreal_EW_final_Bark_mpd<-cbind(Test_name,Boreal_EW_Bark_mpd)


Boreal_EW_Bark_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Bark_mntd<-Boreal_EW_Bark_mntd[-1,]
Test_name<-("Boreal_EW_Bark_mntd")
Boreal_EW_final_Bark_mntd<-cbind(Test_name,Boreal_EW_Bark_mntd)

#Serotiny


Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$Serotiny
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$Serotiny

Boreal_EW_Serotiny_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Serotiny_mpd<-Boreal_EW_Serotiny_mpd[-1,]
Test_name<-("Boreal_EW_Serotiny_mpd")
Boreal_EW_final_Serotiny_mpd<-cbind(Test_name,Boreal_EW_Serotiny_mpd)


Boreal_EW_Serotiny_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Serotiny_mntd<-Boreal_EW_Serotiny_mntd[-1,]
Test_name<-("Boreal_EW_Serotiny_mntd")
Boreal_EW_final_Serotiny_mntd<-cbind(Test_name,Boreal_EW_Serotiny_mntd)

#Grass

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$Grass
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$Grass

Boreal_EW_Grass_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Grass_mpd<-Boreal_EW_Grass_mpd[-1,]
Test_name<-("Boreal_EW_Grass_mpd")
Boreal_EW_final_Grass_mpd<-cbind(Test_name,Boreal_EW_Grass_mpd)

Boreal_EW_Grass_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Grass_mntd<-Boreal_EW_Grass_mntd[-1,]
Test_name<-("Boreal_EW_Grass_mntd")
Boreal_EW_final_Grass_mntd<-cbind(Test_name,Boreal_EW_Grass_mntd)


#Resprout

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$Resprout
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$Resprout

Boreal_EW_Resprout_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Resprout_mpd<-Boreal_EW_Resprout_mpd[-1,]
Test_name<-("Boreal_EW_Resprout_mpd")
Boreal_EW_final_Resprout_mpd<-cbind(Test_name,Boreal_EW_Resprout_mpd)


Boreal_EW_Resprout_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Resprout_mntd<-Boreal_EW_Resprout_mntd[-1,]
Test_name<-("Boreal_EW_Resprout_mntd")
Boreal_EW_final_Resprout_mntd<-cbind(Test_name,Boreal_EW_Resprout_mntd)

#More_than_one_adaptation

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$More_than_one_adaptation
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$More_than_one_adaptation

Boreal_EW_More_than_one_adaptation_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_More_than_one_adaptation_mpd<-Boreal_EW_More_than_one_adaptation_mpd[-1,]
Test_name<-("Boreal_EW_More_than_one_adaptation_mpd")
Boreal_EW_final_More_than_one_adaptation_mpd<-cbind(Test_name,Boreal_EW_More_than_one_adaptation_mpd)


Boreal_EW_More_than_one_adaptation_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_More_than_one_adaptation_mntd<-Boreal_EW_More_than_one_adaptation_mntd[-1,]
Test_name<-("Boreal_EW_More_than_one_adaptation_mntd")
Boreal_EW_final_More_than_one_adaptation_mntd<-cbind(Test_name,Boreal_EW_More_than_one_adaptation_mntd)

#Pyrophillic

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$Pyrophillic
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$Pyrophillic

Boreal_EW_Pyrophillic_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Pyrophillic_mpd<-Boreal_EW_Pyrophillic_mpd[-1,]
Test_name<-("Boreal_EW_Pyrophillic_mpd")
Boreal_EW_final_Pyrophillic_mpd<-cbind(Test_name,Boreal_EW_Pyrophillic_mpd)


Boreal_EW_Pyrophillic_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Pyrophillic_mntd<-Boreal_EW_Pyrophillic_mntd[-1,]
Test_name<-("Boreal_EW_Pyrophillic_mntd")
Boreal_EW_final_Pyrophillic_mntd<-cbind(Test_name,Boreal_EW_Pyrophillic_mntd)

#Pyrophobic

Boreal_EW.matrix <- matrix(0, 2, nrow(bigcsv))
colnames(Boreal_EW.matrix) <- rownames(bigcsv)
Boreal_EW.matrix[1,] <- bigcsv$Boreal_EW & bigcsv$Pyrophobic
Boreal_EW.matrix[2,] <- bigcsv$Boreal_EW & bigcsv$Pyrophobic

Boreal_EW_Pyrophobic_mpd<-ses.mpd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Pyrophobic_mpd<-Boreal_EW_Pyrophobic_mpd[-1,]
Test_name<-("Boreal_EW_Pyrophobic_mpd")
Boreal_EW_final_Pyrophobic_mpd<-cbind(Test_name,Boreal_EW_Pyrophobic_mpd)


Boreal_EW_Pyrophobic_mntd<-ses.mntd(samp = Boreal_EW.matrix, dis =  cophenetic(Boreal_EW.tre))
Boreal_EW_Pyrophobic_mntd<-Boreal_EW_Pyrophobic_mntd[-1,]
Test_name<-("Boreal_EW_Pyrophobic_mntd")
Boreal_EW_final_Pyrophobic_mntd<-cbind(Test_name,Boreal_EW_Pyrophobic_mntd)




#get final csv for Boreal_EW mpd
Boreal_EW_final_mpd_combined<-rbind(Boreal_EW_final_mpd_geog,Boreal_EW_final_Bark_mpd,Boreal_EW_final_Serotiny_mpd,Boreal_EW_final_Grass_mpd,Boreal_EW_final_Resprout_mpd,Boreal_EW_final_Pyrophillic_mpd,Boreal_EW_final_More_than_one_adaptation_mpd,Boreal_EW_final_Pyrophobic_mpd)

#write to csv

write.csv(Boreal_EW_final_mpd_combined, "Outfile_Boreal_EW_final_mpd_combined.csv")

#get final csv for Boreal_EW mntd
Boreal_EW_final_mntd_combined<-rbind(Boreal_EW_final_mntd_geog,Boreal_EW_final_Bark_mntd,Boreal_EW_final_Serotiny_mntd,Boreal_EW_final_Grass_mntd,Boreal_EW_final_Resprout_mntd,Boreal_EW_final_Pyrophillic_mntd,Boreal_EW_final_More_than_one_adaptation_mntd,Boreal_EW_final_Pyrophobic_mntd)

#write to csv

write.csv(Boreal_EW_final_mntd_combined, "Outfile_Boreal_EW_final_mntd_combined.csv")









