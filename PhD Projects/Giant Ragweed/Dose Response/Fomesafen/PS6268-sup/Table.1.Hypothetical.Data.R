####################################################################################################

library(doBy)
library(drc)
library(lattice)

setwd("C:\\Users\\Admin\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")
setwd("C:\\Users\\keshtkar\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")


"HerbSOILout2.txt"

###===============================================================
###   Importing dtata
###===============================================================


data.set<- read.table("Table.1.Hypothetical.Data.txt", header = T)


###===============================================================
###   Error #1: Fitting multiple dose-response models individually
###==============================================================


##========  simultaneously fitted (population A & B)
MODEL.simultaneously<- drm(Response~Dose, Biotype, fct=LL.4(), data=subset( data.set, Biotype!="A"))
plot(MODEL.simultaneously)
summary(MODEL.simultaneously)


###======= individually fitted (population A)
MODEL.Indiv.A<- drm(Response~Dose, Biotype, fct=LL.4(), data=subset(data.set, Biotype=="B"))
plot(MODEL.Indiv.A)
summary(MODEL.Indiv.A)

###======= individually fitted (population B)
MODEL.Indiv.B<- drm(Response~Dose, Biotype, fct=LL.4(), data=subset(data.set, Biotype=="C"))
plot(MODEL.Indiv.B)
summary(MODEL.Indiv.B)










