
library(drc)
library(doBy)
library(plotrix) # Place a "break" mark on an axis


setwd("C:\\Users\\Admin\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")
setwd("C:\\Users\\keshtkar\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")



###===========================================================
###  Error #3: Reporting RI and ED 50 s without standard error 
###===========================================================

DATA.RI<- read.table("Fig.4.A-Resistance Index.txt", header = T)
head(DATA.RI); tail(DATA.RI)


mean.r<- summaryBy(Response~ DOSE+ BIO, data =subset( DATA.RI, BIO=="R" ))
mean.S<- summaryBy(Response~ DOSE+ BIO, data =subset( DATA.RI, BIO=="S" ))

model<- drm(Response~DOSE, BIO, fct = LL.3(),  data = DATA.RI )
modelFit(model)
summary(model)
plot(model)

mselect(model, list(LL.4(), LL.5(), BC.5()))

EDcomp(model, c(50,50))

###===========================================================
###=== Error #5: Models with a negative lower limit 
###===========================================================

Negative.C.Value1<- read.table("Fig.4B-Problem-5-Negative-C-value.txt", header = T);head(Negative.C.Value1); tail(Negative.C.Value1)
Negative.C.Value<- subset(Negative.C.Value1, Bio==2)# Negative.C.Value<- subset(Negative.C.Value1, Bio=="ID-47")
head(Negative.C.Value); tail(Negative.C.Value)

###=========================================================== Model LL.4
drm.4 <- drm(FW~Dos.a.i.ha, Bio,fct=LL.4(),   
             data=Negative.C.Value,  na.action=na.omit)
summary(drm.4)
modelFit(drm.4)
plot(drm.4, ylim=c(-15, 32), xlim=c(0, 200), col = 2, lty = 1)

###=========================================================== Model LL.3
drm.3 <- drm(FW~Dos.a.i.ha, Bio,fct=LL.3(),   
             data=Negative.C.Value,  na.action=na.omit)
summary(drm.3)
modelFit(drm.3)
anova(drm.3, drm.4)# IT IS ok TO REDUCE MODLE

plot(drm.3, add = T, col = 1, ylim=c(-10, 110), xlim=c(0, 200))


summary(drm.4)
summary(drm.3)

anova(drm.3, drm.4)

mselect(drm.4, list(LL.3(), LL.5()))


####===============================================================
####===============================================================
###==================== Making  Figure 4 A&B
####===============================================================
####===============================================================

graphics.off()
tiff(file="Fig.4.tiff",width=5,height=8,units = "cm",res=800, pointsize=4)

par(mfrow= c(2,1), oma=c(3,3, 0,0), mar= c(2,2,1,1),mgp=c(3, 1.1, 0))


library(plotrix) # Place a "break" mark on an axis

####===============================================================
####===================================================== Figure A
####===============================================================
plot(model, xlim = c(0, 300),  xlab="", ylab="", 
     pch = c( 19, 1), lwd=1.3, lty = c(4,1), col=c("#E86464", "#838EAC"),
     cex.axis = 1.4, cex.legend = 1.4, legendPos = c(290,80))
axis.break(1, .2, style = "slash")


text(200, 94, "A", cex =1.4 )


text(65, 50, "RI = 8.8 (± 4.65) ", cex=1.3)#29, 77
#text(80, 80, "ns", font=3)
text(52,43, " P-value =  0.103", cex=1.4, box(), font=3)#family="B",


mtext(" Response",  outer = F, cex = 1.6, side=2, line=3)#, family="A")


####===============================================================
####===================================================== Figure B
####===============================================================
plot(drm.4,          ylim=c(-5, 31), xlim=c(0, 300),  lty = 4, lwd=1.3, xlab="", ylab="", 
     col="#E86464", cex.axis = 1.4, yt=c(-5, 0, 10, 20, 30))
plot(drm.3, add = T, ylim=c(-10, 40), xlim=c(0, 300), lty = 1, lwd=1.3, col= "#838EAC")
axis.break(1, .2, style = "slash")

text(200, 28, "B", cex =1.4 )

############################
segments(.1,  0, 200, 0, lty = 3,  lwd = .7  ,  col = "gray48");
segments(.1, -2.5, 200, -2.5, lty = 3, lwd = .7, col = "gray48")

text(.15, 1,  "LL.3" ,font = 1, cex = 1, col = 4)
text(.15, -1.5, "LL.4" ,font = 1, cex = 1, col = 2)

text(.45,  1, "C = 0" ,font = 3, cex = 1)
text(.55, -1.5, "C = -2.5" ,font = 3, cex = 1)


text(5,  1,  "ED50 = 7.04" ,font = 3, cex = 1)
text(5, -1.5, "ED50 = 7.70" ,font = 3, cex = 1)

#text(8, 4, cex = .8, bquote(paste('ED'['50']*' = ')), font = 3)

#mtext(expression(paste("Fresh weight  " ("g pot"^ "-1"))), 
      outer = F, cex = 1.6, side=2, line=2.8, )#, family="A")

mtext(" Fresh weight (g)",  outer = F, cex = 1.6, side=2, line=3)#, family="A")

mtext(expression(paste("Log dose " ("g a.i. ha"^ "-1"))), outer = F, cex =1.6,
      side=1, line=3.5)#, family="A")


