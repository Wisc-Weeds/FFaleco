library(drc)
library(plotrix)

setwd("C:\\Users\\Admin\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")
setwd("C:\\Users\\keshtkar\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")


### Importing dtata
CD.diffrent.I= read.table("Fig.3-CD.diffrent.I.txt", header = T)
CD.diffrent.II= read.table("Fig.3-CD.diffrent.II.txt", header = T)


head(CD.diffrent.I)
head(CD.diffrent.II)



#####===================================================
##### Curves with diffrent Upper limits (U): Fig.3 A,B,C
#####===================================================

#################============ Raw data

MODEL.CD.I<- drm(Response~Dose, Biotype, fct=LL.4(), data=CD.diffrent.I)
plot(MODEL.CD.I, xlim=c(0, 300), ylim=c(0,70), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.CD.I)
backfit(MODEL.CD.I)

## Calculate THE AED50
##  fOR S population; D/2 ===> 30.09997 /2= 15.04998
##  fOR R population; D/2 ===> 60.20000/2= 30.1
ED(MODEL.CD.I, c(15.04998, 30.1), type  =  "absolute")

#################============ Zero-one Scalled

REL.R<-with(CD.diffrent.I,(Response-MODEL.CD.I$coeff[4])/(MODEL.CD.I$coeff[6]-MODEL.CD.I$coeff[4]))
REL.S<-with(CD.diffrent.I,(Response-MODEL.CD.I$coeff[3])/(MODEL.CD.I$coeff[5]-MODEL.CD.I$coeff[3]))


CD.diffrent.I$REL <-with(CD.diffrent.I,ifelse((Biotype )=="S",REL.S, REL.R))
#

MODEL.CD.I.Rel<- drm(REL~Dose, Biotype, fct=LL.4(), data=CD.diffrent.I)
plot(MODEL.CD.I.Rel, xlim=c(0, 300), ylim=c(0,1), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.CD.I.Rel)
backfit(MODEL.CD.I.Rel)


#################============ Relative Scalled

REL.r<-with(CD.diffrent.I,(Response*100/MODEL.CD.I$coeff[6]))
REL.s<-with(CD.diffrent.I,(Response*100/MODEL.CD.I$coeff[5]))


CD.diffrent.I$REL.100 <-with(CD.diffrent.I,ifelse((Biotype )=="S",REL.s, REL.r))
#

MODEL.CD.I.Rel.100<- drm(REL.100~Dose, Biotype, fct=LL.4(), data=CD.diffrent.I)
plot(MODEL.CD.I.Rel.100, xlim=c(0, 300), ylim=c(0,100), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.CD.I.Rel.100)
backfit(MODEL.CD.I.Rel.100)


#####===================================================
##### Curves with diffrent lower limits LU): Fig.3 D,E,F
#####===================================================

#################============ Raw data

MODEL.CD.II<- drm(Response~Dose, Biotype, fct=LL.4(), data=CD.diffrent.II)
plot(MODEL.CD.II, xlim=c(0, 300), ylim=c(0,70), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.CD.II)
backfit(MODEL.CD.II)


## Calculate THE AED50
##  fOR S population; D/2 ===> 60.19997 /2= 30.09999
##  fOR R population; D/2 ===> 30.09997/2= 15.04998
ED(MODEL.CD.II, c(30.09999, 15.04998), type  =  "absolute")


#################============ Zero-one Scalled

REL.R<-with(CD.diffrent.II,(Response-MODEL.CD.II$coeff[4])/(MODEL.CD.II$coeff[6]-MODEL.CD.II$coeff[4]))
REL.S<-with(CD.diffrent.II,(Response-MODEL.CD.II$coeff[3])/(MODEL.CD.II$coeff[5]-MODEL.CD.II$coeff[3]))


CD.diffrent.II$REL <-with(CD.diffrent.II,ifelse((Biotype )=="S",REL.S, REL.R))
#

MODEL.CD.II.Rel<- drm(REL~Dose, Biotype, fct=LL.4(), data=CD.diffrent.II)
plot(MODEL.CD.II.Rel, xlim=c(0, 300), ylim=c(0,1), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.CD.II.Rel)
backfit(MODEL.CD.II.Rel)


#################============ Relative Scalled

REL.r<-with(CD.diffrent.II,(Response*100/MODEL.CD.II$coeff[6]))
REL.s<-with(CD.diffrent.II,(Response*100/MODEL.CD.II$coeff[5]))


CD.diffrent.II$REL.100 <-with(CD.diffrent.II,ifelse((Biotype )=="S",REL.s, REL.r))
#

MODEL.CD.II.Rel.100<- drm(REL.100~Dose, Biotype, fct=LL.4(), data=CD.diffrent.II)
plot(MODEL.CD.II.Rel.100, xlim=c(0, 300), ylim=c(0,100), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.CD.II.Rel.100)
backfit(MODEL.CD.II.Rel.100)













####===============================================================
####===============================================================
###==================== Making  Figure 3 A-F
####===============================================================
####===============================================================


graphics.off()
tiff(file="Fig.3.New.tiff",width=13,height=16,units = "cm",res=800, pointsize=4)
par(mfrow= c(3,2), oma=c(6,8, .5,.5), mar= c(3,3,2,4),mgp=c(3, 2, 0))

library(plotrix) # Place a "break" mark on an axis

####===============================================================
####===================================================== Figure A
####===============================================================
MODEL.CD.I           #A
MODEL.CD.I.Rel.100   #B
MODEL.CD.I.Rel       #C

MODEL.CD.II           #D
MODEL.CD.II.Rel.100   #E
MODEL.CD.II.Rel       #E

summary(MODEL.CD.I)

ED(MODEL.CD.I, c(50))
ED(MODEL.CD.I, c(15.04998, 30.1), type  =  "absolute")



plot(MODEL.CD.I, xlim=c(0, 320), ylim=c(0,70), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4, lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0,16, 30,20, 40, 60),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, 65, "A", cex =3 )

segments(1,40, 50,40, col = "#E86464", lty = 3, lwd=1.4);
segments(1,16, 50,16, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,40, 50,16, col = "#E86464", lty = 3, lwd=1.5);
segments(50,16, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,16, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);

####===============================================================
####===================================================== Figure D
####===============================================================
summary(MODEL.CD.II)

ED(MODEL.CD.II, c(50))
ED(MODEL.CD.II, c(30.09999, 15.04998), type  =  "absolute")

plot(MODEL.CD.II, xlim=c(0, 320), ylim=c(0,70), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4, lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0, 21, 31, 40, 60),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, 65, "D", cex =3 )

segments(1,21, 50,21, col = "#E86464", lty = 3, lwd=1.4);
segments(1,31, 50,31, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,31, 50,21, col = "#838EAC", lty = 3, lwd=1.5);
segments(50,21, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,21, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);



####===============================================================
####===================================================== Figure B
####===============================================================

summary(MODEL.CD.I.Rel.100)

plot(MODEL.CD.I.Rel.100, xlim=c(0, 320), ylim=c(0,100), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4,lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0,20,40,80, 55, 66,100),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, 95, "B", cex = 3)
segments(1,66, 50,66, col = "#E86464", lty = 3, lwd=1.4);
segments(1,55, 50,55, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,55, 50,66, col = "#E86464", lty = 3, lwd=1.5);

segments(50,55, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,55, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);

####===============================================================
####===================================================== Figure E
####===============================================================
summary(MODEL.CD.II.Rel.100)

plot(MODEL.CD.II.Rel.100, xlim=c(0, 320), ylim=c(0,100), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4,lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0,20,40,60, 80, 52, 70,100),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, 95, "E", cex = 3)
segments(1,70, 50,70, col = "#E86464", lty = 3, lwd=1.4);
segments(1,52, 50,52, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,70, 50,52, col = "#E86464", lty = 3, lwd=1.5);
segments(50,52, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,52, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);






####===============================================================
####===================================================== Figure C
####===============================================================

summary(MODEL.CD.I.Rel)
plot(MODEL.CD.I.Rel, xlim=c(0, 320), ylim=c(0,1), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4, lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0, .5, 1),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, .95, "C", cex = 3)
segments(1,.5, 50,.5, col = "#E86464", lty = 1, lwd=1.5);
segments(1,.5, 50,.5, col = "#838EAC", lty = 4, lwd=1.5);

segments(50,-2, 50,.5, col = "#E86464", lty = 1, lwd=1.5);
segments(50,-2, 50,.5, col = "#838EAC", lty = 4, lwd=1.5);

####===============================================================
####===================================================== Figure F
####===============================================================
summary(MODEL.CD.II.Rel)

plot(MODEL.CD.II.Rel, xlim=c(0, 320), ylim=c(0,1), col = c("#E86464", "#838EAC"), 
     type = "none", legend = F, lwd=1.4, lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0, .5, 1),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, .95, "F", cex = 3)
segments(1,.5, 50,.5, col = "#E86464", lty = 1, lwd=1.5);
segments(1,.5, 50,.5, col = "#838EAC", lty = 4, lwd=1.5);

segments(50,-2, 50,.5, col = "#E86464", lty = 1, lwd=1.5);
segments(50,-2, 50,.5, col = "#838EAC", lty = 4, lwd=1.5);






mtext(expression(paste("Log dose "("g a.i. ha"^ "-1"))) , outer = TRUE, cex = 2.6, 
      side=1, line=4.5, at=.5)

mtext(expression(paste("Response "( "e.g. dry weight pot"^ "-1"))), 
      cex = 2.37, outer = TRUE, side = 2, line = 3.7, at=.84)#1.7

mtext(" Relative response (% of control) ", 
      cex = 2.37, outer = TRUE,  side=2, line=4.5, at=.5)

mtext(" Zero-one scalled response ", 
      cex = 2.37,outer = T,  side=2, line=4.5, at=.17)




