library(drc)


setwd("C:\\Users\\Admin\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")
setwd("C:\\Users\\keshtkar\\Dropbox\\Paper Review Fitness\\5. ED50\\Data Analysis & Figures")


### Importing dtata
D.diffrent= read.table("Fig.2(abc)-D.diffrent.txt", header = T)
C.diffrent= read.table("Fig.2(def)-C.diffrent.txt", header = T)


head(D.diffrent)
head(C.diffrent)

#####===================================================
##### Curves with diffrent Upper limits (U): Fig.2A,B,C
#####===================================================

#################============ Raw data

MODEL.D<- drm(Response.2~Dose, Biotype, fct=LL.4(), data=D.diffrent)
plot(MODEL.D, xlim=c(0, 300), ylim=c(0,70), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.D)
backfit(MODEL.D)

## Calculate THE AED50
##  fOR S population; D/2 ===> 60.19999 /2=30.09999
##  fOR R population; D/2 ===> 30.10000/2=15.05
ED(MODEL.D, c(30.09999,15.05), type  =  "absolute")



#################============ Zero-one Scalled

REL.R<-with(D.diffrent,(Response.2-MODEL.D$coeff[4])/(MODEL.D$coeff[6]-MODEL.D$coeff[4]))
REL.S<-with(D.diffrent,(Response.2-MODEL.D$coeff[3])/(MODEL.D$coeff[5]-MODEL.D$coeff[3]))


D.diffrent$REL <-with(D.diffrent,ifelse((Biotype )=="S",REL.S, REL.R))
#

MODEL.D.Rel<- drm(REL~Dose, Biotype, fct=LL.4(), data=D.diffrent)
plot(MODEL.D.Rel, xlim=c(0, 300), ylim=c(0,1), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.D.Rel)
backfit(MODEL.D.Rel)


#################============ Relative Scalled

REL.r<-with(D.diffrent,(Response.2*100/MODEL.D$coeff[6]))
REL.s<-with(D.diffrent,(Response.2*100/MODEL.D$coeff[5]))


D.diffrent$REL.100 <-with(D.diffrent,ifelse((Biotype )=="S",REL.s, REL.r))
#

MODEL.D.Rel.100<- drm(REL.100~Dose, Biotype, fct=LL.4(), data=D.diffrent)
plot(MODEL.D.Rel.100, xlim=c(0, 300), ylim=c(0,100), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.D.Rel.100)
backfit(MODEL.D.Rel.100)


#####===================================================
##### Curves with diffrent lower limits LU): Fig.2D,E,F
#####===================================================

#################============ Raw data

MODEL.C<- drm(Response~Dose, Biotype, fct=LL.4(), data=C.diffrent)
plot(MODEL.C, xlim=c(0, 300), ylim=c(0,70), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.C)
backfit(MODEL.C)



#################============ Zero-one Scalled

REL.R<-with(C.diffrent,(Response-MODEL.C$coeff[4])/(MODEL.C$coeff[6]-MODEL.C$coeff[4]))
REL.S<-with(C.diffrent,(Response-MODEL.C$coeff[3])/(MODEL.C$coeff[5]-MODEL.C$coeff[3]))


C.diffrent$REL <-with(C.diffrent,ifelse((Biotype )=="S",REL.S, REL.R))
#

MODEL.C.Rel<- drm(REL~Dose, Biotype, fct=LL.4(), data=C.diffrent)
plot(MODEL.C.Rel, xlim=c(0, 300), ylim=c(0,1), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.C.Rel)
backfit(MODEL.C.Rel)


#################============ Relative Scalled

REL.r<-with(C.diffrent,(Response*100/MODEL.C$coeff[6]))
REL.s<-with(C.diffrent,(Response*100/MODEL.C$coeff[5]))


C.diffrent$REL.100 <-with(C.diffrent,ifelse((Biotype )=="S",REL.s, REL.r))
#

MODEL.C.Rel.100<- drm(REL.100~Dose, Biotype, fct=LL.4(), data=C.diffrent)
plot(MODEL.C.Rel.100,  xlim=c(0, 300), ylim=c(0,100), col = c("#838EAC", "#E86464"), type = "none")
summary(MODEL.C.Rel.100)
backfit(MODEL.C.Rel.100)








####===============================================================
####===============================================================
###==================== Making  Figure 2 A-F
####===============================================================
####===============================================================
graphics.off()
tiff(file="Fig.2.New.tiff",width=13,height=16,units = "cm",res=800, pointsize=4)
par(mfrow= c(3,2), oma=c(6,8, .5,.5), mar= c(3,3,2,4),mgp=c(3, 2, 0))

library(plotrix) # Place a "break" mark on an axis

####===============================================================
####===================================================== Figure A
####===============================================================
MODEL.D           #A
MODEL.D.Rel.100   #B
MODEL.D.Rel       #C

MODEL.C           #D
MODEL.C.Rel.100   #E
MODEL.C.Rel       #E




summary(MODEL.D)

ED(MODEL.D, c(50))
ED(MODEL.D, c(15, 30), type = "absolute")

#segments(1,30, 55.4,30, col = "#838EAC", lty = 3);
#segments(1,15, 66,15, col = "#E86464", lty = 3);



plot(MODEL.D,  xlim=c(0, 320), ylim=c(0,70), col = c("#838EAC", '#E86464'), 
     type = "none", legend = F, lwd=1.4, lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0, 20, 40,35,60),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

#abline(h=72.752,  lty=1, col="white", lwd=3)
#abline(v=367,  lty=1, col=0, lwd=3)
text(280, 65, "A", cex =3 )
segments(1,35, 50,35, col = "#E86464", lty = 3, lwd=1.4);
segments(1,20, 50,20, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,35, 50,20, col = "#E86464", lty = 3, lwd=1.5);
segments(50,20, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,20, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);

####===============================================================
####===================================================== Figure D
####===============================================================
summary(MODEL.C)

ED(MODEL.C, c(50))
ED(MODEL.C, c(30), type = "absolute")

#segments(1,30, 55.4,30, col = "#838EAC", lty = 3);
#segments(1,15, 66,15, col = "#E86464", lty = 3);



plot(MODEL.C, xlim=c(0, 320), ylim=c(0,70), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4, lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0,20, 40, 35,60),
     cex.axis = 2.8)

axis.break(1, 12, style = "slash")

text(280, 65, "D", cex =3 )

segments(1,40, 50,40, col = "#E86464", lty = 3, lwd=1.4);
segments(1,35, 50,35, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,40, 50,35, col = "#E86464", lty = 3, lwd=1.5);

segments(50,35, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,35, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);



####===============================================================
####===================================================== Figure B
####===============================================================

summary(MODEL.D.Rel.100)
plot(MODEL.D.Rel.100,  xlim=c(0, 320), ylim=c(0,100), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4,lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0,20, 40,80, 58, 66,100),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, 95, "B", cex = 3)
segments(1,66, 50,66, col = "#838EAC", lty = 3, lwd=1.4);
segments(1,58, 50,58, col = "#E86464", lty = 3, lwd=1.4);


segments(50,66, 50,58, col = "#E86464", lty = 3, lwd=1.5);
segments(50,58, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,58, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);

####===============================================================
####===================================================== Figure E
####===============================================================
summary(MODEL.C.Rel.100)
plot(MODEL.C.Rel.100,  xlim=c(0, 320), ylim=c(0,100), col = c("#838EAC", "#E86464"), 
     type = "none", legend = F, lwd=1.4,lty = c(1,4),
     xt=c(0, 50, 200),
     yt=c(0,20,40,80, 58, 66,100),
     cex.axis = 2.8)
axis.break(1, 12, style = "slash")

text(280, 95, "E", cex = 3)
segments(1,66, 50,66, col = "#E86464", lty = 3, lwd=1.4);
segments(1,58, 50,58, col = "#838EAC", lty = 3, lwd=1.4);

segments(50,66, 50,58, col = "#E86464", lty = 3, lwd=1.5);

segments(50,58, 50,-2, col = "#E86464", lty = 1, lwd=1.5);
segments(50,58, 50,-2, col = "#838EAC", lty = 3, lwd=1.5);


####===============================================================
####===================================================== Figure C
####===============================================================
summary(MODEL.D.Rel)
plot(MODEL.D.Rel, xlim=c(0, 320), ylim=c(0,1), col = c("#838EAC", "#E86464"), 
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
summary(MODEL.C.Rel)
plot(MODEL.C.Rel, xlim=c(0, 320), ylim=c(0,1), col = c("#E86464", "#838EAC"), 
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


