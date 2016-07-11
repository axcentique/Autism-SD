setwd("~/Desktop/Classes/IND E 599 Analytical Methods/Autism Project");
library(VGAM)
library(MASS)
library(glmulti)

autism<-read.csv("autism-poly-n.csv",header=TRUE,na.string=c("","N/A"))
# autism<-read.csv("original data coded.csv",header=TRUE,na.string=c("","N/A"))
autism <- autism[,-which(names(autism) %in% c("ID","Dx","ADI.DxSocial","ADI.DxComm","ADI.DxRep","ADI.CuSocial","ADI.CuComm","ADI.CuRep","ADOS.Module","ADOS.Comm","ADOS.Social","ADOS.Dx","ADOS.Class","ADOS.SevScore","DSM.IV"))]

#autism$Dx <- ifelse(autism$Dx == "ASD",1,2)
#autism$gender <- ifelse(autism$gender == "M",1,2)
#autism$handedness <- ifelse(autism$handedness == "R",1,2)

autism$Dx2 <- ifelse(autism$Dx == 1,1,0)

#autism$ComQ_int <- as.integer(autism$ComQ)
autismglm <- glm(Dx2~Age+BAI+WMSfacesImm_â..â..Raw, data = autism, family = binomial())
summary(autismglm)

# model variable selection
d <- glmulti(Dx~.,data=autism,level=1,fitfunc=lm,method="g",confsetsize=1024,maxsize=5)



autismglm2 <- lm(Dx~Age+WMSfacesImm_â..â..Raw+ComQ, data = autism)
summary(autismglm2)



autism$groups <- c(1,2,3,1,1,3,3,3,3,1,1,1,1,3,3,3,3,3,3,3,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)

autism$gender <- ifelse(autism$gender == 0,1,0)


d <- glmulti(groups~.,data=autism,level=1,fitfunc=glm,method="g",confsetsize=1024,maxsize=5)

mlogit<- vglm(groups~BDI.II+WASI.FullScaleIQ+ComQ, family=multinomial(), data=autism)
summary(mlogit)


groups~gender+BDI.II+WASI.FullScaleIQ+WMSfacesImm_â..â..Raw+ComQ





autism.asd <- autism[1:23,]

autism.asd$groups <- as.numeric(autism.asd$groups)

d <- glmulti(groups~.,data=autism.asd,level=1,fitfunc=glm,method="g",confsetsize=1024,maxsize=5)

# WASI.FullScaleIQ+WASI.VerbalIQ+WASI.PerfIQ+WMSfacesImm_â..â..Raw+WMSfacesImm.Scaled

# switching groups 3 and 4
autism.asd$groups <- c(1,2,4,1,1,4,4,4,4,1,1,1,1,4,4,4,4,4,4,4,3,3,3)

mlogit<- vglm(groups~WASI.FullScaleIQ+WASI.VerbalIQ+WASI.PerfIQ+WMSfacesImm_â..â..Raw, family=multinomial(), data=autism.asd)
summary(mlogit)











