#library(devtools)
#install_github("Rjacobucci/longRPart2")
library(longRPart2)

#MAC
setwd("/Users/kjgrimm/Google Drive/PhD in Quantitative Psych/Projects/Nonlinear Longitudinal Recursive Partitioning/Illustrative Example")

#WORK PC
setwd("C:/Users/gstegman/Google Drive/PhD in Quantitative Psych/Projects/Nonlinear Longitudinal Recursive Partitioning/Illustrative Example")


ECLSK_1000 = read.table("Subset1000_READING_PREDICTORS_AGE.dat",
                        na.strings = ".")


colnames(ECLSK_1000) = c("CHILDID",
                         "AGE",
                         "OCCASION",
                         "GENDER",
                         "W1POVRTY",
                         "RACE",
                         "P1FIRKDG",
                         "C1FMOTOR",
                         "C1GMOTOR",
                         "T1CONTRO" ,
                         "T1INTERP",
                         "T1EXTERN" ,
                         "T1INTERN",
                         "READING")

#GENDER 1 is MALE
#RACE: 1 white non-hispanic, 2 black or af. am., 3 hispanic race specified, 4 hispanic race not specified

#Clean data: delete NAs in AGE and READING, and make sure at least ONE predictor has a value.
ECLSK_1000$RACE = as.factor(ECLSK_1000$RACE)

ECLSK_1000_noNA = ECLSK_1000

ECLSK_1000_noNA$age_5 = ECLSK_1000_noNA$AGE-5

ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$GENDER),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$W1POVRTY),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$C1GMOTOR),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$P1FIRKDG),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$C1FMOTOR),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$T1CONTRO),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$T1INTERP),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$T1EXTERN),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$T1INTERN),]
ECLSK_1000_noNA = ECLSK_1000_noNA[!is.na(ECLSK_1000_noNA$RACE),]

#Split TRAINING vs TESTING
#TRAINING = ECLSK_1000_noNA[0:2500,]
#TESTING = ECLSK_1000_noNA[2501:3061,]

#TRAINING = ECLSK_1000_noNA[complete.cases(ECLSK_1000_noNA), ]
TRAINING = ECLSK_1000_noNA

LONGTREEResults_ECLSK = lrp(method="nlme",
                      nlme.model=READING~b0i+b1i*(1-exp(-b2*age_5)),
                      fixedFormula=b0i+b1i+b2~1,
                      rPartFormula = ~ GENDER + RACE,
                      group= ~ CHILDID,
                      randomFormula=b0i+b1i~1,
                      data=TRAINING,
                      start=c(b0i = -2, b1i = 4, b2 = 1))

LONGTREEResults_ECLSK$rpart_out

lrp2Plot(LONGTREEResults_ECLSK)  #Works... Labels: 2, 4, 5
lrp2Plot_Ross(LONGTREEResults_ECLSK) #Does not work
plot.lrp_Ross(LONGTREEResults_ECLSK) #Works... Labels: 2, 6, 7

#########################################################################
LRP_lme_results = lrp(method="lme",
                      fixedFormula=READING ~ age_5,
                      rPartFormula = ~ GENDER + RACE,
                      randomFormula=~1|CHILDID,
                      data=TRAINING,
                      min.dev = 20)

LRP_lme_results$rpart_out

