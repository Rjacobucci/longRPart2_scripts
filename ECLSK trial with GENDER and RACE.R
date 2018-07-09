setwd("/Users/rjacobuc/Documents/GitHub/longRPart2_scripts")



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

TRAINING = ECLSK_1000_noNA

LONGTREEResults = lrp(method="nlme",
                             nlme.model=READING~b0i+b1i*(1-exp(-b2*age_5)),
                             fixedFormula=b0i+b1i+b2~1,
                             rPartFormula = ~ GENDER + RACE,
                             group= ~ CHILDID,
                             randomFormula=b0i+b1i~1,
                             data=TRAINING,
                             min.dev=5,
                             start=c(-2,4,1))

LONGTREEResults$rpart_out
summary(LONGTREEResults)

plot(LONGTREEResults)

lrp2Plot(LONGTREEResults)
lrpPlot(LONGTREEResults)
lrpTreePlot(LONGTREEResults)
#Results
#> LONGTREEResults$rpart_out
#n= 3061

#node), split, n, deviance, yval
#* denotes terminal node

#1) root 3061 2416.4000 1
#2) RACE=2,3 830  737.8161 1 *
#  3) RACE=1,4 2231 1634.2750 1
#  6) GENDER< 0.5 1120  699.0023 1 *
#  7) GENDER>=0.5 1111  904.1908 1 *
#  > 2416.4 - (737.8161+1634.275)
#[1] 44.3089
#> 1634.275-(699.0023+904.1908)
#[1] 31.0819


LONGTREEResults2 = lrp2function(method="nlme",
                             nlme.model=READING~b0i+b1i*(1-exp(-b2*age_5)),
                             fixedFormula=b0i+b1i+b2~1,
                             rPartFormula = ~ GENDER + RACE,
                             group= ~ CHILDID,
                             randomFormula=b0i+b1i~1,
                             data=TRAINING,
                             start=c(-2,4,1),
                             min.dev = 20)

LONGTREEResults2$rpart_out

#> LONGTREEResults2$rpart_out
#n= 3061

#node), split, n, deviance, yval
#* denotes terminal node

#1) root 3061 2416.4000 1
#2) RACE=2,3 830  737.8161 1 *
#  3) RACE=1,4 2231 1634.2750 1
#  6) GENDER< 0.5 1120  699.0023 1 *
#  7) GENDER>=0.5 1111  904.1908 1 *


LONGTREEResults3 = lrp2function(method="nlme",
                                nlme.model=READING~b0i+b1i*(1-exp(-b2*age_5)),
                                fixedFormula=b0i+b1i+b2~1,
                                rPartFormula = ~ GENDER + RACE,
                                group= ~ CHILDID,
                                randomFormula=b0i+b1i~1,
                                data=TRAINING,
                                start=c(-2,4,1),
                                min.dev = 40)

LONGTREEResults3$rpart_out

#> LONGTREEResults3$rpart_out
#n= 3061

#node), split, n, deviance, yval
#* denotes terminal node

#1) root 3061 2416.4000 1
#2) RACE=2,3 830  737.8161 1 *
#  3) RACE=1,4 2231 1634.2750 1 *


    LONGTREEResults4 = lrp2function(method="nlme",
                                    nlme.model=READING~b0i+b1i*(1-exp(-b2*age_5)),
                                    fixedFormula=b0i+b1i+b2~1,
                                    rPartFormula = ~ GENDER + RACE,
                                    group= ~ CHILDID,
                                    randomFormula=b0i+b1i~1,
                                    data=TRAINING,
                                    start=c(-2,4,1),
                                    min.dev = 50)

    LONGTREEResults4$rpart_out

#> LONGTREEResults4$rpart_out
#n= 4151

#   node), split, n, deviance, yval
#* denotes terminal node

#1) root 4151 2416.4 1 *
