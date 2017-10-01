library(longRPart2)

model.0 = nlme(y~b0i+b1i*time,
               data=ex.data.3,
               fixed=b0i+b1i~1,
               random=b0i+b1i~1,
               group=~id,
               start=c(10,5))


lcart.mod1 <- lrp(method="nlme",
                         nlme.model=y~b0i+b1i*time,
                         fixedFormula=b0i+b1i~1,
                         rPartFormula = ~ z,
                         group= ~ id,
                         randomFormula=b0i+b1i~1,
                         data=ex.data.3,
                         start=c(10,5))

summary(lcart.mod1)
plot(lcart.mod1)
lrp2Plot(lcart.mod1)

