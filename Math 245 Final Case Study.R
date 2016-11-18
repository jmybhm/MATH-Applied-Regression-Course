head(CerealData)

library(Hmisc)
na.patterns <- naclus(CerealData)
naplot(na.patterns, which = 'na per var')
CerealData$missSugar <- ifelse(is.na(CerealData$Sugars), 1, 0)
which(CerealData$missSugar == 1) #43
CerealData$missFibers <- ifelse(is.na(CerealData$Fibers), 1, 0)
which(CerealData$missFibers == 1) #42

#Take out 42,43
CerealData2 <- CerealData[-c(42,43),]

summary(CerealData$Calories)
sd(CerealData$Calories)
summary(CerealData$Sugar)
sd(CerealData$Sugar)
summary(CerealData$Fiber)
sd(CerealData$Fiber)
summary(CerealData$Protein)
sd(CerealData$Protein)

table(CerealData$Brand)
table(CerealData$Crunchy)
table(CerealData$NutritionAds)
table(CerealData$Character)
table(CerealData$Flavors)
table(CerealData$Response)

boxplot(Sugars~Brand, data=CerealData)
boxplot(Fibers~Brand, data=CerealData)
boxplot(Protein~Brand, data=CerealData)
boxplot(Calories~Brand, data=CerealData)

plot(Response~Sugars, data=CerealData, xlab="Sugars", ylab="Probability of Being in Top 10")
title("Top 10 against Sugars")
plot(Response~Fibers, data=CerealData, xlab="Fibers", ylab="Probability of Being in Top 10")
title("Top 10 against Fibers")
plot(Response~Protein, data=CerealData, xlab="Protein", ylab="Probability of Being in Top 10")
title("Top 10 against Protein")
plot(Response~Calories, data=CerealData, xlab="Calories", ylab="Probability of Being in Top 10")
title("Top 10 against Calories")

library(car)
scatterplotMatrix(CerealData[,-c(1,2,11,14,15)], diagonal="histogram", smooth=FALSE, reg.line=FALSE)

#check each variable to see if transformation is needed
hist(CerealData$Sugars)#okay
hist(CerealData$Fibers)
hist(log(CerealData$Fibers))#better
hist(CerealData$Protein)
hist(log(CerealData$Protein))#better
hist(CerealData$Calories)#okay

CerealData$logFiber <- log(CerealData$Fibers+0.5) #What should we do about this?
CerealData$logProtein <- log(CerealData$Protein)

CerealData2$logFiber <- log(CerealData2$Fibers+0.5) #What should we do about this?
CerealData2$logProtein <- log(CerealData2$Protein)

head(CerealData2)
scatterplotMatrix(CerealData2[,c(13,3,4,5,6,16,17,9,10,12)], diagonal="histogram", smooth=FALSE, reg.line=FALSE)
scatterplotMatrix(CerealData2[,c(13,5,6,16,17)], diagonal="histogram", smooth=FALSE, reg.line=FALSE)

-------------------------------------------
  #Start variable selection and outlier exclusion
  #Full model
cereal.glm <- glm(Response~NameType+Crunchy+Brand+Calories+Sugars+logFiber+logProtein+NutritionAds+
                      Character+Flavors+Calories:logFiber+Sugars:logFiber, 
                    data=CerealData2, family=binomial)
summary(cereal.glm)
scatterplotMatrix(CerealData2[,c(13,3,4,5,6,16,17,9,10,12)], diagonal = "none", reg.line = FALSE, smooth = FALSE, 
                  id.method = abs(rstudent(cereal.glm)), id.n = 3, id.col = "red") #too many variables, messy
plot(resid(cereal.glm, type="response"))
which(resid(cereal.glm, type="response") > 0.9)#62 
which(resid(cereal.glm, type="response") < -0.9)
plot(cereal.glm, which=4) #27
which(hatvalues(cereal.glm) > 2*14/77) #a lot

#First test model without #27,62
CerealData3 <- CerealData[-c(42,43,27,62), ]
CerealData3

cereal.glm2 <- glm(Response~NameType+Crunchy+Brand+Calories+Sugars+logFiber+logProtein+NutritionAds+
                      Character+Flavors+Calories:logFiber+Sugars:logFiber, 
                    data=CerealData3, family=binomial)
summary(cereal.glm2) #NOTTTT WORKING!!
plot(resid(cereal.glm2, type="response"))
which(resid(cereal.glm2, type="response") > 0.9)
which(resid(cereal.glm2, type="response") < -0.9)
plot(cereal.glm2, which=4) #8,12,38

cereal.glm3 <- glm(Response~NameType+Brand+Calories+Sugars+logFiber+Character+
                     Calories:logFiber, data=CerealData3, family=binomial)
summary(cereal.glm3)

#diagnostics of cereal.glm3
plot(resid(cereal.glm3, type="response"))
which(resid(cereal.glm3, type="response") > 0.9) #38, 70
which(resid(cereal.glm3, type="response") < -0.9) #none
plot(cereal.glm3, which=4) #none is over .5 

CerealData4 <- CerealData[-c(38,70,42,43,27,62),]
CerealData4 #not working

anova(cereal.glm2, cereal.glm3, test="Chisq") #prefer larger model
summary(cereal.glm2)
cereal.glm4 <- update(cereal.glm3, .~.+Flavors)
#I tried cereal.glm4 by adding one variable each time, and all turned out to prefer larger model.
anova(cereal.glm2, cereal.glm4, test="Chisq") #prefer smaller model, cereal.glm4
summary(cereal.glm4)

cereal.glm5 <- update(cereal.glm4, .~.-logFiber-Calories:logFiber) #logFiber turns out not significant, 
#but we still keep it because we have interaction term calories:logFiber
anova(cereal.glm4, cereal.glm5, test="Chisq") #marginally significant
#do we want cereal.glm5, the smaller model?

summary(cereal.glm5) #Sugars turn out not significant
cereal.glm6 <- update(cereal.glm5, .~.-Sugars)
anova(cereal.glm5, cereal.glm6, test="Chisq") #marginally significant
summary(cereal.glm6)#NameType and Brand are not significant

cereal.glm7 <- update(cereal.glm6, .~.-NameType)
anova(cereal.glm6, cereal.glm7, test="Chisq") #p value is .02, prefer the small model. 
summary(cereal.glm7)

cereal.glm8 <- update(cereal.glm6, .~.-Brand)
anova(cereal.glm7, cereal.glm8, test="Chisq")#p value is .02, prefer the small model. 
summary(cereal.glm8)

#FINAL: cereal.glm7
