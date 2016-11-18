#EDA Graphs 
library(car)
library(lattice)

#EDA graphs for mortality and SO2:check for skewness and outliers 
hist(Pollution$Mortality)
hist(Pollution$SO2)
qqnorm(Pollution$Mortality)
qqline(Pollution$Mortality)

Pollution$logSO2 <- log(Pollution$SO2)
head(Pollution)
hist(logSO2)

head(Pollution)
summary(Pollution)
plot(Mortality~logSO2, data = Pollution)
abline(Simple.lm)
#outliers in Mortality direction
which(Pollution$Mortality > 1100) #=> 60
which(Pollution$Mortality < 800) #=>1
Simple.lm <- lm(Mortality~logSO2, data = Pollution)
plot(Simple.lm)
#created a data frame without the outlier points 
Pollution.new <- Pollution[c(-1,-60), ]

#Simple statistics for Mortality and SO2
summary(Pollution.new$Mortality)
summary(Pollution.new$SO2)
plot(Pollution.new$logSO2, Pollution.new$Mortality)
Simple.lm.new <- lm(Mortality~logSO2, data = Pollution.new)
abline(Simple.lm.new, data = Pollution.new)
-------------------------------------------------------
#[EDA for multiregression]

#1) First deciding which individual explanatory variables we want to explain mortality,
#do summary test of the full model. 

#Mortality.lm1: a full model. 
Mortality.lm1 <- lm(Mortality ~ Precip+Humidity+JanTemp+JulyTemp+Over65+Educ+Density+NonWhite+Poor, data = Pollution.new)
summary(Mortality.lm1)

#The summary output indicates that JanTemp, education, Density, and NonWhite
#are at least marginally significant. 
----------------------------------------------------------
#Mortality.lm2: a reduced model that only includes variables indicated 
#as (marginally) significant in the full model's summary output.
Mortality.lm2 <- lm(Mortality ~ JanTemp+Educ+Density+NonWhite, data = Pollution.new)
summary(Mortality.lm2)
#summary output tells us that every variable is significant. 
anova(Mortality.lm1, Mortality.lm2)
#result: pvalue is 0.268, so we prefer the small model, Mortality.lm2
--------------------------------------------------------------
#Look at scatterplotmatrix and figure out the interaction terms 
Pollution.subset <- subset(Pollution.new, select=c(2,5,8,9,10))
Pollution.subset
scatterplotMatrix(Pollution.subset)

#From the plot, we suspect that there might be interaction between Educ, Density, and NonWhite.
Mortality.lm3 <- lm(Mortality ~ (JanTemp+Educ+Density+NonWhite)+I(JanTemp^2)+I(Educ^2)+I(Density^2)+I(NonWhite^2), data = Pollution.new)
summary(Mortality.lm3)
#Trellis Plot 
#i) Mortality ~ Education, NonWhite
Educ2 <- equal.count(Pollution$Educ, number=4, overlap = 0)
xyplot(Mortality ~ NonWhite | Educ2, data = Pollution.new,
       panel = function(x,y){panel.xyplot(x,y); panel.lmline(x,y)}
)
#ii) Mortality~ Education, Density
Educ2 <- equal.count(Pollution$Educ, number=4, overlap = 0)
xyplot(Mortality ~ Density | Educ2, data = Pollution.new,
       panel = function(x,y){panel.xyplot(x,y); panel.lmline(x,y)}
)
#unparallel!!!

#ii) Mortality~ Education, JanTemp
Educ2 <- equal.count(Pollution$Educ, number=4, overlap = 0)
xyplot(Mortality ~ JanTemp | Educ2, data = Pollution.new,
       panel = function(x,y){panel.xyplot(x,y); panel.lmline(x,y)}
)
#unparallel!!!

#iv) Mortality~ Density, JanTemp
JanTemp2 <- equal.count(Pollution$JanTemp, number=4, overlap = 0)
xyplot(Mortality ~ Density | JanTemp2, data = Pollution.new,
       panel = function(x,y){panel.xyplot(x,y); panel.lmline(x,y)}
)
#v) Mortality~ NonWhite, JanTemp
JanTemp2 <- equal.count(Pollution$JanTemp, number=4, overlap = 0)
xyplot(Mortality ~ NonWhite | JanTemp2, data = Pollution.new,
       panel = function(x,y){panel.xyplot(x,y); panel.lmline(x,y)}
)
#unparallel!!!
#vi) Mortality~ Density, NonWhite
Density2 <- equal.count(Pollution$Density, number=4, overlap = 0)
xyplot(Mortality ~ NonWhite | Density2, data = Pollution.new,
       panel = function(x,y){panel.xyplot(x,y); panel.lmline(x,y)}
)

#Try out two models with and without interaction terms. 
#i) A full model
Mortality.lm4 <- lm(Mortality ~ JanTemp+Educ+Density+NonWhite+I(Educ^2)+Educ:Density+Educ:JanTemp, data = Pollution.new)

#ii) A reduced model 
Mortality.lm2 <- lm(Mortality ~ JanTemp+Educ+Density+NonWhite, data= Pollution.new)
 
anova(Mortality.lm2, Mortality.lm4)
#keep the fuller model Mortality.lm4
summary(Mortality.lm4)
---------------------------------------------------------
#The diagnostics for the full model, Mortality.lm4. 
library(car)
plot(fitted(Mortality.lm4), rstudent(Mortality.lm4))
abline(h = 0)
abline(h = c(-2, 2), col = "red")
abline(h = c(-3, 3), col = "green")
scatterplotMatrix(Pollution.subset, diagonal = "none", reg.line = FALSE, smooth = FALSE, 
                  id.method = abs(rstudent(Mortality.lm4)), id.n = 3, id.col = "red")
#indicates taht 24,52 have largest absolute value jackknife residuals. 
#However, 4 observations that are out of absolute residual values of 2 are only marginally out,
#and none of them goes out of absolute value of 3. 
#We will not remove any of them, especially because out data is quite small.

#Cook's distance
plot(Mortality.lm4, which = 4)

#indicates 4,7,20 have highest cook's distance, but none of them is over 1, 
#We will remove only index=20 that has the highest cook's distance.

ncvTest(Mortality.lm4) #non-constant variance
#The p value is 0.6597534, so we can conclude that there is no non-constant variance problem. 

#Let's check the leverages:#

plot(hatvalues(Mortality.lm4), type = "h")
abline(h = 2*8/60, col = "red") #horizontal line at 2p/n
which(hatvalues(Mortality.lm4) > 2*8/60) #which observations are flagged for investigation?
index <- which(hatvalues(Mortality.lm4) > 2*8/60) #store row numbers
length(index)/60 #fraction flagged
#We probably don't want to remove 8% of the data. Take a look at the leverage plot again. Let's
#just take the more extreme cases and identify them in the scatterplot matrix.
scatterplotMatrix(Pollution.subset, diagonal = "none", reg.line = FALSE, smooth = FALSE,
                  id.method = hatvalues(Mortality.lm4), id.n = 3, id.col = "red")

index <- which(hatvalues(Mortality.lm4) > .6)
index
scatterplotMatrix(Pollution, diagonal = "none", smooth = FALSE,
                  reg.line = FALSE, id.method = index, id.n = 1, id.col = "red")

Index <- Pollution.new[c(19,20), ] 
Index
#We only take out row 19 and 20 of Pollution.new. 
#*The index 19 of the Pollution.new data turns out as index 20 of the original data,
#because Pollution.new is a data frame already without outliers in the 
#Mortality-SO2 plot, the relationship we ultimately want to know. 


influencePlot(Mortality.lm4, id.n =3)

#CONCLUSION: We remove 19 and 20, which are observation from York, PA and Utica, Ny
--------------------------------------------------------------------
#remove outliers from the model Mortality.lm4
summary(Mortality.lm4)
Mortality.lm5 <- lm(Mortality.lm4, subset = -c(19,20))
summary(Mortality.lm5)
#CHANGES: no significance change, nonwhite is the only significant. 
#the estimate of the density changed quite drastically from minus sign to plus.

--------------------------------------
#Quick diagnostics of Mortality.lm5
#residuals plot
plot(fitted(Mortality.lm5), resid(Mortality.lm5)) #plot(xvar, yvar)
plot(fitted(Mortality.lm5), resid(Mortality.lm5), xlab = "fitted values", ylab = "residuals")
abline(h=0)#qqnorm
plot(Mortality.lm5, which=2)

#CONCLUSION: We proceed with Mortality.lm5
--------------------------------------

summary(Mortality.lm5)
anova(Mortality.lm5)

#Mortality.lm6 <- update(Mortality.lm5,.~.-JanTemp-I(Educ^2)-Educ:Density-JanTemp:Educ)
Mortality.lm6 <- lm(Mortality~Educ+Density+NonWhite, data= Pollution.new, subset = -c(19,20))
summary(Mortality.lm6)
anova(Mortality.lm6)

anova(Mortality.lm5, Mortality.lm6)
#the pvalues suggests that we prefer the full model, Mortality.lm5. 
-------------------------
#We try Mortality.lm5 -JanTemp:Educ
Mortality.lm7 <- lm(Mortality~JanTemp+Educ+Density+NonWhite+I(Educ^2)+Educ:Density, data= Pollution.new, subset = -c(19,20))
summary(Mortality.lm7)
anova(Mortality.lm7)
anova(Mortality.lm5, Mortality.lm7)

#We prefer the reduced model, in this case, Mortality.lm7.
------------------------
#Mortality.lm7 - Educ:Density
Mortality.lm8 <- lm(Mortality~JanTemp+Educ+Density+NonWhite+I(Educ^2), data= Pollution.new, subset = -c(19,20))
summary(Mortality.lm8)
anova(Mortality.lm8)
anova(Mortality.lm7, Mortality.lm8)

#we prefer reduced model, Mortality.lm8
----------------------------
#Mortality.lm8 - JanTemp
Mortality.lm9 <- lm(Mortality~ Educ+Density+NonWhite+I(Educ^2), data= Pollution.new, subset = -c(19,20))
summary(Mortality.lm9)
anova(Mortality.lm9)
anova(Mortality.lm8, Mortality.lm9)
#we prefer full model, Mortality.lm8
---------------------------------------
#Mortality.lm8 - I(Educ^2)
Mortality.lm10 <- lm(Mortality~JanTemp+Educ+Density+NonWhite, data= Pollution.new, subset = -c(19,20))
summary(Mortality.lm10)
anova(Mortality.lm10)
anova(Mortality.lm8, Mortality.lm10)
#We prefer small model, Mortality.lm10. 

#The summary output shows that all the variabls in the model are significant.

#CONCLUSION: We proceed with mortality.lm10.
------------------------------------------------------------
#Diagnostics of our final model. 
Mortality.lm10 <- lm(Mortality~JanTemp+Educ+Density+NonWhite, data= Pollution.new, subset = -c(19,20))

library(car)
plot(fitted(Mortality.lm10), rstudent(Mortality.lm10))
abline(h = 0)
abline(h = c(-2, 2), col = "red")
abline(h = c(-3, 3), col = "green")
scatterplotMatrix(Pollution.subset, diagonal = "none", reg.line = FALSE, smooth = FALSE, 
                  id.method = abs(rstudent(Mortality.lm10)), id.n = 1, id.col = "red")
#indicates that 2 has largest absolute value jackknife residuals, which goes out of absolute value of 3. 
#We will remove index 2. 

#Cook's distance
plot(Mortality.lm10, which = 4)

#indicates 2,4,7 have highest cook's distance, but none of them is over 1, 
#We will not remove any outlier here. 

ncvTest(Mortality.lm10) #non-constant variance
#The p value is 0.488139, so we can conclude that there is no non-constant variance problem. 

#Let's check the leverages:
library(car)
library(lattice)
plot(hatvalues(Mortality.lm10), type = "h")
abline(h = 2*5/56, col = "red") #horizontal line at 2p/n
which(hatvalues(Mortality.lm10) > 2*5/56) #which observations are flagged for investigation?
index <- which(hatvalues(Mortality.lm10) > 2*5/56) #p=5, n=56 since we have already moved 4 ourliers.
length(index)/60 #fraction flagged
#We'll take a look at the leverage plot,
#and just take the more extreme cases and identify them in the scatterplot matrix.
#In the leverage plot, there is only one observation that has visibly higher leverage than others.

index <- which(hatvalues(Mortality.lm10) > .25)
index

scatterplotMatrix(Pollution, diagonal = "none", smooth = FALSE,
                  reg.line = FALSE, id.method = index, id.n = 1, id.col = "red")
#CONCLUSION: We remove observation 2 from the jackknife residual plot diagnostics
#,and observation 6 from leverage diagnostics. 
Index <- Pollution.new[c(2,6), ] 
Index

#The outliers are observations from Miami, FL, and San Diego, CA. 

----------------------------------------------------------
#remove outliers from the model Mortality.lm4
summary(Mortality.lm10)
Mortality.11 <- lm(Mortality.lm10, subset = -c(2,6))
summary(Mortality.11)
#CHANGES: no significance change, all variables are significant in both models. 
#the estimate of the Educaiton changes quite drastically, so we can infer that the observations from Miami and San Diego were influential outliers for Mortality against education.

-------------------------------------------------
#Now that all the other variables are controlled, put SO2 into the model.
#Mortality.Final <- update(Mortality.l1, .~. + logSO2)
Mortality.lm12 <- lm(Mortality~JanTemp+Educ+Density+NonWhite+logSO2, data= Pollution.new, subset = -c(2,6,19,20))
#I had to rewrite the lm because update funciton was not working.
summary(Mortality.lm12)

Mortality.lm13 <- lm(Mortality~Educ+Density+NonWhite+logSO2, data= Pollution.new, subset = -c(2,6,19,20))
summary(Mortality.lm13)
anova(Mortality.lm12, Mortality.lm13)
#keep the smaller model, Mortality.lm13
---------------------------------------------------
#Check for multicollinearity problem
library(car)
vif(Mortality.lm13)
Educ  Density NonWhite   logSO2 
1.138091 1.503898 1.049307 1.643408
#No multicollinearity problem!
-----------------------------------------------------