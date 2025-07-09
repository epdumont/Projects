#STA108 Final Project

#Create a data subset 
sleepdata <- read.csv("~/Documents/STA108/final/sleepdata.csv")
Nfinal <- dim(sleepdata)[1]
sap_final <- sample(1:Nfinal,floor(0.794*Nfinal))
mysubset_final <- sleepdata[sap_final,]
write.csv(mysubset_final, file = "~/Documents/STA108/final/dataset.csv", row.names = FALSE)
#mysummary <- summary()

#Fit a model for each variable of interest
classesmissed <- lm(GPA~ClassesMissed, data=mysubset_final)
plot(mysubset_final$ClassesMissed, mysubset_final$GPA, main = "Classes Missed vs. GPA", xlab = "Classes Missed", ylab = "GPA")
abline(classesmissed, col = "red") 

cognitionzscore <- lm(GPA~CognitionZscore, data=mysubset_final)
plot(mysubset_final$CognitionZscore, mysubset_final$GPA, main = "Cognition Z Score vs. GPA", xlab = "Cognition Z Score", ylab = "GPA")
abline(cognitionzscore, col = "red") 

poorsleepquality <- lm(GPA~PoorSleepQuality, data=mysubset_final)
plot(mysubset_final$PoorSleepQuality, mysubset_final$GPA, main = "Poor Sleep Quality vs. GPA", xlab = "Poor Sleep Quality", ylab = "GPA")
abline(poorsleepquality, col = "red")

depressionscore <- lm(GPA~DepressionScore, data=mysubset_final)
plot(mysubset_final$DepressionScore, mysubset_final$GPA, main = "Depression Score vs. GPA", xlab = "Depression Score", ylab = "GPA")
abline(depressionscore, col = "red")

anxietyscore <- lm(GPA~AnxietyScore, data=mysubset_final)
plot(mysubset_final$AnxietyScore, mysubset_final$GPA, main = "Anxiety Score vs. GPA", xlab = "Anxiety Score", ylab = "GPA")
abline(anxietyscore, col = "red")

stressscore <- lm(GPA~StressScore, data=mysubset_final)
plot(mysubset_final$StressScore, mysubset_final$GPA, main = "Stress Score vs. GPA", xlab = "Stress Score", ylab = "GPA")
abline(stressscore, col = "red")

DASscore <- lm(GPA~DASScore, data=mysubset_final)
plot(mysubset_final$DASScore, mysubset_final$GPA, main = "DAS Score vs. GPA", xlab = "DAS Score", ylab = "GPA")
abline(DASscore, col = "red")

happiness <- lm(GPA~Happiness, data=mysubset_final)
plot(mysubset_final$Happiness, mysubset_final$GPA, main = "Happiness vs. GPA", xlab = "Happiness", ylab = "GPA")
abline(happiness, col = "red")

drinks <- lm(GPA~Drinks, data=mysubset_final)
plot(mysubset_final$Drinks, mysubset_final$GPA, main = "Drinks vs. GPA", xlab = "Drinks", ylab = "GPA")
abline(drinks, col = "red")

avgsleep <- lm(GPA~AverageSleep, data=mysubset_final)
plot(mysubset_final$AverageSleep, mysubset_final$GPA, main = "Average Sleep vs. GPA", xlab = "Average Sleep", ylab = "GPA")
abline(avgsleep, col = "red")

weekdaysleep <- lm(GPA~WeekdaySleep, data=mysubset_final)
plot(mysubset_final$WeekdaySleep, mysubset_final$GPA, main = "Weekday Sleep vs. GPA", xlab = "Weekday Sleep", ylab = "GPA")
abline(weekdaysleep, col = "red")

#Fit a full model including all variables of interest
#DepressionScore, AnxietyScore, and StressScore were omitted since DASScore (all three variables combined) was included
fullmodel <- lm(GPA~ClassesMissed+CognitionZscore+PoorSleepQuality+DASScore+Happiness+Drinks+AverageSleep+WeekdaySleep, data=mysubset_final)
anova_full <- anova(fullmodel)
View(anova_full)
summary(fullmodel)

#Compare model with DepressionScore, AnxietyScore, and StressScore
DASmodel <- lm(GPA~ClassesMissed+CognitionZscore+PoorSleepQuality+DepressionScore+AnxietyScore+StressScore+Happiness+Drinks+AverageSleep+WeekdaySleep, data=mysubset_final)
anova_DAS <- anova(DASmodel)
View(anova_DAS)
summary(DASmodel) #Shows specific correlation, different result from fullmodel

#Compare fitted values vs. residuals plots
plot(fullmodel$fitted.values, fullmodel$residuals, main = "Residuals vs. Fitted Values (DASScore)", xlab = "Fitted Values", ylab = " Residuals")
abline(h = 0, col = "blue")

plot(DASmodel$fitted.values, DASmodel$residuals, main = "Residuals vs. Fitted Values (DepressionScore + AnxietyScore + StressScore)", xlab = "Fitted Values", ylab = " Residuals")
abline(h = 0, col = "blue")

#Compare QQ plots
qqnorm(fullmodel$residuals)
qqline(fullmodel$residuals, col = "blue")

qqnorm(DASmodel$residuals)
qqline(DASmodel$residuals, col = "blue")

#Create a restricted model and obtain partial sum of squares and partial F-test
#(which variables to include here??)

#Backward selection
backward <- step(DASmodel, direction = "backward")
summary(backward)

#Forward selection
nullmodel <- lm(GPA~1, data = mysubset_final)
forward <- step(nullmodel, scope=formula(DASmodel), direction = "forward")
summary(forward)

#Stepwise selection
stepwise <- step(nullmodel, scope=formula(DASmodel), direction = "both")
summary(stepwise)
summary(stepwise)$r.squared
summary(stepwise)$adj.r.squared

#Analysis of DAS scoring system
DAScompare_model <- lm(GPA~DepressionScore+AnxietyScore+StressScore+DASScore, data = mysubset_final)
summary(DAScompare_model)

