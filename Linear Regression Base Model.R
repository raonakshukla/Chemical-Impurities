# Installing important Libraries
install.packages('car')
install.packages('caTools')
install.packages('lmerTest')

library(caret)
library(car)
library(caTools)
library(lme4)
library(lmerTest)

setwd('C:\\Users\\raona\\OneDrive\\Documents\\Chemicals')

# Importing the Dataset
df1 = read.csv('training_set.csv')
df3 = read.csv('test_set_KS.csv')
df2 <- read.csv('classification.csv')
head(df1)
hist(log(df1$Impurity.Percent))
tr <- df1[,3:8]
# Assuming your training and test data are in separate vectors named 'train_data' and 'test_data'

# Perform the two-sample Kolmogorov-Smirnov test
ks.result <- ks.test(df1$I,df3$I, alternative = "two.sided")

# Print the test statistic and p-value
cat("Test Statistic:", ks.result$statistic, "\n")
cat("p-value:", ks.result$p.value, "\n")

# Interpretation
if (ks.result$p.value > 0.05) {
  cat("We fail to reject the null hypothesis. There is evidence to suggest the training and test data come from the same distribution.")
} else {
  cat("We reject the null hypothesis. There is evidence to suggest the training and test data come from different distributions.")
}

# Perform the two-sample Kolmogorov-Smirnov test
ks.result <- ks.test(df1$II,df3$II, alternative = "two.sided")

# Print the test statistic and p-value
cat("Test Statistic:", ks.result$statistic, "\n")
cat("p-value:", ks.result$p.value, "\n")

# Interpretation
if (ks.result$p.value > 0.05) {
  cat("We fail to reject the null hypothesis. There is evidence to suggest the training and test data come from the same distribution.")
} else {
  cat("We reject the null hypothesis. There is evidence to suggest the training and test data come from different distributions.")
}

# Perform the two-sample Kolmogorov-Smirnov test
ks.result <- ks.test(df1$III,df3$III, alternative = "two.sided")

# Print the test statistic and p-value
cat("Test Statistic:", ks.result$statistic, "\n")
cat("p-value:", ks.result$p.value, "\n")

# Interpretation
if (ks.result$p.value > 0.05) {
  cat("We fail to reject the null hypothesis. There is evidence to suggest the training and test data come from the same distribution.")
} else {
  cat("We reject the null hypothesis. There is evidence to suggest the training and test data come from different distributions.")
}

# Perform the two-sample Kolmogorov-Smirnov test
ks.result <- ks.test(df1$IV,df3$IV, alternative = "two.sided")

# Print the test statistic and p-value
cat("Test Statistic:", ks.result$statistic, "\n")
cat("p-value:", ks.result$p.value, "\n")

# Interpretation
if (ks.result$p.value > 0.05) {
  cat("We fail to reject the null hypothesis. There is evidence to suggest the training and test data come from the same distribution.")
} else {
  cat("We reject the null hypothesis. There is evidence to suggest the training and test data come from different distributions.")
}

# Perform the two-sample Kolmogorov-Smirnov test
ks.result <- ks.test(df1$V,df3$V, alternative = "two.sided")

# Print the test statistic and p-value
cat("Test Statistic:", ks.result$statistic, "\n")
cat("p-value:", ks.result$p.value, "\n")

# Interpretation
if (ks.result$p.value > 0.05) {
  cat("We fail to reject the null hypothesis. There is evidence to suggest the training and test data come from the same distribution.")
} else {
  cat("We reject the null hypothesis. There is evidence to suggest the training and test data come from different distributions.")
}

# Perform the two-sample Kolmogorov-Smirnov test
ks.result <- ks.test(df1$Temp,df3$Temp, alternative = "two.sided")

# Print the test statistic and p-value
cat("Test Statistic:", ks.result$statistic, "\n")
cat("p-value:", ks.result$p.value, "\n")

# Interpretation
if (ks.result$p.value > 0.05) {
  cat("We fail to reject the null hypothesis. There is evidence to suggest the training and test data come from the same distribution.")
} else {
  cat("We reject the null hypothesis. There is evidence to suggest the training and test data come from different distributions.")
}


# Changing the datatype for impurities
#df1$Impurity.Type <- as.factor(df1$Impurity.Type)
#str(df1)

# Dividing dataset into test and train
set.seed(555)
sample <- sample.split(df1$Impurity.Percent, SplitRatio = 0.8)
train  <- subset(df1, sample == TRUE)
test   <- subset(df1, sample == FALSE)

#Fitting a linear model with classes as a factor
model_base <- lm((Impurity.Percent)~I+II+III+IV+V+Temp,train)
model_lm = lm(Impurity.Percent~sqrt(I)+II+III+sqrt(IV)+sqrt(V)+Impurity.Type,train)
summary(model_base)
summary(model_lm)
anova(model_base)
residualPlots(model_lm,pages=1)
residualPlots(model_base,pages=1)
plot(model_base,pages=1)
qqPlot(model_base,main="Normality Plot", xlab="Quantiles", ylab="Standard Residuals")
anova(model_lm)
shapiro.test(model_base$residuals)
qqnorm(model_lm$residuals)
qqline(model_lm$residuals)

error <- (test$Impurity.Percent-predict(model_base,test))**2
lse <- mean(error)
print(sum(error))

hist(model_base$residuals, main= "Histogram of Residuals", xlab = "Residuals")



# reproducible random sampling
set.seed(1) 

# defining training control as
# repeated cross-validation and 
# value of K is 10 and repetition is 3 times
train_control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(Impurity.Percent~sqrt(I)+II+III+sqrt(IV)+sqrt(V)+Impurity.Type, data = df1, 
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)

# Feature Selection using AIC
model2= step(model_lm,direction="both")
summary(model2)

# Model after AIC
model1 <- train(Impurity.Percent~sqrt(I)+III+sqrt(IV)+sqrt(V)+Impurity.Type, data = df1, 
               method = "lm",
               trControl = train_control)
print(model1)

# Model after AIC
model2 <- train((log(Impurity.Percent))~(I)+III+(IV)+(V)+Impurity.Type, data = df1, 
                method = "lm",
                trControl = train_control)
print(model2)



# Building GLM
model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(1|Impurity.Type)+(II|Impurity.Type)+(III|Impurity.Type)+(IV|Impurity.Type)+(V|Impurity.Type)+(Temp|Impurity.Type), data= train)
summary(model_glm)
plot(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(lse)

model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type)+(II|Impurity.Type)+(III|Impurity.Type)+(IV|Impurity.Type)+(V|Impurity.Type)+(Temp|Impurity.Type), data= train)
summary(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(lse) #28780.96

model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type)+(II|Impurity.Type)+(III|Impurity.Type)+(IV|Impurity.Type)+(V|Impurity.Type), data= train)
summary(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(lse) #11396.79 

model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type)+(II|Impurity.Type)+(III|Impurity.Type)+(IV|Impurity.Type), data= train)
summary(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(lse)#1737.891

model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type)+(II|Impurity.Type)+(III|Impurity.Type), data= train)
summary(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(lse) # 3558.633

model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type)+(II|Impurity.Type), data= train)
summary(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(lse) #217.920


model_glm1 <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type), data= train)
summary(model_glm1)
error <- (test$Impurity.Percent-predict(model_glm1,test))**2
lse <- mean(error)
print(lse)

model_glm1 <- lmer(Impurity.Percent~(II|Impurity.Type), data= train)
summary(model_glm1)
error <- (test$Impurity.Percent-predict(model_glm1,test))**2
lse <- mean(error)
print(lse)

model_glm <- lmerTest::lmer((Impurity.Percent)~sqrt(I)+sqrt(II)+sqrt(IV)+sqrt(V)+(II|Impurity.Type), data= train)
summary(model_glm)
error <- (test$Impurity.Percent-predict(model_glm,test))**2
lse <- mean(error)
print(sum(error))
plot(model_glm,pages=1)
names(model_glm)
residualPlots(model_glm)
AIC(model_glm,model)
model2= step(model_glm,direction="both")
summary(model2)

pred <- as.data.frame(predict(model_glm,df2))
write.csv(pred, file = "chemical_predictions_group_B_week_5.csv", row.names=FALSE)



model2 <- train(Impurity.Percent~sqrt(I)+II+III+sqrt(IV)+V+Temp+(II|Impurity.Type), data = train, 
                method = "glm",
                trControl = train_control)
print(model2)

model_glm <- lmer(Impurity.Percent~I+II+III+IV+V+Temp+(I|Impurity.Type)+(II|Impurity.Type), data= train)
model_glm <- lmer(Impurity.Percent~I+(I|Impurity.Type), data= train)












library(lattice)


xyplot(Impurity.Percent~I|Impurity.Type,df1,lwd=3,type=c("p","r"))
xyplot(Impurity.Percent~II|Impurity.Type,df1,lwd=3,type=c("p","r"))
xyplot(Impurity.Percent~III|Impurity.Type,df1,lwd=3,type=c("p","r"))
xyplot(Impurity.Percent~IV|Impurity.Type,df1,lwd=3,type=c("p","r"))
xyplot(Impurity.Percent~V|Impurity.Type,df1,lwd=3,type=c("p","r"))
xyplot(Impurity.Percent~Temp|Impurity.Type,df1,lwd=3,type=c("p","r"))



model_n = lme(fixed = Impurity.Percent~ )


