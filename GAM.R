installed.packages('mgcv')
library('mgcv')
setwd('C:\\Users\\raona\\OneDrive\\Documents\\Chemicals')
df2 <- read.csv('test_set.csv')
df1 <- read.csv('training_set.csv')
head(df1)
hist(df1$Impurity.Percent)
model <- gam(Impurity.Percent~s(I,k=5)+s(II,k=5)+s(III,k=3)+s(IV,k=6)+s(V,k=5)+s(Temp,k=2)+ti(II,IV)+ti(III,IV)+ti(IV,V), data = df1,method='REML')
error <- (df1$Impurity.Percent-predict(model,df1))**2
lse <- mean(error)
print(sum(error))

model1 <- gam(Impurity.Percent~s(I,k=5)+s(II,k=5)+s(III,k=3)+s(IV,k=6)+s(V,k=5)+s(Temp,k=2), data = df1,method='REML')

AIC(model1,model)

model1 <- gam(Impurity.Percent~s(I,k=9)+s(II,k=9)+s(III,k=9)+s(IV,k=9)+s(V,k=9)+s(Temp,k=9), data = df1,method='REML')
plot(model, pages = 1, main = "Estimate of smooth function")
plot(model$fitted.values,df1$Impurity.Percent,xlab="Fitted Value", ylab="Observed Value",main= "Fit obtained using Ensemble")
summary(model)
gam.check(model, pages= 1)
concurvity(model, full= TRUE)
model1$fitted.values
df1$Impurity.Percent
mean(model$residuals^2)
list <- as.data.frame(predict(model,df2))

for (i in list){
  if (i <= 1.8){
    print(which(list==i))
  }
}

write.csv(list, file = "chemical_predictions_group_B_week.csv", row.names=FALSE)
summary(model)

library(caret)
ctrl <- trainControl(method = "cv", number = 5)
model <- train(Impurity.Percent~s(I,k=5)+s(II,k=5)+s(III,k=3)+s(IV,k=6)+s(V,k=5)+s(Temp,k=2), data = df1, method = "gm", trControl = ctrl)
print(model)


gen_additive_mod(adjust_deg_free = numeric(1), select_features = logical(1)) %>% set_engine("mgcv") %>% set_mode("regression") %>%  translate()
library(mgcv)
gen_additive_mod() %>% 
  set_engine("mgcv") %>% 
  set_mode("regression") %>% 
  fit(mpg ~ wt + gear + cyl + s(disp, k = 10), data = mtcars)
