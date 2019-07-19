# Created on 7/19/2019
# Testing the impact of separating test and validation data
library(MASS)
setwd("E:/ESF/Manuscript3/MODEL_I_SAS/Heiberg_Model_I")
dt = read.csv("Heiberg_R.csv")
# Using natural logarithm of AGB
dt$AGB = log(dt$BIO_MG_HA)

# model using all data
all.model = lm(dt$AGB~Elev_P95+Percentage,data = dt)

# testing using 75% of all data as training and 25% as testing
modelR  ={}
modelRMSE = {}
testRMSE ={}

for (i in 1:100){
  #Split data radomly to train dataset and test dataset
  set.seed(seed = NULL)
  sample = sample.int(n = nrow(dt), size = floor(.75*nrow(dt)), replace = F)
  train = dt[sample, ]
  test  = dt[-sample, ]
  # Build model using train dataset
  sub.model = lm(AGB~Elev_P95+Percentage,data = train)
  modelR[i]=summary(sub.model)$r.squared
  
  coef = coef(sub.model)
  modelRMSE[i]=sqrt(mean((train$BIO_MG_HA-exp(coef[1]+coef[2]*train$Elev_P95+coef[3]*train$Percentage))^2))
  testRMSE[i]=sqrt(mean((test$BIO_MG_HA-exp(coef[1]+coef[2]*test$Elev_P95+coef[3]*test$Percentage))^2))
}

print(summary(modelR))
print(var(modelR))
print(summary(modelRMSE))
print(var(modelRMSE))
print(summary(testRMSE))
print(var(testRMSE))

