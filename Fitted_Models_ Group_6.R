#Group 6

###################################################################################################
# 1st Multiple Linear Regression Model (Without any Trasformtions)
###################################################################################################

Hitters = read.csv("C:/Users/perer/OneDrive/Desktop/Hitters.csv", stringsAsFactors=TRUE)
View(Hitters)
attach(Hitters)
#colnames(Hitters)

#Plots of pairwise comparisons-----> Use arrows in plot tab to move each plot
for (i in 1:length(colnames(Hitters))){
  colName = colnames(Hitters)[i] 
  colData = Hitters[[colName]]        
  plot(colData, Salary, xlab = colName)
}

#Null model to be Improved by adding most significant (Forward Selection)
#1
numericalColums = c(
  "AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CRBI", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("Salary ~", col))   
  model01 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model01)[5])     
  cat("R Squred:", summary(model01)$r.squared, "\n")
}

#CRBI is your strongest single predictor of Salary according to both R²(0.3215) and p-value(2.2e-16)

#2
numericalColums = c(
  "AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("Salary ~ CRBI +", col))   
  model02 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model02)[5])  
}

#next best predictor to add is Hits, because it significantly improves the model.(p = 5.275e-11)

#3
numericalColums = c(
  "AtBat", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("Salary ~ CRBI + Hits +", col))   
  model03 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model03)[5])  
}

#next best predictor to add is PutOuts, because it significantly improves the model.(p = 0.0005143)

#4
numericalColums = c(
  "AtBat", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CWalks", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts +", col))   
  model04 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model04)[5])  
}

#next best predictor to add is AtBat, because it significantly improves the model.(p =0.0026294)

#5
numericalColums = c(
  "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CWalks", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts + AtBat +", col))   
  model05 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model05)[5])  
}

#next best predictor to add is Walks, because it significantly improves the model.(p = 0.0016547)

#6
numericalColums = c(
  "HmRun", "Runs", "RBI", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CWalks", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts + AtBat + Walks +", col))   
  model06 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model06)[5])  
}

#At alpha = 0.05, none of the remaining predictors are statistically significant 

modelAfterFwdSelection = lm(Salary ~ CRBI + Hits + PutOuts + AtBat + Walks) 
modelAfterFwdSelection

#Check the multicolinearity
library(car)
vif(modelAfterFwdSelection)

#Hits(14.227678) and AtBat(15.294325) have very high VIF (>10) implies High multicollinearity
#High multicollinearity inflates standard errors and makes coefficient estimates unstable.
#remove AtBat

#Then New chosen model as follows: 
modelAfterEdit = lm(Salary ~ CRBI + Hits + PutOuts + Walks) 
modelAfterEdit
library(car)
vif(modelAfterEdit)

#Let's Consider categorical Predictor Variables
#1

#reduced
anova(lm(Salary ~ CRBI + Hits + PutOuts + Walks))
#full
modelAfterAddCatgLeague =  lm(Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(League)) 
anova(modelAfterAddCatgLeague)
n=322
p=6
k=5
EssR = 28709807
EssF = 28598963
FCalc = ((EssR-EssF)/(p-k))/((EssF)/(n-p-1))
FCalc
qf(0.05,(p-k),(n-p-1),lower.tail = FALSE)
# FCalc(1.220879) < CV(3.871149) --> Reduced Model is adequate 

#2

#reduced
anova(lm(Salary ~ CRBI + Hits + PutOuts + Walks))
#full
modelAfterAddCatgDivision =  lm(Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division)) 
anova(modelAfterAddCatgDivision)
n=322
p=6
k=5
EssR = 28709807
EssF = 27479268
FCalc = ((EssR-EssF)/(p-k))/((EssF)/(n-p-1))
FCalc
qf(0.05,(p-k),(n-p-1),lower.tail = FALSE)
# FCalc(14.1059) > CV(3.871149) --> Full model is adequate


modelAfterAddCatgDivision =  lm(Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division)) 
anova(modelAfterAddCatgDivision)
summary(modelAfterAddCatgDivision)

fittedData = fitted(modelAfterAddCatgDivision)
stdResiduals = (residuals(modelAfterAddCatgDivision)/(111278)^0.5)

#Model Adequcy 
plot(fittedData,stdResiduals,xlab = "Fitted Values",
     ylab = "Standardized Residuals",main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
#Check for outliers
plot(1:length(stdResiduals),stdResiduals,xlab = "Index",ylab = "Standard Residuals")
abline(h=-2)
abline(h=2)

library(car)
vif(modelAfterAddCatgDivision)

#now Enter Interaction Terms
#1
interactionList = c(
  "CRBI*Hits",
  "CRBI*PutOuts",
  "CRBI*Walks",
  "CRBI*as.factor(Division)",
  "Hits*PutOuts",
  "Hits*Walks",
  "Hits*as.factor(Division)",
  "PutOuts*Walks",
  "PutOuts*as.factor(Division)",
  "Walks*as.factor(Division)"
)
for (col in interactionList) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division) + ", col))   
  modelInter1 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(modelInter1)[5])  
}
#CRBI*Hits. It’s highly significant (2.2e-16)

#2
interactionList = c(
  "CRBI*PutOuts",
  "CRBI*Walks",
  "CRBI*as.factor(Division)",
  "Hits*PutOuts",
  "Hits*Walks",
  "Hits*as.factor(Division)",
  "PutOuts*Walks",
  "PutOuts*as.factor(Division)",
  "Walks*as.factor(Division)"
)
for (col in interactionList) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division) + CRBI*Hits +", col))   
  modelInter2 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(modelInter2)[5])  
}

#Hits*Walks It’s highly significant (4e-07)

#3
interactionList = c(
  "CRBI*PutOuts",
  "CRBI*Walks",
  "CRBI*as.factor(Division)",
  "Hits*PutOuts",
  "Hits*as.factor(Division)",
  "PutOuts*Walks",
  "PutOuts*as.factor(Division)",
  "Walks*as.factor(Division)"
)
for (col in interactionList) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division) + CRBI*Hits + Hits*Walks +", col))   
  modelInter3 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(modelInter3)[5])  
}

#CRBI*PutOuts It’s highly significant(0.0457)

#4
interactionList = c(
  "CRBI*Walks",
  "CRBI*as.factor(Division)",
  "Hits*PutOuts",
  "Hits*as.factor(Division)",
  "PutOuts*Walks",
  "PutOuts*as.factor(Division)",
  "Walks*as.factor(Division)"
)
for (col in interactionList) {
  formula = as.formula(paste("Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division) + CRBI*Hits + Hits*Walks + CRBI*PutOuts +", col))   
  modelInter4 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(modelInter4)[5])  
}
# no more interactions will be added 

#reduced
anova(lm(Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division)))
#Full
modelAfterAddInteractions =  lm(Salary ~ CRBI + Hits + PutOuts + Walks + as.factor(Division) + CRBI*Hits + Hits*Walks + CRBI*PutOuts)
anova(modelAfterAddInteractions)
n=322
p=9
k=6
EssR = 27479268
EssF = 21676501
FCalc = ((EssR-EssF)/(p-k))/((EssF)/(n-p-1))
FCalc
qf(0.05,(p-k),(n-p-1),lower.tail = FALSE)
# FCalc(27.84064) > CV(2.633547) --> Full model is adequate

fittedData = fitted(modelAfterAddInteractions)
stdResiduals = (residuals(modelAfterAddInteractions)/(85341)^0.5)

#Model Adequcy 
plot(fittedData,stdResiduals,xlab = "Fitted Values",
     ylab = "Standardized Residuals",main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

#Check for outliers
plot(1:length(stdResiduals),stdResiduals,xlab = "Index",ylab = "Standard Residuals", main = "Plot of Standard Residuals")
abline(h=-2)
abline(h=2)

(cookDist = cooks.distance(modelAfterAddInteractions))
#install.packages("olsrr")
library(olsrr)
ols_plot_cooksd_chart(modelAfterAddInteractions,type = 2) 

(hatval = hatvalues(modelAfterAddInteractions))
(hatValues = ols_leverage(modelAfterAddInteractions))
ols_plot_resid_lev(modelAfterAddInteractions)

#install.packages("olsrr")
library(olsrr)
ols_plot_diagnostics(modelAfterAddInteractions)  #Use arrow in the plot tab to see other plots 

######################## Final Implementation #####################################
anova(modelAfterAddInteractions)
summary(modelAfterAddInteractions)


###################################################################################################
# 2nd Refined Multiple Linear Regression Model (With Log() Transformation due to heterogeneity of Variance)
###################################################################################################

Hitters = read.csv("C:/Users/perer/OneDrive/Desktop/Hitters.csv", stringsAsFactors=TRUE)
View(Hitters)
attach(Hitters)
#colnames(Hitters)

#Plots of pairwise comparisons-----> Use arrows in plot tab to move each plot
for (i in 1:length(colnames(Hitters))){
  colName = colnames(Hitters)[i] 
  colData = Hitters[[colName]]        
  plot(colData, log(Salary), xlab = colName)
}

#Null model to be Improved by adding most significant (Forward Selection)

#1
numericalColums = c(
  "AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRuns", "CRBI", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~", col))   
  model01 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model01)[5])     
  cat("R Squred:", summary(model01)$r.squared, "\n")
}

#CRuns is your strongest single predictor of log(Salary) according to both highest R² (0.385752) and also the lowest possible p-value (< 2.2e-16)

#2
numericalColums = c(
  "AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRBI", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns +", col))   
  model02 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model02)[5])  
}

#next best predictor to add is Hits, because it significantly improves the model (p = 2.948e-11).

#3
numericalColums = c(
  "AtBat", "HmRun", "Runs", "RBI", "Walks", "Years",
  "CAtBat", "CHits", "CHmRun", "CRBI", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns + Hits +", col))   
  model03 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model03)[5])  
}

#next best predictor to add is Years, because it significantly improves the model (p = 0.00579)

#4
numericalColums = c(
  "AtBat", "HmRun", "Runs", "RBI", "Walks",
  "CAtBat", "CHits", "CHmRun", "CRBI", "CWalks",
  "PutOuts", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns + Hits + Years + ", col))   
  model04 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model04)[5])  
}

#next best predictor to add is PutOuts , because it significantly improves the model.(p = 0.01649)

#5
numericalColums = c(
  "AtBat", "HmRun", "Runs", "RBI", "Walks",
  "CAtBat", "CHits", "CHmRun", "CRBI", "CWalks", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns + Hits + Years + PutOuts + ", col))   
  model05 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model05)[5])  
}

#next best predictor to add is Walks, because it significantly improves the model. (p = 0.0447)

#6
numericalColums = c(
  "AtBat", "HmRun", "Runs", "RBI",
  "CAtBat", "CHits", "CHmRun", "CRBI", "CWalks", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns + Hits + Years + PutOuts + Walks + ", col))   
  model06 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model06)[5])  
}
#next best predictor to add is AtBat , because it significantly improves the model. (p = 0.0143)

#7
numericalColums = c(
  "HmRun", "Runs", "RBI",
  "CAtBat", "CHits", "CHmRun", "CRBI", "CWalks", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns + Hits + Years + PutOuts + Walks + AtBat +", col))   
  model07 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model07)[5])  
}
#next best predictor to add is CWalks , because it significantly improves the model. (p = 0.0313)

#8
numericalColums = c(
  "HmRun", "Runs", "RBI",
  "CAtBat", "CHits", "CHmRun", "CRBI", "Assists", "Errors")
for (col in numericalColums) {
  formula = as.formula(paste("log(Salary) ~ CRuns + Hits + Years + PutOuts + Walks + AtBat + CWalks +", col))   
  model08 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(model08)[5])  
}
#At alpha = 0.05, none of the remaining predictors are statistically significant 

modelAfterFwdSelection = lm(log(Salary) ~ CRuns + Hits + Years + PutOuts + Walks + AtBat + CWalks) 
modelAfterFwdSelection

#Check the multicolinearity
library(car)
vif(modelAfterFwdSelection)

#Hits(14.510476), CRuns(14.158106), CWalks(12.957105)  and AtBat(15.540603) have very high VIF (>10) implies High multicollinearity
#High multicollinearity inflates standard errors and makes coefficient estimates unstable.

#remove AtBat
#Then New chosen model as follows: 
modelAfterEdit1 = lm(log(Salary) ~ CRuns + Hits + Years + PutOuts + Walks + CWalks) 
modelAfterEdit1
library(car)
vif(modelAfterEdit1)

#remove CRuns
#Then New chosen model as follows: 
modelAfterEdit2 = lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks) 
modelAfterEdit2
library(car)
vif(modelAfterEdit2)
# Then we get low VIF (<10) implies less multicollinearity

#Let's Consider categorical Predictor Variables
#1

#reduced
anova(lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks))
#full
modelAfterAddCatgLeague =  lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks + as.factor(League)) 
anova(modelAfterAddCatgLeague)
n=322
p=7
k=6
EssR = 102.068
EssF = 101.133
FCalc = ((EssR-EssF)/(p-k))/((EssF)/(n-p-1))
FCalc
qf(0.05,(p-k),(n-p-1),lower.tail = FALSE)
# FCalc < CV(3.871244) --> Reduced Model is adequate 

#2

#reduced
anova(lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks))
#full
modelAfterAddCatgDivision =  lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks + as.factor(Division)) 
anova(modelAfterAddCatgDivision)
n=322
p=7
k=6
EssR = 102.068
EssF = 100.026
FCalc = ((EssR-EssF)/(p-k))/((EssF)/(n-p-1))
FCalc
qf(0.05,(p-k),(n-p-1),lower.tail = FALSE)
# FCalc > CV --> Full model is adequate

modelAfterAddCatgDivision =  lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks + as.factor(Division)) 

anova(modelAfterAddCatgDivision)
summary(modelAfterAddCatgDivision)

fittedData = fitted(modelAfterAddCatgDivision)
stdResiduals = (residuals(modelAfterAddCatgDivision)/(0.391^0.5)^0.5)

#Model Adequcy 
plot(fittedData,stdResiduals,xlab = "Fitted Values",
     ylab = "Standardized Residuals",main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
#Check for outliers
plot(1:length(stdResiduals),stdResiduals,xlab = "Index",ylab = "Standard Residuals", ylim = c(-3,3))
abline(h=-2)
abline(h=2)

#now Enter Interaction Terms
#1
interactionList = c(
  "Hits*Years",
  "Hits*PutOuts",
  "Hits*Walks",
  "Hits*CWalks",
  "Hits*as.factor(Division)",
  "Years*PutOuts",
  "Years*Walks",
  "Years*CWalks",
  "Years*as.factor(Division)",
  "PutOuts*Walks",
  "PutOuts*CWalks",
  "PutOuts*as.factor(Division)",
  "Walks*CWalks",
  "Walks*as.factor(Division)",
  "CWalks*as.factor(Division)"
)
for (col in interactionList) {
  formula = as.formula(paste("log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks + as.factor(Division) + ", col))   
  modelInter1 = lm(formula, data = Hitters)            
  cat("\n Predictor:", col, "\n")
  print(anova(modelInter1)[5])  
}

#All interaction terms tested are not significant (p > 0.05)

modelAfterAddInteractions =  lm(log(Salary) ~ Hits + Years + PutOuts + Walks + CWalks + as.factor(Division))
anova(modelAfterAddInteractions)
summary(modelAfterAddInteractions)

#After Removing Cwalks (Due to CWalks coefficient adds negligible explanatory power (F = 0.20831, p = 0.6485) at alpha 0.05 it's not significant too)

parsimonyModel = lm(log(Salary) ~ Hits + Years + PutOuts + Walks + as.factor(Division))
anova(parsimonyModel)
summary(parsimonyModel)


fittedData = fitted(parsimonyModel)
stdResiduals = (residuals(parsimonyModel)/(0.389)^0.5)

#Model Adequcy 
plot(fittedData,stdResiduals,xlab = "Fitted Values",
     ylab = "Standardized Residuals",main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
#Check for outliers
plot(1:length(stdResiduals),stdResiduals,xlab = "Index",ylab = "Standard Residuals", ylim = c(-3,3),main = "Plot of Standard Residuals")
abline(h=-2)
abline(h=2)

(cookDist = cooks.distance(parsimonyModel))
#install.packages("olsrr")
library(olsrr)
ols_plot_cooksd_chart(parsimonyModel,type = 2) 


(hatval = hatvalues(parsimonyModel))
(hatValues = ols_leverage(parsimonyModel))
ols_plot_resid_lev(parsimonyModel)

#install.packages("olsrr")
library(olsrr)
ols_plot_diagnostics(parsimonyModel)  #Use arrow in the plot tab to see other plots 

library(car)
vif(parsimonyModel)

######################## Final Implementation #####################################
anova(parsimonyModel)
summary(parsimonyModel)


###################################################################################################
# 3rd Model for Categorized response variable and Logistic regression
###################################################################################################

library(car)
library(caret)
library(pROC)
library(ggplot2)

attach(Hitters)

trainIndex = createDataPartition(Hitters$response, p = 0.8, list = FALSE)

train1 = Hitters[trainIndex, ]
test1  = Hitters[-trainIndex, ]

model1 = glm(response~AtBat+Hits+HmRun+Runs+RBI+Walks+Years+CAtBat+CHits+CHmRun+CRuns+CRBI+CWalks+League+Division+PutOuts+Assists+Errors,data = train1,family = binomial("logit"))

vif(model1)

stepwise_model = step(model1,direction = "both", trace = 1,)
summary(stepwise_model)

vif(stepwise_model)

final_model = glm(response~AtBat+Hits+Runs+Walks+PutOuts+AtBat*CRuns,data = train1, family = binomial("logit"))

summary(final_model)

predict_test = predict(final_model, newdata = test1, type = "response" )

pred_class = ifelse(predict_test > 0.5, 1, 0)

confusionMatrix(factor(pred_class),factor(test1$response), positive = "1")

roc_obj = roc(test1$response, predict_test)
auc(roc_obj)   # Area under the curve
plot(roc_obj, col = "blue", main = "ROC Curve")








