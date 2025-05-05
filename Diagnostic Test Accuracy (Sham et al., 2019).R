
###Doing diagnostic test accuracy

#=======================================#
# 1. Univeiate analysis
#=======================================#

install.packages("mada")
install.packages("mvtnorm")
install.packages("ellipse")
install.packages("mvmeta")
#=======================================#

library(mada)
library(meta)
library(mvtnorm)
library(ellipse)
library(mvmeta)
library(metafor)
library(readxl)
DTA <- read_excel("E:/Med School/Research/Examples of sheets for R/DTA.xlsx")
View(DTA)

#=======================================#
#=======================================#

#Therefore, it is more natural to check the value of each summary
#statistic by performing univariate analysis using the “meta” package
#first, and then to present an SROC curve using the “mada” package

sensitivity_logit= metaprop(DTA$TP, DTA$TP+
                              DTA$FN, common = FALSE, random = TRUE,
                            sm = "PLOGIT", studlab = DTA$id, method.ci = "CP", #CP=Clopper–Pearson method
                            subgroup= DTA$g)
print(sensitivity_logit, digits= 3)
summary(sensitivity_logit)
#To calculate the effect size from proportion-type data, the method of reverting after logit transformation was used. Besides, you
#can enter sm= “PRAW” to use the raw data without transformation, or sm= “PLN” to find the reverted value after log transformation.



forest(sensitivity_logit, digits= 3, rightcols= c("effect", "ci"),
       xlab= "Sensitivity")

#======================================#
#Specificity
#======================================#

specificity_logit = metaprop(DTA$TN, DTA$TN+
                               DTA$FP, common = FALSE, random = TRUE,
                             sm = "PLOGIT", method.ci = "CP", studlab = DTA$id,
                             subgroup= DTA$g)
print(specificity_logit, digits= 3)
summary(specificity_logit)


forest(specificity_logit, digits= 3, rightcols= c("effect", "ci"),
       xlab= "Specificity")


#=================================#
#Diagnostic odds ratio
#=================================#
#For binary data, enter TP, TP+FP, FN, and FN+TN in this order
DOR_model= metabin(TP,TP+FP,FN,FN+TN, sm= "OR",
                   common = FALSE,random = TRUE, method = "Inverse", id, subgroup= g, data= DTA)
print(DOR_model)

forest(DOR_model, digits= 3, rightcols= c("effect", "ci"), xlab
       = "Diagnostic Odds Ratio") #Univariate analysis: diagnostic odds ratio. OR, odds ratio; CI, confidence interval; g, subgroup


#=======================================#
# 2. bivariate analysis
#=======================================#

detach(package:meta) #Both mada and meta use forest function, thus we need to remove meta first
#Diagnostic test accuracy summary line (summary receiver operating characteristic curve)

forest(madad(DTA), type= "sens", xlab= "Sensitivity",
       snames= DTA$id)
forest(madad(DTA), type= "spec", xlab= "Specificity",
       snames= DTA$id)
forest(madauni(DTA))

fit = reitsma(DTA, correction.control= "single") #Bivariate diagnostic random-effects meta-analysis

summary(fit)

#to generate SROC curve
plot(fit, sroclwd= 2, xlim= c(0,1), ylim= c(0,1), main= "SROC
     curve (bivariate model) for Diagnostic Test Accuracy")
points(fpr(DTA), sens(DTA), pch= 20)
legend("bottomleft", c("SROC", "95% CI region"), lwd= c(2,1))


#=======================================#
# Sensitivity and specificity correlation coefficient
#=======================================#

DTA$sn = DTA$TP/(DTA$TP+DTA$FN) #correlation coefficient of sensitivity 
DTA$sp = DTA$TN/(DTA$FP+DTA$TN) #correlation coefficient of specifcity 
DTA$logitsn = log(DTA$sn/(1-DTA$sn))
DTA$logitsp = log(DTA$sp/(1-DTA$sp))

View(DTA)

cor(DTA$logitsn, DTA$logitsp) #correlation coefficient function

#=======================================#
# Meta-regression
#=======================================#
library(meta)

metareg(DOR_model, g, method.tau= "REML", digits= 3)