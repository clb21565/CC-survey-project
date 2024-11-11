library(lavaan)
library(psych)
library(nFactors)
library(corrplot)
library(GPArotation)

#code adapted from Knetka et al 2019 10.1187/cbe.18-04-0064 
data=fread("aggregate_data.csv",stringsAsFactors = TRUE)
  
## lavaan model for CFA - explanatory variables in question are general negative perceptions, researcher rank, and hireability. 
CFA2<-'
gn=~ gn1+gn2
h=~h1+h2+h3+h4
gn~rank
h~rank
gn ~~ 0.001 * gn
'

#run model with MLR estimator, missing data handled by fiml, and group the data by the status of the student profile

C2f_fit <- cfa(CFA2, estimator = "mlr", missing = "fiml", data= data,group="profile")

summary(C2f_fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
