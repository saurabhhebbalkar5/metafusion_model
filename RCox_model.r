library(survAUC)
#install.packages('survival')
library(survival)
library(ggplot2)
library(tidyr)
library(plyr)
library(riskRegression)
library(prodlim)
library(pec)
library(rms)
library(dplyr) 


#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/eur_data_male_full.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_eur_male_full.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.01]
options(max.print=1000)
temp

#bs=0.855
#0.1563216

#---------------------------------------------------------------------------------------------------------

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/eur_data_female_full.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_eur_female_full.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.01]
options(max.print=1000)
temp

#bs=0.918

  
#---------------------------------------------------------------------------------------------------------

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/eas_data_male.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_eas_male.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.5]
options(max.print=1000)
temp

#bs=0.687
#bh=0.3203210

#------------------------------------------------------------------------------------------------------

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/eas_data_female.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_eas_female.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.5]
options(max.print=1000)
temp

#bs=0.722
#bh=0.2786245



#------------------------------------------------------------------------------------------------

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/sas_data_male.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_sas_male.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.05]
options(max.print=1000)
temp

#bs=0.4373
#bh=0.8298951

#-----------------------------------------------------------------------------------------------------

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/sas_data_female.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_sas_female.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.05]
options(max.print=1000)
temp

#bs=0.528
#bh=0.6328648


#-------------------------------------------------------------------------------------------------------


#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/afr_data_female.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_afr_female.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.05]
options(max.print=1000)
temp

#bs=0.592
#bh=0.519434


#-------------------------------------------------------------------------------------------------------
#Admixed Male

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/admixed_data_male.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_admixed_male.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.05]
options(max.print=1000)
temp

#bs=0.631
#bh=0.460342



#-------------------------------------------------------------------------------------------------------
#Admixed Female

#Load the Dataset from Python
dataset = read.csv('/home/saurabh/Admixed_model/admixed_data_female.csv',row.names = 1)
head(dataset)
dim(dataset)

#Selected Columns
data_col =  c('time','affected','age_enroll','age_disease', 'pca1_x', 'pca2_x', 'pca3_x', 'pca4_x', 'geno_array_x', 'prs')
dataset[data_col]
data = dataset[data_col]
dim(data)

fit <- coxph(Surv( age_enroll, age_disease, affected) ~ geno_array_x + pca1_x + pca2_x +
               pca3_x + pca4_x  + prs,
             data = data, x = TRUE)

summary(fit)
fit$coefficients
write.csv(fit$coefficients, "/home/saurabh/Admixed_model/rcox_beta_admixed_female.csv", row.names=TRUE)

#Gives exact baseline survival
survivalbaseline <-summary(survfit(fit))
options(max.print=1000000)
survivalbaseline


baseline = basehaz(fit)
baseline
temp=baseline[baseline[2]>74.99999 & baseline[2]<75.5]
options(max.print=1000)
temp

#bs=0.695
#bh=0.3634771






