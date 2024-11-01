library(tidyverse)
library(broom)
options(scipen=999)
theme_set(theme_classic())


# Set the working directory
setwd("G:/Ivan")

# Load the data
data = readRDS("./Xinlei/Data/merged_data.rds")

## Normalize variables
data$total_LA_volume_sqrt = sqrt(data$total_LA_volume)
hist(data$total_LA_volume_sqrt)
data$total_LA_volume_sqrt_z = scale(data$total_LA_volume_sqrt)[,1]
hist(data$total_LA_volume_sqrt_z)

data$total_Kcal_sqrt = sqrt(data$total_Kcal)
data$total_Kcal_sqrt_z = scale(data$total_Kcal_sqrt)[,1]
hist(data$total_Kcal_sqrt_z)

data$apache3_sqrt = sqrt(data$apache3)
hist(data$apache3_sqrt)
data$apache3_sqrt_z = scale(data$apache3_sqrt)[,1]
hist(data$apache3_sqrt_z)

data$charlson_sqrt = sqrt(data$charlson)
hist(data$charlson_sqrt)
data$charlson_sqrt_z = scale(data$charlson_sqrt)[,1]
hist(data$charlson_sqrt_z)

data$MAX_LACTATE_log = log(data$MAX_LACTATE)
data$MAX_LACTATE_log_z = scale(log(data$MAX_LACTATE))[,1]
data$MAX_TEMP_z = scale(data$MAX_TEMP)[,1]

hist(sqrt(data$MAX_TEMP))

data$total_n6_n3_ratio_4root_z = scale(sqrt(sqrt(data$total_n6_n3_ratio)))[,1]


# Max temp
## Model fit
lm.fit.temp = lm(MAX_TEMP_z ~ total_LA_volume_sqrt_z + age_at_admission + 
                   gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.temp)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "Total LA Volume", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Temp_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Temp_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.temp)
dev.off()


# Mac lactate
## Model fit
lm.fit.lactate = lm(MAX_LACTATE_log_z ~ total_LA_volume_sqrt_z + age_at_admission + 
                   gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.lactate)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "Total LA Volume", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Lactate_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Lactate_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.lactate)
dev.off()


# APACHE III
## Model fit
lm.fit.apache3 = lm(MAX_LACTATE_log_z ~ total_LA_volume_sqrt_z + age_at_admission + 
                      gender + race + charlson_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.apache3)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "Total LA Volume", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_APACHEIII_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_APACHEIII_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.apache3)
dev.off()




## Categorize protein and CHO by quantiles
quantile(data$total_protein, probs = seq(0,1,0.25))
data$total_protein_gr = NA
data$total_protein_gr = ifelse(data$total_protein<=quantile(data$total_protein, 0.25), 1,
                               ifelse(data$total_protein>quantile(data$total_protein, 0.25) & data$total_protein<=quantile(data$total_protein, 0.5), 2,
                                      ifelse(data$total_protein>quantile(data$total_protein, 0.5) & data$total_protein<=quantile(data$total_protein, 0.75), 3, 4))
                               )
data$total_protein_gr = factor(data$total_protein_gr)

quantile(data$total_CHO, probs = seq(0,1,0.25))
data$total_CHO_gr = NA
data$total_CHO_gr = ifelse(data$total_CHO<=quantile(data$total_CHO, 0.25), 1,
                               ifelse(data$total_CHO>quantile(data$total_CHO, 0.25) & data$total_CHO<=quantile(data$total_CHO, 0.5), 2,
                                      ifelse(data$total_CHO>quantile(data$total_CHO, 0.5) & data$total_CHO<=quantile(data$total_CHO, 0.75), 3, 4))
                           )
data$total_CHO_gr = factor(data$total_CHO_gr)



# Max temp
## Protein group
## Model fit
lm.fit.temp.protein = lm(MAX_TEMP_z ~ total_protein_gr + age_at_admission + 
                   gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.temp.protein)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "Protein group 2", "Protein group 3", "Protein group 4", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Temp_Protein_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Temp_Protein_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.temp.protein)
dev.off()


## CHO group
## Model fit
lm.fit.temp.CHO = lm(MAX_TEMP_z ~ total_CHO_gr + age_at_admission + 
                   gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.temp.CHO)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "CHO group 2", "CHO group 3", "CHO group 4", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Temp_CHO_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Temp_CHO_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.temp.CHO)
dev.off()


## n6/n3 ratio
## Model fit
lm.fit.temp.n6n3 = lm(MAX_TEMP_z ~ total_n6_n3_ratio_4root_z + age_at_admission + 
                   gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.temp.n6n3)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "n6/n3 ratio", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Temp_n6n3_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Temp_n6n3_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.temp.n6n3)
dev.off()



# Mac lactate
## Protein group
## Model fit
lm.fit.lactate.protein = lm(MAX_LACTATE_log_z ~ total_protein_gr + age_at_admission + 
                      gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.lactate.protein)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "Protein group 2", "Protein group 3", "Protein group 4", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Lactate_Protein_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Lactate_Protein_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.lactate.protein)
dev.off()


## CHO group
## Model fit
lm.fit.lactate.CHO = lm(MAX_LACTATE_log_z ~ total_CHO_gr + age_at_admission + 
                              gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.lactate.CHO)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "CHO group 2", "CHO group 3", "CHO group 4", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Lactate_CHO_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Lactate_CHO_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.lactate.CHO)
dev.off()


## n6/n3 ratio
## Model fit
lm.fit.lactate.n6n3 = lm(MAX_LACTATE_log_z ~ total_n6_n3_ratio_4root_z + age_at_admission + 
                              gender + race + charlson_sqrt_z + apache3_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.lactate.n6n3)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "n6/n3 ratio", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_Max_Lactate_n6n3_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_Max_Lactate_n6n3_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.lactate.n6n3)
dev.off()



# APACHE III
## Protein group
## Model fit
lm.fit.apache3.protein = lm(MAX_LACTATE_log_z ~ total_protein_gr + age_at_admission + 
                      gender + race + charlson_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.apache3.protein)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "Protein group 2", "Protein group 3", "Protein group 4", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_APACHEIII_Protein_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_APACHEIII_Protein_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.apache3.protein)
dev.off()


## CHO group
## Model fit
lm.fit.apache3.CHO = lm(MAX_LACTATE_log_z ~ total_CHO_gr + age_at_admission + 
                              gender + race + charlson_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.apache3.CHO)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "CHO group 2", "CHO group 3", "CHO group 4", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_APACHEIII_CHO_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_APACHEIII_CHO_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.apache3.CHO)
dev.off()


## n6/n3 ratio
## Model fit
lm.fit.apache3.n6n3 = lm(MAX_LACTATE_log_z ~ total_n6_n3_ratio_4root_z + age_at_admission + 
                          gender + race + charlson_sqrt_z + total_Kcal_sqrt_z, data)
lm_fit = summary(lm.fit.apache3.n6n3)

lm_fit_df = data.frame(variable = rownames(lm_fit$coefficients),
                       beta = lm_fit$coefficients[,c("Estimate")],
                       se = lm_fit$coefficients[,c("Std. Error")],
                       p = lm_fit$coefficients[,c("Pr(>|t|)")]) %>%
  mutate(lower.ci = beta - 1.96 * se,
         upper.ci = beta + 1.96 * se) %>%
  select(variable, beta, se, lower.ci, upper.ci, p)

names(lm_fit_df) = c("Variable", "Coefficient", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")
lm_fit_df$Variable = c("(Intercept)", "n6/n3 ratio", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "Total Kcal")

write.csv(lm_fit_df, "./Xinlei/Result/Exploratory_Analysis_APACHEIII_n6n3_lm_fit_result.csv")

## Model diagnosis
png(filename="./Xinlei/Result/Diagnosis/Exploratory_Analysis_APACHEIII_n6n3_lm_fit.png")
par(mfrow = c(2, 2))
plot(lm.fit.apache3.n6n3)
dev.off()


