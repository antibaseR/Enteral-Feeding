library(tidyverse)
library(survival)
library(patchwork)
library(survminer)
library(randomForestSRC)
options(scipen=999)

# Set the working directory
setwd("G:/Ivan")

# Load the data
data = readRDS("./Xinlei/Data/merged_data.rds")

# Create ICU days
data$icu_days = round(as.numeric(difftime(data$dt_icu_end_sh, data$dt_icu_start_sh, units="days")))

# Visualize the outcome
hist(data$icu_days)
mean(data$icu_days)
sd(data$icu_days)

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


glmdata = data %>%  
  select(icu_days, total_LA_volume_sqrt_z, 
         age_at_admission, gender, race, charlson_sqrt_z, apache3_sqrt_z, 
         total_Kcal_sqrt_z)

completeData = glmdata[complete.cases(glmdata), ]

glm.poisson.fit = glm(icu_days ~ ., family = poisson(link = "log"), glmdata)
library(MASS)
glm.nb.fit = glm.nb(icu_days ~ ., data = glmdata)
library(lmtest)
lrtest(glm.poisson.fit, glm.nb.fit) # nb glm with a better model

glm_fit = summary(glm.nb.fit)

glm_fit_df = data.frame(variable = rownames(glm_fit$coefficients),
                        beta = glm_fit$coefficients[,c("Estimate")],
                        se = glm_fit$coefficients[,c("Std. Error")],
                        p = glm_fit$coefficients[,c("Pr(>|z|)")]) %>%
  mutate(odds = exp(beta),
         lower.ci = exp(beta - 1.96 * se),
         upper.ci = exp(beta + 1.96 * se)) %>%
  dplyr::select(variable, beta, odds, se, lower.ci, upper.ci, p)

names(glm_fit_df) = c("Variable", "Coefficient", "Odds Ratio", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")

glm_fit_df$Variable = c("(Intercept)", "Total LA Volume", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")


# Model diagnosis
# Deviance Residuals
deviance_residuals = residuals(glm.nb.fit, type = "deviance")

# Pearson Residuals
pearson_residuals = residuals(glm.nb.fit, type = "pearson")

# Plot residuals to check for patterns
plot(deviance_residuals, main = "Deviance Residuals")
plot(pearson_residuals, main = "Pearson Residuals")

# Overdispersion
dispersion = sum(pearson_residuals^2) / df.residual(glm.nb.fit)
dispersion

# McFadden's pseudo R-squared
null_model = update(glm.nb.fit, . ~ 1)
pseudo_r_squared = 1 - (logLik(glm.nb.fit) / logLik(null_model))
pseudo_r_squared

# AIC
AIC(glm.nb.fit)



# Save the result
write.csv(glm_fit_df, "./Xinlei/Result/Secondary_Analysis_ICU_Days_glm_fit_result.csv")

detach("package:MASS", unload = TRUE)
