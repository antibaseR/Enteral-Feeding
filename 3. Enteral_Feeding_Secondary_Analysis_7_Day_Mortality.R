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


# Create 28-day mortality
## if the death date is earlier than the ICU admission date than replace it with NA
data$death_date_sh_true = ifelse(difftime(data$death_date_sh, data$dt_icu_start_sh, units="days")<0, NA, data$death_date_sh)
## if the survived subjects' death date is earlier than or the same as the hospital discharge date, then the subjects died in the hospital
data$discharge_to_death_days = difftime(data$death_date_sh_true, data$encounter_end_date_time_sh, unit="days")
data$dead_hosp = ifelse((!is.na(data$discharge_to_death_days) & data$discharge_to_death_days<=0 & data$dead==0), 1, as.numeric(as.character(data$dead)))
## if the dead subjects' death date is later than the hospital discharge date, the subjects survived in hospital
data$dead_hosp = ifelse((!is.na(data$discharge_to_death_days) & data$discharge_to_death_days>0 & data$dead==1), 0, as.numeric(as.character(data$dead)))

## number of days from ICU admission to death
data$icu_to_death_days = difftime(data$death_date_sh_true, data$dt_icu_start_sh, units="days")
## number of days from ICU admission to hospital discharge
data$icu_to_discharge_days = difftime(data$encounter_end_date_time_sh, data$dt_icu_start_sh, units="days")
## replace missing date with discharge date
data$survived_days = ifelse((is.na(data$death_date_sh_true)), data$icu_to_discharge_days, data$icu_to_death_days)

## 7-day mortality: 
### - if the death date is not NA and the icu to death days is > 7 -- survived
### - if the death date is not NA and the icu to death days is <= 7 -- dead
### - if the death date is NA that means we didn't observe the event time but we know the result, the data is censored, so the survival = dead_hosp and time = icu_to_discharge_days
data$dead_7 = ifelse(!(is.na(data$death_date_sh_true)) & data$survived_days>7, 0, 
                      ifelse(!(is.na(data$death_date_sh_true)) & data$survived_days<=7, 1, data$dead_hosp))
data$survival_time = ifelse(data$survived_days>7, 7, data$survived_days)


# Fit Cox ph model
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


coxdata = data %>%
  select(survival_time, dead_7, total_LA_volume_sqrt_z, 
         age_at_admission, gender, race, charlson_sqrt_z, apache3_sqrt_z, 
         total_Kcal_sqrt_z)
coxdata = coxdata[complete.cases(coxdata), ]

#set.seed(960725)
#obj = rfsrc(Surv(survival_time, dead_7) ~ ., coxdata,
#ntree = 1, nodesize = 5, nsplit = 10, importance = TRUE)
#tree = get.tree(obj, 1)
#tree

cox_fit = coxph(Surv(survival_time, dead_7) ~ total_LA_volume_sqrt_z + 
                  age_at_admission + gender + race + charlson_sqrt_z + apache3_sqrt_z + 
                  total_Kcal_sqrt_z, 
                data = coxdata)

summary(cox_fit)

cox_fit_df = data.frame(Variable = names(cox_fit$coefficients),
                        Coefficient = cox_fit$coefficients,
                        `Hazard Ratio` = exp(cox_fit$coefficients),
                        `Std. err.` = sqrt(diag(cox_fit$var)),
                        `Lower 95% CI` = exp(cox_fit$coefficients - 1.96 * sqrt(diag(cox_fit$var))),
                        `Upper 95% CI` = exp(cox_fit$coefficients + 1.96 * sqrt(diag(cox_fit$var))),
                        `P-value` = summary(cox_fit)$coefficient[, "Pr(>|z|)"]
)
names(cox_fit_df) = c("Variable", "Coefficient", "Hazard Ratio", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")

cox_fit_df$Variable = c("Total LA Volume", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")


# model diagnosis
## Martingale residuals
coxdata$resid_mart = residuals(cox_fit, type = "martingale")
## Cox-Snell residuals
coxdata$resid_coxsnell <- -(coxdata$resid_mart - coxdata$dead_7)
## Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, dead_7) ~ 1,
                      data    = coxdata,
                      ties    = c("efron","breslow","exact")[1])
## Nelson-Aalen estimator for baseline hazard (all covariates zero)
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)
## Cox ph assumption
test.ph = cox.zph(cox_fit)

# Plot
## Martingale plot
plt1 = ggplot(data = coxdata, mapping = aes(x = total_LA_volume_sqrt_z, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total LA volume",
       x = "Total LA volume",
       y = "Martingale residual") +
  theme_bw() + theme(legend.key = element_blank())

plt2 = ggplot(data = coxdata, mapping = aes(x = age_at_admission, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Age",
       x = "Age",
       y = "Martingale residual") +
  theme_bw() + theme(legend.key = element_blank())

plt3 = ggplot(data = coxdata, mapping = aes(x = charlson_sqrt_z, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Charlson",
       x = "Charlson",
       y = "Martingale residual") +
  theme_bw() + theme(legend.key = element_blank())

plt4 = ggplot(data = coxdata, mapping = aes(x = apache3_sqrt_z, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(title = "APACHE III",
       x = "APACHE III",
       y = "Martingale residual") +
  theme_bw() + theme(legend.key = element_blank())

plt5 = ggplot(data = coxdata, mapping = aes(x = total_Kcal_sqrt_z, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total Kcal",
       x = "Total Kcal",
       y = "Martingale residual") +
  theme_bw() + theme(legend.key = element_blank())


(plt1 + plt2 + plt3)/(plt4 + plt5)

ggsave("./Xinlei/Result/Diagnosis/Secondary_Analysis_7_Day_Mortality_martingale_plot.png", width = 10, height = 5)

## Overall fit
ggplot(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +
  geom_smooth(se = T) +
  scale_x_continuous(limit = c(0,2.5,.5)) +
  scale_y_continuous(limit = c(0,2.5,.5)) +
  labs(x = "Cox-Snell residuals as pseudo observed times",
       y = "Estimated cumulative hazard at pseudo observed times") +
  theme_bw() + theme(legend.key = element_blank())

### C-index
cindex = survConcordance(Surv(coxdata$survival_time, coxdata$dead_7) ~ predict(cox_fit))
print(cindex) # 0.77

ggsave("./Xinlei/Result/Diagnosis/Secondary_Analysis_7_Day_Mortality_cox_snell_plot.png", width = 8, height = 8)

## Ph assumption
ph.plt = ggcoxzph(test.ph)

pb1 = ggplot_build(ph.plt$`1`)
pb1$plot$labels$y = "Beta(t) for total LA volume"
pb1 = ggplot_gtable(pb1)

pb2 = ggplot_build(ph.plt$`2`)
pb2$plot$labels$y = "Beta(t) for age"
pb2 = ggplot_gtable(pb2)

pb3 = ggplot_build(ph.plt$`3`)
pb3$plot$labels$y = "Beta(t) for gender"
pb3 = ggplot_gtable(pb3)

pb4 = ggplot_build(ph.plt$`4`)
pb4$plot$labels$y = "Beta(t) for race"
pb4 = ggplot_gtable(pb4)

pb5 = ggplot_build(ph.plt$`5`)
pb5$plot$labels$y = "Beta(t) for Charlson"
pb5 = ggplot_gtable(pb5)

pb6 = ggplot_build(ph.plt$`6`)
pb6$plot$labels$y = "Beta(t) for APACHE III"
pb6 = ggplot_gtable(pb6)

pb7 = ggplot_build(ph.plt$`7`)
pb7$plot$labels$y = "Beta(t) for total Kcal"
pb7 = ggplot_gtable(pb7)


pb.plt1 = ggplotify::as.ggplot(pb1)
pb.plt2 = ggplotify::as.ggplot(pb2)
pb.plt3 = ggplotify::as.ggplot(pb3)
pb.plt4 = ggplotify::as.ggplot(pb4)
pb.plt5 = ggplotify::as.ggplot(pb5)
pb.plt6 = ggplotify::as.ggplot(pb6)
pb.plt7 = ggplotify::as.ggplot(pb7)

(pb.plt1 + pb.plt2 + pb.plt3 + pb.plt4 + pb.plt5 + pb.plt6 + pb.plt7) + 
  plot_layout(ncol = 3) +
  plot_annotation(
    theme=theme(plot.title=element_text(hjust=0.5, size = 20))
  )

ggsave("./Xinlei/Result/Diagnosis/Secondary_Analysis_7_Day_Mortality_ph_plot.png", width = 15, height = 15)

## Testing influential observations
ggcoxdiagnostics(cox_fit, type = "deviance", sline.se = F,
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggsave("./Xinlei/Result/Diagnosis/Secondary_Analysis_7_Day_Mortality_deviance_plot.png", width = 8, height = 8)


# Save the result
write.csv(cox_fit_df, "./Xinlei/Result/Secondary_Analysis_7_Day_Mortality_cox_fit_result.csv")

