library(tidyverse)
library(table1)
setwd("G:/Ivan")

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

## 28-day mortality: 
### - if the death date is not NA and the icu to death days is > 28 -- survived
### - if the death date is not NA and the icu to death days is <= 28 -- dead
### - if the death date is NA that means we didn't observe the event time but we know the result, the data is censored, so the survival = dead_hosp and time = icu_to_discharge_days
data$dead_28 = ifelse(!(is.na(data$death_date_sh_true)) & data$survived_days>28, 0, 
                      ifelse(!(is.na(data$death_date_sh_true)) & data$survived_days<=28, 1, data$dead_hosp))


table1_data = data %>%
  mutate(gender = ifelse(gender == 0, "Female", "Male"),
         AKI = as.factor(ifelse(AKI_Category_subgroup == "no_AKI", "No AKI", "AKI")),
         dead_28 = as.factor(ifelse(dead_28 == 0, "Survived", "Dead"))) %>%
  select(age_at_admission, gender, apache3, charlson, total_LA_volume, total_Kcal, AKI, dead_28)
  

label(table1_data$age_at_admission) = "Age"
label(table1_data$gender) = "Gender"
label(table1_data$apache3) = "APACHE-III"
label(table1_data$charlson) = "Charlson"
label(table1_data$dead_28) = "28-day mortality"
label(table1_data$total_Kcal) = "Total Kcal"
label(table1_data$total_LA_volume) = "Total LA volume"


table1(~ .| AKI, data=table1_data)
table1(~ .| dead_28, data=table1_data)

table1_data_sub = table1_data %>%
  filter(total_Kcal >= median(total_Kcal))

table1(~ .| AKI, data=table1_data_sub)
table1(~ .| dead_28, data=table1_data_sub)