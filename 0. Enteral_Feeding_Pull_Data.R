library(tidyverse)
# Set the working directory
setwd("G:/Ivan")

# Load the persistent AKI dataset
data_pAKI_raw = readRDS('G:/Persistent_AKI/Xinlei/Data/Data_Revised.rds')
renal_recovery_data = read_csv(paste("G:/Persistent_AKI/Xinlei/renal_recovery_data_imputed_", 1, ".csv", sep=""))

# variables that needs to be imputed
imp_var = c('age_at_admission',
            'gender',
            'race',
            'charlson',
            'apache3',
            'TOTAL_INPUTS_BEFORE_AKI',
            'TOTAL_SALINE_BEFORE_AKI',
            'TOTAL_OUTPUTS_BEFORE_AKI',
            'TOTAL_Urine_Output_BEFORE_AKI',
            'CUMULATIVE_BALANCE_BEFORE_AKI',
            'MAX_LACTATE',
            'MAX_CHLORIDE',
            "MAX_SODIUM",
            'MIN_ALBUMIN',
            'MIN_PLATELETS',
            'refCreat',
            'weight',
            'height',
            'BMI',
            'sofa_24',
            'MAX_DBP',
            'MAX_HR',
            'MIN_SpO2',
            'AVG_RR',
            'MAX_TEMP',
            'MIN_HGB',
            'MAX_TOTAL_BILI',
            'MAX_INR',
            'MAX_WBC',
            'MIN_PH',
            'BASELINE_eGFR',
            'MIN_PULSE_PRESS_BEFORE_AKI',
            'MAX_PULSE_PRESS_BEFORE_AKI',
            'MIN_MAP_BEFORE_AKI')

# Load the imputed persistent AKI dataset
impDat = readRDS("G:/Persistent_AKI/Xinlei/Data/Data_Imputed.rds")[[1]]

data_pAKI = data_pAKI_raw %>%
  select(-all_of(imp_var))

impDat_sub = impDat %>%
  arrange(patientid, patientvisitid) %>%
  select(patientid, patientvisitid, all_of(imp_var))

# Replace the imputed variables
data_pAKI_imp = merge(data_pAKI, impDat_sub, by = c("patientid", "patientvisitid"))
data_pAKI_imp_renal = merge(data_pAKI_imp, renal_recovery_data, by = c("patientid", "patientvisitid"))

# Save the imputed dataset
saveRDS(data_pAKI_imp_renal, "./Xinlei/Data/imputed_pAKI_data.rds")

# Connect to the server and download the data
library(DBI)
con = dbConnect(odbc::odbc(), "CCM_EHR", timeout = 10)

# this view contains the data for the nutrition formulas and also the times
data_nutrition = tbl(con, "vw_Ivan_Persistent_AKI_nutrition_formulas") %>% 
  collect()

# Read data
#sum(data_nutrition$patientvisitid %in% data_pAKI_raw$patientvisitid)

# List the product name in each dataset
nutrition_product = unique(data_nutrition$io_detail)

# LA transformation
data_transformed = data_nutrition %>%
  select(patientvisitid, io_detail, total_volume, days_after_admission) %>%
  mutate(product = NA,
         LA_metric = NA,
         Kcal_metric = NA,
         protein_metric = NA,
         CHO_metric = NA,
         CHO_source = NA,
         n6_n3_ratio_metric = NA)

product_df = tibble(patientvisitid = data_transformed$patientvisitid,
                    io_detail = data_transformed$io_detail,
                    Glucerna_1.5 = grepl("Glucerna 1.5", data_transformed$io_detail, ignore.case=T),
                    Peptide_1.5 = grepl("Peptide 1.5", data_transformed$io_detail, ignore.case=T),
                    Impact_1.5 = grepl("Impact 1.5", data_transformed$io_detail, ignore.case=T),
                    Isosource_1.5 = grepl("Isosource 1.5", data_transformed$io_detail, ignore.case=T),
                    Jevity_1.5 = grepl("Jevity 1.5", data_transformed$io_detail, ignore.case=T),
                    Jevity_1.2 = grepl("Jevity 1.2", data_transformed$io_detail, ignore.case=T),
                    Nepro_Carb_Steady = grepl("Nepro Carb", data_transformed$io_detail, ignore.case=T),
                    Peptamen_Intense_VHP = grepl("Peptamen Intense VHP", data_transformed$io_detail, ignore.case=T),
                    Promote = grepl("Promote", data_transformed$io_detail, ignore.case=T),
                    Vital_AF_1.2 = grepl("Vital AF 1.2", data_transformed$io_detail, ignore.case=T),
                    Vital_1.2_AF = grepl("Vital 1.2 AF", data_transformed$io_detail, ignore.case=T),
                    Vital_AF_Other_1.2 = grepl("Vital AF, Other: 1.2", data_transformed$io_detail, ignore.case=T),
                    Vital_High_Protein = grepl("Vital High Protein", data_transformed$io_detail, ignore.case=T),
                    Vital_Af_high_protein = grepl("Vital Af high protein", data_transformed$io_detail, ignore.case=T),
                    Vital_1.5 = grepl("Vital 1.5", data_transformed$io_detail, ignore.case=T),
                    Vivonex_RTF = grepl("Vivonex RTF", data_transformed$io_detail, ignore.case=T),
                    Nutren_2.0 = grepl("Nutren 2.0", data_transformed$io_detail, ignore.case=T),
                    osmolite_1.5 = grepl("osmolite 1.5", data_transformed$io_detail, ignore.case=T),
                    osmolite_1.2 = grepl("osmolite 1.2", data_transformed$io_detail, ignore.case=T),
                    Peptamen_1.5 = grepl("Peptamen 1.5", data_transformed$io_detail, ignore.case=T),
                    Jevity_1.0 = grepl("Jevity 1.0", data_transformed$io_detail, ignore.case=T)
)

x = product_df[, 3:ncol(product_df)]
product_df$sum = rowSums(x)
patientvisitid_multiple_product = product_df %>%
  filter(sum > 1)

data_transformed[grepl("Glucerna 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Glucerna 1.5 (Abbott)"
data_transformed[grepl("Glucerna 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 1.18
data_transformed[grepl("Glucerna 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Glucerna 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 8.3
data_transformed[grepl("Glucerna 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 13.3
data_transformed[grepl("Glucerna 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Glucerna 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 2.8

data_transformed[grepl("Peptide 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Impact Peptide 1.5 (Nestle)"
data_transformed[grepl("Impact 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Impact Peptide 1.5 (Nestle)"
data_transformed[grepl("Impact Peptide 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 0.735
data_transformed[grepl("Impact Peptide 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Impact Peptide 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 9.4
data_transformed[grepl("Impact Peptide 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 14
data_transformed[grepl("Impact Peptide 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Impact Peptide 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 1.5
data_transformed[grepl("Isosource 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Isosource 1.5 (Nestle)"
data_transformed[grepl("Isosource 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 0.9
data_transformed[grepl("Isosource 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Isosource 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 6.8
data_transformed[grepl("Isosource 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 17.6
data_transformed[grepl("Isosource 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Glucose syrup"
data_transformed[grepl("Isosource 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 2.4

data_transformed[grepl("Jevity 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Jevity 1.5 (Abbott)"
data_transformed[grepl("Jevity 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 0.52
data_transformed[grepl("Jevity 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Jevity 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 6.4
data_transformed[grepl("Jevity 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 21.6
data_transformed[grepl("Jevity 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Jevity 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 3.7

data_transformed[grepl("Jevity 1.2", data_transformed$io_detail, ignore.case=T), "product"] = "Jevity 1.2 (Abbott)"
data_transformed[grepl("Jevity 1.2", data_transformed$product, ignore.case=T), "LA_metric"] = 0.75
data_transformed[grepl("Jevity 1.2", data_transformed$product, ignore.case=T), "Kcal_metric"] = 120
data_transformed[grepl("Jevity 1.2", data_transformed$product, ignore.case=T), "protein_metric"] = 5.6
data_transformed[grepl("Jevity 1.2", data_transformed$product, ignore.case=T), "CHO_metric"] = 16.9
data_transformed[grepl("Jevity 1.2", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Jevity 1.2", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 6.7

data_transformed[grepl("Nepro Carb", data_transformed$io_detail, ignore.case=T), "product"] = "Nepro Carb Steady (Abbott)"
data_transformed[grepl("Nepro Carb Steady", data_transformed$product, ignore.case=T), "LA_metric"] = 1.43
data_transformed[grepl("Nepro Carb Steady", data_transformed$product, ignore.case=T), "Kcal_metric"] = 180
data_transformed[grepl("Nepro Carb Steady", data_transformed$product, ignore.case=T), "protein_metric"] = 8.1
data_transformed[grepl("Nepro Carb Steady", data_transformed$product, ignore.case=T), "CHO_metric"] = 16
data_transformed[grepl("Nepro Carb Steady", data_transformed$product, ignore.case=T), "CHO_source"] = "Corn syrup solids"
data_transformed[grepl("Nepro Carb Steady", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 5.2

data_transformed[grepl("Peptamen Intense VHP", data_transformed$io_detail, ignore.case=T), "product"] = "Peptamen Intense VHP (Nestle)"
data_transformed[grepl("Peptamen Intense VHP", data_transformed$product, ignore.case=T), "LA_metric"] = 0.4
data_transformed[grepl("Peptamen Intense VHP", data_transformed$product, ignore.case=T), "Kcal_metric"] = 100
data_transformed[grepl("Peptamen Intense VHP", data_transformed$product, ignore.case=T), "protein_metric"] = 9.2
data_transformed[grepl("Peptamen Intense VHP", data_transformed$product, ignore.case=T), "CHO_metric"] = 7.6
data_transformed[grepl("Peptamen Intense VHP", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Peptamen Intense VHP", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 1.6

data_transformed[grepl("Promote", data_transformed$io_detail, ignore.case=T), "product"] = "Promote (Abbott)"
data_transformed[grepl("Promote", data_transformed$product, ignore.case=T), "LA_metric"] = 1
data_transformed[grepl("Promote", data_transformed$product, ignore.case=T), "Kcal_metric"] = 100
data_transformed[grepl("Promote", data_transformed$product, ignore.case=T), "protein_metric"] = 6.3
data_transformed[grepl("Promote", data_transformed$product, ignore.case=T), "CHO_metric"] = 13
data_transformed[grepl("Promote", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Promote", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 10

data_transformed[grepl("Vital AF 1.2", data_transformed$io_detail, ignore.case=T), "product"] = "Vital AF 1.2 (Abbott)"
data_transformed[grepl("Vital 1.2 AF", data_transformed$io_detail, ignore.case=T), "product"] = "Vital AF 1.2 (Abbott)"
data_transformed[grepl("Vital AF, Other: 1.2", data_transformed$io_detail, ignore.case=T), "product"] = "Vital AF 1.2 (Abbott)"
data_transformed[grepl("Vital AF 1.2", data_transformed$product, ignore.case=T), "LA_metric"] = 0
data_transformed[grepl("Vital AF 1.2", data_transformed$product, ignore.case=T), "Kcal_metric"] = 120
data_transformed[grepl("Vital AF 1.2", data_transformed$product, ignore.case=T), "protein_metric"] = 7.5
data_transformed[grepl("Vital AF 1.2", data_transformed$product, ignore.case=T), "CHO_metric"] = 11.1
data_transformed[grepl("Vital AF 1.2", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Vital AF 1.2", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 0

data_transformed[grepl("Vital High Protein", data_transformed$io_detail, ignore.case=T), "product"] = "Vital High Protein (Abbott)"
data_transformed[grepl("Vital Af high protein", data_transformed$io_detail, ignore.case=T), "product"] = "Vital High Protein (Abbott)"
data_transformed[grepl("Vital High Protein", data_transformed$product, ignore.case=T), "LA_metric"] = 0
data_transformed[grepl("Vital High Protein", data_transformed$product, ignore.case=T), "Kcal_metric"] = 100
data_transformed[grepl("Vital High Protein", data_transformed$product, ignore.case=T), "protein_metric"] = 8.7
data_transformed[grepl("Vital High Protein", data_transformed$product, ignore.case=T), "CHO_metric"] = 11.1
data_transformed[grepl("Vital High Protein", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Vital High Protein", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 0

data_transformed[grepl("Vital 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Vital 1.5 (Abbott)"
data_transformed[grepl("Vital 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 0.26
data_transformed[grepl("Vital 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Vital 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 6.8
data_transformed[grepl("Vital 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 18.4
data_transformed[grepl("Vital 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Vital 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 2.2

data_transformed[grepl("Vivonex RTF", data_transformed$io_detail, ignore.case=T), "product"] = "Vivonex RTF (Nestle)"
data_transformed[grepl("Vivonex RTF", data_transformed$product, ignore.case=T), "LA_metric"] = 0
data_transformed[grepl("Vivonex RTF", data_transformed$product, ignore.case=T), "Kcal_metric"] = 100
data_transformed[grepl("Vivonex RTF", data_transformed$product, ignore.case=T), "protein_metric"] = 5
data_transformed[grepl("Vivonex RTF", data_transformed$product, ignore.case=T), "CHO_metric"] = 17.6
data_transformed[grepl("Vivonex RTF", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Vivonex RTF", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 8

data_transformed[grepl("Nutren 2.0", data_transformed$io_detail, ignore.case=T), "product"] = "Nutren 2.0 (Nestle)"
data_transformed[grepl("Nutren 2.0", data_transformed$product, ignore.case=T), "LA_metric"] = 0.68
data_transformed[grepl("Nutren 2.0", data_transformed$product, ignore.case=T), "Kcal_metric"] = 200
data_transformed[grepl("Nutren 2.0", data_transformed$product, ignore.case=T), "protein_metric"] = 8
data_transformed[grepl("Nutren 2.0", data_transformed$product, ignore.case=T), "CHO_metric"] = 20
data_transformed[grepl("Nutren 2.0", data_transformed$product, ignore.case=T), "CHO_source"] = "Corn syrup"
data_transformed[grepl("Nutren 2.0", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 2.5

data_transformed[grepl("osmolite 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Osmolite 1.5 (Abbott)"
data_transformed[grepl("Osmolite 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 0
data_transformed[grepl("Osmolite 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Osmolite 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 6.3
data_transformed[grepl("Osmolite 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 20.4
data_transformed[grepl("Osmolite 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Osmolite 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 0

data_transformed[grepl("osmolite 1.2", data_transformed$io_detail, ignore.case=T), "product"] = "Osmolite 1.2 (Abbott)"
data_transformed[grepl("Osmolite 1.2", data_transformed$product, ignore.case=T), "LA_metric"] = 0
data_transformed[grepl("Osmolite 1.2", data_transformed$product, ignore.case=T), "Kcal_metric"] = 120
data_transformed[grepl("Osmolite 1.2", data_transformed$product, ignore.case=T), "protein_metric"] = 5
data_transformed[grepl("Osmolite 1.2", data_transformed$product, ignore.case=T), "CHO_metric"] = 16.3
data_transformed[grepl("Osmolite 1.2", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Osmolite 1.2", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 0

data_transformed[grepl("Peptamen 1.5", data_transformed$io_detail, ignore.case=T), "product"] = "Peptamen 1.5 (Nestle)"
data_transformed[grepl("Peptamen 1.5", data_transformed$product, ignore.case=T), "LA_metric"] = 0.8064
data_transformed[grepl("Peptamen 1.5", data_transformed$product, ignore.case=T), "Kcal_metric"] = 150
data_transformed[grepl("Peptamen 1.5", data_transformed$product, ignore.case=T), "protein_metric"] = 6.8
data_transformed[grepl("Peptamen 1.5", data_transformed$product, ignore.case=T), "CHO_metric"] = 18.8
data_transformed[grepl("Peptamen 1.5", data_transformed$product, ignore.case=T), "CHO_source"] = "Maltodextrin"
data_transformed[grepl("Peptamen 1.5", data_transformed$product, ignore.case=T), "n6_n3_ratio_metric"] = 7.9

check = data_transformed %>%
  filter(!(patientvisitid %in% patientvisitid_multiple_product$patientvisitid)) %>%
  select(io_detail, product) %>%
  arrange(io_detail) %>%
  distinct(io_detail, .keep_all = TRUE)

# Find the overlapping groups
write.csv(check, "./Xinlei/Result/product.csv")

# Calculate the nutrition value for each day
data_transformed = data_transformed %>%
  filter(!is.na(product)) %>%
  filter(!(patientvisitid %in% patientvisitid_multiple_product$patientvisitid))

data_transformed$LA = data_transformed$total_volume/100 * data_transformed$LA_metric
data_transformed$Kcal = data_transformed$total_volume/100 * data_transformed$Kcal_metric
data_transformed$protein = data_transformed$total_volume/100 * data_transformed$protein_metric
data_transformed$CHO = data_transformed$total_volume/100 * data_transformed$CHO_metric
data_transformed$n6_n3_ratio = data_transformed$total_volume/100 * data_transformed$n6_n3_ratio_metric

# Calculate the total nutrition value for each patient
data_nutrition_total = data_transformed %>%
  filter(total_volume>0) %>%
  group_by(patientvisitid) %>%
  summarise(total_LA_volume = sum(LA),
            total_Kcal = sum(Kcal),
            total_protein = sum(protein),
            total_CHO = sum(CHO),
            total_n6_n3_ratio = sum(n6_n3_ratio))

saveRDS(data_nutrition_total, "./Xinlei/Data/tidied_nutrition_data")

# Load the imputed persistent AKI data
data_pAKI_imp = readRDS("./Xinlei/Data/imputed_pAKI_data.rds")

data_pAKI_imp_sub = data_pAKI_imp %>%
  select(patientvisitid, 
         encounter_start_date_time_sh, encounter_end_date_time_sh, 
         dt_icu_start_sh, dt_icu_end_sh,
         age_at_admission, gender, race, BMI, apache3, charlson,
         dead, death_date_sh, rrt_discharge, renal_recov,
         diabetes, SEPTIC_SHOCK, SEPSIS, vaso_inotr, MV_DAYS, 
         MAX_LACTATE, MAX_TEMP, 
         AKI_Category_subgroup)

# Merge the pAKI data with the LA data
data = merge(data_pAKI_imp_sub, data_nutrition_total, by = "patientvisitid")
data_long = merge(data_pAKI_imp_sub, data_transformed, by = "patientvisitid")
saveRDS(data, "./Xinlei/Data/merged_data.rds")
saveRDS(data_long, "./Xinlei/Data/merged_data_long.rds")

library(haven)
write_dta(data_long, "./Xinlei/Data/merged_data_long.dta")