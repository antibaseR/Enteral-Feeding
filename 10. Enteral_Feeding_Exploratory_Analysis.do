use "G:\Ivan\Xinlei\Data\merged_data_long.dta", clear

egen apache3_sqrt_z = std(sqrt(apache3))
egen charlson_sqrt_z = std(sqrt(charlson))
egen MAX_LACTATE_long_z = std(log(MAX_LACTATE))
egen MAX_TEMP_z = std(MAX_TEMP)
egen Kcal_sqrt_z = std(sqrt(Kcal))

encode CHO_source, generate(CHO_source_num)


* Max temp ~ CHO source
mixed MAX_TEMP_z i.CHO_source_num days_after_admission age_at_admission i.gender i.race charlson_sqrt_z Kcal_sqrt_z || patientvisitid:, nolog