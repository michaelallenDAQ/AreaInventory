# Storage and Transport (25-xx-xxx-xxx)}



#all of these SCCs project with the same data: statewide fuel usage. It is the combination of 
#"GovGas" and "Gasoline" tables, which sum up to total gasoline and gasohol consumption in the state.
gasoline <- projection_tables[["Gasoline"]]
govgas <- projection_tables[["GovGas"]]

#combine govgas and gasoline
all_gas<- left_join(gasoline, govgas, by = c("county", "year"))
all_gas <- all_gas %>% 
  #add together the two throughput columns
  mutate(unit = as.numeric(unit.x) + as.numeric(unit.y)) %>% 
  #drop our old throughputs
  select(-c("unit.x","unit.y"))

#okay, now do the tables


# Storage & Transport > Petroleum and Petroleum Product Storage > Residential Portable Gas Cans > Permeation
#declare the SCC
scc <- 2501011011
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Residential Portable Gas Cans > Evaporation (includes Diurnal losses)
scc <- 2501011012
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Residential Portable Gas Cans > Spillage During Transport
scc <- 2501011013
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Residential Portable Gas Cans > Refilling at the Pump - Vapor Displacement
scc <- 2501011014
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Residential Portable Gas Cans > Refilling at the Pump - Spillage
scc <- 2501011015
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Commercial Portable Gas Cans > Permeation
scc <- 2501012011
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Commercial Portable Gas Cans > Evaporation (includes Diurnal losses)
scc <- 2501012012
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Commercial Portable Gas Cans > Spillage During Transport
scc <- 2501012013
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Commercial Portable Gas Cans > Refilling at the Pump - Vapor Displacement
scc <- 2501012014
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Storage and Transport > Petroleum and Petroleum Product Storage > Commercial Portable Gas Cans > Refilling at the Pump - Spillage
scc <- 2501012015
#pull baseline EPA-estimated emissions from their NEI
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- project_baseline(base_table = temp_table, projection_table = all_gas)
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_25xx <- rbind(NEI_25xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


#remove the gas tables, we don't need them anymore
rm(gasoline, govgas, all_gas)