

#biogenic vegetation
scc <- 2701200000
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- 
  project_baseline(base_table = temp_table, projection_table = projection_tables[["Steady"]])
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_27xx <- merge_with_final_table(NEI_27xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


#biogenic vegetation/agriculture
#this one is just NOX, and technically has a component that accounts partially 
#for fertilizer application? Even though that is antrhopogenic? We call it 
#Biogenic for now, but one day they hopefully will parse out the anthropogenic
#portion of this SCC
scc <- 2701220000
temp_table <- pull_baseline_from_nei(scc)
#project the baseline based on all_gas fuel consumption
temp_table_project <- 
  project_baseline(base_table = temp_table, projection_table = projection_tables[["Steady"]])
# No controls
# No pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_27xx <- merge_with_final_table(NEI_27xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)
