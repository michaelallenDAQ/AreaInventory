
# Miscellaneous Area Sources > Agriculture Production - Crops - as nonpoint > Agricultural Field Burning - whole field set on fire > Unspecified crop type and Burn Method
## This scc covers the burning of crop chaff, stock, and stubble from farming fields, whether piled or in situ.
scc <- 2801500000

# Total emissions - we just get emissions estimates from the NEI.
temp_table <- pull_baseline_from_nei(scc = scc)

# Project on agricultural employment
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls
# No pt source subtractions
# temp_table_project is our final temp_table
temp_table_final <- temp_table_project

# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)

# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Crops > Fertilizer Application > Miscellaneous Fertilizers
scc <- 2801700099

temp_table <- pull_baseline_from_nei(scc = scc)
# Project on agricultural employment
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls
# No pt source subtractions
# temp_table_project is our final temp_table
temp_table_final <- temp_table_project

# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)

# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agricultural Production - Livestock > Beef cattle - finishing operations on feedlots (drylots) > Dust Kicked-up by Hooves
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805001000

temp_table <- pull_baseline_from_nei(scc = scc)

# project on ag employment
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
# Set temp_table_final equal to temp_table_project
temp_table_final <- temp_table_project

# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)

# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Dairy Cattle > Dust Kicked-up by Hooves
# Get total emissions from NEI; project based on agricultural employment
scc <- 2805001010
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Broilers > Dust Kicked-up by Feet
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805001020
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agricultural Production - Livestock > Layers > Dust Kicked-up by Feet
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805001030
temp_table <- pull_baseline_from_nei(scc = scc)

# project on ag employment
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
# Set temp_table_final equal to temp_table_project
temp_table_final <- temp_table_project

# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)

# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Swine > Dust Kicked-up by Hooves
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805001040
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
temp_table_final <- temp_table_project
# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agricultral Production - Livestock > Turkeys > Dust Kicked-up by Feet
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805001050

temp_table <- pull_baseline_from_nei(scc = scc)

# project on ag employment
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
# Set temp_table_final equal to temp_table_project
temp_table_final <- temp_table_project

# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)

# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Beef cattle production composite > Not Elsewhere Classified
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805002000
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Poultry Waste Emissions > Not Elsewhere Classified (see also 28-05-007, -008, -009)
## Note: This scc was in the workbook as 2805030000, but all that it was was the emissions from 2805007100, so we are going to stop using 2805030000 and replace it with 2805007100.

# Get total emissions from NEI; project based on agricultural employment.
scc <- 2805007100
# Total emissions come from a model
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])
# No controls & no pt source subtractions for agricultural livestock sccs
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Poultry Production - broilers > Confinement
# Get total emissions from the NEI; project based on agricultural employment
scc <- 2805009100

temp_table <- pull_baseline_from_nei(scc = scc)

# project on ag employment
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
# Set temp_table_final equal to temp_table_project
temp_table_final <- temp_table_project

# add to final table
NEI_28xx <- rbind(NEI_28xx, temp_table_final)

# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Poultry production - turkeys > Confinement
# Get total emissions from NEI; project based on agricultural employment.
scc <- 2805010100

# Total emissions come from a model
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions for agricultural livestock sccs
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Dairy cattle composite > Not Elsewhere Classified
# Get total emissions from NEI; project based on agricultural employment
scc <- 2805018000
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this SCC
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Swine production composite > Not Elsewhere Classified (see also 28-05-039, -047, -053)
# Get total emissions from NEI; project based on agricultural employment
scc <- 2805025000
# Total emissions come from a model
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])
# No controls & no pt source subtractions for agricultural livestock sccs
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Horses and Ponies Waste Emissions > Not Elsewhere Classified
# Get total emissions from NEI; project based on agricultural employment.
scc <- 2805035000
# Total emissions come from a model
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])
# No controls & no pt source subtractions for agricultural livestock sccs
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Sheep and Lambs Waste Emissions > Total
# Get total emissions from NEI; project based on agricultural employment.
scc <- 2805040000
# Total emissions come from a model
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])
# No controls & no pt source subtractions for agricultural livestock sccs
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)


# Miscellaneous Area Sources > Agriculture Production - Livestock > Goats Waste Emissions > Not Elsewhere Classified
# Get total emissions from NEI; project based on agricultural employment.
scc <- 2805045000
# Total emissions come from a model
temp_table <- pull_baseline_from_nei(scc = scc)
temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["AgJobs"]])

# No controls & no pt source subtractions for agricultural livestock sccs
temp_table_final <- temp_table_project
NEI_28xx <- rbind(NEI_28xx, temp_table_final)
# remove the objects we created for this scc
rm(scc, temp_table, temp_table_project, temp_table_final)
