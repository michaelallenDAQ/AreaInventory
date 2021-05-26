#  This pull functions you will use later in the script. This section gets you the functions to:
#  (project_baseline, XXNEED BACK PROJECTIONXX)
#  Take baseline data, and project it backward or forward  to scale proportionally with a reference projection table, like population, or employment data
#
#  (pull_raw_ww)
#  Pull raw Wagon Wheel data, which will be used to pull EFs
#
#  (pull_ww)
#  Pull filtered Wagon Wheel data, which only has TPY emissions for SCCs
#
#  (pull_nei)
#  Pull nei data, format it to match formatting of ww data
#
#  (add_controls)
#  Add control factors to any specific year/SCC/County combination. Right now, you can not single out specific pollutants
#
#  (pull_efs_from_ww)
#  pull all CAPS/HAPS associated with an SCC, and relevant emission values, from the raw WW table
#
#  (code_to_pol, pol_to_code)
#  that converts a county name into a FIP, or converts a pollutant code into plain language
#
#  (pull_input_tables)
#  pull tables from the input tables excel document into a list
#
#  and more. This section should not pull any variables. It should only generate functions.
#
#
#
#

# FIP-County crosswalk table
counties_fips <- data.frame(county = c("Beaver", "Box Elder", "Cache", "Carbon",
                                       "Daggett", "Davis", "Duchesne", "Emery", 
                                       "Garfield", "Grand", "Iron", "Juab", 
                                       "Kane", "Millard", "Morgan", "Piute", 
                                       "Rich", "Salt Lake", "San Juan", 
                                       "Sanpete", "Sevier", "Summit", "Tooele", 
                                       "Uintah", "Utah", "Wasatch", 
                                       "Washington", "Wayne", "Weber"),
                            fip = seq.int(49001,49057,2))

#This function pulls the entire raw WW, and then changes all
#final emission values to TPY (if they're LB/year, divide by 2000)
pull_ww <- function(ww_path) {
  suppressMessages(ww <- read_csv(ww_path))
  #go through each row. If I find LB, divide the emissions by 2,000 and change to TPY
  ww <- ww %>%
    mutate(TotalEmissions = ifelse(EmissionsUnitofMeasureCode == 'LB',
                                   TotalEmissions/2000,
                                   TotalEmissions )) %>%
    mutate(EmissionsUnitofMeasureCode = ifelse(EmissionsUnitofMeasureCode == 'LB',
                                               'TON',
                                               EmissionsUnitofMeasureCode))
  return(ww)
}

#This converts a FIPS code to a county name
fip_to_county <-
  function(FIP){
    ref_counties <- strsplit(c('Beaver, Box Elder, Cache, Carbon, Daggett, Davis, Duchesne, Emery, Garfield, Grand, Iron, Juab, Kane, Millard, Morgan, Piute, Rich, Salt Lake, San Juan, Sanpete, Sevier, Summit, Tooele, Uintah, Utah, Wasatch, Washington, Wayne, Weber'),', ')
    ref_fips <- seq.int(49001,49057,2)
    df <- data.frame(ref_counties,ref_fips)
    names(df) <- c('county', 'fip')
    
    out_ct <- filter(df,fip==FIP)[[1]]
    return(out_ct)
  }

#this converts a county name to a FIPS code
county_to_fip <-
  function(ct_name){
    #produce conversion table to draw from
    ref_counties <- strsplit(c('Beaver, Box Elder, Cache, Carbon, Daggett, Davis, Duchesne, Emery, Garfield, Grand, Iron, Juab, Kane, Millard, Morgan, Piute, Rich, Salt Lake, San Juan, Sanpete, Sevier, Summit, Tooele, Uintah, Utah, Wasatch, Washington, Wayne, Weber'),', ')
    ref_fips <- seq.int(49001,49057,2)
    df <- data.frame(ref_counties,ref_fips)
    names(df) <- c('county', 'fip')
    
    #make it lowercase just to be sure
    ct_name <- tolower(ct_name)
    ct_name <- str_remove(ct_name,' county')
    ct_name <- .simpleCap(ct_name)
    out_fip <- filter(df,county==ct_name)[[2]]
    
    return(out_fip)
  }

#capitalize first letter of every word in a sentence
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# Function to pull NEI data from the NEI csv in ref_workbooks
# Put in the nei path and the year the data is from
pull_nei <- function(nei_path, year) {
  
  # Read in the NEI data
  # Suppress messages
  suppressWarnings(suppressMessages(nei <- read_csv(nei_path)))
  
  # Delete the "problems" and "spec" attributes. These don't matter.
  attr(nei, 'problems') <- NULL
  attr(nei, 'spec') <- NULL
  
  # Filter 2017 NEI to only have UT data
  nei <- nei %>%
    filter(`fips state code` == '49') 
  
  # Clean up NEI data
  nei <- nei %>%
    # Select only these variables
    select(`fips code`, county, scc, `pollutant code`, `pollutant desc`, 
           `total emissions`, `emissions uom`) %>%
    # Rename the variables to match the naming convention of the WW
    rename("StateAndCountyFIPSCode" = `fips code`,
           "County" = county,
           "SourceClassificationCode" = scc,
           "PollutantCode" = `pollutant code`,
           "Pollutant" = `pollutant desc`,
           "TotalEmissions" = `total emissions`,
           "EmissionsUnitofMeasureCode" = `emissions uom`)

  nei$year <- year

  # Go through each row. If I find LB in the emissions unit of measure code, 
  # divide the emissions by 2,000 and change to TPY.
  nei <- nei %>%
    mutate(TotalEmissions = ifelse(EmissionsUnitofMeasureCode == 'LB',
                                   TotalEmissions/2000,
                                   TotalEmissions )) %>%
    mutate(EmissionsUnitofMeasureCode = ifelse(EmissionsUnitofMeasureCode == 'LB',
                                               'TON',
                                               EmissionsUnitofMeasureCode))
  
  
  return(nei)
}

#' Pull input tables
#' 
#' Pull all of the input tables from the input excel file into a list with
#' each list element representing the data on a specific sheet
#' 
#' @param file_path Pathway to the excel file with input tables
#' @return list with each element the data stored on an input table sheet
#' @export
pull_input_tables <- function(file_path) {
  
  # first get the name of all of the sheets in the input file
  sheets <- readxl::excel_sheets(file_path)
  
  # now, for each of the sheets, read in the data on that particular sheet and
  # assign to a new element in the list
  input_list = lapply(sheets, function(sheet_name) {
    
    # Skip the first 10 rows in each sheet; this is where the table
    # documentation is stored. The data starts on row 11.
    readxl::read_excel(file_path, sheet = sheet_name, skip = 10)
  }
  )
  
  # Name each element in the list the name of the sheet that the data comes from
  names(input_list) <- sheets
  
  # return the list
  return(input_list)
}

#' Create projection tables
#' 
#' Pull projection tables from the input_tables list, format them correctly for
#' use with the project function
#' 
#' We assume that the projection tables will be in one of three formats:
#' * the first column is "county" and contains the names of the counties
#' * the first column is "month" and contains each month in the year, while
#' the year columns contain the estimated values for each month in the year;
#' we will sum up the values for all of the months to get the total estimate for
#' the entire year and then assume that every county has the same projection
#' * if neither of those is the case, we'll assume that each row in the table
#' needs to become its own projection table; we'll break that table up so that
#' each row becomes its own projection table, with values duplicated over all
#' counties
#' 
#' @param table_names Vector of sheet names from input_tables that we want to 
#' convert to projection tables
#' @return list where each element is a projection table
#' @export
pull_projection_tables <- function(table_names, input_list = input_tables) {
  
  # what are the tables we are going to use for projection?
  tables_to_project <- input_list[table_names]
  
  # create an empty list we'll use for storing the projection tables
  projection_tables <- list()

  # now, for each of the tables_to_project, read in the data on that particular 
  # sheet and assign to an element(s) in the projection_tables list
  # for each element in the tables_to_project list, run through this loop
  for(i in 1:length(tables_to_project)) {
    
    # assign the table of interest to "table"
    table <- tables_to_project[[i]]
    
    # check if the first column in the table of interest is named "county"
    if(names(table)[1] == "county") {
      
      # Switch the values in the first column in the table from the county names
      # to their fips
      renamed_table <- table
      ncounties <- dim(table)[1]
      for (j in 1:ncounties) {
        renamed_table[j, 1] <-
          as.factor(county_to_fip(table[[j, 1]]))
      }
      
      # now pivot_longer, so the years are all in one column
      renamed_table <- pivot_longer(renamed_table, cols = colnames(table)[-1],
                                   names_to = "year",
                                   values_to = "unit")
      
      # add an element to the projection_tables list for this table
      projection_tables[names(tables_to_project[i])] <- list(renamed_table)
      
      # remove the renamed_table object from the environment
      rm(renamed_table)
      
      # if the first column is not named county, is it named month?
    } else if(names(table)[1] == "month") {
      
      # if it is, we need to sum over all the months to get the total for the
      # year
      # get the sum and rbind it onto the bottom, call that row "sum"
      table <- rbind(table, c("sum", colSums(table[,-1])))
      
      # now only save the "sum" row that we just rbinded
      table <- filter(table, month == "sum")
      
      # repeat that row 29 times(the number of counties in Utah, which is the
      # same as the number of rows in our counties_fips data frame above)  
      table <- table[rep(seq_len(nrow(table)), each = nrow(counties_fips)), ]
      
      # Add a column to the front of the data frame called county, which
      # contains the fips code for every county. Delete the "month" column.
      table <- cbind(county = counties_fips$fip, table[,-1])
      
      # now pivot_longer, so the years are all in one column
      table <- pivot_longer(table, cols = colnames(table)[-1],
                            names_to = "year",
                            values_to = "unit")
      
      # add an element to the projection_tables list for this table
      projection_tables[names(tables_to_project[i])] <- list(table)
      
      # now the final condition - we have a table that has different rows that
      # each need to be made into their own tables
    } else {
      # for every row in the data frame, we need to create a separate table.
      # let's store these separate tables in a list
      temp_list <- list()
      
      for(j in 1:nrow(table)) {
        temp_list[j] <- list(table[j,])
      }
      
      # let's assign the names of the tables in temp_list to the value stored
      # in the first column
      names(temp_list) <- unlist(table[,1])
      
      # now we need to do something similar to above - repeat every row in each
      # table in the temp_list 29 times and assign the name of the fips
      temp_list <- lapply(temp_list, function(temp_table) {
        # repeat that row 29 times(the number of counties in Utah, which is the
        # same as the number of rows in our counties_fips data frame above)  
        temp_table <- temp_table[rep(seq_len(nrow(temp_table)), 
                                     each = nrow(counties_fips)), ]
        
        # Add a column to the front of the data frame called county, which is 
        # the different fips codes. Delete the first column.
        temp_table <- cbind(county = counties_fips$fip, temp_table[,-1])
        
        # now pivot_longer, so the years are all in one column
        temp_table <- pivot_longer(temp_table, cols = colnames(temp_table)[-1],
                                   names_to = "year",
                                   values_to = "unit")
      }
      )
        
      # now we can assign each of those tables in the temp_list to a separate
      # projection table in our projection_tables list
      for(j in 1:length(temp_list)) {
        # the name of the table will be the name of the list element that the
        # table came from, then underscore, then the name in the first column
        # of the row that the table was built from
        projection_tables[paste0(names(tables_to_project)[i], 
                                 "_", names(temp_list)[j])] <- 
          temp_list[j]
      }
      
      # remove temp_list from the environment
      rm(temp_list)
    }
    
  }
  
  # remove table from the environment
  rm(table)
  
  # return the list of projection tables that we made
  return(projection_tables)
}


#EPA data often give PM in many forms. PM-PRI (primary)is the sum
#of CON (condensible) and FIL (filterable). All condensible PM is 2.5
#
# Old data we gathered do not make this distinction, but I'd like to.
# When comparing new data to old data, you have to consolidate the new data
# for a direct comparison.
consolidate_pm <- function(pols){
  #the primary values are the whole values I'll keep. Everything else is to
  #be removed.
  remove <- c('PM-CON','PM10-FIL','PM25-FIL')
  pols <- pols[!(pols$pollutant %in% remove),]
  pols$pollutant[pols$pollutant == 'PM10-PRI'] <- 'PM10'
  pols$pollutant[pols$pollutant == 'PM25-PRI'] <- 'PM25'
  return(pols)
}

#there are three main xylenes. Old data bundled them together. We can to if we
#want to compare data
consolidate_xylenes <- function(pollutant_table, sccs) {
  xylene_codes <- c(95476, 106423, 108383)
  #there are three xylenes
  #O-Xylene (95476)
  #P-Xylene (106423)
  #M-Xylene (108383)
  #and Xylenes, Mixed Isomers, which is the sum of the three above (1330207)
  
  #this function consolidates the O, P and M into the summed code
  for (cur_scc in sccs) {
    target_data <- pollutant_table %>% filter(SCC == cur_scc)
    
    #test if the total xylenes is there
    if (1330207 %in% target_data$pollutant) {
      cat(cur_scc, 'already has total xylenes\n')
      return(pollutant_table)
    }
    
    if (!95476  %in% target_data$pollutant &
        !106423 %in% target_data$pollutant &
        !108383 %in% target_data$pollutant) {
      cat(cur_scc, 'does not have any of the secondary xylenes\n')
      return(pollutant_table)
    }
    #so now we know we have an appropriate scc emission profile
    #figure out what counties, years we are working with
    all_fips <- unique(target_data$FIPS)
    all_years <- unique(target_data$year)
    for (cur_fip in all_fips) {
      for (cur_year in all_years) {
        smaller_target_data <- target_data %>%
          filter(FIPS == cur_fip & year == cur_year &
                   pollutant %in% xylene_codes)
        tot_xylenes <- sum(smaller_target_data$TPY)
        new_row <- c(cur_fip, cur_scc, cur_year, 1330207, tot_xylenes)
        pollutant_table <- rbind(pollutant_table, new_row)
      }
    }
    
  }
  pollutant_table <- pollutant_table %>%
    filter(!pollutant %in% xylene_codes)
  return(pollutant_table)
  
}

#again, for comparisons. This pulls the model data from any given
#inventory workbook
pull_excel_model_data <- function(model_path, base_year = start_year) {
  #read in the sheet
  model_pols <-
    read_xlsx(model_path, sheet = 'PM25ModelData', skip = 3)
  #add a column for year
  model_pols <- mutate(model_pols, year = base_year)
  #rename TPY column and pollutants column
  model_pols <- model_pols %>%
    rename(TPY = 'Tons/year') %>%
    rename(pollutant = Pollutant)
  #rename all cols and turn SOx into SO2
  model_pols <- model_pols[c('FIPS', 'SCC', 'year', 'pollutant', 'TPY')]
  model_pols$pollutant[model_pols$pollutant == 'SOX'] <- 'SO2'
  return(model_pols)
}


# give each layer description of an SCC, as well as its status
#(active or retired)
describe <- function(scc_val, ref_table = sccref) {
  #pull information from this sccdata row
  sccdata <- sccref %>% filter(SCC == scc_val)
  
  l1 <- sccdata[['scc level one']]
  l2 <- sccdata[['scc level two']]
  l3 <- sccdata[['scc level three']]
  l4 <- sccdata[['scc level four']]
  status <- sccdata[['status']]
  cat('SCC:',scc_val,'\n')
  cat('Level 1:', l1,'\n')
  cat('Level 2:', l2,'\n')
  cat('Level 3:', l3,'\n')
  cat('Level 4:', l4,'\n')
  cat('Status:', status,'\n')
  
}

#convert a pollutant code to its plaintext name
code_to_pol <- function(code,t_table = pollutant_code_table){
  ref_data <- t_table %>% filter(raw == code)
  name <- ref_data[[1, 2]]
  return(name)
}

#convert a plaintext pollutant name over to its pollutant code
#this returns an integer, if you wanna return a string code.... idk
pol_to_code <- function(pol,t_table = pollutant_code_table){
  ref_data <- t_table %>% filter(name == pol)
  code <- as.integer(ref_data[[1,1]])
  return(code)
}

#this pulls the pollutant code table for translating codes to plaintext
#and vice versa
pull_translate_table <- function(pol_ref_path){
  translate_table <- read_xlsx(pol_ref_path)
  translate_table <- select(translate_table, c('Pollutant Code', 'Description'))
  colnames(translate_table) <- c('raw', 'name')
  return(translate_table)
}

#this take in a pollution inventory, and targets an scc with a set of counties.
#It phases in a control factor to that SCC from a start year up to an end year.
#start_year: last year where emissions are uncontrolled, at 100% of their values
#end_year: first year where emission controls are completely phased in.
#end_val: Final control value. 0.3 would mean emissions are 70% reduced.
#pollutants: you can input specific pollutants to control. The default assumption is that
#  you want to control all of them.
add_manual_controls <-
  function(raw_proj_data, sccs,counties, st_year, e_year,end_val,pollutants = NULL,is_pt_sources = FALSE){
    #we later will filter our list down to only control certain pollutants
    #if we haven't singled out any pollutants, make sure we include everything 
    #in 'target_pollutants'
    if (is.null(pollutants)) {
      pollutants <- unique(raw_proj_data$pollutant)
    }
    
    drops <-
      e_year - st_year #number of years you ramp up controls
    distance <- 1 - end_val # total control factor increase
    step <- distance / drops #the amount you drop emissions per year
    
    cleaned_projected_data <- raw_proj_data %>%
      #complex mutate below. making a new column of TPY and ultimately
      #removing the old one
      mutate(
        new_TPY = ifelse(
          # take the raw data. If you don't have the right SCCs or counties, leave TPY
          # unchanged. If you DO have the right SCCs & counties....
          (SCC %in% sccs & FIPS %in% counties & pollutant %in% pollutants),
          ifelse(year > e_year,
                 #If you are after the end_year. implement the full control efficiency
                 TPY * end_val,
                 ifelse(year > st_year,
                        #If you are between the start and end year, implement a control that linearly
                        #travels from start_year+1 to end_year.
                        TPY * (1 - step * (as.numeric(year) - st_year)),
                        #If you are before or equal to the start year, implement no controls.
                        TPY)),
          #if you are not the target counties/pollutant/scc, remain unchanged
          TPY
        )) %>% 
      #remove the old, unedited TPY and put the new one in its place.
      select(-TPY) %>% rename(TPY = new_TPY)
    return(cleaned_projected_data)
  }

#this take in a pollution inventory, and targets an scc with a set of counties.
#It phases in a control factor to that SCC from a start year up to an end year.
#start_year: last year where emissions are uncontrolled, at 100% of their values
#end_year: first year where emission controls are completely phased in.
#end_val: Final control value. 0.3 would mean emissions are 70% reduced.
#pollutants: you can input specific pollutants to control. The default assumption is that
#  you want to control all of them.


#' Add controls
#' 
#' Adjust emissions estimates with applicable controls
#'
#' @param raw_proj_data data frame containing columns for FIPS, SCC, year,
#' pollutant, and TPY
#' @param controls_ws controls table
#' @param use_ww Default is TRUE. This is only relevant if any of the controls
#' applicable to sccs in raw_proj_data are EFDependent. If so, and we want to
#' pull the default emissions factors from WW, leave this as TRUE. If FALSE,
#' we will only compare EFs that are listed in a table supplied to the
#' current_efs parameter. If the EF is not listed in the table supplied to
#' current_efs, we will assume that the current EF is equal to the original EF
#' and give a warning noting which EFs we were unable to compare with current 
#' EFs.
#' @param current_efs (optional) This is only relevant if any of the controls 
#' applicable to sccs in raw_proj_data are EFDependent. If so, and we want to 
#' manually identify the emissions factors we currently use, so input a table 
#' here containing a FIPS, SCC, pollutant, EmissionsFactor, and EFNumerator
#' column. FIPS is the FIPS code of the county with the unique emissions factor,
#' SCC is the SCC code for the control, pollutant is the pollutant the control
#' applies to, emissionsfactor is the emissionsfactor value, and EFNumerator is
#' the numerator for the emissions factor (likely one of LB or TON).
#' It is not necessary to identify the emissions factors for every scc and
#' pollutant affected by a control in raw_proj_data; if it is not identified
#' here, and use_ww = TRUE, we will pull emissions factors not identified here
#' from the ww. The emissions factors identified in the table will be used as
#' baseline emissions factors and compared to the emissions factors in the
#' Controls table.
#' @param baseline_year (optional) This is only relevant if any of the controls
#' are TimeDependent. Year that we should treat as the baseline
#' for emissions in the raw_proj_data (we assume every other year, therefore, is
#' a projection from the baseline_year); if NULL, we assume that the min year
#' in raw_proj_data is the baseline_year
#'
#' @return adjusted proj_data data frame with applicable control percent
#' reductions added
#' @examples
#' scc <- 2415000000
#' temp_table <- pull_baseline_from_ww(scc = scc)
#' temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["ManEmp"]])
#' temp_table_controlled <- add_controls(temp_table_project)
#' 
#' scc <- 2425000000
#' temp_table <- pull_baseline_from_ww(scc = scc)
#' ww_ef <- pull_efs_from_ww(scc = scc, throughput_unit = 'EACH')
#' ww_ef <- ww_ef %>% filter(pollutant == 'VOC') %>% select(TPUPY)
#' ww_ef <- ww_ef[[1,1]] * 2000
#' temp_table$TPY <- temp_table$TPY*(201/ww_ef)
#' temp_table_project <- project_baseline(base_table = temp_table, projection_table = projection_tables[["ManEmp"]])
#' graphic_arts_efs <- data.frame(FIPS = unique(temp_table$FIPS), SCC = scc, pollutant = "VOC", EmissionsFactor = 201, EFNumerator = "lb")
#' temp_table_controlled <- add_controls(raw_proj_data = temp_table_project, current_efs = graphic_arts_efs)
add_controls <- function(raw_proj_data,
                          controls_ws = controls,
                          use_ww = TRUE,
                          current_efs = NULL, 
                          baseline_year = start_year){
  
  # adjust our controls_ws to have a row for each county
  controls_to_apply <- separate_rows(controls_ws, County, sep = ", ")
  
  # add a column onto our controls_to_apply function for the FIPS codes,
  # we'll use this to match with the data in raw_proj_data
  controls_to_apply$FIPS <- sapply(controls_to_apply$County, county_to_fip)
  
  # What unique combos of FIPS, sccs, and pollutants do we have in raw_proj_data?
  unique_combos <- raw_proj_data %>% 
    group_by(FIPS, SCC, pollutant) %>%
    summarize(.groups = "drop")
  
  # only save the unique_combos that show up in the controls_to_apply function,
  # these are the only ones that we'll be working with
  controls_to_apply <- merge(controls_to_apply, unique_combos, 
                             by.x = c("FIPS", "SCC", "PollutantCode"),
                             by.y = c("FIPS", "SCC", "pollutant"))

  # is our controls_to_apply table 0 rows? this would imply that there
  # are no controls to apply for the SCCs in raw_proj_data
  if(nrow(controls_to_apply) == 0) {
    stop(paste0("There are no controls to apply for ", 
               paste(scc, collapse = ", "), "."))
 
  # if not, we've got to do stuff!
  } else {
    
    # create a data frame called adjusted_proj_data that is raw_proj_data
    # this is what we'll modify & return at the end of the function
    adjusted_proj_data <- raw_proj_data
    
    # convert the year column in adjusted_proj_data to a numeric
    adjusted_proj_data$year <- as.numeric(adjusted_proj_data$year)
    
    # check if any of our controls are time dependent
    if(any(controls_to_apply$TimeDependent)) {
      
      print("Some or all of the controls relevant to your data are time dependent. We will be adjusting the relevant controls accordingly.")
      
      # if our baseline_year is NULL, we're going to pull the min baseline_year
      # from adjusted_proj_data and use that as our baseline_year
      if(is.null(baseline_year)) {
        baseline_year <- min(adjusted_proj_data$year)
      }
    }
    
    # rename the PollutantCode column in controls_to_apply to pollutant to
    # match with the name that it has in adjusted_proj_data, so that it is
    # easier to merge these two data frames
    controls_to_apply <- rename(controls_to_apply, pollutant = PollutantCode)
    
    # Are any of our controls to apply EFDependent? If so, we want to make
    # a current_efs table that we can use to compare current_efs with old efs
    # (old efs are stored in the controls ws)
    if(any(controls_to_apply$EFDependent)) {
      
      print("Some or all of the controls relevant to your data are emissions factor dependent. We will be adjusting the relevant controls accordingly.")
      
      # What are they?
      ef_controls <- controls_to_apply %>%
        filter(EFDependent == TRUE)
      
      # Add an EFNumerator column so we can make sure our EFs are still in the
      # same unit
      ef_controls$EFNumerator <- tolower(sapply(ef_controls$EFUnit, function(x) strsplit(x, "/")[[1]][1]))
      
      # we only need to have the columns that are relevant for making our
      # current_efs table
      ef_controls <- ef_controls %>%
        select(FIPS, SCC, pollutant, EmissionsFactor, EFNumerator)
      
      # only save unique ones (this will only be relevant when we have
      # multiple controls that apply to the same FIPS, scc, and pollutant
      # that are EFDependent)
      ef_controls <- unique(ef_controls)
      
      # Was any current_efs table supplied to the function?
      # We normally just fall back on the default WW, so we really only use this
      # for graphic arts and dry cleaning (and, starting with the 2020 NEI, we
      # should start using the default values for those, and won't need this
      # option anymore)
      if(!is.null(current_efs)) {
        
        # Make sure our current_efs table has the appropriate columns/column
        # names, if not, we'll stop the function (below)
        if(!all(c("FIPS", "SCC", "pollutant", "EmissionsFactor", "EFNumerator") %in% colnames(current_efs))) {
          
          stop("The current_efs data is not in the correct format. It needs to have a 'FIPS', 'SCC', 'pollutant', and 'EmissionsFactor' column.") }
        
        else {
          # make the EFNumerator lowercase to make it easy to compare with the
          # EFNumerator in our controls ws
          current_efs$EFNumerator <- tolower(current_efs$EFNumerator)
          
          # check to make sure that our current_efs and ef_controls have the same
          # numerators. If the supplied current_efs has an EF that is not used,
          # we can just ignore it.
          test_numerator <- merge(ef_controls, current_efs, by = c("FIPS", "SCC", "pollutant"), all.y = TRUE)
          
          test_numerator$MatchingNumerators <- ifelse(test_numerator$EFNumerator.x == test_numerator$EFNumerator.y, TRUE, FALSE)
         
          # if not, stop the function 
          if(!all(test_numerator$MatchingNumerators, na.rm = TRUE)) {
            print("EFNumerator from controls_ws does not match with EFNumerator from the current_efs table. We can't compare these emissions factors.")
            print(test_numerator %>% 
                    select(FIPS, SCC, pollutant, EFNumerator.x, EFNumerator.y) %>% 
                    rename("EFNumerator.controls_ws" = EFNumerator.x,
                           "EFNumerator.current_efs" = EFNumerator.y))
            stop("Try again with current_efs that have matching numerators with those in the controls_ws")
          }
          
          # if use_ww == TRUE, we want to gapfill any emissions factors that
          # are not supplied in the current_efs table with the ones from WW
          if(use_ww) {
            # which efs are not in the current_efs table?
            missing_efs <- ef_controls %>%
              select(FIPS, SCC, pollutant, EFNumerator)
            
            # merge those with the current efs table. The ones that are missing
            # will have "NA" for the EmissionsFactor column
            missing_efs <- merge(missing_efs, current_efs, by = c("FIPS", "SCC", "pollutant", "EFNumerator"), all = TRUE)
            
            # only save the missing efs (those that are NA for EmissionsFactor)
            missing_efs <- missing_efs %>%
              filter(is.na(EmissionsFactor))
            
            # if we have any missing_efs, do this
            if(nrow(missing_efs) > 0) {
              # get emission factors from the wagon wheel
              ww_efs <- ww %>%
                filter(StateAndCountyFIPSCode %in% missing_efs$FIPS,
                       SourceClassificationCode %in% missing_efs$SCC,
                       PollutantCode %in% missing_efs$pollutant) %>%
                rename(FIPS = StateAndCountyFIPSCode,
                       SCC = SourceClassificationCode,
                       pollutant = PollutantCode) %>%
                select(FIPS, SCC, pollutant, EmissionFactor, EmissionFactorNumeratorUnitofMeasureCode)
              
              # merge them onto our missing_efs data frame
              missing_efs <- merge(missing_efs, ww_efs, by = c("FIPS", "SCC", "pollutant"), all = TRUE)
              
              # make sure that our numerators are matching
              if(!all(missing_efs$EFNumerator == tolower(missing_efs$EmissionFactorNumeratorUnitofMeasureCode)) | 
                 is.na(all(missing_efs$EFNumerator == tolower(missing_efs$EmissionFactorNumeratorUnitofMeasureCode)))) {
                print("EFNumerator from controls_ws does not match with EmissionFactorNumeratorUnitofMeasureCode from the ww. We can't compare these emissions factors.")
                print(missing_efs %>% 
                        select(FIPS, SCC, pollutant, EFNumerator, EmissionFactorNumeratorUnitofMeasureCode))
                stop("Try again with a current_efs table or use_ww = FALSE")
              }
              
              # replace the values in EmissionsFactor with the values from 
              # Wagon Wheel (in the EmissionFactor column) and delete the
              # EmissionFactor column
              missing_efs$EmissionsFactor <- missing_efs$EmissionFactor
              
              missing_efs <- select(missing_efs, -EmissionFactor, -EmissionFactorNumeratorUnitofMeasureCode)
              
              # Give a notice regarding the emissions factors we're pulling from
              # the Wagon Wheel.
              print("These emission factors were not in the current_efs table. We pulled them from Wagon Wheel.")
              print(missing_efs)
              
              # Now bind the missing_efs onto our current_efs.
              current_efs <- rbind(current_efs, missing_efs)
            }
            
            # Now, if we do not want to gapfill emissions factors from the Wagon Wheel
            # (use_ww == FALSE), but we do have some emissions factors supplied in 
            # current_efs, do this
            
            # This probably will never happen
            # Unless we are comparing to old inventory methods
          } else {
            # which efs are we missing from current_efs?
            missing_efs <- ef_controls %>%
              select(FIPS, SCC, pollutant, EFNumerator)

            missing_efs <- merge(missing_efs, current_efs, by = c("FIPS", "SCC", "pollutant", "EFNumerator"), all = TRUE)
            
            missing_efs <- missing_efs %>%
              filter(is.na(EmissionsFactor)) %>%
              select(-EmissionsFactor)
            
            # okay, so we're just going to give those missing efs the emissions
            # factor from our controls table and print a warning.
            efs_from_controls <- ef_controls %>%
              select(FIPS, SCC, pollutant, EmissionsFactor, EFNumerator)
            
            missing_efs <- merge(missing_efs, efs_from_controls, by = c("FIPS", "SCC", "pollutant", "EFNumerator"), all.x = TRUE)
            
            if(nrow(missing_efs > 0)) {
              print("These emission factors were not in the current_efs table. Since use_ww == FALSE, we are just setting them equal to the emissions factors from the Controls table.")
              print(missing_efs)
              
              current_efs <- rbind(current_efs, missing_efs)
            }
          }
        }
        
        # Now, if no current_efs table is supplied
      } else {
        
        # most common scenario
        if(use_ww) {
          # create a current_efs table
          current_efs <- ef_controls %>%
            select(FIPS, SCC, pollutant, EmissionsFactor, EFNumerator)
          
          # get emission factors from the wagon wheel
          ww_efs <- ww %>%
            filter(StateAndCountyFIPSCode %in% current_efs$FIPS,
                   SourceClassificationCode %in% current_efs$SCC,
                   PollutantCode %in% current_efs$pollutant) %>%
            rename(FIPS = StateAndCountyFIPSCode,
                   SCC = SourceClassificationCode,
                   pollutant = PollutantCode) %>%
            select(FIPS, SCC, pollutant, EmissionFactor, EmissionFactorNumeratorUnitofMeasureCode)
          
          # merge ww_efs onto our current_efs data frame
          current_efs <- merge(current_efs, ww_efs, by = c("FIPS", "SCC", "pollutant"), all = TRUE)
          
          # make sure that our numerators are matching
          if(!all(current_efs$EFNumerator == tolower(current_efs$EmissionFactorNumeratorUnitofMeasureCode)) |
                  is.na(all(current_efs$EFNumerator == tolower(current_efs$EmissionFactorNumeratorUnitofMeasureCode)))) {
            print("EFNumerator from controls_ws does not match with EmissionFactorNumeratorUnitofMeasureCode from the ww. We can't compare these emissions factors.")
            print(current_efs)
            stop("Try again with a current_efs table or use_ww = FALSE")
          }
          
          # replace the values in EmissionsFactor with the values from 
          # Wagon Wheel (in the EmissionFactor column) and delete the
          # EmissionFactor column
          current_efs$EmissionsFactor <- current_efs$EmissionFactor
          
          current_efs <- select(current_efs, -EmissionFactor, -EmissionFactorNumeratorUnitofMeasureCode)

          # Give a notice regarding the emissions factors we're pulling from
          # the Wagon Wheel.
          print("We pulled these emission factors from Wagon Wheel.")
          print(current_efs)
          
          # # if no current_efs are supplied, and use_ww = FALSE, we're just going to
          # assume that we want to use the emissions factors from the controls table
          # as our baseline emissions factor (i.e., no difference between them, so
          # we won't actually do any adjustments due to different emissions factors)
        } else {
          current_efs <- ef_controls %>%
            select(FIPS, SCC, pollutant, EmissionsFactor, EFNumerator)
          
          print("No emissions factors were supplied to current_efs and use_ww = FALSE, so we are going to use the emissions factors from the Controls table. This means that, though there are controls to be applied that are emissions factor dependent, we are going to assume the emissions factors have not changed since the control was created.")
          print("These are the emissions factors that are emissions factor dependent. We are assuming that the baseline emissions factor is the same as what it was when the control was created.")
          print(current_efs)
        }
      }
      
      # double check that there are no missing emissions factors
      test_missing_efs <- merge(ef_controls, current_efs, by = c("FIPS", "SCC", "pollutant"), all.x = TRUE)
      
      if(any(is.na(test_missing_efs$EmissionsFactor.y))) {
        print("There is one or more missing emissions factors in the current_efs table. We can't compare with current emissions factors. Stopping the function.")
        print(current_efs)
        
        stop("Correct the error with a missing emissions factor. This is likely due to the emissions factor not existing in the wagon wheel. You may need to supply this emissions factor, or try use_ww = FALSE.")
      }
    }
    
    # For our controls_to_apply, how many different FIPS, SCC, and
    # pollutant combos do we have? We need to make decisions to adjust TPY based
    # on these
    unique_combos <- controls_to_apply %>% 
      group_by(FIPS, SCC, pollutant) %>%
      summarize(.groups = "drop")
    
    # loop over every row in our unique_combos
    for(i in 1:nrow(unique_combos)) {
      # get the matching pollutant & county from the adjusted_proj_data table
      proj_obs <- adjusted_proj_data %>%
        filter(FIPS == unique_combos$FIPS[i],
               SCC == unique_combos$SCC[i],
               pollutant == unique_combos$pollutant[i])
      
      # and do the same for the controls_to_apply
      relevant_controls <- controls_to_apply %>%
        filter(FIPS == unique_combos$FIPS[i],
               SCC == unique_combos$SCC[i],
               pollutant == unique_combos$pollutant[i])
      
      # check if we have multiple rows where relevant_controls is the same rule
      # this might imply different throughput (i.e. 2311010000). we want to
      # apply the rule only once.
      # basically, if everything is the same except the emissionsfactor, they
      # are duplicate.
      test_duplicate_controls <- relevant_controls %>%
        group_by(FIPS, SCC, pollutant, YearFinalized, County, ControlPct, PctAppliesTo, PhaseInStartYear, PhaseInEndYear, EFDependent, TimeDependent, RuleNumber) %>%
        summarize(.groups = "drop")
      
      # make sure its not EFDependent, otherwise the code will break when we
      # get there.
      if(nrow(test_duplicate_controls) < nrow(relevant_controls) && all(test_duplicate_controls$EFDependent == FALSE)) {
        relevant_controls <- test_duplicate_controls
      }
      
      # now, we need to loop over relevant_controls to adjust the ControlPct
      # make a list that can hold the ControlPcts we need to adjust for each
      # applicable control
      control_pct <- list()
      
      for(j in 1:nrow(relevant_controls)) {
        # get a control_pct table for our controls_to_apply that will identify
        # how much of our control we should apply over time
        # our table will start one year before our start year (to have a starting
        # ControlPct of 1)
        control_pct[j] <- list(data.frame(year = seq(relevant_controls$PhaseInStartYear[j] - 1, 
                                             relevant_controls$PhaseInEndYear[j], 
                                             by = 1), 
                                  # calculate what control pct we'll apply
                                  # each year
                                  ControlPct = seq(1, relevant_controls$ControlPct[j], 
                                                   length.out = (relevant_controls$PhaseInEndYear[j] - 
                                                                   (relevant_controls$PhaseInStartYear[j] - 2))),
                                  PctAppliesTo = relevant_controls$PctAppliesTo[j]))
        
        # now we do our checks to adjust the ControlPct we'll use
        # if our control is TimeDependent, we may need to adjust it
        if(relevant_controls$TimeDependent[j]) {
          # if our baseline year is after the phase in end year, then our
          # controlpct should become 1, since our controls have already
          # been fully phased-in
          # but, we also need to increase emissions estimates in the past
          # in order to account for the times when our controls were not
          # fully phased-in
          if(baseline_year >= relevant_controls$PhaseInEndYear[j]) {
            baseline_pct <- relevant_controls$ControlPct[j]
            
            # okay, the phaseinendyear needs to become "100" now. We should
            # increase emissions estimates for all years prior to this
            control_pct[[j]]$ControlPct <- control_pct[[j]]$ControlPct / baseline_pct
            
            # if our baseline year is after the phaseinstartyear, but not
            # after the phaseinendyear, we've got to do some adjusting
          } else if(baseline_year >= relevant_controls$PhaseInStartYear[j]) {
            
            # what is our expected reduction at the baseline_year?
            baseline_pct <- (filter(control_pct[[j]], year == baseline_year))$ControlPct
            
            # okay, the baseline_year needs to become "100" now. We should
            # increase emissions estimates for years prior to baseline_year, 
            # and decrease emissions estimates for years after baseline_year
            control_pct[[j]]$ControlPct <- control_pct[[j]]$ControlPct / baseline_pct
          }
          # if neither of those conditions hold (therefore, the baseline_year is
          # less than the PhaseInStartYear), then we don't do anything with
          # our control_pct
        }
        
        # now we need to check if our control is EFDependent, if it is, we may
        # need to adjust it if the current EF is different than the historic EF
        if(relevant_controls$EFDependent[j]) {
          old_ef <- relevant_controls$EmissionsFactor[j] * relevant_controls$PctAppliesTo[j]
          
          old_end_ef <- old_ef * relevant_controls$ControlPct[j]
         
          new_ef <- (current_efs %>%
            filter(FIPS == relevant_controls$FIPS[j],
                   SCC == relevant_controls$SCC[j],
                   pollutant == relevant_controls$pollutant[j]))$EmissionsFactor *
            relevant_controls$PctAppliesTo[j]
          
          new_end_ef <- new_ef * relevant_controls$ControlPct[j]
          
          if(new_ef >= old_ef) {
            # do nothing, add the same control percent as always
          } else if(new_ef < old_end_ef) {
            # we should stop adding any control
            control_pct[[j]]$ControlPct <- 1
          } else {
            # adjust the control we apply, so that our new_end_ef will be equal
            # to our old_end_ef
            new_control_pct <- old_end_ef/new_ef
            
            control_pct[[j]]$ControlPct <- seq(1, new_control_pct,
                                             length.out = (relevant_controls$PhaseInEndYear[j] - 
                                                             (relevant_controls$PhaseInStartYear[j] - 2)))
          }
        }
        
        # Now we can apply the control to adjusted_proj_data for our proj_obs
        
        # first, adjust our control_pct table so we can account for any year we
        # might see in our raw_proj_data (i.e., backproject the relevant
        # ControlPct to 1900 and forward project to 2100)
        # create a data frame that holds years from 1900 to the min year
        # we have in the control_pct table and from the max year we have in the
        # table to 2100
        pct_buffer <- data.frame(year = c(seq(1900, min(control_pct[[j]]$year) - 1, by = 1), 
                                          seq(max(control_pct[[j]]$year) + 1, 2100, by = 1)))
        
        # add a column for ControlPct, assign it so that any year prior to the
        # min year is equal to the ControlPct for the min year and so that any
        # year after the max year is equal to the ControlPct for the max year
        # do the same for PctAppliesTo
        pct_buffer <- pct_buffer %>%
          mutate(ControlPct = ifelse(year < min(control_pct[[j]]$year), 
                                      control_pct[[j]][control_pct[[j]]$year == min(control_pct[[j]]$year),]$ControlPct,
                                      control_pct[[j]][control_pct[[j]]$year == max(control_pct[[j]]$year),]$ControlPct),
                 PctAppliesTo = ifelse(year < min(control_pct[[j]]$year), 
                                     control_pct[[j]][control_pct[[j]]$year == min(control_pct[[j]]$year),]$PctAppliesTo,
                                     control_pct[[j]][control_pct[[j]]$year == max(control_pct[[j]]$year),]$PctAppliesTo))
      
        
        # now we need to rbind the pct_buffer onto our our control_pct table
        control_pct[[j]] <- rbind(control_pct[[j]], pct_buffer)
      }
      
      # now we need to make a final control_pct data frame that calculates
      # the actual control we need to apply in each year based on the info
      # stored in the control_pct elements
      control_pct <- do.call(rbind, control_pct)
      
      # now, to calculate the actual control applied, we need to add the
      # controlpct*pctapplies to + 1*(1-pctappliesto)
      # for example, if pctappliesto is only 0.6, we need to do controlpct*
      # 0.6 + 0.4
      # if we have two controls, one being controlpct1 to 0.6 and one being 
      # controlpct2 to 0.4, we need to do controlpct1*0.6 + controlpct2*0.4
      control_pct <- control_pct %>%
        group_by(year) %>%
        summarize(ControlPct = ifelse(sum(PctAppliesTo) == 1,
                                      sum(ControlPct*PctAppliesTo),
                                      sum(ControlPct*PctAppliesTo + (1-sum(PctAppliesTo)))),
                  .groups = "drop") 
      
      # check to make sure that our ControlPct is not negative
# on 20210504, this line threw up an error. I think it should be control_pct$ControlPct
#      if(any(control_pct$PctAppliesTo < 0)) {
      if(any(control_pct$ControlPct < 0)) {
        print("Applicable rules result in a negative ControlPct. This shoudn't happen.")
        print(control_pct)
        
        stop("Fix this.")
      }

      # now we can adjust those columns in proj_obs by the control_pct
      proj_obs <- proj_obs %>%
        left_join(control_pct, by = "year") %>%
        mutate(TPY = TPY*ControlPct) %>%
        select(-ControlPct)
      
      # now, we want to replace the columns in adjusted_proj_data by those
      # that exist in proj_obs
      # rename TPY to TPY_new in proj_obs
      proj_obs <- rename(proj_obs, TPY_new = TPY)
      
      # merge the proj_obs table onto the adjusted_proj_data table
      adjusted_proj_data <- merge(adjusted_proj_data, proj_obs, 
                                  by = c("FIPS", "SCC", "year", "pollutant"),
                                  all = TRUE)
      
      # TPY_new will only exist for the rows that were in proj_obs
      # replace TPY with TPY_new and then delete TPY_new from the adjusted
      # proj data table
      adjusted_proj_data <- adjusted_proj_data %>%
        mutate(TPY = ifelse(is.na(TPY_new), TPY, TPY_new)) %>%
        select(-TPY_new)
    }
    
  }
  
  # add a check for if controls_to_apply includes PM25 but not PM10
  if(any(str_detect(controls_to_apply$pollutant, "PM25")) && 
     !any(str_detect(controls_to_apply$pollutant, "PM10"))) {
    
    # if so, create a new table that compares adjusted data with raw data
    adjust_pm <- merge(adjusted_proj_data, raw_proj_data,
                                by = c("FIPS", "SCC", "year", "pollutant"),
                                all = T)
    
    # add a column to specify if the pollutant is PMPRI
    adjust_pm$PMPRI <- ifelse(adjust_pm$pollutant %in% c("PM25-PRI", "PM10-PRI"), TRUE, FALSE)
    
    # add a column to specify if the pollutant is PM25
    adjust_pm$PMFIL <- ifelse(adjust_pm$pollutant %in% c("PM25-FIL", "PM10-FIL"), TRUE, FALSE)
    
    # only save PM25 PRI and PM25 FIL
    adjust_pm_diff <- adjust_pm %>% 
      filter(pollutant %in% c("PM25-PRI", "PM25-FIL"))
    
    # add a column for TPY diff
    adjust_pm_diff$TPY.diff <- adjust_pm_diff$TPY.y - adjust_pm_diff$TPY.x
    
    # don't save pollutant or TPY values
    adjust_pm_diff <- select(adjust_pm_diff, -pollutant, -TPY.x, -TPY.y)
    
    # merge the diff data frame with our data frame, now we have a column for
    # TPY diff if the pollutant is PMPRI or PMFIL
    adjust_pm <- merge(adjust_pm, adjust_pm_diff,
                       by = c("FIPS", "SCC", "year", "PMPRI", "PMFIL"),
                       all = T)
    
    # We want to subtract TPY diff from the PM10s, this means we are subtracting
    # our estimated reduction in PM25 from PM10, since PM25 falls under PM10
    adjust_pm$TPY.new <- ifelse(adjust_pm$pollutant %in% c("PM10-PRI", "PM10-FIL"),
                              adjust_pm$TPY.x-adjust_pm$TPY.diff,
                              adjust_pm$TPY.x)
    
    # make a new TPY column from TPY.new
    adjust_pm$TPY <- adjust_pm$TPY.new
    
    # replace our adjusted_proj_data with this new adjust_pm data, but only save
    # the columns of interest
    adjusted_proj_data <- adjust_pm %>%
      select(FIPS, SCC, year, pollutant, TPY)
  }
  
  # make the year column into a character in adjusted_proj_data so its the same
  # format table (same classes for each column) as it was in raw_proj_data
  adjusted_proj_data$year <- as.character(adjusted_proj_data$year)
  
  
  # return the adjusted_proj_data
  return(adjusted_proj_data)
}

#this function takes in a table of EFs for an SCC, and combines it with 
#  reference data, like population or manufacturing employment, and it creates
#  an emissions table for that with TPY values.
#
#efs_table: needs columns for (county, SCC, pollutant, TPUPY). TPUPY is TONS 
#  per unit per year.
#ref_data: needs columns for (county, year, unit). unit is the actual value
#  of the reference data, whether that be population or employment etc.
#st_year: starting year to project data for. This is typically the base year
#  of the inventory we gather
#project_to: this function CAN project data into the future or past! IFF the 
#  emissions are projected to scale with the same reference data that is used
#  to produce baseline data. AKA If the baseline data are derived from
#  AND the future data scales with population as well, we can use this function 
#  to project the data.
make_baseline <-
  function(efs_table,
           ref_data,
           st_year = start_year,
           project_to = NULL) {
    #we will fill this in later
    final_table <- vector()
    
    #make sure the FIPS codes are compatible format
    efs_table$county <- as.integer(efs_table$county)
    ref_data$county <- as.integer(ref_data$county)
    
    
    #establish how many years you are working with,
    #if you want to project these data then you establish that here.
    if (!is.null(project_to)) {
      all_years <- st_year:project_to
    } else if (is.null(project_to)) {
      #if you don't want to project, then just select the start year
      all_years <- st_year
    }
    
    for (cur_year in all_years) {
      #first make sure that reference data only has the year we want
      target_ref_data <- ref_data %>% filter(year == cur_year)
      if (dim(target_ref_data)[1] == 0) {
        cat(
          'we do not have reference input data for projecting',
          scc,
          'for the year',
          cur_year,
          '\n'
        )
      }
      
      #merge the tables, then add a new column of TPY
      intermediate_table <-
        left_join(efs_table, target_ref_data, by = "county") %>%
        mutate(TPY = TPUPY * unit)
      final_table <- rbind(final_table, intermediate_table)
    }
    #trim and rename the columns, then return
    final_table <- final_table %>%
      select(county, SCC, year, pollutant, TPY) %>%
      rename('FIPS' = county)
    final_table$year <- as.double(final_table$year)
    
    return(final_table)
  }


#Throughput_unit: ACRE-MONTH, E6CF, EACH
#  The denominator of whatever emission factor we are working with.
pull_efs_from_ww <-
  function(scc, throughput_unit, all_primaries = FALSE, raw_ww = ww) {
    #first, filter out the SCC and get a list of all the pollutants we will
    #work with
    target_data <-
      raw_ww %>% filter(SourceClassificationCode == scc) %>% 
      select(StateAndCountyFIPSCode, SourceClassificationCode,
             ThroughputUnit, PollutantCode, EmissionFactor,
             EmissionFactorNumeratorUnitofMeasureCode,
             EmissionFactorDenominatorUnitofMeasureCode)
    
    
    #if you have lb, make it ton
    target_data <- target_data %>% 
      #target_data <- target_data %>% 
      #change the units from LB to TON
      mutate(EmissionFactor = 
               ifelse(EmissionFactorNumeratorUnitofMeasureCode=='LB',
                      #if yes LB, change it
                      EmissionFactor/2000,
                      #else
                      EmissionFactor)) %>%
      #now that we adjusted the value by 2000x, change the UOM to 'TON'
      mutate(EmissionFactorNumeratorUnitofMeasureCode = 
               ifelse(EmissionFactorNumeratorUnitofMeasureCode=='LB',
                      'TON',
                      EmissionFactorNumeratorUnitofMeasureCode))
    
    #does this SCC have secondary pollutants?
    if (!all_primaries){
      #If we have secondary pollutants, this script only works if the PRIMARY
      #EF is consistent across all counties. If the PRIMARY EF is different
      #between counties, this code will have to be updated
      #
      #first, I will find the baseline value the secondary EFs come from
      primary_ef <- target_data %>% 
        filter(EmissionFactorDenominatorUnitofMeasureCode == throughput_unit) %>%
        select(EmissionFactor) %>% unique()
      #check that there is only a single value we pulled
      if(dim(primary_ef)[1] != 1){
        cat('Pulling secondary pollutants for ',scc,
            ', we have multiple primary emission factors to use. we should only have one.\n')
      }
      #pull the raw number for primary ef
      primary_ef <- primary_ef[[1,1]]
      
      #make sure that all of the secondary pollutants have TON/TON in their 
      #units
      #check numerator
      secondary_num <- target_data %>% 
        filter(EmissionFactorDenominatorUnitofMeasureCode != throughput_unit) %>%
        select(EmissionFactorNumeratorUnitofMeasureCode) %>% unique()
      if(dim(secondary_num)[1] != 1){
        cat('Pulling secondary EFs for ',scc,
            ',we have multiple numerators for the pollutants\n')
      }
      if(secondary_num[[1,1]] != 'TON'){
        cat('Pulling secondary EFs for ',scc,
            ',numerator units are not TON\n')
      }
      #check denominator
      secondary_den <- target_data %>% 
        filter(EmissionFactorDenominatorUnitofMeasureCode != throughput_unit) %>%
        select(EmissionFactorDenominatorUnitofMeasureCode) %>% unique()
      if(dim(secondary_den)[1] != 1){
        cat('Pulling secondary EFs for ',scc,
            ',we have multiple denominators for the pollutants\n')
      }
      if(secondary_den[[1,1]] != 'TON'){
        cat('Pulling secondary EFs for ',scc,
            ',denominator units are not TON\n')
      }
      
      #okay, units of the secondary pollutants are okay.
      #now multiply all of the secondary values by the primary emission factor
      target_data <- target_data %>% 
        mutate(EmissionFactor = ifelse(EmissionFactorDenominatorUnitofMeasureCode == throughput_unit,
                                       #if primary ef
                                       EmissionFactor,
                                       #if secondary ef
                                       EmissionFactor * primary_ef))
    }
    
    #so that was the script to run if we have secondary emissions. this is the
    #code to run if we have all primary emissions:
    else if (all_primaries) {
      #check that numerator and denominator are right.
      if (unique(target_data$EmissionFactorDenominatorUnitofMeasureCode) != throughput_unit) {
        cat('something is wrong with units in pulling primary efs from', scc)
        stop()
      }
      if (unique(target_data$EmissionFactorNumeratorUnitofMeasureCode) == 'LB') {
        target_data <- target_data %>%
          mutate(EmissionFactor = EmissionFactor / 2000) %>%
          mutate(EmissionFactorNumeratorUnitofMeasureCode = 'TON')
      }
      if (unique(target_data$EmissionFactorNumeratorUnitofMeasureCode) != 'TON') {
        cat('something is wrong with units in pulling primary efs from', scc)
        stop()
      }
    }
    
    #okay, now we know we have TONS, and the right throughput units.
    #rename the columns, drop some columns, and return it.
    target_data <- target_data %>% 
      rename(county = StateAndCountyFIPSCode, 
             SCC = SourceClassificationCode,
             pollutant = PollutantCode, 
             TPUPY = EmissionFactor) %>%
      select(-c(ThroughputUnit,
                EmissionFactorNumeratorUnitofMeasureCode,
                EmissionFactorDenominatorUnitofMeasureCode))
    return(target_data)
    
  }

project_baseline <-
  function(base_table, projection_table,e_year = end_year,has_pt = FALSE) {
    #check that the base_table only has a single year in it.
    start_year <- unique(base_table$year)
    if(length(start_year) != 1){
      cat('the base table you are trying to project has more than a single year. That is a problem\n')
    }
    projection_table <- projection_table %>%
      rename(FIPS = county)
    #make sure the FIPS codes are compatible format
    base_table$FIPS <- as.integer(base_table$FIPS)
    projection_table$FIPS <- as.integer(projection_table$FIPS)
    
    #we will add to final_table later
    final_table <- base_table
    
    #establish how many years you are working with,
    all_years <- start_year:e_year
    #drop start_year, as we already added it to the base_table
    all_years <- all_years[all_years != start_year]
    
    base_projection_table <- projection_table %>% 
      filter(year == start_year) %>%
      rename(base_unit = unit, base_year = year)
    
    for (cur_year in all_years) {
      #pull data from the projection table for the relevant year
      new_projection_table <- projection_table %>% 
        filter(year == cur_year) %>%
        rename(new_unit = unit, new_year = year)
      if (dim(new_projection_table)[1] == 0) {
        cat('we do not have reference input data for projecting to',
            cur_year,
            '\n')
      }
      
      #okay. we have our base table, and then data for our base year and 
      #projection year. We need to take the TPY from the base table, divide it
      #by the base_projection_table and multiply by new_projection_table.
      intermediate_table <- 
        left_join(base_table, base_projection_table, by = 'FIPS') %>%
        left_join(new_projection_table, by = 'FIPS')
      
      #now I have all the values I need in one table. Let's do some math
      intermediate_table <- 
        intermediate_table %>% 
        #if the base year unit is 0, we must have 0 as our projected number. We can not
        #make something from nothing. This could be a problem. Like if we have 1 TON/EMPLOYEE,
        #and one year we gain our first employee, we would still assume 0 TPY for that county.
        mutate(TPY = ifelse(base_unit == 0, 0, TPY/base_unit*new_unit))
      
      #filter and rename columns
      if (has_pt) {
        intermediate_table <-
          intermediate_table %>%
          #if we have pt source identifiers (for pt subs), keep that identifier
          select(FIPS, SCC, new_year, pollutant, TPY, is_point) %>%
          rename(year = new_year)
        
      } else if (!has_pt) {
        intermediate_table <-
          intermediate_table %>%
          #if this is an area source baseline, there is no need to have the pt identifier
          select(FIPS, SCC, new_year, pollutant, TPY) %>%
          rename(year = new_year)
      }
      
      #add to the final table
      final_table <- rbind(final_table, intermediate_table)
    }
    final_table$year <- as.double(final_table$year)
    return(final_table)
  }




#this function takes in an area source SCC, any number of point source SCCs,
# the SLEIS data dump, and a potential list of IDs to filter (for WEB/SIP)
#
#it then returns the TPY values of every county that had the SCCs you specified 
#
#MORE DETAILED OPTIONS:
#raw_scc_pull_return:
#  this is for diagnosing problems. If TRUE, this will return the raw data of every 
#  relevant SCC in SLEIS, so you can look at the data yourself.
pull_pt_removal_table <-
  function(pt_sccs,
           area_scc,
           sleis = wide_sleis,
           year = start_year,
           filter_ids = target_ids,
           raw_scc_pull_return = FALSE) {
    
    #filter down to the target SCCs
    potential_removal_table <-
      sleis %>%
      filter(`SCC (Code)` %in% pt_sccs)
    
    #filter down to only our target_ids
    #if we are not targeting any specific ids (like for website),
    #then we make target_ids = every site
    if(is.null(filter_ids)){
      filter_ids <- unique(sleis$`Facility Identifier`)
    }
    
    #if I wanna just see all the relevant sccs, return this filtered table
    if (raw_scc_pull_return){
      #but first, mark if things are pt source or not.
      potential_removal_table <- potential_removal_table %>%
        mutate(is_point = ifelse(`Facility Identifier` %in% filter_ids,
                                 TRUE,
                                 FALSE))
      return(potential_removal_table)
    }
    
    #I think I need to pivot_longer this. Pull column names from columns 14 -> end
    long_removal_table <- potential_removal_table %>% 
      #declaring col names from column 14 onward could potentially come back to bite me.
      pivot_longer(cols = colnames(potential_removal_table)[14:ncol(potential_removal_table)],
                   names_to = 'pollutant') %>%
      rename('TPY'='value') %>%
      filter(!is.na(TPY))
    
    
    #okay, now that things are formatted correctly, let's make two group_by tables:
    #one for pt sources and the other for non-pts
    is_pt_source <- long_removal_table %>% 
      #target all pt source sites
      filter(`Facility Identifier` %in% filter_ids) %>%
      #sum pollutants by county
      group_by(`Reporting Year`,`Facility St/County FIPS`,`pollutant`) %>%
      summarise(tons = sum(TPY, na.rm = TRUE), .groups = 'drop') %>% 
      mutate(is_point = TRUE)
    
    not_pt_source <- long_removal_table %>% 
      filter(!`Facility Identifier` %in% filter_ids) %>%
      group_by(`Reporting Year`,`Facility St/County FIPS`,`pollutant`) %>%
      summarise(tons = sum(TPY, na.rm = TRUE), .groups = 'drop') %>% 
      mutate(is_point = FALSE)
    
    #combine the tables, and format columns 
    return_table <- rbind(is_pt_source,not_pt_source) %>% 
      mutate(SCC = area_scc)
    colnames(return_table)<- c('year','FIPS','pollutant','TPY','is_point','SCC')
    return_table <- return_table %>% select(c('FIPS','SCC','year','pollutant','TPY','is_point'))
    return_table$year <- as.double(return_table$year)
    return(return_table)
  }

#This takes in a base emissions table and a pt removal table. It subtracts the
#point sources for any given scc/pollutant/county/year combo from the bulk 
#emissions table. It doesn't look for any errors or anything though.
#
#this function first subtracts ALL values in the pt source table from the area source table.
#but some of the pt source table values are NOT going to be counted in the pt source 
#inventory, even though they are in SLEIS. We then re-add those TPY values to the area 
#source inventory.
subtract_pt_sources <- function(base_emission_table, pt_source_table){
  #format both tables to merge easily
  base_emission_table$year <- as.double(base_emission_table$year)
  pt_source_table$year <- as.double(pt_source_table$year)
  
  #we are about to left_join the base table and the pt source table. IF there are
  #several emission source in a county, and some are pt sources, some are not,
  #the left_join will create duplicate rows for the TRUE and FALSE TPY values. 
  #We don't want to create duplicate rows, so let's cluster all the TRUE/FALSE
  #TPY values before we merge (this happened with 2401055000)
  grouped_pt_table <- pt_source_table %>% 
    group_by(FIPS, SCC, year, pollutant) %>%
    summarise(TPY = sum(TPY), .groups="keep")
  
  
  merged_table <- left_join(base_emission_table,grouped_pt_table,by=c('FIPS','SCC','year','pollutant')) %>%
    #subtract the pt value from the baseline
    mutate(TPY1 = ifelse(is.na(TPY.y),
                         TPY.x,
                         TPY.x-TPY.y)) %>%
    #if the value is negative, make it 0
    mutate(TPY1 = ifelse(TPY1<0,0,TPY1))
  
  #now make sure the VOC >= all the HAPs
  #If we started off with 10 TPY VOC and 3 TPY xylene
  #then subtract           9 TPY VOC and 1 TPY xylene
  #we would be left with   1 TPY VOC and 2 TPY xylene
  #since xylene is a constituent of VOC, it should never be larger than VOC,
  #so that would be a problem.
  #skip this for now
  
  #add back in the pt sources that technically don't count as pt sources
  #with our given inventory.
  #first let's prep the merged table to add back all of the FALSE pt sources 
  #(those are the ones we will add to the area inventory)
  re_merged_table <- 
    left_join(
      #drop the old TPY cols 
      merged_table %>% select(-c(TPY.x, TPY.y)),
      #drop all the TRUE values from the pt source table.
      pt_source_table %>% filter(is_point == FALSE),
      by = c('FIPS', 'SCC', 'year', 'pollutant')
    ) %>%
    #rename columns for clarity
    rename(base_TPY = TPY1, pt_TPY = TPY) %>%
    #drop is_point, we already filtered it as much as we need
    select(-is_point)
  
  #now add back all of the pt values that were FALSE
  return_table <- re_merged_table %>% 
    #if you do '==FALSE' or something, it doesn't work with NAs. Use '%in% FALSE'
    mutate(TPY = ifelse(is.na(pt_TPY),
                        base_TPY,
                        base_TPY + pt_TPY)) %>%
    #drop the old TPY columns
    select(-c(base_TPY, pt_TPY))
  return(return_table)
}


#oinv = old_inventory; ninv = new_inventory
#if we don't hide_pct, then we will display the percent change
#between the two inventories too.
compare_inventories <-
  function(oinv, ninv, hide_pct = FALSE) {
    oinv <- oinv %>% rename(old_tpy = TPY)
    ninv <- ninv %>% rename(new_tpy = TPY)
    
    oinv$year <- as.numeric(oinv$year)
    ninv$year <- as.numeric(ninv$year)
    #make sure I capture pollutants missing in the old inventory that are
    #in the new inventory
    missing_in_old_inventory <-
      left_join(ninv, oinv, by = c('FIPS', 'SCC', 'year', 'pollutant')) %>%
      filter(is.na(old_tpy)) %>%
      select(FIPS, SCC, year, pollutant, old_tpy, new_tpy)
    #now pull the main table we'll be working with and tack on the missing
    #values
    out_table <-
      left_join(oinv, ninv, by = c('FIPS', 'SCC', 'year', 'pollutant')) %>%
      rbind(missing_in_old_inventory)
    
    #if we want to show percentages, do this bit
    #show percent changes in tpy
    if (!hide_pct) {
      out_table <- out_table %>%
        #if any values are missing, or if we have to divide by 0, just do NA
        mutate(pct_change = ifelse(
          is.na(old_tpy) | is.na(new_tpy) |
            old_tpy == 0,
          NA,
          (new_tpy - old_tpy) / old_tpy * 100
        )) %>%
        # if we went from 0 to 0, change from NA -> 0
        mutate(pct_change = ifelse(old_tpy == 0 & new_tpy == 0,
                                   0,
                                   pct_change))
    }
    #if any values are missing, do NA, else, give TPY change.
    out_table <- out_table %>%
      mutate(tpy_change = ifelse(is.na(old_tpy) | is.na(new_tpy),
                                 NA,
                                 new_tpy - old_tpy))
  }

#for any single area source scc, there may be pt source sccs that cover
#roughly the same process. EPA has a table that maps the area sccs to the pt 
#sccs, called crosswalking.
#this function takes in an area source SCC and returns all of the pt sccs that 
#were deemed equivalent.
pull_pt_scc_crosswalk <- function(area_scc, crosswalk_table = pt_crosswalk_table){
  potential_table <- crosswalk_table %>% filter(`Map to NP SCC` %in% area_scc)
  if (dim(potential_table)[1] == 0){
    stop('There were no pt sccs to pull for ',area_scc)
  }
  pt_sccs <- potential_table$`Point SCC`
  return(pt_sccs)
}

#this is for when we take the raw output of the WW, but then we project 
#the data ourselves, or add controls or anything. Input an SCC
pull_baseline_from_ww <- function(scc,ww_table = ww, st_year = start_year){
  temp_table <- ww_table %>% filter(SourceClassificationCode==scc) 
  if(dim(temp_table)[1] == 0){
    stop('we did not pull any data from the WW for ',scc)
  }
  temp_table <- temp_table %>%
    mutate(year = st_year) 
  #we already modified ww to only have TONS as throughput UOM, but let's double check
  not_tons <- temp_table %>% filter(EmissionsUnitofMeasureCode != 'TON')
  if(dim(not_tons)[1] != 0){
    stop('we are pulling non-ton baseline throughputs from WW')
  }
  temp_table <- temp_table %>%
    select(StateAndCountyFIPSCode,SourceClassificationCode,year,PollutantCode,TotalEmissions)
  colnames(temp_table) <- c('FIPS','SCC','year','pollutant','TPY')
  temp_table$year <- as.double(temp_table$year)
  return(temp_table)
}

#' Get the total emissions from the NEI
#' 
#' Use this function to pull total baseline emissions from the NEI.
#' 
#' @param scc Input a single SCC in a numeric format, a vector of numeric SCCs,
#' or the first few characters of an scc if you want the sum of all sccs that 
#' begin with those characters. 
#' If you use a character, you may want to enter the code in this format:
#' "^20", so that it returns only those that match with the first 2 digits,
#' not matching with those digits anywhere (i.e. 2285002008 would return in your
#' inquiry if you didn't use the ^).
#' @param nei_table Input the NEI table you want to use. By default we use 'nei'
pull_baseline_from_nei <- function(scc, nei_table = nei) {
  
  # Save only the observations from the nei_table for our scc of interest
  # If we are using a "parent" scc, it will be in character format
  # IE, we want all sccs that fall under "2805030", then we would have put into
  # the function "^2805030"
  # Let's check if it is a character. If it is, we'll run it through str_detect
  if (is.character(scc)) {
    temp_table <- nei_table %>% 
      filter(str_detect(SourceClassificationCode, scc)) 
    
    # If not, we want an exact match. 
  } else {
    temp_table <- nei_table %>% 
      # use %in% so we can input a vector of SCCs or just one.
      filter(SourceClassificationCode %in% scc) 
  }
  
  # If there are no observations in the nei_table for our scc of interest, stop 
  # the function and print a message to notify
  if (dim(temp_table)[1] == 0) {
    #> Error: We did not pull any data from the NEI
    stop("we did not pull any data from the NEI for ", scc)
  }
  
  # We already modified nei to only have TONS as throughput UOM, but let's 
  # double check
  not_tons <- temp_table %>% filter(EmissionsUnitofMeasureCode != 'TON')
  if (dim(not_tons)[1] != 0) {
    stop("We are pulling non-ton baseline throughputs from NEI")
  }
  
  # If more than one SCC was pulled, notify which SCCs we pulled.
  if (length(unique(temp_table$SourceClassificationCode)) > 1) {
    # Notify & sum over all of them
    print(paste0("We pulled data for the following SCCs: ", 
                  paste(unique(temp_table$SourceClassificationCode), 
                        collapse = ", "), 
                  ". We are summing TPY over all of them in the resulting output table."
          )
    )
      
    temp_table <- temp_table %>%
      group_by(StateAndCountyFIPSCode, County, PollutantCode, Pollutant, 
                EmissionsUnitofMeasureCode, year) %>%
      summarize(TotalEmissions = sum(TotalEmissions),
                .groups = "keep") %>%
      ungroup()
      
    temp_table$SourceClassificationCode <- paste(scc, collapse = ", ")
      
  # if we input a character string, but we only got one SCC, do this
} else if (is.character(scc)) {
  # Notify the SCC we found data for
  print(paste0("We only found data for this SCC: ",
               paste(unique(temp_table$SourceClassificationCode))))
}
  
  # return the data in this format
  temp_table <- temp_table %>%
    select(StateAndCountyFIPSCode,SourceClassificationCode,year,PollutantCode,
           TotalEmissions)
  colnames(temp_table) <- c('FIPS','SCC','year','pollutant','TPY')
  
  # Note that the "year" column will be the year that the NEI data is from, so 
  # we need to make sure to update the NEI data every time new NEI data is 
  # available.
  temp_table$FIPS <- as.numeric(temp_table$FIPS)
  return(temp_table)
}


#return the leftmost 'n' digits of a number
left <- function(num, digits_to_return = 1){
  digits <- floor(log10(num))+1
  out_num <- floor(num/(10^(digits-digits_to_return)))
  return(out_num)
}

#take in a blanket pollutant profile, and apply it to all states equally
make_simple_efs_table <- function(scc, pollutants, TPUPY) {
  out_efs <- vector()
  #make the base EFs, or what we have for a single county
  base_efs <- data.frame(pollutants, TPUPY) %>% mutate(SCC = scc)
  
  #now copy that county for all counties
  all_fips <- seq.int(49001, 49057, 2)
  for (cur_fip in all_fips) {
    temp_efs <- base_efs %>% mutate(county = cur_fip)
    out_efs <- rbind(out_efs, temp_efs)
  }
  #reorder the columns and return
  out_efs <- out_efs %>% select(county, SCC, pollutants, TPUPY) %>%
    rename('pollutant' = pollutants)
  return(out_efs)
}

#every time we merge a new table into the final table, we want to make sure it
#doesn't already exist, and also that it has all the correct years of projection
#as the final_table
merge_with_final_table <- function(final_table, new_table, overwrite = FALSE){
  
  #first off, if final_table is empty, just add this as the first scc and return
  if(is.null(dim(final_table))){
    final_table <- rbind(final_table, new_table)
    final_table$year <- as.numeric(final_table$year)
    return(final_table)
  }
  final_table$year <- as.numeric(final_table$year)
  new_table$year <- as.numeric(new_table$year)
  
  #okay. Now for the real details. First off: does the SCC already exist in
  #final_table? If so, return a big error
  new_scc <- unique(new_table$SCC)
  if (length(new_scc) !=1){
    cat("Number of SCCs you are merging to final_table !=1: ", new_scc," fix that\n")
    Sys.sleep(1)
    stop()
  }
  #if the sccs is already in final_table:
  if (new_scc %in% final_table$SCC & !overwrite){
    cat("The scc you are adding is already in final_table. use 'overwrite = TRUE' to replace it: ",scc,"\n")
    Sys.sleep(1)
    stop()
  }
  if (new_scc %in% final_table$SCC & overwrite){
    cat("The scc you are adding is already in final_table, but we are overwriting it:",scc,"\n")
    final_table <- final_table %>% filter(SCC != new_scc)
    Sys.sleep(1)
  }
  #if we have made it this far, the new SCC is not already in the final_table, 
  #or we are simply overwriting it.
  #first, let's make sure the years of our current table match the years of final_table
  if(! identical( sort(unique(new_table$year)), sort(unique(final_table$year)) )){
    cat("The years of your merging SCC do not add up with the years of the final_table: ",scc,"\n")
    Sys.sleep(1)
    stop()
  }
  #okay now merge
  final_table <- rbind(final_table, new_table)
  final_table$year = as.numeric(final_table$year)
  return(final_table)
}

