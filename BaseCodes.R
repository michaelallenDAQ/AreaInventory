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
#' Pull projection tables from the input_tables, pivot longer to prepare them
#' for the project function, return a list of projection_tables
#' 
#' @param table_names Vector of sheet names from input_tables that we want to 
#' convert to projection tables
#' @return list with each element the data stored on an input table sheet
#' @export
pull_projection_tables <- function(table_names, input_list = input_tables) {
  
  projection_tables <- input_tables[table_names]

  # now, for each of the sheets, read in the data on that particular sheet and
  # assign to a new element in the list
  projection_tables = lapply(projection_tables, function(table) {
    
    #rename the first column to switch the county names to their fips codes
    renamed_table <- table
    ncounties <- dim(table)[1]
    for (i in 1:ncounties) {
      renamed_table[i, 1] <-
        as.factor(county_to_fip(table[[i, 1]]))
    }
    
    #now pivot_longer
    longer_table <- pivot_longer(renamed_table, cols = colnames(table)[-1],
                                 names_to = "year",
                                 values_to = "unit")
  }
  )
  
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
  model_pols <-
    read_xlsx(model_path, sheet = 'PM25ModelData', skip = 3)
  model_pols <- mutate(model_pols, year = base_year)
  model_pols <- model_pols %>%
    rename(TPY = 'Tons/year') %>%
    rename(pollutant = Pollutant)
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
add_controls <-
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
        mutate(TPY = TPY/base_unit*new_unit)
      
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
    
    return(return_table)
  }

#This takes in a base emissions table and a pt removal table. It subtracts the
#point sources for any given scc/pollutant/county/year combo from the bulk 
#emissions table. It doesn't look for any errors or anything though.
subtract_pt_sources <- function(base_emission_table, pt_source_table){
  merged_table <- left_join(base_emission_table,pt_source_table,by=c('FIPS','SCC','year','pollutant')) %>%
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
  return_table <- merged_table %>% 
    #if you do '==FALSE' or something, it doesn't work with NAs. Use '%in% FALSE'
    mutate(TPY = ifelse(is_point %in% FALSE,
                        TPY1+TPY.y,
                        TPY1)) %>%
    #drop the old TPY columns
    select(-c(TPY.x,TPY.y,is_point,TPY1))
  return(return_table)
}


#oinv = old_inventory; ninv = new_inventory
#if we don't hide_pct, then we will display the percent change
#between the two inventories too.
compare_inventories <-
  function(oinv, ninv, hide_pct = TRUE) {
    oinv <- oinv %>% rename(old_tpy = TPY)
    ninv <- ninv %>% rename(new_tpy = TPY)
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
