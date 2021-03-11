
#These are the shame functions. I shouldn't have to use these, but for now, I will.
#eventually I will not rely on the input table but for now I will
shame_input_table_references <-
  read_xlsx('ref_workbooks/new_input_tables20210126.xlsx',sheet = 'ranges')

shame_pull_input_table_from_workbook <-
  function(table_name,
           st_year = input_st,
           e_year = input_end,
           input_refs = shame_input_table_references,
           convert_fips = TRUE) {
    
    cell_range <-
      shame_input_table_references %>% filter(table_id == table_name)
    cell_range <- cell_range[[2]] 
    raw_input_table <-
      read_xlsx('ref_workbooks/new_input_tables20210126.xlsx',
                sheet = 'tables',
                range = cell_range)
    #turn counties to FIPS
    if (convert_fips) {
      ncounties <- dim(raw_input_table)[1]
      for (i in 1:ncounties) {
        raw_input_table[i, 1] <-
          as.factor(county_to_fip(raw_input_table[[i, 1]]))
      }
    }
    sorted_input_table <-
      pivot_longer(
        raw_input_table,
        cols = colnames(raw_input_table)[-1],
        names_to = 'year',
        values_to = 'unit'
      ) %>%
      rename('county' = all_of(table_name)) %>%
      filter(year %in% st_year:e_year)
  }

###################################
###################################
###################################
###################################
###################################
input_st <- 2014
input_end <- 2030
#pull some projection tables that will be used often

pp <- shame_pull_input_table_from_workbook('populationprojection')
manufacturing_employment <- shame_pull_input_table_from_workbook('ManEmp')
AgJobs <- shame_pull_input_table_from_workbook('AgJobs')
LUST <- shame_pull_input_table_from_workbook('LUST')
NonresidentialConstruction <- shame_pull_input_table_from_workbook('NonresidentialConstruction')
wood <- shame_pull_input_table_from_workbook('wood')
#OTHER EMPLOYMENT/OTHER GENERAL TABLES - TBD





