#2) Base tables:
#This generates the fundamental tables we will work with:
#Wagon wheel
#NEI
#population data
#manufacturing employment (need to update method)
#old_model_data (the most up-to-date version of the area inventory)
#SCC layer descriptions
#and others
#
#I name every path I access 'read_path' so that I don't have a ton of variables floating around when I am done with this section. I should only have big data tables when I am done with this section. The only variables declared here are 'read_path',
#'start_year', and 'end_year'. If you have set the work directory to end in
#'main_workbook' then this should work.

#old_model_data, for filling in non-automated data pulls
read_path <- '20190502 NEI Workbook.xlsx'
suppressMessages(old_model_data <- pull_excel_model_data(read_path))
#You can also read from a csv, but I'll leave the default as pulling data from the workbook itself
#read_path <- 'U:/PLAN/michaelallen/main_workbook/2017_model_data20200709.csv'
#old_model_data <- read.csv(read_path)

#SLEIS data dump, ultimately used to pull pt source emissions and
#pt source throughputs
read_path <- 'ref_workbooks/SLEIS detail 2017.csv'
suppressMessages(raw_SLEIS <- read_csv(read_path))
tidy_sleis <- raw_SLEIS %>%
  #get emission unit identifier and process identifier. Both are needed
  #to uniquely define processes.
  select(
    c(
      'Reporting Year':'NAICS (Code)',
      'Emission Unit Identifier',
      'Process Identifier',
      'Process Descr':'SCC (Descr)',
      'Throughput',
      'Throughput UOM (Code)',
      'Throughput Material (Descr)',
      'Pollutant (Code)',
      'Estimated Emissions (Tons)'
    )
  ) %>% drop_na()
wide_sleis <- tidy_sleis %>%
  pivot_wider(names_from = `Pollutant (Code)`,
              values_from = `Estimated Emissions (Tons)`) %>%
  #filter out test facilities from SLEIS. Eventually, hopefully there 
  #will be no test facilities in SLEIS.
  filter(`Facility Identifier`<20000)

#ww, for pulling TPY values and getting up-to-date emission factors
read_path <- 'ref_workbooks/Wagon Wheel Output.csv'
ww <- pull_ww(read_path)

# NEI, for pulling TPY values
nei_path_2017 <- 'ref_workbooks/2017neiApr_nonpoint/esg_cty_scc_12961.csv'
nei <- pull_nei(nei_path_2017, "2017")

# Input tables maintained in excel
input_table_path <- 'ref_workbooks/new_input_tables20210305.xlsx'
input_tables <- pull_input_tables(file_path = input_table_path)

#statewide_compiled, for pulling emissions for pt source subtraction
#read_path <- 'U:/PLAN/CWILLIAMS/Inventory Database/2017 Statewide Inventory/Compiled statewide data.xlsx'
read_path <- 'ref_workbooks/Compiled statewide data.xlsx'
#only take the first 19 columns, after that it's pivot table stuff
suppressMessages(suppressWarnings(statewide_compiled <- read_xlsx(read_path) %>% select(c(,1:19))))

#scc_descriptions
read_path <- 'ref_workbooks/20200326_All_sccs.csv'
suppressMessages(suppressWarnings(sccref <- read_csv(read_path)))

#pollutant_codes, for translating to plaintext
read_path <- 'ref_workbooks/pollutants.xlsx'
pollutant_code_table <- pull_translate_table(read_path)

#pt_source crosswalk table, for getting crosswalk scc values for pt subtraction
read_path <- 'ref_workbooks/Solvents Template FINAL_20200807.xlsx'
suppressMessages(pt_crosswalk_table <- read_xlsx(read_path, sheet = 'Point Source Emissions'))

#this table is from the ICI NEMO. It associates specific fuels with SCCs that require combustion
#when I work to subtract throughputs for pt sources, I use this to target SCCs.
read_path <- 'ref_workbooks/FuelSCCCrosswalkFromICINEMO_20210203.xlsx'
suppressMessages(fuel_crosswalk_table <- read_xlsx(read_path))

# pull controls table
read_path <- 'ref_workbooks/Controls_20210414.xlsx'
suppressMessages(controls <- read_xlsx(read_path, col_types = c("text", 
                                                                "text",
                                                                "text",
                                                                "text",
                                                                "text",
                                                                "numeric",
                                                                "text",
                                                                "text",
                                                                "numeric",
                                                                "numeric",
                                                                "numeric",
                                                                "numeric",
                                                                "logical",
                                                                "logical",
                                                                "text",
                                                                "text")))

# pull projection tables from input_tables and format correctly
project_tables <- c("Wheat","Corn","Hay","Barley","EnergyBySector","PetroleumForecast",
                    "Population","CoalForecast","Steady","Gasoline","GovGas",
                    "USAviationFuel","AgJobs","ManEmp","VMT","WoodBTU","LUST",
                    "NonresidentialConstruction", "Coal","PtCoal","CommercialEmployment",
                    "Kerosene","PtKerosene","HousingKero", "LPG", "PtLPG", "HousingLPG",
                    "NonResNGMMCF", "PtNG", "NonResNGDecatherm")
projection_tables <- pull_projection_tables(table_names = project_tables)
