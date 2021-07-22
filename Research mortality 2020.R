#######Combining, editing, and summarizing 2019 research mortality data
#######K. Richerson

#/opt/R/64-bit/R-3.5.1/bin/R

rm(list=ls())

library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

#Read in data

in_drive <- "X:/Input/Richerson/Research mortality/2020/original spreadsheets/"

out_drive <- "X:/Output/Richerson other/Research mortality/2020/"

tday <-gsub("-", "", Sys.Date())

#To check that groundfish species and groupings are all correct, import the 2020 template
gfish_template <- read_xlsx(paste0("X:/Input/Richerson/Research mortality/2020/Research Mortality Data Entry Spreadsheet 2020 Data.xlsm"), sheet="Groundfish") %>% 
  rename(grouping = `...1`, 
         species = `...2`, 
         spatial = `...3`, 
         catch_mt = `...4`, 
         depth_captured = `Only if requesting Survival Credit`, 
         released_at_depth =`...6`,
         notes = `...7`) %>% 
  select(-`...8`, -`...9`) %>% 
  filter(!is.na(grouping) & grouping!= "Grouping")

#Get names of excel files that we will be reading in
xls_file_names <- list.files(in_drive)[grepl(".xlsm",list.files(in_drive)) | grepl(".xlsx",list.files(in_drive))]

#Make sure we have them all
length(xls_file_names) == length(list.files(in_drive))

#Any nonstandard sheet names? No
xls_file_names %>%
  purrr::map(function(file_name){ #iterate through each file name and get all sheet names in a list
    
    excel_sheets(paste0(in_drive, file_name))
    
  }) -> sheet_list

unique(unlist(sheet_list)) 

#Read in all groundfish sheets into a list. 
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name. SRP-06 has nonstandard sheet names.

      read_xlsx(paste0(in_drive, file_name),sheet="Groundfish")

  }) -> gfish_list

#Read in the "other species" sheets.
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name. SRP-06 has nonstandard sheet names.
      read_xlsx(paste0(in_drive, file_name),sheet="Other Species")

  }) -> other_list

#Finally, salmon 
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    
    read_xlsx(paste0(in_drive, file_name),sheet="Salmon - Trout - Steelhead")
    
  }) -> salmon_list

gfish1 <- bind_rows(gfish_list , .id = "survey_label") %>% #this lets us look up which survey an entry came from if need to track down oddities
  clean_names() %>% #the names parsed by read_xls are annoying to work with 
  select(-x8, -x9, -x5) %>% #these are empty columns
  rename(grouping = x1, 
         species = x2, 
         spatial = x3,
         catch_mt = x4, 
         depth_captured = only_if_requesting_survival_credit, 
         released_at_depth = x6, 
         notes = x7) %>% 
  filter(!(is.na(grouping) & is.na(species))) %>% 
  filter(grouping != "Grouping") %>% #Because column headings were read in as entries -- remove them
  filter(catch_mt > 0 & !is.na(catch_mt)) #Only want entries with catches

#Make sure the species-grouping combinations match the template
setdiff(distinct(gfish1, grouping, species), distinct(gfish_template, grouping, species))


