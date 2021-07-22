#######Combining, editing, and summarizing 2019 research mortality data
#######K. Richerson

#/opt/R/64-bit/R-3.5.1/bin/R

rm(list=ls())

library(tidyverse)
library(purrr)
library(readxl)
library(janitor)
library(stringr)

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

#Now combine the "other" species sheets and check which ones might need to be added to a different grouping
#They didn't read in nicely and so will need some extra work.

other_list2 <-list()

for(i in 1:length(other_list))
{
  df <- other_list[[i]]
  #This deals with the ones that read in funky EXCEPT the trawl survey data that has a "Common Name" column and scientific name in the "Species" column
  if(names(df)[1] == "Please confirm that the species is not already present under the \"Groundfish\" tab." & df[2,2] != "Common Name")
  {
    names(df) <- c("Species", "Catch Total (#)", "Weight", "Notes")
  }

  #Remove rows with NAs and rows that contain column names, make weight/counts numeric (otherwise can't bind rows)
  df <- df %>% 
    filter(!is.na(Weight) & !Species %in% c("Species", "Common Name")) %>% 
    mutate(`Catch Total (#)` = as.numeric(`Catch Total (#)`),
           Weight = as.numeric(Weight),
           Species = as.character(Species))
  
  #If want to print the sheet numbers with rockfish...
  #if(sum(grepl("rockfish",df$Species)| grepl("Rockfish",df$Species))>0){print(i)} # | grepl("Rockfish",df$Species)
  
  other_list2[[i]] <- df
  
}

#Finally, salmon 
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    
    read_xlsx(paste0(in_drive, file_name),sheet="Salmon - Trout - Steelhead")
    
  }) -> salmon_list

gfish <- bind_rows(gfish_list , .id = "survey_label") %>% #this lets us look up which survey an entry came from if need to track down oddities
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
  filter(catch_mt > 0 & !is.na(catch_mt)) %>%  #Only want entries with catches
  #There are only a few (2 rows) that are eligible for survival credits and all need some editing beacuse info is entered in the notes section instead of the correct column
  mutate(depth_captured_fm = case_when(species == "Canary Rockfish" & notes == "30-40 fm ; 82.7% released at depth" ~ 40,
                                       species == "Yelloweye Rockfish" & notes == "30-40 fm; 50% released at depth" ~ 40)) %>% 
  mutate(released_at_depth = case_when(species == "Canary Rockfish" & notes == "30-40 fm ; 82.7% released at depth" ~ .827,
                                       species == "Yelloweye Rockfish" & notes == "30-40 fm; 50% released at depth" ~ .5,
                                       TRUE ~ 1)) %>% 
  #Don't actually need all these but easier to just include all
  mutate(mort_rate = case_when(species == "Canary Rockfish" & depth_captured_fm <= 10 ~ 0.21,
                               species == "Canary Rockfish" & depth_captured_fm >10 & depth_captured_fm <= 30 ~ 0.25,
                               species == "Canary Rockfish" & depth_captured_fm >30 & depth_captured_fm <= 50 ~ 0.48,
                               species == "Canary Rockfish" & depth_captured_fm >50 & depth_captured_fm <= 100 ~ 0.57,
                               species == "Cowcod Rockfish" & depth_captured_fm <= 10 ~ 0.21,
                               species == "Cowcod Rockfish" & depth_captured_fm >10 & depth_captured_fm <= 20 ~ 0.35,
                               species == "Cowcod Rockfish" & depth_captured_fm >20 & depth_captured_fm <= 30 ~ 0.52,
                               species == "Cowcod Rockfish" & depth_captured_fm >30 & depth_captured_fm <= 100 ~ 0.57,
                               species == "Yelloweye Rockfish" & depth_captured_fm <= 10 ~ 0.22,
                               species == "Yelloweye Rockfish" & depth_captured_fm >10 & depth_captured_fm <= 30 ~ 0.26,
                               species == "Yelloweye Rockfish" & depth_captured_fm >30 & depth_captured_fm <= 50 ~ 0.27,
                               species == "Yelloweye Rockfish" & depth_captured_fm >50 & depth_captured_fm <= 100 ~ 0.57,
                               TRUE ~ 1)) %>% 
  mutate(est_mort_mt = mort_rate * (released_at_depth * as.numeric(catch_mt)) + ((1 - released_at_depth) * as.numeric(catch_mt)))

#Make sure the species-grouping combinations match the template
setdiff(distinct(gfish, grouping, species), distinct(gfish_template, grouping, species))

#Double check those maybe eligible for survival credits:
filter(gfish, species %in%c("Canary Rockfish", "Cowcod Rockfish", "Yelloweye Rockfish")
       & (!is.na(depth_captured) | !is.na(released_at_depth) | !is.na(notes)))%>%
  as.data.frame()
#Seems to look good

#Now do Other species
#Combine, then figure out what groups to assign
other <- bind_rows(other_list2, .id = "survey_label") %>% 
  clean_names() %>% 
  mutate(catch_mt = weight/1000) %>%  #weight is in kg
  mutate(species = stringr::word(species, 1, 2)) %>%  #Just need common name, all happen to be the first 2 words
  mutate(species = ifelse(species == "Lonmgspine Combfish", "Longspine Combfish", species)) %>% 
  mutate(species = ifelse(grepl("Sculpin", species), "Sculpin Unid", species)) %>% 
  mutate(grouping = "Other nongroundfish")
  

#for reference: ecs
ecs <- read_xlsx( "X:/Input/Richerson/Research mortality/2019/ECS and Shared ECS 2017-08-09.xlsx") %>% 
  clean_names() %>% 
  distinct()

#MAke sure none of these are actually ECS
unique(other$species %in% ecs$species) #FALSE

#Now combine salmon
#They didn't read in nicely and so using this workaround
salmon_list2 <-list()

for(i in 1:length(salmon_list))
{
  df <- salmon_list[[i]]
  if(names(df)[4] == "Enter Capture Coordinates")
  {
    names(df) <- c("Species", "Catch Total (#)", "Weight", "Lat", "Lon", "Notes")
  }
  
  df <- df %>%
    filter(!is.na(Species) & Species != "Species") %>%
    mutate(`Catch Total (#)` = as.numeric(`Catch Total (#)`),
           Weight = as.numeric(Weight),
           Species = as.character(Species)) 
  
  salmon_list2[[i]] <- df
  
}

salmon <- bind_rows(salmon_list2) %>% 
  rowwise() 

############Combine, format and output research catch data#################
gfish_other <- gfish %>% 
  select(grouping, species, est_mort_mt) %>% 
  bind_rows(other%>% 
              select(grouping, species, est_mort_mt = catch_mt)) %>% 
  group_by(grouping, species) %>% 
  summarise(total_mort_mt = sum(est_mort_mt))


sum(gfish$est_mort_mt) + sum(other$catch_mt) == sum(gfish_other$total_mort_mt)
#looks good


#Make sure that the top species look right in terms of weight -- no wierd giant mortalities
top_n(ungroup(gfish_other), n = 20, wt = total_mort_mt) %>% arrange(desc(total_mort_mt)) %>% as.data.frame()
#Seems ok???

#output gfish/other
write_csv(gfish_other, paste0(out_drive, "groundfish_other_sp_res_mort_", tday, ".csv"))

#Output salmon
write_csv(salmon, paste0(out_drive, "salmon_res_mort_", tday, ".csv"))




