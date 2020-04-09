#######Combining, editing, and summarizing 2019 research mortality data
#######K. Richerson

#/opt/R/64-bit/R-3.5.1/bin/R

rm(list=ls())

library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

#Read in data

in_drive <- "~/observer/Input/Richerson/Research mortality/2019/Original Spreadsheets/Original Spreadsheets/"

out_drive <- "~/observer/Output/Richerson other/Research mortality/2019/"

tday <-gsub("-", "", Sys.Date())

#Test out a couple to see how they read in if you want
# srp02 <- read_xlsx(path = paste0(in_drive,"wells_2019 Research Mortality Data Entry Spreadsheet.xlsx"), sheet = "Groundfish")
# loa30 <- read_xlsx(path = paste0(in_drive,"Research_Mortality_LOA-30-2019.xlsm"), sheet = "Groundfish")
# srp12<- read_xlsx(path = paste0(in_drive,"Research Mortality Data Entry Spreadsheet 2019 Data_updated Oct. 2019_SRP-12-2019 NWFSC H&L survey.xlsm"), sheet = "Other Species")

#Get names of excel files that we will be reading in
xls_file_names <- list.files(in_drive)[grepl(".xlsm",list.files(in_drive)) | grepl(".xlsx",list.files(in_drive))]

#Here are the permits that correspond with the spreadsheets:
# SRP-02 = wells_2019 Research Mortality Data Entry Spreadsheet
# SRP-06 =  "Research Mortality Data Entry Spreadsheet 2019 Data_updated Oct. 2019_Final.xlsm" ***
# SRP-09 = "SRP 2019 Research Mortality Data Entry SRP-09b.xlsm" 
# SRP-11 = "2019 Research Mortality SRP 11 2019.xlsm"
# SRP-12 =  "Research Mortality Data Entry Spreadsheet 2019 Data_updated Oct. 2019_SRP-12-2019 NWFSC H&L survey.xlsm"
# SRP-22 = "2019_catch_report_SRP_22_2019.xlsm"
# SRP-32 = "Research Mortality Data Entry Spreadsheet 2019 Data_SRP_32_2019.xlsm"
# SRP-101 = "Burke_2019 Research Mortality Data Entry Spreadsheet.xlsm"
# LOA-01 = "2019 Research Mortality Data Entry Spreadsheet LOA-01-2019.xlsm"
# LOA-03 = "20200218_SUDMANT_2019ResearchMortality_LOA-3-2019.xlsm"
# LOA-14 = "LOA-14-2019_CatchReport.xlsm"
# LOA-16 (no GF catch...so N/A)
# LOA-23 (catch = 0, they told me in an email)
# LOA-30 = "Research_Mortality_LOA-30-2019.xlsm" 

length(xls_file_names) #Should be 12

#See if there are non-standard tab names (SRP-06 has "Amended" for data corrections)
xls_file_names %>%
  purrr::map(function(file_name){ #iterate through each file name and get all sheet names in a list
    
    excel_sheets(paste0(in_drive, file_name))
    
  }) -> sheet_list

unique(unlist(sheet_list)) 
#Looks like just SRP-06 is different. Can ignore the "Home" and "Sheet1" sheets.

#Read in all groundfish sheets into a list. 
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name. SRP-06 has nonstandard sheet names.
    
    if(file_name == "Research Mortality Data Entry Spreadsheet 2019 Data_updated Oct. 2019_Final.xlsm")
    {
      
      read_xlsx(paste0(in_drive, file_name),sheet="Groundfish Amended")
      
    }else{
      
      read_xlsx(paste0(in_drive, file_name),sheet="Groundfish")
      
    }
    
  }) -> gfish_list

#Read in the "other species" sheets.
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name. SRP-06 has nonstandard sheet names.
    
    if(file_name == "Research Mortality Data Entry Spreadsheet 2019 Data_updated Oct. 2019_Final.xlsm")
    {
      
      read_xlsx(paste0(in_drive, file_name),sheet="Other Species Amended Again")
      
    }else{
      
      read_xlsx(paste0(in_drive, file_name),sheet="Other Species")
      
    }
    
  }) -> other_list

#Finally, salmon 
xls_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    
    read_xlsx(paste0(in_drive, file_name),sheet="Salmon - Trout - Steelhead")
    
  }) -> salmon_list

#Function to capitalize the first letter of each word in a string EXCEPT "and" (for Black and Yellow Rockfish, etc). This is to make capitalization consistent across species names when needed
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  s2 <- paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
  s3 <- gsub("And", "and", s2)
  return(s3)
}

#Who is using the old spreadsheet? Figure out who had blue/deacon catch AND old groupings (no "Blue/deacon/black rockfish (Oregon)" grouping present in groundfish sheet). I *think* other differences between old and new spreadsheets don't impact the output, e.g. there is no cowcod south that would need to be split into Conception and Monterey.  
gfish_check <- bind_rows(gfish_list , .id = "survey_label") %>% #this lets us look up which survey an entry came from if need to track down oddities
  clean_names() %>% #the names parsed by read_xls are annoying to work with 
  select(-x7, -x8) %>% #these are empty columns
  rename(grouping = x1, species = x2, catch_mt = x3, depth_captured = only_if_requesting_survival_credit, released_at_depth = x5, notes = x6) %>% 
  group_by(survey_label) %>% 
  mutate(catch_mt = as.numeric(catch_mt)) %>% 
  mutate(total_bd = sum(catch_mt[species %in% c("Blue/Deacon Rockfish", "Black Rockfish")], na.rm=T)) %>% 
  mutate(flag = ifelse(unique(total_bd)>0 & !"Blue/deacon/black rockfish (Oregon)" %in% grouping, "bad","ok")) %>% 
  filter(species %in% c("Black Rockfish", "Blue/Deacon Rockfish")) %>% 
  select(survey_label, grouping, species, flag) %>% 
  filter(flag == "bad")

xls_file_names[as.numeric(unique(gfish_check$survey_label))]
#LOA-03 took place in CA and LOA-14 took place in WA, so their blue/deacon/black groupings should be fine. 
#Kinsey (SRP-02) says all their black/blue/deacon catch came from Oregon so will fix below.
#Beth (SRP-06) says their blue/deacon catch was south of 40 10 so will fix below (currently listed as South of 42°N. lat.) 

#Make all groundfish sheets into one DF, fix nonstandard entries, do yelloweye, canary, and cowcod mortality estimates where appropriate. Do in 2 chunks because of pasting limits in Tantalus.   
gfish1 <- bind_rows(gfish_list , .id = "survey_label") %>% #this lets us look up which survey an entry came from if need to track down oddities
  clean_names() %>% #the names parsed by read_xls are annoying to work with 
  select(-x7, -x8) %>% #these are empty columns
  rename(grouping = x1, species = x2, catch_mt = x3, depth_captured = only_if_requesting_survival_credit, released_at_depth = x5, notes = x6) %>% 
  filter(!(is.na(grouping) & is.na(species))) %>% 
  filter(grouping != "Grouping") %>% #Because column headings were read in as entries -- remove them
  filter(catch_mt > 0 & !is.na(catch_mt)) %>% #Only want entries with catches
  #Do a bunch of modifications -- mostly to fix capitalitzation, spacing, etc
  mutate(grouping = ifelse(grouping %in% c("Ecosystem Component","Ecosystem Component Species"), "Ecosystem component species", grouping),
         grouping = gsub("Minor Nearshore Rockfish", "Minor nearshore rockfish", grouping),
         grouping = gsub("Minor Shelf Rockfish", "Minor shelf rockfish", grouping),
         grouping = gsub("Minor Slope Rockfish", "Minor slope rockfish", grouping), 
         grouping = ifelse(species != "Sablefish", gsub("° ","°", grouping), grouping),
         grouping = gsub("Thornyhead", "thornyhead", grouping),
         grouping = gsub("Shared Ecosystem Component Species", "Shared ecosystem component species", grouping),
         grouping = ifelse(species == "Other rockfish", "Other rockfish", grouping),
         grouping = ifelse(grouping == "Cowcod rockfish (Between 40°10' and 34°27' N. lat.)", "Cowcod rockfish (Monterey - Between  40°10' N. lat. and 34°27' N. lat.)", grouping),
         grouping = ifelse(grouping == "Cowcod rockfish (South of 34°27' N. lat.)", "Cowcod rockfish (Conception - South of 34°27' N. lat.)", grouping),
         grouping = ifelse(grouping == "Dungeness Crab", "Dungeness crab", grouping),
         grouping = ifelse(grouping == "Other Groundfish", "Other groundfish", grouping),
         grouping = ifelse(grouping == "Other Non-Groundfish", "Other nongroundfish", grouping),
         grouping = ifelse(grouping == "Pacific spiny dogfish", "Spiny dogfish", grouping),
         grouping = gsub("Pacific Ocean Perch", "Pacific ocean perch", grouping),
         grouping = ifelse(grouping == "Black rockfish (Oregon)", "Blue/deacon/black rockfish (Oregon)", grouping),
         grouping = ifelse(grouping == "Minor nearshore rockfish (South of 42°N. lat.)" & species == "Blue/Deacon Rockfish", "Minor nearshore rockfish (South of 40°10' N. lat.)", grouping),
         grouping = ifelse((species == "Blue/Deacon Rockfish" | species == "Black Rockfish") & survey_label == 12, "Blue/deacon/black rockfish (Oregon)", grouping),
         species = ifelse(species == "Other rockfish", "Rockfish Unid", species),
         species = ifelse(species == "Black/Roughtail skate", "Black Skate", species),
         species = ifelse(species == "Curlfin sole/turbot", "Curlfin Turbot", species),
         species = gsub("Kelp greenling ", "Kelp greenling    ", species), 
         species = ifelse(species == "Southern rock sole", "Rock Sole", species),
         species = ifelse(species == "Pacific spiny dogfish", "Spiny Dogfish Shark", species),
         species = ifelse(species == "Shark Unid", "Shark and Skate Unid", species)) %>% 
  rowwise() %>% 
  mutate(species = simpleCap(species))

gfish <- gfish1 %>%
  ungroup() %>% 
  #There are only a few (3 rows) that are eligible for survival credits and all need some editing
  mutate(depth_captured_fm = case_when(depth_captured == "<50 fathoms" ~ 50,
                                       notes == "20-30 fm, 100% released" ~ 30,
                                       notes == "50-100 fm, 100% released at depth" ~ 100)) %>% 
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
  mutate(est_mort_mt = mort_rate * as.numeric(catch_mt))

#Double check those maybe eligible for survival credits:
filter(gfish, species %in%c("Canary Rockfish", "Cowcod Rockfish", "Yelloweye Rockfish")
       & (!is.na(depth_captured) | !is.na(released_at_depth) | !is.na(notes)))%>%
  #select(survey_label, species, depth_captured, released_at_depth, notes) %>% 
  as.data.frame()
#9 (SRP-12) and 12 (SRP-02) don't provide complete information (missing depth captured or whether they were released at depth). I'm assuming they don't have depth info or that they were not released at depth. 


#Make sure that there are no species - grouping combos in the data that are NOT in our master 2019 spreadsheet. 
gfish_master <- read_xlsx("~/observer/Input/Richerson/Research mortality/2019/Research Mortality Data Entry Spreadsheet 2019 Data.xlsm", sheet = "Groundfish") %>% 
  clean_names() %>% 
  select(x1, x2) %>% 
  rename(grouping = x1, species = x2) %>% 
  filter(!(is.na(grouping) & is.na(species))) %>% 
  filter(grouping != "Grouping")

#Compare master list and data species/grouping combinations to make sure there are no combinations in the data that are not in the master spreadsheet
setdiff((paste0(gfish$grouping, gfish$species)), (paste0(gfish_master$grouping, gfish_master$species)))
#Looks good

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
  
  #This deals with the trawl survey data
  if(names(df)[1] == "Please confirm that the species is not already present under the \"Groundfish\" tab." & df[2,2] == "Common Name")
  {
    names(df) <- c("scientific_name", "Species", "Catch Total (#)", "Weight", "Notes")
    df <- select(df, "Species", "Catch Total (#)", "Weight", "Notes", "scientific_name")
  }
  
  #Remove rows with NAs and rows that contain column names, make weight/counts numeric (otherwise can't bind rows)
  df <- df %>% 
    filter(!is.na(Weight) & !Species %in% c("Species", "Common Name")) %>% 
    mutate(`Catch Total (#)` = as.numeric(`Catch Total (#)`),
           Weight = as.numeric(Weight))
  
  #If want to print the sheet numbers with rockfish...
  #if(sum(grepl("rockfish",df$Species)| grepl("Rockfish",df$Species))>0){print(i)} # | grepl("Rockfish",df$Species)
  
  other_list2[[i]] <- df
  
}

#Combine, then figure out what groups to assign
other <- bind_rows(other_list2, .id = "survey_label") %>% 
  rowwise() %>% 
  mutate(species = simpleCap(Species)) #Imperfect bc some are listed with scientific names but oh well...

#Do we have rockfish, salmon, etc in the "Other" tabs? Note that https://www.nwfsc.noaa.gov/research/divisions/fram/documents/gf_fmp_ch3.pdf says "The category “rockfish” includes all genera and species of the family Scorpaenidae, even if not listed, that occur in the Washington, Oregon, and California area. The Scorpaenidae genera are Sebastes, Scorpaena, Sebastolobus, and Scorpaenodes".

other$species[other$species %in% gfish_master$species]
# [1] "Skate Unid"          "Silvergray Rockfish" "Sharpchin Rockfish"
# [4] "Lanternfish Unid"    "Yellowtail Rockfish"


other$species[grepl("Rockfish", other$species)]
# [1] "Silvergray Rockfish"    "Whitespeckled Rockfish" "Sharpchin Rockfish"
# [4] "Yellowtail Rockfish"

other$species[grepl("Salmon", other$species)]
#[1] "Chinook Salmon"     "King-of-the-Salmon" "King Of The Salmon"

#For ECS and shared ECS, see \\nwcfile\FRAM\Observer\observerData\Analysis_Library\Product Library\2019\Bycatch Reports\Groundfish Mortality\Research Mortality\Pages from GF_FMP_FINAL_Mar2016_Mar282016.pdf and ECS and Shared ECS 2017-08-09.xlsx in the in_drive

ecs <- read_xlsx( "~/observer/Input/Richerson/Research mortality/2019/ECS and Shared ECS 2017-08-09.xlsx") %>% 
  clean_names() %>% 
  select(-x3) %>% 
  distinct()#there's a repeated row, remove

#Double check some of the ECS and shared ECS species that might not exactly match the names from the spreadsheet 
#Should Skate Unid be an ECS? Not sure we can say they are "Endemic species in the family Arhynchobatidae" without more info. HOWEVER the groundfish master list has "Skate Unid" as ECS so to be consistent maybe we should include them as ECS? That's what I'm going with for now...
other$species[grepl("Skate", other$species)]
#"Skate Unid" 

other$species[grepl("Grenadier", other$species)]
#none

other$species[grepl("Codling", other$species) | grepl("Flatnose", other$species)]
# "Hundred Fathom Codling" - no

other$species[grepl("Ratfish", other$species) | grepl("Rat Fish", other$species)]
#none

other$species[grepl("Herring", other$species)]
#Pacific Herring - no

other$species[grepl("Sand Lance", other$species) | grepl("Sandlance", other$species)]
#none

other$species[grepl("Saury", other$species)]
#none

other$species[grepl("Silverside", other$species)]
#none

other$species[grepl("Smelt", other$species) | grepl("smelt", other$species)] #lower case to capture Blacksmelt, etc
#none

other$species[grepl("Squid", other$species) | grepl("Gonatus", other$species) | grepl("squid", other$species)]
#We want Pelagic squid in the families Cranchiidae, Gonatidae, Histioteuthidae, Octopoteuthidae, Ommastrephidae except Humboldt squid (Dosidicus gigas), Onychoteuthidae, and Thysanoteuthidae
# I believe Shared ECS should include these in addition to what's in the list:  "Gonatid Squid", "Gonatopsis Borealis (squid)", "Gonatus Onyx (sqyid)" "Cranchia Scabra (squid)" "Octopoteuthis Deletron (squid)". See below for how I wrangle them in.  

other$species[grepl("Lampanyctus", other$species) | grepl("Tarletonbeania", other$species) | grepl("Lanternfish", other$species)]
# "Tarletonbeania Lanternfish Unid" should be included as "Lanternfish Unid"
# Presumably Broadfin Lanternfish should be included as a SECS b/c they are also myctophids

#Note we are supposed to include mesopelagic fishes in the families Myctophidae, Bathylagidae, Paralepididae, and Gonostomatidae. I may be missing some of these if they're not in the ECS spreadsheet above. 

#Now assign all groupings for the "other" species. Most get "Other nongroundfish".
#Note: Yellowtail Rockfish from SRP-02 are grouped north per email with Kinsey. 
#Note: it appears that the trawl survey (SRP-06, "Research Mortality Data Entry Spreadsheet 2019 Data_updated Oct. 2019_Final.xlsm") has their Chinook salmon listed in both the Salmon and Other Species tabs, so can ignore the salmon in the latter tab. 
#Also correcting a bunch of miscellaneous species that I think were off, removing eulachon
#Doing this in two parts because otherwise chain is too long for Tantalus
other_groups1 <- filter(other, !species %in% c("Chinook Salmon", "Eulachon")) %>% 
  mutate(species = ifelse(is.na(Species) & !is.na(scientific_name), scientific_name, species),
         species = ifelse(species == "Gonatid Squid", "Gonatus Unid", species),
         species = ifelse(species == "Gonatopsis Borealis (squid)", "Boreopacific Armhook Squid", species),
         species = ifelse(species == "Gonatus Onyx (sqyid)", "Clawed Armhook Squid", species),
         species = ifelse(species == "Cranchia Scabra (squid)", "Rough Cranch Squid", species), #FAO common name 
         species = ifelse(species == "Octopoteuthis Deletron (squid)", "Octopus Squid", species), #I think?
         species = ifelse(species == "Tarletonbeania Lanternfish Unid", "Lanternfish Unid", species),
         species = ifelse(species == "Blue Laternfish", "Blue Lanternfish", species),
         species = ifelse(species == "Wolf-eel", "Wolf Eel", species),
         species = ifelse(species == "Squat Lobsters/pinch Bugs Unid", "Squat Lobsters and Pinch Bugs Unid", species),
         species = ifelse(species == "Roskfish(copper Group)", "Rockfish Unid", species), #I guess?
         species = ifelse(species == "Nudibranch Sp. A", "Nudibranch Unid", species), #?
         species = ifelse(grepl("Abraliopsis Felis", species), "Abraliopsis felis", species),
         species = ifelse(species == "Aurelia Aurita", "Aurelia aurita", species),
         species = ifelse(species == "Aurelia Labiata", "Aurelia labiata", species),
         species = ifelse(species == "Egg-Yolk Jellyfish", "Egg-yolk Jellyfish", species),
         species = ifelse(species %in% c("King Of The Salmon", "King-of-the-salmon","King-of-the-Salmon"), "King-of-the-salmon", species),
         species = ifelse(species %in% c("Yellow Ringed Octopus", "Yellow-Ringed Octopus"), "Yellow-ringed Octopus", species),
         species = ifelse(species == "Mackerel (unidentified Spp)", "Mackerel Unid", species),
         species = ifelse(species == "Sixgill Shark", "Bluntnose Sixgill Shark", species), #
         species = ifelse(species == "Chiroteuthis Calyx (squid)", "Chiroteuthis calyx", species),
         species = ifelse(species == "Aequorea", "Aequorea Unid", species),
         species = ifelse(species == "Aequorea Sp. Unid", "Aequorea Unid", species),
         species = ifelse(species == "Animalia", "Animalia Unid", species), #?
         species = ifelse(species == "Articulated Bamboo Corals", "Articulated Bamboo Coral Unid", species),
         species = ifelse(species == "Bathypelagic Shrimp", "Bathypelagic Shrimp Unid", species),
         species = ifelse(species == "Benthoctopus", "Benthoctopus Unid", species),
         species = ifelse(species == "Brisaster", "Brisaster Unid", species),
         species = ifelse(species == "Brittlestars Unid", "Brittlestar Unid", species),
         species = ifelse(species == "Buccinum", "Buccinum Unid", species),
         species = ifelse(species == "Calcigorgia", "Calcigorgia Unid", species), 
         species = ifelse(species == "Carinariidae", "Carinariidae Unid", species), #i forget the singular/plural rules here
         species = ifelse(species == "Ceramaster", "Ceramaster Unid", species),
         species = ifelse(species == "Chrysaora Jellyfish", "Chrysaora Jellyfish Unid", species),
         species = ifelse(species == "Chrysopathes", "Chrysopathes Unid", species),
         species = ifelse(species == "Colus", "Colus Unid", species),
         species = ifelse(species == "Crangon", "Crangon Unid", species),
         species = ifelse(species == "Crescent Sea Cucumbers", "Crescent Sea Cucumber", species),
         species = ifelse(species == "Crested Red Mysids", "Crested Red Mysid Unid", species),
         species = ifelse(species == "Ctenophore", "Ctenophore Unid", species),
         species = ifelse(species == "Deep Sea Free Living Sponges", "Deep Sea Free Living Sponge Unid", species),
         species = ifelse(species == "Deep Sea Papillose Sea Cucumbers", "Deep Sea Papillose Sea Cucumber Unid", species),
         species = ifelse(species == "Deepsea Jellyfish", "Deep-sea Jellyfish Unid", species), #I think?
         species = ifelse(species == "Deep-sea Shrimps", "Deep-sea Shrimp Unid", species),
         species = ifelse(species == "Halipteris", "Halipteris Unid", species),
         species = ifelse(species == "Halipteris (Deepsea)", "Halipteris Unid", species)) 

other_groups <- other_groups1 %>% 
  mutate(species = ifelse(species == "Hormathiid Anemones Unid", "Hormathiid Anemone Unid", species),
         species = ifelse(species == "Henricia", "Henricia Unid", species),
         species = ifelse(species == "Hydrozoa", "Hydrozoa Unid", species),
         species = ifelse(species == "Lampshells Unid", "Lampshell Unid", species),
         species = ifelse(species == "Lava Anemones", "Lava Anemone", species), #??? Google gives me nothing...
         species = ifelse(species == "Lithodes", "Lithodes Unid", species),
         species = ifelse(species == "Luidia", "Luidia Unid", species),
         species = ifelse(species == "Lycodapus", "Lycodapus Unid", species),
         species = ifelse(species == "Mediaster", "Mediaster Unid", species),
         species = ifelse(species == "Moon Jellies", "Moon Jellyfish", species),
         species = ifelse(species == "Mussel Unid (Mytilidae)", "Mussel Unid", species),
         species = ifelse(species == "Neptunea", "Neptunea Unid", species),
         species = ifelse(species == "Oegopsida (squid)", "Oegopsida Unid", species),
         species = ifelse(species == "Oneirodes", "Oneirodes Unid", species),
         species = ifelse(species == "Ophiopholis", "Ophiopholis Unid", species),
         species = ifelse(species == "Ophiura", "Ophiura Unid", species),
         species = ifelse(species == "Orange Actinistolids", "Orange Actinistolid", species),
         species = ifelse(species == "Orange Sun Stars", "Orange Sun Star", species),
         species = ifelse(species == "Pacific Sergestid Shrimp", "Pacific Sergestid", species),
         species = ifelse(species == "Pale Rough Pinch", "Pale Rough Pinch Bug", species),
         species = ifelse(species == "Pandalus", "Pandalus Unid", species),
         species = ifelse(species == "Pinch Bug", "Pinch Bug Unid", species),
         species = ifelse(species == "Pink Hormathiid Anemones", "Pink Hormathiid Anemone Unid", species),
         species = ifelse(species == "Plumarella", "Plumarella Unid", species),
         species = ifelse(species == "Porifera", "Porifera Unid", species),
         species = ifelse(species == "Pteraster", "Pteraster Unid", species),
         species = ifelse(species == "Rabbit-Eared Salp", "Rabbit-eared Salp", species),
         species = ifelse(species == "Red Mysids", "Red Mysid Unid", species),
         species = ifelse(species == "Red Sea Fans", "Red Sea Fan Unid", species),
         species = ifelse(species == "Salpidae", "Salpidae Unid", species),
         species = ifelse(species == "Salps Unid", "Salp Unid", species),
         species = ifelse(species == "Scallop Unid (Pectinidae)", "Scallop Unid", species),
         species = ifelse(species == "Sea Cockroaches Unident", "Sea Cockroaches Unid", species),
         species = ifelse(species == "Sea Pigs", "Sea Pig Unid", species),
         species = ifelse(species == "Short-Spined Pink Star", "Short-spined Pink Star", species),
         species = ifelse(species == "Solariella", "Solariella Unid", species),
         species = ifelse(species == "Solenogasters", "Solenogaster Unid", species),
         species = ifelse(species == "Stomphia", "Stomphia Unid", species),
         species = ifelse(species == "Strongylocentrotus", "Strongylocentrotus Unid", species),
         species = ifelse(species == "Sunrise Jelly", "Sunrise Jellyfish", species),
         species = ifelse(species == "Urticina", "Urticina Unid", species),
         species = ifelse(species == "Wheel Jellies", "Wheel Jellyfish", species),
         species = ifelse(species == "Zoroasteridae", "Zoroasteridae Unid", species)
  ) %>% 
  left_join(ecs) %>% 
  mutate(grouping = ifelse(species == "Boreopacific Armhook Squid", "Shared ecosystem component species", grouping),
         grouping = ifelse(species == "Broadfin Lanternfish", "Shared ecosystem component species", grouping),
         grouping = ifelse(species == "Silvergray Rockfish", "Minor shelf rockfish (South of 40°10' N. lat.)", grouping),
         grouping = ifelse(species == "Whitespeckled Rockfish", "Minor slope rockfish (South of 40°10' N. lat.)", grouping),
         grouping = ifelse(species == "Sharpchin Rockfish", "Minor slope rockfish (South of 40°10' N. lat.)", grouping),
         grouping = ifelse(species == "Yellowtail Rockfish", "Minor shelf rockfish (South of 40°10' N. lat.)", grouping),
         grouping = ifelse(species == "Spiny Dogfish Shark", "Spiny dogfish", grouping),
         grouping = ifelse(species == "Rockfish Unid", "Other rockfish", grouping),
         grouping = ifelse(species == "Skate Unid", "Ecosystem component species", grouping),
         grouping = ifelse(is.na(grouping), "Other nongroundfish", grouping))

#If want to do more spot-checking of species/groups, can look at this object:
other_check <- other_groups %>% 
  select(species, grouping) %>% 
  distinct() %>% 
  arrange(species) %>% 
  mutate(grouping=substr(grouping,1,12)) %>% #Just display first part of grouping
  as.data.frame()

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
           Weight = as.numeric(Weight))
  
  salmon_list2[[i]] <- df
  
}

salmon <- bind_rows(salmon_list2) %>% 
  rowwise() %>% 
  mutate(Notes = ifelse(is.na(Notes) & grepl("hatchery", Species), "hatchery", Notes),
         Species = gsub(" (hatchery)", "", Species, fixed = TRUE)) %>% #For entries where hatchery origin is specified, move that to the notes field
  mutate(Species = simpleCap(Species),
         Species = ifelse(Species == "King Salmon", "Chinook Salmon", Species))
#Note that the trawl survey didn't list the lat/lon of their Chinook catches. 

############Combine, format and output research catch data#################
gfish_other <- gfish %>% 
  select(grouping, species, est_mort_mt) %>% 
  bind_rows(other_groups %>% 
              select(grouping, species, est_mort_mt = Weight)) %>% 
  group_by(grouping, species) %>% 
  summarise(total_mort_mt = sum(est_mort_mt))

#Make sure no species or weights got lost along the way
setdiff(gfish_other$species, c(unique(other_groups$species), unique(gfish$species)))
#looks good
sum(gfish$est_mort_mt) + sum(other_groups$Weight) == sum(gfish_other$total_mort_mt)
#looks good

#Again, if want to spot check can look at this:
gfish_other_check <- gfish_other %>% 
  select(species, grouping) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(species) %>% 
  mutate(grouping=substr(grouping,1,12)) %>% #Just display first part of grouping
  as.data.frame()

#Make sure that the top species look right in terms of weight -- no wierd giant mortalities
top_n(ungroup(gfish_other), n = 20, wt = total_mort_mt) %>% arrange(desc(total_mort_mt)) %>% as.data.frame()
#Seems ok as far as I know???

#output gfish/other
write_csv(gfish_other, paste0(out_drive, "groundfish_other_sp_res_mort_", tday, ".csv"))

#Output salmon
write_csv(salmon, paste0(out_drive, "salmon_res_mort_", tday, ".csv"))

