library(tidyverse)
library(readxl)
library(janitor)

tday <- gsub("-","", Sys.Date())

out_drive <- "V:/Output/Richerson other/Research mortality/2021/"

if(!dir.exists(out_drive)){dir.create(out_drive)}

res_dat <- read_csv("ReportCatchDetailed_20220725.csv") %>% 
  group_by(grouping = grouping_name, species = common_name) %>% 
  summarise(total_mort_mt = sum(catch_including_mortality))

write_csv(res_dat, paste0(out_drive, "groundfish_res_mort_", tday,".csv"))

#just look at which species have highest catches
test <- read_csv("ReportCatchDetailed_20220725.csv") %>% 
  arrange(desc(total_catch_mt)) %>% 
  as.data.frame()

#check that the data in the detailed catch report matches what's in the permit summary
permits_1 <- unique(read_csv("ReportCatchDetailed_20220725.csv")$permit_number)

permits_2 <- unique((read_csv("ReportPermitSummary_2021data.csv") %>% filter(delivery_status == "Delivered"))$permit_number)

setdiff(permits_2, permits_1)

#"LOA-01-2021" "LOA-14-2021" "SRP-11-2021"

#LOA-01 = "IPHC Fishery-independent Setline Survey" - Kayla Ualesi
#LOA-14-2021 = "WDFW Nearshore Rockfish Rod and Reel Survey" - Robert Davis
#SRP-11-2021 ken.massee

########################################

#2023-06-06: Getting spreadsheets directly from submitters. I transcribed the SRP-11 data from "2021 By-Catch -3.xlsx" (sent in by Ken) to "researchCatch_SRP-11-2021.xlsm"

loa01 <- read_xlsx("IPHC FISS 2021_ResearchCatch.xlsm", sheet = "Groundfish", skip = 4) %>% 
  clean_names() #no mort credits

srp11 <- read_xlsx("researchCatch_SRP-11-2021.xlsm", sheet = "Groundfish", skip = 4) %>% 
  clean_names() #no mort credits

loa14 <- read_xlsx("researchCatch_LOA-14-2021.xlsm", sheet = "Groundfish", skip = 4) %>% 
  clean_names() %>% 
  mutate(percent_released_at_depth = ifelse(is.na(percent_released_at_depth), 0, percent_released_at_depth)) %>% 
  mutate(mort_rate = case_when(species_common_name == "Yelloweye Rockfish" & depth_captured_fm == "10-30" ~ 0.26,
                               species_common_name == "Yelloweye Rockfish" & depth_captured_fm == "20-30" ~ 0.27,
                               species_common_name == "Yelloweye Rockfish" & depth_captured_fm == "30-50" ~ 0.57,
                               TRUE ~ 1)) %>% 
  mutate(est_mort_mt = mort_rate * (percent_released_at_depth * as.numeric(total_catch_mt)) + ((1 - percent_released_at_depth) * as.numeric(total_catch_mt)))

old_data <- read_csv(paste0(out_drive, "groundfish_res_mort_20220725.csv"))

new_data <- bind_rows(loa01, srp11) %>% 
  select(grouping,
         species = species_common_name,
         total_mort_mt = total_catch_mt) %>% 
  bind_rows(loa14 %>% 
              select(grouping, 
                     species = species_common_name,
                     total_mort_mt = est_mort_mt)) %>% 
  bind_rows(old_data) %>% 
  group_by(grouping, species) %>% 
  summarise(total_mort_mt = sum(total_mort_mt, na.rm = TRUE)) %>% 
  filter(total_mort_mt > 0)

write_csv(new_data, paste0(out_drive, "groundfish_res_mort_", tday,".csv"))

compare_mort <- new_data %>% 
  full_join(old_data %>% rename(mort_old = total_mort_mt)) %>% 
  mutate(ratio = total_mort_mt/mort_old) %>% 
  rename(mort_corrected = total_mort_mt) %>% 
  mutate(mag = ifelse(mort_corrected > 0.25, "mt > 0.25", "mt < 0.25"))

compare_mort_plot <- compare_mort %>% 
  filter(is.na(ratio) | ratio > 1.05) %>% 
  #filter(ratio > 1.05) %>% 
  mutate(ns_grp = ifelse(grepl("North", grouping), " North", ""),
         ns_grp = ifelse(grepl("South", grouping), " South", ns_grp),
         species_ns = paste0(species, ns_grp)) %>% 
  gather(cat, mt, mort_corrected:mort_old) %>% 
  ggplot(aes(x = species_ns, y = mt, group = cat, fill = cat))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  ggtitle("Corrected 2021 research mortality comparison (species w/ >5% change)")+
  theme_bw()+
  theme(legend.title=element_blank())+
  theme(axis.title.y = element_blank())+
  facet_wrap(~mag, scales = "free")

ggsave(compare_mort_plot, file = paste0(out_drive, "research_mortality_comparison_plot.png"))



