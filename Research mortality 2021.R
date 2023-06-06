library(tidyverse)

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
#"" ken.massee