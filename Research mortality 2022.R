library(tidyverse)

tday <- gsub("-","", Sys.Date())

out_drive <- "V:/Output/Richerson other/Research mortality/2022/"

if(!dir.exists(out_drive)){dir.create(out_drive)}

res_dat <- read_csv("ReportCatchDetailed_2022data_20230606.csv") %>% 
  group_by(grouping = grouping_name, species = common_name) %>% 
  summarise(total_mort_mt = sum(catch_including_mortality))

write_csv(res_dat, paste0(out_drive, "groundfish_res_mort_", tday,".csv"))

#just look at which species have highest catches
test <- read_csv("ReportCatchDetailed_2022data_20230606.csv") %>% 
  arrange(desc(total_catch_mt)) %>% 
  as.data.frame()

#check that the data in the detailed catch report matches what's in the permit summary
permits_1 <- unique(read_csv("ReportCatchDetailed_2022data_20230606.csv")$permit_number)

permits_2 <- unique((read_csv("ReportPermitSummary_2022data_20230606.csv") %>% filter(delivery_status == "Delivered"))$permit_number)

setdiff(permits_2, permits_1)
setdiff(permits_1, permits_2)

unique((read_csv("ReportPermitSummary_2022data_20230606.csv") %>% filter(delivery_status != "Delivered"))$permit_number)
#Monica: "SRP-22-2022 was unable to sample last year so they have no data to report. "
#"I heard back from Leif about LOA-18-2022. ODFW has a policy that does not allow them to use the research catch app. However, he let me know that they did not catch anything in 2022 so there is no data to submit. 
