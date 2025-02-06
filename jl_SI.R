library(tidyverse) #Calling in tidyverse for data management
library(readxl) #Calling in readxl to read in excel files
library(sf)     # Spatial Package
library(terra) #spatial package
library(gstat) #geospatial package
library(tmap) #mapping package
library(dataRetrieval) #Used to retrieve data from USGS
library(viridis)
library('parallel')

#### Calling in isotope data ####

run1 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-03_HT/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  distinct() %>% 
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run2 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-09_HT/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run3 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-13_HT/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs


run4 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-25_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run5 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-27_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run6 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-28_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs


run7 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-05-30_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run8 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-02_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run9 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-04_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run10 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-07_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dxs

run11 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-10_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run12 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-13_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run13 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-15_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run14 <- read_excel("~/Documents/Data/2023_Data/Isotope.Data/2024-06-18_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) %>% #Creating two columns, one is the setting type(Lake or Creek), and also dx
  filter(SITE != "AMK.PRCP.SEPT.2023" & SITE != "AMK.PRCP.AUG.2022" )

run15 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-11-04_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run16 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-11-06_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run17 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-11-10_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run18 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-11-13_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run19 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-11-16_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run20 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-12-04_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run21 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-12-06_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run22 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-12-11_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run23 <- read_excel("~/Documents/Data/2024_Data/isotope.data/2024-12-18_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run24 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2024-12-20_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run25 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-10_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run26 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-15_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(Setting.Type = ifelse(substr(SITE, 1, 1) == "J", "Lake", "Creek"),
         dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run27 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-17_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run28 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-22_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run29 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-25_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run30 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-29_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx

run31 <-  read_excel("~/Documents/Data/2024_Data/isotope.data/2025-01-31_Standard/tmp3_coord_ht_simplified_output.xlsx", sheet = "vial data") %>% #Calling in the excel file
  filter(sample_type == "unknown" & variable != "dxs") %>% #filtering the correct memory correction, sample type, and excluding dxs
  subset(select = -c(organics_flag, sample_type, memcorr_mean, memcorr_sd, memcorr_se, driftcorr_mean, calibrated_u,calibrated_pu, n_injections)) %>% #Removing unneeded columns
  pivot_wider(names_from = variable, values_from = calibrated_mean) %>% #Using pivot wider to get the sample_id column and isotope data wider
  rename(SITE = sample_id) %>%  #Renaming sample_id to match the spatial data
  mutate(dxs = d2H - (8 * d18O),
         SITE = str_replace(SITE, "_0", "")) #Creating two columns, one is the setting type(Lake or Creek), and also dx


iso.2022 <- read_excel("~/Documents/Data/2022_Data/Isotope.Data/JL.Isotopes.xlsx") %>% 
  mutate(Event = as.numeric(Event))

#Joining all the data together
all.iso <- bind_rows(run1, run2) %>% 
  bind_rows(run3) %>% 
  bind_rows(run4) %>% 
  bind_rows(run5) %>% 
  bind_rows(run6) %>% 
  bind_rows(run7) %>% 
  bind_rows(run8) %>% 
  bind_rows(run9) %>% 
  bind_rows(run10) %>% 
  bind_rows(run11) %>% 
  bind_rows(run12) %>% 
  bind_rows(run13) %>% 
  bind_rows(run14) %>% 
  bind_rows(run15) %>% 
  bind_rows(run16) %>%
  bind_rows(run17) %>% 
  bind_rows(run18) %>% 
  bind_rows(run19) %>% 
  bind_rows(run20) %>%
  bind_rows(run21) %>%
  bind_rows(run22) %>% 
  bind_rows(run23) %>%
  bind_rows(run24) %>% 
  bind_rows(run25) %>% 
  bind_rows(run26) %>%
  bind_rows(run27) %>% 
  bind_rows(run28) %>% 
  bind_rows(run29) %>%
  bind_rows(run30) %>% 
  bind_rows(run31) %>% 
  bind_rows(iso.2022) %>% 
  mutate(Setting.Type = case_when(
    str_detect(SITE, regex("J")) ~ "Lake",
    str_detect(SITE, regex("WP")) ~ "Lake",
    SITE == "1" ~ "Lake.outlet", #\\b data \\b tells just look for a single 1 not 10 12 14 etc.
    SITE == "15" ~ "River",
    str_detect(SITE, regex("AMK")) ~ "AMK Tap",
    SITE == "2" ~ "Bay",
    SITE == "8" ~ "Bay",
    SITE == "9" ~ "Bay",
    str_detect(SITE, regex("GV")) ~ "Snow Pack",
    str_detect(SITE, regex("SP")) ~ "Snow Pack",
    str_detect(SITE, regex("RG")) ~ "Rock Glacier",
    TRUE ~ "Stream"))%>% 
  mutate(month = case_when(
    (Event==(1)) ~ 5,
    (Event==(2)) ~ 6,
    (Event==(3)) ~ 6, #\\b data \\b tells just look for a single 1 not 10 12 14 etc.
    (Event==(4)) ~ 7,
    (Event==(5)) ~ 7,
    (Event==(6)) ~ 8,
    (Event==(7)) ~ 7,
    (Event==(8)) ~ 8,
    (Event==(9)) ~ 9,
    (Event==(10)) ~ 6,
    (Event==(11)) ~ 7,
    (Event==(12)) ~ 8,
    (Event==(13)) ~ 9)) %>% 
  mutate(year = case_when(
    (Event == (1)) ~ 2022,
    (Event== (2)) ~ 2022,
    (Event==(3)) ~ 2022, #\\b data \\b tells just look for a single 1 not 10 12 14 etc.
    (Event==(4)) ~ 2022,
    (Event==(5)) ~ 2022,
    (Event==(6)) ~ 2022,
    (Event==(7)) ~ 2023,
    (Event==(8)) ~ 2023,
    (Event==(9)) ~ 2023,
    (Event == (10)) ~ 2024,
    (Event == (11)) ~ 2024,
    (Event == (12)) ~ 2024,
    (Event ==(13)) ~ 2024))


write_csv(all.iso, "~/Documents/Data/Chapter.3/Isotope.Data/isotope.data")
all.iso<-read_csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data")

all.2023 <- all.iso %>% 
  filter(year==2023)
lo.gw <- all.iso %>% 
  filter(Setting.Type== "Lake.outlet" | Setting.Type == "AMK Tap")

iofl <- all.iso %>% 
  filter(SITE == "1" | SITE == "15" ) %>% 
  filter(month == "5" & year == "2022"|month == "8" & year == "2022"|month == "7" & year == "2023"|month == "9" & year == "2023"|
           month == "6" & year == "2024"|month == "7" & year == "2024") %>% 
  group_by(SITE, year)

stream <- all.iso %>% 
  filter(Setting.Type !="Lake" & Setting.Type != "AMK Ranch" & Setting.Type != "AMK.Ranch" & Setting.Type != "AMK Tap"
         & Setting.Type != "Snow Pack" & Setting.Type != "Bay")

stream.2022<- stream %>% 
  filter(Event >=1 & Event <=6)
stream.2023 <- stream %>% 
  filter(Event >= 7 & Event <=9)
stream.2024 <- stream %>% 
  filter(Event >=10 & Event <=12)

stream.2022.2<- stream %>% 
  filter(Event >=1 & Event <=6)%>% 
  filter(SITE == "1" | SITE == "15" )
stream.2023.2 <- stream %>% 
  filter(Event >= 7 & Event <=9)%>% 
  filter(SITE == "1" | SITE =="15")
stream.2024.2 <- stream %>% 
  filter(Event >=10 & Event <=12) %>% 
  filter(SITE == "1" | SITE =="15" )

lake <- all.iso %>% 
  filter(Setting.Type=="Lake"& year  ==2024)
rg <- all.iso %>% 
  filter(Setting.Type=="Rock Glacier")
ggplot()+
  geom_point(data=rg, aes(x=d18O, y =d2H, color = as.factor(Event)))

ggplot()+
  geom_point(data = iofl, aes(x =d18O, y =dxs, color = as.factor(year), shape = SITE, size  = 2))


ggplot()+
  geom_line(data = iofl, aes(x =d18O, y =d2H, color = as.factor(year), linetype = SITE))

ggplot()+
  geom_point(data = lo.gw, aes(x =d18O, y =d2H, color = as.factor(year), shape = Setting.Type))+
  geom_point(data = lo.gw, aes(x =d18O, y =d2H, color = as.factor(year), shape = Setting.Type))+
  geom_point(data = lo.gw, aes(x =d18O, y =d2H, color = as.factor(year), shape = Setting.Type))+
  geom_abline(intercept = 8, slope = 7.95)

ggplot()+
  geom_point(data = stream, aes(x =d18O, y =d2H, color = Setting.Type, shape = as.factor(month)))+
  geom_abline(intercept = 8, slope = 7.95)

ggplot()+
  geom_point(data = all.iso, aes(x =d18O, y =d2H, color = Setting.Type, shape = as.factor(year)))+
  geom_abline(intercept = 8, slope = 7.95)

ggplot()+
  #geom_point(data = stream.2022.2, aes(x =d18O, y =d2H, color = as.factor(month), shape = "2022"))+
  geom_point(data = stream.2023.2, aes(x =d18O, y =d2H, color = as.factor(month), shape = "2023"))+
  #geom_point(data = stream.2024.2, aes(x =d18O, y =d2H, color = as.factor(month), shape = "2024"))+
  geom_abline(intercept = 8, slope = 7.95)

ggplot()+
  geom_point(data = stream.2022.2, aes(x =d18O, y =d2H, color = "2022", shape = SITE))+
  geom_point(data = stream.2023.2, aes(x =d18O, y =d2H, color = "2023", shape = SITE))+
  geom_point(data = stream.2024.2, aes(x =d18O, y =d2H, color = "2024", shape = SITE))+
  geom_abline(intercept = 8, slope = 7.95)

p<- ggplot()+
  geom_point(data = stream.2022.2, aes(x = Event, y =d2H, shape = SITE, color = as.factor(month), size = 2))+
  geom_point(data = stream.2023.2, aes(x = Event, y =d2H, shape = SITE, color = as.factor(month), size = 2))+
  geom_point(data = stream.2024.2, aes(x = Event, y =d2H, shape = SITE, color = as.factor(month), size = 2))

o<- ggplot()+
  geom_point(data = stream.2022.2, aes(x = Event, y =d18O, shape = SITE, color = as.factor(month), size = 2))+
  geom_point(data = stream.2023.2, aes(x = Event, y =d18O, shape = SITE, color = as.factor(month), size = 2))+
  geom_point(data = stream.2024.2, aes(x = Event, y =d18O, shape = SITE, color = as.factor(month), size = 2))

i<-ggplot()+
  geom_point(data = stream.2022.2, aes(x = Event, y =dxs, shape = SITE, color = as.factor(month), size = 2))+
  geom_point(data = stream.2023.2, aes(x = Event, y =dxs, shape = SITE, color = as.factor(month), size = 2))+
  geom_point(data = stream.2024.2, aes(x = Event, y =dxs, shape = SITE, color = as.factor(month), size = 2))

library(patchwork)
p+o+i
write_csv(all.iso, "~/Documents/Data/Chapter.3/Isotope.Data/isotope.data")

riv <- all.iso %>% 
  filter(Setting.Type =="River"&Event >7|Setting.Type =="Lake.outlet" &Event >7)
out <- all.iso %>% 
  filter(Setting.Type=="Lake.outlet")
ggplot(data =riv,aes(y = d2H,x=d18O, color = Setting.Type)) +
  geom_point()+
  geom_smooth(method = "lm", formula = TRUE)+
  scale_color_viridis_d()


event7 <- all.iso %>% 
  filter(Event == 7 & Setting.Type == "Lake")

event8 <- all.iso %>% 
  filter(Event == 8 & Setting.Type == "Lake") %>% 
  group_by(SITE) %>% 
  summarise(d18O = mean(d18O),
            d2H = mean(d2H),
            dxs = mean(dxs))# %>% 
# distinct()

event9 <- all.iso %>% 
  filter(Event == 9 & Setting.Type == "Lake") %>% 
  group_by(SITE) %>% 
  summarise(d18O = mean(d18O),
            d2H = mean(d2H),
            dxs = mean(dxs))# %>% 
# distinct()
area(JL)
x$area_sqkm
expanse(JL)
# Calling Depth data#
#This is to test whether this will clean my variogram up#
JL.depth.202308 <- read_csv('~/Documents/Data/Jackson_Lake_Bathy/2023.08.23.depths.csv',show_col_types = FALSE)

#### Calling in the spatial information & setting up Jackson Lake Grid ####
JL_SP <- read_csv('~/Documents/Data/Lake_YSI/Coords.csv',show_col_types = FALSE) %>% #Calling the latitude and longitude of each sampling point
  filter(grepl('JL', SITE))
JL_SP10 <- read_csv('~/Documents/Data/Lake_YSI/Coord_10.csv',show_col_types = FALSE)  #Calling the latitude and longitude of each sampling point


JL <- read_sf(dsn = "~/Documents/Data/Jackson_Lake_Shapefiles/", layer = "Jack_Lake_Final") #Calling the Jackson Lake shapefile
JL2 <- vect("~/Documents/Data/Jackson_Lake_Shapefiles/", layer = "Jack_Lake_Final") #Calling the Jackson Lake shapefile
JL_SP <- st_as_sf(JL_SP, coords = c('Long', 'Lat'),crs = 4326) #taking the lat and long coordinates in dd and creating a geometric feature of the long lat
JL_SP10 <- st_as_sf(JL_SP10, coords = c('Long', 'Lat'),crs = 4326) #taking the lat and long coordinates in dd and creating a geometric feature of the long lat

JL_SP <- st_transform(JL_SP, crs = st_crs(JL)) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])

JL_SP10 <- st_transform(JL_SP10, crs = st_crs(JL)) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])

event8_sp <- merge(JL_SP, event8, by = "SITE")
event8_sp <- merge(event8_sp, JL.depth.202308, by = "SITE")

event9_sp <- merge(JL_SP, event9, by = "SITE")
event9_sp <- merge(event9_sp, JL.depth.202308, by = "SITE")


grid <- terra::rast(JL, nrows = 1000, ncols = 1000) #Creates a raster grid of 1000 X 1000  
# coordinates of all cells
xy <- terra::xyFromCell(grid, 1:ncell(grid)) #This is gives coordinates to each one of the points that have been created


coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"),
                 crs = st_crs(JL)) #this creates a dataframe with the points from the recently created grid giving the crs as JL
coop <- st_filter(coop, JL) #This essentially clips the grid that was created to the shapefile of Jackson Lake.

qtm(coop)
####Voronoi####
#Calling Isotope data for each sampling event, this will allow me to form 
#voronoi polygons around points sampled during each event
#Calling isotopic data to know what points I sampled during each event
e7 <- all.iso %>% 
  filter(Event == 7 & Setting.Type == "Lake") %>% 
  filter(SITE != "JL3.7" & SITE != "JL8.7") %>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()

e8 <- all.iso %>% 
  filter(Event == 8 & Setting.Type == "Lake")%>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()

e9 <- all.iso %>% 
  filter(Event == 9 & Setting.Type == "Lake")%>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()

e10 <- all.iso %>% 
  filter(Event == 10 & Setting.Type == "Lake")%>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()

e11 <- all.iso %>% 
  filter(Event == 11 & Setting.Type == "Lake")%>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()

e12 <- all.iso %>% 
  filter(Event == 12 & Setting.Type == "Lake")%>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()
#Merging points actual sampled points.
JLe7 <- JL_SP %>% 
  merge(e7)
JLe8 <- JL_SP %>% 
  merge(e8)
JLe9 <- JL_SP %>% 
  merge(e9)
JLe10 <- JL_SP10 %>% 
  merge(e10)
JLe11 <- JL_SP %>% 
  merge(e11)
JLe12 <- JL_SP %>% 
  merge(e12)
#Making voronoi polygons based on the sampling locations
v7 <- terra::voronoi(x = vect(JLe7), bnd = JL)
v8 <- terra::voronoi(x = vect(JLe8), bnd = JL)
v9 <- terra::voronoi(x = vect(JLe9), bnd = JL)
v10 <- terra::voronoi(x = vect(JLe10), bnd = JL)
v11 <- terra::voronoi(x = vect(JLe11), bnd = JL)
v12 <- terra::voronoi(x = vect(JLe12), bnd = JL)
#Cropping the polygons based on the jackson lake shapefile
cv7 <- crop(v7, JL2) 
cv8 <- crop(v8, JL2)
cv9 <- crop(v9, JL2)
cv10 <- crop(v10, JL2)
cv11 <- crop(v11, JL2)
cv12 <- crop(v12, JL2)
#calculating the area of each polygon
cv7$v.area.m2 <- expanse(cv7)
cv8$v.area.m2 <- expanse(cv8)
cv9$v.area.m2 <- expanse(cv9)
cv10$v.area.m2 <- expanse(cv10)
cv11$v.area.m2 <- expanse(cv11)
cv12$v.area.m2 <- expanse(cv12)
#creating dataframes of site name and area
v7.a <- data.frame(SITE = cv7[["SITE"]],
                   v.area.m2 = cv7[["v.area.m2"]],
                   Event = 7)
v8.a <- data.frame(SITE = cv8[["SITE"]],
                   v.area.m2 = cv8[["v.area.m2"]],
                   Event = 8)
v9.a <- data.frame(SITE = cv9[["SITE"]],
                   v.area.m2 = cv9[["v.area.m2"]],
                   Event = 9)
v10.a <- data.frame(SITE = cv10[["SITE"]],
                    v.area.m2 = cv10[["v.area.m2"]],
                    Event = 10)
v11.a <- data.frame(SITE = cv11[["SITE"]],
                    v.area.m2 = cv11[["v.area.m2"]],
                    Event = 11)
v12.a <- data.frame(SITE = cv12[["SITE"]],
                    v.area.m2 = cv12[["v.area.m2"]],
                    Event = 12)
#Writing to disk
write_csv(v7.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v7a")
write_csv(v8.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v8a")
write_csv(v9.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v9a")
write_csv(v10.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v10a")
write_csv(v11.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v11a")
write_csv(v12.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v12a")

####Testing different variable to test effects on isotopic information ####
library(units)
ggplot()+
  geom_point(data = event8_sp, aes(x = distance, y = dxs))



JL_line <- st_cast(JL, "LINESTRING")
plot(JL_line)
JL1 <- event8_sp[1, ]
event8_sp$distance <- st_distance(event8_sp, JL1) 

#### Functions for Inverse Distance Weighting and Nearest Neighbor. ####

#Inverse Distance Weighting:This interpolates values between points by considering all points that have been sampled.Sampling points are weighted with the weights inversely proporational to the distance between the unsampled and sampled locations.

idw.function <- function(variable, dataframe, idw.b){
  gstat(formula = variable ~ 1, locations = dataframe,
        # nmax = nrow(dataframe), # use all the neighbors locations
        nmax = 3,
        set = list(idp = idw.b))} # beta = 1, This is the idw function,using a variable in the formula with an intercept only model, locations are taken from the sp object JL_2023_1, nmax is the number of sites is uses, idp is the beta having weights of 1

pred.function.idw.h <- function(idw_res, pred.dataframe.points, pred.grid, idw.b, num.classes){
  resp <- predict(idw_res, pred.dataframe.points) #this is what creates the predictions on top of the grid that has been constructed
  resp$x <- st_coordinates(resp)[,1] #retrieves the x coordinates
  resp$y <- st_coordinates(resp)[,2] #retrieves the y coordinates
  resp$pred <- resp$var1.pred #creates a prediction column
  
  pred <- terra::rasterize(resp, pred.grid, field = "pred", fun = "mean") #Creating the raster prediction surface
  #plot.title <- paste("Inverse Distance Weighting, beta =", idw.b, sep = " ")
  plot.title <- (paste("September Interpolation 2023 (IDW)"))
  tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis", n = num.classes, title = expression(paste(delta^2, "H (\u2030)"))) + tm_shape(event9_sp)+
    tm_dots(col = "d2H", size = 0.2, palette = "viridis",legend.show = FALSE) +     
    tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 2)} #plotting the raster
#, legend.position = c("right", "top")
pred.function.idw.o <- function(idw_res, pred.dataframe.points, pred.grid, idw.b, num.classes){
  resp <- predict(idw_res, pred.dataframe.points) #this is what creates the predictions on top of the grid that has been constructed
  resp$x <- st_coordinates(resp)[,1] #retrieves the x coordinates
  resp$y <- st_coordinates(resp)[,2] #retrieves the y coordinates
  resp$pred <- resp$var1.pred #creates a prediction column
  
  pred <- terra::rasterize(resp, pred.grid, field = "pred", fun = "mean") #Creating the raster prediction surface
  #plot.title <- paste("Inverse Distance Weighting, beta =", idw.b, sep = " ")
  plot.title <- paste("September Interpolation 2023 (IDW)")
  tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis", n = num.classes, title = expression(paste(delta^18, "O (\u2030)"))) + tm_shape(event9_sp)+
    tm_dots(col = "d18O", size = 0.2, palette = "viridis",legend.show = FALSE) +     
    tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 2)} #plotting the raster
# legend.position = c("right", "top"),
pred.function.idw.dxs <- function(idw_res, pred.dataframe.points, pred.grid, idw.b, num.classes){
  resp <- predict(idw_res, pred.dataframe.points) #this is what creates the predictions on top of the grid that has been constructed
  resp$x <- st_coordinates(resp)[,1] #retrieves the x coordinates
  resp$y <- st_coordinates(resp)[,2] #retrieves the y coordinates
  resp$pred <- resp$var1.pred #creates a prediction column
  
  pred <- terra::rasterize(resp, pred.grid, field = "pred", fun = "mean") #Creating the raster prediction surface
  #plot.title <- paste("Inverse Distance Weighting, beta =", idw.b, sep = " ")
  plot.title <- paste("September Interpolation 2023 (IDW)")
  tm_shape(pred) + tm_raster(alpha = 0.6, palette = "-viridis", n = num.classes, title = "d-excess") + tm_shape(event9_sp)+
    tm_dots(col = "dxs", size = 0.2, palette = "-viridis",legend.show = FALSE) +     
    tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 1)} #plotting the raster

JL_idw_H.8 <- idw.function(event8_sp$d2H, event8_sp, 8) 
JL_idw.H.8 <- pred.function.idw.h(JL_idw_H.8, coop, grid,1, 10)
JL_idw.H.8

JL_idw_O.8 <- idw.function(event8_sp$d18O, event8_sp, 8)
JL_idw.O.8 <- pred.function.idw.o(JL_idw_O.8, coop, grid,1, 10)
JL_idw.O.8

JL_idw_dxs.8 <- idw.function(event8_sp$dxs, event8_sp, 8)
JL_idw.dxs.8 <- pred.function.idw.dxs(JL_idw_dxs.8, coop, grid,1, 10)
JL_idw.dxs.8

JL_idw_H.9 <- idw.function(event9_sp$d2H, event9_sp, 2) 
JL_idw.H.9 <- pred.function.idw.h(JL_idw_H.9, coop, grid,2, 6)
JL_idw.H.9

JL_idw_O.9 <- idw.function(event9_sp$d18O, event9_sp, 2)
JL_idw.O.9 <- pred.function.idw.o(JL_idw_O.9, coop, grid,2, 5)
JL_idw.O.9

JL_idw_dxs.9 <- idw.function(event9_sp$dxs, event9_sp, 2)
JL_idw.dxs.9 <- pred.function.idw.dxs(JL_idw_dxs.9, coop, grid,2, 8)
JL_idw.dxs.9


####Nearest Neighbor #####
res <- gstat(formula = d18O ~ 1, locations = event9_sp, nmax = 55,
             set = list(idp = 0))

resp <- predict(res, coop)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

plot.title <- paste("September Interpolation 2023 (NN)")

pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis", n = 7, title = expression(paste(delta^18, "O (\u2030)")))+ tm_shape(event9_sp)+
  tm_dots(col = "d18O", size = 0.2, palette = "viridis",legend.show = FALSE) +     
  tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 1) #plotting the raster


#### Variogram ####
## Constructing a variogram for dxs from Sampling event 8 ##

hist(event8_sp$dxs)
dxsvc <- variogram((dxs) ~ 1, event8_sp, cloud = TRUE)
plot(dxsvc) #This is a variogram cloud.

dxsv <- variogram((dxs) ~ 1, data = event8_sp)
plot(dxsv) #This is a sample variogram

dxsvinitial <- vgm(psill = 0.6, model = "Wav",
                   range = 2000, nugget = 0.0)
plot(dxsv, dxsvinitial, cutoff = 1000, cex = 1.5)

dxsfv <- fit.variogram(object = dxsv,
                       model = vgm(psill =0.6, model = "Gau",
                                   range = 2000, nugget = 0.0001))
dxsfv
plot(dxsv, dxsfv, cex = 1.5)

####
dxsv <- variogram((dxs) ~ 1, data = event9_sp, alpha = c(0, 45, + 90, 135))
plot(dxsv) #This is a sample variogram

dxsv <- variogram((dxs) ~ 1, data = event8_sp)
plot(dxsv) #This is a sample variogram


dxsvinitial <- vgm(psill = 1, model = "Nug", nugget = 0.2)
plot(dxsv, dxsvinitial, cutoff = 1000, cex = 1.5)

dxsfv <- fit.variogram(object = dxsv,
                       model = vgm("Nug"))
dxsfv
plot(dxsv, dxsfv, cex = 1.5)



#### Kriging Interpolation plots ####
## dxs interpolation ##
k <- gstat(formula = dxs ~ 1, data = event8_sp, model = dxsfv)

kpred <- predict(k, coop)
kpred$dxs <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "dxs", direction = -1) + theme_bw() +
  ggtitle("August Interpolation (Kriging)")+ guides(color=guide_colorbar(title='d-excess')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance", direction = -1) + theme_bw()

## Constructing a variogram for d18O from Sampling event 8 ##
hist(event8_sp$d18O)
Ovc <- variogram((d18O) ~ 1, event8_sp, cloud = TRUE)
plot(Ovc) #This is a variogram cloud.

Ov <- variogram(d18O ~ 1, data = event8_sp)
plot(Ov) #This is a sample variogram

Ovinitial <- vgm(psill = 0.015, model = "Sph",
                 range = 3000, nugget = 0.005)
plot(Ov, Ovinitial, cutoff = 1000, cex = 1.5)


Ofv <- fit.variogram(object = Ov,
                     model = vgm(psill = 0.015, model = "Sph",
                                 range = 3000, nugget = 0.005))
Ofv
plot(Ov, Ofv, cex = 1.5)


## d18O interpolation ##
k <- gstat(formula = d18O ~ 1, data = event8_sp, model = Ofv)

kpred <- predict(k, coop)
kpred$d18O <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "d180") + theme_bw()+
  ggtitle("August Interpolation (Kriging)")+ guides(color=guide_colorbar(title=expression(paste(delta^18, "O (\u2030)")))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()

## Constructing a variogram for d2h from Sampling event 8 ##
hist(event8_sp$d2H)
Hvc <- variogram((d2H) ~ 1, event8_sp, cloud = TRUE)
plot(Hvc) #This is a variogram cloud.

Hv <- variogram(d2H ~ 1, data = event8_sp)
plot(Hv) #This is a sample variogram

Hvinitial <- vgm(psill = 0.15, model = "Exp",
                 range = 2500, nugget = 0.0)
plot(Hv, Hvinitial, cutoff = 1000, cex = 1.5)


Hfv <- fit.variogram(object = Hv,
                     model = vgm(psill = .2, model = "Exp",
                                 range = 4000, nugget = 0.0))
Hfv
plot(Hv, Hfv, cex = 1.5)


## d2h interpolation ##
k <- gstat(formula = d2H ~ 1, data = event8_sp, model = Hfv)

kpred <- predict(k, coop)
kpred$d2H <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "d2H") + theme_bw()+
  ggtitle("August Interpolation (Kriging)")+ guides(color=guide_colorbar(title=expression(paste(delta^2, "H (\u2030)")))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()

## Constructing a variogram for dxs from Sampling event 9 ##
hist(event9_sp$dxs)
dxsvc <- variogram((dxs) ~ 1, event9_sp, cloud = TRUE)
plot(dxsvc) #This is a variogram cloud.

dxsv <- variogram(dxs ~ 1, data = event9_sp)
plot(dxsv) #This is a sample variogram

dxsv1 <- variogram((dxs) ~ 1, data = event9_sp)
plot(dxsv1)

dxsvinitial <- vgm(psill = 0.25, model = "Nug",
                   range = 2000, nugget = 0.25)
plot(dxsv, dxsvinitial, cutoff = 1000, cex = 1.5)


dxsfv <- fit.variogram(object = dxsv,
                       model = vgm(psill = 0.25, model = "Wav",
                                   range = 2500, nugget = 0.25))
dxsfv
plot(dxsv, dxsfv, cex = 1.5)

## Constructing a variogram for d18O from Sampling event 8 ##
hist(event9_sp$d18O)
Ovc <- variogram((d18O) ~ 1, event9_sp, cloud = TRUE)
plot(Ovc) #This is a variogram cloud.

Ov <- variogram(d18O ~ 1, data = event9_sp)
plot(Ov) #This is a sample variogram

Ovinitial <- vgm(psill = 0.01, model = "Sph",
                 range = 2000, nugget = 0.00)
plot(Ov, Ovinitial, cutoff = 1000, cex = 1.5)


Ofv <- fit.variogram(object = Ov,
                     model = vgm(psill = 0.01, model = "Sph",
                                 range = 2000, nugget = 0.))
Ofv
plot(Ov, Ofv, cex = 1.5)

#### Kriging Interpolation plots ####
## dxs interpolation ##
k <- gstat(formula = dxs ~ 1, data = event9_sp, model = dxsfv)

kpred <- predict(k, coop)
kpred$dxs <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "dxs") + theme_bw()

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()

## d18O interpolation ##
k <- gstat(formula = d18O ~ 1, data = event8_sp, model = Ofv)

kpred <- predict(k, coop)
kpred$d18O <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "dxs") + theme_bw()

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()
#### Dual Isotope Plots ####

ggplot() +
  geom_point(data = run1, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run2, aes(x = d18O, y = d2H, col = Setting.Type)) +      
  geom_point(data = run3, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run4, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run5, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run6, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run7, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run8, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run9, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run10, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run11, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run12, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run13, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run14, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_abline(aes(intercept = 8.09, slope = 7.95, col = "LMWL")) + #LMWL from Benjamin (2005)
  geom_abline(aes(intercept = 3.12, slope = 7.94, col = "Idaho Falls LMWL")) #From the 1976-1977 Idaho Fall LMWL

ggplot() +
  geom_point(data = all.iso, aes(x = d18O, y = d2H, col = as.factor(Event))) + 
  geom_abline(aes(intercept = 8.09, slope = 7.95)) + #LMWL from Benjamin (2005)
  geom_abline(aes(intercept = 3.12, slope = 7.94)) #From the 1976-1977 Idaho Fall LMWL


ggplot() +
  geom_point(data = run1, aes(x = d18O, y = d2H, col = "Run 1")) +
  geom_point(data = run2, aes(x = d18O, y = d2H, col = "Run 2")) +      
  geom_point(data = run3, aes(x = d18O, y = d2H, col = "Run 3")) +
  geom_point(data = run4, aes(x = d18O, y = d2H, col = "Run 4")) +
  geom_point(data = run5, aes(x = d18O, y = d2H, col = "Run 5")) +
  geom_point(data = run6, aes(x = d18O, y = d2H, col = "Run 6")) +
  geom_point(data = run7, aes(x = d18O, y = d2H, col = "Run 7")) +
  geom_point(data = run8, aes(x = d18O, y = d2H, col = "Run 8")) +
  geom_point(data = run9, aes(x = d18O, y = d2H, col = "Run 9")) +
  geom_point(data = run10, aes(x = d18O, y = d2H, col = "Run 10")) +
  geom_point(data = run11, aes(x = d18O, y = d2H, col = "Run 11")) +
  geom_point(data = run12, aes(x = d18O, y = d2H, col = "Run 12")) +
  geom_point(data = run13, aes(x = d18O, y = d2H, col = "Run 13")) +
  geom_point(data = run14, aes(x = d18O, y = d2H, col = "Run 14")) +
  #scale_color_viridis_d()+
  geom_abline(aes(intercept = 8.09, slope = 7.95)) + #LMWL from Benjamin (2005)
  geom_abline(aes(intercept = 3.12, slope = 7.94)) 


ggplot() +
  geom_boxplot(data = all.iso, aes(x = SITE, y = dxs))+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




res <- gstat(formula = event8_sp$dxs ~ 1, locations = event8_sp, nmax = 5,
             set = list(idp = 0))

resp <- predict(res, coop)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis")


####Lake Interpolation####
JL_YSI_sur <- read_csv('~/Documents/Data/Lake_YSI/2023_YSI.csv',show_col_types = FALSE) %>%  
  mutate(DATE = as.Date(DATE, "%m/%d/%Y")) %>% 
  filter(sample.event.normal.scheme == 7 |sample.event.normal.scheme == 8|sample.event.normal.scheme == 9| depth.m.2 == 0.5)

ysi.7 <- JL_YSI_sur %>% 
  filter(sample.event.normal.scheme == 7 & depth.m.2 == 0.5)
ysi.8 <- JL_YSI_sur %>% 
  filter(sample.event.normal.scheme == 8 & depth.m.2 == 0.5)
ysi.9 <- JL_YSI_sur %>% 
  filter(sample.event.normal.scheme == 9 & depth.m.2 == 0.5)

####Kriging####
hist(event9_sp$d18O)
Ovc <- variogram((d18O) ~ 1, event9_sp, cloud = TRUE)
plot(Ovc) #This is a variogram cloud.

Ov <- variogram(d18O ~ 1, data = event9_sp)
plot(Ov) #This is a sample variogram

Ovinitial <- vgm(psill = 0.015, model = "Sph",
                 range = 3000, nugget = 0.005)
plot(Ov, Ovinitial, cutoff = 1000, cex = 1.5)


Ofv <- fit.variogram(object = Ov,
                     model = vgm(psill = 0.015, model = "Sph",
                                 range = 3000, nugget = 0.005))
Ofv
plot(Ov, Ofv, cex = 1.5)


## d18O interpolation ##
k <- gstat(formula = d18O ~ 1, data = event9_sp, model = Ofv)

kpred <- predict(k, coop)
kpred$d18O <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "d180") + theme_bw()+
  ggtitle("July Interpolation (Kriging)")+ guides(color=guide_colorbar(title=expression(paste(delta^18, "O (\u2030)")))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()


