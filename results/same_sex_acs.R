library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)
library(writexl)
source('C:/Users/SChildress/Documents/GitHub/psrc-census-tools/library/psrc_census_config.R')
source('C:/Users/SChildress/Documents/GitHub/psrc-census-tools/library/psrc_census.R')


Sys.getenv("CENSUS_API_KEY")

tract_same_sex_married<-psrc_acs_table("B09019_011", "tract", 2019,'acs5')
tract_same_sex_cohabit<-psrc_acs_table("B09019_013", "tract", 2019,'acs5')
#county_ferry<-psrc_acs_table("B08006_013", "county", 2019, 'acs1')
#region_female_under_5<-psrc_acs_table("B01001_027", "region", 2019, 'acs1')

#if you want you can write the data to the clipboard or out to csv or excel
#write.table(county_ferry, "clipboard", sep="\t", row.names=FALSE)
#write.csv(county_ferry, "ferry_workers_by_county.csv")
#write_xlsx(county_ferry, "ferry_workers_by_county.xlsx")
region_same_sex_married<-psrc_acs_table("B09019_011", "region", 2019,'acs5')
region_same_sex_cohabit<-psrc_acs_table("B09019_013", "region", 2019,'acs5')

write.table(region_same_sex_married, "clipboard", sep="\t", row.names=FALSE)
write.table(region_same_sex_cohabit, "clipboard", sep="\t", row.names=FALSE)

county_same_sex_married<-psrc_acs_table("B09019_011", "county", 2019,'acs5')
county_same_sex_cohabit<-psrc_acs_table("B09019_013", "county", 2019,'acs5')


create_tract_map(tract_same_sex_married)
create_tract_map(tract_same_sex_cohabit)