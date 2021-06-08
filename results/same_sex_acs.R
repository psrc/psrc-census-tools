library(tidyverse)
library(sf)
library(leaflet)
library(tidycensus)
library(writexl)
library(htmlwidgets)
source('C:/Users/SChildress/Documents/GitHub/psrc-census-tools/library/psrc_census_config.R')
source('C:/Users/SChildress/Documents/GitHub/psrc-census-tools/library/psrc_census.R')

# See: https://www.census.gov/content/dam/Census/library/publications/2021/acs/acsbr-005.pdf



Sys.getenv("CENSUS_API_KEY")

# make some tables by region and county
region_same_sex_married<-psrc_acs_table("B09019_011", "region", 2016,'acs5')
region_same_sex_cohabit<-psrc_acs_table("B09019_013", "region", 2016,'acs5')


county_same_sex_married<-psrc_acs_table("B09019_011", "county", 2019,'acs5')
county_same_sex_cohabit<-psrc_acs_table("B09019_013", "county", 2019,'acs5')

write.csv(region_same_sex_married, "results/region_same_sex_married.csv", sep="\t", row.names=FALSE)
write.csv(region_same_sex_cohabit, "results/region_same_sex_cohabit.csv", sep="\t", row.names=FALSE)

write.csv(county_same_sex_married, "results/county_same_sex_married.csv", sep="\t", row.names=FALSE)
write.csv(county_same_sex_cohabit, "results/county_same_sex_cohabit.csv", sep="\t", row.names=FALSE)

#map the tracts
tract_same_sex_married<-psrc_acs_table("B09019_011", "tract", 2019,'acs5')
tract_same_sex_cohabit<-psrc_acs_table("B09019_013", "tract", 2019,'acs5')


ss_married_map<-create_tract_map(tract_same_sex_married)
ss_cohabit_map<-create_tract_map(tract_same_sex_cohabit)

saveWidget(ss_married_map, file='results/same_sex_married_map.html')
saveWidget(ss_cohabit_map, file='results/same_sex_cohabit_map.html')