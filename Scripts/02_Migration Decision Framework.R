

getwd()

#read xlsx - specify path, sheet name to read
df <- read_xlsx("Data/20210921_Migration Decision Framework.xlsx",
                        sheet="Tidy data 2", col_types = "text") %>% 
      janitor::clean_names()

glimpse(df)

df_countryreference <- df %>% 
  #country names should be 
  separate(origin_destination, c("A","B", "C","D","E", "F", "G", "H"), sep = "([+;,()])") %>% 
   # separate(examples_references, c("G", "H","I", "J", "K"), sep = "([;])") %>% 
  select(!c(starts_with("x")))  

view(df_countryreference)



#pivot long for countries

# lake victoria = uganda tanzania kenya
# Lake V Basin countries=	Burundi, Kenya, Rwanda, Tanzania, and Uganda[1]

df_long_country <- df_countryreference %>% 
  pivot_longer(
    cols = A:H,  
    values_to = "country") %>% 
#remove spaces from before/after country
   mutate(country=str_trim(country, side = "both") ,
  #those that are empty because of the pivot can be dropped
   drop=ifelse(name!="A" & is.na(country), 1, NA), 
   region1=ifelse(country=="West Africa", "Benin", NA),
  region2=ifelse(country=="West Africa", "Cote d'Ivoire", NA),
  region3=ifelse(country=="West Africa", "Ghana", NA),
  region4=ifelse(country=="West Africa", "Nigeria", NA),
  region5=ifelse(country=="West Africa", "The Gambia", NA),
  region6=ifelse(country=="West Africa", "Togo", NA),
  region7=ifelse(country=="East Africa", "Kenya", NA),
  region8=ifelse(country=="East Africa", "Tanzania", NA),
  region9=ifelse(country=="East Africa", "Uganda", NA),
  region10=ifelse(country=="Southern Africa", "Angola", NA),
  region11=ifelse(country=="Southern Africa", "Botswana", NA),
  region12=ifelse(country=="Southern Africa", "The Democratic Republic of Congo", NA), ###################### check name
  region13=ifelse(country=="Southern Africa", "Lesotho", NA),
  region14=ifelse(country=="Southern Africa", "Malawi", NA),
  region15=ifelse(country=="Southern Africa", "Mozambique", NA),
  region16=ifelse(country=="Southern Africa", "Namibia", NA),
  region17=ifelse(country=="Southern Africa", "South Africa", NA),
  region18=ifelse(country=="Southern Africa", "Eswatini", NA),
  region19=ifelse(country=="Southern Africa", "Zambia", NA),
  region20=ifelse(country=="Southern Africa", "Zimbabwe", NA))  %>% 
  select(!c(name)) %>%
  filter(is.na(drop))
view(df_long_country)

#beware of duplicates! these are sometimes written out and sometimes not




df_long_region<- df_long_country %>%
  pivot_longer(
    cols = region1:region20,
    values_to = "country2",
    names_to= "names2")%>%
  mutate(
    #create separate region variable for regions
    region=ifelse(country %in% c("West Africa"), "West Africa", "Country Specific"),
    region=ifelse(country %in% c("East Africa"), "East Africa", region),
    region=ifelse(country %in% c("Southern Africa"), "Southern Africa", region),
    #rename country var from the region countries
    country=ifelse(country %in% c("West Africa", "East Africa", "Southern Africa"), country2, country)) %>%
 #drop extra variables used to pivot
   select(!c(names2, country2)) 
  view(df_long_region)



 # df_long_region %>%
 #    distinct(region) %>%
 #  pull()





df_migrationstage <- df_long_region %>% 
  separate(migration_stage_relevance, c("A","B", "C","D"), sep = "([,])") %>% 
view(df_migrationstage)



df_long_migrationstage <- df_migrationstage %>% 
  pivot_longer(
    cols = A:D,  
    values_to = "migration_stage_relevance") %>% 
  #remove spaces from before/after country
  mutate(migration_stage_relevance=str_trim(migration_stage_relevance, side = "both"),
         country=str_trim(country, side = "both"),
         drop=ifelse(name!="A" & is.na(migration_stage_relevance), 1, NA),
         country=ifelse(is.na(country), "No Country Data", country)) %>% 
  filter(is.na(drop)) %>% 
  select(!c(drop, name, who, where, when)) 
view(df_long_migrationstage)

# test<- df_long_migrationstage %>%
#   filter(drop==1) %>%
#   view()




df_long_migrationstage %>%
   distinct(migration_pattern) %>%
 pull()
df_long_migrationstage %>%
  distinct(population) %>%
  pull()
nrow(df_long_migrationstage)

# write_tsv(df_long_migrationstage, "DataOut/Migration Decision Framework_20210921_6", na = "NA")
write_excel_csv(df_long_migrationstage, "DataOut/Migration Decision Framework_20210921_6.csv", na = "")
# 
# 
# write_tsv(df_long_migrationstage, "DataOut/Migration Decision Framework_20210921_test", na = " ")
# write_csv(df_long_migrationstage, "DataOut/Migration Decision Framework_20210921", na = " ")
# library(xlsx)
# install.packages("writexl")
# 
# write.xlsx(mydata, "c:/mydata.xlsx")