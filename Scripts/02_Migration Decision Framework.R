# Steps in excel
# removed 3 leading rows
# moved the occurances of references in origin/destination examples to references ~15 rows impacted


getwd()

#read xlsx - specify path, sheet name to read
df <- read_xlsx("Data/20210910_Migration Decision Framework.xlsx",
                        sheet="Tidy data", col_types = "text") %>% 
      janitor::clean_names()

glimpse(df)

df_countryreference <- df %>% 
  #country names should be 
  separate(origin_destination_examples, c("A","B", "C","D","E"), sep = "([+;,()])") %>% 
   separate(examples_references, c("G", "H","I", "J", "K"), sep = "([;])") %>% 
  select(!c(starts_with("x")))  

view(df_countryreference)



#pivot long for countries

# lake victoria = uganda tanzania kenya
# Lake V Basin countries=	Burundi, Kenya, Rwanda, Tanzania, and Uganda[1]

df_long_country <- df_countryreference %>% 
  pivot_longer(
    cols = A:E,  
    values_to = "country") %>% 
#remove spaces from before/after country
   mutate(country=str_trim(country, side = "both") ,
  #those that are empty because of the pivot can be dropped
   drop=ifelse(name!="A" & is.na(country), 1, NA) ,
   # #those that listed "Lake Victoria" also listed relevant countries in parenthesis and can be dropped
   drop=ifelse(country=="Lake Victoria", 1, NA),
  #lake victoria basin countries not identified, should be listed
   basin1=ifelse(country=="Lake Victoria Basin", "Uganda", NA), 
   basin2=ifelse(country=="Lake Victoria Basin", "Tanzania", NA), 
   basin3=ifelse(country=="Lake Victoria Basin", "Rwanda", NA), 
   basin4=ifelse(country=="Lake Victoria Basin", "Kenya", NA), 
   basin5=ifelse(country=="Lake Victoria Basin", "Burundi", NA))
view(df_long_country)

#beware of duplicates! these are sometimes written out and sometimes not

df_long_basin <- df_long_country %>% 
  pivot_longer(
    cols = basin1:basin5,  
    values_to = "country2",
    names_to= "names2") %>%
  mutate(
    #create separate region variable for lake victoria/basin
    region=ifelse(country %in% c("Lake Victoria Basin"), "Lake Victoria Basin", "Other"), 
    region=ifelse((country %in% c("Uganda", "Tanzania", "Kenya") & region!="Lake Victoria Basin"), "Lake Victoria", region),
    #rename country var from the basin countries
    country=ifelse(country=="Lake Victoria Basin", country2, country)) %>% 
 #drop extra variables used to pivot
   select(!c(name, names2, country2)) %>% 
  filter(is.na(drop))
  view(df_long_basin)


 # test<- df_long_basin %>%
 #   filter(!is.na(country2)) %>% 
 #   view()
 
# df_long_basin %>%
#     distinct(region) %>%
#   pull()

#pivot long for references
df_long_ref <- df_long_basin %>% 
  pivot_longer(
    cols = G:K,  
    values_to = "references") %>% 
  #remove spaces from before/after country
  mutate(references=str_trim(references, side = "both"),
  # filter(name!="A" & is.na(country))
          drop=ifelse(name!="G" & is.na(references), 1, NA) , 
  drop=ifelse(country=="", 1, NA)) %>% 
  filter(is.na(drop)) %>% 
   select(!c(drop, name)) 

view(df_long_ref)



  
# test<- df_long_ref %>%
#   filter(country=="") %>%
#   view()
#   #these missing are due to the column that was created when doing text to column separate by "(" and can be deleted
# 
# df_long_ref %>%
#   distinct(migration_stage_relevance) %>%
#   pull()



df_migrationstage <- df_long_ref %>% 
  separate(migration_stage_relevance, c("A","B", "C","D"), sep = "([,])")
view(df_migrationstage)



df_long_migrationstage <- df_migrationstage %>% 
  pivot_longer(
    cols = A:D,  
    values_to = "migration_stage_relevance") %>% 
  #remove spaces from before/after country
  mutate(migration_stage_relevance=str_trim(migration_stage_relevance, side = "both"),
         drop=ifelse(name!="A" & is.na(migration_stage_relevance), 1, NA)) %>% 
  filter(is.na(drop)) %>% 
  select(!c(drop, name)) 
view(df_long_migrationstage)

# test<- df_long_migrationstage %>%
#   filter(drop==1) %>%
#   view()







write_tsv(df_long_migrationstage, "DataOut/Migration Decision Framework_20210910", na = " ")
