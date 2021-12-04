########################################################################################
# Aim: Can PEPFAR Narratives be used to visualize issues related to PEPFAR programs?
# Program area: Viral Load Program indicators (TX_CURR, TX_PVLS)
# Code by: Jessica Stephens for the ICPI Viral Load/EID Sub-Cluster

# This is a final script for the quarter identified in the branch title 
# As presented in VL/EID Qtrly Data Summary for the VL/EID CoOP
########################################################################################

#load libraries - if 1st time running script, review setup to install necessary packages
source("Scripts/00_Setup.R")

####################################################################################
# Narratives excel format require basic munging into Tidy Data
####################################################################################


#check location of project vs data
#getwd()
#if the data is in a different location than the project, set working directory
setwd("C:/Users/jStephens/Documents/ICPI/Narrative Analysis")

#read in excel data set of narratives (these narratives have been read and assigned sentiments)
df <- read_xlsx("Data/NarrativeAnalysis_FY21Q4_TX_CURR_30Nov2021_NM.xlsx", sheet="Sheet1",
               col_types = "text") %>% 
  janitor::clean_names()

  glimpse(df)
  names(df)

df_PVLS <-read_xlsx("Data/NarrativeAnalysis_FY21Q4_TX_PVLS_30Nov2021_ayesha_khalda.xlsx", sheet="Sheet1",
               col_types = "text") %>% 
  janitor::clean_names()

  glimpse(df_PVLS)
  names(df_PVLS)

#combine TX_CURR and TX_PVLS narratives into 1 dataset
df2<-bind_rows(df, df_PVLS) %>% 
  rename(partner=mech_name)

  names(df2)

#pivot key terms from wide to long
  #if there are any differences in coding values, may start cleaning here
df_long <- df2 %>% 
  pivot_longer(
    cols =  covid:tx_pvls_covid,  
    values_to = "val") %>% 
  mutate(val = case_when(val=="I"~"i", 
                       val=="N"~"n", 
                       TRUE~val))

#review pivot
  glimpse(df_long)
  names(df_long)

#pivot values from long to wide (issues and non-issues with own columns)
    #values for issues and non-issues changed to numeric "1" flag to be quantified
df_wider <- df_long %>% 
  pivot_wider(
    names_from = val, 
    values_from = val  ) %>% 
  select(!c( "NA")) %>% 
  mutate(n=case_when( n=="n"~"1", TRUE~n),
    i= case_when(
      i=="i"~"1", TRUE~i), 
    n=as.numeric(n), 
    i=as.numeric(i)) %>% 
  rename(no_issue=n, issue=i)

#review pivot
  glimpse(df_wider)
  names(df_wider)

#final pivot long for tidy data set with issue/non-issue as "category" of sentiment
df_long_cat <- df_wider %>% 
  pivot_longer(
    cols= no_issue:issue,  
    names_to="category",
    values_to="val")

####################################################################
# Visuals require different level of aggregation
# Data munged/aggregated below according to analytic questions
# Aggregation started at lowest (name aka key term) to highest (OU)
####################################################################

##########################################################
# Aggregate by name (key term)
# EX. Display TX_PVLS & TX_CURR narratives by key terms
##########################################################



#Collapse to 1 row by indicator, operating unit, partner & name 
agg_name_long<-df_long_cat %>% 
  select(indicator, operating_unit, partner,name, category, val) %>% 
  group_by(indicator,operating_unit, partner,name, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup() 
  
#Then give value of 1 if that group has reported at least 1 issue/non-issue
agg_name_un<-agg_name_long %>% 
  mutate(val=case_when(
    val>=1~1, TRUE~val))

#used in visuals: z1/z2/standalone visuals

##########################################################
# Aggregate by partner
# EX. Display TX_PVLS & TX_CURR narratives by partner
##########################################################
#Collapse to 1 row by indicator, operating unit & partner
agg_partner_long<-df_long_cat %>% 
  select(indicator, operating_unit, partner, category, val) %>% 
  group_by(indicator,operating_unit, partner, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup()
  
#Then give value of 1 if that group has reported at least 1 issue/non-issue
agg_partner_un<-agg_partner_long %>% 
  mutate(val=case_when(
    val>=1~1, TRUE~val))

#Then drop partner name to collapse for OU level view
agg_partner_un_collapse<-agg_partner_un %>% 
  group_by(indicator, operating_unit, category) %>% 
  summarise(val = sum(val)) %>% 
  ungroup()

#used in visuals: y1/y2

##########################################################
# Aggregate by OU
# EX. Display TX_PVLS & TX_CURR narratives by OU (total) 
#   & by number of partners within OU
##########################################################

#Collapse to 1 row by indicator, operating unit, & sentence
agg_sentence_long<-df_long_cat %>% 
  select(indicator, operating_unit, sentences, val) %>% 
  group_by(indicator,operating_unit, sentences ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup() 
agg_sentence_un<-agg_sentence_long %>% 
  mutate(val=case_when(
    val>=0~1, TRUE~val))
agg_sentence_un_count<-agg_sentence_un %>% 
  select(indicator, operating_unit, val) %>% 
  group_by(indicator,operating_unit ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup() 

#this double counts each time it is used in each different key term
# #total number of occurrences per OU
# agg_ou_cat<-df_long_cat %>% 
#   select(indicator, operating_unit, partner, category, val) %>% 
#   group_by(indicator,operating_unit, category ) %>% 
#   summarize_at(vars(val), sum, na.rm=TRUE) %>% 
#   ungroup()
#   
# #unique partner mentions per OU
# agg_ou_partner_un<-agg_partner_un %>% 
#   select(indicator, operating_unit, category, val) %>% 
#   group_by(indicator,operating_unit, category ) %>% 
#   summarize_at(vars(val), sum, na.rm=TRUE) %>% 
#   ungroup()

#used in visuals: OU_c1/OU_c2/OU_1/OU_2
  
##################################################################################
########################        Visualizations           #########################
##################################################################################

##########################################
# Set up Colors 
##########################################
# TX_PVLS colors
si_rampr("siei") %>% show_col()

si_rampr("moody_blues") %>% show_col()
# 2F2E6F - DARK PURPLE
# CFC3FF - light purple
# 7069B2 - middle


# TX_CURR Colors
si_rampr("burnt_siennas") %>% show_col()
# 923417 - DARK BROWN/ORANGE
# FFB790 - light orange
# BF5A39 - middle color

######################################
#       VISUALS - OU LEVEL
######################################

#context visuals
#which OUs are submitting the most narratives using these key terms (deduplicated)
OU_c1_dd<-
  agg_sentence_un_count %>%
  filter(indicator %in% c("TX_PVLS")) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, val)))+
  geom_col(aes( x=val), fill="#7069B2", alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

OU_c2_dd<-
  agg_sentence_un_count %>%
  filter(indicator %in% c("TX_CURR")) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, val)))+
  geom_col(aes( x=val), fill="#BF5A39", alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(OU_c1_dd + OU_c2_dd) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q4: TX_PVLS AND TX_CURR NARRATIVES WITH KEY TERMS BY OU (DEDUPLICATED)",
    subtitle = "OUs submitting the most narratives capturing the key terms by indicator are Kenya (for both <b style='color:#7069B2'>TX_PVLS</b> and <b style='color:#BF5A39'>TX_CURR</b>)",
    caption = "Source: FY21 Q4 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 



#which OUs are submitting the most narratives using these key terms (total)

OU_c1<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_PVLS")) %>%
  ggplot(aes(y=fct_reorder(operating_unit, val)))+
  geom_col(aes( x=val), fill="#7069B2", alpha=.8)+
  si_style_xgrid()+
  facet_wrap(~indicator)+
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

OU_c2<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_CURR")) %>%
  ggplot(aes(y=fct_reorder(operating_unit, val)))+
  geom_col(aes( x=val), fill="#BF5A39", alpha=.8)+
  si_style_xgrid()+
  facet_wrap(~indicator)+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(OU_c1 + OU_c2) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q4: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY OU (TOTAL OCCURRENCES)",
    subtitle = "OUs submitting the most narratives capturing the key terms by indicator are Kenya (<b style='color:#7069B2'>TX_PVLS</b>) and Asia Region (<b style='color:#BF5A39'>TX_CURR</b>)",
    caption = "Source: FY21 Q4 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))




###############################################
#OU by issues/non issue (total counts still)
#which OUs are submitting the most narratives using these key terms (total), ordered by issue

OU_1<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_PVLS")) %>% 
   mutate(ou_order = ifelse((category == "issue"&indicator=="TX_PVLS"), val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#CFC3FF', '#2F2E6F'))+ 
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


OU_2<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_CURR")) %>% 
  mutate(ou_order = ifelse((category == "issue"&indicator=="TX_CURR"), val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha = .8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#FFB790', '#923417'))+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")



(OU_1 + OU_2) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q4: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY OU, ISSUE OR NO ISSUE",
   subtitle = "Kenya described most <b style='color:#2F2E6F'>TX_PVLS issues</b>, while South Africa described the most <b style='color:#923417'>TX_CURR issues</b>",
     caption = "Source: FY21 Q4 Narratives",
     theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 

# following not included in final visuals, but may be of interest to different stakeholders
# ###############################################
# #OU by issues/non issue (total counts still)
# #ordered by value (issue + non-issue)
# 
# OU_3<-
#   agg_ou_cat %>%
#   filter(indicator %in% c("TX_PVLS")) %>% 
#   ggplot(aes(y=fct_reorder(operating_unit, val)))+
#   geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
#   si_style_xgrid()+
#   facet_wrap(~indicator)+
#   scale_fill_manual(values = c('#CFC3FF', '#2F2E6F'))+ 
#   labs(x = NULL, y = NULL) +
#   theme(legend.position = "none")
# 
# OU_4<-
#   agg_ou_cat %>%
#   filter(indicator %in% c("TX_CURR")) %>% 
#   ggplot(aes(y=fct_reorder(operating_unit, val)))+
#   geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
#   si_style_xgrid()+
#   facet_wrap(~indicator)+
#   scale_fill_manual(values = c('#FFB790', '#923417'))+ 
#   labs(x = NULL, y = NULL)+
#   theme(legend.position = "none")
# 
# 
# (OU_3 + OU_4) + plot_layout(widths  = c(1, 1))+
#   plot_annotation(
#     title = "FY21 Q3: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY OU",
#     subtitle = "Kenya described most <b style='color:#2F2E6F'>TX_PVLS issues</b> and <b style='color:#CFC3FF'>TX_PVLS non-issues</b>, while Asia Region described the most <b style='color:#FFB790'>TX_CURR</b> but a smaller portion of <b style='color:#923417'>TX_CURR</b> issues",
#     caption = "Source: FY21 Q3 Narratives",
#     theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 
# 


######################################
#       VISUALS - PARTNER LEVEL
######################################
#How many partners reported issues, by OU?

y1<-
  agg_partner_un_collapse %>%
  filter(indicator %in% c("TX_PVLS")) %>% 
  mutate(ou_order = ifelse((category == "issue"& indicator=="TX_PVLS"), val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values =  c('#CFC3FF','#2F2E6F'))+ 
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


y2<-
  agg_partner_un_collapse %>%
  filter(indicator %in% c("TX_CURR")) %>% 
  mutate(ou_order = ifelse((category == "issue"&indicator=="TX_CURR"), val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max)))+
  geom_col(aes( x=val, fill=fct_rev(category)),  alpha=.8)+ 
  # si_style()+
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#FFB790', '#923417'))+ 
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")



(y1 + y2) + 
  plot_layout(widths  = c(1, 1)) +
  plot_annotation(
    title = "FY21 Q4: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY PARTNERS WITHIN OU",
    subtitle = "The largest number of partners in Kenya described both <b style='color:#2F2E6F'>TX_PVLS </b> and <b style='color:#923417'>TX_CURR</b> issues",
    caption = "Source: FY21 Q4 Narratives",
    theme = theme(plot.title = element_markdown(),
                  plot.subtitle = element_markdown())) 

 


######################################
#       VISUALS - NAME LEVEL
######################################
#How many partners reported issues by name (key term)


z1<-agg_name_un %>%
  filter(category=="issue" & indicator=="TX_PVLS" ) %>% 
    filter(name=="general_impact_on_vl"| name=="routine_vl_testing" | name=="tx_pvls_covid"|name=="reagent_stockout"|
             name=="results_returned_turn_around_time"|name=="equipment"|name=="facility_headcount"|
             name=="data_reporting"|name=="backlogs"|name=="sample_collection"|name=="sample_transport_testing"|
             name=="staffing"|name=="resources_reallocation"|name=="mmd"|name=="arv_stockout") %>%
  # filter(val>0) %>%
  mutate(name = str_replace_all(name, "_"," ")) %>% 
  #str_to_sentence function in stringr
  #case when to replace tx pvls covid -> TX_PVLS COVID
  mutate( 
    name=str_to_sentence(name),
    name=case_when(
      name=="Results returned turn around time"~"Results Returned/TAT",
      name=="Mmd"~"MMD",
      name=="Arv stockout"~"ARV stockout",
      name=="General impact on vl"~"General impact on VL",
      name=="Routine vl testing"~"Routine VL testing",
      name=="Tx pvls covid"~"COVID-19",
      name=="Total tx curr mech"~"Total TX_CURR Mech",
      TRUE~name) ) %>% 
  ggplot(aes(y=reorder(name, val), x=val)) +
  geom_col(fill="#2F2E6F",  alpha=.8)+  
si_style_xgrid()+
    facet_wrap(~indicator)+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


z2<-agg_name_un %>%
  filter(category=="issue" & indicator=="TX_CURR" ) %>% 
    filter(name=="covid"|name=="mmd"|name=="arv_stockout"|name=="data_reporting"|name=="results_returned_turn_around_time"|
             name=="general_impact_on_treatment"|name=="general_impact_on_vl"|name=="total_tx_curr_mech"|
             name=="staffing"|name=="routine_vl_testing"|name=="facility_headcount"| name=="equipment") %>%
    # filter(val>0) %>%
  mutate(name = str_replace_all(name, "_"," "),
         name=str_to_sentence(name), 
         name=case_when(
           name=="Covid"~"COVID-19",
           name=="Total tx curr mech"~"Total TX_CURR Mech",
           name=="Mmd"~"MMD", 
           name=="Arv stockout"~"ARV stockout", 
           name=="General impact on vl"~"General impact on VL",   
           name=="Routine vl testing"~"Routine VL testing",
           name=="Results returned turn around time"~"Results Returned/TAT",
           TRUE~name)) %>% 
  ggplot(aes(y=reorder(name, val), x=val)) +
  geom_col(fill="#923417",  alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(z1+ z2) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q4: TX_PVLS AND TX_CURR NARRATIVES REVIEWED BY KEY TERMS WITHIN PARTNERS",
    subtitle = "The largest number of partners described issues in routine VL testing and reagent stockout for <b style='color:#2F2E6F'>TX_PVLS</b> and COVID-19 for <b style='color:#923417'>TX_CURR</b>",
    caption = "Source: FY21 Q4 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))
#, when summarizing narratives by partners rather than by OU




#####################################
#stand alone visuals
#####################################
# si_rampr("burnt_siennas") %>% show_col()
# si_rampr("moody_blues") %>% show_col()

#Top 6 Issues for TX_PVLS by OU
# TX_PVLS
agg_name_un %>%
  mutate(
    operating_unit = case_when(
      str_detect(operating_unit, "^Western") ~ "WHR",
      str_detect(operating_unit, "^Dominican") ~ "DR",
      str_detect(operating_unit, "^Democratic Rep") ~ "DRC",
      str_detect(operating_unit, "^West Af") ~ "WAR",
      TRUE ~ operating_unit
    )) %>% 
  group_by(indicator, operating_unit, name, category) %>%
  summarise(val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(category == "issue", 
         indicator == "TX_PVLS", 
          name=="general_impact_on_vl"|name=="reagent_stockout"| name=="routine_vl_testing" 
         | name=="tx_pvls_covid"|name=="results_returned_turn_around_time"|name=="sample_collection",
           # | name=="data_reporting"|name=="equipment"|name=="backlogs"||name=="sample_transport_testing"|name=="facility_headcount"|
           # name=="staffing"|name=="resources_reallocation"|name=="mmd"|name=="arv_stockout",
          val > 0) %>% 
  mutate( name = str_replace_all(name, "_", " "),
    name=str_to_sentence(name),
    name=case_when(
      name=="Results returned turn around time"~"Results Returned/TAT",
      name=="Mmd"~"MMD",
      name=="Arv stockout"~"ARV stockout",
      name=="General impact on vl"~"General impact on VL",
      name=="Routine vl testing"~"Routine VL testing",
      name=="Tx pvls covid"~"COVID-19",
      name=="Total tx curr mech"~"Total TX_CURR Mech",
      TRUE~name) ) %>% 
  ggplot(aes(y=reorder_within(operating_unit, val, name), x=val, fill=name)) +
  geom_col(width=.8) +
  scale_x_continuous(position = "top") +
  scale_y_reordered() +
  # scale_fill_si(palette="moody_blues",alpha=.8)+
  scale_fill_si(palette = "siei", discrete = T) +
  si_style_xgrid() +
  facet_wrap(~name, scales = "free_y")+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "bottom")+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        strip.placement = "outside",
        legend.position = "none")+
  plot_annotation(
    title = "FY21 Q4: NUMBER OF PARTNERS REPORTING ISSUES IN TX_PVLS NARRATIVES BY KEY TERMS AND OU",
     subtitle = "Top 6 Issues for TX_PVLS by OU",
    caption = "Source: FY21 Q4 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))


#Top 6 Issues for TX_CURR by OU
#TX_CURR
agg_name_un %>%
  mutate(
    operating_unit = case_when(
      str_detect(operating_unit, "^Western") ~ "WHR",
      str_detect(operating_unit, "^Dominican") ~ "DR",
      str_detect(operating_unit, "^Democratic Rep") ~ "DRC",
      str_detect(operating_unit, "^West Af") ~ "WAR",
      TRUE ~ operating_unit
    )) %>% 
  group_by(indicator, operating_unit, name, category) %>%
  summarise(val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(category == "issue", 
         indicator == "TX_CURR",
         name=="general_impact_on_treatment"|name=="covid"|name=="mmd"|name=="arv_stockout"|
           name=="data_reporting"| name=="results_returned_turn_around_time",
           #name=="staffing"| |name=="sample_collection"|name=="routine_vl_testing" |
           # name=="general_impact_on_vl"|name=="total_tx_curr_mech"|
           # name=="facility_headcount"| name=="equipment"
         val > 0) %>% 
  mutate(name = str_replace_all(name, "_"," "),
         name=str_to_sentence(name), 
         name=case_when(
           name=="Covid"~"COVID-19",
           name=="Total tx curr mech"~"Total TX_CURR Mech",
           name=="Mmd"~"MMD", 
           name=="Arv stockout"~"ARV stockout", 
           name=="General impact on vl"~"General impact on VL",   
           name=="Routine vl testing"~"Routine VL testing",
           name=="Results returned turn around time"~"Results Returned/TAT",
           TRUE~name)) %>% 
  ggplot(aes(y=reorder_within(operating_unit, val, name), x=val, fill=name)) +
  geom_col(width=.8) +
  scale_x_continuous(position = "top") +
  scale_y_reordered() +
   scale_fill_si(palette = "siei", discrete = T) +
  si_style_xgrid() +
  facet_wrap(~name, scales = "free_y")+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "bottom")+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        strip.placement = "outside",
        legend.position = "none")+
  plot_annotation(
    title = "FY21 Q4: NUMBER OF PARTNERS REPORTING ISSUES IN TX_CURR NARRATIVES BY KEY TERMS AND OU",
    subtitle = "Top 6 Issues for TX_CURR by OU",
    caption = "Source: FY21 Q4 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))




#####################################
#stand alone visual  BY OU
#####################################
# si_rampr("siei") %>% show_col()


#top 6 OUs by FY21 TX_PVLS (D) Targets for TX_PVLS by OU
#SA​, Moz​, TZ​, Nigeria​, Kenya​.Zimbabwe

##TX_PVLS
agg_name_un %>%
  mutate(
    operating_unit = case_when(
      str_detect(operating_unit, "^Western") ~ "WHR",
      str_detect(operating_unit, "^Dominican") ~ "DR",
      str_detect(operating_unit, "^Democratic Rep") ~ "DRC",
      str_detect(operating_unit, "^West Af") ~ "WAR",
      TRUE ~ operating_unit
    )) %>% 
  group_by(indicator, operating_unit, name, category) %>%
  summarise(val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(operating_unit %in% c("South Africa", "Mozambique", "Tanzania", "Nigeria", "Kenya", "Uganda"),
    category == "issue", 
         indicator == "TX_PVLS", 
        #     name=="general_impact_on_vl"|name=="reagent_stockout"| name=="routine_vl_testing" 
        # | name=="tx_pvls_covid"|name=="results_returned_turn_around_time"|name=="sample_collection",
        val > 0) %>% 
  mutate( name = str_replace_all(name, "_", " "),
          name=str_to_sentence(name),
          name=case_when(
            name=="Results returned turn around time"~"Results Returned/TAT",
            name=="Mmd"~"MMD",
            name=="Arv stockout"~"ARV stockout",
            name=="General impact on vl"~"General impact on VL",
            name=="Routine vl testing"~"Routine VL testing",
            name=="Tx pvls covid"~"COVID-19",
            name=="Total tx curr mech"~"Total TX_CURR Mech",
            TRUE~name) ) %>% 
  ggplot(aes(y=reorder_within(name, val, operating_unit), x=val, fill=name)) +
  geom_col(width=.8) +
  scale_x_continuous(position = "top") +
  scale_y_reordered() +
  scale_fill_manual(values = c( '#808080','#808080','#c43d4d','#808080','#808080','#808080','#808080','#808080','#808080','#808080','#808080','#808080','#808080'))+ 
  # scale_fill_si(palette = "siei", discrete = T) +
  si_style_xgrid() +
  facet_wrap(~operating_unit, scales = "free_y")+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "bottom")+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        strip.placement = "outside",
        legend.position = "none")+
  plot_annotation(
    title = "FY21 Q4: NUMBER OF PARTNERS REPORTING ISSUES IN TX_PVLS NARRATIVES BY OU AND KEY TERMS",
    subtitle = "Partners reported <b style='color:#c43d4d'>COVID-19</b> issues for TX_PVLS remained key issue in Uganda alone ",
    caption = "Source: FY21 Q4 Narratives 
              Top 6 OUs by FY21 TX_PVLS (D) Targets for TX_PVLS by OU 
              6th OU = Zimbabwe (no narratives reported), replaced by 7th OU = Uganda",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))



#TX_CURR
agg_name_un %>%
  mutate(
    operating_unit = case_when(
      str_detect(operating_unit, "^Western") ~ "WHR",
      str_detect(operating_unit, "^Dominican") ~ "DR",
      str_detect(operating_unit, "^Democratic Rep") ~ "DRC",
      str_detect(operating_unit, "^West Af") ~ "WAR",
      TRUE ~ operating_unit
    )) %>% 
  group_by(indicator, operating_unit, name, category) %>%
  summarise(val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(operating_unit %in% c("South Africa", "Mozambique", "Tanzania", "Nigeria", "Kenya", "Uganda"),
    category == "issue", 
         indicator == "TX_CURR",
    # name=="covid"|name=="general_impact_on_treatment"|name=="mmd"|name=="arv_stockout"|
    #   name=="data_reporting"| name=="results_returned_turn_around_time",
    val > 0) %>% 
  mutate(name = str_replace_all(name, "_"," "),
         name=str_to_sentence(name), 
         name=case_when(
           name=="Covid"~"COVID-19",
           name=="Total tx curr mech"~"Total TX_CURR Mech",
           name=="Mmd"~"MMD", 
           name=="Arv stockout"~"ARV stockout", 
           name=="General impact on vl"~"General impact on VL",   
           name=="Routine vl testing"~"Routine VL testing",
           name=="Results returned turn around time"~"Results Returned/TAT",
           TRUE~name)) %>% 
  ggplot(aes(y=reorder_within(name, val, operating_unit), x=val, fill=name)) +
  geom_col(width=.8) +
  scale_x_continuous(position = "top") +
  scale_y_reordered() +
  #color specifically covid only
  scale_fill_manual(values = c('#808080','#c43d4d', '#808080','#808080','#808080','#808080','#808080','#808080'))+ 
   # scale_fill_si(palette = "siei", discrete = T) +
  si_style_xgrid() +
  facet_wrap(~operating_unit, scales = "free_y")+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "bottom")+
  theme(axis.title.y = element_blank(),
        # axis.text.y=element_blank(),
        legend.title = element_blank(),
        strip.placement = "outside",
        legend.position = "none")+
  plot_annotation(
    title = "FY21 Q4: NUMBER OF PARTNERS REPORTING ISSUES IN TX_CURR NARRATIVES BY KEY TERMS IN LARGEST TX_PVLS OUS",
    subtitle = "Partners reported <b style='color:#C43D4D'>COVID-19</b> issues for TX_CURR in only half of OUs with largest programs",
    caption = "Source: FY21 Q4 Narratives 
    Top 6 OUs by FY21 TX_PVLS (D) Targets for TX_PVLS by OU
    6th OU = Zimbabwe (no narratives reported), replaced by 7th OU = Uganda",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))
