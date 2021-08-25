source("Scripts/00_Setup.R")

#################################################################################
#       IMPORT DATASETS
#################################################################################


setwd("C:/Users/jStephens/Documents/ICPI/Narrative Analysis")

df <- read_xlsx("Data/NarrativeAnalysis_20210526_TX_CURR.xlsx", sheet="TX_CURR",
               col_types = "text") %>% 
  janitor::clean_names()

  glimpse(df)
  
names(df)

df_PVLS <-read_xlsx("Data/NarrativeAnalysis_20210526_TX_PVLS.xlsx", sheet="TX_PVLS",
               col_types = "text") %>% 
  janitor::clean_names()

glimpse(df)
names(df)

df2<-bind_rows(df, df_PVLS)

#################################################################################
#       CLEAN DATA
#################################################################################


df3<-df2 %>% 
  #drop extra info after "_" in operating unit
  mutate(operating_unit = gsub("\\_.*", "", operating_unit)%>%
         str_trim(side = "both")) %>% 
  #keep only number in mechanism (everying before the first space)
  mutate(mechanism = gsub("\\ ..*","",mechanism) %>% 
           str_trim(side = "both")) %>% 
  mutate(operating_unit=case_when(
    operating_unit=="Democratic Republic of the Congo"~"DRC", 
    operating_unit=="Western Hemisphere Region"~"WHR", 
    operating_unit=="Dominican Republic"~"DR",
    operating_unit=="West Africa Region"~"WAR",
    TRUE~operating_unit )) %>% 
  #remove additional bracket from "[OU"
  mutate(mechanism =  case_when(
    mechanism == "[OU"~"OU",
    TRUE~mechanism) ) 


#################################################################################
#       RESHAPE DATASET
#################################################################################

df_long <- df3 %>% 
  pivot_longer(
    cols = total_tx_curr_mech:equipment,  
    values_to = "val") %>% 
  mutate(val = case_when((indicator=="TX_PVLS"& val=="n")~"i", 
                       (indicator=="TX_PVLS" & val=="p")~"n", 
                       TRUE~val))

glimpse(df_long)
names(df_long)

df_wider <- df_long %>% 
  pivot_wider(
    names_from = val, 
    values_from = val  ) %>% 
  select(!c("389024", "313145", "85440", "NA", "1", "0")) %>% 
  mutate(n=case_when( n=="n"~"1", TRUE~n),
    i= case_when(
      i=="i"~"1", TRUE~i), 
    n=as.numeric(n), 
    i=as.numeric(i)) %>% 
  rename(no_issue=n, issue=i)

glimpse(df_wider)
names(df_wider)

df_long_cat <- df_wider %>% 
  pivot_longer(
    cols= no_issue:issue,  
    names_to="category",
    values_to="val")




#############################################
#########    aggregate by name
agg_name<-df_wider %>% 
   select(indicator, operating_unit, partner, name, no_issue, issue) %>% 
  group_by(indicator, operating_unit, partner, name) %>% 
  summarize_at(vars(issue, no_issue), sum, na.rm=TRUE) %>% 
  ungroup() 
  

agg_name_long<-df_long_cat %>% 
  select(indicator, operating_unit, partner,name, category, val) %>% 
  group_by(indicator,operating_unit, partner,name, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup() 
  

#unique count of name
agg_name_un<-agg_name_long %>% 
  mutate(val=case_when(
    val>=1~1, TRUE~val))


#aggregate to names by unique partner mentions within ou
agg_name_un_collapse<-agg_name_un %>% 
  group_by(indicator, operating_unit,name, category) %>% 
  summarise(val = sum(val))





#############################################
#########   aggregate at partner level
agg_partner<-df_wider %>% 
  select(indicator, operating_unit, partner, no_issue, issue) %>% 
  group_by(indicator,operating_unit, partner ) %>% 
  summarize_at(vars(issue, no_issue), sum, na.rm=TRUE) %>% 
  ungroup()
  

agg_partner_long<-df_long_cat %>% 
  select(indicator, operating_unit, partner, category, val) %>% 
  group_by(indicator,operating_unit, partner, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup()
  

#unique count of partner
agg_partner_un<-agg_partner_long %>% 
  mutate(val=case_when(
    val>=1~1, TRUE~val))

agg_partner_un_collapse<-agg_partner_un %>% 
  group_by(indicator, operating_unit, category) %>% 
  summarise(val = sum(val)) %>% 
  ungroup()


#############################################
#########   aggregate at OU level
agg_ou<-df_wider %>% 
  select(indicator, operating_unit, no_issue, issue) %>% 
  group_by(indicator,operating_unit ) %>% 
  summarize_at(vars(issue, no_issue), sum, na.rm=TRUE) %>% 
  ungroup()
  

agg_ou_cat<-df_long_cat %>% 
  select(indicator, operating_unit, partner, category, val) %>% 
  group_by(indicator,operating_unit, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup()
  


#aggregate to OU by unique partner mentions
agg_ou_partner_un<-agg_partner_un %>% 
  select(indicator, operating_unit, category, val) %>% 
  group_by(indicator,operating_unit, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  ungroup()
  

#################################################################################
#       VISUALS
#################################################################################

#chose colors

# TX_PVLS colors
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

  
######################################
#context visuals

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
    title = "FY21 Q3: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY OU",
    subtitle = "OUs submitting the most narratives capturing the key terms by indicator are Kenya (<b style='color:#7069B2'>TX_PVLS</b>) and Asia Region (<b style='color:#BF5A39'>TX_CURR</b>)",
    caption = "Source: FY21 Q3 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 




######################################
#ordered by issue

OU_1<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_PVLS")) %>% 
   mutate(ou_order = ifelse((category == "issue"&indicator=="TX_PVLS"), val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
   facet_wrap(~indicator)+
  scale_fill_manual(values = c('#CFC3FF', '#2F2E6F'))+ 
  labs(x = NULL, y = NULL) +
   theme(legend.position = "none")

OU_2<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_CURR")) %>% 
  mutate(ou_order = ifelse((category == "issue"&indicator=="TX_CURR"), val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#FFB790', '#923417'))+ 
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(OU_1 + OU_2) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q3: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY OU",
   subtitle = "Kenya described most <b style='color:#2F2E6F'>TX_PVLS</b> issues, while Kenya and South Africa each described the most <b style='color:#923417'>TX_CURR</b> issues",
     caption = "Source: FY21 Q3 Narratives",
     theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 


######################################
#ordered by value

OU_3<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_PVLS")) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, val)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#CFC3FF', '#2F2E6F'))+ 
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

OU_4<-
  agg_ou_cat %>%
  filter(indicator %in% c("TX_CURR")) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, val)))+
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#FFB790', '#923417'))+ 
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(OU_3 + OU_4) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q3: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY OU",
    subtitle = "Kenya described most <b style='color:#2F2E6F'>TX_PVLS issues</b> and <b style='color:#CFC3FF'>TX_PVLS non-issues</b>, while Asia Region described the most <b style='color:#FFB790'>TX_CURR</b> but a smaller portion of <b style='color:#923417'>TX_CURR</b> issues",
    caption = "Source: FY21 Q3 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 

######################################
#       VISUALS - PARTNER LEVEL
######################################


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
  geom_col(aes( x=val, fill=fct_rev(category)), alpha=.8)+ 
  si_style_xgrid()+
  facet_wrap(~indicator)+
  scale_fill_manual(values = c('#FFB790', '#923417'))+ 
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(y1 + y2) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q3: TX_PVLS AND TX_CURR NARRATIVES REVIEWED FOR KEY TERMS BY PARTNERS WITHIN OU",
    subtitle = "The largest number of partners in Kenya described both <b style='color:#2F2E6F'>TX_PVLS </b> and <b style='color:#923417'>TX_CURR</b> issues, when summarizing narratives by partners rather than by OU",
    caption = "Source: FY21 Q3 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown())) 


######################################
#       VISUALS - NAME (key term)  LEVEL
######################################

#########################
# count of issues for each name/term

z1<-agg_name_un %>%
  filter(category=="issue" & indicator=="TX_PVLS" ) %>% 
  filter(name=="tx_pvls_covid"|name=="reagent_stockout"|name=="results_returned"|name=="equipment"|name=="data_reporting"|name=="backlogs"|name=="sample_collection"|name=="sample_transport_testing"|name=="staffing"|name=="resources_reallocation"|name=="kudos") %>% 
  # filter(val>0) %>% 
  mutate(name = str_replace_all(name, "_"," ")) %>% 
  #str_to_sentence function in stringr
  #case when to replace tx pvls covid -> TX_PVLS COVID
  mutate( 
    name=str_to_sentence(name),
    name=case_when(
         name=="Tx pvls covid"~"COVID-19",
         name=="Total tx curr mech"~"Total TX_CURR Mech",
         TRUE~name) ) %>% 
  ggplot(aes(y=reorder(name, val), x=val)) +
  geom_col(fill="#2F2E6F")+  
si_style_xgrid()+
    facet_wrap(~indicator)+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")

z2<-agg_name_un %>%
  filter(category=="issue" & indicator=="TX_CURR" ) %>% 
  filter(name=="TX_CURR_COVID"|name=="mmd"|name=="arv_stockout"|name=="data_reporting"|name=="general_impact_on_treatment"|name=="general_impact_on_vl"|name=="total_tx_curr_mech"|name=="resources_reallocation"|name=="staffing") %>% 
    # filter(val>0) %>% 
   mutate(name = str_replace_all(name, "_"," "),
          name=str_to_sentence(name), 
          name=case_when(
          name=="Tx curr covid"~"COVID-19",
          name=="Total tx curr mech"~"Total TX_CURR Mech",
          name=="Mmd"~"MMD", 
          name=="Arv stockout"~"ARV stockout", 
          name=="General impact on vl"~"General impact on VL",          
          TRUE~name)) %>% 
   ggplot(aes(y=reorder(name, val), x=val)) +
  geom_col(fill="#923417")+  
  si_style_xgrid()+
  facet_wrap(~indicator)+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "none")


(z1+ z2) + plot_layout(widths  = c(1, 1))+
  plot_annotation(
    title = "FY21 Q3: TX_PVLS AND TX_CURR NARRATIVES REVIEWED BY KEY TERMS WITHIN PARTNERS",
    subtitle = "The largest number of partners described <b style='color:#2F2E6F'>TX_PVLS issues</b> mentioned Reagent Stockout, while the largest number of <b style='color:#923417'>TX_CURR issues</b> mentioned MMD",
    caption = "Source: FY21 Q3 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))
#, when summarizing narratives by partners rather than by OU


####################################
# names faceted out by OU

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
         name %in% c("TX_CURR_COVID", "mmd", "arv_stockout", 
                     "data_reporting", "general_impact_on_treatment", 
                     "total_tx_curr_mech", "staffing"),
         val > 0) %>% 
  mutate(name = str_replace_all(name, "_", " ")) %>% 
  ggplot(aes(y=reorder_within(operating_unit, val, name), x=val, fill=name)) +
  geom_col() +
  scale_x_continuous(position = "top") +
  scale_y_reordered() +
  scale_fill_si(palette = "siei", discrete = T) +
  si_style_xgrid() +
  facet_wrap(~name, scales = "free_y")+
  ggtitle("Unique reporting issues by OU")+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "bottom")+
   theme(axis.title.y = element_blank(),
      legend.title = element_blank(),
      strip.placement = "outside",
      legend.position = "top")

#image OU_ISSUES_TX_CURR_NAMES


####################################
# OU faceted out by names
agg_name_un %>%
  filter(category=="issue" & indicator=="TX_CURR") %>% 
  filter(val>0) %>% 
  filter(name=="TX_CURR_COVID"|name=="mmd"|name=="arv_stockout"|name=="data_reporting"|name=="general_impact_on_treatment"|name=="total_tx_curr_mech"|name=="staffing") %>% 
  mutate(name = str_replace_all(name, "_"," "),
         name=str_to_sentence(name), 
         name=case_when(
           name=="Tx curr covid"~"COVID-19",
           name=="Total tx curr mech"~"Total TX_CURR Mech",
           name=="Mmd"~"MMD", 
           name=="Arv stockout"~"ARV stockout", 
           name=="General impact on vl"~"General impact on VL",          
           TRUE~name)) %>% 
   ggplot(aes(y=fct_reorder(operating_unit, name,max), x=val, fill=name))+
  # ggplot(aes(y=reorder_within(operating_unit, val, name), x=val, fill=name)) +
  geom_col(width=.8)+
  si_style()+
  si_style_xgrid()+
  scale_y_reordered()+
  facet_wrap(~name, scales = "free_y")+
  # ggtitle("Unique reporting issues by OU")+
  labs(x = NULL, y = NULL)+
  theme(legend.position = "bottom")+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.title = element_blank(),
        legend.position = "none") +
  plot_annotation(
    title = "FY21 Q3: TX_CURR NARRATIVES REVIEWED BY PARTNER WITHIN KEY TERMS",
    subtitle = "...",
    caption = "Source: FY21 Q3 Narratives",
    theme = theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))

