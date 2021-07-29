source("Scripts/00_Setup.R")


setwd("C:/Users/jStephens/Documents/ICPI/Narrative Analysis")

df <-read_xlsx("NarrativeAnalysis_20210526_TX_CURR.xlsx", sheet="TX_CURR",
               col_types = "text") %>% 
  janitor::clean_names()
  glimpse(df)
names(df)

df %>%
  distinct(operating_unit) %>%
  pull()

df2<-df %>% 
  #drop extra info after "_" in operating unit
  mutate(operating_unit= gsub("\\_.*","",operating_unit)%>%
         str_trim(side = "both")) %>% 
  #keep only number in mechanism (everying before the first space)
  mutate(mechanism= gsub("\\ ..*","",mechanism) %>% 
           str_trim(side = "both")) %>% 
  #remove additional bracket from "[OU"
  mutate(mechanism=  case_when(
    mechanism=="[OU"~"OU",
    TRUE~mechanism)) 

  
df2 %>%
  distinct(mechanism) %>%
  pull()


df_long <- df2 %>% 
  pivot_longer(
    cols= total_tx_curr_mech:kudos,  
    values_to="val")

glimpse(df_long)
names(df_long)

df_wider<- df_long %>% 
  pivot_wider(
    names_from = val, 
    values_from=val  ) %>% 
  select(!c("389024", "313145", "85440", "NA")) %>% 
  mutate(n=case_when(
      n=="n"~"1", TRUE~n),
    i= case_when(
      i=="i"~"1", TRUE~i), 
    n=as.numeric(n), 
    i=as.numeric(i)) %>% 
  rename(no_issue=n, issue=i)

glimpse(df_wider)
names(df_wider)

df_long_Cat<- df_wider %>% 
  pivot_wider(
    names_from = val, 
    values_from=val  ) %>% 
  select(!c("389024", "313145", "85440", "NA")) %>% 
  mutate(n=case_when(
    n=="n"~"1", TRUE~n),
    i= case_when(
      i=="i"~"1", TRUE~i), 
    n=as.numeric(n), 
    i=as.numeric(i)) %>% 
  rename(no_issue=n, issue=i)

df_long_cat<- df_wider %>% 
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
  view

agg_name_long<-df_long_cat %>% 
  select(indicator, operating_unit, partner,name, category, val) %>% 
  group_by(indicator,operating_unit, partner,name, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  view

#unique count of name
agg_name_un<-agg_name_long %>% 
  mutate(val=case_when(
    val>=1~1, TRUE~val))


#############################################
#########   aggregate at partner level
agg_partner<-df_wider %>% 
  select(indicator, operating_unit, partner, no_issue, issue) %>% 
  group_by(indicator,operating_unit, partner ) %>% 
  summarize_at(vars(issue, no_issue), sum, na.rm=TRUE) %>% 
  view

agg_partner_long<-df_long_cat %>% 
  select(indicator, operating_unit, partner, category, val) %>% 
  group_by(indicator,operating_unit, partner, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  view

#unique count of partner
agg_partner_un<-agg_partner_long %>% 
  mutate(val=case_when(
    val>=1~1, TRUE~val))


#############################################
#########   aggregate at OU level
agg_ou<-df_wider %>% 
  select(indicator, operating_unit, no_issue, issue) %>% 
  group_by(indicator,operating_unit ) %>% 
  summarize_at(vars(issue, no_issue), sum, na.rm=TRUE) %>% 
  view

agg_ou_cat<-df_long_cat %>% 
  select(indicator, operating_unit, partner, category, val) %>% 
  group_by(indicator,operating_unit, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  view


#aggregate to OU by unique parter mentions
agg_ou_partner_un<-agg_partner_un %>% 
  select(indicator, operating_unit, category, val) %>% 
  group_by(indicator,operating_unit, category ) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  view

#####################################################################

# visualize all indicators for 1 OU
agg_ou %>%
  ggplot(aes(y=reorder(operating_unit, issue), x=issue)) +
  geom_col()+
  si_style()+
  facet_wrap(~indicator)+
  ggtitle("Number of TX_CURR issues reported by OU")
#image OU_ISSUE_TX_CURR

#unique partner issues by ou
agg_ou_partner_un %>%
  ggplot(aes(y=reorder(operating_unit, val), x=val)) +
  geom_col()+
  si_style()+
  facet_wrap(~indicator)+
  ggtitle("Number of Partners reporting TX_CURR issues by OU")
#image OU_ISSUE_TX_CURR



agg_ou_cat %>%
  ggplot(aes(y=reorder_within(operating_unit, val, category), x=val)) +
  geom_col()+
  si_style()+
  scale_y_reordered()+
  facet_wrap(~category, scales="free_y")+
  ggtitle("Number of TX_CURR narratives reported by OU, by issue or no issue")
#OU_ISSUE_NOISSUE_TX_CURR



agg_ou_cat %>%
  ggplot(aes(y=reorder(operating_unit, val), x=val, fill=category)) +
  geom_col()+
  si_style()+
  facet_wrap(~indicator)+
  ggtitle("Number of TX_CURR issues reported by OU")
#image OU_ISSUE_NOISSUE_TX_CURR_stackedbar


agg_ou_cat %>%
  mutate(ou_order = ifelse(category == "issue", val, 0)) %>% 
  ggplot(aes(y=fct_reorder(operating_unit, ou_order, max), x=val)) +
  geom_col()+
  si_style()+
  facet_wrap(~category, scales="free_y")+
  ggtitle("Number of TX_CURR narratives reported by OU, by issue or no issue")
#image OU_ISSUE_NOISSUE_TX_CURR_order



#####################################################
###########     parter level visuals

#number of parters reporting issues (uniqu partners) - also bad ordering
 agg_partner_un%>%
  ggplot(aes(y=reorder_within(operating_unit, val, category), x=val)) +
  geom_col()+
  si_style()+
  scale_y_reordered()+
  facet_wrap(~category, scales="free_y")+
  ggtitle("Number of Partners Reporting TX_CURR Issues by OU")
#image

#bad ordering - needs group or something
agg_partner_un %>%
  ggplot(aes(y=reorder(operating_unit, val), x=val, fill=category)) +
  geom_col()+
  si_style()+
  facet_wrap(~indicator)+
  ggtitle("Number of Partners Reporting TX_CURR Issues by O")
#image 

 
 
 




# visualize by name
agg_name_un %>%
  filter(category=="issue", operating_unit==c("South Africa")) %>% 
  ggplot(aes(y=reorder(name, val), x=val)) +
  geom_col()+
  si_style()+
  # facet_wrap(~name)+
  ggtitle("South Africa partners report most TX_CURR issues in facility headcount")
#image SA_ISSUES_TX_CURR_NAMES


# visualize by name
agg_name_un %>%
  filter(category=="issue") %>% 
  ggplot(aes(y=reorder(name, val), x=val)) +
  geom_col()+
  si_style()+
  # facet_wrap(~operating_unit)+
  ggtitle("Partners report most TX_CURR issues in COVID, MMD & Data Reporting")
#image OU_ISSUES_TX_CURR_NAMES

# visualize by name
agg_name_un %>%
  filter(category=="issue") %>% 
  # filter(operating_unit=="South Africa" | operating_unit=="Kenya"| operating_unit
  #        =="Asia Region") %>%
  filter(name=="TX_CURR_COVID"|name=="mmd"|name=="arv_stockout"|name=="data_reporting"|name=="general_impact_on_treatment"|name=="total_tx_curr_mech"|name=="staffing") %>% 
  ggplot(aes(y=reorder(name, val), x=val, fill=name)) +
  geom_col()+
  si_style()+
   facet_wrap(~operating_unit)+
  ggtitle("Unique reporting issues by OU")
#image OU_ISSUES_TX_CURR_NAMES


 ############################################################3
####### unused yet




agg %>% 
  subset(operating_unit=="South Africa") %>% 
  ggplot(aes(x=indicator, y=val, color=indicator)) +
  geom_col()+
  scale_color_si("siei")+
  si_style()+
  facet_wrap(~period)+
  ggtitle("Migration Impact on South Africa Treatment Indicators")




#visualize all mentions of migration across the 3 indicators by OU for 1 qtr

agg %>% 
  subset(period=="2021 Q2") %>% 
  ggplot(aes(x=reorder(operating_unit,val), y=val)) +
  geom_col()+
  si_style()+
  ggtitle("Migration Impact on Treatment Indicators in FY21Q2")+
  xlab("OU")+
  ylab("Count")

#attempt to reorder 

agg_fy21q2 <-agg %>% 
  subset(period=="2021 Q2") %>% 
  select(!indicator, !period) %>% 
  group_by(operating_unit) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  view

agg_fy21q2 %>% 
  ggplot(aes(x=reorder(operating_unit,val), y=val)) +
  geom_col()+
  si_style()+
  ggtitle("Migration Impact on Treatment Indicators in FY21Q2")+
  xlab("OU")+
  ylab("Count")
#how can we uncramp the OU names? turn sidewasy!

agg_fy21q2 %>% 
  ggplot(aes(y=reorder(operating_unit,val), x=val)) +
  geom_col(fill="gray70")+
  si_style()+
  ggtitle("Migration Impact on Treatment Indicators in FY21Q2")+
  ylab("OU")+
  xlab("Count")+
  geom_text(aes(label=val), 
            hjust=1, nudge_x=-.5)

#used some labelling recommended here: https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/


#adding color
# burnt_sienna %>% show_col()
# show_col(c(denim, burnt_sienna))
# show_col(c(denim, old_rose, moody_blue, burnt_sienna, golden_sand, genoa, scooter))
#   si_rampr()

#attempt 1

pal <- c(
  "gray85",
  rep("gray70", length(agg_fy21q2$operating_unit) - 4), 
  "denim", "old_rose", "burnt_sienna"
)
pal2 <- c(
  "gray85",
  rep("gray70", length(agg_fy21q2$operating_unit) - 4), 
  "2057A7", "C43D4D", "8980CB"
)

#couldn't get pal or pal2 to work in scale_fill_manual
agg_fy21q2 %>% 
  ggplot(aes(y=reorder(operating_unit,val), x=val, fill=operating_unit)) +
  geom_col(fill="gray70")+
  ggtitle("Migration Impact on Treatment Indicators in FY21Q2")+
  ylab("OU")+
  xlab("Count")+
  geom_text(aes(label=val), 
            hjust=1, nudge_x=-.5)+
  scale_fill_manual(values=pal, guide="none") +
  si_style()




##attempt 2 with scale_fill_identity


agg_fy21q2_col <-
  agg_fy21q2 %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "287c6f",
      row_number() == 2 ~ "8980cb",
      row_number() == 3 ~ "f2bc40",
      operating_unit == "Other" ~ "gray85",
      ## all others should be gray
      TRUE ~ "gray70"
    )
  )

agg_fy21q2_col %>% 
  ggplot(aes(y=reorder(operating_unit,val), x=val, color=operating_unit)) +
  geom_col(fill="gray70")+
  ggtitle("Migration Impact on Treatment Indicators in FY21Q2")+
  ylab("OU")+
  xlab("Count")+
  geom_text(aes(label=val), 
            hjust=1, nudge_x=-.5)+
  scale_fill_identity(guide="none") +
  si_style()