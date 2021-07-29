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

#####################################################################333

#########3 summarize by no issue and issue (2 vals)
agg<-df2 %>% 
   select(indicator, operating_unit, partner) %>% 
  # mutate(val=1) %>% 
  group_by(operating_unit, partner, name, no_issue, issue) %>% 
  summarize_at(vars(val), sum, na.rm=TRUE) %>% 
  view





my_labels <- seq(2004, 2020, 1) %>%
  substr(., 3, 4) %>% paste0("FY", .)
#Adjust FY labels
my_labels <- seq(2004, 2020, 1) %>%
  substr(., 3, 4) %>% paste0("FY", .)
# Keep every 4th label but replace those in between with blanks
cust_labels <- nrsmisc::every_nth(my_labels, 4, inverse = T)
# Add this to your ggplot
scale_x_discrete(labels = cust_labels)






#Visualize OUs for 1 indicator
agg %>% 
  subset(indicator=="TX_CURR") %>% 
  ggplot(aes(x=period, y=val)) +
  geom_col()+
  si_style()+
  facet_wrap(~operating_unit)
# only the last 5 OUs have the period on the y axis. 
#Also the period is squished - can it be turned sideways?


#visualize all indicators for 1 OU
agg %>% 
  subset(operating_unit=="South Africa") %>% 
  ggplot(aes(x=period, y=val)) +
  geom_col()+
  si_style()+
  facet_wrap(~indicator)+
  ggtitle("Migration Impact on South Africa Treatment Indicators")


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