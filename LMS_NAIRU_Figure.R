#############################################################################################################
################################# LABOUR MARKET SLACK NAIRU VS BECRU  ##############################################
#############################################################################################################
pacman::p_load(rstudioapi,stats, ggplot2, tidyverse, dplyr,utils,ggpubr,foreign,base,readxl,pastecs,zoo,eurostat,countrycode,scales,lubridate,directlabels,ggrepel)
current_path <- getActiveDocumentContext()$path #find the current path of the script
setwd(dirname(current_path )) 
getwd()
rm(list = ls())

LMS_countries <- c("Austria","Germany","Finland","Sweden","United Kingdom" ,"United States")

country_list_euro_eurostat <-c( "Austria","Germany","Spain","United Kingdom","United States","Sweden","Belgium", 
                                "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,
                                "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia",
                                "Cyprus","Greece","Malta")


eurozone <-c( "Austria","Belgium","Croatia","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland",
              "Italy","Latvia","Lithuania","Luxembourg", "Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")

country_list_eurostat <-c( "Austria","Germany","Spain","United Kingdom","Sweden","Belgium", 
                                     "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,"Poland" ,
                                     "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia","Romania",
                                     "Cyprus","Greece","Malta","United States","Bulgaria","Czechia","Croatia", "Hungary")

################################# READ DATA OECD  #################################
LMS_data_figures <- read_csv("LMS_annual_data.csv")%>%
  select(-"...1")%>%
  filter(year >1969, country %in% LMS_countries)

unique(as.character(LMS_data_figures$country))  

################################# READ NAIRU DATA #################################
NAIRU_estimates <- read_excel("NAIRU/NAIRU estimates AMECO December 2023.xlsx")%>%
  gather(Year, Values,-Country)%>%
  rename(country = Country)%>%
  mutate(year = as.numeric(as.character(Year)),
         NAIRU =as.numeric(Values))%>%
  select(-Year,-Values)%>%
  filter(country %in% LMS_countries)%>%
  na.omit()%>%
  filter(year <2023 & year >1969)

unique(as.character(NAIRU_estimates$country))  

################################# READ EUROSTAT DATA - ANNUAL #################################
Eurostat_df <- read_csv("Eurostat_df_annual.csv")%>%
  select(-"...1")%>%
  mutate(becru_lm_v1 = sqrt(unemployment_rate*vacancy_rate)) %>%
  mutate(fegap_lm_v1 =unemployment_rate- becru_lm_v1)%>%
  mutate(year = as.numeric(as.character(year)))

################################# MERGE AND PLOT - OECD #################################
df_nairu_becru <-full_join(LMS_data_figures,NAIRU_estimates, by =c("country","year"))%>%
  select(country,year, becru_lm_v1,NAIRU,unemployment_rate_oecd)%>%
  gather(variable,values,-year,-country)%>%
  mutate(date = as.Date(paste(year, "01-01", sep = "-")))

png("Output/NAIRU Figure long series_2024Jan.png", width = 8*300, height = 9*300, res = 300)

ggplot() +
  geom_line(data = df_nairu_becru%>% filter(country !="Spain"),
            aes(x=date, y=values,colour = variable), size=1.3) + 
  theme_minimal()+
  labs(y = "% of labour force", x= "") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "8 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10,labels = function(x) paste0(x, "%"))+
  scale_color_manual(values=c( "#E69F00", "#56B4E9","#999999","#FF9999","#69b3a2","red", "navy"),
                     label = c("BECRU","NAIRU","Unemployment rate")) +
  facet_wrap(~country, ncol = 2)

dev.off()
