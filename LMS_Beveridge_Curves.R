#############################################################################################################
################################# LABOUR MARKET SLACK BEVERIDGE Curves ########################################
#############################################################################################################
pacman::p_load(rstudioapi,stats, ggplot2, tidyverse, dplyr,utils,xts,foreign,base,readxl,seasonal,stargazer,
               xlsx,pastecs,zoo,eurostat,ggrepel,countrycode,scales,lme4,ggpubr,lubridate,directlabels,
               ggrepel,plotrix,plm)
rm(list = ls())
current_path <- getActiveDocumentContext()$path #find the current path of the script
setwd(dirname(current_path )) 
getwd()

country_list_full <-c( "Austria","Germany","Spain","United Kingdom","Sweden","Belgium", "Denmark",
                       "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,"Poland" ,
                       "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia","Romania",
                       "Cyprus","Greece","Malta","United States","Bulgaria","Czechia","Croatia", "Hungary", "United States")

LMS_countries <- c("Austria","Germany","Finland","Sweden","United Kingdom" ,"United States")


################################# READ LONGER - 6 Countries - DTA  #################################
LMS_data_figures_beveridge <- read_csv("LMS_quarterly_data.csv")%>%
  select(-"...1")%>%
  filter(date >"1969-10-01", country %in% LMS_countries)%>%
  group_by(country)%>%
  mutate(group3 = ifelse(date<"2008-01-01", "1970-2007", 
                         ifelse(date >"2007-10-01" & date <"2020-01-01", "2008-2019", "2020-2022")))%>%
  mutate(group4 = ifelse(date<"1980-01-01", "1970-1979", 
                         ifelse(date >"1979-10-01" & date <"2000-01-01", "1980-1999",
                                ifelse(date >"1999-10-01" & date <"2020-01-01","2000-2019","2020-2022"))))%>%
  mutate(group5 = ifelse(date<"1980-01-01", "1970-1979", 
                         ifelse(date >"1979-10-01" & date <"2000-01-01", "1980-1999",
                                ifelse(date >"1999-10-01" & date <"2008-01-01","2000-2007",
                                       ifelse(date >"2007-10-01" & date <"2020-01-01","2008-2019","2020-2022")))))%>%
  ungroup()

LMS_data_figures_beveridge$group3 <- factor(LMS_data_figures_beveridge$group3, levels=c("1970-2007", "2008-2019", "2020-2022"))
LMS_data_figures_beveridge$group4 <- factor(LMS_data_figures_beveridge$group4, levels=c("1970-1979", "1980-1999","2000-2019","2020-2022"))
LMS_data_figures_beveridge$group5 <- factor(LMS_data_figures_beveridge$group5, levels=c("1970-1979", "1980-1999","2000-2007","2008-2019","2020-2022"))


names(LMS_data_figures_beveridge)
cbp <- c( "#999999", "#E69F00", "#56B4E9","#FF9999","#69b3a2","red", "navy")

names4 <- c("1970-1979" = "Q1 1970 to Q4 1979", 
            "1980-1999" = "Q1 1980 to Q4 1999", 
            "2000-2019" = "Q1 2000 to Q4 2019", 
            "2020-2022" = "Q1 2020 to Q4 2022")

names5 <- c("1970-1979" = "Q1 1970 to Q4 1979", 
            "1980-1999" = "Q1 1980 to Q4 1999", 
            "2000-2007" = "Q1 2000 to Q4 2007", 
            "2008-2019" = "Q1 2008 to Q4 2019", 
            "2020-2022" = "Q1 2020 to Q4 2022")

  ##### Austria #####
 LMS_data_figures_Austria<-LMS_data_figures_beveridge%>%
  select(country,date,vacancy_rate_lm_v1,unemployment_rate_oecd,group3,group4,group5)%>%
  filter(country == "Austria")%>%
  na.omit()

  bc4_Austria <- ggplot(LMS_data_figures_Austria,
                         aes(unemployment_rate_oecd, vacancy_rate_lm_v1, color = group4)) +
    geom_path(aes(group = 1))+
    geom_point() +
    ggtitle("The Beveridge curve - Austria") +
    scale_color_manual(values = cbp, labels=names4) +
    scale_fill_manual(values = cbp, labels=names4) +
    xlab("Unemployment rate") +
    ylab("Vacancy rate") +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.8, .75)) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    theme(strip.text.x = element_text(size = 11),
          legend.text=element_text(size= 10),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12),
         title =element_text(size=12))
  
  bc4_Austria
  
  ##### Germany #####
  LMS_data_figures_Germany<-LMS_data_figures_beveridge%>%
    select(country,date,vacancy_rate_lm_v1,unemployment_rate_oecd,group3,group4,group5)%>%
    filter(country == "Germany")%>%
    na.omit()
  
  bc4_Germany <-  ggplot(LMS_data_figures_Germany,
                         aes(unemployment_rate_oecd, vacancy_rate_lm_v1, color = group4)) +
    geom_path(aes(group = 1))+
    geom_point() +
    ggtitle("The Beveridge curve - Germany") +
    scale_color_manual(values = cbp, labels=names4) +
    scale_fill_manual(values = cbp, labels=names4) +
    xlab("Unemployment rate") +
    ylab("Vacancy rate") +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.8, .75)) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    theme(strip.text.x = element_text(size = 11),
          legend.text=element_text(size= 10),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12),
          title =element_text(size=12))
  
  bc4_Germany
  
  
   
  ##### Finland #####
  LMS_data_figures_Finland<-LMS_data_figures_beveridge%>%
    select(country,date,vacancy_rate_lm_v1,unemployment_rate_oecd,group3,group4,group5)%>%
    filter(country == "Finland")%>%
    na.omit()
  
  bc4_Finland <-  ggplot(LMS_data_figures_Finland,
                         aes(unemployment_rate_oecd, vacancy_rate_lm_v1, color = group4)) +
    geom_path(aes(group = 1))+
    geom_point() +
    ggtitle("The Beveridge curve - Finland") +
    scale_color_manual(values = cbp, labels=names4) +
    scale_fill_manual(values = cbp, labels=names4) +
    xlab("Unemployment rate") +
    ylab("Vacancy rate") +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.8, .75)) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    theme(strip.text.x = element_text(size = 11),
          legend.text=element_text(size= 10),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12),
          title =element_text(size=12))
  
  bc4_Finland
  
  
  ##### Sweden #####
  LMS_data_figures_Sweden<-LMS_data_figures_beveridge%>%
    select(country,date,vacancy_rate_lm_v1,unemployment_rate_oecd,group3,group4,group5)%>%
    filter(country == "Sweden")%>%
    na.omit()
 
  bc4_Sweden <-  ggplot(LMS_data_figures_Sweden,
                        aes(unemployment_rate_oecd, vacancy_rate_lm_v1, color = group4)) +
    geom_path(aes(group = 1))+
    geom_point() +
    ggtitle("The Beveridge curve - Sweden") +
    scale_color_manual(values = cbp, labels=names4) +
    scale_fill_manual(values = cbp, labels=names4) +
    xlab("Unemployment rate") +
    ylab("Vacancy rate") +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.8, .75)) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    theme(strip.text.x = element_text(size = 11),
          legend.text=element_text(size= 10),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12),
          title =element_text(size=12))
  
  bc4_Sweden
  ##### UK #####
  LMS_data_figures_UK<-LMS_data_figures_beveridge%>%
    select(country,date,vacancy_rate_lm_v1,unemployment_rate_oecd,group3,group4,group5)%>%
    filter(country == "United Kingdom")%>%
    na.omit()
  
  bc4_UK <-  ggplot(LMS_data_figures_UK,
                   aes(unemployment_rate_oecd, vacancy_rate_lm_v1, color = group4)) +
    geom_path(aes(group = 1))+
    geom_point()+
    ggtitle("The Beveridge curve - United Kingdom") +
    scale_color_manual(values = cbp, labels=names4) +
    scale_fill_manual(values = cbp, labels=names4) +
    xlab("Unemployment rate") +
    ylab("Vacancy rate") +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.8, .75)) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    theme(strip.text.x = element_text(size = 11),
          legend.text=element_text(size= 10),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12),
          title =element_text(size=12))
  
  bc4_UK
  
 
  ##### US #####
  LMS_data_figures_US<-LMS_data_figures_beveridge%>%
    select(country,date,vacancy_rate_lm_v1,unemployment_rate_oecd,group3,group4,group5)%>%
    filter(country == "United States")%>%
    na.omit()
  
  bc4_US <-  ggplot(LMS_data_figures_US,
                    aes(unemployment_rate_oecd, vacancy_rate_lm_v1, color = group4)) +
    geom_path(aes(group = 1))+
    geom_point() +
    ggtitle("The Beveridge curve - United States") +
    scale_color_manual(values = cbp, labels=names4) +
    scale_fill_manual(values = cbp, labels=names4) +
    xlab("Unemployment rate") +
    ylab("Vacancy rate") +
    theme_minimal() +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.8, .75)) +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    theme(strip.text.x = element_text(size = 11),
          legend.text=element_text(size= 10),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12),
          title =element_text(size=12))
  
  bc4_US
 
  ##### Output #####
  
  png("Output/Beveridge curves.png", width = 9*300, height = 9*300, res = 300)
  
  combined_plot1 <- ggarrange(bc4_Austria,bc4_Germany,bc4_Finland,bc4_Sweden,bc4_UK,bc4_US,
                              nrow = 3,
                              ncol = 2)
  combined_plot1
  
  dev.off()

