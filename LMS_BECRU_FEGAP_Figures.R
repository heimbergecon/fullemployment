#############################################################################################################
################################# LABOUR MARKET SLACK FIGURES  ##############################################
#############################################################################################################
pacman::p_load(rstudioapi,stats, ggplot2, tidyverse, dplyr,utils,ggpubr,foreign,base,readxl,pastecs,zoo,
               eurostat,countrycode,scales,lubridate,directlabels,ggrepel,writexl,xlsx)
rm(list = ls())
current_path <- getActiveDocumentContext()$path #find the current path of the script
setwd(dirname(current_path )) 
getwd()

################################# READ DATA  #################################
LMS_data_figures <- read_csv("LMS_quarterly_data.csv")%>%
  select(-"...1")%>%
  filter(date >"1969-10-01")

### OECDE and Individual Business Cycles
OECD_Business_Cycles<- read_csv("Data/OECD_Business_Cycles_13032023.csv")%>%
  select(-"...1")%>%
  filter(xmin >"1969-10-01")
  
################################# Unemployment rates (1970-2022) #################################
names(LMS_data_figures)
png("Output/Figure Unemployment rate.png", width = 6*300, height = 4*300, res = 300)

ggplot() +
  geom_line(data = LMS_data_figures%>%filter(country!= "Spain"),
            aes(x=date, y=unemployment_rate_oecd,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "In % of active population", x= "") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "5 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10,labels = function(x) paste0(x, "%"))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#FF9999","#69b3a2","red", "navy")) +
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))


dev.off()

################################# STATE OF LABOUR MARKET #################################
names(LMS_data_figures)
Final_LMS_data_v1 <-LMS_data_figures%>%
  select(country,date,unemployment_rate_oecd,vacancy_rate_lm_v1)%>%
  group_by(country)%>%
  na.omit()%>%
  ungroup()%>%
  gather(var, value, -country, -date)

names(LMS_data_figures)
Final_LMS_data_v2 <-LMS_data_figures%>%
  select(country,date,unemployment_rate_oecd,vacancy_rate_lm_v1)%>%
  group_by(country)%>%
  na.omit()%>%
  ungroup()%>%
  mutate( ymax = pmax(vacancy_rate_lm_v1, unemployment_rate_oecd),
          ymin = pmin(vacancy_rate_lm_v1, unemployment_rate_oecd),
          fill = vacancy_rate_lm_v1 >= unemployment_rate_oecd)%>%
  distinct()

################# AUSTRIA
AT_p1 <-ggplot() +
  geom_line(data = Final_LMS_data_v1%>%filter(var %in% c("vacancy_rate_lm_v1","unemployment_rate_oecd"),
                                              country == "Austria"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Final_LMS_data_v2%>%filter(country == "Austria"), aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="Austria"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Austria") + 
  scale_y_continuous(limits = c(0, 20.7),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "6 years",date_labels ="%Y",limits = c(min(Final_LMS_data_v1$date), max(Final_LMS_data_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("1988-01-01"), y =7, label = "Unemployment rate", color = "unemployment_rate_oecd")) + 
  geom_text(aes(x = as.Date("1998-01-01"), y = 0, label = "Vacancy rate", color = "vacancy_rate_lm_v1")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2005-01-01"),y = 5),
                  color = "slateblue", label = "Inefficiently slack", nudge_x =0, nudge_y =4) +
  geom_text_repel(aes(as.Date("1973-06-07"),y = 2.5),
                  color = "darkorange", label = "Inefficiently tight", nudge_x = 0, nudge_y = -2.5)

AT_p1

################# GERMANY 
DE_p1 <- ggplot() +
  geom_line(data = Final_LMS_data_v1%>%filter(var %in% c("vacancy_rate_lm_v1","unemployment_rate_oecd"),
                                              country == "Germany"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Final_LMS_data_v2%>%filter(country == "Germany"), aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="Germany", xmin>"1968-10-01"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Germany") + 
  scale_y_continuous(limits = c(0, 20.7),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "6 years",date_labels ="%Y",limits = c(min(Final_LMS_data_v1$date), max(Final_LMS_data_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("1999-01-01"), y = 13, label = "Unemployment rate", color = "unemployment_rate_oecd")) + 
  geom_text(aes(x = as.Date("1999-01-01"), y = 0, label = "Vacancy rate", color = "vacancy_rate_lm_v1")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("1985-01-01"),y = 5),
                  color = "slateblue", label = "Inefficiently slack", nudge_x = 0, nudge_y = 4) +
  geom_text_repel(aes(as.Date("1970-01-01"),y = 1),
                  color = "darkorange", label = "Inefficiently tight", nudge_x =  0, nudge_y = -1) 

DE_p1

################# SWEDEN 
SE_p1<-ggplot() +
  geom_line(data = Final_LMS_data_v1%>%filter(var %in% c("vacancy_rate_lm_v1","unemployment_rate_oecd"),
                                              country == "Sweden"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Final_LMS_data_v2%>%filter(country == "Sweden"), aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="Sweden", xmin>"1974-10-01"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Sweden") + 
  scale_y_continuous(limits = c(0, 20.7),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "6 years",date_labels ="%Y",limits = c(min(Final_LMS_data_v1$date), max(Final_LMS_data_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("1998-04-01"), y = 13, label = "Unemployment rate", color = "unemployment_rate_oecd")) + 
  geom_text(aes(x = as.Date("1998-04-01"), y = 0, label = "Vacancy rate", color = "vacancy_rate_lm_v1")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("1983-04-01"),y =3),
                  color = "slateblue", label = "Inefficiently slack", nudge_x = 0, nudge_y = 3)  
SE_p1



################# FINLAND 
FI_p1<-ggplot() +
  geom_line(data = Final_LMS_data_v1%>%filter(var %in% c("vacancy_rate_lm_v1","unemployment_rate_oecd"),
                                              country == "Finland"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Final_LMS_data_v2%>%filter(country == "Finland"), aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="Finland", xmin>"1969-10-01"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Finland") + 
  scale_y_continuous(limits = c(0, 20.7),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "6 years",date_labels ="%Y",limits = c(min(Final_LMS_data_v1$date), max(Final_LMS_data_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("1980-01-01"), y = 10, label = "Unemployment rate", color = "unemployment_rate_oecd")) + 
  geom_text(aes(x = as.Date("1998-04-01"), y = 0, label = "Vacancy rate", color = "vacancy_rate_lm_v1")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2000-04-01"),y = 10),
                  color = "slateblue", label = "Inefficiently slack", nudge_x = 3000, nudge_y = 5) +
  geom_text_repel(aes(as.Date("1974-01-01"),y = 2),
                  color = "darkorange", label = "Inefficiently tight", nudge_x =  1000, nudge_y = -2) 

FI_p1


################# UNITED KINGDOM 
UK_p1<-ggplot() +
  geom_line(data = Final_LMS_data_v1%>%filter(var %in% c("vacancy_rate_lm_v1","unemployment_rate_oecd"),
                                              country == "United Kingdom"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Final_LMS_data_v2%>%filter(country == "United Kingdom"), aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="United Kingdom", xmin >"1957-10-01"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="United Kingdom") + 
  scale_y_continuous(limits = c(0, 20.7),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "6 years",date_labels ="%Y",limits = c(min(Final_LMS_data_v1$date), max(Final_LMS_data_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("1994-01-01"), y = 12, label = "Unemployment rate", color = "unemployment_rate_oecd")) + 
  geom_text(aes(x = as.Date("1994-01-01"), y = 0, label = "Vacancy rate", color = "vacancy_rate_lm_v1")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2011-01-01"),y = 4),
                  color = "slateblue", label = "Inefficiently slack", nudge_x = 0, nudge_y = 3)

UK_p1

################# UNITED STATES 
Final_LMS_data_v2_US  <-Final_LMS_data_v2%>%
  mutate(fill = ifelse(fill == T & date == "1970-01-01", F,fill))%>%
  mutate(fill = ifelse(fill == T & date == "2020-01-01", F,fill))%>%
  mutate(fill = ifelse(fill == F & date == "2021-01-01", T,fill))%>%
  mutate(fill2 = ifelse(fill == T , NA,fill))

US_p1<-ggplot() +
  geom_line(data = Final_LMS_data_v1%>%filter(var %in% c("vacancy_rate_lm_v1","unemployment_rate_oecd"),
                                              country == "United States"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Final_LMS_data_v2_US%>%filter(country == "United States"), 
              aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="United States"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="United States") +
  scale_y_continuous(limits = c(0,  20.7),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "6 years",date_labels ="%Y",limits = c(min(Final_LMS_data_v1$date), max(Final_LMS_data_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("1984-01-01"), y = 12, label = "Unemployment rate", color = "unemployment_rate_oecd")) + 
  geom_text(aes(x = as.Date("1984-01-01"), y = 1, label = "Vacancy rate", color = "vacancy_rate_lm_v1")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2010-01-01"),y = 10),
                  color = "slateblue", label = "Inefficiently slack", nudge_x = 0, nudge_y = 2) +
  geom_text_repel(aes(as.Date("2019-01-01"),y = 4),
                  color = "darkorange", label = "Inefficiently tight", nudge_x =  1000, nudge_y = -3.2) 

US_p1

################# MERGE AND EXPORT FIGURE #################
png("Output/Figure Labour Market State.png", width = 9*300, height = 9*300, res = 300)

combined_plot1 <- ggarrange(AT_p1,DE_p1,SE_p1,FI_p1,UK_p1,US_p1,
                           nrow = 3,
                           ncol = 2)
combined_plot1

dev.off()

rm(Final_LMS_data_v2, Final_LMS_data_v1)


################################# BECRU FIGURES #################################

png("Output/Figure BECRU.png", width = 6*300, height = 4*300, res = 300)

ggplot() +
  geom_line(data = LMS_data_figures%>% filter(country !="Spain"),
            aes(x=date, y=becru_lm_v1,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "5 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10,labels = function(x) paste0(x, "%"))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#FF9999","#69b3a2","red", "navy")) +
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

dev.off()


################################# UNEMPLOPYMENT GAP (FEGAP) FIGURES #################################

png("Output/Figure FEGAP.png", width = 6*300, height = 4*300, res = 300)

ggplot() +
  geom_line(data = LMS_data_figures%>%filter(country!="Spain"),
            aes(x=date, y=fegap_lm_v1,colour = country), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = "Full employment gap, percentage points", x= "") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11))+
  scale_x_date( date_breaks = "5 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#FF9999","#69b3a2","red", "navy")) +
  geom_rect(data = OECD_Business_Cycles%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

dev.off()


