#############################################################################################################
################################# LABOUR MARKET SLACK DATA  ########################################
#############################################################################################################
pacman::p_load(rstudioapi,stats, ggplot2, tidyverse, dplyr,utils,xts,foreign,base,readxl,seasonal,xlsx,pastecs,zoo,eurostat,ggrepel,countrycode,scales,lme4)
rm(list = ls())
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path )) 
getwd()

var_list <-c("Job vacancies, Total, Unfilled vacancies (stock)","Job vacancies, Total, Unfilled vacancies (stock), sa",        
             "Registered unemployment, Level, Total","Registered unemployment, Level, Total, sa",                    
             "Registered unemployment, Rate, Total","Registered unemployment, Rate, Total, sa")

country_list_LMS <-c( "Austria","Germany","Finland","United Kingdom","Sweden")

country_list_eu <-c( "Belgium" ,"Czech Republic","Denmark","Finland",                     
                     "France","Greece","Hungary","Ireland","Luxembourg","Netherlands" ,
                     "Poland","Portugal" ,"Slovak Republic","Estonia" ,   "Slovenia","Lithuania" )

country_list_euro <-c( "Belgium", "Finland","France","Greece","Ireland","Luxembourg","Netherlands" ,
                       "Portugal" ,"Slovak Republic",  "Estonia" , "Slovenia","Lithuania" )


country_list_euro_eurostat <-c( "Austria","Germany","Spain","United Kingdom","Sweden","Belgium", 
                                "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,
                                "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia",
                                "Cyprus","Greece","Malta")

country_list_euro_eurostat_full <-c( "Austria","Germany","Spain","United Kingdom","Sweden","Belgium", 
                                     "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,
                                     "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia",
                                     "Cyprus","Greece","Malta",
                                     "EA","EA19","EA20","EU" ,"EU27_2007","EU27_2020","EU28" )

####################################### 1) EUROSTAT VACANCY STOCK ##########################################################
#Source: https://ec.europa.eu/eurostat/databrowser/view/JVS_Q_NACE2/default/table?lang=en&category=labour.jvs
Eurostat_fullset_vacancy1 <- read_csv("Data/jvs_q_nace2_linear.csv")%>%
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG)%>%
  spread(s_adj, OBS_VALUE)%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(country %in% country_list_LMS, indic_em =="JOBVAC",
         sizeclas =="TOTAL",nace_r2 =="B-S")%>%
  select(country,TIME_PERIOD,NSA)%>%
  rename(vacancy_stock_eurostat1 = NSA)%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

names(Eurostat_fullset_vacancy1)
unique(as.character(Eurostat_fullset_vacancy1$indic_em))  

### Set 2 before 2009 ( no need to update - 2023 December)
#Source: https://ec.europa.eu/eurostat/databrowser/view/JVS_Q_NACE1__custom_5693618/default/table?lang=en
Eurostat_fullset_vacancy2 <- read_csv("Data/jvs_q_nace1__custom_5693618_linear.csv")%>%
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG)%>%
  spread(s_adj, OBS_VALUE)%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(country %in% country_list_LMS, indic_em =="JOBVAC",
         sizeclas =="TOTAL",nace_r1 =="TOTAL")%>%
  select(country,TIME_PERIOD,NSA)%>%
  rename(vacancy_stock_eurostat2 = NSA)%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

names(Eurostat_fullset_vacancy2)
unique(as.character(Eurostat_fullset_vacancy2$s_adj))  

Eurostat_fullset_vacancy <-full_join(Eurostat_fullset_vacancy1,Eurostat_fullset_vacancy2, by = c("date", "country"))%>%
  mutate(vacancy_stock_eurostat = ifelse(!is.na(vacancy_stock_eurostat1), vacancy_stock_eurostat1,vacancy_stock_eurostat2))%>%
  select(country,date,vacancy_stock_eurostat)

rm(Eurostat_fullset_vacancy1,Eurostat_fullset_vacancy2)

########## 1.1)SEASONALITY ADJUSTMENT - EUROSTAT ##########
for (variable in unique(Eurostat_fullset_vacancy$country)) {
  assign(variable, Eurostat_fullset_vacancy %>% filter(country == variable)%>%select(-country)%>%
           arrange(date), envir = .GlobalEnv)
}

Austria<-Austria%>%
  filter(date >="2010-01-01")

Eurostat_ts_at<- ts(Austria[-1], start = c(2010, 1), freq = 4)
Eurostat_ts_de<- ts(Germany[-1], start = c(2006, 1), freq = 4)
Eurostat_ts_se<- ts(Sweden[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_fi<- ts(Finland[-1], start = c(2002, 1), freq = 4)
Eurostat_ts_uk<- ts(`United Kingdom`[-1], start = c(2001, 2), freq = 4)

for (name in unique(Eurostat_fullset_vacancy$country)) rm(list = name)

Eurostat_ts_adjusted_at <- seas(Eurostat_ts_at)
Eurostat_ts_adjusted_de <- seas(Eurostat_ts_de)
Eurostat_ts_adjusted_se <- seas(Eurostat_ts_se)
Eurostat_ts_adjusted_fi <- seas(Eurostat_ts_fi)
Eurostat_ts_adjusted_uk <- seas(Eurostat_ts_uk)

rm(Eurostat_ts_at,Eurostat_ts_de,Eurostat_ts_fi,Eurostat_ts_se,Eurostat_ts_uk)

plot(Eurostat_ts_adjusted_fi)

df_at <- as.xts(final(Eurostat_ts_adjusted_at))
df_de<- as.xts(final(Eurostat_ts_adjusted_de))
df_se<- as.xts(final(Eurostat_ts_adjusted_se))
df_fi<- as.xts(final(Eurostat_ts_adjusted_fi))
df_uk<- as.xts(final(Eurostat_ts_adjusted_uk))

rm(Eurostat_ts_adjusted_at,Eurostat_ts_adjusted_de,Eurostat_ts_adjusted_fi,Eurostat_ts_adjusted_se,Eurostat_ts_adjusted_uk)

df_at_final <- data.frame(Date = index(df_at), coredata(df_at) )%>%
  mutate(country="Austria")%>%
  rename(vacancy_stock_eurostat=coredata.df_at.)
df_de_final <- data.frame(Date = index(df_de), coredata(df_de) )%>%
  mutate(country="Germany")%>%
  rename(vacancy_stock_eurostat=coredata.df_de.)
df_se_final <- data.frame(Date = index(df_se), coredata(df_se) )%>%
  mutate(country="Sweden")%>%
  rename(vacancy_stock_eurostat=coredata.df_se.)
df_fi_final <- data.frame(Date = index(df_fi), coredata(df_fi) )%>%
  mutate(country="Finland")%>%
  rename(vacancy_stock_eurostat=coredata.df_fi.)
df_uk_final <- data.frame(Date = index(df_uk), coredata(df_uk) )%>%
  mutate(country="United Kingdom")%>%
  rename(vacancy_stock_eurostat=coredata.df_uk.)

listOfDataFrames<-list(df_at_final,df_de_final,df_fi_final,df_se_final,df_uk_final)
df <- do.call("rbind", listOfDataFrames)
rm(df_at_final,df_de_final,df_fi_final,df_se_final,df_uk_final,listOfDataFrames,df_at,df_de,df_uk,df_fi,df_se)

Eurostat_adjusted_final <- df%>%
  separate(Date, c("year", "quarter"), sep = "\\ ")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

rm(Eurostat_fullset_vacancy,df)

####################################### 2) OECD VACANCY STOCK ##########################################################
OECD_vac.dta <- read_csv("Data/LAB_REG_VAC_2023DEC.csv")%>%
  select(LOCATION, Country,SUBJECT,Subject,FREQUENCY, TIME, Value)%>%
  rename(country =Country, value =Value)%>%
  mutate(var ="")%>%
  mutate(var = ifelse(Subject == "Job vacancies, Total, Unfilled vacancies (stock)","vacancies_stock",
                      ifelse(Subject == "Job vacancies, Total, Unfilled vacancies (stock), sa","vacancies_stock_sa",
                             ifelse(Subject =="Registered unemployment, Level, Total", "unemployement_level",
                                    ifelse(Subject =="Registered unemployment, Level, Total, sa", "unemployement_level_sa",
                                           ifelse(Subject =="Registered unemployment, Rate, Total, sa", "unemployement_rate_sa", 
                                                  ifelse(Subject =="Registered unemployment, Rate, Total", "unemployement_rate", 
                                                         ifelse(Subject =="Job vacancies, Total, New vacancies (flow)", "vacancies_flow", 
                                                                ifelse(Subject =="Job vacancies, Total, New vacancies (flow), sa", "vacancies_flow_sa",
                                                                       ifelse(Subject =="Job Vacancies, Public sector, Unfilled vacancies (stock)", "public_vacancies_stock",
                                                                              ifelse(Subject =="Job Vacancies, Public sector, Unfilled vacancies (stock), sa", "public_vacancies_stock_sa",
                                                                                     ifelse(Subject =="Job vacancies, Private sector, Unfilled vacancies (stock)", "private_vacancies_stock","private_vacancies_stock_sa"))))))))))))%>%
  select(-Subject,-SUBJECT)

        
                  
                  
names(OECD_vac.dta)
unique(as.character(OECD_vac.dta$var))  


OECD_vac_q.dta <-OECD_vac.dta%>%
  filter(FREQUENCY =="Q",
         var %in% c("vacancies_stock","vacancies_stock_sa","unemployement_rate_sa","unemployement_level_sa","unemployement_level"))%>%
  select(-FREQUENCY, -LOCATION)%>%
  separate(TIME, c("year", "quarter"), sep = "\\-")%>%
  filter(country %in% c("Austria","Germany", "Finland","Sweden","United Kingdom","United States"))%>%
  spread(var,value)%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(unemployement_rate_sa_vac =unemployement_rate_sa)%>%
  mutate(vacancies_stock = ifelse(country == "United Kingdom" & is.na(vacancies_stock), vacancies_stock_sa,vacancies_stock))

rm(OECD_vac.dta)
names(OECD_vac_q.dta)

####################################### 3 - 1) REGRESS TO CREATE FACTORS PATCHED ##########################################################
names(OECD_vac_q.dta)

vacancy_factors_AT <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Austria")

vacancy_factors_DE <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Germany")

vacancy_factors_FI <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Finland")

vacancy_factors_SE <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Sweden")

vacancy_factors_UK <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="United Kingdom")

#v1: regress eurostat data on oecd data and use coefficients for a linear adjustment - exluding COVID
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_AT, subset= date <"2020-01-01")
vacancy_factors_AT$vacancies_stock_oecd_v1lm <- vacancy_factors_AT$vacancy_stock_eurostat
vacancy_factors_AT[vacancy_factors_AT$date<"2010-01-01",'vacancies_stock_oecd_v1lm'] <- vacfit$coefficients[1] + vacancy_factors_AT[vacancy_factors_AT$date<"2010-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_DE, subset= date <"2020-01-01")
vacancy_factors_DE$vacancies_stock_oecd_v1lm <- vacancy_factors_DE$vacancy_stock_eurostat
vacancy_factors_DE[vacancy_factors_DE$date<"2006-01-01",'vacancies_stock_oecd_v1lm'] <- vacfit$coefficients[1] + vacancy_factors_DE[vacancy_factors_DE$date<"2006-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_SE, subset= date <"2020-01-01")
vacancy_factors_SE$vacancies_stock_oecd_v1lm <- vacancy_factors_SE$vacancy_stock_eurostat
vacancy_factors_SE[vacancy_factors_SE$date<"2001-01-01",'vacancies_stock_oecd_v1lm'] <- vacfit$coefficients[1] + vacancy_factors_SE[vacancy_factors_SE$date<"2001-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_FI, subset= date <"2020-01-01")
vacancy_factors_FI$vacancies_stock_oecd_v1lm <- vacancy_factors_FI$vacancy_stock_eurostat
vacancy_factors_FI[vacancy_factors_FI$date<"2002-01-01",'vacancies_stock_oecd_v1lm'] <- vacfit$coefficients[1] + vacancy_factors_FI[vacancy_factors_FI$date<"2002-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_UK, subset= date <"2020-01-01")
vacancy_factors_UK$vacancies_stock_oecd_v1lm <- vacancy_factors_UK$vacancy_stock_eurostat
vacancy_factors_UK[vacancy_factors_UK$date<"2001-04-01",'vacancies_stock_oecd_v1lm'] <- vacfit$coefficients[1] + vacancy_factors_UK[vacancy_factors_UK$date<"2001-04-01",'vacancies_stock_sa']*vacfit$coefficients[2]
vacancy_factors_UK[vacancy_factors_UK$date>"2020-07-01",'vacancies_stock_oecd_v1lm'] <- vacfit$coefficients[1] + vacancy_factors_UK[vacancy_factors_UK$date>"2020-07-01",'vacancies_stock_sa']*vacfit$coefficients[2]

list_of_dataframes <- list(vacancy_factors_FI,vacancy_factors_SE,vacancy_factors_UK, vacancy_factors_AT,vacancy_factors_DE) 
vacancy_factors_reg <- do.call("rbind", list_of_dataframes)%>%
  select(country,date,vacancies_stock_oecd_v1lm)

rm(list_of_dataframes,vacfit,vacancy_factors_FI,vacancy_factors_SE,vacancy_factors_UK, vacancy_factors_AT,vacancy_factors_DE)

names(OECD_vac_q.dta)
OECD_vac_q_regress1.dta <-full_join(OECD_vac_q.dta,vacancy_factors_reg, by=c("country","date"))
  
rm(vacancy_factors_reg)

####################################### 3 - 2) REGRESS TO CREATE FACTORS ALL BUT 2020 ##########################################################
names(OECD_vac_q.dta)
vacancy_factors_AT <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Austria")

vacancy_factors_DE <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Germany")

vacancy_factors_FI <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Finland")

vacancy_factors_SE <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Sweden")

vacancy_factors_UK <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="United Kingdom")


#v1: regress eurostat data on oecd data and use coefficients for a linear adjustment - exluding COVID
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_AT, subset= date <"2020-01-01")
vacancy_factors_AT$vacancies_stock_oecd_v2lm <- vacancy_factors_AT$vacancy_stock_eurostat
vacancy_factors_AT[vacancy_factors_AT$date<"2020-01-01",'vacancies_stock_oecd_v2lm'] <- vacfit$coefficients[1] + vacancy_factors_AT[vacancy_factors_AT$date<"2020-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_DE, subset= date <"2020-01-01")
vacancy_factors_DE$vacancies_stock_oecd_v2lm <- vacancy_factors_DE$vacancy_stock_eurostat
vacancy_factors_DE[vacancy_factors_DE$date<"2020-01-01",'vacancies_stock_oecd_v2lm'] <- vacfit$coefficients[1] + vacancy_factors_DE[vacancy_factors_DE$date<"2020-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_SE, subset= date <"2020-01-01")
vacancy_factors_SE$vacancies_stock_oecd_v2lm <- vacancy_factors_SE$vacancy_stock_eurostat
vacancy_factors_SE[vacancy_factors_SE$date<"2020-01-01",'vacancies_stock_oecd_v2lm'] <- vacfit$coefficients[1] + vacancy_factors_SE[vacancy_factors_SE$date<"2020-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_FI, subset= date <"2020-01-01")
vacancy_factors_FI$vacancies_stock_oecd_v2lm <- vacancy_factors_FI$vacancy_stock_eurostat
vacancy_factors_FI[vacancy_factors_FI$date<"2020-01-01",'vacancies_stock_oecd_v2lm'] <- vacfit$coefficients[1] + vacancy_factors_FI[vacancy_factors_FI$date<"2020-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_UK, subset= date <"2020-01-01")
vacancy_factors_UK$vacancies_stock_oecd_v2lm <- vacancy_factors_UK$vacancy_stock_eurostat
vacancy_factors_UK[vacancy_factors_UK$date<"2020-01-01",'vacancies_stock_oecd_v2lm'] <- vacfit$coefficients[1] + vacancy_factors_UK[vacancy_factors_UK$date<"2020-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

list_of_dataframes <- list(vacancy_factors_FI,vacancy_factors_SE,vacancy_factors_UK, vacancy_factors_AT,vacancy_factors_DE) 
vacancy_factors_reg <- do.call("rbind", list_of_dataframes)%>%
  select(country,date,vacancies_stock_oecd_v2lm)

rm(list_of_dataframes,vacfit,vacancy_factors_FI,vacancy_factors_SE,vacancy_factors_UK, vacancy_factors_AT,vacancy_factors_DE)

names(OECD_vac_q_regress1.dta)
OECD_vac_q_regress2.dta <-full_join(OECD_vac_q_regress1.dta,vacancy_factors_reg, by=c("country","date"))%>%
  mutate(vacancies_stock_oecd_v2lm = ifelse(date>"2019-10-01", vacancies_stock_sa,vacancies_stock_oecd_v2lm))

rm(OECD_vac_q_regress1.dta,vacancy_factors_reg)


####################################### 3 - 3) REGRESS TO CREATE FACTORS ALL ##########################################################
names(OECD_vac_q.dta)
vacancy_factors_AT <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Austria")

vacancy_factors_DE <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Germany")

vacancy_factors_FI <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Finland")

vacancy_factors_SE <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="Sweden")

vacancy_factors_UK <-full_join(Eurostat_adjusted_final,OECD_vac_q.dta, by = c("date", "country"))%>%
  filter(country=="United Kingdom")


#v1: regress eurostat data on oecd data and use coefficients for a linear adjustment - exluding COVID
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_AT, subset= date <"2020-01-01")
vacancy_factors_AT$vacancies_stock_oecd_v3lm <- vacancy_factors_AT$vacancy_stock_eurostat
vacancy_factors_AT[vacancy_factors_AT$date<"2024-01-01",'vacancies_stock_oecd_v3lm'] <- vacfit$coefficients[1] + vacancy_factors_AT[vacancy_factors_AT$date<"2024-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_DE, subset= date <"2020-01-01")
vacancy_factors_DE$vacancies_stock_oecd_v3lm <- vacancy_factors_DE$vacancy_stock_eurostat
vacancy_factors_DE[vacancy_factors_DE$date<"2024-01-01",'vacancies_stock_oecd_v3lm'] <- vacfit$coefficients[1] + vacancy_factors_DE[vacancy_factors_DE$date<"2024-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_SE, subset= date <"2020-01-01")
vacancy_factors_SE$vacancies_stock_oecd_v3lm <- vacancy_factors_SE$vacancy_stock_eurostat
vacancy_factors_SE[vacancy_factors_SE$date<"2024-01-01",'vacancies_stock_oecd_v3lm'] <- vacfit$coefficients[1] + vacancy_factors_SE[vacancy_factors_SE$date<"2024-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_FI, subset= date <"2020-01-01")
vacancy_factors_FI$vacancies_stock_oecd_v3lm <- vacancy_factors_FI$vacancy_stock_eurostat
vacancy_factors_FI[vacancy_factors_FI$date<"2024-01-01",'vacancies_stock_oecd_v3lm'] <- vacfit$coefficients[1] + vacancy_factors_FI[vacancy_factors_FI$date<"2024-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

rm(vacfit)
vacfit <- lm(vacancy_stock_eurostat ~ vacancies_stock_sa, data=vacancy_factors_UK, subset= date <"2020-01-01")
vacancy_factors_UK$vacancies_stock_oecd_v3lm <- vacancy_factors_UK$vacancy_stock_eurostat
vacancy_factors_UK[vacancy_factors_UK$date<"2024-01-01",'vacancies_stock_oecd_v3lm'] <- vacfit$coefficients[1] + vacancy_factors_UK[vacancy_factors_UK$date<"2024-01-01",'vacancies_stock_sa']*vacfit$coefficients[2]

list_of_dataframes <- list(vacancy_factors_FI,vacancy_factors_SE,vacancy_factors_UK, vacancy_factors_AT,vacancy_factors_DE) 
vacancy_factors_reg <- do.call("rbind", list_of_dataframes)%>%
  select(country,date,vacancies_stock_oecd_v3lm)

rm(list_of_dataframes,vacfit,vacancy_factors_FI,vacancy_factors_SE,vacancy_factors_UK, vacancy_factors_AT,vacancy_factors_DE)

names(OECD_vac_q_regress2.dta)
OECD_vac_q_factored.dta <-full_join(OECD_vac_q_regress2.dta,vacancy_factors_reg, by=c("country","date"))

rm(OECD_vac_q_regress2.dta,vacancy_factors_reg,OECD_vac_q.dta)

####################################### 4) EUROSTAT UNEMPLOYMENT RATE ##########################################################
### Historical series for UK
Eurostat_fullset_unemployment1 <- read_csv("Data/une_rt_q_h_linear2023Dec.csv")%>%
  filter(unit == "PC_ACT", age == "Y15-74", sex =="T",s_adj=="SA") %>% # per ACT (same as active pop)
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -age,-unit,-sex)%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(unemployment_rate_eurostat1 = OBS_VALUE)%>%
  select(country,date,unemployment_rate_eurostat1)

names(Eurostat_fullset_unemployment1)
unique(as.character(Eurostat_fullset_unemployment1$age)) 

### Newer series
Eurostat_fullset_unemployment2 <- read_csv("Data/une_rt_q_linear2023Dec.csv")%>%
  filter(unit == "PC_ACT", age == "Y15-74", sex =="T",s_adj=="SA") %>% # per ACT (same as active pop)
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -age,-unit,-sex)%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(unemployment_rate_eurostat2 = OBS_VALUE)%>%
  select(country,date,unemployment_rate_eurostat2)

names(Eurostat_fullset_unemployment2)
unique(as.character(Eurostat_fullset_unemployment2$age)) 

Eurostat_fullset_unemployment<-full_join(Eurostat_fullset_unemployment1,Eurostat_fullset_unemployment2, by= c("date","country"))%>%
  mutate(unemployment_rate_eurostat = ifelse(!is.na(unemployment_rate_eurostat2),unemployment_rate_eurostat2,unemployment_rate_eurostat1))%>%
  select(-unemployment_rate_eurostat2,-unemployment_rate_eurostat1)%>%
  filter(country %in% c("Austria","Germany", "Finland","Sweden","United Kingdom","United States")) 

rm(Eurostat_fullset_unemployment1,Eurostat_fullset_unemployment2)
unique(as.character(Eurostat_fullset_unemployment$country))  

####################################### 5) OECD UNEMPLOYMENT DATA #######################################
OECD_unemp.dta <- read_csv("Data/DP_UNEMP_LIVE_2023Dec.csv")%>%
  filter(SUBJECT =="TOT", FREQUENCY =="Q")%>%
  select(LOCATION, TIME,Value)%>%
  mutate(country =countrycode(sourcevar = LOCATION, "iso3c", "country.name"))%>%
  filter(country %in% c("Austria","Germany","Sweden","Finland","United Kingdom","United States"))%>%
  select(-LOCATION)%>%
  rename(value =Value)%>%
  separate(TIME, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(unemployement_rate_sa =value)

names(OECD_unemp.dta)

####################################### 6) OECD ACTIVE POPULATION #######################################
### Levels thousand
OECD_labour_q.dta <- read_csv("Data/STLABOUR_DEC2023.csv")%>%
  select(Country,Subject, TIME,MEASURE, Unit, PowerCode, Value)%>%
  filter(Subject != "Population (LFS basis), All ages, All persons" )%>%
  #mutate(var = ifelse(Subject == "Active population, Aged 15 and over, All persons","active_pop",
  #                    ifelse(Subject == "Working age population, Aged 15 and over, All persons", "working_pop","unemp_rate")))%>%
  select(-Subject)%>%
  rename(country=Country, value =Value)%>%
  separate(TIME, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  select(-PowerCode, -Unit)%>%
  filter(country %in% c("Austria","Germany","Sweden","Finland","United Kingdom","United States"))%>%
  rename(active_pop_STSA =value)%>%
#  unite(var_sa, var, MEASURE)%>%
 # select(-PowerCode, -Unit)%>%
 # spread(var_sa,value)%>%
 # select(country,date, active_pop_ST,active_pop_STSA)
  select(country,date,active_pop_STSA)

names(OECD_labour_q.dta)
unique(as.character(OECD_labour_q.dta$MEASURE)) 

####################################### 7) SWEDEN WORKING POPULATION AND UNEMPLOYMENT - SCB #######################################
### Levels thousand, not seasonally adjusted, 
## Age is problematic for the older series since it is only available for 16-64 and not for total 15-74 years 
SCB_pop_unemployment_20230303 <- read_csv("Data/SCB_pop_unemployment_DEC2023_P1.csv")%>%
  filter(age =="total 16-64 years")%>%
  select(-sex,-age)%>%
  rename(var ="labour status")%>%
  gather(time, values1, -var)%>%
  separate(time, c("unit", "time2"), sep = " ")%>%
  separate(time2, c("year", "quarter"), sep = "K")%>%
  mutate(quarter = ifelse(quarter == "1","01",ifelse(quarter == "2","04", ifelse(quarter == "3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  mutate(country = "Sweden")

SCB_pop_unemployment_20230303_P2 <- read_csv("Data/SCB_pop_unemployment_DEC2023_P2.csv")%>%
  filter(age =="total 16-64 years")%>%
  select(-sex,-age)%>%
  rename(var ="labour status")%>%
  gather(time, values2, -var)%>%
  separate(time, c("unit", "time2"), sep = " ")%>%
  separate(time2, c("year", "quarter"), sep = "K")%>%
  mutate(quarter = ifelse(quarter == "1","01",ifelse(quarter == "2","04", ifelse(quarter == "3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  mutate(country = "Sweden")

names(SCB_pop_unemployment_20230303)
unique(as.character(SCB_pop_unemployment_20230303$unit))

### Keep for unemployment
SCB_Sweden_data <-full_join(SCB_pop_unemployment_20230303,SCB_pop_unemployment_20230303_P2, by =c("country","date"  ,"var","unit"))%>%
  mutate(value = ifelse(is.na(values1),values2,values1), .keep="unused")%>%
  filter(var !="total")%>%
  mutate(value=as.numeric(str_remove(value, ",")))

SCB_Sweden_workingpop_unad <- SCB_Sweden_data%>%
  filter(var == "in the labour force", unit =="Thousands")%>%
  select(-var,-country,-unit)

rm(SCB_pop_unemployment_20230303,SCB_pop_unemployment_20230303_P2)

########## 7.1)SEASONALITY ADJUSTMENT - SWEDEN ##########
Sweden_ts<- ts(SCB_Sweden_workingpop_unad[-1], start = c(1970, 1), freq = 4)
Sweden_ts_adjusted <- seas(Sweden_ts)
plot(SCB_Sweden_workingpop_unad$date,SCB_Sweden_workingpop_unad$value, type = "l")
plot(Sweden_ts_adjusted)

df_se <- as.xts(final(Sweden_ts_adjusted))
SCB_Sweden_workingpop <- data.frame(Date = index(df_se), coredata(df_se) )%>%
  mutate(country="Sweden")%>%
  rename(value=coredata.df_se.)%>%
  separate(Date, c("year", "quarter"), sep = "\\ ")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

plot(SCB_Sweden_workingpop$date,SCB_Sweden_workingpop$value, type = "l")
rm(Sweden_ts,Sweden_ts_adjusted,df_se,SCB_Sweden_workingpop_unad)

########## 7.2) MERGE SWEDEN WITH OECD ##########
OECD_labour_q_full1.dta<-full_join(OECD_labour_q.dta,SCB_Sweden_workingpop, by=c("date","country") )%>%
  mutate(active_pop_STSA = ifelse(country =="Sweden", value, active_pop_STSA))%>%
  select(-value)

rm(SCB_Sweden_workingpop,OECD_labour_q.dta)

########## 7.3) UNEMPLOYMENT LEVEL SWEDEN - SCB  ##########
names(SCB_Sweden_data)
unique(as.character(SCB_Sweden_data$unit))

SCB_Sweden_unemp_unad <- SCB_Sweden_data%>%
  filter(unit =="Thousands")%>%
  select(-unit)%>%
  spread(var, value)%>%
  select(date,unemployed)%>%
  mutate(unemployed =unemployed*1000)

plot(SCB_Sweden_unemp_unad$date,SCB_Sweden_unemp_unad$unemployed, type = "l")

########## 7.4) SEASONALITY ADJUSTMENT - SWEDEN ##########
Sweden_ts<- ts(SCB_Sweden_unemp_unad[-1], start = c(1970, 1), freq = 4)
Sweden_ts_adjusted <- seas(Sweden_ts)
plot(Sweden_ts_adjusted)
df_se <- as.xts(final(Sweden_ts_adjusted))
SCB_Sweden_unemp <- data.frame(Date = index(df_se), coredata(df_se) )%>%
  mutate(country="Sweden")%>%
  rename(unemployed=coredata.df_se.)%>%
  separate(Date, c("year", "quarter"), sep = "\\ ")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

plot(SCB_Sweden_unemp$date,SCB_Sweden_unemp$unemployed, type = "l")
rm(Sweden_ts,Sweden_ts_adjusted,df_se,SCB_Sweden_unemp_unad)

########## 7.5) MERGE UNEMPLOYMENT WITH OECD - SWEDEN ##########
names(OECD_vac_q_factored.dta)
OECD_vac_q_factored_SE.dta <-full_join(OECD_vac_q_factored.dta,SCB_Sweden_unemp, by =c("date","country"))%>%
  mutate(unemployement_level_sa= ifelse(country=="Sweden", unemployed,unemployement_level_sa ))%>%
  select(-unemployed)

rm(SCB_Sweden_unemp,SCB_Sweden_data)

####################################### 8) MERGE FINLAND UNEMP AND ACTIVE POP #######################################
##### Process for patching missing previous series (before 1998 for active pop and unemp) is in 20230621_Finland_Data.R
finland_data_unemp <- read_csv("Data/finland.data.csv")%>%
  select(-"...1",-active_pop_oecd)%>%
  rename(unemployement_level_sa=unemployment_level_oecd)

finland_data_vac <- read_csv("Data/finland.data.csv")%>%
  select(-"...1",-unemployment_level_oecd)%>%
  mutate(active_pop_STSA=active_pop_oecd/1000, .keep = "unused")

names(OECD_vac_q_factored.dta)

OECD_labour_q_full.dta<- full_join(OECD_labour_q_full1.dta,finland_data_vac, by=c("date","country"))%>%
  mutate(active_pop_STSA = ifelse(country == "Finland" & !is.na(active_pop_STSA.x) , active_pop_STSA.x,active_pop_STSA.y))%>%
  mutate(active_pop_STSA = ifelse(country != "Finland" , active_pop_STSA.x, active_pop_STSA))%>%
  mutate(diff = active_pop_STSA.x-active_pop_STSA.y)%>%
  select(-active_pop_STSA.x,-active_pop_STSA.y,-diff)

beveridge_data_1<- full_join(OECD_vac_q_factored_SE.dta,finland_data_unemp, by=c("date","country"))%>%
  mutate(unemployement_level_sa = ifelse(country == "Finland" & !is.na(unemployement_level_sa.x) , unemployement_level_sa.x,unemployement_level_sa.y))%>%
  mutate(unemployement_level_sa = ifelse(country != "Finland" , unemployement_level_sa.x, unemployement_level_sa))%>%
  mutate(diff = unemployement_level_sa.x-unemployement_level_sa.y)%>%
  select(-unemployement_level_sa.x,-unemployement_level_sa.y,-diff)
  
names(beveridge_data_1) 
rm(OECD_labour_q_full1.dta,finland_data_vac,OECD_vac_q_factored.dta,finland_data_unemp,OECD_vac_q_factored_SE.dta)
####################################### 9) FINAL DATA DRAFT 1 #######################################
########## 9.1) MERGE UNEMPLOYMENT AND VACANCY ##########
beveridge_data_2 <- full_join(OECD_unemp.dta,beveridge_data_1, by =c("country", "date"))%>%
  select(country, date, everything())%>%
  mutate(unemployment_rate =ifelse(is.na(unemployement_rate_sa_vac), 
                                   unemployement_rate_sa, unemployement_rate_sa_vac))%>%
  select(-unemployement_rate_sa_vac,-unemployement_rate_sa)

rm(OECD_unemp.dta,beveridge_data_1)
########## 9.2) MERGE WORKING POPULATION AND CONTRUCT VACANCY RATE ########## 
names(beveridge_data_2)
beveridge_data <- full_join(beveridge_data_2,OECD_labour_q_full.dta, by =c("country", "date"))%>%
  mutate(active_pop_STSA = active_pop_STSA*1000)%>%
  select(country, date, everything(),-unemployment_rate)%>%
  mutate(vacancy_rate_oecd = vacancies_stock_sa/active_pop_STSA*100,
         unemployment_rate = unemployement_level_sa/active_pop_STSA*100,
         vacancy_rate_v1 = vacancies_stock_oecd_v1lm/active_pop_STSA*100,
         vacancy_rate_v2 = vacancies_stock_oecd_v2lm/active_pop_STSA*100,
         vacancy_rate_v3 = vacancies_stock_oecd_v3lm/active_pop_STSA*100)%>%
  filter(date > "1968-10-01")

rm(OECD_labour_q_full.dta,beveridge_data_2)

####################################### 10) Quarterly Data plus US #######################################
unique(as.character(beveridge_data$country)) 
names(beveridge_data)

Final_LMS_data1 <-beveridge_data%>%
  filter(country != "United States")

# Data from Michaillat and Saez (2022) 
US_quarter <- read_csv("Data/US_quarter_13032023.csv")%>%
  select(-"...1")%>%
  mutate(vacancy_rate_oecd =vacancy_rate, vacancy_rate_v1 =vacancy_rate, vacancy_rate_v2=vacancy_rate
         , vacancy_rate_v3=vacancy_rate)%>%
  select(-vacancy_rate)%>%
  rename(active_pop_STSA=active_pop_oecd)%>%
  filter(date > "1968-10-01")

names(Final_LMS_data1)
names(US_quarter)

######### Adding Q2-4 2022 for US - THS and rate for UNEMP from BLS ######### 
US_SA_ActivePop <- read_excel("Data/SeriesReport-20240108_US_SA_ActivePop.xlsx")%>%
  gather(Month, active_pop_STSA,-Year)%>%
  mutate(active_pop_STSA = active_pop_STSA*1000)%>%
  mutate(Quarter = ifelse(Month %in% c("Jan", "Mar","Feb"),"01",
                          ifelse(Month %in% c("Apr","May","Jun"),"04", 
                                 ifelse(Month %in% c("Jul","Aug","Sep"),"07","10"))))%>%
  select(-Month)%>%
  group_by(Quarter,Year)%>%
  summarise_if(is.numeric, mean)%>%
  ungroup()%>%
  filter(Year == 2022)

US_SA_UNEMP<- read_excel("Data/SeriesReport-20240108_US_SA_Unemprate.xlsx")%>%
  gather(Month, unemployment_rate,-Year)%>%
  mutate(Quarter = ifelse(Month %in% c("Jan", "Mar","Feb"),"01",
                          ifelse(Month %in% c("Apr","May","Jun"),"04", 
                                 ifelse(Month %in% c("Jul","Aug","Sep"),"07","10"))))%>%
  select(-Month)%>%
  group_by(Quarter,Year)%>%
  summarise_if(is.numeric, mean)%>%
  ungroup()%>%
  filter(Year == 2022)

US_SA_JOB <- read_excel("Data/SeriesReport-20240108_US_SA_Jobopenings.xlsx")%>%
  gather(Month, vacancy_stock,-Year)%>%
  mutate(vacancy_stock = vacancy_stock*1000)%>%
  mutate(Quarter = ifelse(Month %in% c("Jan", "Mar","Feb"),"01",
                          ifelse(Month %in% c("Apr","May","Jun"),"04", 
                                 ifelse(Month %in% c("Jul","Aug","Sep"),"07","10"))))%>%
  select(-Month)%>%
  group_by(Quarter,Year)%>%
  summarise_if(is.numeric, mean)%>%
  ungroup()%>%
  filter(Year == 2022)

US2022_1 <- full_join(US_SA_JOB,US_SA_UNEMP, by =c("Year", "Quarter"))
US2022 <- full_join(US2022_1,US_SA_ActivePop, by =c("Year", "Quarter"))%>%
  filter(Quarter != "01")%>%
  mutate(country = "United States")%>%
  mutate(vacancy_rate_v1 = (vacancy_stock/active_pop_STSA )*100)%>%
  mutate(vacancy_rate_v2 = vacancy_rate_v1, 
         vacancy_rate_v3 = vacancy_rate_v1, vacancy_rate_oecd = vacancy_rate_v1)%>%
  select(-vacancy_stock)%>%
  unite(date, Year, Quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

US2022_annual <- full_join(US2022_1,US_SA_ActivePop, by =c("Year", "Quarter"))%>%
  group_by(Year)%>%
  summarise_if(is.numeric, mean)%>%
  ungroup()%>%
  mutate(vacancy_rate_lm_v1 = (vacancy_stock/active_pop_STSA )*100)%>%
  mutate(vacancy_rate_lm_v2 = vacancy_rate_lm_v1, vacancy_rate_lm_v3 = vacancy_rate_lm_v1, 
         vacancy_rate_oecd = vacancy_rate_lm_v1)%>%
  select(-vacancy_stock)%>%
  rename(unemployment_rate_oecd =unemployment_rate,
         active_pop_oecd=active_pop_STSA,
         year =Year)%>%
  mutate(country = "United States")

names(US2022)
names(US_quarter)

US_quarter_final<-bind_rows(US_quarter,US2022)

rm(US_SA_JOB,US_SA_UNEMP,US_quarter,US2022,US_SA_ActivePop,US2022_1)
 
######### CONT WITH FINAL QUARTERLY DATA ######### 
Final_LMS_data_US<-bind_rows(US_quarter_final,Final_LMS_data1)%>%
  arrange(country,date)%>%
  select(country, date,unemployement_level_sa, active_pop_STSA,vacancies_stock_sa,
         vacancies_stock_oecd_v1lm,vacancies_stock_oecd_v2lm,vacancies_stock_oecd_v3lm,unemployment_rate,              
         vacancy_rate_oecd,vacancy_rate_v1,vacancy_rate_v2,vacancy_rate_v3)

Final_LMS_data_Final <-full_join(Final_LMS_data_US,Eurostat_adjusted_final, by=c("country","date"))%>%
  rename(unemployment_level_oecd=unemployement_level_sa,
         active_pop_oecd=active_pop_STSA,
         vacancy_stock_oecd=vacancies_stock_sa, 
         unemployment_rate_oecd =unemployment_rate, 
         vacancy_stock_lm_v1=vacancies_stock_oecd_v1lm, 
         vacancy_stock_lm_v2=vacancies_stock_oecd_v2lm, 
         vacancy_stock_lm_v3=vacancies_stock_oecd_v3lm, 
         vacancy_rate_lm_v1 = vacancy_rate_v1,
         vacancy_rate_lm_v2 = vacancy_rate_v2,
         vacancy_rate_lm_v3 = vacancy_rate_v3)%>%
  mutate(becru_lm_v1 = sqrt(unemployment_rate_oecd*vacancy_rate_lm_v1),
         fegap_lm_v1 =unemployment_rate_oecd- becru_lm_v1,
         becru_lm_v2 = sqrt(unemployment_rate_oecd*vacancy_rate_lm_v2),
         fegap_lm_v2 =unemployment_rate_oecd- becru_lm_v2,
         becru_lm_v3 = sqrt(unemployment_rate_oecd*vacancy_rate_lm_v3),
         fegap_lm_v3 =unemployment_rate_oecd- becru_lm_v3)%>%
  filter(date <"2023-01-01" & date > "1969-10-01")

### Change the filtering for the date here if growth rates for 1970 is needed in annual data 

names(Final_LMS_data_Final)
write.csv(Final_LMS_data_Final, "LMS_quarterly_data.csv") 

rm(beveridge_data,Eurostat_adjusted_final,Final_LMS_data_US,Final_LMS_data1)
####################################### 11) Annual Data plus US  #######################################

names(Final_LMS_data_Final)
Final_LMS_data_annual <-Final_LMS_data_Final%>%
  separate(date, c("year", "date"), sep = "\\-")%>%
  group_by(year,country)%>%
  summarise_if(is.numeric, mean)%>% 
  ungroup()%>% 
  mutate(year =as.numeric(year))%>%
  filter(country != "United States")%>%
  select(-contains("fegap"),-contains("becru"))

# Data from Michaillat and Saez (2022) 
US_annual_13032023 <- read_csv("Data/US_annual_13032023.csv")%>%
  select(-"...1")%>%
  mutate(vacancy_rate_oecd =vacancy_rate, vacancy_rate_lm_v1 =vacancy_rate, vacancy_rate_lm_v2=vacancy_rate,vacancy_rate_lm_v3 =vacancy_rate,unemployment_rate_oecd =unemployment_rate)%>%
  select(-vacancy_rate,-unemployment_rate)%>%
  filter(year !=2022)

### Add 2022 for US from BLS
names(US_annual_13032023)
names(US2022_annual)

LMS_annual_data_final<-bind_rows(US2022_annual,US_annual_13032023)
  
LMS_annual_data<-bind_rows(LMS_annual_data_final,Final_LMS_data_annual)%>%
  mutate(becru_lm_v1 = sqrt(unemployment_rate_oecd*vacancy_rate_lm_v1),
         fegap_lm_v1 =unemployment_rate_oecd- becru_lm_v1,
         becru_lm_v2 = sqrt(unemployment_rate_oecd*vacancy_rate_lm_v2),
         fegap_lm_v2 =unemployment_rate_oecd- becru_lm_v2,
         becru_lm_v3 = sqrt(unemployment_rate_oecd*vacancy_rate_lm_v3),
         fegap_lm_v3 =unemployment_rate_oecd- becru_lm_v3)%>%
  arrange(country,year)%>%
  group_by(country)%>%
  mutate(active_pop_growth_oecd = (active_pop_oecd-lag(active_pop_oecd))/lag(active_pop_oecd)*100)%>%
  ungroup()%>%
  filter(year > 1968 & year <2023)%>%
  select(country,year, active_pop_oecd, active_pop_growth_oecd, everything())
  
names(LMS_annual_data)


write.csv(LMS_annual_data, "LMS_annual_data.csv") 

names(Final_LMS_data_annual)
rm(US_annual_13032023,Final_LMS_data_annual)

####################################### 12) Business Cycles #######################################
# OECDE = OECD Europe, EA19 = Euro Area 19
# percentage change from the previous quarter
OECD_Quarterly_GDP <- read_csv("Data/OECD_Quarterly_GDP_20230314.csv")%>%
  filter(FREQUENCY =="Q", MEASURE =="PC_CHGPP")%>%
  select(-FREQUENCY,-"Flag Codes",-INDICATOR,-SUBJECT,-MEASURE)%>%
  mutate(country =countrycode(sourcevar = LOCATION, "iso3c", "country.name"))%>%
  mutate(country = ifelse(is.na(country), LOCATION, country))%>%
  filter(country %in% c("Germany","Latvia", "Czechia" ,"United Kingdom" ,"Poland","Estonia","Denmark","Spain","OECD", 
                        "Hungary","France","Slovenia","Finland","Netherlands" ,"United States" , "Portugal","Slovakia",
                        "Luxembourg" ,"Sweden" ,"Italy"  ,"Belgium","Ireland","Austria"  ,"Greece","OECDE" ,"EA19",
                        "Lithuania","Bulgaria","Romania" ,"EU27_2020","Croatia"))%>%
  select(-LOCATION)%>%
  arrange(country,TIME)%>%
  group_by(country)%>%
  mutate(negative = ifelse(Value<0, TIME, NA))%>%
  mutate(var = ifelse(!is.na(negative) & !is.na(lead(negative)) &  is.na(lag(negative)) ,"xmin",NA ))%>%
  mutate(var = ifelse(!is.na(negative) & is.na(lead(negative)) &  !is.na(lag(negative)) ,"xmax",var ))%>%
  filter(!is.na(negative) & !is.na(var))%>%
  spread(var,negative)%>%
  arrange(country,TIME)%>%
  select(country,xmin,xmax)%>%
  summarise(across(everything(), ~ na.omit(.x)))%>%
  ungroup()%>%
  separate(xmin, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(xmin, year, quarter, sep = '-')%>%
  mutate(xmin = as.Date(paste(xmin, "-01", sep=''), "%Y-%m-%d"))%>%
  separate(xmax, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(xmax, year, quarter, sep = '-')%>%
  mutate(xmax = as.Date(paste(xmax, "-01", sep=''), "%Y-%m-%d"))


write.csv(OECD_Quarterly_GDP, "Data/OECD_Business_Cycles_13032023.csv") 


