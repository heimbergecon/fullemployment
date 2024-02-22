#############################################################################################################
################################# LABOUR MARKET SLACK EUROSTAT DATA AND FIGURES  ############################
#############################################################################################################
rm(list = ls())
pacman::p_load(rstudioapi,stats, ggplot2, tidyverse, dplyr,utils,ggpubr,xts,foreign,base,readxl,seasonal,xlsx,pastecs,zoo,eurostat,ggrepel,lubridate,countrycode,directlabels,scales,lme4)
current_path <- getActiveDocumentContext()$path #find the current path of the script
setwd(dirname(current_path )) 
getwd()

var_list <-c("Job vacancies, Total, Unfilled vacancies (stock)","Job vacancies, Total, Unfilled vacancies (stock), sa",        
             "Registered unemployment, Level, Total","Registered unemployment, Level, Total, sa",                    
             "Registered unemployment, Rate, Total","Registered unemployment, Rate, Total, sa")


country_list_euro_eurostat <-c( "Austria","Germany","Spain","United Kingdom","Sweden","Belgium", 
                                "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,
                                "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia",
                                "Cyprus","Greece","Malta")

eurozone <-c( "Austria","Belgium","Croatia","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland",
              "Italy","Latvia","Lithuania","Luxembourg", "Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")

european_union <-c( "Austria","Belgium","Bulgaria" ,"Cyprus","Czechia" , "France","Germany" , "Estonia","Greece", "Spain" , 
                    "Finland" , "Croatia", "Hungary", "Ireland","Italy","Lithuania", "Luxembourg","Latvia", "Malta","Denmark",
                    "Netherlands"  ,"Poland","Portugal","Romania","Sweden","Slovenia","Slovakia" ,"United Kingdom" )

country_list_euro_eurostat_full <-c("Austria","Germany","Spain","United Kingdom","Sweden","Belgium", 
                                "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,
                                "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia",
                                "Cyprus","Greece","Malta", "EA","EA19","EA20","EU" ,"EU27_2007","EU27_2020","EU28" )

country_list_LMS <-c( "Austria","Germany","United Kingdom","Sweden","United States" )

####################################### !!! DATA CONSTRUCTION !!!  ##########################################################
####################################### 1) EUROSTAT VACANCY STOCK ##########################################################
#Source: https://ec.europa.eu/eurostat/databrowser/view/JVS_Q_NACE2/default/table?lang=en&category=labour.jvs
Eurostat_fullset_vacancy1 <- read_csv("Data/jvs_q_nace2_linear.csv")%>%
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG)%>%
  spread(s_adj, OBS_VALUE)%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(country %in% european_union, indic_em =="JOBVAC",sizeclas =="TOTAL",nace_r2 =="B-S")%>%
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
  filter(country %in% european_union, indic_em =="JOBVAC", sizeclas =="TOTAL",nace_r1 =="TOTAL")%>%
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

unique(as.character(Eurostat_fullset_vacancy$country))  
rm(Eurostat_fullset_vacancy1,Eurostat_fullset_vacancy2)

########## 1.1) SEASONALITY ADJUSTMENT - EUROSTAT ##########
for (variable in unique(Eurostat_fullset_vacancy$country)) {
  assign(variable, Eurostat_fullset_vacancy %>% filter(country == variable, !is.na(vacancy_stock_eurostat))%>%
           select(-country)%>%
           arrange(date), envir = .GlobalEnv)
}

Austria<-Austria%>%
  filter(date >="2010-01-01")
Estonia<-Estonia%>%
  filter(date <"2020-01-01")

rm(Italy)

Eurostat_ts_at<- ts(Austria[-1], start = c(2010, 1), freq = 4)
Eurostat_ts_be<- ts(Belgium[-1], start = c(2012, 1), freq = 4)
Eurostat_ts_bg<- ts(Bulgaria[-1], start = c(2005, 1), freq = 4)
Eurostat_ts_hr<- ts(Croatia[-1], start = c(2012, 1), freq = 4)
Eurostat_ts_cy<- ts(Cyprus[-1], start = c(2005, 1), freq = 4)
Eurostat_ts_cz<- ts(Czechia[-1], start = c(2005, 1), freq = 4)
Eurostat_ts_ee<- ts(Estonia[-1], start = c(2005, 1), freq = 4)
Eurostat_ts_fi<- ts(Finland[-1], start = c(2002, 1), freq = 4)
Eurostat_ts_de<- ts(Germany[-1], start = c(2006, 1), freq = 4)
Eurostat_ts_el<- ts(Greece[-1], start = c(2002, 4), freq = 4)
Eurostat_ts_hu<- ts(Hungary[-1], start = c(2006, 1), freq = 4)
Eurostat_ts_ie<- ts(Ireland[-1], start = c(2008, 1), freq = 4)
Eurostat_ts_lv<- ts(Latvia[-1], start = c(2005, 1), freq = 4)
Eurostat_ts_lt<- ts(Lithuania[-1], start = c(2002, 1), freq = 4)
Eurostat_ts_lu<- ts(Luxembourg[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_mt<- ts(Malta[-1], start = c(2017, 1), freq = 4)
Eurostat_ts_nl<- ts(Netherlands[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_po<- ts(Poland[-1], start = c(2010, 1), freq = 4)
Eurostat_ts_pt<- ts(Portugal[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_ro<- ts(Romania[-1], start = c(2005, 1), freq = 4)
Eurostat_ts_sk<- ts(Slovakia[-1], start = c(2004, 1), freq = 4)
Eurostat_ts_sl<- ts(Slovenia[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_se<- ts(Sweden[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_es<- ts(Spain[-1], start = c(2001, 1), freq = 4)
Eurostat_ts_uk<- ts(`United Kingdom`[-1], start = c(2001, 2), freq = 4)

for (name in unique(Eurostat_fullset_vacancy$country)) rm(list = name)

Eurostat_ts_adjusted_at <- seas(Eurostat_ts_at)
Eurostat_ts_adjusted_de <- seas(Eurostat_ts_de)
Eurostat_ts_adjusted_se <- seas(Eurostat_ts_se)
Eurostat_ts_adjusted_es <- seas(Eurostat_ts_es)
Eurostat_ts_adjusted_uk <- seas(Eurostat_ts_uk)
Eurostat_ts_adjusted_be <- seas(Eurostat_ts_be)
Eurostat_ts_adjusted_bg <- seas(Eurostat_ts_bg)
Eurostat_ts_adjusted_hr <- seas(Eurostat_ts_hr)
Eurostat_ts_adjusted_cy <- seas(Eurostat_ts_cy)
Eurostat_ts_adjusted_cz <- seas(Eurostat_ts_cz)
Eurostat_ts_adjusted_ee <- seas(Eurostat_ts_ee)
Eurostat_ts_adjusted_fi <- seas(Eurostat_ts_fi)
Eurostat_ts_adjusted_el <- seas(Eurostat_ts_el)
Eurostat_ts_adjusted_hu <- seas(Eurostat_ts_hu)
Eurostat_ts_adjusted_ie <- seas(Eurostat_ts_ie)
Eurostat_ts_adjusted_lv <- seas(Eurostat_ts_lv)
Eurostat_ts_adjusted_lt <- seas(Eurostat_ts_lt)
Eurostat_ts_adjusted_lu <- seas(Eurostat_ts_lu)
Eurostat_ts_adjusted_mt <- seas(Eurostat_ts_mt)
Eurostat_ts_adjusted_nl <- seas(Eurostat_ts_nl)
Eurostat_ts_adjusted_po <- seas(Eurostat_ts_po)
Eurostat_ts_adjusted_ro <- seas(Eurostat_ts_ro)
Eurostat_ts_adjusted_sk <- seas(Eurostat_ts_sk)
Eurostat_ts_adjusted_sl <- seas(Eurostat_ts_sl)
Eurostat_ts_adjusted_pt <- seas(Eurostat_ts_pt)

plot(Eurostat_ts_adjusted_lu)

df_at <- as.xts(final(Eurostat_ts_adjusted_at))
df_de<- as.xts(final(Eurostat_ts_adjusted_de))
df_se<- as.xts(final(Eurostat_ts_adjusted_se))
df_es<- as.xts(final(Eurostat_ts_adjusted_es))
df_uk<- as.xts(final(Eurostat_ts_adjusted_uk))
df_be<- as.xts(final(Eurostat_ts_adjusted_be))
df_bg<- as.xts(final(Eurostat_ts_adjusted_bg))
df_hr<- as.xts(final(Eurostat_ts_adjusted_hr))
df_cy<- as.xts(final(Eurostat_ts_adjusted_cy))
df_cz<- as.xts(final(Eurostat_ts_adjusted_cz))
df_ee<- as.xts(final(Eurostat_ts_adjusted_ee))
df_fi<- as.xts(final(Eurostat_ts_adjusted_fi))
df_el<- as.xts(final(Eurostat_ts_adjusted_el))
df_hu<- as.xts(final(Eurostat_ts_adjusted_hu))
df_ie<- as.xts(final(Eurostat_ts_adjusted_ie))
df_lv<- as.xts(final(Eurostat_ts_adjusted_lv))
df_lt<- as.xts(final(Eurostat_ts_adjusted_lt))
df_lu<- as.xts(final(Eurostat_ts_adjusted_lu))
df_mt<- as.xts(final(Eurostat_ts_adjusted_mt))
df_nl<- as.xts(final(Eurostat_ts_adjusted_nl))
df_po<- as.xts(final(Eurostat_ts_adjusted_po))
df_ro<- as.xts(final(Eurostat_ts_adjusted_ro))
df_sk<- as.xts(final(Eurostat_ts_adjusted_sk))
df_sl<- as.xts(final(Eurostat_ts_adjusted_sl))
df_pt<- as.xts(final(Eurostat_ts_adjusted_pt))

df_final_at <- data.frame(Date = index(df_at), coredata(df_at) )%>%
  mutate(country="Austria")%>%
  rename(vacancy_stock_eurostat=coredata.df_at.)
df_final_de <- data.frame(Date = index(df_de), coredata(df_de) )%>%
  mutate(country="Germany")%>%
  rename(vacancy_stock_eurostat=coredata.df_de.)
df_final_se <- data.frame(Date = index(df_se), coredata(df_se) )%>%
  mutate(country="Sweden")%>%
  rename(vacancy_stock_eurostat=coredata.df_se.)
df_final_es <- data.frame(Date = index(df_es), coredata(df_es) )%>%
  mutate(country="Spain")%>%
  rename(vacancy_stock_eurostat=coredata.df_es.)
df_final_uk <- data.frame(Date = index(df_uk), coredata(df_uk) )%>%
  mutate(country="United Kingdom")%>%
  rename(vacancy_stock_eurostat=coredata.df_uk.)
df_final_be<- data.frame(Date = index(df_be), coredata(df_be) )%>%
  mutate(country="Belgium")%>%
  rename(vacancy_stock_eurostat=coredata.df_be.)
df_final_bg <- data.frame(Date = index(df_bg), coredata(df_bg) )%>%
  mutate(country="Bulgaria")%>%
  rename(vacancy_stock_eurostat=coredata.df_bg.)
df_final_hr <- data.frame(Date = index(df_hr), coredata(df_hr) )%>%
  mutate(country="Croatia")%>%
  rename(vacancy_stock_eurostat=coredata.df_hr.)
df_final_cy <- data.frame(Date = index(df_cy), coredata(df_cy) )%>%
  mutate(country="Cyprus")%>%
  rename(vacancy_stock_eurostat=coredata.df_cy.)
df_final_cz <- data.frame(Date = index(df_cz), coredata(df_cz) )%>%
  mutate(country="Czechia")%>%
  rename(vacancy_stock_eurostat=coredata.df_cz.)
df_final_ee <- data.frame(Date = index(df_ee), coredata(df_ee) )%>%
  mutate(country="Estonia")%>%
  rename(vacancy_stock_eurostat=coredata.df_ee.)
df_final_fi <- data.frame(Date = index(df_fi), coredata(df_fi) )%>%
  mutate(country="Finland")%>%
  rename(vacancy_stock_eurostat=coredata.df_fi.)
df_final_el <- data.frame(Date = index(df_el), coredata(df_el) )%>%
  mutate(country="Greece")%>%
  rename(vacancy_stock_eurostat=coredata.df_el.)
df_final_hu <- data.frame(Date = index(df_hu), coredata(df_hu) )%>%
  mutate(country="Hungary")%>%
  rename(vacancy_stock_eurostat=coredata.df_hu.)
df_final_ie <- data.frame(Date = index(df_ie), coredata(df_ie) )%>%
  mutate(country="Ireland")%>%
  rename(vacancy_stock_eurostat=coredata.df_ie.)
df_final_lv <- data.frame(Date = index(df_lv), coredata(df_lv) )%>%
  mutate(country="Latvia")%>%
  rename(vacancy_stock_eurostat=coredata.df_lv.)
df_final_lt <- data.frame(Date = index(df_lt), coredata(df_lt) )%>%
  mutate(country="Lithuania")%>%
  rename(vacancy_stock_eurostat=coredata.df_lt.)
df_final_lu <- data.frame(Date = index(df_lu), coredata(df_lu) )%>%
  mutate(country="Luxembourg")%>%
  rename(vacancy_stock_eurostat=coredata.df_lu.)
df_final_mt <- data.frame(Date = index(df_mt), coredata(df_mt) )%>%
  mutate(country="Malta")%>%
  rename(vacancy_stock_eurostat=coredata.df_mt.)
df_final_nl <- data.frame(Date = index(df_nl), coredata(df_nl) )%>%
  mutate(country="Netherlands")%>%
  rename(vacancy_stock_eurostat=coredata.df_nl.)
df_final_po <- data.frame(Date = index(df_po), coredata(df_po) )%>%
  mutate(country="Poland")%>%
  rename(vacancy_stock_eurostat=coredata.df_po.)
df_final_ro <- data.frame(Date = index(df_ro), coredata(df_ro) )%>%
  mutate(country="Romania")%>%
  rename(vacancy_stock_eurostat=coredata.df_ro.)
df_final_sk <- data.frame(Date = index(df_sk), coredata(df_sk) )%>%
  mutate(country="Slovakia")%>%
  rename(vacancy_stock_eurostat=coredata.df_sk.)
df_final_sl <- data.frame(Date = index(df_sl), coredata(df_sl) )%>%
  mutate(country="Slovenia")%>%
  rename(vacancy_stock_eurostat=coredata.df_sl.)
df_final_pt <- data.frame(Date = index(df_pt), coredata(df_pt) )%>%
  mutate(country="Portugal")%>%
  rename(vacancy_stock_eurostat=coredata.df_pt.)

Pattern1<-grep("df_final_",names(.GlobalEnv),value=TRUE)
Pattern1_list<-do.call("list",mget(Pattern1))

df <- do.call("rbind", Pattern1_list)

rm(list = ls(pattern = "df_final_"))
rm(list = ls(pattern = "df_"))
rm(list = ls(pattern = "Eurostat_ts_"))
rm(Pattern1,Pattern1_list)

Eurostat_adjusted_final <- df%>%
  separate(Date, c("year", "quarter"), sep = "\\ ")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))

rm(Eurostat_fullset_vacancy,df)

########## 1.2) FRANCE VACANCY STOCK ########## 
# https://dares.travail-emploi.gouv.fr/donnees/les-emplois-vacants (survey)
# https://dares.travail-emploi.gouv.fr/dossier/les-demandeurs-demploi (registered)
France_vacancies <- read_excel("Data/France_vacancies.xlsx")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q4","04", ifelse(quarter == "Q7","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  mutate(country = "France")%>%
  mutate(france_vacancy_registered=france_vacancy_registered*1000,
         france_vacancy_sa_registered=france_vacancy_sa_registered*1000)%>%
  mutate(diff=france_vacancy_registered-france_vacancy_survey, 
         diff_sa=france_vacancy_sa_registered-france_vacancy_sa_survey)%>%
  mutate(vacancy_stock_eurostat=france_vacancy_sa_registered)%>%
  select(country,date,vacancy_stock_eurostat)

names(France_vacancies)
unique(as.character(France_vacancies$date))  

Eurostat_adjusted_stock<-rbind(France_vacancies,Eurostat_adjusted_final)

rm(France_vacancies,Eurostat_adjusted_final)

####################################### 2) EUROSTAT UNEMPLOYMENT RATE ##########################################################
### Historical series for UK
Eurostat_fullset_unemployment1 <- read_csv("Data/une_rt_q_h_linear2023Dec.csv")%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(country %in% european_union,unit == "PC_ACT", age == "Y15-74", sex =="T",s_adj=="SA") %>% # per ACT (same as active pop)
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -age,-unit,-sex)%>%
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
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(country %in% european_union,unit == "PC_ACT", age == "Y15-74", sex =="T",s_adj=="SA") %>% # per ACT (same as active pop)
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -age,-unit,-sex)%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(unemployment_rate_eurostat2 = OBS_VALUE)%>%
  select(country,date,unemployment_rate_eurostat2)

names(Eurostat_fullset_unemployment2)
unique(as.character(Eurostat_fullset_unemployment2$country)) 

Eurostat_fullset_unemployment<-full_join(Eurostat_fullset_unemployment1,Eurostat_fullset_unemployment2, by= c("date","country"))%>%
  mutate(unemployment_rate_eurostat = ifelse(!is.na(unemployment_rate_eurostat2),unemployment_rate_eurostat2,unemployment_rate_eurostat1))%>%
  select(-unemployment_rate_eurostat2,-unemployment_rate_eurostat1)

unique(as.character(Eurostat_fullset_unemployment$country)) 

rm(Eurostat_fullset_unemployment1,Eurostat_fullset_unemployment2)

####################################### 3) EUROSTAT ACTIVE POPULATION (Thousand) ##########################################################
Eurostat_activepop_a <- read_csv("Data/lfsi_emp_q_h_linear_2023Dec.csv")%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(unit == "THS_PER",country %in% european_union, s_adj=="SA", sex =="T",
         age == "Y15-64")%>%
  spread(indic_em,OBS_VALUE)%>%
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -unit,-sex,-EMP_LFS,-age,-s_adj)%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  select(country,date,ACT)%>%
  rename(active_pop_a = ACT)

names(Eurostat_activepop_a)
unique(as.character(Eurostat_activepop_a$age))  

Eurostat_activepop_sa <- read_csv("Data/lfsi_emp_q_linear_2023Dec.csv")%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  filter(unit == "THS_PER",country %in% european_union, s_adj=="SA", sex =="T",
         age == "Y15-64")%>%
  spread(indic_em,OBS_VALUE)%>%
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -unit,-sex,-EMP_LFS,-age,-s_adj)%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  select(country,date,ACT)%>%
  rename(active_pop_sa = ACT)

names(Eurostat_activepop_sa)
unique(as.character(Eurostat_activepop_sa$indic_em))  

Eurostat_activepop<-full_join(Eurostat_activepop_a,Eurostat_activepop_sa, by= c("date","country"))%>%
  mutate(active_pop = ifelse(!is.na(active_pop_sa),active_pop_sa,active_pop_a))%>%
  select(-active_pop_a,-active_pop_sa)

rm(Eurostat_activepop_a,Eurostat_activepop_sa)
####################################### 4) EUROSTAT MERGE AND CREATE VACANCY RATE ##########################################################
names(Eurostat_activepop)
names(Eurostat_adjusted_stock)

Eurostat_vacancy_rate1<-full_join(Eurostat_adjusted_stock,Eurostat_activepop, by= c("date","country"))%>%
  mutate(vacancy_rate_eurostat = (vacancy_stock_eurostat/active_pop)/10)%>%
  filter(country != "Denmark", country!="United Kingdom")%>%
  filter(!(country =="France" & date <"2003-01-01"))%>%
  filter(!(country =="Luxembourg" & date <"2003-01-01"))#%>%
  #filter(!is.na(vacancy_rate_eurostat))

########## 4.1)ITALY VACANCY RATE ##########
Italy_Vacancy_rate <- read_csv("Data/DCSC_POSTIVAC_2_2023DEC.csv")%>%
  rename(country = Territory, vacancy_rate_eurostat_it=Value)%>%
  filter(`Employees class`=="10 and over", CORREZ =="Y",ATECO_2007=="0015")%>%
  separate(TIME, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  select(country,date ,vacancy_rate_eurostat_it)

names(Italy_Vacancy_rate)
unique(as.character(Italy_Vacancy_rate$ATECO_2007))  

Eurostat_vacancy_rate2<-full_join(Eurostat_vacancy_rate1,Italy_Vacancy_rate,by=c("country","date"))%>%
  mutate(vacancy_rate_eurostat = ifelse(country == "Italy",vacancy_rate_eurostat_it,vacancy_rate_eurostat))%>%
  filter(!is.na(vacancy_rate_eurostat))%>%
  select(-vacancy_rate_eurostat_it)%>%
  arrange(country, date)

rm(Italy_Vacancy_rate,Eurostat_vacancy_rate1,Eurostat_adjusted_stock,Eurostat_activepop)

########## 4.2)OECD-SAEZ DATA ##########
LMS_data <- read_csv("LMS_quarterly_data.csv")%>%
  select(-"...1")%>%
  filter(date >"1999-10-01")%>%
  select(country,date, vacancy_rate_lm_v1,unemployment_rate_oecd,active_pop_oecd)

Eurostat_vacancy_rate<-full_join(Eurostat_vacancy_rate2,LMS_data,by=c("country","date"))%>%
  mutate(region = ifelse(country %in% c("Austria", "Belgium", "France", "Germany", "Netherlands", "Luxembourg"),"Continental",
                         ifelse(country %in% c("Denmark", "Finland","Sweden"),"Nordic",
                                ifelse(country%in% c("Greece", "Italy", "Cyprus", "Portugal", "Spain", "Malta"),"Southern",
                                       ifelse(country%in% c("Czechia", "Bulgaria", "Estonia","Croatia", "Latvia", "Lithuania", 
                                                            "Hungary", "Poland", "Romania", "Slovenia", "Slovakia"),"Eastern","Anglo-American")))))

rm(Eurostat_vacancy_rate2,LMS_data)
####################################### 5) EUROSTAT FINAL MERGE  ##########################################################
Eurostat_df<-full_join(Eurostat_vacancy_rate,Eurostat_fullset_unemployment,by=c("country","date"))%>%
  filter(!is.na(region))%>%
  mutate(active_pop = ifelse(country %in% c("United States","United Kingdom"), active_pop_oecd,active_pop))%>%
  mutate(vacancy_rate = ifelse(country %in% c("United States","United Kingdom"), vacancy_rate_lm_v1,vacancy_rate_eurostat))%>%
  mutate(unemployment_rate = ifelse(country %in% c("United States","United Kingdom","Germany","Austria","Sweden","Finland"), unemployment_rate_oecd,unemployment_rate_eurostat))%>%
  group_by(country)%>%
  mutate(active_pop = ifelse(country %in% c("United States","United Kingdom"),active_pop, (active_pop*1000)))%>%
  mutate(active_pop_growth = (active_pop-lag(active_pop))/lag(active_pop)*100)%>%
  ungroup()%>%
  select(date,country,region,unemployment_rate,vacancy_rate,active_pop,active_pop_growth,vacancy_stock_eurostat )%>%
  filter(date <"2023-01-01")

names(Eurostat_df)

write.csv(Eurostat_df, "Eurostat_df_quarterly.csv")

unique(as.character(Eurostat_df$country))  

Eurostat_df <- read_csv("Eurostat_df_quarterly.csv")%>%
  select(-"...1")

###### Annual Data ######
names(Eurostat_df)

Eurostat_annual_df <-Eurostat_df%>%
  separate(date, c("year", "date"), sep = "\\-")%>%
  group_by(country,year)%>%
  summarise_if(is.numeric, mean)%>%
  ungroup()%>%
  filter(year <2023)

write.csv(Eurostat_annual_df, "Eurostat_df_annual.csv")



####################################### 6) POPULATION DATA - WEIGHTS  ##########################################################
POPULATION_OECD <- read_csv("Data/POPULATION_OECD_QNA_24042023115316468.csv")%>%
  filter(!Country %in% c("Denmark","Luxembourg", "Czech Republic", "Croatia"),
         MEASURE =="PERSA")%>%
  select(Country,TIME,Value)%>%
  separate(TIME, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(population = Value, country = Country)%>%
  mutate(region = ifelse(country %in% c("Austria", "Belgium", "France", "Germany", "Netherlands", "Luxembourg"),"Continental",
                         ifelse(country %in% c("Denmark", "Finland","Sweden"),"Nordic",
                                ifelse(country%in% c("Greece", "Italy", "Cyprus", "Portugal", "Spain", "Malta"),"Southern",
                                       ifelse(country%in% c("Czech Republic", "Bulgaria", "Estonia","Croatia", "Latvia", "Lithuania", "Hungary", "Poland", "Romania", "Slovenia", "Slovak Republic"),"Eastern","Anglo-American")))))%>%
  group_by(region,date) %>%
  filter( date > "1999-10-01")

unique(as.character(POPULATION_OECD$country))  

Eurostat_population <- read_csv("Data/naidq_10_pe__custom_5958119_linear.csv")%>%
  filter(geo %in% c("MT","CY"),s_adj =="NSA" )%>%
  select(-DATAFLOW,-"LAST UPDATE",-freq,-OBS_FLAG, -unit,-na_item,-s_adj)%>%
  mutate(country =countrycode(sourcevar = geo, "eurostat", "country.name"))%>%
  mutate(country = ifelse(is.na(country), geo,country))%>%
  separate(TIME_PERIOD, c("year", "quarter"), sep = "\\-")%>%
  mutate(quarter = ifelse(quarter == "Q1","01",ifelse(quarter == "Q2","04", ifelse(quarter == "Q3","07","10"))))%>%
  unite(date, year, quarter, sep = '-')%>%
  mutate(date = as.Date(paste(date, "-01", sep=''), "%Y-%m-%d"))%>%
  rename(population =OBS_VALUE)%>%
  select(country,date,population)%>%
  mutate(region = "Southern")%>%
  filter( date > "1999-10-01")

population_weight <- rbind(POPULATION_OECD,Eurostat_population)%>%
  group_by(region)%>%
  mutate(region_total = sum(population)) %>%
  ungroup()%>%
  mutate(weight = population/region_total)%>%
  mutate(country =ifelse(country == "Slovak Republic", "Slovakia",country))%>%
  mutate(eu = ifelse(!country %in% c("United States", "United Kingdom"), "EU", 
                     ifelse(country  == "United States", "US","UK")))%>%
  mutate(euro = ifelse(country %in% eurozone, "Eurozone", 
                          ifelse(country  == "United States", "US",
                                 ifelse(country == "United Kingdom", "UK", "Non-Euro"))))%>%
  group_by(eu)%>%
  mutate(eu_total = sum(population)) %>%
  ungroup()%>%
  group_by(euro)%>%
  mutate(euro_total = sum(population)) %>%
  ungroup()%>%
  mutate(weight_euro = population/euro_total)%>%
  mutate(weight_eu = population/region_total)

unique(as.character(population_weight$country))  

rm(Eurostat_population,POPULATION_OECD)

########## 6.1)WEIGHTING ##########
#### REMOVING COUNTRIES WITH ADMIN DATA 
Eurostat_df_weighted<-full_join(Eurostat_df,population_weight,by=c("country","date","region"))%>%
  filter(!country %in% c("Luxembourg", "Czechia", "Croatia"))
unique(as.character(Eurostat_df_weighted$country))  
unique(as.character(Eurostat_df$country))  

rm(population_weight)

names(Eurostat_df_weighted)
Eurostat_df_agg <-Eurostat_df_weighted%>%
  select(date, country, region, unemployment_rate, vacancy_rate, weight)%>%
  group_by(region,date) %>% 
  summarise_at(vars(ends_with('_rate')), 
               funs(weighted.mean(., weight)), na.rm=T)%>%
  ungroup()%>%
  mutate(becru = sqrt(unemployment_rate*vacancy_rate)) %>%
  mutate(fegap =unemployment_rate- becru)


write.csv(Eurostat_df_agg, "Eurostat_df_agg.csv") 
####################################### 7) PLOTS  ##########################################################
####################################### !!! START HERE FOR FIGURES !!!  #######################################
Eurostat_df <- read_csv("Eurostat_df_quarterly.csv")%>%
  select(-"...1")

Eurostat_annual_df <- read_csv("Eurostat_df_annual.csv")%>%
  select(-"...1")

Eurostat_df_agg <- read_csv("Eurostat_df_agg_20240109.csv")%>%
  select(-"...1")%>%
  mutate(becru = sqrt(unemployment_rate*vacancy_rate)) %>%
  mutate(fegap =unemployment_rate- becru)

Eurostat_df_euro <-Eurostat_df_weighted%>%
  select(date, country, euro, unemployment_rate, vacancy_rate, weight_euro)%>%
  group_by(euro,date) %>% 
  summarise_at(vars(ends_with('_rate')), 
               funs(weighted.mean(., weight_euro)), na.rm=T)%>%
  ungroup()%>%
  mutate(becru = sqrt(unemployment_rate*vacancy_rate)) %>%
  mutate(fegap =unemployment_rate- becru)

Eurostat_df_eu <-Eurostat_df_weighted%>%
  select(date, country, eu, unemployment_rate, vacancy_rate, weight_eu)%>%
  group_by(eu,date) %>% 
  summarise_at(vars(ends_with('_rate')), 
               funs(weighted.mean(., weight_eu)), na.rm=T)%>%
  ungroup()%>%
  mutate(becru = sqrt(unemployment_rate*vacancy_rate)) %>%
  mutate(fegap =unemployment_rate- becru)

Eurostat_df_unweighted_euro <- Eurostat_df%>%
  select(-region)%>%
  filter(!country %in% c("Luxembourg", "Czechia", "Croatia"))%>%
  mutate(euro = ifelse(country %in% eurozone, "Eurozone", 
                     ifelse(country  == "United States", "US",
                            ifelse(country == "United Kingdom", "UK", "Non-Euro"))))%>%
  group_by(euro,date) %>% 
  summarise_if(is.numeric, mean, na.rm=T)%>%
  ungroup()%>%
  mutate(becru = sqrt(unemployment_rate*vacancy_rate)) %>%
  mutate(fegap =unemployment_rate- becru)%>%
  rename(becru_unweighted=becru, fegap_unweighted = fegap,
         unemployment_rate_unweighted = unemployment_rate,vacancy_rate_unweighted = vacancy_rate)


names(Eurostat_df_unweighted_euro)
names(Eurostat_df_agg)

Eurostat_merged_euro <- full_join(Eurostat_df_unweighted_euro,Eurostat_df_euro, by =c("euro","date"))%>%
  filter(date < "2023-01-01")%>%
  filter(euro %in% c("Eurozone", "US"))%>%
  select(euro,date, fegap, fegap_unweighted)%>%
  gather(var, values, -date, -euro)%>%
  unite(var_euro, var, euro)%>%
  spread(var_euro,values)%>%
  select(-fegap_unweighted_US)%>%
  gather(var, values, -date)%>%
  filter(date >"2002-10-01")
  
### OECDE and Individual Business Cycles
OECD_Business_Cycles_short<- read_csv("Data/OECD_Business_Cycles_13032023.csv")%>%
  select(-"...1")%>%
  filter(xmin >"1999-10-01")

########## 7.1) STATE OF THE LABOUR MARKET ##########
names(Eurostat_df_agg)
Eurostat_df_agg_v1 <-Eurostat_df_agg%>%
  select(region,date,unemployment_rate,vacancy_rate)%>%
  group_by(region)%>%
  na.omit()%>%
  ungroup()%>%
  gather(var, value, -region, -date)

Eurostat_df_agg_v2 <-Eurostat_df_agg%>%
  select(region,date,unemployment_rate,vacancy_rate)%>%
  group_by(region)%>%
  na.omit()%>%
  ungroup()%>%
  mutate( ymax = pmax(vacancy_rate, unemployment_rate),
          ymin = pmin(vacancy_rate, unemployment_rate),
          fill = vacancy_rate > unemployment_rate)%>%
  distinct()

Eurostat_df_agg_v2_na <-Eurostat_df_agg_v2%>%
  mutate(ymax1 = ifelse(vacancy_rate>unemployment_rate,vacancy_rate,NA))%>%
  mutate(ymax2 = ifelse(unemployment_rate>vacancy_rate,unemployment_rate,NA))%>%
  mutate(ymin1 = ifelse(vacancy_rate>unemployment_rate,unemployment_rate,NA))%>%
  mutate(ymin2 = ifelse(unemployment_rate>vacancy_rate,vacancy_rate,NA))%>%
  mutate(fill_T =ifelse(fill== T, T,NA),
         fill_F=ifelse(fill== F, F,NA))
  
### Anglo-American #####
AA<-ggplot() +
  geom_line(data = Eurostat_df_agg_v1%>%filter(region == "Anglo-American",date < "2023-01-01"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Eurostat_df_agg_v2_na%>%filter(region == "Anglo-American",date < "2023-01-01"), 
              aes(date, ymin = ymin1, ymax = ymax1, fill = fill, alpha = 0.1)) + 
  geom_ribbon(data = Eurostat_df_agg_v2_na%>%filter(region == "Anglo-American",date < "2023-01-01"), 
              aes(date, ymin = ymin2, ymax = ymax2, fill = fill, alpha = 0.1)) +
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Anglo-American") + 
  scale_y_continuous(limits = c(0, 21),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "4 years",date_labels ="%Y",limits = c(min(Eurostat_df_agg_v1$date), max(Eurostat_df_agg_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("2013-01-01"), y =10, label = "Unemployment rate", color = "unemployment_rate")) + 
  geom_text(aes(x = as.Date("2013-01-01"), y = 0.7, label = "Vacancy rate", color = "vacancy_rate")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2004-01-01"),y = 5),
                  color = "slateblue", label = "Inefficiently slack", nudge_x =0, nudge_y = 1.5) +
  geom_text_repel(aes(as.Date("2022-01-01"),y = 6),
                  color = "darkorange", label = "Inefficiently tight", nudge_x =-500, nudge_y = -5) 

AA

### Continental #####
CON<-ggplot() +
  geom_line(data = Eurostat_df_agg_v1%>%filter(region == "Continental",date < "2023-01-01"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Eurostat_df_agg_v2%>%filter(region == "Continental",date < "2023-01-01"), 
              aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Continental") + 
  scale_y_continuous(limits = c(0, 21),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "4 years",date_labels ="%Y",limits = c(min(Eurostat_df_agg_v1$date), max(Eurostat_df_agg_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("2013-01-01"), y =9.5, label = "Unemployment rate", color = "unemployment_rate")) + 
  geom_text(aes(x = as.Date("2013-01-01"), y = 0.7, label = "Vacancy rate", color = "vacancy_rate")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2004-01-01"),y = 9),
                  color = "slateblue", label = "Inefficiently slack", nudge_x =0, nudge_y = 2) 

CON

### Eastern #####
EAST<-ggplot() +
  geom_line(data = Eurostat_df_agg_v1%>%filter(region == "Eastern",date < "2023-01-01"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Eurostat_df_agg_v2%>%filter(region == "Eastern",date < "2023-01-01"), 
              aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Eastern") + 
  scale_y_continuous(limits = c(0, 21),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "4 years",date_labels ="%Y",limits = c(min(Eurostat_df_agg_v1$date), max(Eurostat_df_agg_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("2014-01-01"), y =12.5, label = "Unemployment rate", color = "unemployment_rate")) + 
  geom_text(aes(x = as.Date("2013-01-01"), y =0 , label = "Vacancy rate", color = "vacancy_rate")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2004-01-01"),y = 14),
                  color = "slateblue", label = "Inefficiently slack", nudge_x =0, nudge_y = 1) 

EAST

### Nordic #####
NORD<-ggplot() +
  geom_line(data = Eurostat_df_agg_v1%>%filter(region == "Nordic",date < "2023-01-01"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Eurostat_df_agg_v2%>%filter(region == "Nordic",date < "2023-01-01"), 
              aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Nordic") + 
  scale_y_continuous(limits = c(0, 21),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "4 years",date_labels ="%Y",limits = c(min(Eurostat_df_agg_v1$date), max(Eurostat_df_agg_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("2013-01-01"), y =9.5, label = "Unemployment rate", color = "unemployment_rate")) + 
  geom_text(aes(x = as.Date("2013-01-01"), y =0 , label = "Vacancy rate", color = "vacancy_rate")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2004-01-01"),y = 7),
                  color = "slateblue", label = "Inefficiently slack", nudge_x =0, nudge_y = 2) 

NORD

### Southern #####
SOUTH<-ggplot() +
  geom_line(data = Eurostat_df_agg_v1%>%filter(region == "Southern",date < "2023-01-01"),
            aes(x=date, y=value, group=var, color=var), size=1.5) + 
  geom_ribbon(data = Eurostat_df_agg_v2%>%filter(region == "Southern",date < "2023-01-01"), 
              aes(date, ymin = ymin, ymax = ymax, fill = fill, alpha = 0.1)) + 
  scale_alpha(guide = 'none')+
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))+
  theme_minimal()+
  labs(y = "Share of labour force", x= "",subtitle ="Southern") + 
  scale_y_continuous(limits = c(0, 21),labels = function(x) paste0(x, "%")) +
  theme(legend.position="top",legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_date( date_breaks = "4 years",date_labels ="%Y",limits = c(min(Eurostat_df_agg_v1$date), max(Eurostat_df_agg_v1$date))) +
  scale_fill_manual(values=c("slateblue", "darkorange"))+
  scale_color_manual(values=c("slateblue", "darkorange"))+
  geom_text(aes(x = as.Date("2013-01-01"), y =20, label = "Unemployment rate", color = "unemployment_rate")) + 
  geom_text(aes(x = as.Date("2013-01-01"), y =-0.3 , label = "Vacancy rate", color = "vacancy_rate")) + 
  guides(fill = "none", color = "none")+
  geom_text_repel(aes(as.Date("2003-01-01"),y = 10),
                  color = "slateblue", label = "Inefficiently slack", nudge_x =0, nudge_y = 2) 

SOUTH

#### MERGE AND EXPORT ####
png("Output/EUROSTAT Figure Labour Market State_weighted.png", width = 9*300, height = 9*300, res = 300)

combined_plot1 <- ggarrange(AA,NORD,CON,SOUTH,EAST,
                            nrow = 3,
                            ncol = 2)
combined_plot1

dev.off()

########## 7.2) BECRU ##########
names(Eurostat_df_agg)

png("Output/Figure Population-weighted BECRU estimates for different country groups, 2000-2022.png", width = 6*300, height = 4*300, res = 300)

ggplot() +
  geom_line(data = Eurostat_df_agg%>%
              filter(date < "2023-01-01"),
            aes(x=date, y=becru,colour = region), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10,labels = function(x) paste0(x, "%"))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#FF9999","#69b3a2","red", "navy")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

dev.off()

### Anglo-American ####
AA<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>%filter(region=="Anglo-American",date < "2023-01-01")%>%
              mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)),
            aes(x=date, y=unemp_eff,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "",subtitle ="Anglo-American") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=6,labels = function(x) paste0(x, "%"),limits = c(0, 9))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

AA

### Continental ####
CON<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>%filter(region=="Continental",date < "2023-01-01")%>%mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)),
            aes(x=date, y=unemp_eff,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "",subtitle ="Continental") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=6,labels = function(x) paste0(x, "%"),limits = c(0, 9))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

CON
### Eastern ####
EAST<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>%filter(region=="Eastern",date < "2023-01-01")%>%mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)),
            aes(x=date, y=unemp_eff,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "",subtitle ="Eastern") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=6,labels = function(x) paste0(x, "%"),limits = c(0, 9))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

EAST

### Nordic ####
NORD<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>%filter(region=="Nordic",date < "2023-01-01")%>%mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)),
            aes(x=date, y=unemp_eff,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "",subtitle ="Nordic") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=6,labels = function(x) paste0(x, "%"),limits = c(0, 9))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

NORD

### Southern ####
SOUTH<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>%filter(region=="Southern",date < "2023-01-01")%>%mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)),
            aes(x=date, y=unemp_eff,colour = country), size=1.3) + 
  theme_minimal()+
  labs(y = "BECRU, in % of labour force", x= "",subtitle ="Southern") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=6,labels = function(x) paste0(x, "%"),limits = c(0, 9))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

SOUTH

#### MERGE AND EXPORT BECRU ####
png("Output/Figure BECRU estimates for the extended country sample, 2000-2022.png", width = 9*300, height = 10*300, res = 300)

combined_plot1 <- ggarrange(AA,NORD,CON,SOUTH,EAST,
                            nrow = 3,
                            ncol = 2)
combined_plot1

dev.off()


########## 7.3) FULL EMPLOPYMENT GAP ##########
png("Output/Figure Population-weighted full employment gaps for different country groups, 2000-2022.png", width = 6*300, height = 4*300, res = 300)

ggplot() +
  geom_line(data = Eurostat_df_agg%>%
              filter(date < "2023-01-01"),
            aes(x=date, y=fegap,colour = region), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = "Full employment gap, percentage points", x= "") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10)+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

dev.off()

### EUROZONE ####
unique(as.character(Eurostat_merged_euro$country))  

png("Output/Figure Full employment gaps for the Euro area and the US, 2000-2022.png", width = 6*300, height = 4*300, res = 300)

ggplot() +
  geom_line(data = Eurostat_merged_euro%>%
              filter(date < "2023-01-01"),
            aes(x=date, y=values,colour = var), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = "Full employment gap, percentage points", x= "") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(size = 10))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=10)+
  scale_color_manual(label = c("population-weighted euro area", "unweighted euro area", "US"),
                     values=c("#69b3a2", "#999999","#FF9999","#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

dev.off()

### Anglo-American ####
AA<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>% filter(region =="Anglo-American",date < "2023-01-01")%>%
              mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)) %>%
              mutate(gap =unemployment_rate- unemp_eff),
            aes(x=date, y=gap,colour = country), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = " ", x= "",subtitle ="Anglo-American") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=8,limits = c(-3.5, 26))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

AA

### Continental ####
CON<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>% filter(region =="Continental",date < "2023-01-01")%>%
              mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)) %>%
              mutate(gap =unemployment_rate- unemp_eff),
            aes(x=date, y=gap,colour = country), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = "Full employment gap, percentage points", x= "",subtitle ="Continental") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=8,limits = c(-3.5, 26))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

CON
### Eastern ####
EAST<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>% filter(region =="Eastern",date < "2023-01-01")%>%
              mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)) %>%
              mutate(gap =unemployment_rate- unemp_eff),
            aes(x=date, y=gap,colour = country), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = " ", x= "",subtitle ="Eastern") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=8,limits = c(-3.5,26))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

EAST

### Nordic ####
NORD<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>% filter(region =="Nordic",date < "2023-01-01")%>%
              mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)) %>%
              mutate(gap =unemployment_rate- unemp_eff),
            aes(x=date, y=gap,colour = country), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = " ", x= "",subtitle ="Nordic") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=8,limits = c(-3.5,26))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

NORD

### Southern ####
SOUTH<-ggplot() +
  geom_line(data = Eurostat_df_weighted%>% filter(region =="Southern",date < "2023-01-01")%>%
              mutate(unemp_eff = sqrt(unemployment_rate*vacancy_rate)) %>%
              mutate(gap =unemployment_rate- unemp_eff),
            aes(x=date, y=gap,colour = country), size=1.3) +
  geom_hline(yintercept=0,size=0.7 , color = "black", linetype="dotted")+ 
  theme_minimal()+
  labs(y = " ", x= "",subtitle ="Southern") +
  theme(axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),legend.title=element_blank(),
        legend.position="bottom",
        #axis.title.y = element_text(size = 10),
        strip.text.x = element_text(size = 11),
        legend.text=element_text(size= 11),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12))+
  scale_x_date( date_breaks = "4 years",date_labels ="%Y") +
  scale_y_continuous(n.breaks=8,limits = c(-3.5,26))+
  scale_color_manual(values=c("#999999", "#E69F00", "#FF9999","#69b3a2","#56B4E9", "#F0E442","#0072B9", "#D55E00", "#CC79A7", "#117733", "#332288")) +
  geom_rect(data = OECD_Business_Cycles_short%>%filter(country =="OECDE"),aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            alpha = I(.5), fill = I("lightgrey"))

SOUTH

#### MERGE AND EXPORT GAP ####
png("Output/Figure  Beveridge full employment gap for the extended sample of 25 countries, 2000-2022.png", width = 9*300, height = 10*300, res = 300)

combined_plot1 <- ggarrange(AA,NORD,CON,SOUTH,EAST,
                            nrow = 3,
                            ncol = 2)
combined_plot1

dev.off()






