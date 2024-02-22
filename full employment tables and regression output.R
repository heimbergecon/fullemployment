rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table", 
              "countrycode", "GGally", "car", "urca", "pco", "purrr")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library(stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)
library(countrycode)
library(GGally)
library(car)
library(urca)
library(pco)
library(purrr)
library(scales) #for retrieving 'percent'ages
library(tidyverse)
library(ggpubr)


#read data
data <- fread(here("data/full-employment-regs_final.csv"), check.names = FALSE, header=TRUE)
# data <- fread(here("data/full-employment-regs.csv"), check.names = FALSE, header=TRUE) #does NOT include decade dummies yet !!

#restrict sample (exclude France and Italy (currently no data) and Spain (problems with vacancy rate))
data <- subset(data, ccode %in% c('AUT', 'DEU', 'FIN', 'SWE', 'GBR', 'USA'))

#modifying political left-right scale for easier interpretation with the interaction effects
#.. before 0='far left', 10='far right', NOW: 0='far right', 10='far left'
data$LRG_pg <- 10 - data$LRG_pg 
data$LRG_cp <- 10 - data$LRG_cp

# creating consistent dataset
data_cons <- data %>% 
  as.data.frame() %>% 
  ungroup() %>% 
  arrange(ccode, year) %>% 
  group_by(ccode) %>% 
  dplyr::mutate(FEGAP.lag1 = dplyr::lag(FEGAP, n=1, default=NA)) %>% 
  dplyr::mutate(EPL.lag1 = dplyr::lag(EPL, n=1, default=NA)) %>%      #defining lag var's here, because lag() operations within plm-regression structure (if dplyr::lag() is used)
  dplyr::mutate(UDENS.lag1 = dplyr::lag(UDENS, n=1, default=NA)) %>%  #.. s.t. ignore groups (e.g. lag(UDENS) of FIN in 1971 will become UDENS val of DEU from 2020 or 2022)
  dplyr::mutate(dUDENS.lag1 = dplyr::lag(UDENS, n=1, default=NA)-dplyr::lag(UDENS, n=2, default=NA)) %>%  #.. s.t. ignore groups (e.g. lag(UDENS) of FIN in 1971 will become UDENS val of DEU from 2020 or 2022)
  dplyr::mutate(TFP.lag1 = dplyr::lag(TFP, n=1, default=NA)) %>%      #.. Alternative: consistently call the 'plm::lag()' command!
  dplyr::mutate(EGLOB.lag1 = dplyr::lag(EGLOB, n=1, default=NA)) %>%
  dplyr::mutate(ACTPOP.lag1 = dplyr::lag(ACTPOP, n=1, default=NA)) %>%
  dplyr::mutate(ACCU.lag1 = dplyr::lag(ACCU, n=1, default=NA)) %>%
  dplyr::mutate(INFL.lag1 = dplyr::lag(INFL, n=1, default=NA)) %>%
  dplyr::mutate(LRG_pg.lag1 = dplyr::lag(LRG_pg, n=1, default=NA)) %>% 
  dplyr::mutate(LRG_cp.lag1 = dplyr::lag(LRG_cp, n=1, default=NA)) %>% 
  dplyr::mutate(PUCA.lag1 = dplyr::lag(PUCA, n=1, default=NA)) %>% 
  dplyr::mutate(PRCA.lag1 = dplyr::lag(PRCA, n=1, default=NA)) %>% 
  dplyr::mutate(OG.lag1 = dplyr::lag(OG, n=1, default=NA)) %>% 
  dplyr::mutate(LTU.lag1 = dplyr::lag(LTU, n=1, default=NA)) %>% 
  dplyr::mutate(DUEP_EU = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1, 1, 0),
                DUEP_UKUS = ifelse(DCOU_USA==1 | DCOU_GBR==1, 1, 0),
                DUEP_EUUK = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1 | DCOU_GBR==1, 1, 0),
                DUEP_US = ifelse(DCOU_USA==1, 1, 0),
                DCLU_EU = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1 | DCOU_GBR==1, 1, 0),
                DCLU_US = ifelse(DCOU_USA==1, 1, 0)) %>% 
  dplyr::mutate(NAIRU_gap.lag1 = dplyr::lag(NAIRU_gap, n=1, default=NA)) %>% 
  filter(!(year %in% (2021:2022)), #NB: current data till 2019, i.e. lags including 2020
         !(year==1970 & ccode %in% c('GBR', 'SWE', 'FIN')), #missing ACTPOP for all 3 & FEGAP for GBR
         !(year==1970 & ccode %in% c('AUT', 'DEU', 'USA')), #.. due to lags based regs, also no obs vor AUT, DEU, and USA
         !(year==1971 & ccode %in% c('GBR', 'SWE', 'FIN')), #missing ACTPOP (GBR) and missing ACTPOP.lag1 (SWE, FIN)
         !(year==1972 & ccode %in% c('GBR')) ) #missing ACTPOP.lag1
#!(year %in% (1982:1983) & ccode == 'USA')) #missing UDENS values in '81 and '82 -> cause of lag skip '82 and '83


##########################################################
################## MAIN TEXT OUTPUT ######################
##########################################################

### (I) MAIN TEXT TABLES ###

# Table 2, descriptive table
stargazer(as.data.frame(data_cons[c("year", "ccode", "FEGAP", "FEGAP.lag1", "EPL.lag1", "UDENS.lag1", "TFP.lag1", "EGLOB.lag1", "ACTPOP.lag1", "ACCU.lag1", "INFL.lag1", "LRG_pg.lag1")]), 
          # title="Descriptive statistics", 
          digits=2, 
          type='html', out=here("output/Table 2 (descriptive table).htm"))


# Table 3, baseline panel regression approach - Regressions securing the identification effect / Endogeneity-adjusted regressions --> using the "Felbermayr" approach (i.e., lags to account for potential endogeneity matters)
#Hysteresis effects
reg_id.lag_v1 <- plm(FEGAP ~ FEGAP.lag1 , 
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
#Labour market institutions
reg_id.lag_v2 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1,   
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons) 
#Structural changes, TFP growth and globalisation, active population
reg_id.lag_v3 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1, 
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
#Macroeconomic factors
reg_id.lag_v4 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1, 
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
#Political factors
reg_id.lag_v5 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1, # + lag(UDENS)*lag(LRG_pg), 
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons )
#Time period dummies
reg_id.lag_v6 <- plm(FEGAP ~  FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                       Eighties + Nineties + FinancialCrisis + EuroCrisis, 
                     index=c("ccode", "year"), model="within", effect="individual", data=data_cons)
#EU x fegap
reg_id.lag_v7 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                       DCLU_EU*FEGAP.lag1,
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
#EU_countries x fegap
reg_id.lag_v8 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                       DCLU_EU*FEGAP.lag1, # DCOU_AUT*FEGAP.lag1 + DCOU_DEU*FEGAP.lag1 + DCOU_FIN*FEGAP.lag1 + DCOU_SWE*FEGAP.lag1 + DCOU_GBR*FEGAP.lag1 ,
                     index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)


#preparation for stargazer tables
ses.reg_id.lag_v1 <- list(coeftest(reg_id.lag_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v1 <- list(coeftest(reg_id.lag_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v1 <- list(coeftest(reg_id.lag_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v2 <- list(coeftest(reg_id.lag_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v2 <- list(coeftest(reg_id.lag_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v2 <- list(coeftest(reg_id.lag_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v3 <- list(coeftest(reg_id.lag_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v3 <- list(coeftest(reg_id.lag_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v3 <- list(coeftest(reg_id.lag_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v4 <- list(coeftest(reg_id.lag_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v4 <- list(coeftest(reg_id.lag_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v4 <- list(coeftest(reg_id.lag_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v5 <- list(coeftest(reg_id.lag_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v5 <- list(coeftest(reg_id.lag_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v5 <- list(coeftest(reg_id.lag_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v6 <- list(coeftest(reg_id.lag_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v6 <- list(coeftest(reg_id.lag_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v6 <- list(coeftest(reg_id.lag_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v7 <- list(coeftest(reg_id.lag_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v7 <- list(coeftest(reg_id.lag_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v7 <- list(coeftest(reg_id.lag_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag_v8 <- list(coeftest(reg_id.lag_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag_v8 <- list(coeftest(reg_id.lag_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag_v8 <- list(coeftest(reg_id.lag_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
#ses.reg_id.lag_v9 <- list(coeftest(reg_id.lag_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
#tvals.reg_id.lag_v9 <- list(coeftest(reg_id.lag_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
#pvals.reg_id.lag_v9 <- list(coeftest(reg_id.lag_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_id.lag_v1, reg_id.lag_v2, reg_id.lag_v3, reg_id.lag_v4, reg_id.lag_v5, reg_id.lag_v6, reg_id.lag_v7, reg_id.lag_v8,  
          se= list(unlist(ses.reg_id.lag_v1),   unlist(ses.reg_id.lag_v2),    unlist(ses.reg_id.lag_v3),   unlist(ses.reg_id.lag_v4),   unlist(ses.reg_id.lag_v5),   unlist(ses.reg_id.lag_v6),   unlist(ses.reg_id.lag_v7),   unlist(ses.reg_id.lag_v8)), 
          t = list(unlist(tvals.reg_id.lag_v1), unlist(tvals.reg_id.lag_v2),  unlist(tvals.reg_id.lag_v3), unlist(tvals.reg_id.lag_v4), unlist(tvals.reg_id.lag_v5), unlist(tvals.reg_id.lag_v6), unlist(tvals.reg_id.lag_v7), unlist(tvals.reg_id.lag_v8)), 
          p = list(unlist(pvals.reg_id.lag_v1), unlist(pvals.reg_id.lag_v2),  unlist(pvals.reg_id.lag_v3), unlist(pvals.reg_id.lag_v4), unlist(pvals.reg_id.lag_v5), unlist(pvals.reg_id.lag_v6), unlist(pvals.reg_id.lag_v7), unlist(pvals.reg_id.lag_v8)),
          type='html', out=here("output/Table 3 (reg baseline incl UK).htm"))






##########################################################
################## APPENDIX OUTPUT #######################
##########################################################


### (II) Pre-testing
### (II.1) multicollinearity testing

# e.g. https://www.codingprof.com/3-ways-to-test-for-multicollinearity-in-r-examples/
# good overview, https://anyi-guo.medium.com/correlation-pearson-vs-spearman-c15e581c12ce
# generally note, 'plm silently drops perfect collinear variables'
#a) `corr`, `corrplot`, `ggpairs` - correlation indices below 0.9 good
cor_vars <- data_cons %>% 
  ungroup() %>% 
  select(FEGAP, FEGAP.lag1, EPL.lag1, UDENS.lag1, TFP.lag1, EGLOB.lag1, ACTPOP.lag1, ACCU.lag1, INFL.lag1, LRG_pg.lag1,
         LRG_cp.lag1, PUCA.lag1, PRCA.lag1, OG.lag1, LTU.lag1, DCLU_EU)
as.data.frame(.)

# Table A 2, Spearman correlation analysis
cor_vars.tabout <- cor_vars %>% cor(., use="na.or.complete", method='spearman') %>% round(., 2) #'pearson' is the default method in cor
cor_vars.tabout
stargazer(cor_vars.tabout,
          digits = 2, summary=FALSE, rownames = T, type='html', out=here("output/Table A2 (spearman correlation analysis).htm"))
corrplot::corrplot(cor_vars %>% cor(., use="na.or.complete", method='spearman'))

cor_test.tabout <- cor_vars %>% corr.test(., use="na.or.complete", method='spearman') #'pearson' is the default method in cor
cor_test.tabout

#ggpairs(data, columns=c("FEGAP", "EPL", "UDENS", "TFP", "EGLOB", "ACTPOP", "PUCA", "PRCA","INFL", "OG", "LTU", "LRG_pg", "PS"))
#ggpairs(data_cons, columns=c("FEGAP.lag1", "EPL.lag1", "UDENS.lag1", "TFP.lag1", "EGLOB.lag1", "ACTPOP.lag1", "ACCU.lag1", "INFL.lag1", "LRG_pg.lag1",
#                             "LRG_cp.lag1", "PUCA.lag1", "PRCA.lag1", "OG.lag1", "LTU.lag1"))

#cor_pvals <- (cor_vars %>% corr.test(., use="na.or.complete", method='spearman'))$p
#corPlot(corCi(cor_vars, method='spearman'), pval=cor_pvals)
#sapply(1:length(cor_vars), function(x) cor.test(cor_vars[,x+1], cor_vars$FEGAP))


#b) VIF
#vif_reg_base_2 <- car::vif(reg_base_v2) #not working, bc `plm` does not provide reg results with intercept

#.. hence writing function to compute VIF manually
vif_calc <- function(reg_model) {
  X <- reg_model$model[,2:length(reg_model$model)] # obtaining values of independent variables
  corX <- cor(X)   # obtaining correlation matrix
  Tmat <- solve(corX) # obtaining tolerance matrix as inverse of cor-matrix (tolerance = proportion of variance NOT explained by other var's)
  VIF <- diag(Tmat) # obtaining VIF by extracting the diagnonal elements 
  names(VIF) <- colnames(X)
  round(VIF, 2)
}

#vif_calc(reg_base_v2); vif_calc(reg_base_v3a); vif_calc(reg_base_v3b); vif_calc(reg_base_v4a); vif_calc(reg_base_v4b); vif_calc(reg_base_v5a); vif_calc(reg_base_v5b)
vif_calc(reg_id.lag_v2); vif_calc(reg_id.lag_v3); vif_calc(reg_id.lag_v4); vif_calc(reg_id.lag_v5); vif_calc(reg_id.lag_v6); vif_calc(reg_id.lag_v7); vif_calc(reg_id.lag_v8)

#.. check correctness of approach with simple lm model:
reg_base_v1_lm <- lm(FEGAP ~  EPL + UDENS, data=data)
car::vif(reg_base_v1_lm)
vif_calc(reg_base_v1_lm) #check! values are the same


##c) eigenvalues and condition index
#library("olsrr")
#ols_eigen_cindex(reg_base_v1_lm)
#ols_eigen_cindex(reg_base_v1) #again, not working - `plm` not providing adequate elements



### (II.2) unit root tests

pdata <- data_cons %>% 
  select("ccode", "year", "FEGAP", "EPL", "UDENS", "TFP", "EGLOB", "ACTPOP", "ACCU", "INFL", "LRG_pg", "DCLU_EU") %>% 
  filter(year < 2020,   #now restrict non-lagged dataset, since 2020 includes some missings and we aim for a balanced panel
         year >= 1972)  #... and missing ACTPOP for GBR in 1971

pdata.lag1 <- data_cons %>% 
  ungroup() %>% 
  arrange(ccode, year) %>% 
  group_by(ccode) %>% 
  dplyr::mutate(FEGAP.lag1 = dplyr::lag(FEGAP, n=1, default=NA)) %>% 
  dplyr::mutate(EPL.lag1 = dplyr::lag(EPL, n=1, default=NA)) %>%    
  dplyr::mutate(UDENS.lag1 = dplyr::lag(UDENS, n=1, default=NA)) %>%  
  dplyr::mutate(dUDENS.lag1 = dplyr::lag(UDENS, n=1, default=NA)-dplyr::lag(UDENS, n=2, default=NA)) %>%  #add difference of lagUDENS
  dplyr::mutate(TFP.lag1 = dplyr::lag(TFP, n=1, default=NA)) %>%      
  dplyr::mutate(EGLOB.lag1 = dplyr::lag(EGLOB, n=1, default=NA)) %>%
  dplyr::mutate(ACTPOP.lag1 = dplyr::lag(ACTPOP, n=1, default=NA)) %>%
  dplyr::mutate(ACCU.lag1 = dplyr::lag(ACCU, n=1, default=NA)) %>%
  dplyr::mutate(INFL.lag1 = dplyr::lag(INFL, n=1, default=NA)) %>%
  dplyr::mutate(LRG_pg.lag1 = dplyr::lag(LRG_pg, n=1, default=NA)) %>%  
  ungroup() %>% 
  filter(year >= 1973) %>%   #NB: ACTPOP.lag1 missing for GBR in 1971 and 1972, also dUDENS.lag1 starting from 1973
  select("ccode", "year", "FEGAP.lag1", "EPL.lag1", "UDENS.lag1", "dUDENS.lag1", "TFP.lag1", "EGLOB.lag1", "ACTPOP.lag1", "ACCU.lag1", "INFL.lag1", "LRG_pg.lag1")  


pdata.lessUSA <- pdata %>% filter(ccode != 'USA') #creating a version w/o USA since no EPL change for US!
pdata.lag1.lessUSA <- pdata.lag1 %>% filter(ccode != 'USA') #creating a version w/o USA since no EPL change for US!

pdata <- pdata.frame(pdata, index = c("ccode", "year"))
pdata.lag1 <- pdata.frame(pdata.lag1, index = c("ccode", "year"))
pdata.lessUSA <- pdata.frame(pdata.lessUSA, index = c("ccode", "year"))
pdata.lag1.lessUSA <- pdata.frame(pdata.lag1.lessUSA, index = c("ccode", "year"))

pdata.FEGAP <- pdata[, 'FEGAP'];  pdata.FEGAP.ts <- ts(pdata.FEGAP, start=1, frequency=1)
pdata.EPL <- pdata[, 'EPL'];  pdata.EPL.ts <- ts(pdata.EPL, start=1, frequency=1)
pdata.UDENS <- pdata[, 'UDENS'];  pdata.UDENS.ts <- ts(pdata.UDENS, start=1, frequency=1)
pdata.TFP <- pdata[, 'TFP'];  pdata.TFP.ts <- ts(pdata.TFP, start=1, frequency=1)
pdata.EGLOB <- pdata[, 'EGLOB'];  pdata.EGLOB.ts <- ts(pdata.EGLOB, start=1, frequency=1)
pdata.ACTPOP <- pdata[, 'ACTPOP'];  pdata.ACTPOP.ts <- ts(pdata.ACTPOP, start=1, frequency=1)
pdata.ACCU <- pdata[, 'ACCU'];  pdata.ACCU.ts <- ts(pdata.ACCU, start=1, frequency=1)
pdata.INFL <- pdata[, 'INFL'];  pdata.INFL.ts <- ts(pdata.INFL, start=1, frequency=1)
pdata.LRG_pg <- pdata[, 'LRG_pg'];  pdata.LRG_pg.ts <- ts(pdata.LRG_pg, start=1, frequency=1)

pdata.FEGAP.lag1 <- pdata.lag1[, 'FEGAP.lag1'];  pdata.FEGAP.lag1.ts <- ts(pdata.FEGAP.lag1, start=1, frequency=1)
pdata.EPL.lag1 <- pdata.lag1[, 'EPL.lag1'];  pdata.EPL.lag1.ts <- ts(pdata.EPL.lag1, start=1, frequency=1)
pdata.UDENS.lag1 <- pdata.lag1[, 'UDENS.lag1'];  pdata.UDENS.lag1.ts <- ts(pdata.UDENS.lag1, start=1, frequency=1)
pdata.dUDENS.lag1 <- pdata.lag1[, 'dUDENS.lag1'];  pdata.dUDENS.lag1.ts <- ts(pdata.dUDENS.lag1, start=1, frequency=1)
pdata.TFP.lag1 <- pdata.lag1[, 'TFP.lag1'];  pdata.TFP.lag1.ts <- ts(pdata.TFP.lag1, start=1, frequency=1)
pdata.EGLOB.lag1 <- pdata.lag1[, 'EGLOB.lag1'];  pdata.EGLOB.lag1.ts <- ts(pdata.EGLOB.lag1, start=1, frequency=1)
pdata.ACTPOP.lag1 <- pdata.lag1[, 'ACTPOP.lag1'];  pdata.ACTPOP.lag1.ts <- ts(pdata.ACTPOP.lag1, start=1, frequency=1)
pdata.ACCU.lag1 <- pdata.lag1[, 'ACCU.lag1'];  pdata.ACCU.lag1.ts <- ts(pdata.ACCU.lag1, start=1, frequency=1)
pdata.INFL.lag1 <- pdata.lag1[, 'INFL.lag1'];  pdata.INFL.lag1.ts <- ts(pdata.INFL.lag1, start=1, frequency=1)
pdata.LRG_pg.lag1 <- pdata.lag1[, 'LRG_pg.lag1'];  pdata.LRG_pg.lag1.ts <- ts(pdata.LRG_pg.lag1, start=1, frequency=1)


#i) LLC (Levin Lin Chu) testing -> requiring panel data structure with 'index' attributes
purtest(FEGAP ~ 1, data=pdata, exo='trend', test='levinlin')  #p<0.01, stat
purtest(EPL ~ 1, data=pdata.lessUSA, exo='trend', test='levinlin') #p<0.01, stat [excluding USA, since EPL doesn't change for USA case]
purtest(UDENS ~ 1, data=pdata, exo='trend', test='levinlin')  #p > 0.10, stat
purtest(TFP ~ 1, data=pdata, exo='trend', test='levinlin')    #p<0.01, stat
purtest(EGLOB ~ 1, data=pdata, exo='trend', test='levinlin')  #p<0.01, stat
purtest(ACTPOP ~ 1, data=pdata, exo='trend', test='levinlin') #p<0.01, stat
purtest(ACCU ~ 1, data=pdata, exo='trend', test='levinlin')   #p<0.01, stat
purtest(INFL ~ 1, data=pdata, exo='trend', test='levinlin')   #p<0.01, stat
purtest(LRG_pg ~ 1, data=pdata, exo='trend', test='levinlin') #p<0.05, stat

purtest(FEGAP.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin')  #p<0.01, stat
purtest(EPL.lag1 ~ 1, data=pdata.lag1.lessUSA, exo='trend', test='levinlin') #p<0.01, stat [excluding USA, since EPL doesn't change for USA case]
purtest(UDENS.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin')  #p > 0.10, NON-stat
purtest(dUDENS.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin') #p<0.01, stat
purtest(TFP.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin')    #p<0.01, stat
purtest(EGLOB.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin')  #p<0.01, stat
purtest(ACTPOP.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin') #p<0.01, stat
purtest(ACCU.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin')   #p<0.01, stat
purtest(INFL.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin')   #p<0.01, stat
purtest(LRG_pg.lag1 ~ 1, data=pdata.lag1, exo='trend', test='levinlin') #p<0.05, stat


#ii) IPS testing - 
purtest(FEGAP ~ 1, data=pdata, exo='trend', test='ips')       #p<0.01, stat
purtest(EPL ~ 1, data=pdata.lessUSA, exo='trend', test='ips') #p<0.01, stat [data excluding USA]
purtest(UDENS ~ 1, data=pdata, exo = "trend", test = "ips")   #p<0.10, weakly stat
purtest(TFP ~ 1, data=pdata, exo = "trend", test = "ips")     #p<0.01, stat
purtest(EGLOB ~ 1, data=pdata, exo = "trend", test = "ips")   #p<0.10, weakly stat
purtest(ACTPOP ~ 1, data=pdata, exo='trend', test='ips')      #p<0.01, stat
purtest(ACCU ~ 1, data=pdata, exo = "trend", test = "ips")    #p<0.01, stat
purtest(INFL ~ 1, data=pdata, exo = "trend", test = "ips")    #p<0.01, stat
purtest(LRG_pg ~ 1, data=pdata, exo = "trend", test = "ips")  #p<0.05, stat

purtest(FEGAP.lag1 ~ 1, data=pdata.lag1, exo='trend', test='ips')       #p<0.01, stat
purtest(EPL.lag1 ~ 1, data=pdata.lag1.lessUSA, exo='trend', test='ips') #p<0.01, stat [data excluding USA]
purtest(UDENS.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")   #p<0.10, weakly stat
purtest(dUDENS.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")   #p<0.10, weakly stat
purtest(TFP.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")     #p<0.01, stat
purtest(EGLOB.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")   #p<0.10, weakly stat
purtest(ACTPOP.lag1 ~ 1, data=pdata.lag1, exo='trend', test='ips')      #p<0.01, stat
purtest(ACCU.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")    #p<0.01, stat
purtest(INFL.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")    #p<0.01, stat
purtest(LRG_pg.lag1 ~ 1, data=pdata.lag1, exo = "trend", test = "ips")  #p<0.05, stat

#.. weirdly slightly different values when using the formula-less syntax (which should not be the case...)
#purtest(pdata.FEGAP, exo = "trend", test = "ips") #or: 'madwu', 'levinlin'
#purtest(pdata.EPL, exo = "trend", test = "ips")
#purtest(pdata.UDENS, exo = "trend", test = "ips")
#purtest(pdata.TFP, exo = "trend", test = "ips")
#purtest(pdata.EGLOB, exo = "trend", test = "ips")
#purtest(pdata.ACTPOP, exo = "trend", test = "ips")
#purtest(pdata.ACCU, exo = "trend", test = "ips")
#purtest(pdata.INFL, exo = "trend", test = "ips")
#purtest(pdata.LRG_pg, exo = "trend", test = "ips")

#.. and cross-sectionally augmented IPS test:
cipstest(pdata$FEGAP, type='drift', model='cmg') #.. not working.. probably bc of too few individuals (only 5 states and 49 years)
#.. NB: command works for 'Produc' test dataset from plm, where 48 states and 17 years are available
cipstest(pdata$EPL, type='trend')   #.. not working
cipstest(pdata$UDENS, type='trend') #.. not working
cipstest(pdata$TFP, type='trend')   #.. not working
cipstest(pdata$EGLOB, type='trend') #.. not working
cipstest(pdata$ACTPOP, type='trend')#.. not working
cipstest(pdata$ACCU, type='trend')  #.. not working
cipstest(pdata$INFL, type='trend')  #.. not working
cipstest(pdata$LRG_pg, type='trend')#.. not working 


#iii) MW testing
purtest(pdata[,-c(1,2)], pmax = 4, exo = "trend", test = "madwu")
purtest(pdata.lag1[,-c(1,2)], pmax = 4, exo = "trend", test = "madwu") #all regressors          |
purtest(pdata.lag1[,-c(1,2,6)], pmax = 4, exo = "trend", test = "madwu") #excluding dUDENS.lag1 > same results p<0.01
purtest(pdata.lag1[,-c(1,2,5)], pmax = 4, exo = "trend", test = "madwu") #excluding dUDENS.lag1 |


#iv) ADF testing - gold standard for time series data
# ADF test: deltaY = gamma*Y_t-1 + a0 + a2*t + epsilon
# trend / drift / none: tau3/2/1: gamma = 0
#                       phi3/1: gamma = 0 & a2 = 0
#                       phi2: gamma = 0 & a2 = 0 & a0 = 0

# # running the ADF test for all country-stacked variables
# summary(ur.df(pdata.FEGAP.ts, type='trend', lags=0, selectlags='BIC')) #gamma: p>0.05, stat
# summary(ur.df(pdata.EPL.ts, type='trend', lags=0, selectlags='BIC'))   #gamma: p>0.10, stat
# summary(ur.df(pdata.UDENS.ts, type='trend', lags=0, selectlags='BIC')) #gamma: p>0.10, stat
# summary(ur.df(pdata.TFP.ts, type='trend', lags=0, selectlags='BIC'))   #gamma: p<0.01, strongly non-stat
# summary(ur.df(pdata.EGLOB.ts, type='trend', lags=0, selectlags='BIC')) #gamma: p<0.05, non-stat
# summary(ur.df(pdata.ACTPOP.ts, type='trend', lags=0, selectlags='BIC'))#gamma: p<0.01, stat
# summary(ur.df(pdata.ACCU.ts, type='trend', lags=0, selectlags='BIC'))  #gamma: p>0.10, stat
# summary(ur.df(pdata.INFL.ts, type='trend', lags=0, selectlags='BIC'))  #gamma: p<0.01, strongly non-stat
# summary(ur.df(pdata.LRG_pg.ts, type='trend', lags=0, selectlags='BIC'))#gamma: p<0.01, strongly non-stat
# 
# summary(ur.df(pdata.FEGAP.ts[1:49], type='trend', lags=0, selectlags='BIC'))
# summary(ur.df(pdata.FEGAP.ts, type='trend', lags=0, selectlags='BIC'))


#doing ADF test for each time series variable by individual countries
## NB: how to read `ur.df()` output?! --> if z.lag.1-coefficient is significant then stationary! (H0: non-stationary)
##.. (i.e., p-value next to it OR comparison of t-value with critical value of tau3): 
vectlist1 <- list(pdata.FEGAP.ts, pdata.EPL.ts, pdata.UDENS.ts, pdata.dUDENS.ts)
varnumber = 1
for (tsvar in vectlist1) {
  start = 1
  print(paste0("VARIABLE ", varnumber))
  for (i in levels(unique(attributes(tsvar)[[2]][[1]]))) {
    print(paste0("Country ", i, " showing: "))
    end = start + 48
    print(paste0("start ", start, " / end ", end))
    res <- ur.df(tsvar[start:end], type='trend', lags=0, selectlags='BIC')
    print(summary(res))
    start = start + 49
  }
  start = 1
  varnumber = varnumber + 1
}


vectlist2 <- list(pdata.TFP.ts, pdata.EGLOB.ts, pdata.ACTPOP.ts)
varnumber = 1
for (tsvar in vectlist2) {
  start = 1
  print(paste0("VARIABLE ", varnumber))
  for (i in levels(unique(attributes(tsvar)[[2]][[1]]))) {
    print(paste0("Country ", i, " showing: "))
    end = start + 48
    print(paste0("start ", start, " / end ", end))
    res <- ur.df(tsvar[start:end], type='trend', lags=0, selectlags='BIC')
    print(summary(res))
    start = start + 49
  }
  start = 1
  varnumber = varnumber + 1
}

vectlist3 <- list(pdata.ACCU.ts, pdata.INFL.ts, pdata.LRG_pg.ts)
varnumber = 1
for (tsvar in vectlist3) {
  start = 1
  print(paste0("VARIABLE ", varnumber))
  for (i in levels(unique(attributes(tsvar)[[2]][[1]]))) {
    print(paste0("Country ", i, " showing: "))
    end = start + 48
    print(paste0("start ", start, " / end ", end))
    res <- ur.df(tsvar[start:end], type='trend', lags=0, selectlags='BIC')
    print(summary(res))
    start = start + 49
  }
  start = 1
  varnumber = varnumber + 1
}


## for lag-version:
vectlist1.lag1 <- list(pdata.FEGAP.lag1.ts, pdata.EPL.lag1.ts, pdata.UDENS.lag1.ts, pdata.dUDENS.lag1.ts)
varnumber = 1
for (tsvar in vectlist1.lag1) {
  start = 1
  print(paste0("VARIABLE ", varnumber, "-------------------------------- |"))
  for (i in levels(unique(attributes(tsvar)[[2]][[1]]))) {
    print(paste0("Country ", i, " showing: "))
    end = start + 47
    print(paste0("start ", start, " / end ", end))
    res.a <- ur.df(tsvar[start:end], type='trend', lags=0, selectlags='BIC')
    res.b <- res.a@testreg$coefficients
    print(res.b)
    start = start + 48
  }
  start = 1
  varnumber = varnumber + 1
}

vectlist2.lag1 <- list(pdata.TFP.lag1.ts, pdata.EGLOB.lag1.ts, pdata.ACTPOP.lag1.ts)
varnumber = 1
for (tsvar in vectlist2.lag1) {
  start = 1
  print(paste0("VARIABLE ", varnumber, "-------------------------------- |"))
  for (i in levels(unique(attributes(tsvar)[[2]][[1]]))) {
    print(paste0("Country ", i, " showing: "))
    end = start + 47
    print(paste0("start ", start, " / end ", end))
    res.a <- ur.df(tsvar[start:end], type='trend', lags=0, selectlags='BIC')
    res.b <- res.a@testreg$coefficients
    print(res.b)
    start = start + 48
  }
  start = 1
  varnumber = varnumber + 1
}

vectlist3.lag1 <- list(pdata.ACCU.lag1.ts, pdata.INFL.lag1.ts, pdata.LRG_pg.lag1.ts)
varnumber = 1
for (tsvar in vectlist3.lag1) {
  start = 1
  print(paste0("VARIABLE ", varnumber, "-------------------------------- |"))
  for (i in levels(unique(attributes(tsvar)[[2]][[1]]))) {
    print(paste0("Country ", i, " showing: "))
    end = start + 47
    print(paste0("start ", start, " / end ", end))
    res.a <- ur.df(tsvar[start:end], type='trend', lags=0, selectlags='BIC')
    res.b <- res.a@testreg$coefficients
    print(res.b)
    start = start + 48
  }
  start = 1
  varnumber = varnumber + 1
}


#write out data for Eviews for further testing:
write_excel_csv(pdata.lag1,file = here("output/data_cons_for_eviews.csv"))

#exclude again dUDENS.lag1
pdata.lag1[,"dUDENS.lag1"] <- NULL


### (II.3) Co-integration tests
#.. were carried out in EViews, Version 10+ 



### (II.4) Hausman test

testdata <- data.frame(id = rep(1:3, each = 10), year = rep(1:10, 3), testy = rnorm(30))
testpdata <- pdata.frame(testdata, index = c("id", "year"))
testy <- testpdata[ , "testy"]
testy_ts <- ts(testy, start = c(1,1), frequency = 1)

# Hausmann test for checking diff's of FE and RE specification
# running the RE models
reg_base_v1.re <- plm(FEGAP ~  lag(FEGAP), index=c("ccode", "year"), model="random", effect="twoways", data=data)
reg_base_v2.re <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS, index=c("ccode", "year"), model="random", effect="twoways", data=data)


#.. error messages, because of too few individuals (8 coefficients, but only 5 individuals..)
reg_base_v3.re <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB, index=c("ccode", "year"), model="random", effect="twoways", data=data)
reg_base_v4a.re <- plm(FEGAP ~ lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU, index=c("ccode", "year"), model="random", effect="twoways", data=data)
reg_base_v4b.re <- plm(FEGAP ~ lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL, index=c("ccode", "year"), model="random", effect="twoways", data=data)
reg_base_v5a.re <- plm(FEGAP ~ lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL + LRG_pg, index=c("ccode", "year"), model="random", effect="twoways", data=data)
reg_base_v5b.re <- plm(FEGAP ~ lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL + LRG_pg*UDENS, index=c("ccode", "year"), model="random", effect="twoways", data=data)

# running the Hausmann test [as most common specification test to test the consistency of the RE estimator]
#.. note: the H0 is that the covariance between IV(s) and alpha is zero -> if so, then RE is preferred over FE. 
#.... if H0 is not true [and H1 is true], we must go with the FE-model
phtest(reg_base_v1, reg_base_v1.re) #H0 -> RE
phtest(reg_base_v2, reg_base_v2.re) #H0 -> RE

phtest(reg_base_v3, reg_base_v3.re) 
phtest(reg_base_v4, reg_base_v4.re)
phtest(reg_base_v5a, reg_base_v5a.re)
phtest(reg_base_v5b, reg_base_v5b.re)
phtest(reg_base_v5c, reg_base_v5c.re)








### (III) Robustness checks

## (A) country and cluster dummy extension
# Table A 7, Table with country and cluster dummies -> due to multi-collinearity reasons when including additional dummies, we run one-way fixed effects reg's
reg_rob.cocl_v1 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_rob.cocl_v2 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCOU_AUT + DCOU_DEU + DCOU_FIN + DCOU_SWE + DCOU_GBR + DCOU_USA, 
                       index=c("ccode", "year"), model="within", effect="time", data=data_cons)
reg_rob.cocl_v3 <- plm(FEGAP ~ DCOU_AUT + DCOU_DEU + DCOU_FIN + DCOU_SWE + DCOU_GBR + DCOU_USA,  
                       index=c("ccode", "year"), model="within", effect="time", data=data_cons)
reg_rob.cocl_v4 <- plm(FEGAP ~  FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCOU_AUT:FEGAP.lag1 + DCOU_DEU:FEGAP.lag1 + DCOU_FIN:FEGAP.lag1 + DCOU_SWE:FEGAP.lag1 + DCOU_GBR:FEGAP.lag1,
                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_rob.cocl_v5 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_CON + DCLU_SOD + DCLU_LIB, 
                       index=c("ccode", "year"), model="within", effect="time", data=data_cons)
reg_rob.cocl_v6 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_CON:FEGAP.lag1 + DCLU_SOD:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)

ses.reg_rob.cocl_v1 <- list(coeftest(reg_rob.cocl_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.cocl_v1 <- list(coeftest(reg_rob.cocl_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.cocl_v1 <- list(coeftest(reg_rob.cocl_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.cocl_v2 <- list(coeftest(reg_rob.cocl_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.cocl_v2 <- list(coeftest(reg_rob.cocl_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.cocl_v2 <- list(coeftest(reg_rob.cocl_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.cocl_v3 <- list(coeftest(reg_rob.cocl_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.cocl_v3 <- list(coeftest(reg_rob.cocl_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.cocl_v3 <- list(coeftest(reg_rob.cocl_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.cocl_v4 <- list(coeftest(reg_rob.cocl_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.cocl_v4 <- list(coeftest(reg_rob.cocl_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.cocl_v4 <- list(coeftest(reg_rob.cocl_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.cocl_v5 <- list(coeftest(reg_rob.cocl_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.cocl_v5 <- list(coeftest(reg_rob.cocl_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.cocl_v5 <- list(coeftest(reg_rob.cocl_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.cocl_v6 <- list(coeftest(reg_rob.cocl_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.cocl_v6 <- list(coeftest(reg_rob.cocl_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.cocl_v6 <- list(coeftest(reg_rob.cocl_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

stargazer(reg_rob.cocl_v1, reg_rob.cocl_v2, reg_rob.cocl_v3, reg_rob.cocl_v4, reg_rob.cocl_v5, reg_rob.cocl_v6,  
          se= list(unlist(ses.reg_rob.cocl_v1),   unlist(ses.reg_rob.cocl_v2),   unlist(ses.reg_rob.cocl_v3),   unlist(ses.reg_rob.cocl_v4),   unlist(ses.reg_rob.cocl_v5),   unlist(ses.reg_rob.cocl_v6)), 
          t = list(unlist(tvals.reg_rob.cocl_v1), unlist(tvals.reg_rob.cocl_v2), unlist(tvals.reg_rob.cocl_v3), unlist(tvals.reg_rob.cocl_v4), unlist(tvals.reg_rob.cocl_v5), unlist(tvals.reg_rob.cocl_v6)), 
          p = list(unlist(pvals.reg_rob.cocl_v1), unlist(pvals.reg_rob.cocl_v2), unlist(pvals.reg_rob.cocl_v3), unlist(pvals.reg_rob.cocl_v4), unlist(pvals.reg_rob.cocl_v5), unlist(pvals.reg_rob.cocl_v6)),
          type='html', out=here("output/Table A 7 (reg country clusters).htm"))


## (B.1) 3 year averaged data
# Table A 8, Extending the lagged baseline regression by 3 year averaged data
data.3ya <- data %>%
  group_by(ccode, 
           period.3y = cut(year, seq(1970, 2022, by = 3), right = FALSE)) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  dplyr::mutate(FEGAP3.lag1 = dplyr::lag(FEGAP, n=1, default=NA)) %>% 
  dplyr::mutate(EPL3.lag1 = dplyr::lag(EPL, n=1, default=NA)) %>%      #defining lag var's here, because lag() operations within plm-regression structure
  dplyr::mutate(UDENS3.lag1 = dplyr::lag(UDENS, n=1, default=NA)) %>%  #.. s.t. ignore groups (e.g. lag(UDENS) of FIN in 1971 will become UDENS val of DEU from 2020 or 2022)
  dplyr::mutate(TFP3.lag1 = dplyr::lag(TFP, n=1, default=NA)) %>%
  dplyr::mutate(EGLOB3.lag1 = dplyr::lag(EGLOB, n=1, default=NA)) %>%
  dplyr::mutate(ACTPOP3.lag1 = dplyr::lag(ACTPOP, n=1, default=NA)) %>%
  dplyr::mutate(ACCU3.lag1 = dplyr::lag(ACCU, n=1, default=NA)) %>%
  dplyr::mutate(INFL3.lag1 = dplyr::lag(INFL, n=1, default=NA)) %>%
  dplyr::mutate(LRG_pg3.lag1 = dplyr::lag(LRG_pg, n=1, default=NA)) %>% 
  dplyr::mutate(LRG_cp3.lag1 = dplyr::lag(LRG_cp, n=1, default=NA)) %>% 
  dplyr::mutate(DUEP_EU = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1, 1, 0),
                DUEP_UKUS = ifelse(DCOU_USA==1 | DCOU_GBR==1, 1, 0),
                DUEP_EUUK = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1 | DCOU_GBR==1, 1, 0),
                DUEP_US = ifelse(DCOU_USA==1, 1, 0)) %>%
  ungroup()

# regressions
reg_id.lag.3ya_v1 <- plm(FEGAP ~ FEGAP3.lag1 , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)
reg_id.lag.3ya_v2 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1, 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)
reg_id.lag.3ya_v3 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1 + TFP3.lag1 + EGLOB3.lag1 + ACTPOP3.lag1, 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)
reg_id.lag.3ya_v4 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1 + TFP3.lag1 + EGLOB3.lag1 + ACTPOP3.lag1 + ACCU3.lag1 + INFL3.lag1, 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)
reg_id.lag.3ya_v5 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1 + TFP3.lag1 + EGLOB3.lag1 + ACTPOP3.lag1 + ACCU3.lag1 + INFL3.lag1 + LRG_pg3.lag1 , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)
reg_id.lag.3ya_v6 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1 + TFP3.lag1 + EGLOB3.lag1 + ACTPOP3.lag1 + ACCU3.lag1 + INFL3.lag1 + LRG_pg3.lag1 +
                           Eighties + Nineties + FinancialCrisis + EuroCrisis, 
                         index=c("ccode", "year"), model="within", effect="individual", data=data.3ya)
reg_id.lag.3ya_v7 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1 + TFP3.lag1 + EGLOB3.lag1 + ACTPOP3.lag1 + ACCU3.lag1 + INFL3.lag1 + LRG_pg3.lag1 +
                           FEGAP3.lag1*DUEP_EUUK , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)
reg_id.lag.3ya_v8 <- plm(FEGAP ~ FEGAP3.lag1 + EPL3.lag1 + UDENS3.lag1 + TFP3.lag1 + EGLOB3.lag1 + ACTPOP3.lag1 + ACCU3.lag1 + INFL3.lag1 + LRG_pg3.lag1 +
                           FEGAP3.lag1*DCOU_AUT + FEGAP3.lag1*DCOU_DEU + FEGAP3.lag1*DCOU_FIN + FEGAP3.lag1*DCOU_SWE + FEGAP3.lag1*DCOU_GBR , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.3ya)

#preparation for stargazer tables
ses.reg_id.lag.3ya_v1 <- list(coeftest(reg_id.lag.3ya_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v1 <- list(coeftest(reg_id.lag.3ya_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v1 <- list(coeftest(reg_id.lag.3ya_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v2 <- list(coeftest(reg_id.lag.3ya_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v2 <- list(coeftest(reg_id.lag.3ya_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v2 <- list(coeftest(reg_id.lag.3ya_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v3 <- list(coeftest(reg_id.lag.3ya_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v3 <- list(coeftest(reg_id.lag.3ya_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v3 <- list(coeftest(reg_id.lag.3ya_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v4 <- list(coeftest(reg_id.lag.3ya_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v4 <- list(coeftest(reg_id.lag.3ya_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v4 <- list(coeftest(reg_id.lag.3ya_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v5 <- list(coeftest(reg_id.lag.3ya_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v5 <- list(coeftest(reg_id.lag.3ya_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v5 <- list(coeftest(reg_id.lag.3ya_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v6 <- list(coeftest(reg_id.lag.3ya_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v6 <- list(coeftest(reg_id.lag.3ya_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v6 <- list(coeftest(reg_id.lag.3ya_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v7 <- list(coeftest(reg_id.lag.3ya_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v7 <- list(coeftest(reg_id.lag.3ya_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v7 <- list(coeftest(reg_id.lag.3ya_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.3ya_v8 <- list(coeftest(reg_id.lag.3ya_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.3ya_v8 <- list(coeftest(reg_id.lag.3ya_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.3ya_v8 <- list(coeftest(reg_id.lag.3ya_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_id.lag.3ya_v1, reg_id.lag.3ya_v2, reg_id.lag.3ya_v3, reg_id.lag.3ya_v4, reg_id.lag.3ya_v5, reg_id.lag.3ya_v6, reg_id.lag.3ya_v7, reg_id.lag.3ya_v8,   
          se= list(unlist(ses.reg_id.lag.3ya_v1),   unlist(ses.reg_id.lag.3ya_v2),   unlist(ses.reg_id.lag.3ya_v3),   unlist(ses.reg_id.lag.3ya_v4),   unlist(ses.reg_id.lag.3ya_v5),   unlist(ses.reg_id.lag.3ya_v6),   unlist(ses.reg_id.lag.3ya_v7),   unlist(ses.reg_id.lag.3ya_v8)), 
          t = list(unlist(tvals.reg_id.lag.3ya_v1), unlist(tvals.reg_id.lag.3ya_v2), unlist(tvals.reg_id.lag.3ya_v3), unlist(tvals.reg_id.lag.3ya_v4), unlist(tvals.reg_id.lag.3ya_v5), unlist(tvals.reg_id.lag.3ya_v6), unlist(tvals.reg_id.lag.3ya_v7), unlist(tvals.reg_id.lag.3ya_v8)), 
          p = list(unlist(pvals.reg_id.lag.3ya_v1), unlist(pvals.reg_id.lag.3ya_v2), unlist(pvals.reg_id.lag.3ya_v3), unlist(pvals.reg_id.lag.3ya_v4), unlist(pvals.reg_id.lag.3ya_v5), unlist(pvals.reg_id.lag.3ya_v6), unlist(pvals.reg_id.lag.3ya_v7), unlist(pvals.reg_id.lag.3ya_v8)),
          type='html', out=here("output/Table A 8 (reg 3year avg).htm"))


## (B.2) 5 year averaged data
# Table A 9, Extending the lagged baseline regression by 5 year averaged data
#sub-dataset for regression of 5yr averages
data.5ya <- data %>%
  group_by(ccode, 
           period.5y = cut(year, seq(1970, 2022, by = 5), right = FALSE)) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  dplyr::mutate(FEGAP5.lag1 = dplyr::lag(FEGAP, n=1, default=NA)) %>% 
  dplyr::mutate(EPL5.lag1 = dplyr::lag(EPL, n=1, default=NA)) %>%      #defining lag var's here, because lag() operations within plm-regression structure
  dplyr::mutate(UDENS5.lag1 = dplyr::lag(UDENS, n=1, default=NA)) %>%  #.. s.t. ignore groups (e.g. lag(UDENS) of FIN in 1971 will become UDENS val of DEU from 2020 or 2022)
  dplyr::mutate(TFP5.lag1 = dplyr::lag(TFP, n=1, default=NA)) %>%
  dplyr::mutate(EGLOB5.lag1 = dplyr::lag(EGLOB, n=1, default=NA)) %>%
  dplyr::mutate(ACTPOP5.lag1 = dplyr::lag(ACTPOP, n=1, default=NA)) %>%
  dplyr::mutate(ACCU5.lag1 = dplyr::lag(ACCU, n=1, default=NA)) %>%
  dplyr::mutate(INFL5.lag1 = dplyr::lag(INFL, n=1, default=NA)) %>%
  dplyr::mutate(LRG_pg5.lag1 = dplyr::lag(LRG_pg, n=1, default=NA)) %>% 
  dplyr::mutate(LRG_cp5.lag1 = dplyr::lag(LRG_cp, n=1, default=NA)) %>% 
  dplyr::mutate(DUEP_EU = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1, 1, 0),
                DUEP_UKUS = ifelse(DCOU_USA==1 | DCOU_GBR==1, 1, 0),
                DUEP_EUUK = ifelse(DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1 | DCOU_SWE==1 | DCOU_GBR==1, 1, 0),
                DUEP_US = ifelse(DCOU_USA==1, 1, 0)) %>% 
  ungroup()


reg_id.lag.5ya_v1 <- plm(FEGAP ~ FEGAP5.lag1 , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)
reg_id.lag.5ya_v2 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1, 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)
reg_id.lag.5ya_v3 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1 + TFP5.lag1 + EGLOB5.lag1 + ACTPOP5.lag1, 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)
reg_id.lag.5ya_v4 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1 + TFP5.lag1 + EGLOB5.lag1 + ACTPOP5.lag1 + ACCU5.lag1 + INFL5.lag1, 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)
reg_id.lag.5ya_v5 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1 + TFP5.lag1 + EGLOB5.lag1 + ACTPOP5.lag1 + ACCU5.lag1 + INFL5.lag1 + LRG_pg5.lag1 , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)
reg_id.lag.5ya_v6 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1 + TFP5.lag1 + EGLOB5.lag1 + ACTPOP5.lag1 + ACCU5.lag1 + INFL5.lag1 + LRG_pg5.lag1 +
                           Eighties + Nineties + FinancialCrisis + EuroCrisis, 
                         index=c("ccode", "year"), model="within", effect="individual", data=data.5ya)
reg_id.lag.5ya_v7 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1 + TFP5.lag1 + EGLOB5.lag1 + ACTPOP5.lag1 + ACCU5.lag1 + INFL5.lag1 + LRG_pg5.lag1 +
                           FEGAP5.lag1*DUEP_EUUK , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)
reg_id.lag.5ya_v8 <- plm(FEGAP ~ FEGAP5.lag1 + EPL5.lag1 + UDENS5.lag1 + TFP5.lag1 + EGLOB5.lag1 + ACTPOP5.lag1 + ACCU5.lag1 + INFL5.lag1 + LRG_pg5.lag1 +
                           FEGAP5.lag1*DCOU_AUT + FEGAP5.lag1*DCOU_DEU + FEGAP5.lag1*DCOU_FIN + FEGAP5.lag1*DCOU_SWE + FEGAP5.lag1*DCOU_GBR , 
                         index=c("ccode", "year"), model="within", effect="twoways", data=data.5ya)

#preparation for stargazer tables
ses.reg_id.lag.5ya_v1 <- list(coeftest(reg_id.lag.5ya_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v1 <- list(coeftest(reg_id.lag.5ya_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v1 <- list(coeftest(reg_id.lag.5ya_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v2 <- list(coeftest(reg_id.lag.5ya_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v2 <- list(coeftest(reg_id.lag.5ya_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v2 <- list(coeftest(reg_id.lag.5ya_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v3 <- list(coeftest(reg_id.lag.5ya_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v3 <- list(coeftest(reg_id.lag.5ya_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v3 <- list(coeftest(reg_id.lag.5ya_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v4 <- list(coeftest(reg_id.lag.5ya_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v4 <- list(coeftest(reg_id.lag.5ya_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v4 <- list(coeftest(reg_id.lag.5ya_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v5 <- list(coeftest(reg_id.lag.5ya_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v5 <- list(coeftest(reg_id.lag.5ya_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v5 <- list(coeftest(reg_id.lag.5ya_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v6 <- list(coeftest(reg_id.lag.5ya_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v6 <- list(coeftest(reg_id.lag.5ya_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v6 <- list(coeftest(reg_id.lag.5ya_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v7 <- list(coeftest(reg_id.lag.5ya_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v7 <- list(coeftest(reg_id.lag.5ya_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v7 <- list(coeftest(reg_id.lag.5ya_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_id.lag.5ya_v8 <- list(coeftest(reg_id.lag.5ya_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_id.lag.5ya_v8 <- list(coeftest(reg_id.lag.5ya_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_id.lag.5ya_v8 <- list(coeftest(reg_id.lag.5ya_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_id.lag.5ya_v1, reg_id.lag.5ya_v2, reg_id.lag.5ya_v3, reg_id.lag.5ya_v4, reg_id.lag.5ya_v5, reg_id.lag.5ya_v6, reg_id.lag.5ya_v7, reg_id.lag.5ya_v8,   
          se= list(unlist(ses.reg_id.lag.5ya_v1),   unlist(ses.reg_id.lag.5ya_v2),   unlist(ses.reg_id.lag.5ya_v3),   unlist(ses.reg_id.lag.5ya_v4),   unlist(ses.reg_id.lag.5ya_v5),   unlist(ses.reg_id.lag.5ya_v6),   unlist(ses.reg_id.lag.5ya_v7),   unlist(ses.reg_id.lag.5ya_v8)), 
          t = list(unlist(tvals.reg_id.lag.5ya_v1), unlist(tvals.reg_id.lag.5ya_v2), unlist(tvals.reg_id.lag.5ya_v3), unlist(tvals.reg_id.lag.5ya_v4), unlist(tvals.reg_id.lag.5ya_v5), unlist(tvals.reg_id.lag.5ya_v6), unlist(tvals.reg_id.lag.5ya_v7), unlist(tvals.reg_id.lag.5ya_v8)), 
          p = list(unlist(pvals.reg_id.lag.5ya_v1), unlist(pvals.reg_id.lag.5ya_v2), unlist(pvals.reg_id.lag.5ya_v3), unlist(pvals.reg_id.lag.5ya_v4), unlist(pvals.reg_id.lag.5ya_v5), unlist(pvals.reg_id.lag.5ya_v6), unlist(pvals.reg_id.lag.5ya_v7), unlist(pvals.reg_id.lag.5ya_v8)),
          type='html', out=here("output/Table A 9 (reg 5year avg).htm"))



## (C) NAIRU_gap extension
# Table A 10, Modifying the lagged baseline regression by using NAIRU_GAP instead of FEGAP

reg_lag.NAIRU_gap_v1 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 , 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_lag.NAIRU_gap_v2 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_lag.NAIRU_gap_v3 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_lag.NAIRU_gap_v4 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_lag.NAIRU_gap_v5 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1, # + lag(UDENS)*lag(LRG_pg), 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_lag.NAIRU_gap_v6 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              Eighties + Nineties + FinancialCrisis + EuroCrisis, 
                            index=c("ccode", "year"), model="within", effect="individual", data=data_cons)
reg_lag.NAIRU_gap_v7 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              NAIRU_gap.lag1*DUEP_EUUK , 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_lag.NAIRU_gap_v8 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              NAIRU_gap.lag1*DCOU_AUT + NAIRU_gap.lag1*DCOU_DEU + NAIRU_gap.lag1*DCOU_FIN + NAIRU_gap.lag1*DCOU_SWE + NAIRU_gap.lag1*DCOU_GBR , 
                            index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)

#preparation for stargazer tables
ses.reg_lag.NAIRU_gap_v1 <- list(coeftest(reg_lag.NAIRU_gap_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v1 <- list(coeftest(reg_lag.NAIRU_gap_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v1 <- list(coeftest(reg_lag.NAIRU_gap_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v2 <- list(coeftest(reg_lag.NAIRU_gap_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v2 <- list(coeftest(reg_lag.NAIRU_gap_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v2 <- list(coeftest(reg_lag.NAIRU_gap_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v3 <- list(coeftest(reg_lag.NAIRU_gap_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v3 <- list(coeftest(reg_lag.NAIRU_gap_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v3 <- list(coeftest(reg_lag.NAIRU_gap_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v4 <- list(coeftest(reg_lag.NAIRU_gap_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v4 <- list(coeftest(reg_lag.NAIRU_gap_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v4 <- list(coeftest(reg_lag.NAIRU_gap_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v5 <- list(coeftest(reg_lag.NAIRU_gap_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v5 <- list(coeftest(reg_lag.NAIRU_gap_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v5 <- list(coeftest(reg_lag.NAIRU_gap_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v6 <- list(coeftest(reg_lag.NAIRU_gap_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v6 <- list(coeftest(reg_lag.NAIRU_gap_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v6 <- list(coeftest(reg_lag.NAIRU_gap_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v7 <- list(coeftest(reg_lag.NAIRU_gap_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v7 <- list(coeftest(reg_lag.NAIRU_gap_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v7 <- list(coeftest(reg_lag.NAIRU_gap_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.NAIRU_gap_v8 <- list(coeftest(reg_lag.NAIRU_gap_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.NAIRU_gap_v8 <- list(coeftest(reg_lag.NAIRU_gap_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.NAIRU_gap_v8 <- list(coeftest(reg_lag.NAIRU_gap_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_lag.NAIRU_gap_v1, reg_lag.NAIRU_gap_v2, reg_lag.NAIRU_gap_v3, reg_lag.NAIRU_gap_v4, reg_lag.NAIRU_gap_v5, reg_lag.NAIRU_gap_v6, reg_lag.NAIRU_gap_v7, reg_lag.NAIRU_gap_v8,  
          se= list(unlist(ses.reg_lag.NAIRU_gap_v1),   unlist(ses.reg_lag.NAIRU_gap_v2),    unlist(ses.reg_lag.NAIRU_gap_v3),   unlist(ses.reg_lag.NAIRU_gap_v4),   unlist(ses.reg_lag.NAIRU_gap_v5),   unlist(ses.reg_lag.NAIRU_gap_v6),   unlist(ses.reg_lag.NAIRU_gap_v7),   unlist(ses.reg_lag.NAIRU_gap_v8)), 
          t = list(unlist(tvals.reg_lag.NAIRU_gap_v1), unlist(tvals.reg_lag.NAIRU_gap_v2),  unlist(tvals.reg_lag.NAIRU_gap_v3), unlist(tvals.reg_lag.NAIRU_gap_v4), unlist(tvals.reg_lag.NAIRU_gap_v5), unlist(tvals.reg_lag.NAIRU_gap_v6), unlist(tvals.reg_lag.NAIRU_gap_v7), unlist(tvals.reg_lag.NAIRU_gap_v8)), 
          p = list(unlist(pvals.reg_lag.NAIRU_gap_v1), unlist(pvals.reg_lag.NAIRU_gap_v2),  unlist(pvals.reg_lag.NAIRU_gap_v3), unlist(pvals.reg_lag.NAIRU_gap_v4), unlist(pvals.reg_lag.NAIRU_gap_v5), unlist(pvals.reg_lag.NAIRU_gap_v6), unlist(pvals.reg_lag.NAIRU_gap_v7), unlist(pvals.reg_lag.NAIRU_gap_v8)),
          type='html', out=here("output/Table A 9 (NAIRUGAP).htm"))



## D) additional regressions, w/o so far omitted regressors
# Table A 11, with add'l regressors: OG, LTU, PUCA, PRCA, LR_cp
testdat <- data_cons
testdat[testdat$ccode=='FIN' & testdat$year>=1980, 'LTU'] <- imputeTS::na_kalman(testdat[testdat$ccode=='FIN' & testdat$year>=1980,'LTU']) #..imputing values for balanced panel
testdat[testdat$ccode=='FIN', 'LTU.lag1'] <- lag(testdat[testdat$ccode=='FIN', 'LTU'])

# OG / alternative business cycle measurement
reg_lag.rob_v1 <- plm(FEGAP ~  FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 + 
                        OG.lag1, 
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
ses.reg_lag.rob_v1 <- list(coeftest(reg_lag.rob_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.rob_v1 <- list(coeftest(reg_lag.rob_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.rob_v1 <- list(coeftest(reg_lag.rob_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

# LTU / accounting for unemployment hysteresis 
reg_lag.rob_v2 <- plm(FEGAP ~             + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 + 
                        LTU.lag1 , 
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
ses.reg_lag.rob_v2 <- list(coeftest(reg_lag.rob_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.rob_v2 <- list(coeftest(reg_lag.rob_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.rob_v2 <- list(coeftest(reg_lag.rob_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

# PUCA & PRCA / capital accumulation
reg_lag.rob_v3 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 +            + INFL.lag1 + LRG_pg.lag1 + 
                        PRCA.lag1 + PUCA.lag1, 
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
ses.reg_lag.rob_v3 <- list(coeftest(reg_lag.rob_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.rob_v3 <- list(coeftest(reg_lag.rob_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.rob_v3 <- list(coeftest(reg_lag.rob_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

# LR_cp / different political left-right variable
reg_lag.rob_v4 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + 
                        LRG_cp.lag1, 
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
ses.reg_lag.rob_v4 <- list(coeftest(reg_lag.rob_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.rob_v4 <- list(coeftest(reg_lag.rob_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.rob_v4 <- list(coeftest(reg_lag.rob_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
#ses.reg_lag.rob_v5 <- list(coeftest(reg_lag.rob_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
#tvals.reg_lag.rob_v5 <- list(coeftest(reg_lag.rob_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
#pvals.reg_lag.rob_v5 <- list(coeftest(reg_lag.rob_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors



# PS / indirect power factor
#reg_base_v5c <- plm(FEGAP ~  EPL + UDENS + TFP + TGLOB + PUCA + PRCA + INFL + PS, index=c("ccode", "year"), model="within", effect="twoways", data=data)
#summary(reg_base_v5c)
#coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))
#ses.reg_base_v5c <- list(coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
#tvals.reg_base_v5c <- list(coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
#pvals.reg_base_v5c <- list(coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors


#stargazer table
stargazer(reg_id.lag_v5, reg_lag.rob_v1, reg_lag.rob_v2, reg_lag.rob_v3, reg_lag.rob_v4,
          se= list(unlist(ses.reg_id.lag_v5),   unlist(ses.reg_lag.rob_v1),   unlist(ses.reg_lag.rob_v2),   unlist(ses.reg_lag.rob_v3),   unlist(ses.reg_lag.rob_v4)   ), 
          t = list(unlist(tvals.reg_id.lag_v5), unlist(tvals.reg_lag.rob_v1), unlist(tvals.reg_lag.rob_v2), unlist(tvals.reg_lag.rob_v3), unlist(tvals.reg_lag.rob_v4) ), 
          p = list(unlist(pvals.reg_id.lag_v5), unlist(pvals.reg_lag.rob_v1), unlist(pvals.reg_lag.rob_v2), unlist(pvals.reg_lag.rob_v3), unlist(pvals.reg_lag.rob_v4) ),
          type='html', out=here("output/Table A 11 (addl regressors).htm"))


## E) Adding euro introduction regression check-ups
# Table A 12, evaluating the determinants for the periods of the Maastricht treaty, Euro access
data_cons <- data_cons %>% 
  mutate(DMAAS = ifelse((DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1) & year>=1993, 1, 0),
         DEURO = ifelse((DCOU_AUT==1 | DCOU_DEU==1 | DCOU_FIN==1) & year>=1999, 1, 0),
         DMAAS_AUT = ifelse(DCOU_AUT==1 & year>=1993, 1, 0),
         DMAAS_DEU = ifelse(DCOU_DEU==1 & year>=1993, 1, 0),
         DMAAS_FIN = ifelse(DCOU_FIN==1 & year>=1993, 1, 0),
         # DMAAS_GBR = ifelse(DCOU_GBR==1 & year>=1993 & year<=2020, 1, 0),  #UK never intended to enter EMU, and hence was not subject to Maastricht criteria (yet, other fiscaul rules, e.g. via SGP)
         DEURO_AUT = ifelse(DCOU_AUT==1 & year>=1999, 1, 0),
         DEURO_DEU = ifelse(DCOU_DEU==1 & year>=1999, 1, 0),
         DEURO_FIN = ifelse(DCOU_FIN==1 & year>=1999, 1, 0))

#1) Benchmark
reg_lag.euro_v1 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons)
#2) Benchmark + interaction DEUP x FEGAP.lag1
reg_lag.euro_v2 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons)
#3) Maastricht treaty period (until Euro introduction)
reg_lag.euro_v3 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1 + DMAAS, #DMAAS_AUT + DMAAS_DEU + DMAAS_FIN, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons)
#4) Euro introduction and henceforth
reg_lag.euro_v4 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1 + DEURO, #DEURO_AUT + DEURO_DEU + DEURO_FIN, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons)
#5) Time filter pre-maastricht for all
reg_lag.euro_v5 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons %>% filter(year<1993))
#6) Time filter maastr.-euro for all
reg_lag.euro_v6 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons %>% filter(year>=1993 & year<1999))
#7) Time filter euro-financial crisis
reg_lag.euro_v7 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons %>% filter(year>=1999 & year<2008))
#8) Time filter financial crisis and afterwards
reg_lag.euro_v8 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons %>% filter(year>=2008))
#9) Time filter 2000-2022/2019
reg_lag.euro_v9 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                         DCLU_EU:FEGAP.lag1, 
                       index=c("ccode", "year"), model="within", effect="twoways", 
                       data=data_cons %>% filter(year>=2000))


#preparation for stargazer tables
ses.reg_lag.euro_v1 <- list(coeftest(reg_lag.euro_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v1 <- list(coeftest(reg_lag.euro_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v1 <- list(coeftest(reg_lag.euro_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v2 <- list(coeftest(reg_lag.euro_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v2 <- list(coeftest(reg_lag.euro_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v2 <- list(coeftest(reg_lag.euro_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v3 <- list(coeftest(reg_lag.euro_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v3 <- list(coeftest(reg_lag.euro_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v3 <- list(coeftest(reg_lag.euro_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v4 <- list(coeftest(reg_lag.euro_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v4 <- list(coeftest(reg_lag.euro_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v4 <- list(coeftest(reg_lag.euro_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v5 <- list(coeftest(reg_lag.euro_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v5 <- list(coeftest(reg_lag.euro_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v5 <- list(coeftest(reg_lag.euro_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v6 <- list(coeftest(reg_lag.euro_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v6 <- list(coeftest(reg_lag.euro_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v6 <- list(coeftest(reg_lag.euro_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v7 <- list(coeftest(reg_lag.euro_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v7 <- list(coeftest(reg_lag.euro_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v7 <- list(coeftest(reg_lag.euro_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v8 <- list(coeftest(reg_lag.euro_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v8 <- list(coeftest(reg_lag.euro_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v8 <- list(coeftest(reg_lag.euro_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_lag.euro_v9 <- list(coeftest(reg_lag.euro_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_lag.euro_v9 <- list(coeftest(reg_lag.euro_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_lag.euro_v9 <- list(coeftest(reg_lag.euro_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_lag.euro_v1, reg_lag.euro_v2, reg_lag.euro_v3, reg_lag.euro_v4, reg_lag.euro_v5, reg_lag.euro_v6, reg_lag.euro_v7, reg_lag.euro_v8, reg_lag.euro_v9,  
          se= list(unlist(ses.reg_lag.euro_v1),   unlist(ses.reg_lag.euro_v2),    unlist(ses.reg_lag.euro_v3),   unlist(ses.reg_lag.euro_v4),   unlist(ses.reg_lag.euro_v5),   unlist(ses.reg_lag.euro_v6),   unlist(ses.reg_lag.euro_v7),   unlist(ses.reg_lag.euro_v8),   unlist(ses.reg_lag.euro_v9)), 
          t = list(unlist(tvals.reg_lag.euro_v1), unlist(tvals.reg_lag.euro_v2),  unlist(tvals.reg_lag.euro_v3), unlist(tvals.reg_lag.euro_v4), unlist(tvals.reg_lag.euro_v5), unlist(tvals.reg_lag.euro_v6), unlist(tvals.reg_lag.euro_v7), unlist(tvals.reg_lag.euro_v8), unlist(tvals.reg_lag.euro_v9)), 
          p = list(unlist(pvals.reg_lag.euro_v1), unlist(pvals.reg_lag.euro_v2),  unlist(pvals.reg_lag.euro_v3), unlist(pvals.reg_lag.euro_v4), unlist(pvals.reg_lag.euro_v5), unlist(pvals.reg_lag.euro_v6), unlist(pvals.reg_lag.euro_v7), unlist(pvals.reg_lag.euro_v8), unlist(pvals.reg_lag.euro_v9)),
          type='html', out=here("output/Table A 12 (single Maastricht, Euro).htm"))


## F.1) Extending the time specific analysis by decade-based subset regressions with interaction dummies - using FEGAPS
# Table A 13, Decade subset regressions with interactions

#1) Benchmark
reg_rob.dec_FEGAP_v1 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons)
#2) Benchmark + interaction DEUP x FEGAP.lag1
reg_rob.dec_FEGAP_v2 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              DCLU_EU:FEGAP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons)
#3) 70s period
reg_rob.dec_FEGAP_v3 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              DCLU_EU:FEGAP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons %>% filter(year<1980))
#4) 80s period
reg_rob.dec_FEGAP_v4 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              DCLU_EU:FEGAP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons %>% filter(year<1990 & year>= 1980))
#5) 90s period
reg_rob.dec_FEGAP_v5 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              DCLU_EU:FEGAP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons %>% filter(year<2000 & year>=1990))
#6) 2000s period
reg_rob.dec_FEGAP_v6 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              DCLU_EU:FEGAP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons %>% filter(year<2010 & year>=2000))
#7) 2010s period
reg_rob.dec_FEGAP_v7 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                              DCLU_EU:FEGAP.lag1, 
                            index=c("ccode", "year"), model="within", effect="twoways", 
                            data=data_cons  %>% filter(year<2020 & year>=2010))

#preparation for stargazer tables
ses.reg_rob.dec_FEGAP_v1 <- list(coeftest(reg_rob.dec_FEGAP_v1,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v1 <- list(coeftest(reg_rob.dec_FEGAP_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v1 <- list(coeftest(reg_rob.dec_FEGAP_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_FEGAP_v2 <- list(coeftest(reg_rob.dec_FEGAP_v2,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v2 <- list(coeftest(reg_rob.dec_FEGAP_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v2 <- list(coeftest(reg_rob.dec_FEGAP_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_FEGAP_v3 <- list(coeftest(reg_rob.dec_FEGAP_v3,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v3 <- list(coeftest(reg_rob.dec_FEGAP_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v3 <- list(coeftest(reg_rob.dec_FEGAP_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_FEGAP_v4 <- list(coeftest(reg_rob.dec_FEGAP_v4,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v4 <- list(coeftest(reg_rob.dec_FEGAP_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v4 <- list(coeftest(reg_rob.dec_FEGAP_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_FEGAP_v5 <- list(coeftest(reg_rob.dec_FEGAP_v5,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v5 <- list(coeftest(reg_rob.dec_FEGAP_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v5 <- list(coeftest(reg_rob.dec_FEGAP_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_FEGAP_v6 <- list(coeftest(reg_rob.dec_FEGAP_v6,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v6 <- list(coeftest(reg_rob.dec_FEGAP_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v6 <- list(coeftest(reg_rob.dec_FEGAP_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_FEGAP_v7 <- list(coeftest(reg_rob.dec_FEGAP_v7,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_FEGAP_v7 <- list(coeftest(reg_rob.dec_FEGAP_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_FEGAP_v7 <- list(coeftest(reg_rob.dec_FEGAP_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_rob.dec_FEGAP_v1, reg_rob.dec_FEGAP_v2, reg_rob.dec_FEGAP_v3, reg_rob.dec_FEGAP_v4, reg_rob.dec_FEGAP_v5, reg_rob.dec_FEGAP_v6, reg_rob.dec_FEGAP_v7,   
          se= list(unlist(ses.reg_rob.dec_FEGAP_v1),   unlist(ses.reg_rob.dec_FEGAP_v2),    unlist(ses.reg_rob.dec_FEGAP_v3),   unlist(ses.reg_rob.dec_FEGAP_v4),   unlist(ses.reg_rob.dec_FEGAP_v5),   unlist(ses.reg_rob.dec_FEGAP_v6),   unlist(ses.reg_rob.dec_FEGAP_v7)), 
          t = list(unlist(tvals.reg_rob.dec_FEGAP_v1), unlist(tvals.reg_rob.dec_FEGAP_v2),  unlist(tvals.reg_rob.dec_FEGAP_v3), unlist(tvals.reg_rob.dec_FEGAP_v4), unlist(tvals.reg_rob.dec_FEGAP_v5), unlist(tvals.reg_rob.dec_FEGAP_v6), unlist(tvals.reg_rob.dec_FEGAP_v7)), 
          p = list(unlist(pvals.reg_rob.dec_FEGAP_v1), unlist(pvals.reg_rob.dec_FEGAP_v2),  unlist(pvals.reg_rob.dec_FEGAP_v3), unlist(pvals.reg_rob.dec_FEGAP_v4), unlist(pvals.reg_rob.dec_FEGAP_v5),   unlist(pvals.reg_rob.dec_FEGAP_v6), unlist(pvals.reg_rob.dec_FEGAP_v7)),
          type='html', out=here("output/Table A 13 (decades - fegap).htm"))



## F.2) Extending the time specific analysis by decade-based subset regressions with interaction dummies - using NAIRUGAPS
# Table A 14, Decade subset regressions with interactions
reg_rob.dec_NAIRUGAP_v1 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons)
#2) Benchmark + interaction DEUP x NAIRU_gap.lag1
reg_rob.dec_NAIRUGAP_v2 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                                 DCLU_EU:NAIRU_gap.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons)
#3) 70s period
reg_rob.dec_NAIRUGAP_v3 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                                 DCLU_EU:NAIRU_gap.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons %>% filter(year<1980))
#4) 80s period
reg_rob.dec_NAIRUGAP_v4 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                                 DCLU_EU:NAIRU_gap.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons %>% filter(year<1990 & year>= 1980))
#5) 90s period
reg_rob.dec_NAIRUGAP_v5 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                                 DCLU_EU:NAIRU_gap.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons %>% filter(year<2000 & year>=1990))
#6) 2000s period
reg_rob.dec_NAIRUGAP_v6 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                                 DCLU_EU:NAIRU_gap.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons %>% filter(year<2010 & year>=2000))
#7) 2010s period
reg_rob.dec_NAIRUGAP_v7 <- plm(NAIRU_gap ~ NAIRU_gap.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                                 DCLU_EU:NAIRU_gap.lag1, 
                               index=c("ccode", "year"), model="within", effect="twoways", 
                               data=data_cons  %>% filter(year<2020 & year>=2010))

#preparation for stargazer tables
ses.reg_rob.dec_NAIRUGAP_v1 <- list(coeftest(reg_rob.dec_NAIRUGAP_v1,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v1 <- list(coeftest(reg_rob.dec_NAIRUGAP_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v1 <- list(coeftest(reg_rob.dec_NAIRUGAP_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_NAIRUGAP_v2 <- list(coeftest(reg_rob.dec_NAIRUGAP_v2,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v2 <- list(coeftest(reg_rob.dec_NAIRUGAP_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v2 <- list(coeftest(reg_rob.dec_NAIRUGAP_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_NAIRUGAP_v3 <- list(coeftest(reg_rob.dec_NAIRUGAP_v3,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v3 <- list(coeftest(reg_rob.dec_NAIRUGAP_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v3 <- list(coeftest(reg_rob.dec_NAIRUGAP_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_NAIRUGAP_v4 <- list(coeftest(reg_rob.dec_NAIRUGAP_v4,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v4 <- list(coeftest(reg_rob.dec_NAIRUGAP_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v4 <- list(coeftest(reg_rob.dec_NAIRUGAP_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_NAIRUGAP_v5 <- list(coeftest(reg_rob.dec_NAIRUGAP_v5,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v5 <- list(coeftest(reg_rob.dec_NAIRUGAP_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v5 <- list(coeftest(reg_rob.dec_NAIRUGAP_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_NAIRUGAP_v6 <- list(coeftest(reg_rob.dec_NAIRUGAP_v6,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v6 <- list(coeftest(reg_rob.dec_NAIRUGAP_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v6 <- list(coeftest(reg_rob.dec_NAIRUGAP_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_rob.dec_NAIRUGAP_v7 <- list(coeftest(reg_rob.dec_NAIRUGAP_v7,   vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.dec_NAIRUGAP_v7 <- list(coeftest(reg_rob.dec_NAIRUGAP_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.dec_NAIRUGAP_v7 <- list(coeftest(reg_rob.dec_NAIRUGAP_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_rob.dec_NAIRUGAP_v1, reg_rob.dec_NAIRUGAP_v2, reg_rob.dec_NAIRUGAP_v3, reg_rob.dec_NAIRUGAP_v4, reg_rob.dec_NAIRUGAP_v5, reg_rob.dec_NAIRUGAP_v6, reg_rob.dec_NAIRUGAP_v7,   
          se= list(unlist(ses.reg_rob.dec_NAIRUGAP_v1),   unlist(ses.reg_rob.dec_NAIRUGAP_v2),    unlist(ses.reg_rob.dec_NAIRUGAP_v3),   unlist(ses.reg_rob.dec_NAIRUGAP_v4),   unlist(ses.reg_rob.dec_NAIRUGAP_v5),   unlist(ses.reg_rob.dec_NAIRUGAP_v6),   unlist(ses.reg_rob.dec_NAIRUGAP_v7)), 
          t = list(unlist(tvals.reg_rob.dec_NAIRUGAP_v1), unlist(tvals.reg_rob.dec_NAIRUGAP_v2),  unlist(tvals.reg_rob.dec_NAIRUGAP_v3), unlist(tvals.reg_rob.dec_NAIRUGAP_v4), unlist(tvals.reg_rob.dec_NAIRUGAP_v5), unlist(tvals.reg_rob.dec_NAIRUGAP_v6), unlist(tvals.reg_rob.dec_NAIRUGAP_v7)), 
          p = list(unlist(pvals.reg_rob.dec_NAIRUGAP_v1), unlist(pvals.reg_rob.dec_NAIRUGAP_v2),  unlist(pvals.reg_rob.dec_NAIRUGAP_v3), unlist(pvals.reg_rob.dec_NAIRUGAP_v4), unlist(pvals.reg_rob.dec_NAIRUGAP_v5),   unlist(pvals.reg_rob.dec_NAIRUGAP_v6), unlist(pvals.reg_rob.dec_NAIRUGAP_v7)),
          type='html', out=here("output/Table A 14 (decades - nairugap).htm"))



## G) comparing EU with US for regs                                                        
# Table A 14, analyzing determinants of FEGAPS for EU vs US
reg_rob.uep_v1 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 ,
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_rob.uep_v2 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                        DCLU_US:FEGAP.lag1 + DCLU_US:EPL.lag1 + DCLU_US:UDENS.lag1 + DCLU_US:TFP.lag1 + DCLU_US:EGLOB.lag1 + DCLU_US:ACTPOP.lag1 + DCLU_US:ACCU.lag1 + DCLU_US:INFL.lag1 + DCLU_US:LRG_pg.lag1,
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_rob.uep_v3 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                        DCLU_EU:FEGAP.lag1 + DCLU_EU:EPL.lag1 + DCLU_EU:UDENS.lag1 + DCLU_EU:TFP.lag1 + DCLU_EU:EGLOB.lag1 + DCLU_EU:ACTPOP.lag1 + DCLU_EU:ACCU.lag1 + DCLU_EU:INFL.lag1 + DCLU_EU:LRG_pg.lag1,
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_rob.uep_v4 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + PUCA.lag1 + PRCA.lag1 + INFL.lag1 + LRG_pg.lag1 +
                        DCLU_US:FEGAP.lag1 + DCLU_US:EPL.lag1 + DCLU_US:UDENS.lag1 + DCLU_US:TFP.lag1 + DCLU_US:EGLOB.lag1 + DCLU_US:ACTPOP.lag1 + DCLU_US:PUCA.lag1 + DCLU_US:PRCA.lag1 + DCLU_US:INFL.lag1 + DCLU_US:LRG_pg.lag1,
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
reg_rob.uep_v5 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + PUCA.lag1 + PRCA.lag1 + INFL.lag1 + LRG_pg.lag1 +
                        DCLU_EU:FEGAP.lag1 + DCLU_EU:EPL.lag1 + DCLU_EU:UDENS.lag1 + DCLU_EU:TFP.lag1 + DCLU_EU:EGLOB.lag1 + DCLU_EU:ACTPOP.lag1 + DCLU_EU:PUCA.lag1 + DCLU_EU:PRCA.lag1 + DCLU_EU:INFL.lag1 + DCLU_EU:LRG_pg.lag1,
                      index=c("ccode", "year"), model="within", effect="twoways", data=data_cons)
#potential extension for subset of 2000-2020, but avoided due to huge dimension of table
# reg_rob.uep_v6 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
#                         DCLU_US:FEGAP.lag1 + DCLU_US:EPL.lag1 + DCLU_US:UDENS.lag1 + DCLU_US:TFP.lag1 + DCLU_US:EGLOB.lag1 + DCLU_US:ACTPOP.lag1 + DCLU_US:ACCU.lag1 + DCLU_US:INFL.lag1 + DCLU_US:LRG_pg.lag1,
#                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons %>% filter(year>=2000))
# reg_rob.uep_v7 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
#                         DCLU_EU:FEGAP.lag1 + DCLU_EU:EPL.lag1 + DCLU_EU:UDENS.lag1 + DCLU_EU:TFP.lag1 + DCLU_EU:EGLOB.lag1 + DCLU_EU:ACTPOP.lag1 + DCLU_EU:ACCU.lag1 + DCLU_EU:INFL.lag1 + DCLU_EU:LRG_pg.lag1,
#                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons %>% filter(year>=2000))
# reg_rob.uep_v8 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + PUCA.lag1 + PRCA.lag1 + INFL.lag1 + LRG_pg.lag1 +
#                         DCLU_US:FEGAP.lag1 + DCLU_US:EPL.lag1 + DCLU_US:UDENS.lag1 + DCLU_US:TFP.lag1 + DCLU_US:EGLOB.lag1 + DCLU_US:ACTPOP.lag1 + DCLU_US:PUCA.lag1 + DCLU_US:PRCA.lag1 + DCLU_US:INFL.lag1 + DCLU_US:LRG_pg.lag1,
#                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons %>% filter(year>=2000))
# reg_rob.uep_v9 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + PUCA.lag1 + PRCA.lag1 + INFL.lag1 + LRG_pg.lag1 +
#                         DCLU_EU:FEGAP.lag1 + DCLU_EU:EPL.lag1 + DCLU_EU:UDENS.lag1 + DCLU_EU:TFP.lag1 + DCLU_EU:EGLOB.lag1 + DCLU_EU:ACTPOP.lag1 + DCLU_EU:PUCA.lag1 + DCLU_EU:PRCA.lag1 + DCLU_EU:INFL.lag1 + DCLU_EU:LRG_pg.lag1,
#                       index=c("ccode", "year"), model="within", effect="twoways", data=data_cons %>% filter(year>=2000))


ses.reg_rob.uep_v1 <- list(coeftest(reg_rob.uep_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.uep_v1 <- list(coeftest(reg_rob.uep_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.uep_v1 <- list(coeftest(reg_rob.uep_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_rob.uep_v2 <- list(coeftest(reg_rob.uep_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.uep_v2 <- list(coeftest(reg_rob.uep_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.uep_v2 <- list(coeftest(reg_rob.uep_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_rob.uep_v3 <- list(coeftest(reg_rob.uep_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.uep_v3 <- list(coeftest(reg_rob.uep_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.uep_v3 <- list(coeftest(reg_rob.uep_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_rob.uep_v4 <- list(coeftest(reg_rob.uep_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.uep_v4 <- list(coeftest(reg_rob.uep_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.uep_v4 <- list(coeftest(reg_rob.uep_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_rob.uep_v5 <- list(coeftest(reg_rob.uep_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob.uep_v5 <- list(coeftest(reg_rob.uep_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob.uep_v5 <- list(coeftest(reg_rob.uep_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_rob.uep_v6 <- list(coeftest(reg_rob.uep_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
# tvals.reg_rob.uep_v6 <- list(coeftest(reg_rob.uep_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
# pvals.reg_rob.uep_v6 <- list(coeftest(reg_rob.uep_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
# ses.reg_rob.uep_v7 <- list(coeftest(reg_rob.uep_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
# tvals.reg_rob.uep_v7 <- list(coeftest(reg_rob.uep_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
# pvals.reg_rob.uep_v7 <- list(coeftest(reg_rob.uep_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
# ses.reg_rob.uep_v8 <- list(coeftest(reg_rob.uep_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
# tvals.reg_rob.uep_v8 <- list(coeftest(reg_rob.uep_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
# pvals.reg_rob.uep_v8 <- list(coeftest(reg_rob.uep_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
# ses.reg_rob.uep_v9 <- list(coeftest(reg_rob.uep_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
# tvals.reg_rob.uep_v9 <- list(coeftest(reg_rob.uep_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
# pvals.reg_rob.uep_v9 <- list(coeftest(reg_rob.uep_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

stargazer(reg_rob.uep_v1, reg_rob.uep_v2, reg_rob.uep_v3, reg_rob.uep_v4, reg_rob.uep_v5, reg_rob.uep_v6, #reg_rob.uep_v7, reg_rob.uep_v8, reg_rob.uep_v9,
          se= list(unlist(ses.reg_rob.uep_v1),   unlist(ses.reg_rob.uep_v2),    unlist(ses.reg_rob.uep_v3),   unlist(ses.reg_rob.uep_v4),   unlist(ses.reg_rob.uep_v5),   unlist(ses.reg_rob.uep_v6)), #,   unlist(ses.reg_rob.uep_v7),   unlist(ses.reg_rob.uep_v8),   unlist(ses.reg_rob.uep_v9)), 
          t = list(unlist(tvals.reg_rob.uep_v1), unlist(tvals.reg_rob.uep_v2),  unlist(tvals.reg_rob.uep_v3), unlist(tvals.reg_rob.uep_v4), unlist(tvals.reg_rob.uep_v5), unlist(tvals.reg_rob.uep_v6)), #, unlist(tvals.reg_rob.uep_v7), unlist(tvals.reg_rob.uep_v8), unlist(tvals.reg_rob.uep_v9)), 
          p = list(unlist(pvals.reg_rob.uep_v1), unlist(pvals.reg_rob.uep_v2),  unlist(pvals.reg_rob.uep_v3), unlist(pvals.reg_rob.uep_v4), unlist(pvals.reg_rob.uep_v5), unlist(pvals.reg_rob.uep_v6)), #, unlist(pvals.reg_rob.uep_v7), unlist(pvals.reg_rob.uep_v8), unlist(pvals.reg_rob.uep_v9)),
          type='html', out=here("output/Table A 15 EU-UEP EUUK vs US_PUCA PRCA.htm"))




## H) applying regression framework to 25 countries sample between 2000 and 2022
# Table A 16, analyzing determinants of FEGAPS in the short period panel with the extended countries sample of 25 countries

#i) obtain panel for shorter period
shortdata <- fread(here("data/full-employment-regs_short-panel_final.csv"), check.names = FALSE, header=TRUE) #25 countries
shortdata <- shortdata %>% dplyr::rename(ACTPOP = ACTPOP_growth)

#modifying political left-right scale for easier interpretation with the interaction effects
#.. before 0='far left', 10='far right', NOW: 0='far right', 10='far left'
shortdata$LRG_pg <- 10 - shortdata$LRG_pg 
shortdata$LRG_cp <- 10 - shortdata$LRG_cp

shortdata.28c <- shortdata
shortdata.25c <- subset(shortdata, !(ccode %in% c('CZE', 'HRV', 'LUX'))) #excluding obsrvts with only admin and no survey data 
shortdata.20c <- subset(shortdata, !(ccode %in% c('BEL', 'FRA', 'GRC', 'HRV', 'IRL', 'MLT', 'POL', 'PRT'))) #2006 onwards
shortdata.15c <- subset(shortdata, !(ccode %in% c('BEL', 'FRA', 'GRC', 'HRV', 'IRL', 'MLT', 'POL', 'PRT', 'HUN', 'LVA', 'ROU', 'SVK', 'LTU'))) #2004-2022
shortdata.14c <- subset(shortdata, !(ccode %in% c('BEL', 'FRA', 'GRC', 'HRV', 'IRL', 'MLT', 'POL', 'PRT', 'HUN', 'LVA', 'ROU', 'SVK', 'ITA', 'LUX'))) #2001 onwards

shortdata <- shortdata.25c #shortdata.20c #shortdata.16c


#ii) obtaining consistent dataset
shortdata <- shortdata %>% 
  arrange(ccode, year) %>% 
  group_by(ccode) %>% 
  dplyr::mutate(FEGAP.lag1 = dplyr::lag(FEGAP, n=1, default=NA)) %>% 
  dplyr::mutate(EPL.lag1 = dplyr::lag(EPL, n=1, default=NA)) %>%      #defining lag var's here, because lag() operations within plm-regression structure
  dplyr::mutate(UDENS.lag1 = dplyr::lag(UDENS, n=1, default=NA)) %>%  #.. s.t. ignore groups (e.g. lag(UDENS) of FIN in 1971 will become UDENS val of DEU from 2020 or 2022)
  dplyr::mutate(TFP.lag1 = dplyr::lag(TFP, n=1, default=NA)) %>%
  dplyr::mutate(EGLOB.lag1 = dplyr::lag(EGLOB, n=1, default=NA)) %>%
  dplyr::mutate(ACTPOP.lag1 = dplyr::lag(ACTPOP, n=1, default=NA)) %>%
  dplyr::mutate(ACCU.lag1 = dplyr::lag(ACCU, n=1, default=NA)) %>%
  dplyr::mutate(INFL.lag1 = dplyr::lag(INFL, n=1, default=NA)) %>%
  dplyr::mutate(LRG_pg.lag1 = dplyr::lag(LRG_pg, n=1, default=NA)) %>% 
  dplyr::mutate(LRG_cp.lag1 = dplyr::lag(LRG_cp, n=1, default=NA)) %>% 
  dplyr::mutate(PUCA.lag1 = dplyr::lag(PUCA, n=1, default=NA)) %>%
  dplyr::mutate(PRCA.lag1 = dplyr::lag(PRCA, n=1, default=NA))


#iii) use CONSistently available observations 
shortdata_cons <- shortdata %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  filter(!(!is.na(EPL.lag1) & !is.na(UDENS.lag1) & (is.na(TFP.lag1) | is.na(EGLOB.lag1) | is.na(ACTPOP.lag1) ) ))

shortdata_cons_pg <- shortdata_cons %>%
  arrange(ccode, year) %>%
  ungroup() %>% 
  filter(!(!is.na(EPL.lag1) & !is.na(UDENS.lag1) & !is.na(TFP.lag1) & !is.na(EGLOB.lag1) & !is.na(ACTPOP.lag1) & 
             !is.na(ACCU.lag1) & !is.na(INFL.lag1) & ( is.na(LRG_pg.lag1) ) ))

shortdata_cons_pg <- shortdata_cons_pg %>%
  select(ccode, year, FEGAP, FEGAP.lag1, EPL.lag1,  UDENS.lag1, TFP.lag1, 
         EGLOB.lag1, ACTPOP.lag1, ACCU.lag1, INFL.lag1, LRG_pg.lag1, 
         FinancialCrisis, EuroCrisis, CovidCrisis, 
         DCLU_SOD, DCLU_CON, DCLU_MED, DCLU_EAS, DCLU_LIB,
         PUCA.lag1, PRCA.lag1) %>% 
  na.omit() 

shortdata_cons_pg <- shortdata_cons_pg[complete.cases(shortdata_cons_pg), ] #for only keeping the complete cases

#Hysteresis effects
reg_base_v1 <- plm(FEGAP ~ FEGAP.lag1 ,
                   index=c("ccode", "year"), model="within", effect="twoways", data=shortdata_cons_pg)
#Labour market institutions
reg_base_v2 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1,
                   index=c("ccode", "year"), model="within", effect="twoways", data=shortdata_cons_pg)
#Labour structural changes
reg_base_v3 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1,
                   index=c("ccode", "year"), model="within", effect="twoways", data=shortdata_cons_pg)
#adding macro factors                                    #or: + PUCA + PRCA  ..vs.. + PRINV + PUINV
reg_base_v4 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1,
                   index=c("ccode", "year"), model="within", effect="twoways", data=shortdata_cons_pg)
#adding political factors
reg_base_v5 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1,
                   index=c("ccode", "year"), model="within", effect="twoways", data=shortdata_cons_pg)
#time period dummies
reg_base_v6 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                     FinancialCrisis + EuroCrisis + CovidCrisis,
                   index=c("ccode", "year"), model="within", effect="individual", data=shortdata_cons_pg)
#welfare regime dummies
reg_base_v7 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 +
                     DCLU_SOD + DCLU_CON + DCLU_MED + DCLU_EAS + DCLU_LIB ,
                   index=c("ccode", "year"), model="within", effect="time", data=shortdata_cons_pg)
#welfare regime dummies and interactions
reg_base_v8 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 + 
                     FEGAP.lag1:DCLU_SOD + FEGAP.lag1:DCLU_CON + FEGAP.lag1:DCLU_MED + FEGAP.lag1:DCLU_EAS + FEGAP.lag1:DCLU_LIB , 
                   index=c("ccode", "year"), model="within", effect="time", data=shortdata_cons_pg )
reg_base_v9 <- plm(FEGAP ~ FEGAP.lag1 + EPL.lag1 + UDENS.lag1 + TFP.lag1 + EGLOB.lag1 + ACTPOP.lag1 + ACCU.lag1 + INFL.lag1 + LRG_pg.lag1 + 
                     FEGAP.lag1:DCLU_SOD + FEGAP.lag1:DCLU_CON + FEGAP.lag1:DCLU_MED + FEGAP.lag1:DCLU_EAS + FEGAP.lag1:DCLU_LIB , 
                   index=c("ccode", "year"), model="within", effect="time", data=shortdata_cons_pg %>% filter(ccode != 'IRL'))


#preparation for stargazer tables
ses.reg_base_v1 <- list(coeftest(reg_base_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v1 <- list(coeftest(reg_base_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v1 <- list(coeftest(reg_base_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v2 <- list(coeftest(reg_base_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v2 <- list(coeftest(reg_base_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v2 <- list(coeftest(reg_base_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v3 <- list(coeftest(reg_base_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v3 <- list(coeftest(reg_base_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v3 <- list(coeftest(reg_base_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v4 <- list(coeftest(reg_base_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v4 <- list(coeftest(reg_base_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v4 <- list(coeftest(reg_base_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v5 <- list(coeftest(reg_base_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v5 <- list(coeftest(reg_base_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v5 <- list(coeftest(reg_base_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v6 <- list(coeftest(reg_base_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v6 <- list(coeftest(reg_base_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v6 <- list(coeftest(reg_base_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v7 <- list(coeftest(reg_base_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v7 <- list(coeftest(reg_base_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v7 <- list(coeftest(reg_base_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v8 <- list(coeftest(reg_base_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v8 <- list(coeftest(reg_base_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v8 <- list(coeftest(reg_base_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v9 <- list(coeftest(reg_base_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v9 <- list(coeftest(reg_base_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v9 <- list(coeftest(reg_base_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

#stargazer table
stargazer(reg_base_v1, reg_base_v2, reg_base_v3, reg_base_v4, reg_base_v5, reg_base_v6, reg_base_v7, reg_base_v8, reg_base_v9,  
          t = list(unlist(tvals.reg_base_v1), unlist(tvals.reg_base_v2), unlist(tvals.reg_base_v3), unlist(tvals.reg_base_v4), unlist(tvals.reg_base_v5), unlist(tvals.reg_base_v6), unlist(tvals.reg_base_v7), unlist(tvals.reg_base_v8), unlist(tvals.reg_base_v9)), 
          se= list(unlist(ses.reg_base_v1),   unlist(ses.reg_base_v2),   unlist(ses.reg_base_v3),   unlist(ses.reg_base_v4),   unlist(ses.reg_base_v5),   unlist(ses.reg_base_v6),   unlist(ses.reg_base_v7),   unlist(ses.reg_base_v8),   unlist(ses.reg_base_v9)), 
          p = list(unlist(pvals.reg_base_v1), unlist(pvals.reg_base_v2), unlist(pvals.reg_base_v3), unlist(pvals.reg_base_v4), unlist(pvals.reg_base_v5), unlist(pvals.reg_base_v6), unlist(pvals.reg_base_v7), unlist(pvals.reg_base_v8), unlist(pvals.reg_base_v9)),
          type='html', out=here("output/Table A 16 (short panel reg).htm"))

