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
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
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
library(plyr)
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


#read data
data <- fread(here("full-employment-regs.csv"), check.names = FALSE, header=TRUE)
# data <- fread(here("data/full-employment-regs.csv"), check.names = FALSE, header=TRUE) #does NOT include decade dummies yet !!

#restrict sample (exclude France and Italy (currently no data) and Spain (problems with vacancy rate))
data <- subset(data, ccode %in% c('AUT', 'DEU', 'SWE', 'GBR', 'USA'))

#modifying political left-right scale for easier interpretation with the interaction effects
#.. before 0='far left', 10='far right', NOW: 0='far right', 10='far left'
data$LRG_pg <- 10 - data$LRG_pg 
data$LRG_cp <- 10 - data$LRG_cp




#basic preliminary plots
plot_var_and_fegap <- function(var_name) {
  data %>% 
    ggplot(.) +
    geom_line(aes(x=year, y=!!sym(var_name), color=ccode), linetype=1) +
    geom_line(aes(x=year, y=FEGAP, color=ccode), linetype=2) +
    labs(subtitle = paste0(var_name , ' and FEGAP (dashed)'))
}

plot_var_and_fegap("PS")
plot_var_and_fegap("ROP")
plot_var_and_fegap("LRG_pg")
plot_var_and_fegap("LRG_cp")
plot_var_and_fegap("PUINV")
plot_var_and_fegap("PRINV")
plot_var_and_fegap("FEGAP")




##### (I) PANEL REGRESSIONS

### (1) Baseline regressions

#Hysteresis effects
reg_base_v1 <- plm(FEGAP ~  lag(FEGAP) , index=c("ccode", "year"), model="within", effect="twoways", data=data)

#Labour market institutions
reg_base_v2 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS, index=c("ccode", "year"), model="within", effect="twoways", data=data)
summary(reg_base_v2)
coeftest(reg_base_v2, vcov.=function(x) vcovHC(x, type="sss"))

#Labour structural changes
reg_base_v3 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB, index=c("ccode", "year"), model="within", effect="twoways", data=data)
summary(reg_base_v3)
coeftest(reg_base_v3, vcov.=function(x) vcovHC(x, type="sss"))

#adding macro factors                                    #or: + PUCA + PRCA  ..vs.. + PRINV + PUINV
reg_base_v4a <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU , index=c("ccode", "year"), model="within", effect="twoways", data=data)
summary(reg_base_v4a)
coeftest(reg_base_v4a, vcov.=function(x) vcovHC(x, type="sss"))

#.. adding inflation                            # w/o LTU ...
reg_base_v4b <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL, index=c("ccode", "year"), model="within", effect="twoways", data=data)

#adding political factors
reg_base_v5a <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg, index=c("ccode", "year"), model="within", effect="twoways", data=data)
summary(reg_base_v5a)
coeftest(reg_base_v5a, vcov.=function(x) vcovHC(x, type="sss"))

reg_base_v5b <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg, index=c("ccode", "year"), model="within", effect="twoways", data=data)
summary(reg_base_v5b)
coeftest(reg_base_v5b, vcov.=function(x) vcovHC(x, type="sss"))


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

ses.reg_base_v4a <- list(coeftest(reg_base_v4a, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v4a <- list(coeftest(reg_base_v4a, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v4a <- list(coeftest(reg_base_v4a, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_base_v4b <- list(coeftest(reg_base_v4b, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v4b <- list(coeftest(reg_base_v4b, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v4b <- list(coeftest(reg_base_v4b, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_base_v5a <- list(coeftest(reg_base_v5a, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v5a <- list(coeftest(reg_base_v5a, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v5a <- list(coeftest(reg_base_v5a, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
ses.reg_base_v5b <- list(coeftest(reg_base_v5b, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_base_v5b <- list(coeftest(reg_base_v5b, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_base_v5b <- list(coeftest(reg_base_v5b, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors


#stargazer table
stargazer(reg_base_v1, reg_base_v2, reg_base_v3, reg_base_v4a, reg_base_v4b, reg_base_v5a, reg_base_v5b,  
          t = list(unlist(tvals.reg_base_v1), unlist(tvals.reg_base_v2), unlist(tvals.reg_base_v3), unlist(tvals.reg_base_v4a), unlist(tvals.reg_base_v4b), unlist(tvals.reg_base_v5a), unlist(tvals.reg_base_v5b)), 
          se= list(unlist(ses.reg_base_v1),   unlist(ses.reg_base_v2),   unlist(ses.reg_base_v3),   unlist(ses.reg_base_v4a),   unlist(ses.reg_base_v4b),   unlist(ses.reg_base_v5a),   unlist(ses.reg_base_v5b)), 
          p = list(unlist(pvals.reg_base_v1), unlist(pvals.reg_base_v2), unlist(pvals.reg_base_v3), unlist(pvals.reg_base_v4a), unlist(pvals.reg_base_v4b), unlist(pvals.reg_base_v5a), unlist(pvals.reg_base_v5b)),
          type='html', out="output/reg out 1 base 5 w lag, ACCU excl OG, LTU TWFE.htm")

car::vif(reg_base_v1)
library(metan)



### Extended regression by adding dummies

### (2a) specific periods                                                #excl: + OG + LTU
                                                                      #out: PUCA + PRCA
reg_ext.yr_v1 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg +
                       Eighties, index=c("ccode", "year"), model="within", effect="individual", data=data)

reg_ext.yr_v2 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg +
                       Nineties, index=c("ccode", "year"), model="within", effect="individual", data=data)

reg_ext.yr_v3 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg +
                       FinancialCrisis, index=c("ccode", "year"), model="within", effect="individual", data=data)

reg_ext.yr_v4 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg +
                       EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=data)

reg_ext.yr_v5 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg +
                       Nineties + FinancialCrisis + EuroCrisis + Eighties, index=c("ccode", "year"), model="within", effect="individual", data=data)



ses.reg_ext.yr_v1 <- list(coeftest(reg_ext.yr_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.yr_v1 <- list(coeftest(reg_ext.yr_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.yr_v1 <- list(coeftest(reg_ext.yr_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.yr_v2 <- list(coeftest(reg_ext.yr_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.yr_v2 <- list(coeftest(reg_ext.yr_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.yr_v2 <- list(coeftest(reg_ext.yr_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.yr_v3 <- list(coeftest(reg_ext.yr_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.yr_v3 <- list(coeftest(reg_ext.yr_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.yr_v3 <- list(coeftest(reg_ext.yr_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.yr_v4 <- list(coeftest(reg_ext.yr_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.yr_v4 <- list(coeftest(reg_ext.yr_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.yr_v4 <- list(coeftest(reg_ext.yr_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.yr_v5 <- list(coeftest(reg_ext.yr_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.yr_v5 <- list(coeftest(reg_ext.yr_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.yr_v5 <- list(coeftest(reg_ext.yr_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

stargazer(reg_ext.yr_v1, reg_ext.yr_v2, reg_ext.yr_v3, reg_ext.yr_v4, reg_ext.yr_v5,
          t = list(unlist(tvals.reg_ext.yr_v1), unlist(tvals.reg_ext.yr_v2), unlist(tvals.reg_ext.yr_v3), unlist(tvals.reg_ext.yr_v4), unlist(tvals.reg_ext.yr_v5)), 
          se= list(unlist(ses.reg_ext.yr_v1),   unlist(ses.reg_ext.yr_v2),   unlist(ses.reg_ext.yr_v3),   unlist(ses.reg_ext.yr_v4), unlist(ses.reg_ext.yr_v5)), 
          p = list(unlist(pvals.reg_ext.yr_v1), unlist(pvals.reg_ext.yr_v2), unlist(pvals.reg_ext.yr_v3), unlist(pvals.reg_ext.yr_v4), unlist(pvals.reg_ext.yr_v5)),
          type='html', out="output/reg out 2a ext periods 4 w lag, ACCU excl OG, LTU.htm")

fixef(reg_ext.yr_v1)
fixef(reg_ext.yr_v2)
fixef(reg_ext.yr_v3)
fixef(reg_ext.yr_v4)
fixef(reg_ext.yr_v5)


### (2b) dummies for clusters (cl) --> due to multi-collinearity reasons when including additional dummies, we run one-way fixed effects reg's
#                                                                   #excl: + OG + LTU
reg_ext.cl_v1 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                       DCLU_CON, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.cl_v2 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                       DCLU_SOD, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.cl_v3 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                       DCLU_LIB, index=c("ccode", "year"), model="within", effect="time", data=data)
 #reg_ext.cl_v3a <- lm(FEGAP ~  EPL + UDENS + TFP + TGLOB + ACCU + PUINV + PRINV + INFL + OG + LTU + PS + LRG_pg + UDENS*LRG_pg +
 #                        DCLU_LIBEU, index=c("ccode", "year"), model="within", effect="individual", data=data)
 #reg_ext.cl_v3b <- lm(FEGAP ~  EPL + UDENS + TFP + TGLOB + ACCU + PUINV + PRINV + INFL + OG + LTU + PS + LRG_pg + UDENS*LRG_pg +
 #                        DCLU_LIBUS, index=c("ccode", "year"), model="within", effect="individual", data=data)

reg_ext.cl_v4 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                        DCLU_SOD + DCLU_LIB + DCLU_CON, index=c("ccode", "year"), model="within", effect="time", data=data)

ses.reg_ext.cl_v1 <- list(coeftest(reg_ext.cl_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.cl_v1 <- list(coeftest(reg_ext.cl_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.cl_v1 <- list(coeftest(reg_ext.cl_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.cl_v2 <- list(coeftest(reg_ext.cl_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.cl_v2 <- list(coeftest(reg_ext.cl_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.cl_v2 <- list(coeftest(reg_ext.cl_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.cl_v3 <- list(coeftest(reg_ext.cl_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.cl_v3 <- list(coeftest(reg_ext.cl_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.cl_v3 <- list(coeftest(reg_ext.cl_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
#ses.reg_ext.cl_v3a <- list(coeftest(reg_ext.cl_v3a, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
#tvals.reg_ext.cl_v3a <- list(coeftest(reg_ext.cl_v3a, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
#pvals.reg_ext.cl_v3a <- list(coeftest(reg_ext.cl_v3a, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors
#ses.reg_ext.cl_v3b <- list(coeftest(reg_ext.cl_v3b, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
#tvals.reg_ext.cl_v3b <- list(coeftest(reg_ext.cl_v3b, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
#pvals.reg_ext.cl_v3b <- list(coeftest(reg_ext.cl_v3b, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.cl_v4 <- list(coeftest(reg_ext.cl_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.cl_v4 <- list(coeftest(reg_ext.cl_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.cl_v4 <- list(coeftest(reg_ext.cl_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

stargazer(reg_ext.cl_v1, reg_ext.cl_v2, reg_ext.cl_v3, reg_ext.cl_v4, #reg_ext.cl_v3a, reg_ext.cl_v3b, 
          t = list(unlist(tvals.reg_ext.cl_v1), unlist(tvals.reg_ext.cl_v2), unlist(tvals.reg_ext.cl_v3), unlist(tvals.reg_ext.cl_v4)), # unlist(tvals.reg_ext.cl_v3a), unlist(tvals.reg_ext.cl_v3b)), 
          se= list(unlist(ses.reg_ext.cl_v1),   unlist(ses.reg_ext.cl_v2),   unlist(ses.reg_ext.cl_v3),   unlist(ses.reg_ext.cl_v4)),  #unlist(ses.reg_ext.cl_v3a),   unlist(ses.reg_ext.cl_v3b)), 
          p = list(unlist(pvals.reg_ext.cl_v1), unlist(pvals.reg_ext.cl_v2), unlist(pvals.reg_ext.cl_v3), unlist(pvals.reg_ext.cl_v4)), #unlist(pvals.reg_ext.cl_v3a), unlist(pvals.reg_ext.cl_v3b)),
          type='html', out="output/reg out 2b ext cluster 3 w PUCA excl OG, LTU.htm")


### (2c) Dummies for single countries (co)
#                                                                   #excl: + OG + LTU
reg_ext.co_v1 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                      DCOU_AUT, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.co_v2 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                      DCOU_DEU, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.co_v3 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                      DCOU_SWE, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.co_v4 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                      DCOU_GBR, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.co_v5 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                      DCOU_USA, index=c("ccode", "year"), model="within", effect="time", data=data)

reg_ext.co_v6 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + TGLOB + ACCU + INFL  + LRG_pg + UDENS*LRG_pg +
                       DCOU_AUT + DCOU_DEU + DCOU_SWE + DCOU_GBR + DCOU_USA, index=c("ccode", "year"), model="within", effect="time", data=data)

ses.reg_ext.co_v1 <- list(coeftest(reg_ext.co_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.co_v1 <- list(coeftest(reg_ext.co_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.co_v1 <- list(coeftest(reg_ext.co_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.co_v2 <- list(coeftest(reg_ext.co_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.co_v2 <- list(coeftest(reg_ext.co_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.co_v2 <- list(coeftest(reg_ext.co_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.co_v3 <- list(coeftest(reg_ext.co_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.co_v3 <- list(coeftest(reg_ext.co_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.co_v3 <- list(coeftest(reg_ext.co_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.co_v4 <- list(coeftest(reg_ext.co_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.co_v4 <- list(coeftest(reg_ext.co_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.co_v4 <- list(coeftest(reg_ext.co_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.co_v5 <- list(coeftest(reg_ext.co_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.co_v5 <- list(coeftest(reg_ext.co_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.co_v5 <- list(coeftest(reg_ext.co_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

ses.reg_ext.co_v6 <- list(coeftest(reg_ext.co_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_ext.co_v6 <- list(coeftest(reg_ext.co_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_ext.co_v6 <- list(coeftest(reg_ext.co_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors


stargazer(reg_ext.co_v1, reg_ext.co_v2, reg_ext.co_v3, reg_ext.co_v4, reg_ext.co_v5, reg_ext.co_v6,
          t = list(unlist(tvals.reg_ext.co_v1), unlist(tvals.reg_ext.co_v2), unlist(tvals.reg_ext.co_v3), unlist(tvals.reg_ext.co_v4), unlist(tvals.reg_ext.co_v5), unlist(tvals.reg_ext.co_v6)), 
          se= list(unlist(ses.reg_ext.co_v1),   unlist(ses.reg_ext.co_v2),   unlist(ses.reg_ext.co_v3),   unlist(ses.reg_ext.co_v4),   unlist(ses.reg_ext.co_v5),   unlist(ses.reg_ext.co_v6)),  
          p = list(unlist(pvals.reg_ext.co_v1), unlist(pvals.reg_ext.co_v2), unlist(pvals.reg_ext.co_v3), unlist(pvals.reg_ext.co_v4), unlist(pvals.reg_ext.co_v5), unlist(pvals.reg_ext.co_v6)), 
          type='html', out="output/reg out 2c ext countries 4 w lag, ACCU, excl OG, LTU.htm")






#### (II) PRE-TESTING

### (II.1) multicollinearity testing

# e.g. https://www.codingprof.com/3-ways-to-test-for-multicollinearity-in-r-examples/
# good overview, https://anyi-guo.medium.com/correlation-pearson-vs-spearman-c15e581c12ce
# generally note, 'plm silently drops perfect collinear variables'
#a) `corr`, `corrplot`, `ggpairs` - correlation indices below 0.9 good
ggpairs(data, columns=c("FEGAP", "EPL", "UDENS", "TFP", "EGLOB", "PUCA", "PRCA","INFL", "OG", "LTU", "LRG_pg", "PS"))

cor_vars <- data %>% 
  mutate(lagFEGAP = lag(FEGAP, n=1) ) %>%  #, lag2FEGAP = lag(FEGAP, n=2)) %>% 
  select("FEGAP", "lagFEGAP", "EPL", "UDENS", "TFP", "EGLOB", "ACCU", "INFL", "LRG_pg", "LTU", "PUCA", "PRCA", "OG") %>% 
  #select("FEGAP", "lagFEGAP", "lag2FEGAP", "EPL", "UDENS", "TFP", "EGLOB", "ACCU", "INFL", "LRG_pg", "LTU", "PUCA", "PRCA", "OG") %>% 
  as.data.frame(.)

cor_vars %>% cor(., use="na.or.complete", method='spearman') #'pearson' is the default method in cor
cor_vars %>% corr.test(., use="na.or.complete", method='spearman') #'pearson' is the default method in cor
corrplot::corrplot(cor_vars %>% cor(., use="na.or.complete", method='spearman'))
#cor_pvals <- (cor_vars %>% corr.test(., use="na.or.complete", method='spearman'))$p
#corPlot(corCi(cor_vars, method='spearman'), pval=cor_pvals)
#sapply(1:length(cor_vars), function(x) cor.test(cor_vars[,x+1], cor_vars$FEGAP))

#b) VIF
vif_reg_base_2 <- car::vif(reg_base_v2) #not working, bc `plm` does not provide reg results with intercept

#.. hence writing function to compute VIF manually
vif_calc <- function(reg_model) {
  X <- reg_model$model[,2:length(reg_model$model)] # obtaining values of independent variables
  corX <- cor(X)   # obtaining correlation matrix
  Tmat <- solve(corX) # obtaining tolerance matrix as inverse of cor-matrix (tolerance = proportion of variance NOT explained by other var's)
  VIF <- diag(Tmat) # obtaining VIF by extracting the diagnonal elements 
  names(VIF) <- colnames(X)
  round(VIF, 2)
}

vif_calc(reg_base_v2)
vif_calc(reg_base_v3)
vif_calc(reg_base_v4a)
vif_calc(reg_base_v4b)
vif_calc(reg_base_v5a)
vif_calc(reg_base_v5b)


#.. check correctness of approach with simple lm model:
reg_base_v1_lm <- lm(FEGAP ~  EPL + UDENS, data=data)
car::vif(reg_base_v1_lm)
vif_calc(reg_base_v1_lm) #check! values are the same


##c) eigenvalues and condition index
#library("olsrr")
#ols_eigen_cindex(reg_base_v1_lm)
#ols_eigen_cindex(reg_base_v1) #again, not working - `plm` not providing adequate elements



### (II.2) unit root tests

pdata <- data %>% 
  select("ccode", "year", "FEGAP", "EPL", "UDENS", "TFP", "EGLOB", "ACCU", "INFL", "LRG_pg") %>% 
  #filter(ccode != 'USA') %>% 
  filter(year >= 1971 & year <= 2019) %>% #due to missing values of GBR in 1970 -> aiming for a balanced panel here!
  mutate(UDENS = if_else(is.na(UDENS) & year==1981 & ccode=='USA', 21.7, UDENS)) %>% #..imputing values for balanced panel
  mutate(UDENS = if_else(is.na(UDENS) & year==1982 & ccode=='USA', 20.6, UDENS)) %>% 
  na.omit()
pdata <- pdata.frame(pdata, index = c("ccode", "year"))

pdata.FEGAP <- pdata[, 'FEGAP'];  pdata.FEGAP.ts <- ts(pdata.FEGAP, start=1, frequency=1)
pdata.EPL <- pdata[, 'EPL'];  pdata.EPL.ts <- ts(pdata.EPL, start=1, frequency=1)
pdata.UDENS <- pdata[, 'UDENS'];  pdata.UDENS.ts <- ts(pdata.UDENS, start=1, frequency=1)
pdata.TFP <- pdata[, 'TFP'];  pdata.TFP.ts <- ts(pdata.TFP, start=1, frequency=1)
pdata.EGLOB <- pdata[, 'EGLOB'];  pdata.EGLOB.ts <- ts(pdata.EGLOB, start=1, frequency=1)
pdata.ACCU <- pdata[, 'ACCU'];  pdata.ACCU.ts <- ts(pdata.ACCU, start=1, frequency=1)
pdata.INFL <- pdata[, 'INFL'];  pdata.INFL.ts <- ts(pdata.INFL, start=1, frequency=1)
pdata.LRG_pg <- pdata[, 'LRG_pg'];  pdata.LRG_pg.ts <- ts(pdata.LRG_pg, start=1, frequency=1)


#i) LLC (Levin Lin Chu) testing -> requiring panel data structure with 'index' attributes
purtest(FEGAP ~ 1, data=pdata, exo='trend', test='levinlin') #p<0.01, stat
purtest(EPL ~ 1, data=pdata, exo='trend', test='levinlin')   #p<0.01, stat [data excluding USA]
purtest(UDENS ~ 1, data=pdata, exo='trend', test='levinlin') #p<0.10, stat
purtest(TFP ~ 1, data=pdata, exo='trend', test='levinlin')   #p<0.01, stat
purtest(EGLOB ~ 1, data=pdata, exo='trend', test='levinlin') #p<0.01, stat
purtest(ACCU ~ 1, data=pdata, exo='trend', test='levinlin')  #p<0.01, stat
purtest(INFL ~ 1, data=pdata, exo='trend', test='levinlin')  #p<0.01, stat
purtest(LRG_pg ~ 1, data=pdata, exo='trend', test='levinlin')#p<0.05, stat


#ii) IPS testing - 
purtest(FEGAP ~ 1, data=pdata, exo='trend', test='ips')     #p<0.01, stat
purtest(EPL ~ 1, data=pdata, exo='trend', test='ips')       #p<0.01, stat [data excluding USA]
purtest(UDENS ~ 1, data=pdata, exo = "trend", test = "ips") #p<0.10, weakly stat
purtest(TFP ~ 1, data=pdata, exo = "trend", test = "ips")   #p<0.01, stat
purtest(EGLOB ~ 1, data=pdata, exo = "trend", test = "ips") #p<0.10, weakly stat
purtest(ACCU ~ 1, data=pdata, exo = "trend", test = "ips")  #p<0.01, stat
purtest(INFL ~ 1, data=pdata, exo = "trend", test = "ips")  #p<0.01, stat
purtest(LRG_pg ~ 1, data=pdata, exo = "trend", test = "ips")#p<0.05, stat

#.. weirdly slightly different values when using the formula-less syntax (which should not be the case...)
#purtest(pdata.FEGAP, exo = "trend", test = "ips") #or: 'madwu', 'levinlin'
#purtest(pdata.EPL, exo = "trend", test = "ips")
#purtest(pdata.UDENS, exo = "trend", test = "ips")
#purtest(pdata.TFP, exo = "trend", test = "ips")
#purtest(pdata.EGLOB, exo = "trend", test = "ips")
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
cipstest(pdata$ACCU, type='trend')  #.. not working
cipstest(pdata$INFL, type='trend')  #.. not working
cipstest(pdata$LRG_pg, type='trend')#.. not working 


#iii) MW testing
purtest(pdata[,-c(1,2)], pmax = 4, exo = "trend", test = "madwu")


#iv) ADF testing - gold standard for time series data
# ADF test: deltaY = gamma*Y_t-1 + a0 + a2*t + epsilon
# trend / drift / none: tau3/2/1: gamma = 0
#                       phi3/1: gamma = 0 & a2 = 0
#                       phi2: gamma = 0 & a2 = 0 & a0 = 0
summary(ur.df(pdata.FEGAP.ts, type='trend', lags=0, selectlags='BIC')) #gamma: p>0.10, stat
summary(ur.df(pdata.EPL.ts, type='trend', lags=0, selectlags='BIC'))   #gamma: p>0.10, stat
summary(ur.df(pdata.UDENS.ts, type='trend', lags=0, selectlags='BIC')) #gamma: p>0.10, stat
summary(ur.df(pdata.TFP.ts, type='trend', lags=0, selectlags='BIC'))   #gamma: p<0.01, strongly non-stat
summary(ur.df(pdata.EGLOB.ts, type='trend', lags=0, selectlags='BIC')) #gamma: p<0.05, non-stat
summary(ur.df(pdata.ACCU.ts, type='trend', lags=0, selectlags='BIC'))  #gamma: p>0.10, stat
summary(ur.df(pdata.INFL.ts, type='trend', lags=0, selectlags='BIC'))  #gamma: p<0.01, strongly non-stat
summary(ur.df(pdata.LRG_pg.ts, type='trend', lags=0, selectlags='BIC'))#gamma: p<0.01, strongly non-stat




#pdata_balanced <- plm_sample(pdata, 0.5, drop = FALSE)
pdata_balanced <- make.pbalanced(pdata)
pdata_balanced['ccode'=='USA' & 'year'==1981, 'UDENS'] <- 
  
  ips_fegap <- punitroot(FEGAP ~ 1, index=c("ccode", "year"), data=pdata_balanced, test="ips")
unitrootTable(trend = "nc")



### (II.3) Co-integration tests
#i) pedroni99m test
#restructuring data
p_t <- as.numeric(levels(unique(pdata$year))) #time
p_i <- as.character(unique(pdata$ccode)) #individuals
p_v <- names(pdata[,-c(1,2)]) #variables, w/o year and ccode
parray <- array(NA, dim = c(length(p_t), length(p_i), length(p_v)))
for (i in 1:length(p_t)) {
  for (j in 1:length(p_i)) {
    for (k in 1:length(p_v)) {
      parray[i, j, k] <- pdata[pdata$year == p_t[i] & pdata$ccode == p_i[j], p_v[k]]
    }
  }
}
 #not working bc pedroni99m is only set up for max 6 variables
pedroni99m(parray, kk = 0, type.stat = 2, ka = 2)

coint.ped_test <- parray[,,c(1,4,7,8)] #FEGAP=1, TFP=4, EGLOB=5, INFL=7, LRG_pg=8
pedroni99m(coint.ped_test, kk = 0, type.stat = 2, ka = 2)


#ii) Johansen test
#. for type='trace'
coint.joh_test.tra <- ca.jo(pdata[,-c(1,2)] , type = "trace", ecdet = "trend", K = 2)
sum.joh.tra <- summary(coint.joh_test.tra)
sum.joh.tra
round(attributes(sum.joh.tra)$V[,1], 3)
round(attributes(sum.joh.tra)$V[,2], 3)
#.. VECM estimation #1, model in differences
coint.joh_vecm1.tra <- cajorls(coint.joh_test.tra, r=2) #running OLS regression of VECM and getting param's of VECM model in differences
coint.joh_vecm1.tra
coeftest(coint.joh_vecm1.tra$rlm)
#.. VECM estimation #2, model in levels
coint.joh_vecm2.tra <- vec2var(coint.joh_test.tra, r=2) #transforming VECM model in differences (with error correction terms) to VAR in levels
coeftest(coint.joh_vecm2.tra$vec2var)
#.. VECM estimation #3, model in differences by Johansen MLE method
VECM(pdata, lag=2, r=2)

#. for type='eigen'
coint.joh_test.eig <- ca.jo(pdata[,-c(1,2)], type = "eigen", ecdet = "trend", K = 2)
sum.joh.eig <- summary(coint.joh_test.eig)
sum.joh.eig
round(attributes(sum.joh.eig)$V[,1], 3)
#.. VECM estimation #1, model in differences
coint.joh_vecm1.eig <- cajorls(coint.joh_test.eig, r=2) #running OLS regression of VECM and getting param's of VECM model in differences
coint.joh_vecm1.eig
coeftest(coint.joh_vecm1.eig$rlm)
#.. VECM estimation #2, model in levels
coint.joh_vecm2.eig <- vec2var(coint.joh_test.eig, r=2) #transforming VECM model in differences (with error correction terms) to VAR in levels
coeftest(coint.joh_vecm2.eig$vec2var)



coint.joh_test2 <- ca.jo(shifted_pdata, type = "trace", ecdet = "trend", K = 2)
cajorls(coint.joh_test2, r=2) 
vec2var(coint.joh_test2, r=2)


#.. problem: beta.se of different length than beta[-1] [source: https://stats.stackexchange.com/questions/96645/finding-significance-levels-for-cointegrating-coefficients-in-cajorls]
#cajo_beta_create <- function(cajo_o, cajorls_o) {
#  alfa <- coef(cajorls_o$rlm)[1, ]
#  residuals <- resid(cajorls_o$rlm)
#  N <- nrow(residuals)
#  sigma <- crossprod(residuals) / N
#  beta <- cajorls_o$beta
#  # standard errors
#  beta.se <- sqrt(diag(kronecker(solve(crossprod(cajo_o@RK[, -1])), solve(t(alfa) %*% solve(sigma) %*% alfa))))
#  beta.se2 <- c(NA, beta.se)
#  beta.t <- c(NA, beta[-1] / beta.se)
#  beta.pvalue <- dt(beta.t, df=cajorls_o$rlm$df.residual)     # p values
#  
#  #tr <- createTexreg(coef.names = as.character(rownames(beta)), coef = as.numeric(beta), se = beta.se2, pvalues=beta.pvalue,
#  #                   gof.names = c('Dummy'), gof=c(1), gof.decimal=c(FALSE))
#  #return(tr)
#}
#cajo_beta_create(coint.joh_test, coint.joh_vecm)
#screenreg(cajo_beta_create(V1.eigen, vecm))


#str(attributes(jotest)) #..obtaining list with all output features
coint_matrix <- cbind(attributes(coint.joh_test)$V[,1], attributes(coint.joh_test)$V[,1])
vdata <- pdata %>% as.data.frame() %>% select(year, FEGAP, EPL, UDENS, TFP, EGLOB, ACCU, INFL, LRG_pg) #%>% modify_if(is.factor, as.character) 
vdata <- VAR(vdata, p=1, type="const")
  


#.. possible extensions:
# panel VECM: examining short-run and long-run dynamics
# wald test: determining Granger causality
# linearity tests




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







#### (III) FURTHER ROBUSTNESS TESTS

# 1) additional regressions, w/o so far omitted regressors

# OG / alternative business cycle measurement
reg_rob_v1 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg + OG, index=c("ccode", "year"), model="within", effect="twoways", data=data)
ses.reg_rob_v1 <- list(coeftest(reg_rob_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob_v1 <- list(coeftest(reg_rob_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob_v1 <- list(coeftest(reg_rob_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

# LTU / accounting for unemployment hysteresis 
reg_rob_v2a <- plm(FEGAP ~  EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg + LTU, index=c("ccode", "year"), model="within", effect="twoways", data=data)
ses.reg_rob_v2a <- list(coeftest(reg_rob_v2a, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob_v2a <- list(coeftest(reg_rob_v2a, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob_v2a <- list(coeftest(reg_rob_v2a, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

reg_rob_v2b <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_pg + UDENS*LRG_pg + LTU, index=c("ccode", "year"), model="within", effect="twoways", data=data)
ses.reg_rob_v2b <- list(coeftest(reg_rob_v2b, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob_v2b <- list(coeftest(reg_rob_v2b, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob_v2b <- list(coeftest(reg_rob_v2b, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

# PUCA & PRCA / capital accumulation
reg_rob_v3 <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + INFL + LRG_pg + UDENS*LRG_pg + PRCA + PUCA, index=c("ccode", "year"), model="within", effect="twoways", data=data)
ses.reg_rob_v3 <- list(coeftest(reg_rob_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob_v3 <- list(coeftest(reg_rob_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob_v3 <- list(coeftest(reg_rob_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

# LR_cp / different political left-right variable
reg_rob_v4a <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_cp, index=c("ccode", "year"), model="within", effect="twoways", data=data)
ses.reg_rob_v4a <- list(coeftest(reg_rob_v4a, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob_v4a <- list(coeftest(reg_rob_v4a, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob_v4a <- list(coeftest(reg_rob_v4a, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors

reg_rob_v4b <- plm(FEGAP ~  lag(FEGAP) + EPL + UDENS + TFP + EGLOB + ACCU + INFL + LRG_cp + UDENS*LRG_cp, index=c("ccode", "year"), model="within", effect="twoways", data=data)
ses.reg_rob_v4b <- list(coeftest(reg_rob_v4b, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_rob_v4b <- list(coeftest(reg_rob_v4b, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
pvals.reg_rob_v4b <- list(coeftest(reg_rob_v4b, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors


# PS / indirect power factor
#reg_base_v5c <- plm(FEGAP ~  EPL + UDENS + TFP + TGLOB + PUCA + PRCA + INFL + PS, index=c("ccode", "year"), model="within", effect="twoways", data=data)
#summary(reg_base_v5c)
#coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))
#ses.reg_base_v5c <- list(coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
#tvals.reg_base_v5c <- list(coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust standard errors
#pvals.reg_base_v5c <- list(coeftest(reg_base_v5c, vcov.=function(x) vcovHC(x, type="sss"))[,4]) #heteroskedasticity-robust standard errors


#stargazer table
stargazer(reg_base_v5b, reg_rob_v1, reg_rob_v2a, reg_rob_v2b, reg_rob_v3, reg_rob_v4a, reg_rob_v4b,
          t = list(unlist(tvals.reg_base_v5b), unlist(tvals.reg_rob_v1), unlist(tvals.reg_rob_v2a), unlist(tvals.reg_rob_v2b), unlist(tvals.reg_rob_v3), unlist(tvals.reg_rob_v4a), unlist(tvals.reg_rob_v4b)), 
          se= list(unlist(ses.reg_base_v5b),   unlist(ses.reg_rob_v1),   unlist(ses.reg_rob_v2a),   unlist(ses.reg_rob_v2b),   unlist(ses.reg_rob_v3),   unlist(ses.reg_rob_v4a),   unlist(ses.reg_rob_v4b)), 
          p = list(unlist(pvals.reg_base_v5b), unlist(pvals.reg_rob_v1), unlist(pvals.reg_rob_v2a), unlist(pvals.reg_rob_v2b), unlist(pvals.reg_rob_v3), unlist(pvals.reg_rob_v4a), unlist(pvals.reg_rob_v4b)),
          type='html', out="output/reg out 3 rob 1 first.htm")








