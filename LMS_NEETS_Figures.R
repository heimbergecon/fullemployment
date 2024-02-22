#############################################################################################################
################################# LABOUR MARKET SLACK NEETS, PRODUCTIVTY AND WAGES  ########################################
#############################################################################################################
pacman::p_load(rstudioapi,stats, ggplot2, tidyverse, dplyr,utils,xts,foreign,base,readxl,seasonal,stargazer,
               xlsx,pastecs,zoo,eurostat,ggrepel,countrycode,scales,lme4,ggpubr,lubridate,directlabels,
               ggrepel,plotrix,plm)
current_path <- getActiveDocumentContext()$path #find the current path of the script
setwd(dirname(current_path )) 
getwd()
rm(list = ls())

country_list_full <-c( "Austria","Germany","Spain","United Kingdom","Sweden","Belgium", "Denmark",
                           "Finland","France","Italy","Ireland","Luxembourg","Netherlands" ,"Poland" ,
                           "Portugal" ,"Slovakia",  "Estonia" , "Slovenia","Lithuania", "Latvia","Romania",
                           "Cyprus","Greece","Malta","United States","Bulgaria","Czechia","Croatia", "Hungary", "United States")

LMS_countries <- c("Austria","Germany","Finland","Sweden","United Kingdom" ,"United States")

################################# 1. NEETS DATA  ########################################
LMS_neets_data <- read_csv("LMS_neets_data.csv.csv")%>%
    select(-"...1")

################################# 2. BECRU PLOTS AND CORRELATION COEFFICIENTS  ########################################
##### 2.1. Austria PLOT BECRU GAP - NEETS ########################################
Austria_neets_data<-LMS_neets_data%>%filter(country == "Austria")%>%
  select(country,year,fegap_lm_v1,neets_prc)%>%na.omit()

cortest <-cor.test(Austria_neets_data$neets_prc, Austria_neets_data$fegap_lm_v1, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

png("Output/Figure BECRU NEET.png", width = 8*300, height = 9*300, res = 300)

mat <- matrix(c(1, 2,  # First, second
                3, 4,
                5, 6), # and third plot
              nrow = 3, ncol = 2,
              byrow = TRUE)

layout(mat = mat)
## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Austria_neets_data$year, Austria_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,11), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Austria", side = 3, adj = 0)
mtext("Cor.coeff = -0.06", side = 3, adj = 1)
axis(2, ylim=c(0,11),col="black",las=1) 
#mtext("NEET, percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)      

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Austria_neets_data$year, Austria_neets_data$fegap_lm_v1, pch=16,  xlab="", ylab="", ylim=c(0,6), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
#mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(0,6), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Austria_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","Full employment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)


##### 2.2. Germany PLOT BECRU GAP - NEETS ########################################
Germany_neets_data<-LMS_neets_data%>%filter(country == "Germany")%>%select(country,year,fegap_lm_v1,neets_prc)%>%na.omit()

cortest <-cor.test(Germany_neets_data$neets_prc, Germany_neets_data$fegap_lm_v1, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Germany_neets_data$year, Germany_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,13), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Germany", side = 3, adj = 0)
mtext("Cor.coeff = 0.88***", side = 3, adj = 1)
axis(2, ylim=c(0,13),col="black",las=1)  
#mtext("NEET, percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Germany_neets_data$year, Germany_neets_data$fegap_lm_v1, pch=16,  xlab="", ylab="", ylim=c(0,10), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
#mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(0,10), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Germany_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","Full employment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 2.3. Finland PLOT BECRU GAP - NEETS ########################################
Finland_neets_data<-LMS_neets_data%>%filter(country == "Finland")%>%
  select(country,year,fegap_lm_v1,neets_prc)%>%na.omit()

cortest <-cor.test(Finland_neets_data$neets_prc, Finland_neets_data$fegap_lm_v1, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Finland_neets_data$year, Finland_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,11), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Finland", side = 3, adj = 0)
mtext("Cor.coeff = 0.84***", side = 3, adj = 1)
axis(2, ylim=c(0,11),col="black",las=1)  
mtext("NEET, percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Finland_neets_data$year, Finland_neets_data$fegap_lm_v1, pch=16,  xlab="", ylab="", ylim=c(0,12), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
#mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(0,12), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Finland_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","Full employment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 2.4. Sweden PLOT BECRU GAP - NEETS ########################################
Sweden_neets_data<-LMS_neets_data%>%filter(country == "Sweden")%>%select(country,year,fegap_lm_v1,neets_prc)%>%na.omit()

cortest <-cor.test(Sweden_neets_data$neets_prc, Sweden_neets_data$fegap_lm_v1, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Sweden_neets_data$year, Sweden_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,12), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Sweden", side = 3, adj = 0)
mtext("Cor.coeff = 0.59**", side = 3, adj = 1)
axis(2, ylim=c(0,12),col="black",las=1)  
#mtext("NEET, as percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Sweden_neets_data$year, Sweden_neets_data$fegap_lm_v1, pch=16,  xlab="", ylab="", ylim=c(0,7), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(0,7), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Sweden_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","Full employment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 2.5. United Kingdom PLOT BECRU GAP - NEETS ########################################
UK_neets_data<-LMS_neets_data%>%filter(country == "United Kingdom")%>%select(country,year,fegap_lm_v1,neets_prc)%>%na.omit()

cortest <-cor.test(UK_neets_data$neets_prc, UK_neets_data$fegap_lm_v1, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(UK_neets_data$year, UK_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,18), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("United Kingdom", side = 3, adj = 0)
mtext("Cor.coeff = 0.31", side = 3, adj = 1)
axis(2, ylim=c(0,18),col="black",las=1)  
#mtext("NEET, as percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(UK_neets_data$year, UK_neets_data$fegap_lm_v1, pch=16,  xlab="", ylab="", ylim=c(0,4), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
#mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(0,4), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(UK_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","Full employment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 2.6. United States PLOT BECRU GAP - NEETS ########################################
US_neets_data<-LMS_neets_data%>%filter(country == "United States")%>%select(country,year,fegap_lm_v1,neets_prc)%>%na.omit()

cortest <-cor.test(US_neets_data$neets_prc, US_neets_data$fegap_lm_v1, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)
## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(US_neets_data$year, US_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,21), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("United States", side = 3, adj = 0)
mtext("Cor.coeff = 0.97***", side = 3, adj = 1)
axis(2, ylim=c(0,21),col="black",las=1)  
#mtext("NEET, as percentage of people in relevant population groupe",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(US_neets_data$year, US_neets_data$fegap_lm_v1, pch=16,  xlab="", ylab="", ylim=c(-2,6), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
#mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(-2,6), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(US_neets_data$year),8))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","Full employment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

dev.off()

rm(US_neets_data,UK_neets_data,Sweden_neets_data,Finland_neets_data,Germany_neets_data,Austria_neets_data)

################################# 3. NAIRU PLOTS AND CORRELATION COEFFICIENTS  ########################################
NAIRU_estimates <- read_excel("NAIRU/NAIRU estimates AMECO December 2023.xlsx")%>%
  gather(Year, Values,-Country)%>%
  rename(country = Country)%>%
  mutate(year = as.numeric(as.character(Year)),
         NAIRU =as.numeric(Values))%>%
  select(-Year,-Values)%>%
  filter(country %in% LMS_countries)%>%
  filter(year > 1969 & year < 2023)

unique(as.character(NAIRU_estimates$country))  

NAIRU_NEET <- full_join(NAIRU_estimates,LMS_neets_data, by = c("country","year"))%>%
  mutate(NAIRU_gap = unemployment_rate_oecd-NAIRU)

##### 3.1. Austria PLOT NAIRU GAP - NEETS ########################################
Austria_neets_data<-NAIRU_NEET%>%filter(country == "Austria")%>%
  select(country,year,NAIRU_gap,neets_prc)%>%na.omit()

cortest <-cor.test(Austria_neets_data$neets_prc, Austria_neets_data$NAIRU_gap, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

png("Output/Figure NAIRU NEET.png", width = 8*300, height = 9*300, res = 300)

mat <- matrix(c(1, 2,  # First, second
                3, 4,
                5, 6), # and third plot
              nrow = 3, ncol = 2,
              byrow = TRUE)

layout(mat = mat)
## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Austria_neets_data$year, Austria_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,11), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Austria", side = 3, adj = 0)
mtext("Cor.coeff = 0.06", side = 3, adj = 1)
axis(2, ylim=c(0,11),col="black",las=1) 
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)      

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Austria_neets_data$year, Austria_neets_data$NAIRU_gap, pch=16,  xlab="", ylab="", ylim=c(-1,4), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
axis(4, ylim=c(-1,4), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Austria_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","NAIRU unemployment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)


##### 3.2. Germany PLOT NAIRU GAP - NEETS ########################################
Germany_neets_data<-NAIRU_NEET%>%filter(country == "Germany")%>%
  select(country,year,NAIRU_gap,neets_prc)%>%na.omit()

cortest <-cor.test(Germany_neets_data$neets_prc, Germany_neets_data$NAIRU_gap, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Germany_neets_data$year, Germany_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,13), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Germany", side = 3, adj = 0)
mtext("Cor.coeff = -0.02", side = 3, adj = 1)
axis(2, ylim=c(0,13),col="black",las=1)  
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Germany_neets_data$year, Germany_neets_data$NAIRU_gap, pch=16,  xlab="", ylab="", ylim=c(0,4), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
axis(4, ylim=c(0,4), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Germany_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","NAIRU unemployment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 3.3. Finland PLOT NAIRU GAP - NEETS ########################################
Finland_neets_data<-NAIRU_NEET%>%
  filter(country == "Finland")%>%select(country,year,NAIRU_gap,neets_prc)%>%na.omit()

cortest <-cor.test(Finland_neets_data$neets_prc, Finland_neets_data$NAIRU_gap, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Finland_neets_data$year, Finland_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,11), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Finland", side = 3, adj = 0)
mtext("Cor.coeff = 0.64**", side = 3, adj = 1)
axis(2, ylim=c(0,11),col="black",las=1)  
mtext("NEET, percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Finland_neets_data$year, Finland_neets_data$NAIRU_gap, pch=16,  xlab="", ylab="", ylim=c(0,6), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
axis(4, ylim=c(0,6), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Finland_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","NAIRU unemployment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 3.4. Sweden PLOT NAIRU GAP - NEETS ########################################
Sweden_neets_data<-NAIRU_NEET%>%filter(country == "Sweden")%>%
  select(country,year,NAIRU_gap,neets_prc)%>%na.omit()

cortest <-cor.test(Sweden_neets_data$neets_prc, Sweden_neets_data$NAIRU_gap, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(Sweden_neets_data$year, Sweden_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,12), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("Sweden", side = 3, adj = 0)
mtext("Cor.coeff = 0.24", side = 3, adj = 1)
axis(2, ylim=c(0,12),col="black",las=1)  
#mtext("NEET, as percentage of people in relevant population group",side=2,line=2.5)
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(Sweden_neets_data$year, Sweden_neets_data$NAIRU_gap, pch=16,  xlab="", ylab="", ylim=c(-1,3), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
mtext("NAIRU full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(-1,3), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(Sweden_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","NAIRU unemployment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 3.5. United Kingdom PLOT NAIRU GAP - NEETS ########################################
UK_neets_data<-NAIRU_NEET%>%filter(country == "United Kingdom")%>%
  select(country,year,NAIRU_gap,neets_prc)%>%na.omit()

cortest <-cor.test(UK_neets_data$neets_prc, UK_neets_data$NAIRU_gap, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(UK_neets_data$year, UK_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,18), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("United Kingdom", side = 3, adj = 0)
mtext("Cor.coeff = -0.23", side = 3, adj = 1)
axis(2, ylim=c(0,18),col="black",las=1)  
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(UK_neets_data$year, UK_neets_data$NAIRU_gap, pch=16,  xlab="", ylab="", ylim=c(-4,2), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
axis(4, ylim=c(-4,2), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(UK_neets_data$year),5))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","NAIRU unemployment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

##### 3.6. United States PLOT NAIRU GAP - NEETS ########################################
US_neets_data<-NAIRU_NEET%>%filter(country == "United States")%>%
  select(country,year,NAIRU_gap,neets_prc)%>%na.omit()

cortest <-cor.test(US_neets_data$neets_prc, US_neets_data$NAIRU_gap, 
                   method = "pearson")
cortest
round(cortest$estimate, digits = 2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(US_neets_data$year, US_neets_data$neets_prc, pch=16, axes=FALSE, ylim=c(0,21), xlab="", ylab="", 
     type="b",col="#69b3a2")
mtext("United States", side = 3, adj = 0)
mtext("Cor.coeff = 0.9***", side = 3, adj = 1)
axis(2, ylim=c(0,21),col="black",las=1)  
box()
grid(nx = NA, ny = NULL,
     lty = 1,      
     col = "lightgray", 
     lwd = 0.5)     

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(US_neets_data$year, US_neets_data$NAIRU_gap, pch=16,  xlab="", ylab="", ylim=c(-2,4), 
     axes=FALSE, type="b", col="#E69F00")
## a little farther out (line=4) to make room for labels
#mtext("Full employment gap, percentage points",side=4,col="black",line=2) 
axis(4, ylim=c(-2,4), col="black",col.axis="black",las=1)

## Draw the time axis
axis(1,pretty(range(US_neets_data$year),8))

## Add Legend
legend("bottom",inset = c(0, -0.4),
       legend=c("NEET, % of people (left y-axis)","NAIRU unemployment gap (right y-axis)"),
       bty = "n",
       text.col=c("black","black"),
       pch=c(16,16),
       col=c("#69b3a2","#E69F00"),
       lty = c(1, 2),
       lwd = 2,
       xpd = TRUE,
       horiz = F)

dev.off()

rm(US_neets_data,UK_neets_data,Sweden_neets_data,Finland_neets_data,Germany_neets_data,Austria_neets_data)

