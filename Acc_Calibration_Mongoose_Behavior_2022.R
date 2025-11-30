#Code for analyses in "Identifying social behaviours related to disease transmission in banded mongoose from accelerometer data" By Paige Van de Vuurst, Kathleen A. Alexander
#First published: 11 July 2023 in Ecological Solutions and Evidence
#Available at: https://doi.org/10.1002/2688-8319.12228

#Begin by setting a working directory (i.e., file where all your data is stored)
setwd("C:/Users/paige/Desktop/Data/Accele_Data_AlexLab")

#install.packages(c("pracma", "rgl"))
#install.packages("rgl")
#install the library for these packages in your session
library (pracma)
library (rgl)
library(ggplot2)

library(xfun)
library(xfun)
library(dplyr)
library(tidyr)
library(magrittr)
#install.packages("xfun")
#install.packages("rgl", repos='http://cran.cnr.berkeley.edu/')

#You will need X11 to operate on a mac - download at https://www.xquartz.org/
#rgl does not work with monterey OS system 
## This code provides example of Calibration of the Technosmart Europe AGM magnetometer by fitting an ellipsoid to a set of calibration data points 
###and transforming it into a sphere centered in zero

##Theoretical aspects of this script are based in ST Microelectronics's algorithm by Andrea Vitali, which you may find at:
###https://www.st.com/content/ccc/resource/technical/document/design_tip/group0/a2/98/f5/d4/9c/48/4a/d1/DM00286302/files/DM00286302.pdf/jcr:content/translations/en.DM00286302.pdf

#Calibration movement manual for TechnoSmart Model Accelerometer can be found at: http://www.technosmart.eu/gallery.php

#To midigate formatting errors, Open the calbration and movement CSV (i.e., data downloaded from Accelerometer) in a reader first
##Delte any non-numeric rows 

#Now you are ready to read your CSV file and source the calibration functions text file

source("C:/Users/paige/Desktop/Data/Accele_Data_AlexLab/AGM_functions.txt") 
#Set source to your working directory (where data and AGM_functions text file are)
calibration.data = read.csv("calibration.csv", header=T) 

#Plotting the raw data: the three axes should produce an ellipsoid-like shape
##Turning on/off the logger will produce outlying values which should be excluded from analysis. 

#Plot the calibration data 
doubleplot(calibration.data$Mx,calibration.data$My,calibration.data$Mz,"Raw calibration data")
mel_calibration=calibration.data
df = mel_calibration %>% 
  mutate(n=row_number()) %>% 
  subset((between(n,1300,4300) | between(n,7700,9500)) & between(Mx,-50,50) & between(My,-50,50) & between(Mz,-50,50))
#acc axes values. setting work space of acc field. Setting above or below 50 as "impossible" 
#calibration.data#[1000:20000,] ## adjust the row index to subset
doubleplot(df$Mx,df$My,df$Mz,"Raw calibration data")

values = ellipsoid_fit(df$Mx,df$My,df$Mz)

## Choose a scaling value according to what you want the sphere radius to measure
## Should be calculated based on animal size 
scaling = 1

## Apply correction to the calibration data and plot it
calibrated = correct(df$Mx,df$My,df$Mz, values$offset,values$gain,values$eigenvec,scaling)
doubleplot(calibrated$MagX_c,calibrated$MagY_c,calibrated$MagZ_c,"Corrected calibration data")


calibrated %>% 
  mutate(n=row_number()) %>% 
  set_colnames(c("X","Y","Z","n")) %>% 
  pivot_longer(cols = c("X","Y","Z"), names_to = "axis",values_to = "mag") %>% 
  ggplot(aes(x = n, y = mag)) + geom_line() + facet_grid(axis~.)

df %>% 
  select(Mx,My,Mz, n) %>% 
  #mutate(n=row_number()) %>% 
  set_colnames(c("X","Y","Z","n")) %>% 
  subset(between(n,1300,4300) | between(n,7700,9500)) %>% 
  pivot_longer(cols = c("X","Y","Z"), names_to = "axis",values_to = "mag") %>% 
  ggplot(aes(x = n, y = mag)) + geom_line() + facet_grid(axis~.)

###################################################################################################

## Finally, read your real magnetometer data and apply the same correction to it. 
## If errors are encountered, remember to subset by eliminating data produced by magnet swipes upon turning the logger on/off
test_dat<-read.csv("test_nov5_copy.csv", header=T)
test_dat
#If testing data is error free, continue to calibration 

my.data=test_dat
my.data.calibrated = correct(my.data$accX,my.data$accY,my.data$accZ, 
                             values$offset,values$gain,values$eigenvec,scaling)

my.data.calibrated
## You might want to plot your data before and after correction. Use doubleplot function to do so. 
## Depending on the size of your dataframe this might take some time
doubleplot(my.data$accX,my.data$accY,my.data$accZ,"Raw data")
doubleplot(my.data.calibrated$MagX_c,my.data.calibrated$MagY_c,my.data.calibrated$MagZ_c,"Corrected data")


my.data %>% 
  select(accX,accY,accZ) %>% 
  mutate(n=row_number()) %>% 
  set_colnames(c("X","Y","Z","n")) %>% 
  #subset(between(n,1300,4300) | between(n,7700,9500)) %>% 
  subset(between(n,0,5000)) %>% 
  pivot_longer(cols = c("X","Y","Z"), names_to = "axis",values_to = "mag") %>% 
  ggplot(aes(x = n, y = mag)) + geom_line() + facet_grid(axis~.)

my.data.calibrated %>% 
  select(MagX_c,MagY_c,MagZ_c) %>% 
  mutate(n=row_number()) %>% 
  set_colnames(c("X","Y","Z","n")) %>% 
  #subset(between(n,1300,4300) | between(n,7700,9500)) %>% 
  subset(between(n,0,5000)) %>% 
  pivot_longer(cols = c("X","Y","Z"), names_to = "axis",values_to = "mag") %>% 
  ggplot(aes(x = n, y = mag)) + geom_line() + facet_grid(axis~.)

################################

#Add your time stamps back in after calibration 
my.data.calibrated$t<-test_dat$Time
#check data 
my.data.calibrated

time<-my.data.calibrated$t
time
#Read in time as character 
t2<-as.character(time)

t2
my.data.calibrated$t2<-t2
#plot three axes of movement (x,y,and z)
whole_run<-ggplot(my.data.calibrated, aes(x=t)) + 
  geom_line(aes(y = MagX_c), color = "blue") + 
  geom_line(aes(y = MagY_c), color="green", linetype="twodash") +
  geom_line(aes(y= MagZ_c), color="red")+scale_x_discrete(guide = guide_axis(check.overlap=T))

whole_run
#Save Calibrated Data 
write.csv(my.data.calibrated, "calibrated_data_nov5.csv")

#Read in subset of specific activities based on timestamps 
alert<-read.csv("Alert_timestamp1_nov5.csv", header=T)
alert
#convert to Data frame 
alert<-as.data.frame(alert)
plot(alert)
# Plot specific activity via three axes of movment 
alert_plot1<-ggplot(alert, aes(x=t)) + geom_line(aes(y = MagX_c), color = "blue")+ 
  geom_line(aes(y = MagY_c), color="green") + 
  geom_line(aes(y= MagZ_c), color="red")+ theme_classic()+   
  scale_x_discrete(guide = guide_axis(check.overlap=T))

alert_plot1

