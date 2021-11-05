#Chem Derivatives

library(tidyverse)
library(here)
library(XML)

point_one_molar_xml <- xmlToDataFrame(here("Derivatives/input/0.10_NaOH.cmbl"))
point_one_molar_xml <- xmlParse(here("Derivatives/input/0.10_NaOH.cmbl"))

point_one_molar <- read_csv(here("Derivatives/input/0.10_M_NaOH.csv"))
point_zero_five_molar <- read_csv(here("Derivatives/input/0.05_M_NaOH.csv"))

ggplot()+
  geom_point(data = point_one_molar,aes(x=time,y=absorbance))+
  geom_point(data = point_zero_five_molar,aes(x=time,y=absorbance))+
  geom_smooth(data = point_one_molar,aes(x=time,y=absorbance),color="purple")+
  geom_smooth(data = point_zero_five_molar,aes(x=time,y=absorbance),color="green")

