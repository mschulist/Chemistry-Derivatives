#Chem Derivatives

library(tidyverse)
library(here)
library(XML)
library(ggpubr)

#Importing Data
point_one_molar_xml <- xmlToDataFrame(here("Derivatives/input/0.10_NaOH.cmbl"))
point_one_molar_xml_parse <- xmlParse(here("Derivatives/input/0.10_NaOH.cmbl"))
point_one_molar_list <- xmlToList(point_one_molar_xml_parse)
point_one_molar_time <- read_table(point_one_molar[["DataSet"]][["DataColumn"]][["ColumnCells"]],col_names = FALSE)
point_one_molar_absorbance <- read_table(point_one_molar[["DataSet"]][["DataColumn"]][["ColumnCells"]],col_names = FALSE)

#Boring way to import data
point_one_molar <- read_csv(here("Derivatives/input/0.10_M_NaOH.csv"))
point_zero_five_molar <- read_csv(here("Derivatives/input/0.05_M_NaOH.csv"))

#Graphing Data
ggplot()+
  geom_point(data = point_one_molar,aes(x=time,y=absorbance))+
  geom_point(data = point_zero_five_molar,aes(x=time,y=absorbance))+
  geom_smooth(data = point_one_molar,aes(x=time,y=absorbance),color="purple")+
  geom_smooth(data = point_zero_five_molar,aes(x=time,y=absorbance),color="green")

#Finding the Initial Rate (est. using first 4 points)
initial_point_one_molar <- point_one_molar[1:4,]
initial_point_zero_five_molar <- point_zero_five_molar[1:4,]

#Plotting the initial rates
colors <- c("0.10 M NaOH" = "purple","0.05 M NaOH" = "green")
ggplot()+
  geom_point(data = point_one_molar,aes(x=time,y=absorbance,color="0.10 M NaOH"))+
  geom_point(data = point_zero_five_molar,aes(x=time,y=absorbance,color="0.05 M NaOH"))+
  geom_smooth(data = initial_point_one_molar,aes(x=time,y=absorbance,color="0.10 M NaOH"),method="lm",se=FALSE,fullrange=T)+
  geom_smooth(data = initial_point_zero_five_molar,aes(x=time,y=absorbance,color="0.05 M NaOH"),method="lm",se=FALSE,fullrange=T)+
  stat_regline_equation(data=initial_point_one_molar,aes(x=time, y=absorbance),label.x=0,label.y = .35)+
  stat_regline_equation(data=initial_point_zero_five_molar,aes(x=time, y=absorbance),label.x=40,label.y = .58)+
  ylim(.1,.65)+ ggtitle("Crystal Violet Absorbance Over Time: Initial Rates")+
  labs(
    x="Time",
    y="Absorbance",
    color = "Legend"
  )

#Getting the numbers
lm(initial_point_one_molar$absorbance~initial_point_one_molar$time) %>% 
  [["coefficients"]][["initial_point_one_molar$time"]] -> initial_rate_point_one_molar
