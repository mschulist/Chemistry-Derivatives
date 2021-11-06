#Chem Derivatives

library(tidyverse)
library(here)
library(XML)
library(ggpubr)

#Importing Data (Doesn't work because Logger Pro has stupid formatting - uses the same name for different columns)
point_one_molar_xml <- xmlToDataFrame(here("Derivatives/input/0.10_NaOH.cmbl"))
point_one_molar_xml_parse <- xmlParse(here("Derivatives/input/0.10_NaOH.cmbl"))
point_one_molar_list <- xmlToList(point_one_molar_xml_parse)
point_one_molar_time <- read_table(point_one_molar[["DataSet"]][["DataColumn"]][["ColumnCells"]],col_names = FALSE)
point_one_molar_absorbance <- read_table(point_one_molar[["DataSet"]][["DataColumn"]][["ColumnCells"]],col_names = FALSE)

#Boring way to import data **Needed for order**
point_one_molar <- read_csv(here("Derivatives/input/0.10_M_NaOH.csv"))
point_zero_five_molar <- read_csv(here("Derivatives/input/0.05_M_NaOH.csv"))

#Graphing Data
ggplot()+
  geom_point(data = point_one_molar,aes(x=time,y=absorbance))+
  geom_point(data = point_zero_five_molar,aes(x=time,y=absorbance))+
  geom_smooth(data = point_one_molar,aes(x=time,y=absorbance),color="purple")+
  geom_smooth(data = point_zero_five_molar,aes(x=time,y=absorbance),color="green")

#Getting first 4 points (est. using first 4 points) **Needed for order**
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

#Getting the numbers **Needed for order**
initial_list_point_one_molar <- lm(initial_point_one_molar$absorbance~initial_point_one_molar$time) 
initial_rate_point_one_molar <- abs(initial_list_point_one_molar[["coefficients"]][["initial_point_one_molar$time"]])

initial_list_point_zero_five_molar <- lm(initial_point_zero_five_molar$absorbance~initial_point_zero_five_molar$time)
initial_rate_point_zero_five_molar <- abs(initial_list_point_zero_five_molar[["coefficients"]][["initial_point_zero_five_molar$time"]])

#Putting the rates into a table **Needed for order**

rates <- c(initial_rate_point_one_molar,initial_rate_point_zero_five_molar)
molarity <- c("0.10","0.05")
rate_table <- data.frame(as.numeric(molarity),as.numeric(rates))
colnames(rate_table) <- c("molarity","rate")

#Finally getting the order **Needed for order**

order <- round((rate_table[1,1]/rate_table[2,1])/(rate_table[1,2]/rate_table[2,2]))
print(order)

