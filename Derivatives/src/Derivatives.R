#Chem Derivatives

library(tidyverse)
library(here)
library(XML)
library(xml2)
library(ggpubr)

#File Locations **Needed for order**
point_one_molar_cmbl <- here("Derivatives/input/0.10_NaOH.cmbl")
point_zero_five_molar_cmbl <- here("Derivatives/input/0.05_NaOH.cmbl")

#Importing Data **Needed for order**
point_one_molar_read <- read_xml(point_one_molar_cmbl)
point_zero_five_molar_read <- read_xml(point_zero_five_molar_cmbl)
#Sort by the Column Cells (the data we care about in the XML file)
point_one_molar_columns <- point_one_molar_read %>% xml_find_all('//ColumnCells') 
point_zero_five_molar_columns <- point_zero_five_molar_read %>% xml_find_all('//ColumnCells')
#Get the nodes of the time and absorbance separately
point_one_molar_time <- xmlParse(point_one_molar_columns[[1]])
point_one_molar_absorbance <- xmlParse(point_one_molar_columns[[2]])
point_zero_five_molar_time <- xmlParse(point_zero_five_molar_columns[[1]])
point_zero_five_molar_absorbance <- xmlParse(point_zero_five_molar_columns[[2]])
#Convert the nodes into string with "\n" delim
point_one_molar_time_list <- xmlToList(point_one_molar_time)
point_one_molar_absorbance_list <- xmlToList(point_one_molar_absorbance)
point_zero_five_molar_time_list <- xmlToList(point_zero_five_molar_time)
point_zero_five_molar_absorbance_list <- xmlToList(point_zero_five_molar_absorbance)
#Get rid of the "\n" delim and create vector
point_one_molar_time_read <- read_csv(point_one_molar_time_list,col_names = FALSE)
point_one_molar_absorbance_read <- read_csv(point_one_molar_absorbance_list,col_names = FALSE)
point_zero_five_molar_time_read <- read_csv(point_zero_five_molar_time_list,col_names = FALSE)
point_zero_five_molar_absorbance_read <- read_csv(point_zero_five_molar_absorbance_list,col_names = FALSE)
#Add Column Names
colnames(point_one_molar_time_read) <- "time"
colnames(point_one_molar_absorbance_read) <- "absorbance"
colnames(point_zero_five_molar_time_read) <- "time"
colnames(point_zero_five_molar_absorbance_read) <- "absorbance"
#Make the vectors into a data.frame (tibble)
point_one_molar <- tibble(point_one_molar_time_read,point_one_molar_absorbance_read)
point_zero_five_molar <- tibble(point_zero_five_molar_time_read,point_zero_five_molar_absorbance_read)
#Clean up (get rid of objects no longer needed)
rm(list=ls()[! ls() %in% c("point_one_molar","point_zero_five_molar")])

#Boring way to import data (Fixed with XML Imports)
#point_one_molar <- read_csv(here("Derivatives/input/0.10_M_NaOH.csv"))
#point_zero_five_molar <- read_csv(here("Derivatives/input/0.05_M_NaOH.csv"))

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
rates <- c(initial_rate_point_zero_five_molar,initial_rate_point_one_molar)
molarity <- c("0.05","0.10")
rate_table <- data.frame(as.numeric(molarity),as.numeric(rates))
colnames(rate_table) <- c("molarity","rate")
rate_table <- arrange(rate_table,molarity)

#Finally getting the order **Needed for order**
order <- log(round(rate_table[2,2]/rate_table[1,2]))/log(rate_table[2,1]/rate_table[1,1])
print(order)


