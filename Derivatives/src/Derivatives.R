#Chem Derivatives

library(tidyverse)
library(here)
library(XML)
library(xml2)
library(ggpubr)
library(ggpmisc)
library(cowplot)
library(googlesheets4)

#Get list of files
input_list <- list.files("Derivatives/input")
#Get the file for 0.05 and 0.10 Molar
cmbl_files <- str_subset(input_list, pattern = "cmbl")
point_one_molar_file <- str_subset(cmbl_files, pattern = "10")
point_zero_five_molar_file <- str_subset(cmbl_files, pattern = "5")

#File Locations **Needed for order**
point_one_molar_cmbl <- here("Derivatives/input/", point_one_molar_file)
point_zero_five_molar_cmbl <- here("Derivatives/input/", point_zero_five_molar_file)

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

#Getting the best fit curves
point_one_molar_fit <- summary(lm(point_one_molar$absorbance~poly(point_one_molar$time,5,raw=TRUE), data=point_one_molar))[["coefficients"]]
point_zero_five_molar_fit <- lm(point_zero_five_molar$absorbance~poly(point_zero_five_molar$time,5,raw=TRUE), data=point_zero_five_molar)


#Graphing Data
colors <- c("0.10 M NaOH" = "purple","0.05 M NaOH" = "green")
ggplot()+
  geom_point(data = point_one_molar,aes(x=time,y=absorbance, color = "0.10 M NaOH"))+
  geom_point(data = point_zero_five_molar,aes(x=time,y=absorbance, color = "0.05 M NaOH"))+
  geom_smooth(data = point_zero_five_molar, aes(x=time, y=absorbance), method = "gam", se = F)
  labs(
    title = "Absorbance of Crystal Violet vs Time of 0.10 M and 0.05 M NaOH",
    color = "Concentration of NaOH"
  )+ xlab("Time (s)")+ylab("Absorbance (Au)")

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
  stat_regline_equation(data=initial_point_one_molar,aes(x=time, y=absorbance),label.x=0,label.y = .4)+
  stat_regline_equation(data=initial_point_zero_five_molar,aes(x=time, y=absorbance),label.x=40,label.y = .58)+
  ylim(.1,.65)+ ggtitle("Crystal Violet Absorbance Over Time: Initial Rates")+
  labs(
    x="Time (s)",
    y="Absorbance (Au)",
    color = "Concentration"
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

rm(list=ls()[! ls() %in% c("point_one_molar","point_zero_five_molar")])

#Using the integrated rate law
point_one_molar %>% 
  mutate(log(point_one_molar$absorbance)) %>% 
  mutate(1/point_one_molar$absorbance) -> int_point_one_molar
colnames(int_point_one_molar) <- c("time","absorbance","ln_absorbance","1/absorbance")

point_zero_five_molar %>% 
  mutate(log(point_zero_five_molar$absorbance)) %>% 
  mutate(1/point_zero_five_molar$absorbance) -> int_point_zero_five_molar

#Graphing the integrating
#Oth Order
ggplot(data = int_point_one_molar,aes(x=time,y=absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(aes(label = ..r.label..),r.digits = 5, label.x = 25)+
  xlab("Time (s)") + ylab("Absorbance")+
  labs(
    title = "0th Order"
  ) -> zero_order

#1st Order
ggplot(data = int_point_one_molar,aes(x=time,y=ln_absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(aes(label = ..r.label..),r.digits = 5, label.x = 30)+
  xlab("Time (s)") + ylab("ln(absorbance)")+
  labs(
    title = "1st Order"
  ) -> first_order

#2nd Order 
ggplot(data = int_point_one_molar,aes(x=time,y=1/absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(aes(label = ..r.label..),r.digits = 5, label.x = 100)+
  xlab("Time (s)") + ylab("1/absorbance")+
  labs(
    title = "2nd Order"
  ) -> second_order

#All Together graphing
plot_grid(
  zero_order,
  first_order,
  second_order
)

#Find the order from the highest R^2 value
zero_order_r_squared <- summary(lm(int_point_one_molar$absorbance~int_point_one_molar$time))[["r.squared"]]
first_order_r_squared <- summary(lm(int_point_one_molar$ln_absorbance~int_point_one_molar$time))[["r.squared"]]
second_order_r_squared <- summary(lm(int_point_one_molar$`1/absorbance`~int_point_one_molar$time))[["r.squared"]]

r_squared_tibble <- tibble(c(zero_order_r_squared,first_order_r_squared,second_order_r_squared),c(0,1,2)) 
colnames(r_squared_tibble) <- c("r_squared","order")
r_squared_tibble %>% 
  filter(r_squared==max(r_squared_tibble$r_squared)) -> max_r_squared

#Finally printing the order
print(max_r_squared$order)

#Cleaning up 
rm(list=ls()[! ls() %in% c("point_one_molar","point_zero_five_molar")])

#Getting the k value
#rate = k[CV][OH]
#rate/[CV][OH] = k
#Finding the rate requires finding the derivative of the points
#Concentration of OH is known at 0.05 M or 0.10 M
#Concentration of [CV] requires Beer's Law
#Beer's Law: Absorbance = length(1cm) * constant * concentration
#absorbance/constant = concentration
#constant for 566.7 wavelength is 58291
#absorbance/58291 = concentration
point_one_molar <- point_one_molar %>% 
  mutate(concentration = absorbance/58291)

#Making another table with points from a best fit curve
point_one_molar_points <- loess.smooth(point_one_molar$time, point_one_molar$concentration, evaluation = length(point_one_molar$time)) %>% 
  as.data.frame() %>% 
  rename(time = x, concentration = y)

#Now getting the rates for each given time
rates <- unlist(map(.x = 1:length(point_one_molar_points$time), ~nth(point_one_molar_points$concentration,.x)-nth(point_one_molar_points$concentration,.x-1)/((nth(point_one_molar_points$time,.x)-(nth(point_one_molar_points$time,.x-1))))))

#Adding rates to the point_one_molar df
point_one_molar_points <- point_one_molar_points %>% 
  mutate(rates)

#Getting the k value
k_values <- as_vector(map2(.x = point_one_molar_points$concentration, .y = point_one_molar_points$rates, ~.y/(.x * 0.1)))

k_value <- mean(k_values, na.rm = T)
print(k_value)

