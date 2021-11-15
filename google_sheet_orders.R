##Using Google Sheets to Find the Order

#Load Libraries
library(tidyverse)
library(here)
library(ggpubr)
library(ggpmisc)
library(cowplot)
library(googlesheets4)

#Download Data
raw_data <- read_sheet("1a8l07lQ10MwJUHiL4wxex1g1ewfrx2GVZhUAc7FDFwg")
colnames(raw_data) <-  c("time","absorbance")

#Using the integrated rate law
raw_data %>% 
  mutate(log(raw_data$absorbance)) %>% 
  mutate(1/raw_data$absorbance) -> int_data
colnames(int_data) <- c("time","absorbance","ln_absorbance","1/absorbance")


#Graphing the integrating
#Oth Order
ggplot(data = int_data,aes(x=time,y=absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(r.digits = 5)+
  labs(
    title = "0th Order"
  ) -> zero_order_graph

#1st Order
ggplot(data = int_data,aes(x=time,y=ln_absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(r.digits = 5)+
  labs(
    title = "1st Order"
  ) -> first_order_graph


#2nd Order 
ggplot(data = int_data,aes(x=time,y=1/absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(r.digits = 5)+
  labs(
    title = "2nd Order"
  ) -> second_order_graph

#All Together graphing
plot_grid(
  zero_order_graph,
  first_order_graph,
  second_order_graph)


#Find the order from the highest R^2 value
zero_order_r_squared <- summary(lm(int_data$absorbance~int_data$time))[["r.squared"]]
first_order_r_squared <- summary(lm(int_data$ln_absorbance~int_data$time))[["r.squared"]]
second_order_r_squared <- summary(lm(int_data$`1/absorbance`~int_data$time))[["r.squared"]]

r_squared_tibble <- tibble(c(zero_order_r_squared,first_order_r_squared,second_order_r_squared),c(0,1,2)) 
colnames(r_squared_tibble) <- c("r_squared","order")
r_squared_tibble %>% 
  filter(r_squared==max(abs(r_squared_tibble$r_squared))) -> max_r_squared

#Finally printing the order
print(max_r_squared$order)

#Exporting answers to google sheets

#Renaming Columns
colnames(int_data) <- c("time","zero_order","first_order","second_order")

#Added values to vectors
zero_order <- append(int_data$zero_order,c(NA,zero_order_r_squared))
first_order <- append(int_data$first_order,c(NA,first_order_r_squared))
second_order <- append(int_data$second_order,c(NA,second_order_r_squared))
sheet_time <- append(int_data$time,c(NA,max_r_squared$order))

#Making new data frame for export
sheets_int_data <- tibble(sheet_time,zero_order,first_order,second_order)
colnames(sheets_int_data) <- c("Time","0th Order","1st Order","2nd Order")

#Exporting to sheet
sheet_write(sheets_int_data, ss = "1a8l07lQ10MwJUHiL4wxex1g1ewfrx2GVZhUAc7FDFwg", sheet = 2)

