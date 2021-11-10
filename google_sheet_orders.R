##Using Google Sheets to Find the Order

#Load Libraries
library(tidyverse)
library(here)
library(ggpubr)
library(ggpmisc)
library(cowplot)
library(shiny)
library(googlesheets4)

#Download Data
gs4_deauth()
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
  ) -> zero_order

#1st Order
ggplot(data = int_data,aes(x=time,y=ln_absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(r.digits = 5)+
  labs(
    title = "1st Order"
  ) -> first_order

#2nd Order 
ggplot(data = int_data,aes(x=time,y=1/absorbance))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  stat_cor(r.digits = 5)+
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
zero_order_r_squared <- summary(lm(int_data$absorbance~int_data$time))[["r.squared"]]
first_order_r_squared <- summary(lm(int_data$ln_absorbance~int_data$time))[["r.squared"]]
second_order_r_squared <- summary(lm(int_data$`1/absorbance`~int_data$time))[["r.squared"]]

r_squared_tibble <- tibble(c(zero_order_r_squared,first_order_r_squared,second_order_r_squared),c(0,1,2)) 
colnames(r_squared_tibble) <- c("r_squared","order")
r_squared_tibble %>% 
  filter(r_squared==max(abs(r_squared_tibble$r_squared))) -> max_r_squared

#Finally printing the order
print(max_r_squared$order)

