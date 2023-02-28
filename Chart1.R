### First Trends Over Time Chart
#To see the trend of 'BOOK's checkouts, how has the number of checkouts changed over time? 

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

#load file
spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)

#code

df <- spl_df %>% 
  select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(total_Checkouts = sum(Checkouts)) 


First_df <- spl_df %>% select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear) %>% 
  filter("2014" <= CheckoutYear & CheckoutYear <= "2023") %>% 
  summarize(total_Checkouts = sum(Checkouts)) 


#graph
chart_1 <- ggplot(data = First_df) +
  geom_line(aes( x = CheckoutYear,
                 y = total_Checkouts,
                 color = total_Checkouts)) +
  scale_color_gradient(low = "#f2c2ff", high = "#005fad")   +
  labs(
    title = "Trend of Books' Checkouts between 2014 and 2023",
    x = "Checkout Year",
    y = "The number of checkouts"
  ) +
  scale_y_continuous(labels = label_number_si()) 

chart_1







