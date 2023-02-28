### Second Trends Over Time Chart
#To see the trend of 'EBOOK's checkouts, how has the number of checkouts changed over time? 

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


Second_df <- spl_df %>% select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "EBOOK") %>% 
  filter("2014" <= CheckoutYear & CheckoutYear <= "2023") %>% 
  summarize(total_Checkouts = sum(Checkouts)) 

#graph
chart_2 <- ggplot(data = Second_df) +
  geom_line(mapping = aes( x = CheckoutYear,
                          y = total_Checkouts,
                          color = CheckoutYear)) +
  scale_color_gradient(low = "#E25033", high = "#771C19")   +
  labs(
    title = "Trend of Ebooks' Checkouts between 2014 and 2023",
    x = "Checkout Year",
    y = "The number of checkouts"
  ) +
  scale_y_continuous(labels = label_number_si()) 

chart_2
