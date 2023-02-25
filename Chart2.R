### Second Trends Over Time Chart
#To see the trend of 'EBOOK's checkouts, how has the number of checkouts changed over time? 

library("dplyr")
library("stringr")
library("ggplot2")

spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)

df <- spl_df %>% 
  select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(total_Checkouts = sum(Checkouts)) 


Second_df <- spl_df %>% select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear) %>% 
  filter(MaterialType == "EBOOK") %>% 
  filter("2014" <= CheckoutYear & CheckoutYear <= "2023") %>% 
  summarize(total_Checkouts = sum(Checkouts)) 

bp_sec <- ggplot(data = Second_df) +
  geom_col(mapping = aes( x = CheckoutYear,
                          y = total_Checkouts,
                          fill = CheckoutYear)) +
  scale_y_continuous(labels = label_number_si()) +
  scale_colour_brewer(palette = "Greens") +
  labs(
    title = "Trend of Ebooks' Checkouts between 2014 and 2023",
    x = "Checkout Year",
    y = "The number of checkouts",
  ) 

bp_sec
