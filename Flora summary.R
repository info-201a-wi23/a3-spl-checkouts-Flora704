#summary

library("dplyr")
library("stringr")
library("ggplot2")

spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)

# What is the average number of checkouts for this dataset related to George Orwell?
avg_num_checkout <- spl_df %>% summarize(mean_checkout = mean(Checkouts)) %>% pull(mean_checkout)

avg_num_checkout

# What is the year with the most/least checkouts for a book that you're interested in?
min_checkout <- spl_df %>% filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)

min_checkout 

max_checkout <- spl_df %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)

max_checkout 

# What is the year with the most/least checkouts for ebooks?
max_ebook <- spl_df %>% group_by(MaterialType == "EBOOK") %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)

max_ebook 

min_ebook <- spl_df %>% group_by(MaterialType == "EBOOK") %>% filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)

min_ebook

# How has the number of print book checkouts changed over time? Compare the total number of checkouts now and five years ago. 
total_checkout_2023 <- spl_df %>% filter(CheckoutYear == 2023) %>% summarize(total = sum(Checkouts, na.rm = TRUE)) %>%  pull(total)

total_checkout_2023

total_checkout_2018 <- spl_df %>% filter(CheckoutYear == 2018) %>% summarize(total = sum(Checkouts, na.rm = TRUE)) %>%  pull(total)

total_checkout_2018

Difference_between_2023_and_2018 <- total_checkout_2018 - total_checkout_2023

Difference_between_2023_and_2018

# How many NEW checkouts were there when new checkouts occurred the most? 


most_new_checkouts <- spl_df %>% mutate(new_Checkouts = Checkouts - lag(Checkouts)) %>% filter(new_Checkouts == max(new_Checkouts, na.rm = TRUE)) %>% pull(new_Checkouts)

most_new_checkouts



### First Trends Over Time Chart











