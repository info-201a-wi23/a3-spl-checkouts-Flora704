#summary

library("dplyr")
library("stringr")
library("ggplot2")

spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)

# What is the average number of checkouts in this dataset?
avg_num_checkout <- spl_df %>% summarize(mean_checkout = mean(Checkouts)) %>% pull(mean_checkout)

avg_num_checkout

# What is the most checkouts in 2020 in this dataset?
max_checkout_2020 <- spl_df %>% filter(CheckoutYear == 2020) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(Checkouts)

max_checkout_2020

# What is the title of the most checkouts in 2020 in this dataset? Pull the title.
max_checkout_2020_title <- spl_df %>% filter(CheckoutYear == 2020) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(Title)

max_checkout_2020_title


# What is the most checkouts in 2020 for "ebook" in this dataset?
max_ebook_2020 <- spl_df %>% filter(MaterialType == "EBOOK") %>% filter(CheckoutYear == 2020) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(Checkouts)

max_ebook_2020

# What is the title of the most checkouts in 2020 for "ebook" in this dataset? Pull the title.
max_ebook_2020_title <- spl_df %>% filter(MaterialType == "EBOOK") %>% filter(CheckoutYear == 2020) %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(Title)

max_ebook_2020_title

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
#To see the trend of books' checkouts, how has the number of checkouts changed over time? 

df <- spl_df %>% select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(total_Checkouts = sum(Checkouts)) 


First_df <- spl_df %>% select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear) %>%
  summarize(total_Checkouts = sum(Checkouts)) 

bp <- ggplot(data = First_df) +
  geom_col(mapping = aes( x = CheckoutYear,
                          y = total_Checkouts,
                          fill = CheckoutYear))









