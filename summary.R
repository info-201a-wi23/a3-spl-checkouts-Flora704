#summary

library("dplyr")
library("stringr")
library("ggplot2")

spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)

# What is the average number of checkouts for this dataset related to George Orwell?
avg_num_checkout <- summarize(mean_checkout = mean(Checkouts)) %>%pull(mean_checkout)

# What is the month or year with the most/least checkouts for a book that you're interested in?
min_checkout <- spl_df %>% filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
min_checkout 

max_checkout <- spl_df %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
max_checkout 

# What is the month or year with the most/least checkouts for ebooks?
max_ebook <- spl_df %>% group_by(MaterialType == "EBOOK") %>% filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
max_ebook 

min_ebook <- spl_df %>% group_by(MaterialType == "EBOOK") %>% filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>% pull(CheckoutYear)
min_ebook
# How has the number of print book checkouts changed over time? Compare the total number of checkouts now and five years ago. 
total_checkout_now <- spl_df %>% filter(CheckoutYear == 2023) %>% summarize(total = sum(Checkouts, na.rm = TRUE)) %>%  pull(total)

total_checkout_now

total_checkout_2018 <- spl_df %>% filter(CheckoutYear == 2018) %>% summarize(total = sum(Checkouts, na.rm = TRUE)) %>%  pull(total)

total_checkout_2018

# How many NEW checkouts were there when the most checkouts occurred?  
most_new_checkouts <- spl_df %>% mutate(new_checkouts = Checkouts - lag(Checkouts)) %>% filter(new_checkouts == max(new_checkouts, na.rm = T)) %>% pull(new_checkouts)

most_new_checkouts







