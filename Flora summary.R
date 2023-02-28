#summary

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)

# What is the average number of checkouts in this dataset?
avg_num_checkout <- spl_df %>% 
  summarize(mean_checkout = mean(Checkouts)) %>% 
  pull(mean_checkout)

avg_num_checkout

# What is the most checkouts in 2020 in this dataset?
max_checkout_2020 <- spl_df %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Checkouts)

max_checkout_2020

# What is the title of the most checkouts in 2020 in this dataset? Pull the title.
max_checkout_2020_title <- spl_df %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>%
  pull(Title)

max_checkout_2020_title


# What is the most checkouts in 2020 for "ebook" in this dataset?
max_ebook_2020 <- spl_df %>% 
  filter(MaterialType == "EBOOK") %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Checkouts)

max_ebook_2020

# What is the title of the most checkouts in 2020 for "ebook" in this dataset? Pull the title.
max_ebook_2020_title <- spl_df %>% 
  filter(MaterialType == "EBOOK") %>% 
  filter(CheckoutYear == 2020) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Title)

max_ebook_2020_title

# How has the number of print book checkouts changed over time? Compare the total number of checkouts now and five years ago. 
total_checkout_2023 <- spl_df %>% 
  filter(CheckoutYear == 2023) %>% 
  summarize(total = sum(Checkouts, na.rm = TRUE)) %>%  
  pull(total)

total_checkout_2023

total_checkout_2018 <- spl_df %>% 
  filter(CheckoutYear == 2018) %>% 
  summarize(total = sum(Checkouts, na.rm = TRUE)) %>%  
  pull(total)

total_checkout_2018

Difference_between_2023_and_2018 <- total_checkout_2018 - total_checkout_2023

Difference_between_2023_and_2018

# How many NEW checkouts were there when new checkouts occurred the most? 
most_new_checkouts <- spl_df %>% 
  mutate(new_Checkouts = Checkouts - lag(Checkouts)) %>% 
  filter(new_Checkouts == max(new_Checkouts, na.rm = TRUE)) %>% 
  pull(new_Checkouts)

most_new_checkouts


### First Trends Over Time Chart
#To see the trend of 'BOOK's checkouts, how has the number of checkouts changed over time? 

df <- spl_df %>% 
  select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(total_Checkouts = sum(Checkouts)) 


First_df <- spl_df %>% select(MaterialType, CheckoutYear, Checkouts) %>%
  group_by(CheckoutYear) %>% 
  filter("2014" <= CheckoutYear & CheckoutYear <= "2023") %>% 
  summarize(total_Checkouts = sum(Checkouts)) 

bp_fir <- ggplot(data = First_df) +
  geom_col(mapping = aes( x = CheckoutYear,
                          y = total_Checkouts,
                          fill = CheckoutYear)) +
  scale_y_continuous(labels = label_number_si()) +
  labs(
    title = "Trend of Books' Checkouts between 2014 and 2023",
    x = "Checkout Year",
    y = "The number of checkouts",
    color = "Checkout Year"
  )


bp_fir

### Second Trends Over Time Chart
#To see the trend of 'EBOOK's checkouts, how has the number of checkouts changed over time? 

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
  scale_color_brewer(palette = "Greens") +
  labs(
    title = "Trend of Ebooks' Checkouts between 2014 and 2023",
    x = "Checkout Year",
    y = "The number of checkouts",
  ) 

bp_sec

### Third Chart
#To see what  the books that the number of checkouts is over 80 are in 2022, make a pie chart. 

Third_df <- spl_df %>% select(CheckoutYear, Checkouts, Title) %>%
  filter("2022" == CheckoutYear) %>% 
  group_by(Title) %>% 
  summarize(total_Checkouts = sum(Checkouts)) %>% 
  filter(total_Checkouts > 80)

Third_df

bp_last <- ggplot(data = Third_df) +
  geom_col(mapping = aes( x = Title,
                          y = total_Checkouts,
                          fill = total_Checkouts)) +
  scale_y_continuous(labels = label_number_si()) +
  scale_color_brewer(palette = "Greens") +
  labs(
    title = "Distribution of the books with the number of checkouts (more than 80)",
    x = "Title of the Book",
    y = "The number of checkouts",
  ) +
  coord_flip() 


bp_last








