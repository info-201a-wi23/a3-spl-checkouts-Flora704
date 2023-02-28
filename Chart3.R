### Third Chart
#To see what  the books that the number of checkouts is over 80 are in 2022, make a bar chart.  

library("dplyr")
library("stringr")
library("ggplot2")
library("scales")


#load file
spl_df <- read.csv("~/Checkout_df.csv", stringsAsFactors = F)


#code

Third_df <- spl_df %>% select(CheckoutYear, Checkouts, Title) %>%
  filter("2022" == CheckoutYear) %>% 
  group_by(Title) %>% 
  summarize(total_Checkouts = sum(Checkouts)) %>% 
  filter(total_Checkouts > 80)

Third_df

#graph
Chart_3 <- ggplot(data = Third_df) +
  geom_col(mapping = aes( x = Title,
                          y = total_Checkouts,
                          color = total_Checkouts)) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_manual(values = c("#005fad", "#a8eddf", "#deffad", "#f2c2ff", "#ffde82", "#fc8b94", "#e6e3ff")) +
  labs(
    title = "Distribution of the books with the number of checkouts (more than 80)",
    x = "Title of the Book",
    y = "The number of checkouts",
  ) +
  coord_flip() 


Chart_3

