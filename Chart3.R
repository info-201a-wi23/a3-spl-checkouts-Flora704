
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")


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