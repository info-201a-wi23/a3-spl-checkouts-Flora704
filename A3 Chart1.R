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