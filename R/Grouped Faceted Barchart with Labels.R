df_sel <- df4 %>% 
  filter(metric %in% c("Dairy Trips", "Percent Dairy Trips", 
                       "Dry Grocery Trips", "Percent Dry Grocery Trips",
                       "Meat Trips", "Percent Meat Trips",
                       "Produce Trips", "Percent Produce Trips")) %>%
  spread(metric, value) %>% 
  mutate(`Dairy Trips` = round(`Dairy Trips`/1e6, 1),
         `Dry Grocery Trips` = round(`Dry Grocery Trips`/1e6, 1),
         `Meat Trips` = round(`Meat Trips`/1e6, 1),
         `Produce Trips` = round(`Produce Trips`/1e6, 1)) %>% 
  rename(`Dairy Trips (MM)` = `Dairy Trips`,
         `Dry Grocery Trips (MM)` = `Dry Grocery Trips`,
         `Meat Trips (MM)` = `Meat Trips`,
         `Produce Trips (MM)` = `Produce Trips`) %>% 
  gather(metric, value, `Dairy Trips (MM)`:`Percent Produce Trips`) %>% 
  group_by(metric) %>% 
  mutate(ymin = 0,
         ymax = ceiling(1.2*max(value))) %>% 
  ungroup()
# View(df_sel)

# Labels inside bars
title_text <- "Sales Metrics by Customer Segment"
g <- ggplot(df_sel, aes(x = customer_segment, y = value, fill = customer_segment)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = comma(value)),
            vjust = 0,
            color = "black", 
            size = 3.5) +
  # fontface = "bold") +
  geom_blank(aes(y = ymax)) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ metric, ncol = 2, scales = "free") +
  labs(title = title_text,
       subtitle = "Conventional Banners, August 2016 - July 2017",
       x = "Value",
       y = "") +
  scale_fill_discrete(name="Customer\nSegment") +
  theme(strip.text = element_text(size = 10, face="bold"))
g
