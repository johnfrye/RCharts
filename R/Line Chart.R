
# Environment -------------------------------------------------------------

rm(list=ls())
pacman::p_load(stats, devtools, purrr, httr, readxl, openxlsx, 
               data.table, reshape2, dplyr, WorksheetFunctions, 
               writexl, tidyr, feather, smutilities,
               ggplot2, scales, ggthemes)

setoptions()
wd <- here::here()
ad <- here::here("analysis")
dd <- here::here("data")
gd <- here::here("graphics")
fd <- here::here("feather")

ses.info <- sessionInfo()
time <- time_stamp()

mem_used()

# Load Data ---------------------------------------------------------------

stock_amzn <- import("~/Core_Charts/data/AMZN_stock.csv")
names(stock_amzn) <- fixnames(stock_amzn)
stock_amzn %>% names()
stock_amzn %>% head()

mutate(ymax = ceiling(1.2*max(value)))

# Plot --------------------------------------------------------------------

ggplot(stock_amzn, aes(x = date, close, group = 1)) +
  geom_line(color = 'cyan') +
  geom_area(fill = 'cyan', alpha = .1) +
  labs(x = 'Date',
       y = 'Closing\nPrice',
       title = "Amazon's stock price has increased dramatically\nover the last 20 years") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444"),
        panel.background = element_rect(fill = '#444B5A'),
        panel.grid.minor = element_line(color = '#4d5566'),
        panel.grid.major = element_line(color = '#586174'),
        plot.title = element_text(size = 28),
        axis.title = element_text(size = 18, color = '#555555'),
        axis.title.y = element_text(vjust = 1, angle = 0),
        axis.title.x = element_text(hjust = 0))

# Revised Version ---------------------------------------------------------

df2 <- df %>% 
  mutate(ymax = ceiling(1.2*max(value)))

title_text <- "Sales and Profitability 2010 - 2017 YTD"
subtitle_text <- "($MM)"

g <- ggplot(df2, aes(x = year, y = value, group = 1)) +
  geom_line(color = 'darkblue') +
  geom_ribbon(aes(ymin = min(value), ymax = value), 
              fill = 'cadetblue', alpha = .1) +
  geom_text(aes(label = comma(value)),
            vjust = -0.5,
            color = "darkblue", 
            size = 3) +
  geom_blank(aes(y = ymax)) +
  facet_wrap(store~metric, ncol = 3, scales = "free_y") +
  labs(x = 'Fiscal Year',
       y = '$, Millions',
       title = title_text,
       subtitle = subtitle_text) +
  theme(strip.text = element_text(size = 9, 
                                  color = "darkblue"))

filename <- paste0(title_text, ".pdf")
ggsave(fp(gd, filename), g, width = 12, height = 8.5)







