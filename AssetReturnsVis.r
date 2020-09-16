library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
library(highcharter)

# The symbols vector holds our tickers. 
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2005-01-01", 
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)

# XTS method
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

# Tidyverse method, to long, tidy format
asset_returns_long <- 
  prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns))))

highchart(type = "stock") %>% 
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns_xts$SPY, 
                name = names(asset_returns_xts$SPY)) %>%
  hc_add_series(asset_returns_xts$EFA, 
                name = names(asset_returns_xts$EFA)) %>%
  hc_add_series(asset_returns_xts$IJS, 
                name = names(asset_returns_xts$IJS)) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE)


hc_spy <- hist(asset_returns_xts$SPY, breaks = 50, plot = FALSE)

hchart(hc_spy) %>% 
  hc_title(text = "SPY Log Returns Distribution")

# Make so all titles centered in the upcoming ggplots
theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>% 
  ggplot(aes(x = returns, fill = asset)) + 
  geom_histogram(alpha = 0.25, binwidth = .01)


asset_returns_long %>% 
  ggplot(aes(x = returns, fill = asset)) + 
  geom_histogram(alpha = 0.25, binwidth = .01) + 
  facet_wrap(~asset) + 
  ggtitle("Monthly Returns Since 2005")

asset_returns_long %>% 
  ggplot(aes(x = returns, colour = asset, fill = asset)) +
  stat_density(geom = "line", alpha = 1) +
  ggtitle("Monthly Returns Since 2005") +
  xlab("monthly returns") +
  ylab("distribution") 


asset_returns_long %>% 
  ggplot(aes(x = returns, colour = asset, fill = asset)) +
  stat_density(geom = "line", alpha = 1) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2005") +
  xlab("monthly returns") +
  ylab("distribution") 


asset_returns_long %>% 
  ggplot(aes(x = returns, colour = asset, fill = asset)) +
  stat_density(geom = "line", alpha = 1) +
  geom_histogram(alpha = 0.25, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2005") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme(plot.title = element_text(colour = "cornflowerblue"),  
        strip.text.x = element_text(size = 8, colour = "white"), 
        strip.background = element_rect(colour = "white", fill = "cornflowerblue"), 
        axis.text.x = element_text(colour = "cornflowerblue"), 
        axis.text = element_text(colour = "cornflowerblue"), 
        axis.ticks.x = element_line(colour = "cornflowerblue"), 
        axis.text.y = element_text(colour = "cornflowerblue"), 
        axis.ticks.y = element_line(colour = "cornflowerblue"),
        axis.title = element_text(colour = "cornflowerblue"),
        legend.title = element_text(colour = "cornflowerblue"),
        legend.text = element_text(colour = "cornflowerblue")
  )
