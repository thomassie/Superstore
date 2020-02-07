

rm(list = ls())

library(RCurl)
library(tidyverse)
library(readxl)
library(skimr)
library(ggExtra)
library(GGally)
library(lubridate)
library(gganimate)
library(plotly)

dd_org_url <- "https://raw.githubusercontent.com/thomassie/Superstore/master/Data/Superstore/Sample%20-%20EU%20Superstore.csv"
dd_org <- read.csv2(dd_org_url, stringsAsFactors = FALSE)

dim(dd_org)
str(dd_org)
skim(dd_org)
head(dd_org)


dd <- dd_org %>% 
  mutate_at(c("Order.Date", "Ship.Date"), as.Date, format = "%d.%m.%y") %>% 
  mutate_at(c("Sales", "Quantity", "Discount", "Profit"), as.numeric) %>% 
  mutate(year_Order = year(Order.Date))

ggpairs(dd, columns = c("Country", "Category", "Quantity", "Sales", "Profit"))

p1 <- dd %>% 
  # filter(., Country == "Switzerland") %>% 
  ggplot(.,
         aes(x = Sales, 
             y = Profit,
             colour = Country)) +
  geom_point() +
  theme_minimal()

p1

ggMarginal(p1, type = "density")







