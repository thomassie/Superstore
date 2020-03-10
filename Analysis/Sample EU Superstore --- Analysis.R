

rm(list = ls())

library(RCurl)
library(tidyverse)
library(readxl)
library(skimr)
library(forecats)
library(ggExtra)
library(GGally)
library(lubridate)
library(gganimate)
library(plotly)
library(RColorBrewer)
library(wesanderson)



###########################################

dd_org_url <- "https://raw.githubusercontent.com/thomassie/Superstore/master/Data/Superstore/Sample%20-%20EU%20Superstore.csv"
dd_org <- read.csv2(dd_org_url, stringsAsFactors = FALSE)

dim(dd_org)
str(dd_org)
skim(dd_org)
head(dd_org)



###########################################

dd <- dd_org %>% 
  mutate_at(c("Order.Date", "Ship.Date"), as.Date, format = "%d.%m.%y") %>% 
  mutate_at(c("Sales", "Quantity", "Discount", "Profit"), as.numeric) %>% 
  mutate_at(c("Ship.Mode", "Segment", "City", "State", "Country", "Region", "Category", "Sub.Category", "Customer.ID", "Product.ID", "Product.Name"), as.factor) %>% 
  mutate(Year_Order = as.factor(year(Order.Date)))

ggpairs(dd, columns = c("Country", "Category", "Quantity", "Sales", "Profit"))





###########################################

p1 <- dd %>% 
  # filter(., Country == "Switzerland") %>%
  ggplot(.,
         aes(x = Sales, 
             y = Profit,
             colour = Year_Order)) +
  geom_point(alpha = 0.6) +
  # scale_x_log10() +
  # scale_color_brewer(palette = "Paired") +
  scale_colour_manual(values = wes_palette("Darjeeling1")) +
  theme_minimal() +
  theme(legend.position = "none")

p1

ggMarginal(p1, type = "density", fill = "#999999", colour = "#999999")



###########################################



dd_sum <- dd %>% 
  group_by(Year_Order, Region, Country) %>% 
  summarise(Sales_sum = sum(Sales),
            Profit_sum = sum(Profit))

dd_viz <- dd_sum %>% 
  


dd %>% 
  ggplot(aes(x = Country, y = Sales)) +
  geom_tile() +
  coord_flip()

staticplot = ggplot(dd, aes(rank, group = C, 
                                       fill = as.factor(country_name), color = as.factor(country_name))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +







