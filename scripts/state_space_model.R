library(tidyverse)
library(rstan)

# read the data
dt <- read_csv("data/Perkensiella.csv") %>% 
  mutate(Date = lubridate::dmy(Date))
head(dt)


# plot sum of abundance by date
dt %>% 
  group_by(Site, Date) %>% 
  summarise(Num = sum(Abundance, na.rm = TRUE)) %>% 
  ggplot(data = ., 
         aes(x = Date, y = Num, col = Site)) +
  geom_point() +
  theme_bw() +
  scale_x_date(date_breaks = "3 month",
               date_minor_breaks = "1 month",
               date_labels = "%Y %b") +
  # scale_color_viridis_c(option = "E", direction = -1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(y = "Perkenseilla observed")



# prepare data for state-space modelling 
monthly_data <- dt %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date),
         site = as.factor(Site)) %>% 
  group_by(site, year, month) %>% 
  summarise(num = sum(Abundance, na.rm = TRUE)) %>% 
  mutate(ym = paste0(year, "-", month)) 
head(monthly_data)


ggplot(data = monthly_data %>% filter(site == "Site2CAIRNS YST 201819"), 
       aes(x = ym, y = num, col = site, group = 1)) +
  geom_point() +
  geom_path()









