library(tidyverse)
library(lubridate)
library(rstan)
library(bayesplot)

#
# data visualisation ------------------------------------------------------
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


## plot monthly change 
monthly_data %>% 
  # filter(site == levels(monthly_data$site)[15]) %>% 
  arrange(ym) %>% 
  ggplot(data = ., 
         aes(x = ym, y = num, col = site, group = 1)) +
  geom_point() +
  geom_path()


## average the number based on month/year over sites
dt %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date),
         site = as.factor(Site)) %>% 
  group_by(year, month) %>% 
  summarise(num = sum(Abundance, na.rm = TRUE)) %>% 
  mutate(ym = paste0(year, "-", month)) %>% 
  arrange(ym) %>% 
  ggplot(data = .,
         aes(x = ym,  y = num, group = 1)) +
  geom_point() +
  geom_path() +
  theme(axis.text.x = element_text(angle = 45))



# Stan model --------------------------------------------------------------
options(mc.cores = 8)
rstan_options(auto_write = TRUE)

# aggregating data
agg_data <- dt %>% 
  mutate(
    # year = year(Date),
    # month = month(Date),
    # day = day(Date),
    ym = format(as.Date(Date), "%Y-%m"),
    site = as.factor(Site)) %>% 
  group_by(ym) %>% 
  summarise(num = sum(Abundance, na.rm = TRUE)) %>% 
  # mutate(ym = paste0(year, "-", month)) %>% 
  arrange(ym)

# generate dates, including the gaps
mindate <- min(dt$Date) %>% 
  as.Date() %>% 
  format("%Y-%m") %>% 
  paste0("-01") %>% 
  as.Date()
maxdate <- max(dt$Date) %>% 
  as.Date() %>% 
  format("%Y-%m") %>% 
  paste0("-01") %>% 
  as.Date()
fulldate <- seq.Date(mindate, maxdate, by = "month") %>% 
  as.Date() %>% 
  format("%Y-%m") %>% 
  data.frame(ym = .)

stan_data <- left_join(fulldate, agg_data, by = "ym")
stan_data

# data for stan model
model_data <- list(
  N = nrow(agg_data),
  y = agg_data$num,
  T = nrow(agg_data) + 3, # test forecasting
  # t = which(!is.na(stan_data$num))
  t = 1:nrow(agg_data)
)
model_data

# read the stan file
rm(mod)
mod <- stan_model(file = "ssd_model.stan")

# sample the parameters
mod_fit <- sampling(
  object = mod, 
  data = model_data,
  chains = 4,
  warmup = 1000,
  iter = 4000,
  cores = 8,
  verbose = TRUE
)

sm <- summary(mod_fit)
sm$summary |> head()


plot(mod_fit)

color_scheme_set("red")
mcmc_intervals(mod_fit, pars = c("mu[1]", "sigma_s", "sigma_t_s"))
traceplot(mod_fit, pars = c("mu[1]", "sigma_s", "sigma_t_s"))
traceplot(mod_fit)


# prediction
ext_fit <- extract(mod_fit)

posterior_pred <- data.frame(
  med = apply(ext_fit$prediction, 2, median),
  sd = apply(ext_fit$prediction, 2, sd)
) %>% 
  mutate(step = seq_len(length(med)),
         sd = ifelse(sd > 100, 99, sd), # bound the uncertainty before including covariates
         ym = c(agg_data$ym, "2020-12", "2021-01", "2021-02"))

ggplot(data = posterior_pred, 
       aes(x = ym, y = med, group = 1)) +
  geom_point() +
  geom_path() +
  geom_ribbon(aes(ymin = med - 2*sd, ymax = med + 2*sd), alpha = 0.1) +
  # ylim(0, 500) +
  geom_point(data = stan_data, aes(x =ym, y = num), color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Time", y = "Total observed Perkinsiella")

