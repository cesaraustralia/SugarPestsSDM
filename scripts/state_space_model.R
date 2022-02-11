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



# date functions ----------------------------------------------------------
# generate dates, including the gaps
seq_date <- function(date){
  mindate <- min(date) %>% 
    as.Date() %>% 
    format("%Y-%m") %>% 
    paste0("-01") %>% 
    as.Date()
  maxdate <- max(date) %>% 
    as.Date() %>% 
    format("%Y-%m") %>% 
    paste0("-01") %>% 
    as.Date()
  out <- seq.Date(mindate, maxdate, by = "month") %>% 
    as.Date() %>% 
    format("%Y-%m") %>% 
    data.frame(ym = .)
  return(out)
}
# add n months to a vector of dates
add_months <- function(date_list, n = 3){
  lastdate <- date_list %>% 
    paste0("-01") %>% 
    max() %>% 
    as.Date()
  maxdate <- seq.Date(lastdate, by = "month", length.out = n + 1) %>% 
    # as.Date() %>% 
    format("%Y-%m")
  return(c(date_list, maxdate[-1]))
}

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


# full data for Stan
stan_data <- seq_date(dt$Date) %>% 
  left_join(agg_data, by = "ym")
stan_data


# the number of month to forecast to
increment <- 3
# data for the Stan model
model_data <- list(
  N = nrow(agg_data),
  y = agg_data$num,
  T = nrow(stan_data) + increment, 
  t = 1:nrow(agg_data),
  ids = which(!is.na(stan_data$num)),
  month = stan_data$ym %>% 
    add_months(increment) %>% 
    ym() %>% 
    month()
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
  iter = 3000,
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
apply(ext_fit$prediction, 2, median)

posterior_pred <- data.frame(
  med = apply(ext_fit$prediction, 2, median),
  sd = apply(ext_fit$prediction, 2, sd)
) %>% 
  mutate(
    step = seq_len(length(med)),
    sd = ifelse(sd > 100, 99, sd), # limit uncertainty before including covariates
    ym = add_months(stan_data$ym, n = increment)
  )


# plot the prediction
ggplot(data = posterior_pred, 
       aes(x = ym, y = med, group = 1)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_path() +
  geom_ribbon(aes(ymin = med - 2*sd, ymax = med + 2*sd), alpha = 0.1) +
  # ylim(0, 500) +
  geom_point(data = stan_data, aes(x =ym, y = num), 
             color = "red", shape = 5, size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(x = "Time", y = "Total observed Perkinsiella")


