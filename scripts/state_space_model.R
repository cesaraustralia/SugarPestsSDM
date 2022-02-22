library(tidyverse)
library(lubridate)
library(rstan)
library(bayesplot)

source("R/transformations_fun.R")

# read the data
dt <- read_csv("data/Perkensiella.csv") %>% 
  mutate(Date = lubridate::dmy(Date))
head(dt)

#
# data visualisation ------------------------------------------------------
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
  # month_sin = circular_month(stan_data$ym, increment, sin = TRUE),
  # month_cos = circular_month(stan_data$ym, increment, sin = FALSE)
)
model_data


# read the stan file
if(exists("mod")) rm(mod, mod_fit); gc();
mod <- stan_model(file = "ssm_model.stan")

# sample the parameters
mod_fit <- sampling(
  object = mod, 
  data = model_data,
  chains = 1,
  warmup = 2000,
  iter = 6000,
  cores = 8,
  open_progress = FALSE,
  verbose = TRUE
)

# check the prediction and samples
apply(extract(mod_fit)$prediction, 2, median)


# sm <- summary(mod_fit)
# sm$summary |> head()
# plot(mod_fit)

color_scheme_set("red")
mcmc_intervals(mod_fit, pars = c("sigma_s0", "sigma_s", "b1_month"))#, "b2_month"))
traceplot(mod_fit, pars =  c("sigma_s0", "sigma_s", "b1_month"))#, "b2_month"))
# traceplot(mod_fit)


# prediction
ext_fit <- extract(mod_fit)

posterior_pred <- data.frame(
  med = apply(ext_fit$prediction, 2, median),
  sd = apply(ext_fit$prediction, 2, sd)
) %>% 
  mutate(
    step = seq_len(length(med)),
    sd = ifelse(sd > 100, 99, sd), # limit uncertainty before including covariates
    ym = add_months(stan_data$ym, n = increment)
  )

# add day and convert ym to date for time intervals in x.axis
posterior_pred <- posterior_pred %>% 
  mutate(ymd = as.Date(paste0(ym, "-01")))
stan_data <- stan_data %>% 
  mutate(ymd = as.Date(paste0(ym, "-01")))



# plot the prediction
ggplot(data = posterior_pred, aes(x = ymd)) +
  geom_point(aes(x = ymd, y = med, group = 1, color = "Predicted"),
             size = 1.5, shape = 16, data = posterior_pred) +
  geom_path(aes(x = ymd, y = med, group = 1), alpha = 0.8, data = posterior_pred) +
  # geom_ribbon(aes(ymin = med - 2 * sd, ymax = med + 2 * sd, group = 1),
  #             alpha = 0.2,
  #             data = posterior_pred) +
  # ylim(0, 500) +
  geom_point(aes(x = ymd, y = num, color = "Observed"),
             shape = 5, size = 2, data = stan_data) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y") +
  labs(x = "Date", y = "Total observed Perkinsiella", color = "")


