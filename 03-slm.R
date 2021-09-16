############# Load packages and data ====
library(dplyr)
library(broom)
library(purrr)
library(ggplot2)
library(GGally)

ads <- fst::read_fst('data/ad_conversion.fst')
str(ads)
summary(ads)

ggpairs(
  ads,
  lower = list(
    continuous = wrap('smooth_loess', colour = 'red4')
  )
)

############# Clicks vs. spent ====
ggplot(
  ads,
  aes(x = spent_usd, y = n_clicks)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_log10(limits = c(NA, NA)) +
  scale_y_log10(limits = c(NA, NA))

ggplot(
  ads,
  aes(x = spent_usd, y = n_clicks)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_sqrt(limits = c(NA, NA)) +
  scale_y_sqrt(limits = c(NA, NA))

transformations = c('I', 'log10', 'sqrt', 'rt4')

# equation <- n_clicks ~ spent_usd
# transform <- c('I', 'log10')
# data = ads
# n_predictions = 100

models <- map(
  transformations,
  ~ lm_transform(n_clicks ~ spent_usd, ads, transform = .)
) %>%
  set_names(transformations)
str(models, 2)

models_summary <- models %>%
  transpose() %>%
  getElement('quality') %>%
  bind_rows(.id = 'model')
models_summary

prediction_data <- models %>%
  transpose() %>%
  getElement('predictions') %>%
  bind_rows(.id = 'model')
prediction_data

ggplot(
  ads,
  aes(x = spent_usd, y = n_clicks)
) +
  geom_point(alpha = .4) +
  geom_line(data = prediction_data, aes(colour = model), size = 1)

############# Once again, but with different variables ====
ggplot(
  ads,
  aes(x = spent_usd, y = n_impressions)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_log10(limits = c(NA, NA)) +
  scale_y_log10(limits = c(NA, NA))

ggplot(
  ads,
  aes(x = spent_usd, y = n_impressions)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_sqrt(limits = c(NA, NA)) +
  scale_y_sqrt(limits = c(NA, NA))

models <- map(
  transformations,
  ~ lm_transform(n_impressions ~ spent_usd, ads, transform = .)
) %>%
  set_names(transformations)
str(models, 2)

models_summary <- models %>%
  transpose() %>%
  getElement('quality') %>%
  bind_rows(.id = 'model')
models_summary

prediction_data <- models %>%
  transpose() %>%
  getElement('predictions') %>%
  bind_rows(.id = 'model')
prediction_data

ggplot(
  ads,
  aes(x = spent_usd, y = n_impressions)
) +
  geom_point(alpha = .4) +
  geom_line(data = prediction_data, aes(colour = model), size = 2) +
  scale_y_log10()

############# Clicks vs. Impressions ====
ggplot(
  ads,
  aes(x = n_impressions, y = n_clicks)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_log10(limits = c(NA, NA)) +
  scale_y_log10(limits = c(NA, NA))

ggplot(
  ads,
  aes(x = n_impressions, y = n_clicks)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE) +
  scale_x_sqrt(limits = c(NA, NA)) +
  scale_y_sqrt(limits = c(NA, NA))

ggplot(
  ads,
  aes(x = n_impressions^.25, y = n_clicks^.25)
) +
  geom_point(alpha = .6, colour = 'red4') +
  geom_smooth(method = 'lm', se = TRUE)

# transformations <- c('I', 'log10')

models <- map(
  transformations,
  ~ lm_transform(n_clicks ~ n_impressions, ads, transform = ., n_predictions = 10000)
) %>%
  set_names(transformations)
str(models, 2)

models_summary <- models %>%
  transpose() %>%
  getElement('quality') %>%
  bind_rows(.id = 'model')
models_summary

prediction_data <- models %>%
  transpose() %>%
  getElement('predictions') %>%
  bind_rows(.id = 'model')
prediction_data

p <- ggplot(
  ads,
  aes(x = n_impressions, y = n_clicks)
) +
  geom_point(alpha = .4) +
  geom_line(data = prediction_data, aes(colour = model), size = 2, alpha = .75)
p

p + scale_y_log10()
p + scale_x_log10() + scale_y_log10()

p + stat_smooth()

p <- ggplot(
  ads,
  aes(x = n_impressions^.25, y = n_clicks^.25)
) +
  geom_point(alpha = .4) +
  geom_line(data = prediction_data, aes(colour = model), size = 2, alpha = .75)
p

mod <- lm(log10(n_clicks) ~ log10(n_impressions), ads)
summary(mod)
glance(mod)
