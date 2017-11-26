library(googlesheets)
library(tidyverse)
library(lubridate)
library(ggrepel)

theme_set(theme_bw())

rm(list = ls())

set.seed(1234)

gs_ls()

aquarium_metrics <- gs_title("Aquarium metrics")

df_aquarium_metrics <- aquarium_metrics %>% 
  gs_read(ws = 1)

df_aquarium_metrics_recommended <- aquarium_metrics %>% 
  gs_read(ws = 2)

df_aquarium_metrics <- df_aquarium_metrics %>% 
  left_join(df_aquarium_metrics_recommended) %>% 
  mutate(date = mdy(date))

df_aquarium_metrics %>% 
  group_by(metric) %>% 
  summarize(last_date = last(date), 
            last_measurement = last(measurement))-> df_aquarium_metrics_tag

df_aquarium_metrics %>% 
  filter(before_after == "After") %>% 
  ggplot(aes(metric, measurement)) +
  geom_hline(aes(yintercept = min), color = "red") +
  geom_hline(aes(yintercept = max), color = "red", linetype = 2) +
  geom_jitter(alpha = .5,
              width = .05,
              height = 0) +
  geom_point(data = df_aquarium_metrics_tag, aes(x = metric, y = last_measurement), shape = 21, size = 3, fill = "red") +
  facet_wrap(~metric, 
             scales = "free",
             nrow = 1)
ggsave(paste0("images/Aquarium metrics summary ", Sys.Date(), ".png"))

df_aquarium_metrics %>% 
  filter(before_after == "After") %>% 
  ggplot(aes(date, measurement, group = 1)) +
  geom_hline(aes(yintercept = min), color = "red") +
  geom_hline(aes(yintercept = max), color = "red", linetype = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~metric,
             scales = "free_y",
             ncol = 1)
ggsave(paste0("images/Aquarium metrics timeline ", Sys.Date(), ".png"))