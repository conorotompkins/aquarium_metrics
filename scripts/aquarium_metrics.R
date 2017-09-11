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

df_aquarium_metrics_last <-  df_aquarium_metrics %>% 
  group_by(metric) %>% 
  summarize(measurement = last(measurement))

df_aquarium_metrics %>% 
  filter(before_after == "After") %>% 
  ggplot(aes(metric, measurement)) +
  geom_hline(aes(yintercept = min), color = "red") +
  geom_hline(aes(yintercept = max), color = "red", linetype = 2) +
  geom_jitter(alpha = .5,
              width = .1,
              height = 0) +
  #geom_label_repel(data = df_aquarium_metrics_last, aes(x = metric, y = measurement, label = measurement), segment.size = 5) +
  facet_wrap(~metric, 
             scales = "free")
ggsave(paste0("images/Aquarium metrics summary ", Sys.Date(), ".png"))

df_aquarium_metrics %>% 
  filter(before_after == "After") %>% 
  ggplot(aes(date, measurement, group = 1)) +
  geom_hline(aes(yintercept = min), color = "red") +
  geom_hline(aes(yintercept = max), color = "red", linetype = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~metric,
             scales = "free_y")
ggsave(paste0("images/Aquarium metrics timeline ", Sys.Date(), ".png"))
