library(tidyverse)

dados <- read_csv("data/data.csv")

dados
colnames(dados)
head(dados$radius_mean)

dados %>%
  summarise(
    media = mean(radius_mean),
    desvio = sd(radius_mean)
  )

ggplot(dados, aes(x = diagnosis, y = area_mean)) +
  geom_boxplot()

ggplot(dados, aes(x = radius_mean, y = area_mean, color = diagnosis)) +
  geom_point()

ggplot(dados, aes(x = concavity_mean, y = compactness_mean)) +
  geom_point()

ggplot(dados, aes(x = concavity_mean, y = compactness_mean, color = diagnosis)) +
  geom_point()

dados %>%
  select(concavity_mean, compactness_mean, area_mean, radius_mean) %>%
  cor()

dados %>%
  group_by(diagnosis) %>%
  summarise(
    media_area = mean(area_mean),
    desvio_area = sd(area_mean)
  )

dados %>%
  group_by(diagnosis) %>%
  summarise(
    media_concavity = mean(concavity_mean),
    desvio_concavity = sd(concavity_mean)
  )

t.test(area_mean ~ diagnosis, data = dados)
t.test(concavity_mean ~ diagnosis, data = dados)
