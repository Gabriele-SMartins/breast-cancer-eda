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

# criar pasta figures/ caso não exista
dir.create("figures", showWarnings = FALSE)

# 1) Boxplot: area_mean por diagnóstico
p_box_area <- ggplot(dados, aes(x = diagnosis, y = area_mean, fill = diagnosis)) +
  geom_boxplot() +
  theme_minimal()

# 2) Scatter: radius_mean vs area_mean (com cor)
p_scatter_size <- ggplot(dados, aes(x = radius_mean, y = area_mean, color = diagnosis)) +
  geom_point(alpha = 0.8) +
  theme_minimal()

# 3) Scatter: concavity_mean vs compactness_mean (sem cor)
p_scatter_shape <- ggplot(dados, aes(x = concavity_mean, y = compactness_mean)) +
  geom_point(alpha = 0.8) +
  theme_minimal()

# 4) Scatter: concavity_mean vs compactness_mean (com cor)
p_scatter_shape_col <- ggplot(dados, aes(x = concavity_mean, y = compactness_mean, color = diagnosis)) +
  geom_point(alpha = 0.8) +
  theme_minimal()

# mostrar na tela (opcional, mas legal)
p_box_area
p_scatter_size
p_scatter_shape
p_scatter_shape_col

# salvar em PNG
ggsave("figures/boxplot_area_mean.png", plot = p_box_area, width = 6, height = 4, dpi = 300)
ggsave("figures/scatter_radius_area.png", plot = p_scatter_size, width = 6, height = 4, dpi = 300)
ggsave("figures/scatter_concavity_compactness.png", plot = p_scatter_shape, width = 6, height = 4, dpi = 300)
ggsave("figures/scatter_concavity_compactness_by_diagnosis.png", plot = p_scatter_shape_col, width = 6, height = 4, dpi = 300)


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
