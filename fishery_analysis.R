# 2024-12-20

# packages
library(tidyverse)
library(stats)
library(vegan)
library(mgcv)
# library(gratia)
library(showtext)
library(ggpubr)
library(magick)

# フォントの埋め込み
font_add_google(name = "Noto Sans", family = "ns")
theme_pubr(base_family = "ns", base_size = 10) |> 
  theme_set()
showtext_auto()

# data preparation
country_group = read_rds("./data/country_groups.rds")
water_area = read_rds("./data/water_area.rds")
production = read_rds("./data/global_fishery_production.rds")

# 東シナ海沿岸国の漁獲量の推移
country_group |> 
  inner_join(production) |> 
  group_by(Name_En, PERIOD) |> 
  summarise(sum = sum(VALUE),
            .groups = "drop") |> 
  mutate(Name_En = str_replace(Name_En, "Taiwan Province of China", "Taiwan"),
         Name_En = str_replace(Name_En, "Republic of Korea", "Korea")) |> 
  ggplot() + 
  geom_line(
    aes(x = PERIOD, y = log10(sum / 10^(7)), linetype = Name_En, colour = Name_En)
  ) + 
  scale_x_continuous(
    limits = c(1950, 2030),
    breaks = seq(1950, 2030, 20)
  ) + 
  scale_y_continuous(
    name = "log10Fishery Production (Mt)",
    limits = c(-2, 2),
    breaks = seq(-2, 2, length = 5)
  ) + 
  scale_colour_viridis_d(end = 0.8) + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.background = element_blank(),
    axis.title.x = element_blank()
  )

height = 80
width = height
pdfname = "./image/log10fishery_production_in_ECS.pdf"
pngname = str_replace(pdfname, "pdf", "PNG")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) |>
  image_write(pngname)

# Captureだけプロット
country_group |> 
  inner_join(production) |> 
  filter(PRODUCTION_SOURCE_DET.CODE == "CAPTURE") |> 
  group_by(Name_En, PERIOD) |> 
  summarise(sum = sum(VALUE),
            .groups = "drop") |> 
  mutate(Name_En = str_replace(Name_En, "Taiwan Province of China", "Taiwan"),
         Name_En = str_replace(Name_En, "Republic of Korea", "Korea")) |> 
  ggplot() + 
  geom_line(
    aes(x = PERIOD, y = log10(sum / 10^(7)), linetype = Name_En, colour = Name_En)
  ) + 
  scale_x_continuous(
    limits = c(1950, 2030),
    breaks = seq(1950, 2030, 20)
  ) + 
  scale_y_continuous(
    name = "log10Fishery Production (Mt)",
    limits = c(-2.5, 2.0),
    breaks = seq(-2.5, 2.0, length = 6)
  ) + 
  scale_colour_viridis_d(end = 0.8) + 
  theme(
    legend.position = c(0, 1),
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.justification = c(0, 1),
    axis.title.x = element_blank()
  )

height = 80
width = height
pdfname = "./image/log10fishery_production_in_ECS_by_capture.pdf"
pngname = str_replace(pdfname, "pdf", "PNG")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) |>
  image_write(pngname)

# capture 以外のデータをプロット
country_group |> 
  inner_join(production) |> 
  filter(!PRODUCTION_SOURCE_DET.CODE == "CAPTURE") |> 
  group_by(Name_En, PERIOD) |> 
  summarise(sum = sum(VALUE),
            .groups = "drop") |> 
  mutate(Name_En = str_replace(Name_En, "Taiwan Province of China", "Taiwan"),
         Name_En = str_replace(Name_En, "Republic of Korea", "Korea")) |> 
  ggplot() + 
  geom_line(
    aes(x = PERIOD, y = log10(sum / 10^(7)), linetype = Name_En, colour = Name_En)
  ) + 
  scale_x_continuous(
    limits = c(1950, 2030),
    breaks = seq(1950, 2030, 20)
  ) + 
  scale_y_continuous(
    name = "log10Fishery Production (Mt)",
    limits = c(-4, 2.0),
    breaks = seq(-4, 2.0, length = 4)
  ) + 
  scale_colour_viridis_d(end = 0.8) + 
  theme(
    legend.position = c(0, 1),
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.justification = c(0, 1),
    axis.title.x = element_blank()
  )

height = 80
width = height
pdfname = "./image/log10fishery_production_in_ECS_without_capture.pdf"
pngname = str_replace(pdfname, "pdf", "PNG")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) |>
  image_write(pngname)

# 採捕データだけで一般化加法モデル
df = country_group |> 
  inner_join(production) |> 
  filter(PRODUCTION_SOURCE_DET.CODE == "CAPTURE") |> 
  group_by(Name_En, PERIOD) |> 
  summarise(sum = log10(sum(VALUE) / 10 ^ 7),
            .groups = "drop") |> 
  mutate(Name_En = str_replace(Name_En, "Taiwan Province of China", "Taiwan"),
         Name_En = str_replace(Name_En, "Republic of Korea", "Korea")) |> 
  mutate(Name_En = factor(Name_En, levels = unique(Name_En)))

null_m = gam(formula = sum ~ 1, 
    data = df,
    family = gaussian(link = "identity"))

m1 = gam(formula = sum ~ s(PERIOD, k = 5, by = Name_En) + Name_En,
    data = df,
    family = gaussian(link = "identity"))

AIC(null_m, m1)

pdata = df |> 
  group_by(Name_En) |> 
  expand(PERIOD = seq(min(PERIOD), max(PERIOD), length = 24))

pdata = predict(m1, newdata = pdata, se.fit = TRUE,
                type = "response") |> 
  bind_cols(pdata)

# smooth_data = smooth_estimates(m1)

df |> 
  ggplot() + 
  geom_point(
    aes(x = PERIOD, y = sum, colour = Name_En),
    alpha = 0.5
  ) + 
  geom_line(
    aes(x = PERIOD, y = fit, colour = Name_En),
    data = pdata
  ) +
  geom_ribbon(
    aes(x = PERIOD, ymin = fit - se.fit, ymax = fit + se.fit, fill = Name_En),
    alpha = 0.3,
    data = pdata
  ) + 
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) + 
  scale_x_continuous(
    limits = c(1950, 2030),
    breaks = seq(1950, 2030, 20)
  ) + 
  scale_y_continuous(
    name = "log10Fishery Production (Mt)",
    limits = c(-2.5, 2.0),
    breaks = seq(-2.5, 2.0, length = 4)
  ) + 
  scale_colour_viridis_d(end = 0.8) + 
  scale_fill_viridis_d(end = 0.8)

height = 80
width = height
pdfname = "./image/gam_log10fishery_capture.pdf"
pngname = str_replace(pdfname, "pdf", "PNG")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) |>
  image_write(pngname)

anova(m1)
