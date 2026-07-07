# ==============================================================================
# This script is used to produce sector temporal trends
# The sector must me selected manually
#
#==============================================================================
library(dplyr); library(tidyr); library(ggplot2)

raw <- "data/raw"
inter <- "data/intermediate"
proc <- "data/processed"
figs <- "outputs/figures"
sectors <- "outputs/sectors"


df <- read.csv(file.path(proc, "final.csv"))
sect <- "iron_steel"


#=============================================================================
df_country <- df %>%
  filter(sector == sect) %>%
  mutate(emiss_intensity = ver_emiss / output) %>%
  select(year, country, emissions = ver_emiss, surplus, emiss_intensity, output)


df_mean <- df_country %>%
  group_by(year) %>%
  summarise(across(c(emissions, surplus, emiss_intensity, output),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(country = "MEAN")

df_plot <- bind_rows(df_country, df_mean) %>%
  pivot_longer(c(emissions, surplus, emiss_intensity, output),
               names_to = "metric", values_to = "value")


metric_labels <- c(
  emissions       = "Verified Emissions",
  output          = "Output",
  surplus         = "Surplus",
  emiss_intensity = "Emission intensity"
)
df_plot$metric <- factor(df_plot$metric, levels = names(metric_labels))


country_labels <- c(
  "fra"   = "France",
  "ger"   = "Germany",
  "ita"   = "Italy",
  "spa"   = "Spain",
  "pol"   = "Poland",
  "MEAN" = "Total"   
)

countries_only <- setdiff(unique(df_plot$country), "MEAN")
df_plot$country <- factor(df_plot$country, levels = c(countries_only, "MEAN"))


country_colors <- c(
  setNames(palette.colors(length(countries_only), palette = "Okabe-Ito")[seq_along(countries_only)],
           countries_only),
  MEAN = "black"
)

country_widths <- c(
  setNames(rep(0.55, length(countries_only)), countries_only),
  MEAN = 1.3
)


p <- ggplot(df_plot,
            aes(year, value,
                colour = country, linewidth = country, group = country)) +
  geom_line() +
  facet_wrap(~ metric, scales = "free_y", nrow = 1,
             labeller = labeller(metric = metric_labels),
             strip.position = "top") +
  scale_colour_manual(values = country_colors, labels = country_labels, name = NULL) +
  scale_linewidth_manual(values = country_widths, labels = country_labels, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = NULL, y = NULL,
       title = "Paper") +                            
  theme_bw(base_size = 11) +
  theme(
    plot.title        = element_text(face = "bold", size = 12,
                                     margin = margin(b = 8)),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = "grey92"),
    panel.border      = element_rect(colour = "grey70"),
    strip.background  = element_rect(fill = "grey95", colour = NA),
    strip.text        = element_text(face = "bold", hjust = 0,
                                     margin = margin(4, 4, 4, 4)),
    axis.ticks        = element_line(colour = "grey60"),
    legend.position   = "right",
    legend.key        = element_blank(),
    legend.key.height = unit(0.7, "lines"),
    legend.text       = element_text(size = 9),
    plot.margin       = margin(10, 10, 10, 10)
  )

p


ggsave(file.path(sectors, "paper_trends.png"), p,                          
       width = 11, height = 5.5)
