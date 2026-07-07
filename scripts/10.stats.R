# ==============================================================================
# This script is used to produce basic statistics
#
# ==============================================================================
library(dplyr); library(tidyr); library(ggplot2); library(moments)
library(patchwork); library(gridExtra)

raw <- "data/raw"
inter <- "data/intermediate"
proc <- "data/processed"
stats <- "outputs/stats"
figs <- "outputs/figures"


df <- read.csv(file.path(proc, "final.csv"))
df$period   <- factor(df$period,   levels = c("Pre 2013", "Post 2013"))


# period mean
tab_periodo <- df %>%
  group_by(sector, period) %>%
  summarise(
    mean_emiss    = mean(ver_emiss,   na.rm = TRUE),
    mean_coverage = mean(coverage,    na.rm = TRUE),
    mean_elect    = mean(share_elect, na.rm = TRUE),
    mean_ec_int   = mean(ec_intensity,na.rm = TRUE),
    .groups = "drop"
  )
print(tab_periodo)
write.csv(tab_periodo, file.path(stats, "tab_mean_sector_period.csv"), row.names = FALSE)


p_dumbbell <- df %>%
  group_by(sector, period) %>%
  summarise(mean_emiss = mean(ver_emiss, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = mean_emiss, y = sector, color = period)) +
  geom_point(size = 4) +
  geom_line(aes(group = sector), color = "grey60") +
  scale_color_manual(values = c("Pre 2013" = "tomato", "Post 2013" = "steelblue")) +
  labs(x = "Mean Verified Emissions", y = NULL, color = NULL) +
  theme(legend.position = "bottom")
print(p_dumbbell)
ggsave(file.path(stats, "mean_emiss_pre_post.png"), p_dumbbell, width = 8, height = 5, dpi = 300)


df_tot <- df %>%
  group_by(year) %>%
  summarise(ver_emiss = sum(ver_emiss, na.rm = TRUE), .groups = "drop")

p_tot <- ggplot(df_tot, aes(x = year, y = ver_emiss)) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick",
              linewidth = 0.8) +
  geom_line(linewidth = 1.1, color = "grey15") +
  geom_point(size = 2, color = "grey15") +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  annotate("text", x = 2013, y = max(df_tot$ver_emiss), label = "Benchmark (2013)",
           hjust = -0.05, vjust = 1, size = 3, color = "grey40") +
  annotate("text", x = 2019, y = max(df_tot$ver_emiss), label = "MSR / price rise (2019)",
           hjust = -0.05, vjust = 1, size = 3, color = "grey40") +
  scale_x_continuous(breaks = seq(2008, 2023, 2)) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) paste0(x / 1e6, " Mt")
  ) +
  labs(
    title = "Aggregate verified emissions, five EU manufacturing sectors",
    subtitle = "Sum across five countries, 2008–2023",
    x = NULL, y = "Verified emissions (MtCO\u2082e)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line        = element_line(color = "grey70"),
    axis.ticks       = element_line(color = "grey70")
  )

print(p_tot)
ggsave(file.path(stats, "10_total_emiss.png"), p_tot, width = 9, height = 5.5, dpi = 300, bg = "white")