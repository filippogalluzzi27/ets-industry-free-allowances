# ==============================================================================
# This script is used to produce basic statistics
#
#==============================================================================
library(dplyr); library(tidyr); library(ggplot2); library(gridExtra)

raw <- "data/raw"
inter <- "data/intermediate"
proc <- "data/processed"
stats <- "outputs/stats"
trend <- "outputs/trends"


df <- read.csv(file.path(proc, "final.csv"))
df$period   <- factor(df$period,   levels = c("Pre 2013", "Post 2013"))



#==============================================================================
# EUA TREND
# ==============================================================================
df_eua <- df %>% distinct(year, eua_price)   

fig1 <- ggplot(df_eua, aes(x = year, y = eua_price)) +
  # bande dei due regimi: schiacciato vs salita
  annotate("rect", xmin = -Inf, xmax = 2019, ymin = -Inf, ymax = Inf,
           fill = "grey95", alpha = 0.6) +
  geom_vline(xintercept = c(2013, 2019), linetype = "dashed",
             color = "grey55", linewidth = 0.5) +
  geom_line(linewidth = 1.1, color = "grey15") +
  geom_point(size = 1.8, color = "grey15") +
  annotate("text", x = 2013, y = Inf, label = "Benchmark (2013)",
           vjust = 1.4, hjust = -0.05, size = 3, color = "grey40") +
  annotate("text", x = 2019, y = Inf, label = "MSR / price rise (2019)",
           vjust = 1.4, hjust = -0.05, size = 3, color = "grey40") +
  scale_x_continuous(breaks = seq(2008, 2023, 2),
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = function(x) paste0(x, " \u20ac"),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "EU ETS allowance price",
    subtitle = "Annual average, EEX spot, 2008\u20132023",
    x = NULL, y = "EUA price (\u20ac/tCO\u2082e)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title          = element_text(face = "bold", size = 13),
    plot.subtitle       = element_text(color = "grey40", size = 10),
    plot.title.position = "plot",
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    axis.line           = element_line(color = "grey70"),
    axis.ticks          = element_line(color = "grey70")
  )

fig1
ggsave(file.path(trend, "eua.png"), plot = fig1, width = 8, height = 5, dpi = 300, bg = "white")


# ==============================================================================
# Electricity/Gas TREND
# ==============================================================================
fig2 <- ggplot(data = df, aes(x = year, y = ratio_price, group = country, color = country)) +
  geom_line(linewidth = 0.5) +                      
  labs(title="Electricity price/Gas price",
       x="Year", y="")  +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot")+
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 2013, y = Inf, label = "Phase III",
           vjust = 1.5, hjust = 1.1, size = 3)+
  annotate("text", x = 2019, y = Inf, label = "Price rise",
           vjust = 1.5, hjust = 1.1, size = 3)+
  scale_color_viridis_d(option = "D", labels = c(
    "fra" = "France","ger" = "Germany","ita" = "Italy","spa" = "Spain", "pol"= "Poland"))+
  stat_summary(aes(group = 1),fun = mean, geom = "line", color = "red", linewidth = 1.2)

fig2
ggsave(file.path(trend, "ratio_price.png"), plot = fig2, width = 8, height = 5, dpi = 300)



# ==============================================================================
# VERIFIED EMISSIONS
# ==============================================================================
df_mean <- df %>%
  group_by(year, sector) %>%
  summarise(ver_emiss = mean(ver_emiss, na.rm = TRUE), .groups = "drop")

fig3 <- ggplot(data = df_mean, aes(x = year, y = ver_emiss, group = sector, color = sector)) +
  scale_y_log10()+
  geom_line(linewidth = 1) +                        
  labs(title="Verified Emissions (log)",
       x="Year", y="")  +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot")+
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 2013, y = Inf, label = "Phase III",
           vjust = 1.5, hjust = 1.1, size = 3)+
  annotate("text", x = 2019, y = Inf, label = "Price rise",
           vjust = 1.5, hjust = 1.1, size = 3)+
  scale_color_viridis_d(option = "D", labels = c(
    "iron_steel" = "Iron & Steel","ceramics" = "Ceramics","glass" = "Glass",
    "paper" = "Paper", "pulp"= "Pulp"))+
  stat_summary(aes(group = 1),fun = mean, geom = "line", color = "red", linewidth = 1.2)


fig3
ggsave(file.path(trend, "emissions.png"), plot = fig3, width = 8, height = 5, dpi = 300)


# ==============================================================================
# FREE ALLOWANCES
# ==============================================================================
df_mean <- df %>%
  group_by(year, sector) %>%
  summarise(free_all = mean(free_all, na.rm = TRUE), .groups = "drop")

fig4 <- ggplot(data = df_mean, aes(x = year, y = free_all, group = sector, color = sector)) +
  scale_y_log10()+
  geom_line(linewidth = 1) +                        
  labs(title="Free allowances (log)",
       x="Year", y="")  +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot")+
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 2013, y = Inf, label = "Phase III",
           vjust = 1.5, hjust = 1.1, size = 3)+
  annotate("text", x = 2019, y = Inf, label = "Price rise",
           vjust = 1.5, hjust = 1.1, size = 3)+
  scale_color_viridis_d(option = "D", labels = c(
    "iron_steel" = "Iron & Steel","ceramics" = "Ceramics","glass" = "Glass",
    "paper" = "Paper", "pulp"= "Pulp"))

fig4
ggsave(file.path(trend, "allowances.png"), plot = fig4, width = 8, height = 5, dpi = 300)


# ==============================================================================
# ELECTRICITY SHARE
# ==============================================================================
df_mean <- df %>%
  group_by(year, sector) %>%
  summarise(share_elect = mean(share_elect, na.rm = TRUE), .groups = "drop")

fig5 <- ggplot(data = df_mean, aes(x = year, y = share_elect, group = sector, color = sector)) +
  geom_line(linewidth = 1) +                        
  labs(title="Electricity share (%)",
       x="Year", y="")  +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot")+
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 2013, y = Inf, label = "Phase III",
           vjust = 1.5, hjust = 1.1, size = 3)+
  annotate("text", x = 2019, y = Inf, label = "Price rise",
           vjust = 1.5, hjust = 1.1, size = 3)+
  scale_color_viridis_d(option = "D", labels = c(
    "iron_steel" = "Iron & Steel","ceramics" = "Ceramics","glass" = "Glass",
    "paper" = "Paper", "pulp"= "Pulp"))

fig5
ggsave(file.path(trend, "electricity.png"), plot = fig5, width = 8, height = 5, dpi = 300)



# ==============================================================================
# SURPLUS
# ==============================================================================
df_mean <- df %>%
  group_by(year, sector) %>%
  summarise(surplus = mean(surplus, na.rm = TRUE), .groups = "drop")

fig6 <- ggplot(data = df_mean, aes(x = year, y = surplus, color = sector)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  facet_wrap(~ sector, scales = "free_y", ncol = 3,
             labeller = labeller(sector = c(
               "iron_steel" = "Iron & Steel",
               "ceramics"   = "Ceramics",
               "glass"      = "Glass",
               "paper"      = "Paper",
               "pulp"       = "Pulp"
             ))) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 2013, y = Inf, label = "Phase III",
           vjust = 1.5, hjust = 1.1, size = 3) +
  annotate("text", x = 2019, y = Inf, label = "Price rise",
           vjust = 1.5, hjust = 1.1, size = 3) +
  scale_x_continuous(breaks = seq(2008, 2023, 4)) +
  scale_color_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Surplus by Sector", x = NULL, y = "") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "none",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line          = element_line(color = "black"),
    strip.text         = element_text(face = "bold"),
    plot.title         = element_text(face = "bold", size = 14),
    plot.title.position = "plot"
  )
fig6
ggsave(file.path(trend, "surplus.png"), plot = fig6, width = 10, height = 6, dpi = 300)  

