# ==============================================================================
# This script is used to produce basic descriptive statistics
#
#==============================================================================
library(dplyr); library(tidyr); library(ggplot2); library(gridExtra)
library(purrr);library(patchwork)

raw <- "data/raw"
inter <- "data/intermediate"
proc <- "data/processed"
figs <- "outputs/figures"


df <- read.csv(file.path(proc, "final.csv"))
df$period   <- factor(df$period,   levels = c("Pre 2013", "Post 2013"))
df$period_2 <- factor(df$period_2, levels = c("2013-2019", "2020+"))



#===============================================================================
vars_num <- c("free_all", "ver_emiss", "coverage", "surplus",
              "share_elect", "ec_intensity")
vars_num <- vars_num[vars_num %in% names(df)]

sector_labels <- c(
  "iron_steel" = "Iron & Steel",
  "paper"      = "Paper",
  "pulp"       = "Pulp",
  "ceramics"   = "Ceramics",
  "glass"      = "Glass"
)
var_labels <- c(
  "free_all"     = "Free Allowances",
  "ver_emiss"    = "Verified Emissions",
  "coverage"     = "Coverage Ratio",
  "surplus"      = "Surplus",
  "share_elect"  = "Electricity Share (%)",
  "ec_intensity" = "Energy Intensity",
  "period"       = "Period",
  "period_2"     = "Period"
)

df$sector <- factor(df$sector, levels = names(sector_labels), labels = sector_labels)



#=========================================================================
# Boxplots
#=========================================================================
make_boxplot <- function(data, var, group_x, fill_var = NULL, title_suffix = "") {
  p <- ggplot(data, aes(x = .data[[group_x]], y = .data[[var]])) +
    theme_minimal() +
    labs(y = var_labels[var], x = NULL, fill = "Period",
         title = paste0(var_labels[var], title_suffix)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!is.null(fill_var)) {
    p <- p + geom_boxplot(aes(fill = .data[[fill_var]]), alpha = 0.7,
                          outlier.color = "red", outlier.size = 1.5)
  } else {
    p <- p + geom_boxplot(aes(fill = .data[[group_x]]), alpha = 0.7,
                          outlier.color = "red", outlier.size = 1.5) +
      theme(legend.position = "none")
  }
  return(p)
}

# Settore — tutti gli anni
plots_sector <- map(vars_num, ~ make_boxplot(df, .x, "sector"))
p_sector <- wrap_plots(plots_sector, ncol = 3)
print(p_sector)
ggsave(file.path(figs, "box_sector.png"), p_sector, width = 12, height = 8, dpi = 300)


# Settore × pre/post 2013
plots_sp_2013 <- map(vars_num, ~ make_boxplot(df, .x, "sector", fill_var = "period"))
p_sp_2013 <- wrap_plots(plots_sp_2013, ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "")
print(p_sp_2013)
ggsave(file.path(figs, "box_sector_2013.png"), p_sp_2013, width = 12, height = 8, dpi = 300)



# Settore × 2013-2019 vs 2020+
plots_sp_2020 <- map(vars_num, ~ make_boxplot(df %>% dplyr::filter(!is.na(period_2)),
                                              .x, "sector", fill_var = "period_2"))
p_sp_2020 <- wrap_plots(plots_sp_2020, ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "")
print(p_sp_2020)
ggsave(file.path(figs, "box_sector_2020.png"), p_sp_2020, width = 12, height = 8, dpi = 300)





#=========================================================================
# Trend
#=========================================================================
vars_trend <- c("eua_price", "ratio_price","ver_emiss", "coverage",
                "surplus", "share_elect", "ec_intensity", "output_index_2008")
vars_trend <- vars_trend[vars_trend %in% names(df)]

var_labels_trend <- c(
  "eua_price"          = "EUA Price (€)",
  "ratio_price"        = "Electricity/Gas Price Ratio",
  "ver_emiss"          = "Verified Emissions",
  "coverage"           = "Coverage Ratio",
  "surplus"            = "Surplus",
  "share_elect"        = "Electricity Share (%)",
  "ec_intensity"       = "Energy Intensity",
  "output_index_2008"  = "Output Index (2008 = 1)"
)

df_sector_avg <- df %>%
  group_by(year, sector) %>%
  summarise(across(all_of(vars_trend), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

for (v in vars_trend) {
  p <- ggplot(df_sector_avg, aes(x = year, y = .data[[v]], color = sector)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2013, linetype = "dashed", color = "red", alpha = 0.6) +
    annotate("text", x = 2013.3, y = Inf, label = "Benchmark", vjust = 1.5,
             color = "red", size = 3, fontface = "italic") +
    scale_x_continuous(breaks = seq(2008, 2023, 2)) +
    scale_color_discrete(labels = sector_labels) +
    labs(x = NULL, y = var_labels_trend[v], color = "Sector") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
  ggsave(paste0(file.path(figs, "trend_"), v, ".png"), p, width = 8, height = 5, dpi = 300)
}



