# ==============================================================================
# This script is used to manage verified emissions and free allowances 
# for the five countries and sectors selected.
#
# Data source: EU Emissions Trading System (ETS) data viewer
# 
# Sectors:
# Production of pig iron or steel, Production of pulp, Production of paper or 
# cardboard, Manufacture of glass, Manufacture of ceramics
#
# Countries:
# Italy, Germany, Poland, Spain, France
#
# ==============================================================================
library(dplyr); library(tidyr); library(readxl); library(purrr); library(stringr)

raw <- "data/raw"
proc <- "data/processed"
data_path <- file.path(raw, "free_all_ver_emiss")



# ==============================================================================
# Sectors and countries configuration
# ==============================================================================
COUNTRIES <- c("ita", "fra", "ger", "spa", "pol")

# Map sector-code
SECTOR_MAP <- c(
  "24" = "iron_steel",
  "36" = "paper",
  "35" = "pulp",
  "31" = "glass",
  "32" = "ceramics"
)


#==============================================================================
# Definition of function for extraction of selected values
#==============================================================================
process_country <- function(country_code) {
  # Path for each country data; only read the header for each file
  path <- file.path(data_path, paste0(country_code, ".xlsx"))
  raw_data  <- read_excel(path, col_names = FALSE)
  header  <- as.character(raw_data[1, ])
  sectors <- as.character(raw_data[2, ])
  
  # Selection fo row where the section "verified emissions" begins
  ve_start <- which(str_detect(header, "2\\. Verified"))[1]
  
  # Years
  year_col <- as.integer(raw_data[3:nrow(raw_data), 2, drop = TRUE])
  
  # Selection of free allowances and verified emissions
  fa_data <- raw_data[3:nrow(raw_data), 3:(ve_start - 1), drop = FALSE]
  ve_data <- raw_data[3:nrow(raw_data), ve_start:ncol(raw_data), drop = FALSE]
  
  # Rename columns with the appropriate names
  colnames(fa_data) <- sectors[3:(ve_start - 1)]
  colnames(ve_data) <- sectors[ve_start:ncol(raw_data)]

  # ???  
  fa_data$year <- year_col
  ve_data$year <- year_col
  
  # Wide -> Long transformation
  fa_long <- fa_data %>%
    pivot_longer(-year, names_to = "sector_label", values_to = "free_all") %>%
    mutate(sector = str_extract(sector_label, "^\\d{2}")) %>%
    select(year, sector, free_all)
  ve_long <- ve_data %>%
    pivot_longer(-year, names_to = "sector_label", values_to = "ver_emiss") %>%
    mutate(sector = str_extract(sector_label, "^\\d{2}")) %>%
    select(year, sector, ver_emiss)

  # Join of the two variables by year-sector  
  full_join(fa_long, ve_long, by = c("year", "sector")) %>%
    mutate(
      country   = country_code,
      free_all  = as.numeric(str_remove_all(free_all,  "\\s")), # Removed spaces between numbers
      ver_emiss = as.numeric(str_remove_all(ver_emiss, "\\s"))    
    ) %>%
    select(year, country, sector, free_all, ver_emiss)
}



# ==============================================================================
# Execution of function
# ==============================================================================
df_final <- map_dfr(COUNTRIES, process_country)


df_final <- df_final %>%
  mutate(sector = SECTOR_MAP[sector]) %>%
  filter(!is.na(sector)) %>%
  arrange(country, sector, year)

write.csv(df_final, file.path(PROCESSED, "fa-ve.csv"), row.names = FALSE)