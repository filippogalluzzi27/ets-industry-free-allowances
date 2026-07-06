# ==============================================================================
# Questo script serve per shape i dati delle free allowances e delle verified
# emissions dei paesi e dei settori selezionati
# ==============================================================================
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)



# ==============================================================================
# CONFIG
# ==============================================================================
RAW       <- file.path("data", "raw", "free_all_ver_emiss")
PROCESSED <- file.path("data", "processed")

COUNTRIES <- c("ita", "fra", "ger", "spa", "pol")

# Codici dei settori del dataset
SECTOR_MAP <- c(
  "24" = "iron_steel",
  "36" = "paper",
  "35" = "pulp",
  "31" = "glass",
  "32" = "ceramics"
)


# ==============================================================================
# FUNCTION
# ==============================================================================
process_country <- function(country_code) {
  path <- file.path(RAW, paste0(country_code, ".xlsx"))
  raw  <- read_excel(path, col_names = FALSE)
  header  <- as.character(raw[1, ])
  sectors <- as.character(raw[2, ])
  
  # Selezionare la riga dove inizia la sezione verified emissions
  ve_start <- which(str_detect(header, "2\\. Verified"))[1]
  
  # Anni dalla colonna 2
  year_col <- as.integer(raw[3:nrow(raw), 2, drop = TRUE])
  
  # Selezione dei dati delle free allowances e delle verified emissions
  fa_data <- raw[3:nrow(raw), 3:(ve_start - 1), drop = FALSE]
  ve_data <- raw[3:nrow(raw), ve_start:ncol(raw), drop = FALSE]
  
  colnames(fa_data) <- sectors[3:(ve_start - 1)]
  colnames(ve_data) <- sectors[ve_start:ncol(raw)]
  
  fa_data$year <- year_col
  ve_data$year <- year_col
  
  # Trasformare da wide a long
  fa_long <- fa_data %>%
    pivot_longer(-year, names_to = "sector_label", values_to = "free_all") %>%
    mutate(sector = str_extract(sector_label, "^\\d{2}")) %>%
    select(year, sector, free_all)
  
  ve_long <- ve_data %>%
    pivot_longer(-year, names_to = "sector_label", values_to = "ver_emiss") %>%
    mutate(sector = str_extract(sector_label, "^\\d{2}")) %>%
    select(year, sector, ver_emiss)
  
  full_join(fa_long, ve_long, by = c("year", "sector")) %>%
    mutate(
      country   = country_code,
      free_all  = as.numeric(str_remove_all(free_all,  "\\s")), # rimuossi gli spazi tra i numeri
      ver_emiss = as.numeric(str_remove_all(ver_emiss, "\\s"))    
    ) %>%
    select(year, country, sector, free_all, ver_emiss)
}


# ==============================================================================
# EXECUTION
# ==============================================================================
df_final <- map_dfr(COUNTRIES, process_country)

df_final <- df_final %>%
  mutate(sector = SECTOR_MAP[sector]) %>%
  filter(!is.na(sector)) %>%
  arrange(country, sector, year)

write.csv(df_final, file.path(PROCESSED, "fa-ve.csv"), row.names = FALSE)