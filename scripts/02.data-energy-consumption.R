# ==============================================================================
# This script is used to extract energy consumption and output data
# For the selected countries and sectors
# 
# Each country has a different file, with different rows corresponding to the
# same variable; therefore, it has been encessary to map a different string of numbers
# for each country. A manual selection for the different rows for each country-sector
# has been implemented
#
# Data source: JRC-IDEES 2023
# 
# Sectors:
# Production of pig iron or steel, Production of pulp, Production of paper or 
# cardboard, Manufacture of glass, Manufacture of ceramics
#
# Countries:
# Italy, Germany, Poland, Spain, France
#
# Variables: 
# physical output (kt), total energy consumption, electricity consumption
#
# ==============================================================================
library(dplyr); library(tidyr); library(readxl); library(purrr)

raw <- "data/raw"
inter <- "data/intermediate"
data_path <- file.path(raw, "jrc-idees-2023-industry")



# ==============================================================================
# Configuration for countries and years
# ==============================================================================
COUNTRIES <- c("IT", "DE", "ES", "FR", "PL")

COUNTRY_MAP <- c(
  "IT" = "ita", "DE" = "ger", "FR" = "fra",
  "PL" = "pol", "ES" = "spa"
)

YEARS <- as.character(2005:2023)



# ==============================================================================
# Definition of the extraction functions; the function gets as an input a vector of 
# row indices that correspond to the selected variables; each country has a different
# set of row indices, specified in each section. The +1 in sum_rows offsets the 
# header row
# ==============================================================================
to_numeric_safe <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))

sum_rows <- function(df, row_indices, year_cols) {
  colSums(sapply(df[row_indices + 1, year_cols], to_numeric_safe), na.rm = TRUE)
}



# ==============================================================================
# IRON & STEEL  (sheet: ISI)
# row 5  = physical output (kt steel)
# row 23 = total energy consumption
# row 43 = distributed steam
# row 44 = electricity
# ==============================================================================
process_isi <- function(country) {
  # path for the corresponding sector
  file        <- file.path(data_path, paste0("JRC-IDEES-2023_Industry_", country, ".xlsx"))
  # exact name for the sector in the original file
  country_col <- paste0(country, ": Iron and steel")
  # reads the ISI sheet
  df          <- read_excel(file, sheet = "ISI", col_names = TRUE)
  year_cols   <- intersect(YEARS, names(df))
  
  # selection of the corresponding row (+1 offset for header) and rename
  sel    <- df[c(5, 23, 43, 44) + 1, c(country_col, year_cols)]
  labels <- c("output", "ec_total", "distr_steam", "elect")    
  
  # selection of variables of interest and reshape dataset
  sel %>%
    mutate(var = labels) %>%
    select(-all_of(country_col)) %>%
    pivot_longer(-var, names_to = "year", values_to = "value") %>%
    pivot_wider(names_from = var, values_from = value) %>%
    mutate(across(-year, to_numeric_safe),
           country = COUNTRY_MAP[country], sector = "iron_steel",
           year = as.integer(year))
}


# ==============================================================================
# PAPER  (sheet: PPA_fec + PPA per output)
# row 30     = total energy consumption
# rows electricity = [31,32,33,34,40,53,66,79]  (sum)
# rows steam = [52,65,78]                 (sum)
# row 9 (PPA) = physical output
# ==============================================================================
process_pap <- function(country) {
  file        <- file.path(data_path, paste0("JRC-IDEES-2023_Industry_", country, ".xlsx"))
  df          <- read_excel(file, sheet = "PPA_fec", col_names = TRUE)
  year_cols   <- intersect(YEARS, names(df))
  
  tibble(
    year        = as.integer(year_cols),
    ec_total    = to_numeric_safe(unlist(df[30 + 1, year_cols])),
    elect       = sum_rows(df, c(31, 32, 33, 34, 40, 53, 66, 79), year_cols),
    distr_steam = sum_rows(df, c(52, 65, 78), year_cols),
    output      = to_numeric_safe(unlist(
      read_excel(file, sheet = "PPA", col_names = TRUE)[9 + 1, year_cols]
    )),
    country = COUNTRY_MAP[country], sector = "paper"
  )
}



# ==============================================================================
# PULP  (sheet: PPA_fec + PPA per output)
# row 3      = total energy consumption
# rows electricity = [4, 5,6,7,13,14,27,28]  (sum)
# row 26     = distributed steam
# row 8 (PPA)= physical output
# ==============================================================================
process_plp <- function(country) {
  file        <- file.path(data_path, paste0("JRC-IDEES-2023_Industry_", country, ".xlsx"))
  df          <- read_excel(file, sheet = "PPA_fec", col_names = TRUE)
  year_cols   <- intersect(YEARS, names(df))
  
  tibble(
    year        = as.integer(year_cols),
    ec_total    = to_numeric_safe(unlist(df[3 + 1, year_cols])),
    elect       = sum_rows(df, c(4, 5, 6, 7, 13, 14, 27, 28), year_cols),
    distr_steam = to_numeric_safe(unlist(df[26 + 1, year_cols])),
    output      = to_numeric_safe(unlist(
      read_excel(file, sheet = "PPA", col_names = TRUE)[8 + 1, year_cols]
    )),
    country = COUNTRY_MAP[country], sector = "pulp"
  )
}



# ==============================================================================
# GLASS  (sheet: NMM_fec + NMM for output)
# row 97      = total energy consumption
# rows electricity  = [98,99,100,101,107,115,116,124,125]  (sum)
# row 9 (NMM) = physical output
# ==============================================================================
process_glass <- function(country) {
  file      <- file.path(data_path, paste0("JRC-IDEES-2023_Industry_", country, ".xlsx"))
  df        <- read_excel(file, sheet = "NMM_fec", col_names = TRUE)
  year_cols <- intersect(YEARS, names(df))
  
  tibble(
    year     = as.integer(year_cols),
    ec_total = to_numeric_safe(unlist(df[97 + 1, year_cols])),
    elect    = sum_rows(df, c(98, 99, 100, 101, 107, 115, 116, 124, 125), year_cols),
    output   = to_numeric_safe(unlist(
      read_excel(file, sheet = "NMM", col_names = TRUE)[9 + 1, year_cols]
    )),
    country = COUNTRY_MAP[country], sector = "glass"
  )
}

# ==============================================================================
# CERAMICS  (sheet: NMM_fec + NMM per output)
# row 46      = total energy consumption
# rows electricity  = [47,48,49,50,56,57,76,86,94]  (sum)
# row 8 (NMM) = physical output
# ==============================================================================
process_ceramics <- function(country) {
  file      <- file.path(data_path, paste0("JRC-IDEES-2023_Industry_", country, ".xlsx"))
  df        <- read_excel(file, sheet = "NMM_fec", col_names = TRUE)
  year_cols <- intersect(YEARS, names(df))
  
  tibble(
    year     = as.integer(year_cols),
    ec_total = to_numeric_safe(unlist(df[46 + 1, year_cols])),
    elect    = sum_rows(df, c(47, 48, 49, 50, 56, 57, 76, 86, 94), year_cols),
    output   = to_numeric_safe(unlist(
      read_excel(file, sheet = "NMM", col_names = TRUE)[8 + 1, year_cols]
    )),
    country = COUNTRY_MAP[country], sector = "ceramics"
  )
}




# ==============================================================================
# Execution and merge for df_final with only selected variables
#
# New variables:
#     energy consumption intensity = ec_total/output
#     electricity intensity = electricity/output
#     steam intensity = steam /output
#     share electricity = electricity/total energy consumption
#     share steam = steam/total energy consumption
# ==============================================================================
run_all <- function(fn) map_dfr(COUNTRIES, fn)

df_isi      <- run_all(process_isi)
df_pap      <- run_all(process_pap)
df_plp      <- run_all(process_plp)
df_glass    <- run_all(process_glass)
df_ceramics <- run_all(process_ceramics)

df_final <- bind_rows(df_isi, df_pap, df_plp, df_glass, df_ceramics) %>%
  replace(is.na(.), 0) %>%
  mutate(
    ec_intensity    = ifelse(output   == 0, 0, ec_total    / output),
    elect_intensity = ifelse(output   == 0, 0, elect       / output),
    steam_intensity = ifelse(output   == 0, 0,
                             ifelse("distr_steam" %in% names(.), distr_steam / output, 0)),
    share_elect     = ifelse(ec_total == 0, 0, elect       / ec_total * 100),
    share_steam = ifelse(ec_total == 0, 0,
                         ifelse("distr_steam" %in% names(.), distr_steam / ec_total * 100, 0))) %>%
  select(year, country, sector, output, ec_total,
         share_elect, share_steam,
         ec_intensity, elect_intensity, steam_intensity)


write.csv(df_final, file.path(inter, "ener-consumpt.csv"), row.names = FALSE)
