# ==============================================================================
# This script is used to extract gas and electricity prices for each country
# 
# Data source: Eurostat (nrg_pc_205)
#
# ==============================================================================
library(dplyr); library(lubridate); library(stringr)

raw <- "data/raw"
inter <- "data/intermediate"


# ==============================================================================
# Configuration
# ==============================================================================
# mapping country names (it works both for geo codes ("IT"/"DE"/...) and for extended names
names_map <- c(
  "FR" = "fra", "France"  = "fra",
  "IT" = "ita", "Italy"   = "ita",
  "PL" = "pol", "Poland"  = "pol",
  "DE" = "ger", "Germany" = "ger",
  "ES" = "spa", "Spain"   = "spa"
)

TAX_CODE <- "X_VAT"

# Industrial consumption for non-households consumers
BAND_ELECT <- "MWH20000-69999"   # Electricity, band IE (20 000-69 999 MWh)
BAND_GAS   <- "GJ100000-999999"  # Gas, band I4 (100 000-999 999 GJ)



# ==============================================================================
# Electricity
# ==============================================================================
elect <- read.csv(file.path(raw, "elect_price.csv"))

# FIlter for consumption band and taxes
elect <- elect %>%
  filter(nrg_cons == BAND_ELECT, tax == TAX_CODE)

# Rename of variables, annual mean for country
elect <- elect %>%
  mutate(
    country   = recode(geo, !!!names_map),
    year      = as.integer(str_sub(TIME_PERIOD, 1, 4)),
    OBS_VALUE = suppressWarnings(as.numeric(OBS_VALUE))
  ) %>%
  filter(country %in% c("fra", "ita", "pol", "ger", "spa")) %>%
  group_by(country, year) %>%
  summarise(elect_price = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")



# ==============================================================================
# Natural gas
# ==============================================================================
gas <- read.csv(file.path(raw, "gas_price.csv"))
gas <- gas %>%
  filter(nrg_cons == BAND_GAS, tax == TAX_CODE)
gas <- gas %>%
  mutate(
    country   = recode(geo, !!!names_map),
    year      = as.integer(str_sub(TIME_PERIOD, 1, 4)),
    OBS_VALUE = suppressWarnings(as.numeric(OBS_VALUE))
  ) %>%
  filter(country %in% c("fra", "ita", "pol", "ger", "spa")) %>%
  group_by(country, year) %>%
  summarise(gas_price = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")


# ==============================================================================
# Merge and save
# ==============================================================================
df <- elect %>%
  left_join(gas, by = c("country", "year"))

write.csv(df, "data/processed/elect-gas-price.csv", row.names = F)