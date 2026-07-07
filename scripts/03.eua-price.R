# ==============================================================================
# This script is used to extract European Union Allowances prices
#
# Data source: 
#
# ==============================================================================
library(dplyr); library(lubridate)

raw <- "data/raw"
inter <- "data/intermediate"

# ==============================================================================
eua <- read.csv(file.path(raw, "eua.csv"))


# Selection of the first two columns (date and price)
eua <- eua[, 1:2]

# rename columns
colnames(eua) <- c("date", "eua_price")

eua <- eua %>%
  mutate(
    date = dmy(date),        
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(eua_price = mean(eua_price, na.rm = TRUE)) %>%
  ungroup()

write.csv(eua, file.path(inter, "eua.csv"), row.names = F)
