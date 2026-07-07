# ==============================================================================
# Questo script estrae i dati delle EUA
# ==============================================================================
library(dplyr)
library(lubridate)

eua <- read.csv("data/raw/eua.csv")

# Seleziono le prime due colonne (data e prezzo)
eua <- eua[, 1:2]

# rename columns
colnames(eua) <- c("date", "eua_price")

eua <- eua %>%
  mutate(
    date = dmy(date),        # interpreta correttamente "05/01/2005"
    year = year(date)
  ) %>%
  group_by(year) %>%
  summarise(eua_price = mean(eua_price, na.rm = TRUE)) %>%
  ungroup()

write.csv(eua, "data/processed/eua.csv", row.names = F)
