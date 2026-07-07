# ==============================================================================
# This script is used to merge the dataset
# The output is the final dataset used for the analysis
#
# The variables are:
# 
# ==============================================================================
library(dplyr)

raw <- "data/raw"
inter <- "data/intermediate"
proc <- "data/processed"



#==============================================================================
# Data upload (years>2007)
#==============================================================================
ec <- read.csv(file.path(inter, "ener-consumpt.csv"))
elect_gas  <- read.csv(file.path(inter, "elect-gas-price.csv"))
eua  <- read.csv(file.path(inter, "eua.csv"))
fa_ve <- read.csv(file.path(inter, "fa-ve.csv"))

elect_gas <- filter(elect_gas, year>2007)
df <- ec %>%
  inner_join(elect_gas, by = c("country", "year")) %>%
  inner_join(eua, by = "year") %>%
  inner_join(fa_ve, by = c("country", "year", "sector")) %>%
  distinct()


#==============================================================================
# Check for duplicates and others
#==============================================================================
duplicates <- df[duplicated(df[, c("country", "sector", "year")]), ]
nrow(duplicates)
table(df$year)
table(df$country) # Poland has no paper sector
aggregate(ver_emiss ~ year, data = df, sum)
colSums(is.na(df))



#==============================================================================
# New variables
#==============================================================================
df <- df %>%
  mutate(
    coverage    = free_all / ver_emiss,
    surplus     = free_all - ver_emiss,
    ratio_price = elect_price / gas_price,
    incentive   = surplus * eua_price,
    period = factor(
      ifelse(year <= 2012, "Pre 2013", "Post 2013"),
      levels = c("Pre 2013", "Post 2013")
    ),
    period_2 = factor(
      case_when(
        year <  2013               ~ NA_character_,
        year >= 2013 & year < 2020 ~ "2013-2019",
        year >= 2020               ~ "2020+"
      ),
      levels = c("2013-2019", "2020+")
    )
  )


# 2008 output index
base_2008 <- df %>%
  filter(year == 2008) %>%
  select(sector, country, output) %>%
  rename(output_base_2008 = output)
df <- df %>%
  left_join(base_2008, by = c("sector", "country")) %>%
  mutate(
    output_index_2008 = output / output_base_2008,
  ) %>%
  select(-output_base_2008)



#==============================================================================
# Reordered columns
#==============================================================================
df <- df %>%
  select(-share_steam, -steam_intensity, elect_price, gas_price,
         -ec_total, -elect_intensity)

write.csv(df, file.path(proc, "final.csv"), row.names = FALSE)






