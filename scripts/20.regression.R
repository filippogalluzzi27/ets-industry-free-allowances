# ==============================================================================
# This script produces tables with regression results
#
#==============================================================================
library(dplyr); library(fixest)

proc <- "data/processed"
df <- read.csv(file.path(proc, "final.csv"))
regre <- "output/regression_tabs"

#==============================================================================
# Creation of log variables, dummies, interactions
df <- df %>%
  mutate(
    log_emiss = log(ver_emiss),
    log_free  = log(free_all),
    D2013     = as.integer(year >= 2013),
    D2020     = as.integer(year >= 2020),
    csid      = interaction(country, sector, drop = TRUE)
  )





# ==============================================================================
# TABLE 1a : 2008-2023, dummy benchmark D2013; FE sectors only
# ==============================================================================
dfA <- df

a1 <- feols(log_emiss ~ eua_price | sector, dfA)
a2 <- feols(log_emiss ~ eua_price + log_free | sector, dfA)
a3 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 | sector, dfA)
a4 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect | sector, dfA)
a5 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect + D2013 | sector, dfA)

etable(a1, a2, a3, a4, a5, cluster = ~ csid,
       title = "Tab.1a - FE sectors | cluster country-sector", tex = TRUE, file = file.path(regre, "tab_1a.tex"))


# ==============================================================================
# TABLE 1b : 2008-2023, dummy benchmark D2013; FE sectors-country
# ==============================================================================
au1 <- feols(log_emiss ~ eua_price | country^sector, dfA)
au2 <- feols(log_emiss ~ eua_price + log_free | country^sector, dfA)
au3 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 | country^sector, dfA)
au4 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect | country^sector, dfA)
au5 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect + D2013 | country^sector, dfA)

etable(au1, au2, au3, au4, au5, cluster = ~ csid,
       title = "Tab.1b - FE country-sector | cluster country-sector", tex = TRUE, file = file.path(regre, "tab_1b.tex"))

# ==============================================================================
# TABLE 1c : Test for slope change post-2013; interaction log_free-D2013FE sectors-country
# ==============================================================================
aint_s <- feols(log_emiss ~ eua_price + log_free*D2013 + output_index_2008 + share_elect | sector,         dfA)
aint_u <- feols(log_emiss ~ eua_price + log_free*D2013 + output_index_2008 + share_elect | country^sector, dfA)

etable(aint_s, aint_u, cluster = ~ csid,
       title = "Tab.1c - Interaction log_free-D2013", tex = TRUE, file = file.path(regre, "tab_1c.tex"))



# ==============================================================================
# TABLE 2a : 2013-2023, dummy 2020; FE sectors only
# ==============================================================================
dfB <- df %>% filter(year >= 2013)

b1 <- feols(log_emiss ~ eua_price | sector, dfB)
b2 <- feols(log_emiss ~ eua_price + log_free | sector, dfB)
b3 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 | sector, dfB)
b4 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect | sector, dfB)
b5 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect + D2020 | sector, dfB)

etable(b1, b2, b3, b4, b5, cluster = ~ csid,
       title = "Tab.2a - FE sectors | cluster country-sector", tex = TRUE, file = file.path(regre, "tab_2a.tex"))

# ==============================================================================
# TABLE 2b : 2013-2023, dummy 2020; FE sectors-country
# ==============================================================================
bu1 <- feols(log_emiss ~ eua_price | country^sector, dfB)
bu2 <- feols(log_emiss ~ eua_price + log_free | country^sector, dfB)
bu3 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 | country^sector, dfB)
bu4 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect | country^sector, dfB)
bu5 <- feols(log_emiss ~ eua_price + log_free + output_index_2008 + share_elect + D2020 | country^sector, dfB)

etable(bu1, bu2, bu3, bu4, bu5, cluster = ~ csid,
       title = "Tab.2b - FE country-sector | cluster country-sector", tex = TRUE, file = file.path(regre, "tab_2b.tex"))


# ==============================================================================
# TABLE 2c : 2013-2023, dummy 2020; interaction log_free-D2020
# ==============================================================================
bint_u <- feols(log_emiss ~ eua_price + log_free*D2020 + output_index_2008 + share_elect | country^sector, dfB)
etable(bint_u, cluster = ~ csid,
       title = "Tab.2c - Interaction log_free-D2020", tex = TRUE, file = file.path(regre, "tab_2c.tex"))







