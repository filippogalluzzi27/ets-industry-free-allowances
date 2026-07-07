# This repository contains the scripts for the analysis on EU ETS and energy-intensive industry for the Master thesis.



## 📁 Repository structure

```
.
├── scripts/
│   ├── 01.free_allowances.R
│   ├── 02.data-energy-consumption.R
│   ├── 03.eua-price.R
│   ├── 04.elect-gas-price.R
│   ├── 05.data-merge.R
│   ├── 10.stats.R
│   ├── 11.trend.R
│   ├── 12.descriptive.R
│   ├── 13.grafici_temporali_settori.R
│   └── 20.regression.R
├── data/
│   ├── raw
│       ├── elect_price.csv
│       ├── eua.csv
│       ├── gas_price.csv
│       ├── free_all_ver_emiss       # folder that contains .csv for each country
│       └── jrc-idees-2023-industry  # folder that contains .xlsx for each country
│   ├── processed
│       └── final.csv
│   └── intermediate/
│       ├──elect-gas-price.csv
│       ├── ener-consumpt.csv
│       ├── eua.csv
│       └── fa-ve.csv
├── outputs/                     
│       ├── figures           # boxplots and single variable trends
│       ├── regression_tabs   # results from regression
│       ├── sectors           # trends for specific sectors
│       ├── stats             # descriptive statistics
│       └── trends            # variable trends
└── README.md
```
