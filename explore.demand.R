library(tidyverse)
library(here)
library(haven)
library(readxl)


## Global Findex

findex <- read_xlsx("data/global_findex.xlsx", sheet = "Data",
                    na = "")

# Rename first columns
findex <- rename(findex, year = "X__1", cntry.code = "X__2", country = "X__3",
                 region = "X__4", inc.grp = "X__5")

mobileaccount.t.d.7
mobileaccount.t.d.8

findex %>%
  group_by(inc.grp)