library(tidyverse)
library(here)
library(haven)
library(readxl)
library(sf)
library(tmap)
library(readr)


### Date:
  # 1. Global Findex
  # 2. IMF Financial Access Survey
  # 3. WB region and income classification
  # 4. Financial Access Survey '16, Kenya
  # 5. Geospatial data

# Make any significant changes and tidying here.


############################# Import + Clean Data #############################
## 1. Global Findex
findex <- read_xlsx("data/global_findex.xlsx", sheet = "Data",
                    na = "")

  # Rename first columns
  findex <- rename(findex, year = "X__1", cntry.code = "X__2", country = "X__3",
                 region = "X__4", inc.grp = "X__5")
  
  # Check region variables
  table(findex$region)
  findex$country[findex$region=="i"]
  findex$region[findex$country=="Afghanistan"]
  findex$region[findex$region=="i"] <- "South Asia"
  

## 2. IMF - Financial Access Survey
imf <- read_dta("data/imf_fas.dta")
head(imf$iso3)
imf <- rename(imf, cntry.code = "iso3")


## 3. Region and income classification variables
class <- read_xls("data/wb_class.xls", sheet = "List of economies")
head(class)
class <- class %>%
  select(X__2, X__3, X__5, X__6)

  # Make first row column names
  class <- class[-c(1:3), ]
  class <- class[-2, ]
  colnames(class) = class[1,]
  class = class[-1,]
  head(class)

  class <- rename(class, inc.grp = "Income group", country = "Economy", cntry.code = "Code")

  # Drop last 56 rows
  tail(class, n = 60)
  class <- head(class, -56)
  tail(class)

  # Merge IMF + income group and region data
  imf <- left_join(imf, class, by = "cntry.code")
  
  # Check for NAs
  imf %>%
    filter(is.na(inc.grp)) %>%
    group_by(economy) %>%
    summarize(n = n_distinct(economy))
  
  # Change income group for Kosovo
  imf$inc.grp[imf$economy=="Kosovo, Republic of"] <- "Lower middle income"
  
  # Anguilla and Montserrat are still NA, so remove from data
  imf <- imf %>%
    filter(!is.na(inc.grp))
  
  imf$Region <- as.factor(imf$Region)
  class(imf$Region)
  imf$Region <- relevel(imf$Region, "Sub-Saharan Africa")  

  
## 4. Financial Access Survey '16, Kenya
finac.k.16 <- read_dta("data/fin_access16_kenya.dta")

  # Clean variable names
  #finac.k <- rename(finac.k, sex.res = "gender_of_respondent", rural = "cluster_type")
  
  # Make factors
  #finac.k$rural <- factor(finac.k$rural, labels = c("rural", "urban"))
  #finac.k$rural
  
  # 1 = rural, 2 = urban
  #finac.k$area <- 0
  #finac.k$area[finac.k$rural==2] <- 1
  # Area: 0 = rural, 1 = urban
  
  # Create dummy variables
  finac.k.16$e4_1[finac.k.16$e4_1==2] <- 0
  finac.k.16$e4_3[finac.k.16$e4_3==2] <- 0
  finac.k.16$e4_4[finac.k.16$e4_4==2] <- 0
  finac.k.16$e4_5[finac.k.16$e4_5==2] <- 0
  finac.k.16$e4_7[finac.k.16$e4_7==2] <- 0
  finac.k.16$e4_9[finac.k.16$e4_9==2] <- 0
  
  finac.k.16$e4_1 <- as.numeric(finac.k.16$e4_1)


## 5. Geospatial data
kenya <- read_sf(dsn="data/kenya_shapefiles", layer = "ken_admbndl_admALL_iebc_itos_20180607")

  # Initial plot and check
  plot(kenya)
  st_crs(kenya)
  class(kenya)
  head(kenya)
  
  kenya <- select(kenya, -(validON:validTo))
  

############################# Export Data #############################
write_csv(findex, path = "data/clean/clean.findex.csv")
write_csv(imf, path = "data/clean/clean.imf.csv")
write_csv(finac.k.16, path = "data/clean/clean.finac.k.16.csv")
