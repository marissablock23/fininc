library(tidyverse)
library(here)
library(haven)
library(readxl)
library(sf)


### Data:
# 1. Global Findex
# 2. IMF Financial Access Survey
# 3. WB region and income classification
# 4. Financial Access Survey '15, Kenya
# 5. Financial Access Survey '13, Kenya
# 6. Financial Access Survey '06, Kenya
# 7. Geospatial data
# 8. WDI Data (2013-2017)
# 9. Population Data

# Make any significant changes and tidying here.


############################# Import + Clean Data #############################
## 1. Global Findex
findex <- read_xlsx("../fininc-data/global_findex.xlsx", sheet = "Data",
                    na = "")

# Rename first columns
findex <- rename(findex, year = "..1", cntry.code = "..2", country = "..3",
                 region = "..4", inc.grp = "..5")

  # Check region variables
  table(findex$region)
  findex$country[findex$region=="i"]
  findex$region[findex$country=="Afghanistan"]
  findex$region[findex$region=="i"] <- "South Asia"
  

## 2. IMF - Financial Access Survey
imf <- read_dta("../fininc-data/imf_fas.dta")
head(imf$iso3)
imf <- rename(imf, cntry.code = "iso3")


## 3. Region and income classification variables
class <- read_xls("../fininc-data/wb_class.xls", sheet = "List of economies")
head(class)

  # Make first row column names
  class <- class %>%
    select("..3", "..4", "..6", "..7")
    
  class <- class[-c(1:3), ]
  class <- class[-2, ]
  colnames(class) = class[1,]
  class <- class[-1,]
  class <- class %>%
    rename(inc.grp = "Income group", country = "Economy", cntry.code = "Code")

  
  # Drop last 56 rows
  tail(class, n = 60)
  class <- head(class, -56)
  tail(class)
  
  # Change country code for Kosovo
  imf$cntry.code[imf$economy=="Kosovo, Republic of"] <- "XKX"

  # Merge IMF + income group and region data
  imf <- left_join(imf, class, by = "cntry.code")
  
  # Check for NAs
  imf %>%
    filter(is.na(inc.grp)) %>%
    group_by(economy) %>%
    summarize(n = n_distinct(economy))

  # Anguilla and Montserrat are still NA, so remove from data
  imf <- imf %>%
    filter(!is.na(inc.grp))
  
  imf$Region <- as.factor(imf$Region)
  class(imf$Region)
  imf$Region <- relevel(imf$Region, "Sub-Saharan Africa")  


## 4. Financial Access Survey '15, Kenya
finac.k.15 <- read_dta("../fininc-data/fin_access16_kenya.dta")

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
  finac.k.15$e4_1[finac.k.15$e4_1==2] <- 0
  finac.k.15$e4_3[finac.k.15$e4_3==2] <- 0
  finac.k.15$e4_4[finac.k.15$e4_4==2] <- 0
  finac.k.15$e4_5[finac.k.15$e4_5==2] <- 0
  finac.k.15$e4_7[finac.k.15$e4_7==2] <- 0
  finac.k.15$e4_9[finac.k.15$e4_9==2] <- 0
  
  finac.k.15$e4_1 <- as.numeric(finac.k.15$e4_1)
  
  # Make counties as factors
  finac.k.15 <- finac.k.15 %>%
      mutate(county = factor(a2, labels = c("Nairobi", "Nyandarua", "Nyeri", "Kirinyaga", "Murang'a", "Kiambu", "Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu",
                                        "Taita Taveta", "Marsabit", "Isiolo", "Meru", "Tharaka-Nithi", "Embu", "Kitui", "Machakos", "Makueni", "Garissa",
                                        "Wajir", "Mandera", "Siaya", "Kisumu", "Migori", "Homa Bay", "Kisii", "Nyamira", "Turkana", "West Pokot", "Samburu",
                                        "Trans Nzoia", "Baringo", "Uasin Gishu", "Elgeyo-Marakwet", "Nandi", "Laikipia", "Nakuru", "Narok", "Kajiado",
                                        "Kericho", "Bomet", "Kakamega", "Vihiga", "Bungoma", "Busia")))
    
  


  
## 5. Financial Access Survey '13, Kenya

finac.k.13 <- read_dta("../fininc-data/FinAccess_Retail_2013_public.dta")


## 6. Financial Access Survey '06, Kenya

finac.k.06 <- read_dta("../fininc-data/FinAccess_Retail_2006_public.dta")


## 7. Geospatial data
kenya <- read_sf(dsn="../fininc-data/ken_admbnda_adm1_iebc_20180607", layer = "ken_admbnda_adm1_iebc_20180607")
  
  # Cut excess variables
  kenya <- kenya %>%
    select(Shape_Leng, Shape_Area, ADM1_EN, geometry) %>%
    rename(county = "ADM1_EN")

  # Project data
  kenya <- st_transform(kenya, 21097)
  

  ## Bank Locations
  bank.loc <- read_xlsx("../fininc-data/banks.xlsx")
  bank.loc <- rename(bank.loc, lat = "GPS Latitude", long = "GPS Longitude", county = "County")
  bank.loc$county <- str_to_title(bank.loc$county)
  
  # Make dummy variable if location is mobile money agent
  bank.loc <- rename(bank.loc, mobilemon = `Does the Outlet Offer Mobile Money Services?`)
  bank.loc$mobilemon[bank.loc$mobilemon=="Yes"] <- 1
  bank.loc$mobilemon[bank.loc$mobilemon=="No"] <- 0
  bank.loc$mobilemon[is.na(bank.loc$mobilemon)] <- 0
  table(bank.loc$mobilemon)
  
  # Remove outliers - maybe data entry issue
  bank.loc <- filter(bank.loc, bank.loc$lat>-5 & bank.loc$lat<5)
  
  # Check county names
  bank.loc %>%
    group_by(county) %>%
    summarize(n = n()) %>%
    full_join(kenya, by = "county") %>%
    print(n = 51)
  
  # Rename counties to match shape file and Fin Access surveys
  bank.loc$county[bank.loc$county=="Marsabet"] <- "Marsabit"
  bank.loc$county[bank.loc$county=="Taita-Taveta"] <- "Taita Taveta"
  bank.loc$county[bank.loc$county=="Muranga"] <- "Murang'a"
  bank.loc$county[bank.loc$county=="Elgeyo Marakwet"] <- "Elgeyo-Marakwet"
  
  # Convert to SF
  banksf <- st_as_sf(bank.loc, coords = c("long", "lat"), crs = 21097)
  class(banksf)
  head(banksf)
  banksf <- banksf %>%
    select(county, geometry)
  
  
  ## MFIs
  mfi.loc <- read_xlsx("../fininc-data/mfi.xlsx")
  mfi.loc <- rename(mfi.loc, lat = "GPS Latitude", long = "GPS Longitude", county = "County")
  mfi.loc$county <- str_to_title(mfi.loc$county)
  
  # Initial plot to check for outliers
  ggplot() +
    geom_point(data = mfi.loc, aes(long, lat))
  
  # Check county names
  mfi.loc %>%
    group_by(county) %>%
    summarize(n = n()) %>%
    full_join(kenya, by = "county") %>%
    print(n = 51)
  
  # Rename counties to match shape file and Fin Access surveys
  mfi.loc$county[mfi.loc$county=="Marsabet"] <- "Marsabit"
  mfi.loc$county[mfi.loc$county=="Taita-Taveta"] <- "Taita Taveta"
  mfi.loc$county[mfi.loc$county=="Muranga"] <- "Murang'a"
  mfi.loc$county[mfi.loc$county=="Elgeyo Marakwet"] <- "Elgeyo-Marakwet"
  
  
  ## MF Banks
  mfib.loc <- read_xlsx("../fininc-data/mfib.xlsx")
  mfib.loc <- rename(mfib.loc, lat = "GPS Latitude", long = "GPS Longitude", county = "County")
  mfib.loc$county <- str_to_title(mfib.loc$county)

  # Initial plot to check for outliers
  ggplot() +
    geom_point(data = mfib.loc, aes(long, lat))
  
  # Remove last rows
  mfib.loc <- mfib.loc %>%
    filter(!is.na(county))
  
  # Check county names
  mfib.loc %>%
    group_by(county) %>%
    summarize(n = n()) %>%
    full_join(kenya, by = "county") %>%
    print(n = 51)
  
  # Rename counties to match shape file and Fin Access surveys
  mfib.loc$county[mfib.loc$county=="Taita-Taveta"] <- "Taita Taveta"
  mfib.loc$county[mfib.loc$county=="Muranga"] <- "Murang'a"
  mfib.loc$county[mfib.loc$county=="Elgeyo Marakwet"] <- "Elgeyo-Marakwet"


  # Map of Africa
  africa <- read_sf(dsn="../fininc-data/countries", layer = "ne_50m_admin_0_countries")
  
  africa <- africa %>%
    filter(CONTINENT == "Africa") %>%
    rename(cntry.code = "ISO_A3")  
  
  

## 8. WDI Data
wdi <- read_csv("../fininc-data/wdi.csv", na = "..",
                col_types = cols(
                  `2013 [YR2013]` = col_number() 
                ))

  # Drop last 5 rows
  wdi <- head(wdi, -5)
  tail(wdi)

  # Rename variables
  wdi <- rename(wdi, country = "Country Name", cntry.code = "Country Code")

  # Reshape year variables to column
  wdi <- wdi %>%
    gather(`2013 [YR2013]`, `2014 [YR2014]`, `2015 [YR2015]`, `2016 [YR2016]`, `2017 [YR2017]`,
           key = year, value = value)

  # Drop Series Name
  wdi <- select(wdi, -`Series Name`)
  
  # Reshape Series Code column to variables
  wdi <- wdi %>%
    spread(key = `Series Code`, value = value)
  
  # Make year variable factor
  wdi$year <- str_sub(wdi$year, 1, 4)
  wdi$year <- as.factor(wdi$year)
  
  # Drop variables with all NAs
  wdi <- wdi[, colSums(is.na(wdi)) != nrow(wdi)]
  
  # Make variables names lowercase
  colnames(wdi) <- tolower(colnames(wdi))
  colnames(wdi)

  # Merge with income classification
  wdi <- left_join(wdi, class, by = "cntry.code")


## Population
  pop <- read_xlsx("../fininc-data/population.xlsx", sheet = "County_Pop_Projection _UNICEF",
                                       na = "")
  head(pop)
  tail(pop)
  
  pop <- pop %>%
    rename(county = "..1", pop_2009 = "Total", pop_2010 = "..5", pop_2011 = "..6", pop_2012 = "..7", pop_2013 = "..8", pop_2014 = "..9",
           pop_2015 = "..10", pop_2016 = "..11", pop_2017 = "..12", pop_2018 = "..13", pop_2019 = "..14") %>%
    select(county, pop_2009:pop_2019) %>%
    filter(county!="Name" & county!="KENYA (national projection)" & !is.na(county) & county!="Total")

  # Check county names
  pop %>%
    select(county) %>%
    full_join(kenya, by = "county") %>%
    print(n = 51)
    
  # Rename counties to match shape file and Fin Access surveys
  pop$county[pop$county=="Taita taveta"] <- "Taita Taveta"
  pop$county[pop$county=="Homabay"] <- "Homa Bay"
  pop$county[pop$county=="Tharaka-nithi"] <- "Tharaka-Nithi"
  pop$county[pop$county=="Muranga"] <- "Murang'a"


############################# Export Data #############################
write_csv(findex, path = "../fininc-data/clean/clean.findex.csv")
write_csv(imf, path = "../fininc-data/clean/clean.imf.csv")
write_csv(finac.k.15, path = "../fininc-data/clean/clean.finac.k.15.csv")
write_csv(finac.k.13, path = "../fininc-data/clean/clean.finac.k.13.csv")
write_csv(finac.k.06, path = "../fininc-data/clean/clean.finac.k.06.csv")
write_csv(wdi, path = "../fininc-data/clean/clean.wdi.csv")
write_csv(pop, path = "../fininc-data/clean/clean.pop.csv")