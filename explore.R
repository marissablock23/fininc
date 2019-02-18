library(tidyverse)
library(here)
library(treemapify)
library(RColorBrewer)
library(extrafont)
library(ggrepel)
library(sf)
library(ggspatial)
library(ggridges)
library(waffle)


### Load data. This is the clean data, which has been prepared in the script prep.R

### Note: Final graphs are available in the R Markdown file.

findex <- read_csv("../fininc-data/clean/clean.findex.csv")
imf <- read_csv("../fininc-data/clean/clean.imf.csv")
finac.k.15 <- read_csv("../fininc-data/clean/clean.finac.k.15.csv")
finac.k.13 <- read_csv("../fininc-data/clean/clean.finac.k.13.csv")
finac.k.06 <- read_csv("../fininc-data/clean/clean.finac.k.06.csv")
wdi <- read_csv("../fininc-data/clean/clean.wdi.csv")
pop <- read_csv("../fininc-data/clean/clean.pop.csv")

#####################################################################

## Average # of Financial Institutions, 2017
imf %>%
  filter(year==2017) %>%
  group_by(inc.grp) %>%
  summarize(mfi = mean(i_branches_pop_A3B1a, na.rm = TRUE), 
            bank = mean(i_branches_A1_pop, na.rm = TRUE),
            cu = mean(i_branches_A2_pop, na.rm = TRUE)) %>%
  gather("mfi", "bank", "cu", key = "type", value = "mean") %>%
  mutate(inc.grp = as.factor(inc.grp)) %>%
  mutate(inc.grp = fct_relevel(inc.grp, "Low income", "Lower middle income",
                               "Upper middle income", "High income")) %>%
  ggplot(aes(x = inc.grp, y = mean, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Low income countries tend to have more non-traditional financial institutions",
    subtitle = "Average # of Financial Institutions per 100,000 individuals, 2017",
    caption = "Source: IMF Financial Access Survey",
    x = "Income Group",
    y = "Average per 100,000 individuals"
  ) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
  scale_fill_discrete(name = NULL, labels = c("Commercial Banks", 
                                                "Credit Unions and \nFinancial Cooperatives",
                                                "Microfinance Institutions")) +
  theme(plot.title = element_text(family = "Avenir Next", face = "bold"),
        panel.background = element_rect(fill = "white", color = "black"))


##################################################################### 
## UNFINISHED

# Map

## Dot Density of banked and unbanked

# Create county labels
labels3 <- kenya %>%
  cbind(st_coordinates(st_centroid(kenya))) %>%
  filter(county=="Mandera" | county=="Garissa" | county=="Wajir")

# No bank
nobank <- finac.k.15 %>%
  filter(bank_usage==3) %>%
  group_by(county) %>%
  summarize(n = n())

# Used to bank
prevbank <- finac.k.15 %>%
  filter(bank_usage==2) %>%
  group_by(county) %>%
  summarize(n2 = n())

# Merge no bank + used to bank
bank.usage <- nobank %>%
  left_join(prevbank, by = "county")


sf <- finac.k.15 %>%
  filter(bank_usage==1) %>%
  group_by(county) %>%
  summarize(n3 = n()) %>%
  left_join(bank.usage, by = "county") %>%
  left_join(kenya, by = "county") %>%
  st_as_sf() %>%
  select(county, n3, n, n2, geometry)

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

num_dots <- as.data.frame(sf) %>%
  select(n3, n, n2) %>%
  mutate_all(funs(./1)) %>%
  mutate_all(random_round)

sf_dots <- map_df(names(num_dots),
                  ~st_sample(sf, size = num_dots[,.x], type = "random") %>%
                    st_cast("POINT") %>%
                    st_coordinates() %>%
                    as_tibble() %>%
                    setNames(c("lon", "lat")) %>%
                    mutate(mob = .x)
) %>%
  slice(sample(1:n()))

ggplot() +
  geom_sf(data = sf, fill = "transparent", color = "white") +
  geom_point(data = sf_dots, aes(lon, lat, color = mob), size = 1, alpha = 0.7) +
  geom_label(data = labels3, aes(X, Y, label = county), size = 3, family = "Avenir Next") +
  coord_sf(crs = 21097, datum = NA) +
  scale_color_brewer(name = NULL, labels = c("Never banked", "Previously banked", "Currently banked"), palette = "Dark2") +
  labs(
    x = NULL,
    y = NULL,
    title = "Individuals who have never banked dominate",
    subtitle = "Some counties in the northeast tend to have more unbanked individuals",
    caption = "Source: Financial Access Survey 2015-16, Kenya"
  ) +
  theme(text = element_text(family = "Avenir Next", color = "white"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "bottom", legend.direction = "horizontal",
        aspect.ratio = 1.05,
        plot.background = element_rect(fill = "#212121", color = NA), 
        panel.background = element_rect(fill = "#212121", color = NA),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", color = NA),
        legend.text = element_text(size = 10),
  )



# Map merging shape file and fin access survey

# Make bar plot
finac.k.15 %>%
  mutate(nobank = bank_usage - 2) %>%
  group_by(county) %>%
  filter(nobank==1) %>%
  summarize(n = n()) %>%
  left_join(pop, by = "county") %>%
  mutate(usepcap = (n/pop_2015)*100000) %>%
  ggplot(aes(x = reorder(county, usepcap), y = (usepcap), fill = cut_number((usepcap), 4))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = NULL, values = c("#a3d8c8", "#76c4ad", "#48b192", "#1b9e77")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    x = NULL,
    y = "Unbanked per capita"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.05, 0.97),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect())



# Create county labels
labels2 <- kenya %>%
  cbind(st_coordinates(st_centroid(kenya))) %>%
  filter(county=="Isiolo" | county=="Lamu" | county=="Marsabit")
  labels2$county[labels2$county=="Isiolo"] <- "Isiolo, 83.7"
  labels2$county[labels2$county=="Lamu"] <- "Lamu, 41.5"
  labels2$county[labels2$county=="Marsabit"] <- "Marsabit, 43.9"

# Map
finac.k.15 %>%
  mutate(nobank = bank_usage - 2) %>%
  group_by(county) %>%
  filter(nobank==1) %>%
  summarize(n = n()) %>%
  left_join(pop, by = "county") %>%
  mutate(usepcap = (n/pop_2015)*100000) %>%
  left_join(kenya, by = "county") %>%
  cbind(st_coordinates(st_centroid(kenya))) %>%
  mutate(density = n/Shape_Area) %>%
  ggplot() +
  geom_sf(aes(fill = usepcap)) +
  geom_label_repel(data = labels2, aes(X, Y, label = county), size = 3, family = "Avenir Next") +
  coord_sf(datum=NA) +
  scale_fill_distiller(name = NULL, palette = "Greens", direction = 1) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        axis.title = element_blank()) +
  labs(title = "Individuals who have never banked are \nconcentrated in three counties",
       subtitle = "Individuals who never banked per 100,000 in 2015",
       caption = "Source: 2015-16 Financial Access Survey \n Population data is a projection from UNICEF")

pop %>%
  group_by(county) %>%
  select(county, pop_2015) %>%
  ggplot(aes(x = reorder(county, pop_2015), y = (pop_2015), fill = cut_number((pop_2015), 4))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = NULL, values = c("#a3d8c8", "#76c4ad", "#48b192", "#1b9e77"),
                    labels = c("Less than 25.99%", "26% - 41.3%", "41.4% - 50.5%", "50.6% or more")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  my.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.05, 0.97),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect())


# Strip Chart
wdi %>%
  filter(year==2016) %>%
  select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr) %>%
  left_join(imf) %>%
  select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr, i_depositors_A1_pop, i_deposit_acc_A1_pop, Region) %>%
  filter(!is.na(fr.inr.rinr) & !is.na(ny.gdp.pcap.kd) & !is.na(i_depositors_A1_pop)) %>%
  ggplot(aes(x = i_deposit_acc_A1_pop, y = factor(1))) +
  geom_point(alpha = 0.2, size = 3) +
  labs(y = NULL) +
  scale_y_discrete(labels = NULL) +
  theme(axis.ticks.y = element_blank(),
        panel.grid = element_blank())

# Ridgeline Plot
wdi %>%
  filter(year==2016) %>%
  select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr) %>%
  left_join(imf) %>%
  select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr, i_depositors_A1_pop, i_deposit_acc_A1_pop, Region) %>%
  filter(!is.na(fr.inr.rinr) & !is.na(ny.gdp.pcap.kd) & !is.na(i_depositors_A1_pop)) %>%
  ggplot(aes(x = i_deposit_acc_A1_pop, y = as.factor(Region))) +
  geom_density_ridges(fill = "#1b9e77", alpha = 0.5)


# Bar chart - Business owners + Bank Accounts
finac.k.15 %>%
  filter(!is.na(l11_1)) %>%
  group_by(l11_1) %>%
  mutate(bank = "Account at a bank") %>%
  ggplot(aes(x = as.factor(bank), fill = as.factor(l11_1))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_brewer(palette = "Dark2", name = NULL, labels = c("Currently use", "Used to use", "Never used")) +
  labs(
    title = "An overwhelming majority of Kenyan business-owners \nhave never used a bank account",
    subtitle = "At least for business purposes",
    caption = "Source: Financial Access Survey, 2015-2016, Kenya",
    x = NULL,
    y = NULL
  ) +
  theme(text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.05, 0.97),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect())


## Slope graph
finac.k.06 <- mutate(finac.k.06, year = 2006)
finac.k.06 <- rename(finac.k.06, bank_usage = "A4", savings_usage = "X_savings", credit_usage = "X_credit", mfi_usage = "X_cur_mfi", insurance_usage = "X_useins")
finac.k.15 <- mutate(finac.k.15, year = 2015)


y2006 <- finac.k.06 %>%
  select(year, bank_usage, savings_usage, credit_usage, mfi_usage, insurance_usage)
y2015 <- finac.k.15 %>%
  select(year, bank_usage, savings_usage, credit_usage, mfi_usage, insurance_usage)

combine <-bind_rows(y2006, y2015)
combine <- combine %>%
  rename(Banks = "bank_usage", Savings = "savings_usage", Credit = "credit_usage", Microfinance = "mfi_usage", Insurance = "insurance_usage") %>%
  gather(Banks, Savings, Credit, Microfinance, Insurance, key = "type", value = "value") %>%
  group_by(year, type) %>%
  summarize(total1 = sum(value[value==1]), total2 = n(), pct = (total1/total2)*100)

ggplot(data = combine, aes(x = year, y = pct, group = type)) +
  geom_line(aes(color = type, alpha = 1), size = 2) +
  geom_point(aes(color = type, alpha = 1), size = 4) +
  scale_x_continuous(name = "Year", position = "top", breaks = c(2006, 2015)) +
  scale_color_brewer(palette = "Dark2") +
  geom_text_repel(data = combine %>% filter(year==2006), aes(label = paste0(type, ":", round(pct, 2), "%")),
                  hjust = -0.1,
                  vjust = -1,
                  family = "Avenir Next",
                  size = 4) +
  geom_text_repel(data = combine %>% filter(year==2015), aes(label = paste0(type, ":", round(pct, 2), "%")),
                  hjust = 1,
                  vjust = -1,
                  family = "Avenir Next",
                  size = 4) +
  labs(
    title = "Commercial banks and savings products have seen the \n largest increase in usage from 2006 to 2015",
    subtitle = "Credit and microfinance have hardly changed, \n while insurance has decreased slightly",
    caption = "Note: Individuals may have more than one financial product, \n so these numbers do not sum to 100. \n Source: Financial Access Survey, Kenya, 2006 and 2015"
  ) +
  theme(text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "none",
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank())



# Make bar plot
findex %>%
  filter(year==2017 & region=="Sub-Saharan Africa (excluding high income)") %>%
  select(country, account.t.d) %>%
  ggplot(aes(x = reorder(country, account.t.d), y = (account.t.d*100), fill = cut_number((account.t.d*100), 4))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = NULL, values = c("#a3d8c8", "#76c4ad", "#48b192", "#1b9e77"),
                    labels = c("Less than 25.99%", "26% - 41.3%", "41.4% - 50.5%", "50.6% or more")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    x = NULL,
    y = "% of respondents with a financial account (Age +15)"
  ) +
  my.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.05, 0.97),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect())

# Make labels
labels <- africa %>%
  cbind(st_coordinates(st_centroid(africa))) %>%
  filter(ADMIN=="Kenya" | ADMIN=="Namibia" | ADMIN=="South Africa" | ADMIN=="Central African Republic" | ADMIN=="Niger")
labels$ADMIN[labels$ADMIN=="Central African Republic"] <- "CAR"

# Create data frame of missing countries from Findex
missing <- africa %>%
  filter(ADMIN=="Sudan" | ADMIN== "Angola" | ADMIN=="eSwatini" | ADMIN=="Burundi" | ADMIN=="Gambia" | ADMIN=="Guinea-Bissau" | ADMIN=="Equatorial Guinea" | ADMIN=="Western Sahara" | ADMIN=="Somalia" | ADMIN=="Somaliland" | ADMIN=="Eritrea") %>%
  select(ADMIN, geometry, cntry.code) %>%
  mutate(account.t.d = NA)

# Map
africa %>%
  left_join(findex, by = "cntry.code") %>%
  filter(year==2017 & region=="Sub-Saharan Africa (excluding high income)" | region=="Middle East & North Africa (excluding high income)") %>%
  select(cntry.code, geometry, account.t.d, ADMIN) %>%
  rbind(missing) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = cut_number(account.t.d, 4)), size = 0.2, color = "black") +
  geom_label_repel(data = labels, aes(X, Y, label = ADMIN), family = "Avenir Next", size = 3) +
  coord_sf(datum=NA) +
  scale_fill_manual(name = NULL, values = c("#a3d8c8", "#76c4ad", "#48b192", "#1b9e77"), na.value = "grey",
                    labels = c("Less than 25.99%", "26% - 41.3%", "41.4% - 50.5%", "50.6% or more")) +
  my.theme +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal",
        plot.margin =
  ) +
  labs(
    title = "Kenya, Namibia, and South Africa have the highest \nproportion of the population having a financial account",
    subtitle = "Whereas financial access is low in Niger and \nthe Central African Republic (CAR)",
    caption = "Source: Global Findex, 2017\nMap is from https://www.naturalearthdata.com/")



#### OLD
            daily = sum(f6_1[f6_1==1]),
            prp = daily/n) %>%
  left_join(pop, by = "county") %>%
  mutate(usepcap =(daily/pop_2015)*1000) %>%
#  ggplot(aes(x = usepcap)) +
#    geom_histogram(bindwidth = 3)
  
  
  left_join(kenya, by = "county") %>%
  cbind(st_coordinates(st_centroid(kenya))) %>%
  ggplot() +
  geom_sf(aes(fill = usepcap)) +
  coord_sf(datum=NA) +
  scale_fill_distiller(name = "Proportion", palette = "Greens", direction = 1) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Avenir Next")) +
  labs(title = "Daily Usage of Mobile Money",
       subtitle = "Per Capita Usage in 2015",
       caption = "Source: 2015-16 Financial Access Survey \n Population data is a projection from UNICEF")


## Cartogram
finac.k.15 %>%
  group_by(county) %>%
  filter(!is.na(f6_1)) %>%
  summarize(n = n(),
            daily = sum(f6_1[f6_1==1]),
            prp = daily/n) %>%
  left_join(pop, by = "county") %>%
  mutate(usepcap =(daily/pop_2015)*1000) %>%
  #  ggplot(aes(x = usepcap)) +
  #    geom_histogram(bindwidth = 3)
  
  
  left_join(kenya, by = "county") %>%
  st_as_sf() %>%
  cartogram_cont("daily", itermax=5) %>%
  ggplot() +
    geom_sf(aes(fill = daily))


      
  
## Dot Density
# MFI
mfi.agg <- mfi.loc %>%
  group_by(county) %>%
  summarize(n = n())

# MFI Banks
mfib.agg <- mfib.loc %>%
  group_by(county) %>%
  summarize(n = n())

# Merge MFIs
mfi <- mfi.agg %>%
  left_join(mfib.agg, by = "county") %>%
  mutate(n.y = replace_na(n.y, 0)) %>%
  group_by(county) %>%
  summarize(mfi = sum(n.x, n.y))

sf <- bank.loc %>%
  group_by(county, mobilemon) %>%
  summarize(n = n()) %>%
  spread(key = mobilemon, value = n) %>%
  rename(no = `0`, yes = `1`) %>%
  mutate(yes = replace_na(yes, 0)) %>%
  left_join(mfi, by = "county") %>%
  mutate(mfi = replace_na(mfi, 0)) %>%
  left_join(kenya, by = "county") %>%
  st_as_sf() %>%
  select(county, yes, no, mfi, geometry) %>%
  filter(county=="Nairobi" | county=="Kiambu" | county== "Narok" | county== "Murang'a" | county== "Nakuru" | county== "Kajiado" | county== "Nyandarua"
         | county== "Kakamega" | county== "Bungoma" | county== "Bomet" | county=="Baringo" | county=="Nyamira" | county == "Migori" | county== "Homa Bay"
         | county== "Kisii" | county== "Kisumu" | county == "Kericho" | county== "Vihiga" | county== "Busia" | county== "Siaya" | county== "Nandi")

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

num_dots <- as.data.frame(sf) %>%
  select(yes, no, mfi) %>%
  mutate_all(funs(./5)) %>%
  mutate_all(random_round)

sf_dots <- map_df(names(num_dots),
                  ~st_sample(sf, size = num_dots[,.x], type = "random") %>%
                    st_cast("POINT") %>%
                    st_coordinates() %>%
                    as_tibble() %>%
                    setNames(c("lon", "lat")) %>%
                    mutate(mob = .x)
                  ) %>%
  slice(sample(1:n()))

  ggplot() +
    geom_sf(data = sf, fill = "transparent", color = "white") +
    geom_point(data = sf_dots, aes(lon, lat, color = mob), size = 1, alpha = 0.7) +
    #coord_sf(xlim=c(-20000, 450332), ylim = c(-200000, -20000)) +
    coord_sf(crs = 21097, datum = NA) +
    scale_color_brewer(name = NULL, labels = c("MFI", "Just bank", "Bank + Mobile Money"), palette = "Dark2") +
    labs(
      x = NULL,
      y = NULL,
      title = "Banks vs. Banks + Mobile Money Outlets",
      subtitle = "Most banks do not double-up as a mobile money outlet \n\nOne dot = 5 institutions",
      caption = "Source: Financial Access Survey 2015-16, Kenya"
    ) +
    theme(text = element_text(family = "Avenir Next", color = "white"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          legend.position = "bottom", legend.direction = "horizontal",
          aspect.ratio = 1.05,
          plot.background = element_rect(fill = "#212121", color = NA), 
          panel.background = element_rect(fill = "#212121", color = NA),
          legend.background = element_rect(fill = "#212121", color = NA),
          legend.key = element_rect(fill = "#212121", colour = NA),
          legend.text = element_text(size = 10),
          )
  


##  

  ## Dot Density of banked and unbanked
  # No bank
  nobank <- finac.k.15 %>%
    filter(bank_usage==3) %>%
    group_by(county) %>%
    summarize(n = n())
  
  # Used to bank
  prevbank <- finac.k.15 %>%
    filter(bank_usage==2) %>%
    group_by(county) %>%
    summarize(n2 = n())
  
  # Merge no bank + used to bank
  bank.usage <- nobank %>%
    left_join(prevbank, by = "county")
  
  
  sf <- finac.k.15 %>%
    filter(bank_usage==1) %>%
    group_by(county) %>%
    summarize(n3 = n()) %>%
    left_join(bank.usage, by = "county") %>%
    left_join(kenya, by = "county") %>%
    st_as_sf() %>%
    select(county, n3, n, n2, geometry)
   # filter(county=="Nairobi" | county=="Kiambu" | county== "Narok" | county== "Murang'a" | county== "Nakuru" | county== "Kajiado" | county== "Nyandarua"
    #       | county== "Kakamega" | county== "Bungoma" | county== "Bomet" | county=="Baringo" | county=="Nyamira" | county == "Migori" | county== "Homa Bay"
     #      | county== "Kisii" | county== "Kisumu" | county == "Kericho" | county== "Vihiga" | county== "Busia" | county== "Siaya" | county== "Nandi")
  
  random_round <- function(x) {
    v=as.integer(x)
    r=x-v
    test=runif(length(r), 0.0, 1.0)
    add=rep(as.integer(0),length(r))
    add[r>test] <- as.integer(1)
    value=v+add
    ifelse(is.na(value) | value<0,0,value)
    return(value)
  }
  
  num_dots <- as.data.frame(sf) %>%
    select(n3, n, n2) %>%
    mutate_all(funs(./1)) %>%
    mutate_all(random_round)
  
  sf_dots <- map_df(names(num_dots),
                    ~st_sample(sf, size = num_dots[,.x], type = "random") %>%
                      st_cast("POINT") %>%
                      st_coordinates() %>%
                      as_tibble() %>%
                      setNames(c("lon", "lat")) %>%
                      mutate(mob = .x)
  ) %>%
    slice(sample(1:n()))
  
  ggplot() +
    geom_sf(data = sf, fill = "transparent", color = "white") +
    geom_point(data = sf_dots, aes(lon, lat, color = mob), size = 1, alpha = 0.7) +
    #coord_sf(xlim=c(-20000, 450332), ylim = c(-200000, -20000)) +
    coord_sf(crs = 21097, datum = NA) +
    scale_color_brewer(name = NULL, labels = c("Never banked", "Previously banked", "Currently banked"), palette = "Dark2") +
    labs(
      x = NULL,
      y = NULL,
      title = "Individuals who have never banked dominate",
      subtitle = "Some counties in the northeast tend to have more unbanked",
      caption = "Source: Financial Access Survey 2015-16, Kenya"
    ) +
    theme(text = element_text(family = "Avenir Next", color = "white"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          legend.position = "bottom", legend.direction = "horizontal",
          aspect.ratio = 1.05,
          plot.background = element_rect(fill = "#212121", color = NA), 
          panel.background = element_rect(fill = "#212121", color = NA),
          legend.background = element_rect(fill = "#212121", color = NA),
          legend.key = element_rect(fill = "#212121", color = NA),
          legend.text = element_text(size = 10),
    )
  
  








banksf %>%
  cbind(st_coordinates(st_centroid(banksf))) %>%
  print(n = 100)
  ggplot() +
  geom_sf(data = kenya, fill = "transparent") +
  geom_point(aes(X, Y))

kenya %>%
  left_join(bank.loc, by = "county") %>%
  select(county, geometry, lat, long) %>%
  ggplot() +
  geom_sf() +
  geom_point(aes(long, lat))


kenya %>%
  ggplot() +
  geom_sf()

banksf %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = kenya, fill = "transparent")



kenya %>%
  mutate(area = st_area(geometry)) %>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(area))) +
  scale_fill_discrete()
  geom_sf(fill = "#7570b3", color = "black") +
  # Add District Labels
  geom_label_repel(aes(X, Y, label = DISTNAME), size = 2)

+
  geom_sf(data = bank.loc, size = 0.5)
  annotation_scale()
  annotation_north_arrow(style = north_arrow_minimal)

  
  
bank.loc %>%
  group_by(county) %>%
  summarize(n = n())



finac.k.15 %>%
  filter(f6_1==2) %>%
  ggplot(aes(x = as.character(a2), y = f6_1)) +
  geom_bar(stat = "identity")



finac.k %>%
  select(cluster_type, q5_2) %>%
  group_by(cluster_type, q5_2) %>%
  summarize(n = n()) %>%
  group_by(cluster_type) %>%
  mutate(countT = sum(n)) %>%
  group_by(q5_2, add=TRUE) %>%
  mutate(per=round(100*n/countT, 2)) %>%
  
  ggplot(aes(x = as.factor(q5_2), y = as.numeric(per))) +
  geom_line()

finac.k %>%
  group_by(a3) %>%
  summarize(mean.inc = mean(total_income, na.rm = TRUE),
            mean.sav = mean(r17_11, na.rm = TRUE),
            mean.hh = mean(a_7_1, na.rm = TRUE),
            mean.age = mean(age, na.rm = TRUE)) %>%
  #filter(mean.inc<150000) %>%
  ggplot(aes(x = mean.hh, y = mean.sav, size = mean.age, color = mean.inc)) +
  geom_point() +
  scale_size_continuous(trans="log", range = c(1, 10))
geom_smooth(model = lm)
scale_color_brewer(palette = "Set1")


ggplot(finac.k, aes(x = a_7_1, y = r17_11)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~gender_of_household_head) +
  geom_jitter()




finac.k %>%
  filter(!is.na(l11_1)) %>%
  group_by(l11_1) %>%
  #summarize(n = n()) %>%
  mutate(bank = "Account at a bank") %>%
  #spread(key = l11_1, value = n) %>%
  #mutate(bank = "bank")
  ggplot(aes(x = as.factor(bank), fill = as.factor(l11_1))) +
  geom_bar(position = "dodge")

sum(is.na(finac.k$l11_1))
