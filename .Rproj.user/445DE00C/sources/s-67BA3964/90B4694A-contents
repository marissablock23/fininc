library(tidyverse)
library(here)
library(treemapify)
library(RColorBrewer)
library(extrafont)
library(ggrepel)
library(sf)
library(ggspatial)
library(maptools)

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





# Map merging shape file and fin access survey

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








  ggplot() +
  geom_sf() +
  geom_point(aes(long, lat))



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
