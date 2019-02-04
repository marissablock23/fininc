library(tidyverse)
library(here)
library(treemapify)
library(RColorBrewer)
library(extrafont)
library(ggrepel)

### Load data. This is the clean data, which has been prepared in the script prep.R

### Note: Final graphs are at the top of the script. 

findex <- read_csv("../fininc-data/clean/clean.findex.csv")
imf <- read_csv("../fininc-data/clean/clean.imf.csv")
finac.k.15 <- read_csv("../fininc-data/clean/clean.finac.k.15.csv")
finac.k.13 <- read_csv("../fininc-data/clean/clean.finac.k.13.csv")
finac.k.06 <- read_csv("../fininc-data/clean/clean.finac.k.06.csv")
wdi <- read_csv("../fininc-data/clean/clean.wdi.csv")

#####################################################################
# FINAL GRAPHS

## Set theme
theme_set()


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
ggsave(filename = "Graphs/type_bar.pdf", width = 8, height = 5)


## Growth in Mobile Money Transactions by Region
imf %>%
  filter(!is.na(i_mob_transactions_value_GDP), year>2011) %>%
  group_by(Region, year) %>%
  summarize(mean = mean(i_mob_transactions_value_GDP, na.rm = TRUE)) %>%
  mutate(Region2 = factor(Region, levels = c("Sub-Saharan Africa", "East Asia & Pacific", "South Asia", "Europe & Central Asia", "Latin America & Caribbean",
                                            "Middle East & North Africa"))) %>%
  ggplot(aes(x = year, y = mean, color = Region2)) +
  geom_line(size = 0.5) +
  labs(
    title = "Mobile money useage has grown significantly in Sub-Saharan Africa",
    subtitle = "Measured as average value of mobile money transactions (% of GDP)",
    caption = "Source: IMF Financial Access Survey",
    x = "Year",
    y = "Percent of GDP"
  ) +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017)) +
  scale_color_brewer(name = "Region", palette = "Dark2") +
  geom_point() +
  theme(text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        #panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.05, 0.97),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect())
ggsave(filename = "Graphs/mm_line.pdf", width = 8, height = 5)


## Mobile Money in Africa
findex %>%
  filter(year == 2017, region=="Sub-Saharan Africa (excluding high income)") %>%
  select(country, mobileaccount.t.d.1, mobileaccount.t.d.2) %>%
  filter(!is.na(mobileaccount.t.d.1)) %>%
  arrange(mobileaccount.t.d.2) %>%
  mutate(country = factor(country, country), mobileaccount.t.d.1 = mobileaccount.t.d.1*100, 
         mobileaccount.t.d.2 = mobileaccount.t.d.2*100) %>%
  ggplot() +
  geom_segment(aes(x=country, xend = country, y = mobileaccount.t.d.2,
                   yend = mobileaccount.t.d.1), color = "black") +
  geom_point(aes(x = country, y = mobileaccount.t.d.2), color = "#7570b3", size = 2) +
  geom_point(aes(x = country, y = mobileaccount.t.d.1), color = "#1b9e77", size = 2) +
  geom_text(aes(x = "Kenya", y = 65, label = "Female"), nudge_x = -0.7, nudge_y = 0.9,
            color = "black", size = 3, family = "Avenir Next", angle = 20) +
  geom_text(aes(x = "Kenya", y = 78, label = "Male"), nudge_x = -0.7,
            color = "black", size = 3, family = "Avenir Next", angle = 20) +
  coord_flip() +
  labs(
    title = "While Africa leads the way in mobile money access, in almost all countries, a higher proportion 
of males have mobile money accounts",
    subtitle = "Gender Difference in Mobile Money Access",
    caption = "Note: Excludes South Sudan and Central Africa Republic, for which there is no data.
    Source: Global Findex, 2017",
    y = "% of respondents with mobile money accounts (Age +15)",
    x = NULL
  ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  theme(text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        panel.grid.minor.y =element_blank(),
        panel.grid.major.y = element_blank())
ggsave(filename = "Graphs/mm_dotplot.pdf", width = 10, height = 7)


## Heat Map of Demographics by Financial Product
finac.k.15 %>%
  group_by(cluster_type, gender_of_respondent) %>%
  summarize(Bank = mean(e4_1, na.rm = TRUE)*100,
            SACCO = mean(e4_3, na.rm = TRUE)*100,
            Microfinance = mean(e4_4, na.rm = TRUE)*100,
            Savings = mean(e4_5, na.rm = TRUE)*100,
            Loans = mean(e4_7, na.rm = TRUE)*100,
            Insurance = mean(e4_9, na.rm = TRUE)*100) %>%
  gather("Bank", "SACCO", "Microfinance", "Savings", "Loans", "Insurance", key = "product", value = "proportion") %>%
  unite(area_sex, cluster_type, gender_of_respondent) %>%
  mutate(area_sex = factor(area_sex, levels = c("1_1", "1_2", "2_1", "2_2"),
                           labels = c("Rural male", "Rural female", "Urban male", "Urban female"))) %>%
  mutate(product = factor(product, levels = c("Savings", "Bank", "Loans", "Insurance", "SACCO", "Microfinance"))) %>%
  ggplot(aes(x = product, y = area_sex, fill = proportion)) +
  geom_tile(color = "black", size = 0.25) +
  geom_text(aes(label = paste0(round(proportion,1), "%"), family = "Avenir Next", fontface = "bold")) +
  scale_fill_gradient2(name = "Proportion", low = "#e8f5f1", mid = "#76c4ad", high = "#1b9e77", guide = "colorbar", midpoint = 40) +
  #scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Most Kenyans utilize savings accounts, \n regardless of sex or location",
    subtitle = "The proportion of individuals with particular financial products by sex and area",
    caption = "Source: Financial Access Survey 2015-16, Kenya",
    y = NULL,
    x = NULL
  ) +
  theme(text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        panel.background = element_rect(fill = "white"))


## Tree map (Sex: 1 = Male, 2 = Female)
# First, create dataframe of labels
label <- finac.k.15 %>%
  filter(l5_1!="NA", l5_1<995) %>%
  mutate(type = factor(l5_1, labels = c("Commercial bank loan", "Microfinance loan",
                                        "SACCO loan", "Money lender", "Chama (Savings group)", 
                                        "Loan from family/friends", "Gift from family/friends", "Income from another business", "Sale of assets", 
                                        "Own savings", "Inherited", "Government fund", "Other", "Don't know", "Mobile" ))) %>%
  group_by(type, gender_of_respondent) %>%
  summarize(n = n()) %>%
  summarize(total1 = sum(n[gender_of_respondent==1]), total2 = sum(n[gender_of_respondent==2])) %>%
  mutate(male = sum(total1), female = sum(total2), pct.m = (total1/male)*100, pct.f = (total2/female)*100) %>%
  gather(pct.f, pct.m, key = "Sex", value = "pct") %>%
  mutate(label = paste0(type, ": ", round(pct, 1), "%")) %>%
  select(type, Sex, label)
  label$Sex[label$Sex=="pct.f"] <- 2
  label$Sex[label$Sex=="pct.m"] <- 1
  label$Sex<- as.numeric(label$Sex)
# Now, get dataframe of counts and merge in labels
finac.k.15 %>%
  filter(l5_1!="NA", l5_1<995) %>%
  rename(Sex = "gender_of_respondent") %>%
  mutate(type = factor(l5_1, labels = c("Commercial bank loan", "Microfinance loan",
                                        "SACCO loan", "Money lender", "Chama (Savings group)", 
                                        "Loan from family/friends", "Gift from family/friends", "Income from another business", "Sale of assets", 
                                        "Own savings", "Inherited", "Government fund", "Other", "Don't know", "Mobile" ))) %>%
  group_by(type, Sex) %>%
  summarize(n = n()) %>%
  left_join(label) %>%
  mutate(Sex2 = factor(Sex, levels = c("1", "2"),
                       labels = c("Male", "Female"))) %>%
  ggplot(aes(area = n, fill = Sex2, label = label, subgroup = Sex2)) +
  geom_treemap(fill = "white") +
  geom_treemap(aes(alpha = n)) +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_subgroup_text(color = "black", place = "bottomleft", family = "Avenir Next", fontface = "italic") +
  geom_treemap_text(aes(label = label),
                    place = "center",
                    grow = F,
                    color = "black",
                    min.size = 1,
                    reflow = TRUE,
                    family = "Avenir Next"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  labs(title = "Many business owners user their own savings as start-up capital \n rather than loans",
       subtitle = "Females also receive more gifts from family or friends.",
       caption = "Source: Financial Access Survey, 2015-2016, Kenya"
  ) +
  theme(text = element_text(family = "Avenir Next"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"))


## Bar graph
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
ggsave(filename = "Graphs/bar2.pdf", width = 8, height = 5)


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
              size = 5) +
    geom_text_repel(data = combine %>% filter(year==2015), aes(label = paste0(type, ":", round(pct, 2), "%")),
              hjust = 1,
              vjust = -1,
              family = "Avenir Next",
              size = 5) +
    labs(
      title = "Commercial banks and savings products have seen the \n largest increase in usage from 2006 to 2015",
      subtitle = "Credit and microfinance have hardly changed, while insurance has decreased slightly",
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
  
  
## Bubble Chart
  # Create dataframe of labels - MDG, TLS, BRA, QAT, SGP, KWT, BRN, ARG, SUR
  labels.bubble <-   wdi %>%
    filter(year==2016, cntry.code!="SSD") %>%
    select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr) %>%
    left_join(imf) %>%
    select(cntry.code, economy, year, ny.gdp.pcap.kd, fr.inr.rinr, i_depositors_A1_pop, i_deposit_acc_A1_pop, Region) %>%
    filter(!is.na(fr.inr.rinr)) %>%
    filter(cntry.code %in% c("MDG", "TLS", "BRA", "QAT", "SGP", "KWT", "BRN", "ARG", "SUR", "BWA"))
    
  
  wdi %>%
    filter(year==2016) %>%
    select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr) %>%
    left_join(imf) %>%
    select(cntry.code, year, ny.gdp.pcap.kd, fr.inr.rinr, i_depositors_A1_pop, i_deposit_acc_A1_pop, Region) %>%
    filter(!is.na(fr.inr.rinr) & !is.na(ny.gdp.pcap.kd) & !is.na(i_depositors_A1_pop)) %>%
  ggplot(aes(x = ny.gdp.pcap.kd, y = fr.inr.rinr, size = i_depositors_A1_pop, color = Region)) +
    geom_point(alpha = 0.7) +
   geom_label_repel(data = labels.bubble, aes(label = economy), family = "Avenir Next", size = 3,
                   show.legend = FALSE, angle = 30, nudge_x = 0.2, box.padding = 0.5) +
    #geom_label(aes(label = cntry.code)) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = "The Cost of Formal Finance",
      subtitle = "As measured by the real interest rate, GDP per capita",
      caption = "Source: World Development Indicators, IMF Financial Access Survey, 2016",
      y = "Real Interest Rate",
      x = "GDP per capita, (constant 2010 US$)"
    ) +
    theme(text = element_text(family = "Avenir Next"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic")) +
    annotate("rect", xmin = 0, xmax = 20000, ymin = 38, ymax = 55, alpha = 0.15) +
    annotate("text", x = 17000, y = 50, label = "High cost of finance", family = "Avenir Next") +
    annotate("rect", xmin = 1000, xmax = 16000, ymin = -18, ymax = -2, alpha = 0.15) +
    annotate("text", x = 15000, y = -15, label = "Low cost of finance", family = "Avenir Next") +
    scale_size_continuous(name = "# of Depositors per 1,000 adults")
  
  

##################################################################### 
# UNFINISHED

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
