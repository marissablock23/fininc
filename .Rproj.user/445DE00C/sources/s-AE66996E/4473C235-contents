library(tidyverse)
library(here)
library(treemapify)
library(RColorBrewer)
library(extrafont)

### Load data. This is the clean data, which has been prepared in the script prep.R

### Note: Final graphs are at the top of the script. 

findex <- read_csv("data/clean/clean.findex.csv")
imf <- read_csv("data/clean/clean.imf.csv")
finac.k.16 <- read_csv("data/clean/clean.finac.k.16.csv")

#####################################################################
# FINAL GRAPHS
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
  theme(plot.title = element_text(family = "Arial Bold"))
ggsave(filename = "Graphs/type_bar.pdf", width = 8, height = 5)


## Growth in Mobile Money Transactions by Region
imf %>%
  filter(!is.na(i_mob_transactions_value_GDP), year>2011) %>%
  group_by(Region, year) %>%
  summarize(mean = mean(i_mob_transactions_value_GDP, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean, color = Region)) +
  geom_line(size = 0.5) +
  labs(
    title = "Mobile money useage has grown significantly in Sub-Saharan Africa",
    subtitle = "Measured as average value of mobile money transactions (% of GDP)",
    caption = "Source: IMF Financial Access Survey",
    x = "Year",
    y = "Percent of GDP"
  ) +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017)) +
  scale_color_brewer(palette = "Dark2") +
  geom_point()
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
  geom_point(aes(x = country, y = mobileaccount.t.d.2), color = "pink") +
  geom_point(aes(x = country, y = mobileaccount.t.d.1), color = "blue") +
  coord_flip() +
  labs(
    title = "Gender Differences in Mobile Money Access",
    subtitle = "While Africa leads the way in mobile money access, in almost all countries, a higher proportion 
of males have mobile money accounts",
    caption = "Note: Excludes South Sudan and Central Africa Republic, for which there is no data.
    Source: Global Findex, 2017",
    y = "% of respondents with mobile money accounts (+15)",
    x = NULL
  ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80))
ggsave(filename = "Graphs/mm_dotplot.pdf", width = 10, height = 7)



finac.k %>%
  group_by(cluster_type, gender_of_respondent) %>%
  summarize(bank = mean(e4_1, na.rm = TRUE)*100,
            sacco = mean(e4_3, na.rm = TRUE)*100,
            micro = mean(e4_4, na.rm = TRUE)*100,
            savings = mean(e4_5, na.rm = TRUE)*100,
            loans = mean(e4_7, na.rm = TRUE)*100,
            insurance = mean(e4_9, na.rm = TRUE)*100) %>%
  gather("bank", "sacco", "micro", "savings", "loans", "insurance", key = "product", value = "proportion") %>%
  unite(area_sex, cluster_type, gender_of_respondent) %>%
  mutate(area_sex = factor(area_sex, levels = c("1_1", "1_2", "2_1", "2_2"),
                           labels = c("Rural male", "Rural female", "Urban male", "Urban female"))) %>%
  ggplot(aes(x = product, y = area_sex, fill = proportion)) +
  geom_tile(color = "black", size = 0.25) +
  geom_text(aes(label = paste0(round(proportion,1), "%"))) +
  scale_fill_gradient2(name = "Proportion") +
  labs(
    title = "Most Kenyans utilize savings accounts, regardless of sex or location",
    subtitle = "The proportion of individuals with particular financial products by sex and area",
    caption = "Source: Financial Access Survey, Kenya",
    y = NULL,
    x = NULL
  )






finac.k %>%
  filter(l5_1!="NA", l5_1<995) %>%
  mutate(type = factor(l5_1, labels = c("Commercial bank loan", "Microfinance loan",
                                        "SACCO loan", "Money lender", "Chama (Savings group)", 
                                        "Loan from family/friends", "Gift from family/friends", "Income from another business", "Sale of assets", 
                                        "Own savings", "Inherited", "Government fund", "Other", "Don't know", "Mobile" ))) %>%
  group_by(type, gender_of_respondent) %>%
  summarize(n = n()) %>%
ggplot(aes(area = n, fill = as.factor(gender_of_respondent), label = as.character(type), subgroup = as.factor(gender_of_respondent))) +
  geom_treemap(fill = "black") +
  geom_treemap(aes(alpha = n)) +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_text(aes(label = type),
                        place = "center",
                        grow = F,
                        color = "white",
                        min.size = 1,
                        reflow = TRUE,
                        ) +
  scale_fill_discrete() +
  theme(legend.position = "none") +
  labs(title = "A majority of business owners user their own savings as start-up capital \n rather than loans",
       subtitle = "Female,                                                                                                                              Male",
       caption = "Source: Financial Access Survey, 2015-2016, Kenya"
  )



finac.k %>%
  filter(!is.na(l11_1)) %>%
  group_by(l11_1) %>%
  mutate(bank = "Account at a bank") %>%
  ggplot(aes(x = as.factor(bank), fill = as.factor(l11_1))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_brewer(palette = "Oranges", name = NULL, labels = c("Currently use", "Used to use", "Never used")) +
  labs(
    title = "An overwhelming majority of Kenyan business-owners have never used a bank account",
    subtitle = "At least for business purposes",
    caption = "Source: Financial Acces Survey, 2015-2016, Kenya",
    x = NULL
  )

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
