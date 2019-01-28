library(tidyverse)
library(here)
library(treemapify)

### 1. Run R script "prep" first to get clean data.

#####################################################################################

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
                                                "Microfinance Institutions"))
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
            savings = mean(e4_5, na.rm = TRUE)*100) %>%
  gather("bank", "sacco", "micro", "savings", key = "product", value = "proportion") %>%
  unite(area_sex, cluster_type, gender_of_respondent) %>%
  mutate(area_sex = factor(area_sex, levels = c("1_1", "1_2", "2_1", "2_2"),
                           labels = c("Rural male", "Rural female", "Urban male", "Urban female"))) %>%
  ggplot(aes(x = product, y = area_sex, fill = proportion)) +
  geom_tile(color = "white", size = 0.25) +
  geom_text(aes(label = paste0(round(proportion,1), "%"))) +
  labs(
    title = "Most Kenyans utilize savings accounts, regardless of sex or location",
    subtitle = "Proportion of individuals with particular financial products by sex and area",
    caption = "Source Financial Access Survey, Kenya",
    y = NULL,
    x = NULL
  )


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
  group_by(l5_1) %>%
  summarize(n = n()) %>%
  filter(l5_1!="NA", l5_1<995) %>%
  mutate(text = c("Commercial bank loan", "Microfinance loan", "SACCO loan"))
ggplot(aes(area = n, fill = n)) +
  geom_treemap() +
  geom_treemap_text(aes(label = as.character(l5_1)))