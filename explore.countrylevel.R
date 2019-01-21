library(tidyverse)
library(here)
library(haven)
library(readxl)

############################# Import Data #############################
## Global Findex
findex <- read_xlsx("data/global_findex.xlsx", sheet = "Data",
                    na = "")

# Rename first columns
findex <- rename(findex, year = "X__1", cntry.code = "X__2", country = "X__3",
                 region = "X__4", inc.grp = "X__5")


## IMF - Financial Access Survey
imf <- read_dta("data/imf_fas.dta")
head(imf$iso3)
imf <- rename(imf, cntry.code = "iso3")


## Region and income classification variables
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


# Merge IMF + income group and region variables
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
ggsave(filename = "type_bar.pdf", width = 8, height = 5)


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
ggsave(filename = "mm_line.pdf", width = 8, height = 5)


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
ggsave(filename = "mm_dotplot.pdf", width = 10, height = 7)
  
 