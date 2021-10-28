# INFO 658: Disability and Health Data System (DHDS) Visualization

## Background
Data sourced from the CDC's [Disability and Health Data System (DHDS) Data Set](https://data.cdc.gov/Disability-Health/Disability-and-Health-Data-System-DHDS-/k62p-6esq)

Visualizations created in [R](https://www.r-project.org/)

---
### Load Data
```
library(tidyverse)

df <- read_csv("[local_path_to_csv]/DHDS.csv")
```

### Disability Type by Region
```
# filter by HHS region, rows that describe a disability type, in 2019
df_hhs_region <- df %>%
  filter((LocationDesc == ("HHS Region 1") | LocationDesc == ("HHS Region 2") | LocationDesc == ("HHS Region 3") |
           LocationDesc == ("HHS Region 4") | LocationDesc == ("HHS Region 5") | LocationDesc == ("HHS Region 6") |
           LocationDesc == ("HHS Region 7") | LocationDesc == ("HHS Region 8") | LocationDesc == ("HHS Region 9") |
           LocationDesc == ("HHS Region 10")) &
           StratificationCategory1 == "Disability Type" & Year == "2019"
         )

# count disability types by region
df_region_sum <- df_hhs_region %>%
  group_by(LocationDesc) %>%
  summarize(
    count(df_hhs_region, Stratification1)
  )
  
# plot disabilities against occurrences
ggplot(df_region_sum, aes(x = Stratification1, y = n)) +
  geom_bar(stat="identity")

# at this point, notice the values have been normalized, want to see proportional to US population
# look at column descriptions on CDC site to see weightedNumber for occurrence in population
ggplot(df_hhs_region, aes(x = Stratification1, y = WeightedNumber)) +
  geom_bar(stat="identity")

# see it by region
ggplot(df_hhs_region, aes(x = Stratification1, y = WeightedNumber, fill = Stratification1)) +
  geom_bar(stat="identity") +
  facet_wrap(~LocationDesc) +
  theme(axis.text.x=element_blank()) +
  labs(
    x = "Disability Type",
    y = "Occurrences in US population",
    title = "Occurrences of Disability Types by HHS Region in 2019",
    subtitle = "Health & Human Services (HHS) Regions divide the US into 10 regional offices",
    fill = "Disability Type"
  )

#save as png
ggsave("images/disabilities_region.png",
       width = 16,
       height = 8,
       dpi = 600)  
```

### Disability by Age and Year
```
#see it by age, Stratification1 for these rows is only the any vs no disability indicator
ggplot(df_age, aes(x = Stratification1, y = WeightedNumber, fill = Stratification1)) +
  geom_bar(stat="identity") +
  facet_wrap(Year~Stratification2, ncol = 3) +
  scale_fill_manual(values=c("#1a63c9", "#bbc0c7")) +
  labs(
    x = "Disability Status",
    y = "Occurrences in US population",
    title = "Occurrences of Disability by Age Group and Year 2016 - 2019",
    fill = "Disability Status"
  ) +
  theme_bw()

ggsave("images/disability_age.png",
       width = 6,
       height = 8,
       dpi = 600)
```

### Disability by Sex and Year
```
df_sex <- df %>%
  filter(Stratification2 == "Male" | Stratification2 == "Female")

#see it by sex
#note that duplicates have not been removed, for example if an individual has both vision and mobile disabilities
ggplot(df_sex, aes(x = Stratification2, y = WeightedNumber, fill = Stratification2)) +
  geom_bar(stat="identity") +
  facet_wrap(Year~Stratification1, ncol = 4) +
  labs(
    x = "Sex",
    y = "Occurrences in US population",
    title = "Male and Female Occurrences of Disability 2016 - 2019",
    fill = "Sex"
  ) +
  theme_bw()

ggsave("images/disability_sex.png",
       width = 8, #in inches
       height = 6,
       dpi = 600)
```

### Barriers by Cost of Health Care
```
# filter for Barriers & Costs of Health Care
df_barriers <- df %>%
  filter(Category == "Barriers & Costs of Health Care" & LocationAbbr != "US" & LocationAbbr != "HHS1"
         & LocationAbbr != "HHS2" & LocationAbbr != "HHS3" & LocationAbbr != "HHS4"
         & LocationAbbr != "HHS5" & LocationAbbr != "HHS6" & LocationAbbr != "HHS7"
         & LocationAbbr != "HHS8" & LocationAbbr != "HHS9" & LocationAbbr != "HHS10"
         & IndicatorID == "CBARRIER"& Year == "2019"
         )

#weighted barriers by state
ggplot(df_barriers, aes(x = LocationAbbr, y = WeightedNumber, fill = LocationAbbr)) +
  geom_bar(stat="identity") +
  theme_bw() +
  labs(
    x = "State Abbreviation",
    y = "Occurrences in US population",
    title = "Occurrences of Disability Where Patients Report Cost Barriers to Care in 2019",
    subtitle = "Occurrences where the indicator is 'Could not see a doctor due to cost in the past 12 months among adults 18 years of age or older'",
    fill = "States"
  )

ggsave("images/barriers_cost.png",
       width = 16,
       height = 8,
       dpi = 600)
```
