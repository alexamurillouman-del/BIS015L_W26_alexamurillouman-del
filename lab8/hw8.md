---
title: "Homework 8"
author: "Ale Murillo Umanzor"
date: "2/04/26"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---

## Instructions
Answer the following questions and/or complete the exercises in RMarkdown. Please embed all of your code and push the final work to your repository. Your report should be organized, clean, and run free from errors. Remember, you must remove the `#` for any included code chunks to run.  

## Load the libraries

``` r
library("tidyverse")
library("janitor")
#library("naniar")
options(scipen = 999)
```

## About the Data
For this assignment we are going to work with a data set from the [United Nations Food and Agriculture Organization](https://www.fao.org/fishery/en/collection/capture) on world fisheries. These data were downloaded and cleaned using the `fisheries_clean.Rmd` script.  

Load the data `fisheries_clean.csv` as a new object titled `fisheries_clean`.

``` r
fisheries_clean <- read_csv("data/fisheries_clean.csv")
```

1. Explore the data. What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables, etc.? You may use the functions that you prefer.

``` r
summary(fisheries_clean)
```

```
##      period      continent          geo_region          country         
##  Min.   :1950   Length:1055015     Length:1055015     Length:1055015    
##  1st Qu.:1980   Class :character   Class :character   Class :character  
##  Median :1996   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :1994                                                           
##  3rd Qu.:2010                                                           
##  Max.   :2023                                                           
##  scientific_name    common_name        taxonomic_code         catch           
##  Length:1055015     Length:1055015     Length:1055015     Min.   :       0.0  
##  Class :character   Class :character   Class :character   1st Qu.:       0.0  
##  Mode  :character   Mode  :character   Mode  :character   Median :       2.9  
##                                                           Mean   :    5089.9  
##                                                           3rd Qu.:     400.0  
##                                                           Max.   :12277000.0  
##     status         
##  Length:1055015    
##  Class :character  
##  Mode  :character  
##                    
##                    
## 
```

2. Convert the following variables to factors: `period`, `continent`, `geo_region`, `country`, `scientific_name`, `common_name`, `taxonomic_code`, and `status`.

``` r
fisheries_clean <- fisheries_clean %>% 
  mutate(period= as.factor(period),
         continent= as.factor(continent),
         geo_region= as.factor(geo_region),
         country= as.factor(country),
         scientific_name= as.factor(scientific_name),
         common_name= as.factor(common_name),
         taxonomic_code= as.factor(taxonomic_code),
         status= as.factor(status)) %>% 
  relocate(period, continent, geo_region, country, scientific_name, common_name, taxonomic_code, status)
```

##Are there any missing values in the data? If so, which variables contain missing values and how many are missing for each variable


4. How many countries are represented in the data?

``` r
fisheries_clean %>% 
  summarize(n_countries=n_distinct(country))
```

```
## # A tibble: 1 × 1
##   n_countries
##         <int>
## 1         249
```

5. The variables `common_name` and `taxonomic_code` both refer to species. How many unique species are represented in the data based on each of these variables? Are the numbers the same or different?

``` r
fisheries_clean %>% 
  summarize(n_common_name= n_distinct(common_name), n_taxonomic_code= n_distinct (taxonomic_code)) 
```

```
## # A tibble: 1 × 2
##   n_common_name n_taxonomic_code
##           <int>            <int>
## 1          3390             3722
```

6. In 2023, what were the top five countries that had the highest overall catch?


``` r
fisheries_clean %>% 
  filter(period=="2023") %>% 
  group_by(country) %>% 
  summarize(total_catch=sum(catch)) %>% 
  arrange(desc(total_catch))
```

```
## # A tibble: 238 × 2
##    country                  total_catch
##    <fct>                          <dbl>
##  1 China                      13424705.
##  2 Indonesia                   7820833.
##  3 India                       6177985.
##  4 Russian Federation          5398032 
##  5 United States of America    4623694 
##  6 Peru                        3519381.
##  7 Viet Nam                    3417238.
##  8 Japan                       2904942.
##  9 Chile                       2596488.
## 10 Norway                      2546846.
## # ℹ 228 more rows
```

7. In 2023, what were the top 10 most caught species? To keep things simple, assume `common_name` is sufficient to identify species. What does `NEI` stand for in some of the common names? How might this be concerning from a fisheries management perspective?

NEI stands for Not Elsewhere Included

``` r
fisheries_clean %>% 
  filter(period=="2023") %>% 
  group_by(common_name) %>% 
  summarize(total_catch= sum(catch)) %>% 
   arrange(desc(total_catch))
```

```
## # A tibble: 2,870 × 2
##    common_name                    total_catch
##    <fct>                                <dbl>
##  1 Marine fishes NEI                 8553907.
##  2 Freshwater fishes NEI             5880104.
##  3 Alaska pollock(=Walleye poll.)    3543411.
##  4 Skipjack tuna                     2954736.
##  5 Anchoveta(=Peruvian anchovy)      2415709.
##  6 Blue whiting(=Poutassou)          1739484.
##  7 Pacific sardine                   1678237.
##  8 Yellowfin tuna                    1601369.
##  9 Atlantic herring                  1432807.
## 10 Scads NEI                         1344190.
## # ℹ 2,860 more rows
```

8. For the species that was caught the most above (not NEI), which country had the highest catch in 2023?

In 2023, the country with the highest total catch was China, followed by Indonesia, India, etc.

``` r
top_species <- fisheries_clean %>%
  filter(period == "2023", !str_detect(common_name, "NEI")) %>%
  group_by(common_name) %>%
  summarize(total_catch = sum(catch, na.rm = T)) %>%
  arrange(desc(total_catch))
```


``` r
fisheries_clean %>%
  filter(period == "2023") %>%
  group_by(country) %>%
  summarise(total_catch = sum(catch, na.rm = T)) %>%
  arrange(desc(total_catch))
```

```
## # A tibble: 238 × 2
##    country                  total_catch
##    <fct>                          <dbl>
##  1 China                      13424705.
##  2 Indonesia                   7820833.
##  3 India                       6177985.
##  4 Russian Federation          5398032 
##  5 United States of America    4623694 
##  6 Peru                        3519381.
##  7 Viet Nam                    3417238.
##  8 Japan                       2904942.
##  9 Chile                       2596488.
## 10 Norway                      2546846.
## # ℹ 228 more rows
```

9. How has fishing of this species changed over the last decade (2013-2023)? Create a  plot showing total catch by year for this species.

From 2013-2023 the total catch for this species remains fairly stable with small year-to-year fluctuations. Catch peaks in the late 2010s, drops around 2020, and then partially recovers by 2023, indicating no strong long-term trend over the decade.

``` r
fisheries_clean %>%
  filter(period %in% c(2013, 2014, 2015, 2016, 2017,
                       2018, 2019, 2020, 2021, 2022, 2023)) %>%
  group_by(period) %>%
  summarise(total_catch = sum(catch, na.rm = T)) %>%
  ggplot(aes(x = period, y = total_catch, fill=period)) +
  geom_col() +
  labs(title = "Total decade catch",
       x= "Year",
       y="Total catch")
```

![](hw8_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

10. Perform one exploratory analysis of your choice. Make sure to clearly state the question you are asking before writing any code.

How does total catch differ by continent in 2023?

``` r
fisheries_clean %>% 
  filter(period == "2023") %>% 
  group_by(continent) %>% 
  summarize(total_catch = sum(catch, na.rm = T)) %>% 
  arrange(desc(total_catch))
```

```
## # A tibble: 6 × 2
##   continent total_catch
##   <fct>           <dbl>
## 1 Asia        48578981.
## 2 Americas    17375444.
## 3 Europe      14430135.
## 4 Africa      10711862.
## 5 Oceania      1581909.
## 6 <NA>           46166.
```

## Knit and Upload
Please knit your work as an .html file and upload to Canvas. Homework is due before the start of the next lab. No late work is accepted. Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  
