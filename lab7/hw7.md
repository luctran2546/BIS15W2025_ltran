---
title: "Homework 7"
author: "Luc-Tanton Tran"
date: "2025-02-03"
output:
  html_document: 
    theme: spacelab
    keep_md: true
---



## Instructions
Answer the following questions and complete the exercises in RMarkdown. Please embed all of your code and push your final work to your repository. Your final lab report should be organized, clean, and run free from errors. Remember, you must remove the `#` for the included code chunks to run. Be sure to add your name to the author header above.  

Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  

## Load the libraries

``` r
library(tidyverse)
library(janitor)
```

For this assignment we are going to work with a data set from the [United Nations Food and Agriculture Organization](http://www.fao.org/about/en/) on world fisheries.

Load the data `fisheries.csv` as a new object titled `fisheries`.

``` r
fisheries <- clean_names(read_csv("data/fisheries.csv"))
```

```
## Rows: 376771 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (6): country, common_name, isscaap_taxonomic_group, asfis_species_number...
## dbl (4): isscaap_group_number, fao_major_fishing_area, year, catch
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

1. Explore the data. What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables, etc.? You may use the functions that you prefer.

``` r
names(fisheries)
```

```
##  [1] "country"                 "common_name"            
##  [3] "isscaap_group_number"    "isscaap_taxonomic_group"
##  [5] "asfis_species_number"    "asfis_species_name"     
##  [7] "fao_major_fishing_area"  "measure"                
##  [9] "year"                    "catch"
```

``` r
dim(fisheries)
```

```
## [1] 376771     10
```

``` r
anyNA(fisheries)
```

```
## [1] TRUE
```

``` r
class(fisheries)
```

```
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

2. How many countries are represented in the data? Provide a count and list their names.

``` r
fisheries %>% 
  select(country) %>% 
  summarize(n_country = n_distinct(country))
```

```
## # A tibble: 1 × 1
##   n_country
##       <int>
## 1       203
```

``` r
fisheries %>% 
  select(country) %>% 
  distinct(country) %>% 
  pull(country)
```

```
##   [1] "Albania"                   "Algeria"                  
##   [3] "American Samoa"            "Angola"                   
##   [5] "Anguilla"                  "Antigua and Barbuda"      
##   [7] "Argentina"                 "Aruba"                    
##   [9] "Australia"                 "Bahamas"                  
##  [11] "Bahrain"                   "Bangladesh"               
##  [13] "Barbados"                  "Belgium"                  
##  [15] "Belize"                    "Benin"                    
##  [17] "Bermuda"                   "Bonaire/S.Eustatius/Saba" 
##  [19] "Bosnia and Herzegovina"    "Brazil"                   
##  [21] "British Indian Ocean Ter"  "British Virgin Islands"   
##  [23] "Brunei Darussalam"         "Bulgaria"                 
##  [25] "Cabo Verde"                "Cambodia"                 
##  [27] "Cameroon"                  "Canada"                   
##  [29] "Cayman Islands"            "Channel Islands"          
##  [31] "Chile"                     "China"                    
##  [33] "China, Hong Kong SAR"      "China, Macao SAR"         
##  [35] "Colombia"                  "Comoros"                  
##  [37] "Congo, Dem. Rep. of the"   "Congo, Republic of"       
##  [39] "Cook Islands"              "Costa Rica"               
##  [41] "Croatia"                   "Cuba"                     
##  [43] "Cura\xe7ao"                "Cyprus"                   
##  [45] "C\xf4te d'Ivoire"          "Denmark"                  
##  [47] "Djibouti"                  "Dominica"                 
##  [49] "Dominican Republic"        "Ecuador"                  
##  [51] "Egypt"                     "El Salvador"              
##  [53] "Equatorial Guinea"         "Eritrea"                  
##  [55] "Estonia"                   "Ethiopia"                 
##  [57] "Falkland Is.(Malvinas)"    "Faroe Islands"            
##  [59] "Fiji, Republic of"         "Finland"                  
##  [61] "France"                    "French Guiana"            
##  [63] "French Polynesia"          "French Southern Terr"     
##  [65] "Gabon"                     "Gambia"                   
##  [67] "Georgia"                   "Germany"                  
##  [69] "Ghana"                     "Gibraltar"                
##  [71] "Greece"                    "Greenland"                
##  [73] "Grenada"                   "Guadeloupe"               
##  [75] "Guam"                      "Guatemala"                
##  [77] "Guinea"                    "GuineaBissau"             
##  [79] "Guyana"                    "Haiti"                    
##  [81] "Honduras"                  "Iceland"                  
##  [83] "India"                     "Indonesia"                
##  [85] "Iran (Islamic Rep. of)"    "Iraq"                     
##  [87] "Ireland"                   "Isle of Man"              
##  [89] "Israel"                    "Italy"                    
##  [91] "Jamaica"                   "Japan"                    
##  [93] "Jordan"                    "Kenya"                    
##  [95] "Kiribati"                  "Korea, Dem. People's Rep" 
##  [97] "Korea, Republic of"        "Kuwait"                   
##  [99] "Latvia"                    "Lebanon"                  
## [101] "Liberia"                   "Libya"                    
## [103] "Lithuania"                 "Madagascar"               
## [105] "Malaysia"                  "Maldives"                 
## [107] "Malta"                     "Marshall Islands"         
## [109] "Martinique"                "Mauritania"               
## [111] "Mauritius"                 "Mayotte"                  
## [113] "Mexico"                    "Micronesia, Fed.States of"
## [115] "Monaco"                    "Montenegro"               
## [117] "Montserrat"                "Morocco"                  
## [119] "Mozambique"                "Myanmar"                  
## [121] "Namibia"                   "Nauru"                    
## [123] "Netherlands"               "Netherlands Antilles"     
## [125] "New Caledonia"             "New Zealand"              
## [127] "Nicaragua"                 "Nigeria"                  
## [129] "Niue"                      "Norfolk Island"           
## [131] "Northern Mariana Is."      "Norway"                   
## [133] "Oman"                      "Other nei"                
## [135] "Pakistan"                  "Palau"                    
## [137] "Palestine, Occupied Tr."   "Panama"                   
## [139] "Papua New Guinea"          "Peru"                     
## [141] "Philippines"               "Pitcairn Islands"         
## [143] "Poland"                    "Portugal"                 
## [145] "Puerto Rico"               "Qatar"                    
## [147] "Romania"                   "Russian Federation"       
## [149] "R\xe9union"                "Saint Barth\xe9lemy"      
## [151] "Saint Helena"              "Saint Kitts and Nevis"    
## [153] "Saint Lucia"               "Saint Vincent/Grenadines" 
## [155] "SaintMartin"               "Samoa"                    
## [157] "Sao Tome and Principe"     "Saudi Arabia"             
## [159] "Senegal"                   "Serbia and Montenegro"    
## [161] "Seychelles"                "Sierra Leone"             
## [163] "Singapore"                 "Sint Maarten"             
## [165] "Slovenia"                  "Solomon Islands"          
## [167] "Somalia"                   "South Africa"             
## [169] "Spain"                     "Sri Lanka"                
## [171] "St. Pierre and Miquelon"   "Sudan"                    
## [173] "Sudan (former)"            "Suriname"                 
## [175] "Svalbard and Jan Mayen"    "Sweden"                   
## [177] "Syrian Arab Republic"      "Taiwan Province of China" 
## [179] "Tanzania, United Rep. of"  "Thailand"                 
## [181] "TimorLeste"                "Togo"                     
## [183] "Tokelau"                   "Tonga"                    
## [185] "Trinidad and Tobago"       "Tunisia"                  
## [187] "Turkey"                    "Turks and Caicos Is."     
## [189] "Tuvalu"                    "US Virgin Islands"        
## [191] "Ukraine"                   "Un. Sov. Soc. Rep."       
## [193] "United Arab Emirates"      "United Kingdom"           
## [195] "United States of America"  "Uruguay"                  
## [197] "Vanuatu"                   "Venezuela, Boliv Rep of"  
## [199] "Viet Nam"                  "Wallis and Futuna Is."    
## [201] "Yemen"                     "Yugoslavia SFR"           
## [203] "Zanzibar"
```



3. Based on the `asfis_species_number`, how many different fish species were caught?

``` r
n_distinct(fisheries$asfis_species_number)
```

```
## [1] 1551
```

4. What is the total catch for each `isscaap_taxonomic_group` in the data set?

``` r
fisheries %>% 
  group_by(isscaap_taxonomic_group) %>% 
  summarize(
    total = sum(catch, na.rm=T)) %>% 
  arrange(-total)
```

```
## # A tibble: 30 × 2
##    isscaap_taxonomic_group           total
##    <chr>                             <dbl>
##  1 Herrings, sardines, anchovies   1898529
##  2 Cods, hakes, haddocks           1218092
##  3 Miscellaneous pelagic fishes    1217644
##  4 Miscellaneous coastal fishes     995248
##  5 Tunas, bonitos, billfishes       978951
##  6 Marine fishes not identified     863949
##  7 Miscellaneous demersal fishes    452445
##  8 Squids, cuttlefishes, octopuses  311928
##  9 Sharks, rays, chimaeras          263569
## 10 Shrimps, prawns                  228203
## # ℹ 20 more rows
```

5. Based on the `asfis_species_name`, what are the top 5 most caught species? (note: "Osteichthyes" is not a species, it refers to unidentified fish. You should filter this one out.).

``` r
fisheries |> 
  filter(asfis_species_name != "Osteichthyes") |> 
  group_by(asfis_species_number, asfis_species_name) |> 
  summarize(total_catches = sum(catch, na.rm = TRUE), .groups = 'keep') |> 
  arrange(-total_catches)
```

```
## # A tibble: 1,547 × 3
## # Groups:   asfis_species_number, asfis_species_name [1,547]
##    asfis_species_number asfis_species_name      total_catches
##    <chr>                <chr>                           <dbl>
##  1 1210600208           Engraulis ringens              540739
##  2 1480401601           Theragra chalcogramma          473722
##  3 1210500105           Clupea harengus                264304
##  4 1480400202           Gadus morhua                   217930
##  5 1750102501           Katsuwonus pelamis             194923
##  6 1210501303           Sardinops sagax                176094
##  7 1750100201           Scomber japonicus              175444
##  8 1702300405           Trachurus murphyi              152034
##  9 1210501301           Sardinops melanostictus        144774
## 10 1230400201           Mallotus villosus              131239
## # ℹ 1,537 more rows
```

6. For the species that was caught the most, which country had the highest catch?

``` r
fisheries %>% 
  select(country, common_name, asfis_species_name, catch) %>% 
  filter(asfis_species_name == "Engraulis ringens") %>% 
  group_by(country) |> 
  summarize(
    total_catch = sum(catch, na.rm= TRUE)
  ) |> 
  arrange(-total_catch)
```

```
## # A tibble: 3 × 2
##   country total_catch
##   <chr>         <dbl>
## 1 Peru         439717
## 2 Chile        100694
## 3 Ecuador         328
```

7. Which country had the largest overall catch between the years 2002-2012?

``` r
fisheries %>% 
  select(country, year, catch) %>% 
  filter(between(year, 2002, 2012)) %>%
    group_by(country) %>% 
    summarize(overall_catch = sum(catch, na.rm=TRUE)) %>% 
  arrange(-overall_catch)
```

```
## # A tibble: 200 × 2
##    country                  overall_catch
##    <chr>                            <dbl>
##  1 China                           269238
##  2 United States of America        167309
##  3 Russian Federation              112028
##  4 Indonesia                       104362
##  5 Japan                           104152
##  6 Chile                            97030
##  7 Spain                            92286
##  8 Peru                             86482
##  9 Portugal                         69778
## 10 India                            69681
## # ℹ 190 more rows
```

8. Which country had the largest catch in a single year? What year was it? 

``` r
fisheries %>% 
  select(country, year, catch) %>% 
    group_by(country, year) %>% 
    summarize(overall_catch = sum(catch, na.rm=TRUE), .groups='keep') %>% 
  arrange(-overall_catch)
```

```
## # A tibble: 11,583 × 3
## # Groups:   country, year [11,583]
##    country  year overall_catch
##    <chr>   <dbl>         <dbl>
##  1 Peru     1970         77000
##  2 Peru     1971         76800
##  3 Peru     1968         62700
##  4 China    2001         39327
##  5 Japan    1972         30167
##  6 Japan    1988         29701
##  7 China    2002         28967
##  8 China    2010         27851
##  9 Japan    1976         27825
## 10 China    1998         27593
## # ℹ 11,573 more rows
```

9. Fishing practices for sharks, rays, chimaeras and other cartilaginous fish (group 38) are of particular concern due to their conservation status. Make a new dataframe `sharks` that only contains data on this group. 

``` r
sharks <- fisheries %>% 
  filter(isscaap_group_number == "38")
sharks
```

```
## # A tibble: 23,210 × 10
##    country common_name               isscaap_group_number isscaap_taxonomic_gr…¹
##    <chr>   <chr>                                    <dbl> <chr>                 
##  1 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  2 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  3 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  4 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  5 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  6 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  7 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  8 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  9 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
## 10 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
## # ℹ 23,200 more rows
## # ℹ abbreviated name: ¹​isscaap_taxonomic_group
## # ℹ 6 more variables: asfis_species_number <chr>, asfis_species_name <chr>,
## #   fao_major_fishing_area <dbl>, measure <chr>, year <dbl>, catch <dbl>
```

10. Using the `sharks` dataframe, use `mutate()` to create a new column called `fishing_pressure` that is based on the total catch. If the total catch is greater than 75, the fishing pressure is considered "high", otherwise it is considered "moderate".

``` r
sharks %>% 
  mutate(fishing_pressure = ifelse(catch > 75, "high", "moderate"))
```

```
## # A tibble: 23,210 × 11
##    country common_name               isscaap_group_number isscaap_taxonomic_gr…¹
##    <chr>   <chr>                                    <dbl> <chr>                 
##  1 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  2 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  3 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  4 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  5 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  6 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  7 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  8 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
##  9 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
## 10 Albania Angelsharks, sand devils…                   38 Sharks, rays, chimaer…
## # ℹ 23,200 more rows
## # ℹ abbreviated name: ¹​isscaap_taxonomic_group
## # ℹ 7 more variables: asfis_species_number <chr>, asfis_species_name <chr>,
## #   fao_major_fishing_area <dbl>, measure <chr>, year <dbl>, catch <dbl>,
## #   fishing_pressure <chr>
```

11. For sharks, how does the average catch differ between high and moderate fishing pressure?

``` r
sharks %>% 
  mutate(fishing_pressure = ifelse(catch > 75, "high", "moderate")) %>% 
  group_by(fishing_pressure) %>% 
  summarize(
    average_catch = mean(catch),
    n= n()
  )
```

```
## # A tibble: 3 × 3
##   fishing_pressure average_catch     n
##   <chr>                    <dbl> <int>
## 1 high                      86.6   779
## 2 moderate                  14.3 13672
## 3 <NA>                      NA    8759
```

12. Perform one analysis of your choice on the fisheries dataframe that includes a minimum of three lines of code and two functions. Write a sentence or two that explains the intent of your code.

``` r
fisheries %>% 
  select(country, asfis_species_name, catch) %>% #select variables of interest
  filter(catch >= 50) %>% #filtering for any catch above 50 tonnes
  group_by(country) %>% #grouping by country
  summarize(
    average_catch = mean(catch, na.rm=T), #calculate average catch per country
    n=n()) %>% 
  arrange(-average_catch) #arrange in desc order of catch per country
```

```
## # A tibble: 191 × 3
##    country                  average_catch     n
##    <chr>                            <dbl> <int>
##  1 Myanmar                          1590.    48
##  2 Peru                             1511.   391
##  3 Viet Nam                          869.   102
##  4 China                             705.  1022
##  5 Chile                             680.   514
##  6 Korea, Dem. People's Rep          447.    39
##  7 Norway                            405.   702
##  8 Un. Sov. Soc. Rep.                328.  1020
##  9 Japan                             321.  2306
## 10 Bangladesh                        321.    94
## # ℹ 181 more rows
```

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.   
