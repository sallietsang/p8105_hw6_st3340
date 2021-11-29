hw6
================
sze pui
11/28/2021

#tidy data lets import and clean the library first… For tidying data we
have mainly two tasks: 1.convert numeric to factor where appropriate
2.check for missing data

``` r
#task 1 convert numeric to factor where appropriate
birthweight_df =read_csv("data/birthweight.csv") %>% 
  janitor::clean_names()%>% 
  #convert babysex,frace,malform,mrace into factor and recode them 
  mutate( babysex= as.factor(babysex),
          frace= as.factor(frace),
          malform = as.factor(malform),
          mrace = as.factor(mrace))%>% 
  #babysex: baby’s sex (male = 1, female = 2)
  mutate(babysex=  recode(babysex,"1" = "male","2"= "female"),
         frace= recode(frace,"1" = "white", "2" = "black", "3" = "asian", "4" = "puerto rican", "8" = "other", "9" = "unknown"),
        malform=  recode(malform,"0" = "absent", "1" = "present" ),
        mrace = recode(mrace,"1" = "White", "2" = "Black", "3" = "Asian", "4" = "Puerto Rican", "8" = "Other")) %>% 
                           rename(dad_race = frace, mom_race = mrace)
```

    ## Rows: 4342 Columns: 20

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#task 2  check for missing data
sum(is.na(birthweight_df))
```

    ## [1] 0

There is no missing data in the birthweight dataframe.

#Regression model
