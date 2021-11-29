hw6
================
sze pui
11/28/2021

#Problem 1 ##tidy data lets import and clean the library first… For
tidying data we have mainly two tasks: 1.convert numeric to factor where
appropriate 2.check for missing data

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

There is no missing data in the birthweight dataframe, therefore we dont
habe to deal with the missing data using such as na.omit()/drop_na()

##Regression model Propose a regression model for birthweight.

When I run the linear regression with all variables, I only keep the
variables showing a very small p-values. A small p value of a variable
indicates that it is significant to the baby’s birthweight.For example,
menarche ,mother’s age at menarche, is eliminated since it has a large
p-value with 2.261406e-01  
Therefore, based on the above selection rule, I selected the following
variables with very small varibales: baby’s head circumference , baby’s
length, mother’s weight,babysex, family monthly income, gestational
age,mom’s age, mother’s pre-pregnancy BMI, average number of cigarettes
smoked per day during pregnancy

``` r
#Model fitting
fit = lm(bwt ~ blength +bhead + delwt+ babysex +fincome +gaweeks +momage +ppbmi +smoken , data = birthweight_df) 
 
#tidying output 
fit %>% 
  broom::tidy() %>% 
  select(term, estimate, p.value)%>% 
  knitr::kable(digits = 3)
```

| term          |  estimate | p.value |
|:--------------|----------:|--------:|
| (Intercept)   | -6115.005 |       0 |
| blength       |    77.486 |       0 |
| bhead         |   134.626 |       0 |
| delwt         |     3.546 |       0 |
| babysexfemale |    30.909 |       0 |
| fincome       |     1.009 |       0 |
| gaweeks       |    12.675 |       0 |
| momage        |     4.120 |       0 |
| ppbmi         |   -14.059 |       0 |
| smoken        |    -2.821 |       0 |

Let’s do the diagnostics!

``` r
birthweight_df %>% 
  modelr::add_residuals(fit) %>% 
   modelr::add_predictions(fit) %>% 
  ggplot(aes(x = pred, y = resid)) + geom_point()+
   labs(x ="predicted baby’s birth weight (grams)", 
        y= "residuals", 
        title = "scatterplots of prediction against residuals ")
```

![](hw6_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#Comparision to two other models
