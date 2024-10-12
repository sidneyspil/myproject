My Dataset
================
2024-09-26

``` r
# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))
```

``` r
#install.packages("dplyr")
library(car)
```

    ## Loading required package: carData

``` r
library(dplyr) #recode variables
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:car':
    ## 
    ##     recode

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)
#library(psych)
#library(bruceR)
library(haven) #load CSV

#install.packages("nortest")

#ibrary(nortest)
#install.packages("car")
```

Reading dataset

``` r
# Set working directory
#setwd("C:/Users/sidne/OneDrive/Documents/GitHub/myproject")

# Read the TSV file
data <- read.table("C:/Users/sidne/OneDrive/Desktop/PSY329/38964-0001-Data.tsv", sep = "\t", header = TRUE, fill = TRUE, quote = "")

# Write the data to a CSV file
write.csv(data, "Excel-Data.csv", row.names = FALSE)

#open file
data <- read.csv("C:/Users/sidne/OneDrive/Desktop/PSY329/Excel-Data.csv")
```

Cleaning data set and recoding variables

``` r
clean_data <- data %>%
  select(SEX,HHINC,HAPPY,SAT1,SAT2,SAT3,SAT4,SAT5,LONELY_A,LONELY_B,LONELY_C) 

clean_data$SEX<-recode(data$SEX, '1' = 'Male', '2' = 'Female', '3'='Other')

clean_data$HOUSE_INCOME<-recode(data$HHINC, '1' = 'Low', '2' = 'Low', '3'='Low','4'='Low','5'='Low','6'='Middle','7'='Middle','8'='Middle','9'='Middle','10'='Middle','11'='High','12'='High')
```

    ## Warning: Unreplaced values treated as NA as `.x` is not compatible.
    ## Please specify replacements exhaustively or supply `.default`.

``` r
clean_data$HAPPY_meaning<-recode(data$HAPPY, '1' = 'Very happy', '2' = 'Rather happy', '3'='Not very happy','4'='Not at all happy')

clean_data <- clean_data %>%
   mutate(Lonely=rowMeans(cbind(LONELY_A,LONELY_B,LONELY_C))) #1 hardly ever feel lonely to 5 often feel lonely 

clean_data <- clean_data %>%
  mutate(Lonely = ifelse(Lonely > 3, NA, Lonely)) #removing participants that responded don't know or refused the question

clean_data <- clean_data %>%
   mutate(Life_satisfaction=rowMeans(cbind(SAT1,SAT2,SAT3,SAT4,SAT5))) #1 strongly disagree to 5 strongly agree higher score = higher life satisfaction

clean_data <- clean_data %>%
  mutate(Life_satisfaction = ifelse(Life_satisfaction > 5, NA, Life_satisfaction))

clean_data$HAPPY_REV <- 5 - clean_data$HAPPY #reverse code happy so higher number = happier

clean_data <- clean_data %>%
  select(SEX,HOUSE_INCOME,HAPPY_meaning, HAPPY_REV,Life_satisfaction, SAT1,SAT2,SAT3,SAT4,SAT5,Lonely, LONELY_A,LONELY_B,LONELY_C)

clean_data<- drop_na(clean_data)


write.csv(clean_data, "C:/Users/sidne/OneDrive/Desktop/PSY329/clean_data.csv", row.names = FALSE)
```

Normality of Lonely

``` r
ggplot(clean_data, aes(x = Lonely)) + geom_histogram(binwidth = 0.5) + facet_wrap(~SEX)+theme_classic() #between SEX
```

![](Dataset-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(clean_data, aes(x = Lonely)) + geom_histogram(binwidth = 0.5) + facet_wrap(~HOUSE_INCOME)+theme_classic() #between INCOME
```

![](Dataset-analysis_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
clean_data %>%
  group_by(SEX) %>% 
  summarize(W = shapiro.test(Lonely)$statistic, p_value = shapiro.test(Lonely)$p.value)
```

    ## # A tibble: 3 × 3
    ##   SEX        W  p_value
    ##   <chr>  <dbl>    <dbl>
    ## 1 Female 0.920 9.36e-41
    ## 2 Male   0.896 8.63e-44
    ## 3 Other  0.815 1.60e- 4

``` r
clean_data %>%
  group_by(HOUSE_INCOME) %>% 
  summarize(W = shapiro.test(Lonely)$statistic, p_value = shapiro.test(Lonely)$p.value)
```

    ## # A tibble: 3 × 3
    ##   HOUSE_INCOME     W  p_value
    ##   <chr>        <dbl>    <dbl>
    ## 1 High         0.837 8.98e-19
    ## 2 Low          0.920 1.68e-45
    ## 3 Middle       0.883 4.48e-36

Normality of Life satisfaction

``` r
ggplot(clean_data, aes(x = Life_satisfaction)) + geom_histogram(binwidth = 0.5) + facet_wrap(~SEX)+theme_classic()
```

![](Dataset-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(clean_data, aes(x = Life_satisfaction)) + geom_histogram(binwidth = 0.5) + facet_wrap(~HOUSE_INCOME)+theme_classic()
```

![](Dataset-analysis_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
clean_data %>%
  group_by(SEX) %>% 
  summarize(W = shapiro.test(Life_satisfaction)$statistic, p_value = shapiro.test(Life_satisfaction)$p.value)
```

    ## # A tibble: 3 × 3
    ##   SEX        W  p_value
    ##   <chr>  <dbl>    <dbl>
    ## 1 Female 0.981 2.63e-22
    ## 2 Male   0.978 4.91e-23
    ## 3 Other  0.931 5.97e- 2

``` r
clean_data %>%
  group_by(HOUSE_INCOME) %>% 
  summarize(W = shapiro.test(Life_satisfaction)$statistic, p_value = shapiro.test(Life_satisfaction)$p.value)
```

    ## # A tibble: 3 × 3
    ##   HOUSE_INCOME     W  p_value
    ##   <chr>        <dbl>    <dbl>
    ## 1 High         0.946 4.06e-10
    ## 2 Low          0.983 4.79e-24
    ## 3 Middle       0.967 8.39e-21

Normality of Happiness

``` r
ggplot(clean_data, aes(x = HAPPY_REV)) + geom_histogram(binwidth = 1) + facet_wrap(~SEX)+theme_classic()
```

![](Dataset-analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(clean_data, aes(x = HAPPY_REV)) + geom_histogram(binwidth = 1) + facet_wrap(~HOUSE_INCOME)+theme_classic()
```

![](Dataset-analysis_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
clean_data %>%
  group_by(SEX) %>% 
  summarize(W = shapiro.test(HAPPY_REV)$statistic, p_value = shapiro.test(HAPPY_REV)$p.value)
```

    ## # A tibble: 3 × 3
    ##   SEX        W  p_value
    ##   <chr>  <dbl>    <dbl>
    ## 1 Female 0.817 2.66e-54
    ## 2 Male   0.826 2.85e-52
    ## 3 Other  0.866 1.66e- 3

``` r
clean_data %>%
  group_by(HOUSE_INCOME) %>% 
  summarize(W = shapiro.test(HAPPY_REV)$statistic, p_value = shapiro.test(HAPPY_REV)$p.value)
```

    ## # A tibble: 3 × 3
    ##   HOUSE_INCOME     W  p_value
    ##   <chr>        <dbl>    <dbl>
    ## 1 High         0.750 7.34e-23
    ## 2 Low          0.830 1.63e-58
    ## 3 Middle       0.793 1.73e-44

Variance of Lonely

``` r
clean_data %>%
  group_by(SEX) %>%
  summarize(variacne = var(Lonely))
```

    ## # A tibble: 3 × 2
    ##   SEX    variacne
    ##   <chr>     <dbl>
    ## 1 Female    0.398
    ## 2 Male      0.422
    ## 3 Other     0.496

``` r
leveneTest(Lonely~SEX, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    2  1.2168 0.2962
    ##       7308

``` r
clean_data %>%
  group_by(HOUSE_INCOME) %>%
  summarize(variacne = var(Lonely))
```

    ## # A tibble: 3 × 2
    ##   HOUSE_INCOME variacne
    ##   <chr>           <dbl>
    ## 1 High            0.339
    ## 2 Low             0.416
    ## 3 Middle          0.369

``` r
leveneTest(Lonely~HOUSE_INCOME, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2  9.9273 4.949e-05 ***
    ##       7308                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Variance of Life satisfaction

``` r
clean_data %>%
  group_by(SEX) %>%
  summarize(variacne = var(Life_satisfaction))
```

    ## # A tibble: 3 × 2
    ##   SEX    variacne
    ##   <chr>     <dbl>
    ## 1 Female    0.953
    ## 2 Male      1.02 
    ## 3 Other     1.39

``` r
leveneTest(Life_satisfaction~SEX, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)  
    ## group    2  2.9108 0.0545 .
    ##       7308                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
clean_data %>%
  group_by(HOUSE_INCOME) %>%
  summarize(variacne = var(Life_satisfaction))
```

    ## # A tibble: 3 × 2
    ##   HOUSE_INCOME variacne
    ##   <chr>           <dbl>
    ## 1 High            0.796
    ## 2 Low             0.960
    ## 3 Middle          0.897

``` r
leveneTest(Life_satisfaction~HOUSE_INCOME, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2   9.328 8.996e-05 ***
    ##       7308                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Variance of Happiness

``` r
clean_data %>%
  group_by(SEX) %>%
  summarize(variacne = var(HAPPY_REV))
```

    ## # A tibble: 3 × 2
    ##   SEX    variacne
    ##   <chr>     <dbl>
    ## 1 Female    0.515
    ## 2 Male      0.549
    ## 3 Other     0.734

``` r
leveneTest(HAPPY_REV~SEX, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value   Pr(>F)   
    ## group    2  6.1011 0.002252 **
    ##       7308                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
clean_data %>%
  group_by(HAPPY_REV) %>%
  summarize(variacne = var(Life_satisfaction))
```

    ## # A tibble: 4 × 2
    ##   HAPPY_REV variacne
    ##       <dbl>    <dbl>
    ## 1         1    0.465
    ## 2         2    0.541
    ## 3         3    0.616
    ## 4         4    0.674

``` r
leveneTest(HAPPY_REV~HOUSE_INCOME, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2    11.6 9.341e-06 ***
    ##       7308                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
