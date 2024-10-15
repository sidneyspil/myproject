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
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

    ## The following object is masked from 'package:car':
    ## 
    ##     logit

``` r
library(bruceR)
```

    ## 
    ## bruceR (v2024.6)
    ## Broadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## ✔ data.table ✔ emmeans
    ## ✔ dplyr      ✔ lmerTest
    ## ✔ tidyr      ✔ effectsize
    ## ✔ stringr    ✔ performance
    ## ✔ ggplot2    ✔ interactions
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## For full functionality, please install all dependencies:
    ## install.packages("bruceR", dep=TRUE)
    ## 
    ## Online documentation:
    ## https://psychbruce.github.io/bruceR
    ## 
    ## To use this package in publications, please cite:
    ## Bao, H.-W.-S. (2024). bruceR: Broadly useful convenient and efficient R functions (Version 2024.6) [Computer software]. https://CRAN.R-project.org/package=bruceR

    ## 
    ## These packages are dependencies of `bruceR` but not installed:
    ## - pacman, openxlsx, ggtext, vars, phia, MuMIn, GGally
    ## 
    ## ***** Install all dependencies *****
    ## install.packages("bruceR", dep=TRUE)

``` r
library(haven) #load CSV

#install.packages("nortest")

#ibrary(nortest)
#install.packages("car")
```

Reading dataset

``` r
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
clean_data <- clean_data %>% filter(SEX != "Other")

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
#clean_data %>%
 # group_by(SEX) %>% 
  #summarize(W = shapiro.test(Lonely)$statistic, p_value = shapiro.test(Lonely)$p.value)

#clean_data %>%
 # group_by(HOUSE_INCOME) %>% 
  #summarize(W = shapiro.test(Lonely)$statistic, p_value = shapiro.test(Lonely)$p.value)

describeBy(Lonely ~ SEX, data= clean_data)
```

    ## 
    ##  Descriptive statistics by group 
    ## SEX: Female
    ##        vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## Lonely    1 3749 1.85 0.63      2    1.81 0.99   1   3     2 0.26    -0.95 0.01
    ## ------------------------------------------------------------ 
    ## SEX: Male
    ##        vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## Lonely    1 3533 1.76 0.65   1.67     1.7 0.99   1   3     2 0.43    -0.95 0.01

``` r
describeBy(Lonely ~ HOUSE_INCOME, data= clean_data)
```

    ## 
    ##  Descriptive statistics by group 
    ## HOUSE_INCOME: High
    ##        vars   n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## Lonely    1 355 1.53 0.58   1.33    1.45 0.49   1   3     2 0.86    -0.31 0.03
    ## ------------------------------------------------------------ 
    ## HOUSE_INCOME: Low
    ##        vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## Lonely    1 4970 1.88 0.64      2    1.85 0.99   1   3     2 0.21    -1.02 0.01
    ## ------------------------------------------------------------ 
    ## HOUSE_INCOME: Middle
    ##        vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## Lonely    1 1957 1.66 0.61   1.67    1.59 0.99   1   3     2 0.59    -0.65 0.01

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
#clean_data %>%
 # group_by(SEX) %>% 
  #summarize(W = shapiro.test(Life_satisfaction)$statistic, p_value = shapiro.test(Life_satisfaction)$p.value)

#clean_data %>%
 # group_by(HOUSE_INCOME) %>% 
  #summarize(W = shapiro.test(Life_satisfaction)$statistic, p_value = shapiro.test(Life_satisfaction)$p.value)

describeBy(Life_satisfaction ~ SEX, data= clean_data)
```

    ## 
    ##  Descriptive statistics by group 
    ## SEX: Female
    ##                   vars    n mean   sd median trimmed  mad min max range  skew
    ## Life_satisfaction    1 3749 3.08 0.98    3.2     3.1 1.19   1   5     4 -0.19
    ##                   kurtosis   se
    ## Life_satisfaction    -0.68 0.02
    ## ------------------------------------------------------------ 
    ## SEX: Male
    ##                   vars    n mean   sd median trimmed  mad min max range  skew
    ## Life_satisfaction    1 3533 3.11 1.01    3.2    3.13 1.19   1   5     4 -0.21
    ##                   kurtosis   se
    ## Life_satisfaction    -0.69 0.02

``` r
describeBy(Life_satisfaction ~ HOUSE_INCOME, data= clean_data)
```

    ## 
    ##  Descriptive statistics by group 
    ## HOUSE_INCOME: High
    ##                   vars   n mean   sd median trimmed  mad min max range  skew
    ## Life_satisfaction    1 355 3.69 0.89    3.8    3.77 0.89   1   5     4 -0.77
    ##                   kurtosis   se
    ## Life_satisfaction     0.35 0.05
    ## ------------------------------------------------------------ 
    ## HOUSE_INCOME: Low
    ##                   vars    n mean   sd median trimmed  mad min max range  skew
    ## Life_satisfaction    1 4970 2.94 0.98      3    2.95 1.19   1   5     4 -0.06
    ##                   kurtosis   se
    ## Life_satisfaction    -0.68 0.01
    ## ------------------------------------------------------------ 
    ## HOUSE_INCOME: Middle
    ##                   vars    n mean   sd median trimmed  mad min max range  skew
    ## Life_satisfaction    1 1957 3.36 0.95    3.6    3.41 0.89   1   5     4 -0.49
    ##                   kurtosis   se
    ## Life_satisfaction    -0.38 0.02

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
#clean_data %>%
 # group_by(SEX) %>% 
  #summarize(W = shapiro.test(HAPPY_REV)$statistic, p_value = shapiro.test(HAPPY_REV)$p.value)

#clean_data %>%
 # group_by(HOUSE_INCOME) %>% 
  #summarize(W = shapiro.test(HAPPY_REV)$statistic, p_value = shapiro.test(HAPPY_REV)$p.value)

describeBy(HAPPY_REV ~ SEX, data= clean_data)
```

    ## 
    ##  Descriptive statistics by group 
    ## SEX: Female
    ##           vars    n mean   sd median trimmed mad min max range  skew kurtosis
    ## HAPPY_REV    1 3749 3.01 0.72      3    3.04   0   1   4     3 -0.46     0.22
    ##             se
    ## HAPPY_REV 0.01
    ## ------------------------------------------------------------ 
    ## SEX: Male
    ##           vars    n mean   sd median trimmed mad min max range  skew kurtosis
    ## HAPPY_REV    1 3533 3.05 0.74      3    3.09   0   1   4     3 -0.44    -0.09
    ##             se
    ## HAPPY_REV 0.01

``` r
describeBy(HAPPY_REV ~ HOUSE_INCOME, data= clean_data)
```

    ## 
    ##  Descriptive statistics by group 
    ## HOUSE_INCOME: High
    ##           vars   n mean   sd median trimmed mad min max range  skew kurtosis
    ## HAPPY_REV    1 355 3.41 0.67      4    3.51   0   1   4     3 -0.87     0.31
    ##             se
    ## HAPPY_REV 0.04
    ## ------------------------------------------------------------ 
    ## HOUSE_INCOME: Low
    ##           vars    n mean   sd median trimmed mad min max range  skew kurtosis
    ## HAPPY_REV    1 4970 2.95 0.74      3    2.98   0   1   4     3 -0.41     0.01
    ##             se
    ## HAPPY_REV 0.01
    ## ------------------------------------------------------------ 
    ## HOUSE_INCOME: Middle
    ##           vars    n mean   sd median trimmed mad min max range  skew kurtosis
    ## HAPPY_REV    1 1957 3.15 0.67      3     3.2   0   1   4     3 -0.44     0.21
    ##             se
    ## HAPPY_REV 0.02

Variance of Lonely

``` r
clean_data %>%
  group_by(SEX) %>%
  summarize(variacne = var(Lonely))
```

    ## # A tibble: 2 × 2
    ##   SEX    variacne
    ##   <chr>     <dbl>
    ## 1 Female    0.398
    ## 2 Male      0.422

``` r
leveneTest(Lonely~SEX, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    1  2.4308  0.119
    ##       7280

``` r
clean_data %>%
  group_by(HOUSE_INCOME) %>%
  summarize(variacne = var(Lonely))
```

    ## # A tibble: 3 × 2
    ##   HOUSE_INCOME variacne
    ##   <chr>           <dbl>
    ## 1 High            0.340
    ## 2 Low             0.414
    ## 3 Middle          0.369

``` r
leveneTest(Lonely~HOUSE_INCOME, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2  9.4275 8.147e-05 ***
    ##       7279                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Variance of Life satisfaction

``` r
clean_data %>%
  group_by(SEX) %>%
  summarize(variacne = var(Life_satisfaction))
```

    ## # A tibble: 2 × 2
    ##   SEX    variacne
    ##   <chr>     <dbl>
    ## 1 Female    0.953
    ## 2 Male      1.02

``` r
leveneTest(Life_satisfaction~SEX, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value  Pr(>F)  
    ## group    1  3.3263 0.06822 .
    ##       7280                  
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
    ## 1 High            0.800
    ## 2 Low             0.957
    ## 3 Middle          0.894

``` r
leveneTest(Life_satisfaction~HOUSE_INCOME, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2  9.0104 0.0001235 ***
    ##       7279                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Variance of Happiness

``` r
clean_data %>%
  group_by(SEX) %>%
  summarize(variacne = var(HAPPY_REV))
```

    ## # A tibble: 2 × 2
    ##   SEX    variacne
    ##   <chr>     <dbl>
    ## 1 Female    0.515
    ## 2 Male      0.549

``` r
leveneTest(HAPPY_REV~SEX, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value   Pr(>F)   
    ## group    1  10.317 0.001324 **
    ##       7280                    
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
    ## 1 High            0.800
    ## 2 Low             0.957
    ## 3 Middle          0.894

``` r
leveneTest(HAPPY_REV~HOUSE_INCOME, clean_data)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value    Pr(>F)    
    ## group    2  11.011 1.679e-05 ***
    ##       7279                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
