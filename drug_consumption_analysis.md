Drug_Consumption_Analysis
================
2025-12-02

# Data Processing

``` r
data_one <- read.csv("data/Drug_consumption.csv")
data_one <- data_one |> select(-Semer)

data_two <- data_one |> pivot_longer(
  cols = Alcohol:VSA,
  names_to = "drug_type",
  values_to = "consumption_level"
)

data_two <- data_two |> mutate(consumption_binary = case_when(
  consumption_level == "CL6" | consumption_level == "CL5" | consumption_level == "CL4" | consumption_level == "CL3" |
    consumption_level == "CL2" ~ "User",
  consumption_level == "CL1" | consumption_level == "CL0" ~ "Non-user"
))

data_three <- data_two |> group_by(ID) |>
  summarise(user_status = sum(consumption_binary == "User"))

data_two <- data_two |> mutate(consumption_legal = case_when(
  drug_type == "Alcohol" | drug_type == "Caff" | drug_type == "Nicotine" |
                          drug_type == "Choc" | drug_type == "Amyl" | drug_type == "Legalh" ~ "legal",
  drug_type == "Benzos" | drug_type == "Amphet" | drug_type == "Cannabis" |
                             drug_type == "Coke" | drug_type == "Crack" | drug_type == "Ecstasy" |
                             drug_type == "Heroin" | drug_type == "Ketamine" | drug_type == "LSD" |
                             drug_type == "Meth" | drug_type == "Mushrooms" | drug_type == "VSA" ~ "illegal"
))

data_one <- left_join(data_one, data_three)
```

    ## Joining with `by = join_by(ID)`

``` r
data_one$user_status_real = factor(data_one$user_status, ordered = TRUE, levels = c(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
))

data_one_prime <- read.csv("data/cleaned_drug_data.csv")
data_one_prime <- data_one_prime |> pivot_longer(
  cols = Alcohol:VSA,
  names_to = "drug_type",
  values_to = "consumption_level"
)

data_one_prime_user_status <- data_one_prime |> group_by(ID) |>
  summarise(user_consumption_level = sum(consumption_level))

data_one <- left_join(data_one, data_one_prime_user_status)
```

    ## Joining with `by = join_by(ID)`

``` r
data_one_prime_prime <- data_one_prime |> pivot_wider(
  names_from = drug_type,
  values_from = consumption_level
)

data_one_prime_prime <- left_join(data_one_prime_prime, data_one_prime_user_status)
```

    ## Joining with `by = join_by(ID)`

``` r
data_one_prime_use_no_use <-
  data_one_prime_prime |> mutate(across(Alcohol:VSA, ~case_when(
    . == 0 | . == 1 ~ "Not User",
    . == 2 | . == 3 | . == 4 | . == 5 | . == 6 ~ "User"
  ), .names = "{.col}_binary"))
```

# Drug Consumption and Drug Type

To analyze drug consumption for each user, we converted all ordinal
measures for each drug into numeric variables. Then, they were added
together to calculate the total drug consumption level. A binary
variable was also created for each drug to detail whether each
participant was a user of that drug or not. For example, if a
participant had an 0 or 1 level of consumption for alcohol, that person
would be classified as a non-user of alcohol. However, for consumption
levels of 2 or higher, that participant would be classified as a user.
Finally, a variable of user-status was created to measure how many drugs
a participant was using, legal and illegal.

    ## `summarise()` has grouped output by 'consumption_legal'. You can override using
    ## the `.groups` argument.

|           | Statistic          |  Value |
|:----------|:-------------------|-------:|
| X-squared | Chi-squared        | 8709.9 |
| df        | Degrees of Freedom |    6.0 |
|           | P-Value            |    0.0 |

![](drug_consumption_analysis_files/figure-gfm/pressure-1.png)<!-- -->

To see whether the type of drug–legal or illegal–a person takes has any
relationship to the consumption level of said drug, we conducted a
chi-squared test for the two categorical variables. The results of the
test, with a p-value of close to 0, suggests a significant association.

To examine the direction of this relationship, 2 bar graphs–for both
legal and illegal drugs–was constructed to compare the expected
frequency and the observed frequency for all consumption levels. For
illegal drugs, the observed counts for consumption levels 0, 1, 2, and 3
are all higher than the expected frequency. Meanwhile, the observed
counts are all much lower than the expected counts for consumption
levels 4, 5, and 6.

By contrast, for legal drugs, the observed counts for consumption levels
0, 1, 2, and 3 are all lower than the expected frequency. Meanwhile, the
observed counts are all much higher than the expected counts for
consumption levels 4, 5, and 6.

These results indicate that consumption levels are generally higher for
legal drugs compared to illegal drugs.

``` r
plot_histogram_1 <- data_one |>
  ggplot(aes(x = as.numeric(user_consumption_level))) +
  geom_histogram(bins = 50)

plot_histogram_2 <- data_one |>
  ggplot(aes(x = log1p(as.numeric(user_consumption_level)))) +
  geom_histogram(bins = 50)

plot_histogram_patch <- plot_histogram_1 + plot_histogram_2
plot_histogram_patch
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range (`stat_bin()`).
    ## Removed 8 rows containing non-finite outside the scale range (`stat_bin()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Along this direction, we wanted to examine whether total drug
consumption for a participant can be predicted by examining the types of
drugs a person uses.

To predict this model, we first examined the distribution of the drug
consumption level.However, this distribution was heavily skewed to the
right. Thus, by transforming the variable into the log of total drug
consumption, we are able to get a much more approximately normal
distribution, suited for linear regression.

Predictors were the binary variables that detail whether a participant
used or didn’t use said drug.

``` r
model_user_binary_consumption <- lm(log1p(user_consumption_level) ~ Alcohol_binary + Amphet_binary + Amyl_binary +
                                      Benzos_binary + Caff_binary + Cannabis_binary + Choc_binary + Coke_binary + 
                                      Crack_binary + Ecstasy_binary + Heroin_binary + Ketamine_binary +
                                      Legalh_binary + LSD_binary + Meth_binary + Mushrooms_binary + Nicotine_binary +
                                      VSA_binary, data_one_prime_use_no_use)
model_user_binary_consumption |> summary() |> broom::glance() |>
  mutate(model = c("Drug Type Consumption")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of OCEAN Scores from User Consumption"
    , col.names = c(
        "Model", "R-squared", "Adj. R-squared"
      , "Sigma", "F-statistic", "p-value", "df", "Residual df", "N"
    )
    , digits    = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Key Statistics for Prediction of OCEAN Scores from User Consumption
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:right;">

R-squared
</th>

<th style="text-align:right;">

Adj. R-squared
</th>

<th style="text-align:right;">

Sigma
</th>

<th style="text-align:right;">

F-statistic
</th>

<th style="text-align:right;">

p-value
</th>

<th style="text-align:right;">

df
</th>

<th style="text-align:right;">

Residual df
</th>

<th style="text-align:right;">

N
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Drug Type Consumption
</td>

<td style="text-align:right;">

0.883
</td>

<td style="text-align:right;">

0.882
</td>

<td style="text-align:right;">

0.16
</td>

<td style="text-align:right;">

778.647
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

18
</td>

<td style="text-align:right;">

1857
</td>

<td style="text-align:right;">

1876
</td>

</tr>

</tbody>

</table>

From the summary of our model, the adjusted R-squared value is equal to
0.882. This suggests a good fit and that the total drug consumption
level can be approximately modeled from which drugs a person uses. The
estimated parameters of this fit are also shown in the following table.

``` r
model_user_binary_consumption |> summary() |>
  broom::tidy() |>
  kbl(
      caption     = "Types of Drugs Consumed on Drug Consumption Level"
    , col.names   = c("Predictor", "Estimate", "SE", "t-statistic", "p-value")
    , digits      = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Types of Drugs Consumed on Drug Consumption Level
</caption>

<thead>

<tr>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)
</td>

<td style="text-align:right;">

1.965
</td>

<td style="text-align:right;">

0.038
</td>

<td style="text-align:right;">

51.217
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Alcohol_binaryUser
</td>

<td style="text-align:right;">

0.207
</td>

<td style="text-align:right;">

0.020
</td>

<td style="text-align:right;">

10.139
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Amphet_binaryUser
</td>

<td style="text-align:right;">

0.085
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

7.670
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Amyl_binaryUser
</td>

<td style="text-align:right;">

0.031
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

2.941
</td>

<td style="text-align:right;">

0.003
</td>

</tr>

<tr>

<td style="text-align:left;">

Benzos_binaryUser
</td>

<td style="text-align:right;">

0.127
</td>

<td style="text-align:right;">

0.009
</td>

<td style="text-align:right;">

13.484
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Caff_binaryUser
</td>

<td style="text-align:right;">

0.430
</td>

<td style="text-align:right;">

0.027
</td>

<td style="text-align:right;">

15.714
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Cannabis_binaryUser
</td>

<td style="text-align:right;">

0.208
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

19.036
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Choc_binaryUser
</td>

<td style="text-align:right;">

0.299
</td>

<td style="text-align:right;">

0.028
</td>

<td style="text-align:right;">

10.678
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Coke_binaryUser
</td>

<td style="text-align:right;">

0.086
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

7.580
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Crack_binaryUser
</td>

<td style="text-align:right;">

0.033
</td>

<td style="text-align:right;">

0.015
</td>

<td style="text-align:right;">

2.249
</td>

<td style="text-align:right;">

0.025
</td>

</tr>

<tr>

<td style="text-align:left;">

Ecstasy_binaryUser
</td>

<td style="text-align:right;">

0.092
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

7.448
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Heroin_binaryUser
</td>

<td style="text-align:right;">

0.053
</td>

<td style="text-align:right;">

0.015
</td>

<td style="text-align:right;">

3.458
</td>

<td style="text-align:right;">

0.001
</td>

</tr>

<tr>

<td style="text-align:left;">

Ketamine_binaryUser
</td>

<td style="text-align:right;">

0.058
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

4.934
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Legalh_binaryUser
</td>

<td style="text-align:right;">

0.132
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

12.560
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

LSD_binaryUser
</td>

<td style="text-align:right;">

0.059
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

4.835
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Meth_binaryUser
</td>

<td style="text-align:right;">

0.114
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

10.016
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Mushrooms_binaryUser
</td>

<td style="text-align:right;">

0.079
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

6.724
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Nicotine_binaryUser
</td>

<td style="text-align:right;">

0.167
</td>

<td style="text-align:right;">

0.009
</td>

<td style="text-align:right;">

17.616
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

VSA_binaryUser
</td>

<td style="text-align:right;">

0.040
</td>

<td style="text-align:right;">

0.013
</td>

<td style="text-align:right;">

3.163
</td>

<td style="text-align:right;">

0.002
</td>

</tr>

</tbody>

</table>

To interpret these parameters, we can examine the relationship between
drug consumption level and a participant’s use of meth. If a participant
uses meth, then said user would have a log of total drug consumption
which is 0.114 higher than those who do not use meth. From statistical
analysis, since all the predictors have a p-value of approximately 0, we
can conclude that all these predictors have a significant association
with the log of total drug consumption.

# Intervention for Drug Consumption

After obtaining user drug consumption level, it is important to analyze
possible areas for treatment and rehabilitation. To that end, we
analyzed whether there was a significant association between total
consumption level and all OCEAN personality traits, impulsivity and
sensation-seeking. This way, it may be possible to predict and narrow
down personality traits that could use improvement.

From the histograms fo OCEAN scores, impulsivity and sensation-seeking,
we see that all distributions are approximately normal as to not
requiring any further variable transformation. The plots that
demonstrate total user consumption vs OCEAN scores–and its fit–are
displayed below. A table showing each model’s key statistics–including
R-squared and p-values–are also shown below.

``` r
model_user_Oscore <- lm(Oscore~user_consumption_level, data = data_one_prime_use_no_use)
plot_1 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, Oscore)) +
  geom_point(color = "red") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_user_Cscore <- lm(Cscore~user_consumption_level, data = data_one_prime_use_no_use)
plot_2 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, Cscore)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_user_Escore <- lm(Escore~user_consumption_level, data = data_one_prime_use_no_use)
plot_3 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, Escore)) +
  geom_point(color = "yellow") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_user_Ascore <- lm(Ascore~user_consumption_level, data = data_one_prime_use_no_use)
plot_4 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, Ascore)) +
  geom_point(color = "orange") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_user_Nscore <- lm(Nscore~user_consumption_level, data = data_one_prime_use_no_use)
plot_5 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, Nscore)) +
  geom_point(color = "purple") +
  geom_smooth(method="lm", se=TRUE, color="blue")

plot_patch <- (plot_1 + plot_2 + plot_3)/(plot_4 + plot_5)
plot_patch
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
model_user_Oscore |> summary() |> broom::glance() |>
  bind_rows(summary(model_user_Cscore) |> broom::glance()) |>
  bind_rows(summary(model_user_Escore) |> broom::glance()) |>
  bind_rows(summary(model_user_Ascore) |> broom::glance()) |>
  bind_rows(summary(model_user_Nscore) |> broom::glance()) |>
  mutate(model = c("O score model", "C score model", "E score model", 
                   "A score model", "N score model")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of OCEAN Scores from User Consumption"
    , col.names = c(
        "Model", "R-squared", "Adj. R-squared"
      , "Sigma", "F-statistic", "p-value", "df", "Residual df", "N"
    )
    , digits    = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Key Statistics for Prediction of OCEAN Scores from User Consumption
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:right;">

R-squared
</th>

<th style="text-align:right;">

Adj. R-squared
</th>

<th style="text-align:right;">

Sigma
</th>

<th style="text-align:right;">

F-statistic
</th>

<th style="text-align:right;">

p-value
</th>

<th style="text-align:right;">

df
</th>

<th style="text-align:right;">

Residual df
</th>

<th style="text-align:right;">

N
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

O score model
</td>

<td style="text-align:right;">

0.141
</td>

<td style="text-align:right;">

0.141
</td>

<td style="text-align:right;">

0.923
</td>

<td style="text-align:right;">

307.546
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

C score model
</td>

<td style="text-align:right;">

0.100
</td>

<td style="text-align:right;">

0.100
</td>

<td style="text-align:right;">

0.947
</td>

<td style="text-align:right;">

209.316
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

E score model
</td>

<td style="text-align:right;">

0.000
</td>

<td style="text-align:right;">

0.000
</td>

<td style="text-align:right;">

0.998
</td>

<td style="text-align:right;">

0.840
</td>

<td style="text-align:right;">

0.36
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

A score model
</td>

<td style="text-align:right;">

0.044
</td>

<td style="text-align:right;">

0.043
</td>

<td style="text-align:right;">

0.975
</td>

<td style="text-align:right;">

85.505
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

N score model
</td>

<td style="text-align:right;">

0.034
</td>

<td style="text-align:right;">

0.033
</td>

<td style="text-align:right;">

0.982
</td>

<td style="text-align:right;">

65.558
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

</tbody>

</table>

The only personality trait that doesn’t have a significant association
with the total drug use consumption is Extraversion, which describes how
outgoing and sociable the subject is. This is concluded due to the model
having a p-value of 0.36, which is larger than a significance p-value of
0.05.

We also attempted to examine if there is a significant association
between the total drug use consumption and impulsive and sensation
seeking via. linear regression. Below, their association is plotted and
key statistics for the fit models–such as the R squared values– are
shown.

Unlike OCEAN scores, by examining our p-values, we can conclude that
total user drug consumption has a significant association with both
impulsivity and sensation-seeking.

``` r
model_user_Impulsivity <- lm(Impulsive~user_consumption_level, data = data_one_prime_use_no_use)
plot_6 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, Impulsive)) +
  geom_point(color = "cyan") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_user_SS<- lm(SS~user_consumption_level, data = data_one_prime_use_no_use)
plot_7 <- ggplot(data_one_prime_use_no_use, aes(user_consumption_level, SS)) +
  geom_point(color = "green") +
  geom_smooth(method="lm", se=TRUE, color="blue")

plot_patch_2 = plot_6 + plot_7
plot_patch_2
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
model_user_Impulsivity |> summary() |> broom::glance() |>
  bind_rows(summary(model_user_SS) |> broom::glance()) |>
  mutate(model = c("Impulsivity model", "Sensation Seeking model")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of Impulsivity and SS Scores from User Consumption"
    , col.names = c(
        "Model", "R-squared", "Adj. R-squared"
      , "Sigma", "F-statistic", "p-value", "df", "Residual df", "N"
    )
    , digits    = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Key Statistics for Prediction of Impulsivity and SS Scores from User
Consumption
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:right;">

R-squared
</th>

<th style="text-align:right;">

Adj. R-squared
</th>

<th style="text-align:right;">

Sigma
</th>

<th style="text-align:right;">

F-statistic
</th>

<th style="text-align:right;">

p-value
</th>

<th style="text-align:right;">

df
</th>

<th style="text-align:right;">

Residual df
</th>

<th style="text-align:right;">

N
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Impulsivity model
</td>

<td style="text-align:right;">

0.137
</td>

<td style="text-align:right;">

0.137
</td>

<td style="text-align:right;">

0.887
</td>

<td style="text-align:right;">

298.306
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

Sensation Seeking model
</td>

<td style="text-align:right;">

0.248
</td>

<td style="text-align:right;">

0.247
</td>

<td style="text-align:right;">

0.835
</td>

<td style="text-align:right;">

616.962
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

1
</td>

<td style="text-align:right;">

1874
</td>

<td style="text-align:right;">

1876
</td>

</tr>

</tbody>

</table>

Finally, all parametric estimates for all significant models are shown
in the table below.

``` r
model_user_Oscore |> summary() |> broom::tidy() |> filter(term %in% c("user_consumption_level")) |>
  bind_rows(summary(model_user_Cscore) |> broom::tidy() |> filter(term %in% c("user_consumption_level"))) |>
  bind_rows(summary(model_user_Ascore) |> broom::tidy() |> filter(term %in% c("user_consumption_level"))) |>
  bind_rows(summary(model_user_Nscore) |> broom::tidy() |> filter(term %in% c("user_consumption_level"))) |>
  bind_rows(summary(model_user_Impulsivity) |> broom::tidy() |> filter(term %in% c("user_consumption_level"))) |>
  bind_rows(summary(model_user_SS) |> broom::tidy() |> filter(term %in% c("user_consumption_level"))) |>
  mutate(model = c("Consumption on O score", "Consumption on C score",
                   "Consumption on A score", "Consumption on N score", "Consumption on Impulsivity",
                   "Consumption on Sensation Seeking")) |>
  relocate(model) |>
  kbl(
      caption     = "Effect of Psychological Predictors on Drug Consumption Levels"
    , col.names   = c("Model", "Predictor", "Estimate", "SE", "t-statistic", "p-value")
    , digits      = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Effect of Psychological Predictors on Drug Consumption Levels
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Consumption on O score
</td>

<td style="text-align:left;">

user_consumption_level
</td>

<td style="text-align:right;">

0.025
</td>

<td style="text-align:right;">

0.001
</td>

<td style="text-align:right;">

17.537
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

Consumption on C score
</td>

<td style="text-align:left;">

user_consumption_level
</td>

<td style="text-align:right;">

-0.021
</td>

<td style="text-align:right;">

0.001
</td>

<td style="text-align:right;">

-14.468
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

Consumption on A score
</td>

<td style="text-align:left;">

user_consumption_level
</td>

<td style="text-align:right;">

-0.014
</td>

<td style="text-align:right;">

0.002
</td>

<td style="text-align:right;">

-9.247
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

Consumption on N score
</td>

<td style="text-align:left;">

user_consumption_level
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

0.002
</td>

<td style="text-align:right;">

8.097
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

Consumption on Impulsivity
</td>

<td style="text-align:left;">

user_consumption_level
</td>

<td style="text-align:right;">

0.024
</td>

<td style="text-align:right;">

0.001
</td>

<td style="text-align:right;">

17.272
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

Consumption on Sensation Seeking
</td>

<td style="text-align:left;">

user_consumption_level
</td>

<td style="text-align:right;">

0.032
</td>

<td style="text-align:right;">

0.001
</td>

<td style="text-align:right;">

24.839
</td>

<td style="text-align:right;">

0
</td>

</tr>

</tbody>

</table>

The table of estimates predict that with higher total drug consumption
level, the user isle likely to be more curious (O score), less organized
and conscientious (C score), less agreeable (A score), more prone to
negative emotions and anxiety (N score), and more impulsive and
sensation-seeking. However, for all models, their adjusted R-squared
values suggest that total user drug consumption only accounts for a
small portion across all personality traits.

# Drug Consumption Risk Assessment

``` r
data_Escore_1 = data_one |> dplyr::select(user_consumption_level, Escore)

plot_Escore <- ggplot(data_Escore_1, aes(Escore, log1p(user_consumption_level))) +
  geom_point(color = "yellow") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_Escore <- lm(log1p(user_consumption_level) ~ Escore, data = data_Escore_1)


data_Oscore_1 = data_one |> dplyr::select(user_consumption_level, Oscore)

plot_Oscore <- ggplot(data_Oscore_1, aes(Oscore, log1p(user_consumption_level))) +
  geom_point(color = "red") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_Oscore <- lm(log1p(user_consumption_level) ~ Oscore, data = data_Oscore_1)


data_Ascore_1 = data_one |> dplyr::select(user_consumption_level, AScore)

plot_Ascore <- ggplot(data_Ascore_1, aes(AScore, log1p(user_consumption_level))) +
  geom_point(color = "orange") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_Ascore <- lm(log1p(user_consumption_level) ~ AScore, data = data_Ascore_1)

data_Nscore_1 = data_one |> dplyr::select(user_consumption_level, Nscore)

plot_Nscore <- ggplot(data_Nscore_1, aes(Nscore, log1p(user_consumption_level))) +
  geom_point(color = "purple") +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_Nscore <- lm(log1p(user_consumption_level) ~ Nscore, data = data_Nscore_1)

data_Cscore_1 = data_one |> dplyr::select(user_consumption_level, Cscore)

plot_Cscore <- ggplot(data_Cscore_1, aes(Cscore, log1p(user_consumption_level))) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE, color="blue")

model_Cscore <- lm(log1p(user_consumption_level) ~ Cscore, data = data_Cscore_1)


model_Oscore |> summary() |> broom::tidy() |>
  bind_rows(summary(model_Cscore) |> broom::tidy()) |>
  bind_rows(summary(model_Escore) |> broom::tidy()) |>
  bind_rows(summary(model_Ascore) |> broom::tidy()) |>
  bind_rows(summary(model_Nscore) |> broom::tidy()) |>
  filter(term != "(Intercept)") |>
  filter(p.value < 0.05) |>
  mutate(model = c("O model", "C model", 
                   "A model", "N model")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of Impulsivity and SS Scores from User Consumption"
    , col.names = c(
        "Model", "Predictor", "Estimate", "SE", "t-statistic", "p-value"
    )
    , digits    = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Key Statistics for Prediction of Impulsivity and SS Scores from User
Consumption
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

O model
</td>

<td style="text-align:left;">

Oscore
</td>

<td style="text-align:right;">

0.177
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

17.718
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

C model
</td>

<td style="text-align:left;">

Cscore
</td>

<td style="text-align:right;">

-0.154
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

-15.187
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

A model
</td>

<td style="text-align:left;">

AScore
</td>

<td style="text-align:right;">

-0.094
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

-8.910
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

N model
</td>

<td style="text-align:left;">

Nscore
</td>

<td style="text-align:right;">

0.083
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

7.882
</td>

<td style="text-align:right;">

0
</td>

</tr>

</tbody>

</table>

``` r
plot_patch_3 <- (plot_Oscore + plot_Cscore + plot_Escore)/(plot_Ascore + plot_Nscore)
plot_patch_3
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Again, across all the OCEAN personality traits, the significant
associations suggest that people who are more curious and entertain new
ideas (O score), less self-disciplined and diligent (C score), less
likely to be cooperative or kind (A score), and more likely to
experience emotions like anxiety or sadness (N score) may be more at
risk in regards to total drug consumption, both legal and illegal.

``` r
data_Impulsivity_1 = data_one |> dplyr::select(user_consumption_level, Impulsive)

plot_impulsivity <- ggplot(data_Impulsivity_1, aes(Impulsive, log1p(user_consumption_level))) + 
  geom_point(color = "cyan") +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

model_Impulsivity <- lm(log1p(user_consumption_level) ~ Impulsive, data = data_Impulsivity_1)

data_SS_1 = data_one |> dplyr::select(user_consumption_level, SS)

plot_SS <- ggplot(data_SS_1, aes(SS, log1p(user_consumption_level))) + 
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

model_SS <- lm(log1p(user_consumption_level) ~ SS, data = data_SS_1)

plot_patch_4 <- plot_impulsivity + plot_SS
plot_patch_4
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
model_Impulsivity |> summary() |> broom::tidy() |>
  bind_rows(summary(model_SS) |> broom::tidy()) |>
  mutate(model = c("Impulsivity model", "Impulsivity model", "SS model", "SS model")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of Impulsivity and SS Scores from User Consumption"
    , col.names = c(
        "Model", "Predictor", "Estimate", "SE", "t-statistic", "p-value"
    )
    , digits    = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Key Statistics for Prediction of Impulsivity and SS Scores from User
Consumption
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Impulsivity model
</td>

<td style="text-align:left;">

(Intercept)
</td>

<td style="text-align:right;">

3.435
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

342.844
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

Impulsivity model
</td>

<td style="text-align:left;">

Impulsive
</td>

<td style="text-align:right;">

0.176
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

16.753
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

SS model
</td>

<td style="text-align:left;">

(Intercept)
</td>

<td style="text-align:right;">

3.437
</td>

<td style="text-align:right;">

0.009
</td>

<td style="text-align:right;">

367.344
</td>

<td style="text-align:right;">

0
</td>

</tr>

<tr>

<td style="text-align:left;">

SS model
</td>

<td style="text-align:left;">

SS
</td>

<td style="text-align:right;">

0.238
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

24.412
</td>

<td style="text-align:right;">

0
</td>

</tr>

</tbody>

</table>

Likewise, the significant associations suggest that people who are more
impulsive and sensation-seeking may be more at risk in regards to total
drug consumption, both legal and illegal.

To take all of these personality traits into account for total drug
consumption, we developed 2 models. One model uses linear regression,
but doesn’t take any of the interactions between OCEAN trait scores,
impulsivity, and sensation seeking into account. The second model takes
all possible interactions into account.

The first table below displays the estimates of our personality trait
predictors without any interaction. It is notable that–in this
model–impulsivity has a p-value of 0.894, suggesting that the effects it
has on total drug consumption overlaps with other personality traits.

The second table is the results of comparing our two models through the
AIC test. Since the model with interaction between the personality
traits has a lower AIC score, it is tempting to conclude that taking
interaction into account is the better option.

For further testing, we compared key statistics such as R-squared
between the two models. Indeed, we observe that the interaction model
accounts for more of the variation in total drug consumption level
compared to the model with no interaction.

``` r
model_no_interaction_psychology <- lm(log1p(user_consumption_level) ~ Oscore + Cscore +
                                 AScore + Nscore + Impulsive + SS, data = data_one)

model_interaction_psychology <- lm(log1p(user_consumption_level) ~ (Oscore + Cscore +
                                 AScore + Nscore + Impulsive + SS)^6, data = data_one)

model_no_interaction_psychology |> summary() |>
  broom::tidy() |>
  kbl(
      caption     = "Effect of Psychological Predictors on Drug Consumption Levels"
    , col.names   = c("Predictor", "Estimate", "SE", "t-statistic", "p-value")
    , digits      = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Effect of Psychological Predictors on Drug Consumption Levels
</caption>

<thead>

<tr>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)
</td>

<td style="text-align:right;">

3.437
</td>

<td style="text-align:right;">

0.009
</td>

<td style="text-align:right;">

393.577
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Oscore
</td>

<td style="text-align:right;">

0.109
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

11.138
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Cscore
</td>

<td style="text-align:right;">

-0.094
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

-9.333
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

AScore
</td>

<td style="text-align:right;">

-0.038
</td>

<td style="text-align:right;">

0.009
</td>

<td style="text-align:right;">

-4.026
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Nscore
</td>

<td style="text-align:right;">

0.026
</td>

<td style="text-align:right;">

0.010
</td>

<td style="text-align:right;">

2.695
</td>

<td style="text-align:right;">

0.007
</td>

</tr>

<tr>

<td style="text-align:left;">

Impulsive
</td>

<td style="text-align:right;">

-0.002
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

-0.134
</td>

<td style="text-align:right;">

0.894
</td>

</tr>

<tr>

<td style="text-align:left;">

SS
</td>

<td style="text-align:right;">

0.159
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

12.770
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

</tbody>

</table>

``` r
knitr::kable(AIC(model_no_interaction_psychology, model_interaction_psychology))
```

|                                 |  df |      AIC |
|:--------------------------------|----:|---------:|
| model_no_interaction_psychology |   8 | 1684.909 |
| model_interaction_psychology    |  65 | 1646.347 |

``` r
model_no_interaction_psychology |> summary() |> broom::glance() |>
  bind_rows(summary(model_interaction_psychology) |> broom::glance()) |>
  mutate(model = c("Psychology Model No Interaction", "Psychology Model with Interaction")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of User Consumption from Psychological Traits"
    , col.names = c(
        "Model", "R-squared", "Adj. R-squared"
      , "Sigma", "F-statistic", "p-value", "df", "Residual df", "N"
    )
    , digits    = c(1, 2, 2, 0, 2, 5, 0, 0, 0)
  )
```

<table>

<caption>

Key Statistics for Prediction of User Consumption from Psychological
Traits
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:right;">

R-squared
</th>

<th style="text-align:right;">

Adj. R-squared
</th>

<th style="text-align:right;">

Sigma
</th>

<th style="text-align:right;">

F-statistic
</th>

<th style="text-align:right;">

p-value
</th>

<th style="text-align:right;">

df
</th>

<th style="text-align:right;">

Residual df
</th>

<th style="text-align:right;">

N
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Psychology Model No Interaction
</td>

<td style="text-align:right;">

0.34
</td>

<td style="text-align:right;">

0.34
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

161.20
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

6
</td>

<td style="text-align:right;">

1869
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

Psychology Model with Interaction
</td>

<td style="text-align:right;">

0.39
</td>

<td style="text-align:right;">

0.37
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

18.58
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

63
</td>

<td style="text-align:right;">

1812
</td>

<td style="text-align:right;">

1876
</td>

</tr>

</tbody>

</table>

``` r
model_interaction_psychology |> summary() |>
  broom::tidy() |>
  filter(p.value < 0.05) |>
  filter(term %in% c("Oscore", "Cscore", "AScore", "Nscore", "SS")) |>
  kbl(
      caption     = "Effect of Psychological Predictors on Drug Consumption Levels", 
      col.names   = c("Predictor", "Estimate", "SE", "t-statistic", "p-value"), 
      digits      = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Effect of Psychological Predictors on Drug Consumption Levels
</caption>

<thead>

<tr>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Oscore
</td>

<td style="text-align:right;">

0.120
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

10.015
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Cscore
</td>

<td style="text-align:right;">

-0.115
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

-9.212
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

AScore
</td>

<td style="text-align:right;">

-0.058
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

-4.868
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Nscore
</td>

<td style="text-align:right;">

0.031
</td>

<td style="text-align:right;">

0.012
</td>

<td style="text-align:right;">

2.558
</td>

<td style="text-align:right;">

0.011
</td>

</tr>

<tr>

<td style="text-align:left;">

SS
</td>

<td style="text-align:right;">

0.173
</td>

<td style="text-align:right;">

0.014
</td>

<td style="text-align:right;">

12.453
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

</tbody>

</table>

Going forward, we will employ the linear model of total drug consumption
vs O score, C score, A score, N score, and sensation-seeking with
interaction as we take more variables into account. The parametric
estimates for each significant personality trait predictor is shown in
the table above.

To answer the original research question, while there is a significant
association between a person’s drug consumption level and their
personality traits, only 37 percent of this variation is accounted by
the personality traits. Thus, there is likely other factors that
contribute to a person’s consumption of drugs, legal and illegal.

To summarize the direction of the correlation between all of our
relevant variables–including OCEAN score, impulsivity,
sensation-seeking, and log of user drug consumption level– is displayed
in the following correlation heatmap.

``` r
data_one_transformed <- data_one |> mutate(log_user_consumption_level = log1p(user_consumption_level))

data_correlation = data_one_transformed |> select(Nscore, Escore, Oscore, AScore, Cscore, 
                                             Impulsive, SS, log_user_consumption_level) |>
  na.omit(log_user_consumption_level)

corr_mat <- round(cor(data_correlation), 3)
head(corr_mat)
```

    ##           Nscore Escore Oscore AScore Cscore Impulsive     SS
    ## Nscore     1.000 -0.431  0.008 -0.218 -0.392     0.173  0.078
    ## Escore    -0.431  1.000  0.245  0.158  0.308     0.114  0.208
    ## Oscore     0.008  0.245  1.000  0.037 -0.059     0.275  0.419
    ## AScore    -0.218  0.158  0.037  1.000  0.245    -0.230 -0.209
    ## Cscore    -0.392  0.308 -0.059  0.245  1.000    -0.336 -0.230
    ## Impulsive  0.173  0.114  0.275 -0.230 -0.336     1.000  0.623
    ##           log_user_consumption_level
    ## Nscore                         0.179
    ## Escore                        -0.032
    ## Oscore                         0.379
    ## AScore                        -0.202
    ## Cscore                        -0.331
    ## Impulsive                      0.361

``` r
corr_mat[upper.tri(corr_mat)] <- NA

melted_corr_mat <- melt(corr_mat)

p <- ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var1, Var2, label = value, color = "black", size = 4))

ggplotly(p)
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-d2ff40441300a9803138" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-d2ff40441300a9803138">{"x":{"data":[{"x":[1,2,3,4,5,6,7,8],"y":[1,2,3,4,5,6,7,8],"z":[[1,0,0.30677847658979734,0.14884696016771487,0.027253668763102711,0.42208245981830883,0.3556953179594689,0.42627533193570927],[null,1,0.47239692522711385,0.41160027952480777,0.51642208245981824,0.38085255066387141,0.44654088050314467,0.27882599580712791],[null,null,1,0.32704402515723269,0.25995807127882598,0.49336128581411598,0.59399021663172602,0.56603773584905659],[null,null,null,1,0.47239692522711385,0.14046121593291402,0.1551362683438155,0.16002795248078264],[null,null,null,null,1,0.066387141858839954,0.14046121593291402,0.069881201956673633],[null,null,null,null,null,1,0.73654786862334032,0.55345911949685533],[null,null,null,null,null,null,1,0.64430468204053104],[null,null,null,null,null,null,null,1]],"text":[["Var1: Nscore<br />Var2: Nscore<br />value:  1.000","Var1: Escore<br />Var2: Nscore<br />value: -0.431","Var1: Oscore<br />Var2: Nscore<br />value:  0.008","Var1: AScore<br />Var2: Nscore<br />value: -0.218","Var1: Cscore<br />Var2: Nscore<br />value: -0.392","Var1: Impulsive<br />Var2: Nscore<br />value:  0.173","Var1: SS<br />Var2: Nscore<br />value:  0.078","Var1: log_user_consumption_level<br />Var2: Nscore<br />value:  0.179"],["Var1: Nscore<br />Var2: Escore<br />value:     NA","Var1: Escore<br />Var2: Escore<br />value:  1.000","Var1: Oscore<br />Var2: Escore<br />value:  0.245","Var1: AScore<br />Var2: Escore<br />value:  0.158","Var1: Cscore<br />Var2: Escore<br />value:  0.308","Var1: Impulsive<br />Var2: Escore<br />value:  0.114","Var1: SS<br />Var2: Escore<br />value:  0.208","Var1: log_user_consumption_level<br />Var2: Escore<br />value: -0.032"],["Var1: Nscore<br />Var2: Oscore<br />value:     NA","Var1: Escore<br />Var2: Oscore<br />value:     NA","Var1: Oscore<br />Var2: Oscore<br />value:  1.000","Var1: AScore<br />Var2: Oscore<br />value:  0.037","Var1: Cscore<br />Var2: Oscore<br />value: -0.059","Var1: Impulsive<br />Var2: Oscore<br />value:  0.275","Var1: SS<br />Var2: Oscore<br />value:  0.419","Var1: log_user_consumption_level<br />Var2: Oscore<br />value:  0.379"],["Var1: Nscore<br />Var2: AScore<br />value:     NA","Var1: Escore<br />Var2: AScore<br />value:     NA","Var1: Oscore<br />Var2: AScore<br />value:     NA","Var1: AScore<br />Var2: AScore<br />value:  1.000","Var1: Cscore<br />Var2: AScore<br />value:  0.245","Var1: Impulsive<br />Var2: AScore<br />value: -0.230","Var1: SS<br />Var2: AScore<br />value: -0.209","Var1: log_user_consumption_level<br />Var2: AScore<br />value: -0.202"],["Var1: Nscore<br />Var2: Cscore<br />value:     NA","Var1: Escore<br />Var2: Cscore<br />value:     NA","Var1: Oscore<br />Var2: Cscore<br />value:     NA","Var1: AScore<br />Var2: Cscore<br />value:     NA","Var1: Cscore<br />Var2: Cscore<br />value:  1.000","Var1: Impulsive<br />Var2: Cscore<br />value: -0.336","Var1: SS<br />Var2: Cscore<br />value: -0.230","Var1: log_user_consumption_level<br />Var2: Cscore<br />value: -0.331"],["Var1: Nscore<br />Var2: Impulsive<br />value:     NA","Var1: Escore<br />Var2: Impulsive<br />value:     NA","Var1: Oscore<br />Var2: Impulsive<br />value:     NA","Var1: AScore<br />Var2: Impulsive<br />value:     NA","Var1: Cscore<br />Var2: Impulsive<br />value:     NA","Var1: Impulsive<br />Var2: Impulsive<br />value:  1.000","Var1: SS<br />Var2: Impulsive<br />value:  0.623","Var1: log_user_consumption_level<br />Var2: Impulsive<br />value:  0.361"],["Var1: Nscore<br />Var2: SS<br />value:     NA","Var1: Escore<br />Var2: SS<br />value:     NA","Var1: Oscore<br />Var2: SS<br />value:     NA","Var1: AScore<br />Var2: SS<br />value:     NA","Var1: Cscore<br />Var2: SS<br />value:     NA","Var1: Impulsive<br />Var2: SS<br />value:     NA","Var1: SS<br />Var2: SS<br />value:  1.000","Var1: log_user_consumption_level<br />Var2: SS<br />value:  0.491"],["Var1: Nscore<br />Var2: log_user_consumption_level<br />value:     NA","Var1: Escore<br />Var2: log_user_consumption_level<br />value:     NA","Var1: Oscore<br />Var2: log_user_consumption_level<br />value:     NA","Var1: AScore<br />Var2: log_user_consumption_level<br />value:     NA","Var1: Cscore<br />Var2: log_user_consumption_level<br />value:     NA","Var1: Impulsive<br />Var2: log_user_consumption_level<br />value:     NA","Var1: SS<br />Var2: log_user_consumption_level<br />value:     NA","Var1: log_user_consumption_level<br />Var2: log_user_consumption_level<br />value:  1.000"]],"colorscale":[[0,"#132B43"],[0.027253668763102711,"#152E47"],[0.066387141858839954,"#17334E"],[0.069881201956673633,"#17334E"],[0.14046121593291402,"#1C3C5A"],[0.14884696016771487,"#1C3D5B"],[0.1551362683438155,"#1C3E5C"],[0.16002795248078264,"#1D3E5D"],[0.25995807127882598,"#234B6E"],[0.27882599580712791,"#244D71"],[0.30677847658979734,"#265176"],[0.32704402515723269,"#275379"],[0.3556953179594689,"#29577E"],[0.38085255066387141,"#2B5A83"],[0.41160027952480777,"#2D5E88"],[0.42208245981830883,"#2E608A"],[0.42627533193570927,"#2E608B"],[0.44654088050314467,"#2F638E"],[0.47239692522711385,"#316693"],[0.49336128581411598,"#326997"],[0.51642208245981824,"#346C9B"],[0.55345911949685533,"#3671A2"],[0.56603773584905659,"#3773A4"],[0.59399021663172602,"#3977A9"],[0.64430468204053104,"#3D7EB2"],[0.73654786862334032,"#438BC4"],[1,"#56B1F7"]],"type":"heatmap","showscale":false,"autocolorscale":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8],"y":[1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8],"text":[1,-0.43099999999999999,0.0080000000000000002,-0.218,-0.39200000000000002,0.17299999999999999,0.078,0.17899999999999999,null,1,0.245,0.158,0.308,0.114,0.20799999999999999,-0.032000000000000001,null,null,1,0.036999999999999998,-0.058999999999999997,0.27500000000000002,0.41899999999999998,0.379,null,null,null,1,0.245,-0.23000000000000001,-0.20899999999999999,-0.20200000000000001,null,null,null,null,1,-0.33600000000000002,-0.23000000000000001,-0.33100000000000002,null,null,null,null,null,1,0.623,0.36099999999999999,null,null,null,null,null,null,1,0.49099999999999999,null,null,null,null,null,null,null,1],"hovertext":["Var1: Nscore<br />Var2: Nscore<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: Escore<br />Var2: Nscore<br />value: -0.431<br />value: -0.431<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: Nscore<br />value:  0.008<br />value:  0.008<br />colour: black<br />size: 4","Var1: AScore<br />Var2: Nscore<br />value: -0.218<br />value: -0.218<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: Nscore<br />value: -0.392<br />value: -0.392<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: Nscore<br />value:  0.173<br />value:  0.173<br />colour: black<br />size: 4","Var1: SS<br />Var2: Nscore<br />value:  0.078<br />value:  0.078<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: Nscore<br />value:  0.179<br />value:  0.179<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: Escore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: Escore<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: Escore<br />value:  0.245<br />value:  0.245<br />colour: black<br />size: 4","Var1: AScore<br />Var2: Escore<br />value:  0.158<br />value:  0.158<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: Escore<br />value:  0.308<br />value:  0.308<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: Escore<br />value:  0.114<br />value:  0.114<br />colour: black<br />size: 4","Var1: SS<br />Var2: Escore<br />value:  0.208<br />value:  0.208<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: Escore<br />value: -0.032<br />value: -0.032<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: Oscore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: Oscore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: Oscore<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: AScore<br />Var2: Oscore<br />value:  0.037<br />value:  0.037<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: Oscore<br />value: -0.059<br />value: -0.059<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: Oscore<br />value:  0.275<br />value:  0.275<br />colour: black<br />size: 4","Var1: SS<br />Var2: Oscore<br />value:  0.419<br />value:  0.419<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: Oscore<br />value:  0.379<br />value:  0.379<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: AScore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: AScore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: AScore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: AScore<br />Var2: AScore<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: AScore<br />value:  0.245<br />value:  0.245<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: AScore<br />value: -0.230<br />value: -0.230<br />colour: black<br />size: 4","Var1: SS<br />Var2: AScore<br />value: -0.209<br />value: -0.209<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: AScore<br />value: -0.202<br />value: -0.202<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: Cscore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: Cscore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: Cscore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: AScore<br />Var2: Cscore<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: Cscore<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: Cscore<br />value: -0.336<br />value: -0.336<br />colour: black<br />size: 4","Var1: SS<br />Var2: Cscore<br />value: -0.230<br />value: -0.230<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: Cscore<br />value: -0.331<br />value: -0.331<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: Impulsive<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: Impulsive<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: Impulsive<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: AScore<br />Var2: Impulsive<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: Impulsive<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: Impulsive<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: SS<br />Var2: Impulsive<br />value:  0.623<br />value:  0.623<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: Impulsive<br />value:  0.361<br />value:  0.361<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: SS<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: SS<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: SS<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: AScore<br />Var2: SS<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: SS<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: SS<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: SS<br />Var2: SS<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: SS<br />value:  0.491<br />value:  0.491<br />colour: black<br />size: 4","Var1: Nscore<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Escore<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Oscore<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: AScore<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Cscore<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: Impulsive<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: SS<br />Var2: log_user_consumption_level<br />value:     NA<br />value:     NA<br />colour: black<br />size: 4","Var1: log_user_consumption_level<br />Var2: log_user_consumption_level<br />value:  1.000<br />value:  1.000<br />colour: black<br />size: 4"],"textfont":{"size":17.142175392501688,"color":"rgba(248,118,109,1)"},"type":"scatter","mode":"text","hoveron":"points","name":"black","legendgroup":"black","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1],"y":[1],"name":"3d3ec5a547205e6edfda69a6d8227320","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"skip","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#132B43"],[0.0033444816053511887,"#132B44"],[0.0066889632107023393,"#132C44"],[0.010033444816053528,"#142C45"],[0.013377926421404679,"#142D45"],[0.016722408026755866,"#142D46"],[0.020066889632107017,"#142D46"],[0.023411371237458206,"#142E47"],[0.026755852842809357,"#152E47"],[0.030100334448160546,"#152F48"],[0.033444816053511697,"#152F48"],[0.036789297658862886,"#152F49"],[0.040133779264214034,"#153049"],[0.043478260869565223,"#16304A"],[0.046822742474916371,"#16304A"],[0.05016722408026756,"#16314B"],[0.053511705685618749,"#16314B"],[0.056856187290969903,"#16324C"],[0.060200668896321051,"#17324D"],[0.06354515050167224,"#17324D"],[0.066889632107023422,"#17334E"],[0.070234113712374577,"#17334E"],[0.073578595317725773,"#17344F"],[0.076923076923076913,"#18344F"],[0.08026755852842811,"#183450"],[0.08361204013377925,"#183550"],[0.086956521739130446,"#183551"],[0.090301003344481628,"#183651"],[0.093645484949832783,"#193652"],[0.096989966555183937,"#193652"],[0.10033444816053512,"#193753"],[0.10367892976588632,"#193754"],[0.10702341137123746,"#193854"],[0.11036789297658861,"#1A3855"],[0.11371237458193981,"#1A3955"],[0.11705685618729099,"#1A3956"],[0.12040133779264214,"#1A3956"],[0.12374581939799328,"#1A3A57"],[0.12709030100334448,"#1B3A57"],[0.13043478260869565,"#1B3B58"],[0.13377926421404682,"#1B3B59"],[0.13712374581939801,"#1B3B59"],[0.14046822742474918,"#1C3C5A"],[0.14381270903010035,"#1C3C5A"],[0.14715719063545152,"#1C3D5B"],[0.15050167224080269,"#1C3D5B"],[0.15384615384615385,"#1C3D5C"],[0.15719063545150502,"#1D3E5C"],[0.16053511705685619,"#1D3E5D"],[0.16387959866220736,"#1D3F5D"],[0.16722408026755853,"#1D3F5E"],[0.1705685618729097,"#1D3F5F"],[0.17391304347826086,"#1E405F"],[0.17725752508361203,"#1E4060"],[0.18060200668896323,"#1E4160"],[0.18394648829431437,"#1E4161"],[0.18729096989966557,"#1E4261"],[0.19063545150501671,"#1F4262"],[0.1939799331103679,"#1F4263"],[0.19732441471571904,"#1F4363"],[0.20066889632107024,"#1F4364"],[0.20401337792642141,"#1F4464"],[0.20735785953177258,"#204465"],[0.21070234113712374,"#204465"],[0.21404682274247491,"#204566"],[0.21739130434782611,"#204566"],[0.22073578595317725,"#214667"],[0.22408026755852845,"#214668"],[0.22742474916387961,"#214768"],[0.23076923076923078,"#214769"],[0.23411371237458195,"#214769"],[0.23745819397993312,"#22486A"],[0.24080267558528429,"#22486A"],[0.24414715719063546,"#22496B"],[0.24749163879598662,"#22496C"],[0.25083612040133779,"#224A6C"],[0.25418060200668896,"#234A6D"],[0.25752508361204013,"#234A6D"],[0.2608695652173913,"#234B6E"],[0.26421404682274247,"#234B6E"],[0.26755852842809363,"#244C6F"],[0.2709030100334448,"#244C70"],[0.27424749163879603,"#244C70"],[0.27759197324414714,"#244D71"],[0.28093645484949836,"#244D71"],[0.28428093645484948,"#254E72"],[0.2876254180602007,"#254E72"],[0.29096989966555187,"#254F73"],[0.29431438127090304,"#254F74"],[0.2976588628762542,"#254F74"],[0.30100334448160537,"#265075"],[0.30434782608695654,"#265075"],[0.30769230769230771,"#265176"],[0.31103678929765888,"#265176"],[0.31438127090301005,"#275277"],[0.31772575250836121,"#275278"],[0.32107023411371238,"#275278"],[0.32441471571906355,"#275379"],[0.32775919732441472,"#275379"],[0.33110367892976589,"#28547A"],[0.33444816053511706,"#28547B"],[0.33779264214046822,"#28557B"],[0.34113712374581939,"#28557C"],[0.34448160535117062,"#28567C"],[0.34782608695652173,"#29567D"],[0.35117056856187295,"#29567D"],[0.35451505016722407,"#29577E"],[0.35785953177257523,"#29577F"],[0.36120401337792646,"#2A587F"],[0.36454849498327763,"#2A5880"],[0.36789297658862874,"#2A5980"],[0.37123745819397991,"#2A5981"],[0.37458193979933113,"#2A5982"],[0.3779264214046823,"#2B5A82"],[0.38127090301003341,"#2B5A83"],[0.38461538461538464,"#2B5B83"],[0.38795986622073581,"#2B5B84"],[0.39130434782608697,"#2C5C85"],[0.39464882943143809,"#2C5C85"],[0.39799331103678937,"#2C5D86"],[0.40133779264214048,"#2C5D86"],[0.40468227424749165,"#2C5D87"],[0.40802675585284282,"#2D5E87"],[0.41137123745819404,"#2D5E88"],[0.41471571906354515,"#2D5F89"],[0.41806020066889632,"#2D5F89"],[0.42140468227424749,"#2E608A"],[0.42474916387959871,"#2E608A"],[0.42809364548494983,"#2E618B"],[0.43143812709030099,"#2E618C"],[0.43478260869565222,"#2E618C"],[0.43812709030100339,"#2F628D"],[0.4414715719063545,"#2F628D"],[0.44481605351170567,"#2F638E"],[0.44816053511705689,"#2F638F"],[0.45150501672240806,"#30648F"],[0.45484949832775923,"#306490"],[0.45819397993311034,"#306590"],[0.46153846153846156,"#306591"],[0.46488294314381273,"#306592"],[0.4682274247491639,"#316692"],[0.47157190635451501,"#316693"],[0.47491638795986624,"#316793"],[0.47826086956521741,"#316794"],[0.48160535117056857,"#326895"],[0.4849498327759198,"#326895"],[0.48829431438127091,"#326996"],[0.49163879598662208,"#326996"],[0.49498327759197325,"#326997"],[0.49832775919732447,"#336A98"],[0.50167224080267558,"#336A98"],[0.50501672240802675,"#336B99"],[0.50836120401337792,"#336B99"],[0.51170568561872909,"#346C9A"],[0.51505016722408026,"#346C9B"],[0.51839464882943143,"#346D9B"],[0.52173913043478259,"#346D9C"],[0.52508361204013387,"#346E9D"],[0.52842809364548493,"#356E9D"],[0.5317725752508361,"#356E9E"],[0.53511705685618727,"#356F9E"],[0.53846153846153855,"#356F9F"],[0.5418060200668896,"#3670A0"],[0.54515050167224077,"#3670A0"],[0.54849498327759205,"#3671A1"],[0.55183946488294322,"#3671A1"],[0.55518394648829428,"#3772A2"],[0.55852842809364545,"#3772A3"],[0.56187290969899673,"#3773A3"],[0.56521739130434789,"#3773A4"],[0.56856187290969895,"#3773A4"],[0.57190635451505012,"#3874A5"],[0.5752508361204014,"#3874A6"],[0.57859531772575257,"#3875A6"],[0.58193979933110374,"#3875A7"],[0.58528428093645479,"#3976A8"],[0.58862876254180607,"#3976A8"],[0.59197324414715724,"#3977A9"],[0.59531772575250841,"#3977A9"],[0.59866220735785958,"#3978AA"],[0.60200668896321075,"#3A78AB"],[0.60535117056856191,"#3A79AB"],[0.60869565217391308,"#3A79AC"],[0.61204013377926425,"#3A79AC"],[0.61538461538461542,"#3B7AAD"],[0.61872909698996659,"#3B7AAE"],[0.62207357859531776,"#3B7BAE"],[0.62541806020066892,"#3B7BAF"],[0.62876254180602009,"#3C7CB0"],[0.63210702341137126,"#3C7CB0"],[0.63545150501672243,"#3C7DB1"],[0.6387959866220736,"#3C7DB1"],[0.64214046822742477,"#3C7EB2"],[0.64548494983277593,"#3D7EB3"],[0.6488294314381271,"#3D7FB3"],[0.65217391304347838,"#3D7FB4"],[0.65551839464882944,"#3D7FB5"],[0.65886287625418072,"#3E80B5"],[0.66220735785953189,"#3E80B6"],[0.66555183946488294,"#3E81B6"],[0.66889632107023422,"#3E81B7"],[0.67224080267558528,"#3F82B8"],[0.67558528428093645,"#3F82B8"],[0.67892976588628773,"#3F83B9"],[0.68227424749163879,"#3F83BA"],[0.68561872909699006,"#4084BA"],[0.68896321070234123,"#4084BB"],[0.69230769230769229,"#4085BB"],[0.69565217391304357,"#4085BC"],[0.69899665551839463,"#4086BD"],[0.70234113712374591,"#4186BD"],[0.70568561872909707,"#4186BE"],[0.70903010033444813,"#4187BF"],[0.71237458193979941,"#4187BF"],[0.71571906354515047,"#4288C0"],[0.71906354515050164,"#4288C1"],[0.72240802675585292,"#4289C1"],[0.72575250836120397,"#4289C2"],[0.72909698996655525,"#438AC2"],[0.73244147157190642,"#438AC3"],[0.73578595317725748,"#438BC4"],[0.73913043478260876,"#438BC4"],[0.74247491638795982,"#438CC5"],[0.7458193979933111,"#448CC6"],[0.74916387959866226,"#448DC6"],[0.75250836120401332,"#448DC7"],[0.7558528428093646,"#448EC8"],[0.75919732441471577,"#458EC8"],[0.76254180602006683,"#458FC9"],[0.76588628762541811,"#458FC9"],[0.76923076923076927,"#458FCA"],[0.77257525083612044,"#4690CB"],[0.77591973244147161,"#4690CB"],[0.77926421404682267,"#4691CC"],[0.78260869565217395,"#4691CD"],[0.78595317725752512,"#4792CD"],[0.78929765886287617,"#4792CE"],[0.79264214046822745,"#4793CF"],[0.79598662207357873,"#4793CF"],[0.79933110367892979,"#4894D0"],[0.80267558528428096,"#4894D0"],[0.80602006688963224,"#4895D1"],[0.80936454849498329,"#4895D2"],[0.81270903010033446,"#4896D2"],[0.81605351170568563,"#4996D3"],[0.8193979933110368,"#4997D4"],[0.82274247491638808,"#4997D4"],[0.82608695652173914,"#4998D5"],[0.8294314381270903,"#4A98D6"],[0.83277591973244158,"#4A99D6"],[0.83612040133779264,"#4A99D7"],[0.83946488294314392,"#4A9AD8"],[0.84280936454849498,"#4B9AD8"],[0.84615384615384615,"#4B9BD9"],[0.84949832775919742,"#4B9BDA"],[0.85284280936454848,"#4B9BDA"],[0.85618729096989965,"#4C9CDB"],[0.85953177257525093,"#4C9CDB"],[0.86287625418060199,"#4C9DDC"],[0.86622073578595327,"#4C9DDD"],[0.86956521739130443,"#4D9EDD"],[0.87290969899665549,"#4D9EDE"],[0.87625418060200677,"#4D9FDF"],[0.87959866220735783,"#4D9FDF"],[0.882943143812709,"#4DA0E0"],[0.88628762541806028,"#4EA0E1"],[0.88963210702341133,"#4EA1E1"],[0.89297658862876261,"#4EA1E2"],[0.89632107023411378,"#4EA2E3"],[0.89966555183946484,"#4FA2E3"],[0.90301003344481612,"#4FA3E4"],[0.90635451505016718,"#4FA3E5"],[0.90969899665551845,"#4FA4E5"],[0.91304347826086962,"#50A4E6"],[0.91638795986622068,"#50A5E7"],[0.91973244147157196,"#50A5E7"],[0.92307692307692313,"#50A6E8"],[0.92642140468227419,"#51A6E8"],[0.92976588628762546,"#51A7E9"],[0.93311036789297663,"#51A7EA"],[0.9364548494983278,"#51A8EA"],[0.93979933110367897,"#52A8EB"],[0.94314381270903003,"#52A9EC"],[0.94648829431438131,"#52A9EC"],[0.94983277591973247,"#52AAED"],[0.95317725752508364,"#53AAEE"],[0.95652173913043481,"#53ABEE"],[0.95986622073578609,"#53ABEF"],[0.96321070234113715,"#53ACF0"],[0.96655518394648832,"#54ACF0"],[0.9698996655518396,"#54ADF1"],[0.97324414715719065,"#54ADF2"],[0.97658862876254182,"#54AEF2"],[0.97993311036789299,"#55AEF3"],[0.98327759197324416,"#55AFF4"],[0.98662207357859544,"#55AFF4"],[0.98996655518394649,"#55B0F5"],[0.99331103678929766,"#56B0F6"],[0.99665551839464894,"#56B1F6"],[1,"#56B1F7"]],"colorbar":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":0,"thickness":23.039999999999996,"title":"value","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"tickmode":"array","ticktext":["0.0","0.5","1.0"],"tickvals":[0.30185068716515251,0.65009201024924301,0.99833333333333329],"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"ticklen":2,"len":0.5,"yanchor":"top","y":1}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":23.305936073059364,"r":7.3059360730593621,"b":37.260273972602747,"l":177.53424657534251},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,8.5999999999999996],"tickmode":"array","ticktext":["Nscore","Escore","Oscore","AScore","Cscore","Impulsive","SS","log_user_consumption_level"],"tickvals":[1,2,3,4,5,6,7,8],"categoryorder":"array","categoryarray":["Nscore","Escore","Oscore","AScore","Cscore","Impulsive","SS","log_user_consumption_level"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Var1","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,8.5999999999999996],"tickmode":"array","ticktext":["Nscore","Escore","Oscore","AScore","Cscore","Impulsive","SS","log_user_consumption_level"],"tickvals":[1,2,3,4,5,6,7,8],"categoryorder":"array","categoryarray":["Nscore","Escore","Oscore","AScore","Cscore","Impulsive","SS","log_user_consumption_level"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Var2","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"y":0.5,"yanchor":"top","title":{"text":"colour<br />size<br />value","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"511c7bfb4139":{"x":{},"y":{},"fill":{},"type":"heatmap"},"511c24e86591":{"x":{},"y":{},"fill":{},"label":{},"colour":{},"size":{}}},"cur_data":"511c7bfb4139","visdat":{"511c7bfb4139":["function (y) ","x"],"511c24e86591":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

# Education

To test whether there is a significant relationship between a person’s
education status and their consumption of drugs, we decided to create a
variable in the data that counted how many drugs a person uses. We
classified this as an ordinal variable in terms of severity. Thus, this
relationship’s significance is shown in the following chi-squared test.

``` r
data_five <- data_one |> group_by(Education, user_status) |> summarize(count = n()) |>
  na.omit(Education)
```

    ## `summarise()` has grouped output by 'Education'. You can override using the
    ## `.groups` argument.

``` r
data_six <- data_five |> pivot_wider(
  names_from = user_status,
  values_from = count,
  values_fill = 0
)

chi_test_result_education <- chisq.test(data_six[ , -1])
```

    ## Warning in chisq.test(data_six[, -1]): Chi-squared approximation may be
    ## incorrect

``` r
tidy_chi_test_result_education <- tidy(chi_test_result_education)

chi_square_education_summary <- data.frame(
  Statistic = c("Chi-squared", "Degrees of Freedom", "P-Value"),
  Value = c(round(tidy_chi_test_result_education$statistic, 2),
            round(tidy_chi_test_result_education$parameter, 2),
            round(tidy_chi_test_result_education$p.value, 3)
            )
)

kable(chi_square_education_summary)
```

|           | Statistic          |  Value |
|:----------|:-------------------|-------:|
| X-squared | Chi-squared        | 396.24 |
| df        | Degrees of Freedom | 144.00 |
|           | P-Value            |   0.00 |

Since our chi-squared test resulted in a p-value of near 0, we can
conclude that there is a significant relationship between a person’s
education status and their drug consumption level, and that these two
variables are not independent.

To examine the direction of this relationship, we decided to plot out
the distribution of total drug consumption level for each level of
education in box-plots. This plot can be seen below.

``` r
data_education <- data_one |> mutate(Education = factor(Education, levels = c("Left school before 16 years", "Left school at 16 years", "Left school at 17 years", "Left school at 18 years",
                                                  "Some college or university, no certificate or degree",
                                                  "University degree", "Masters degree",
                                                  "Professional certificate/ diploma", "Doctorate degree"))) |>
  arrange(Education)

plot_education <- data_education |> ggplot(aes(x = Education, y = log1p(user_consumption_level), fill = Education)) +
  geom_boxplot() +
  labs(title = "Log of User Consumption Dependent on Education",
       x = "Education",
       y = "Log User Consumption Level") +
  theme_minimal() + 
  theme(axis.text.x = element_blank())

plot_education
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

It is notable that as a person’s education goes up from “left school
before 16 years” to “some college or university, but no certificate or
degree,” a person’s total drug consumption level increases on average.
However, from that point on, as education status goes up, the level of
total drug consumption level generally decreases.

Furthermore, to analyze the relationship between education status and
our relevant personality traits, we conducted ANOVA test to see if the
mean traits of O score, C score, A score, N score, and Sensation-Seeking
differed among at least 2 of our education groups. The results of the
ANOVA tests are summarized below.

``` r
data_Oscore_Education_prime <- data_education |> select(Education, Oscore)
Oscore_aov_education_model <- aov(Oscore~Education, data = data_Oscore_Education_prime)
tidy_anova_Oscore <- tidy(Oscore_aov_education_model) |> mutate(model = "O score education")

data_Cscore_Education_prime <- data_education |> select(Education, Cscore)
Cscore_aov_education_model <- aov(Cscore~Education, data = data_Cscore_Education_prime)
tidy_anova_Cscore <- tidy(Cscore_aov_education_model) |> mutate(model = "C score education")

data_Ascore_Education_prime <- data_education |> select(Education, AScore)
Ascore_aov_education_model <- aov(AScore~Education, data = data_Ascore_Education_prime)
tidy_anova_Ascore <- tidy(Ascore_aov_education_model) |> mutate(model = "O score education")

data_Nscore_Education_prime <- data_education |> select(Education, Nscore)
Nscore_aov_education_model <- aov(Nscore~Education, data = data_Nscore_Education_prime)
tidy_anova_Nscore <- tidy(Nscore_aov_education_model) |> mutate(model = "N score education")

data_SS_Education_prime <- data_education |> select(Education, SS)
SS_aov_education_model <- aov(SS~Education, data = data_SS_Education_prime)
tidy_anova_SS <- tidy(SS_aov_education_model) |> mutate(model = "SS education")


kable(bind_rows(tidy_anova_Oscore, tidy_anova_Cscore, tidy_anova_Ascore, tidy_anova_Nscore, tidy_anova_SS))
```

| term      |   df |      sumsq |     meansq | statistic |  p.value | model             |
|:----------|-----:|-----------:|-----------:|----------:|---------:|:------------------|
| Education |    8 |  121.32548 | 15.1656851 | 16.266137 | 0.00e+00 | O score education |
| Residuals | 1875 | 1748.15070 |  0.9323470 |        NA |       NA | O score education |
| Education |    8 |  163.21359 | 20.4016983 | 22.351158 | 0.00e+00 | C score education |
| Residuals | 1875 | 1711.46320 |  0.9127804 |        NA |       NA | C score education |
| Education |    8 |   26.61024 |  3.3262806 |  3.376858 | 7.46e-04 | O score education |
| Residuals | 1875 | 1846.91701 |  0.9850224 |        NA |       NA | O score education |
| Education |    8 |   34.11085 |  4.2638564 |  4.338687 | 3.37e-05 | N score education |
| Residuals | 1875 | 1842.66116 |  0.9827526 |        NA |       NA | N score education |
| Education |    8 |  111.51425 | 13.9392816 | 15.967770 | 0.00e+00 | SS education      |
| Residuals | 1875 | 1636.80673 |  0.8729636 |        NA |       NA | SS education      |

From our ANOVA table, it is observed that all the tests yielded p-values
of near 0. Thus, we can conclude that the O trait, C trait, A trait, N
trait, and sensation-seeking traits differ significantly between at
least 2 education groups for each trait. To further examine the specific
differences, we plotted the distribution of all trait scores for each
education status below.

``` r
plot_Education_Oscore <- data_Oscore_Education_prime |> ggplot(aes(x = Education, 
                              y = Oscore, fill = Education)) + geom_boxplot() + 
  theme(axis.text.x = element_blank())
plot_Education_Cscore <- data_Cscore_Education_prime |> ggplot(aes(x = Education, y = Cscore, fill = Education)) + geom_boxplot() +
  theme(axis.text.x = element_blank())
plot_Education_Ascore <- data_Ascore_Education_prime |> ggplot(aes(x = Education, y = AScore, fill = Education)) + geom_boxplot() +
  theme(axis.text.x = element_blank())
plot_Education_Nscore <- data_Nscore_Education_prime |> ggplot(aes(x = Education, y = Nscore, fill = Education)) + geom_boxplot() +
  theme(axis.text.x = element_blank())
plot_Education_SS <- data_SS_Education_prime |> ggplot(aes(x = Education, y = SS, fill = Education)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

plot_education_patch <- ggarrange(plot_Education_Oscore, plot_Education_Cscore, plot_Education_Ascore,
                                  plot_Education_Nscore, plot_Education_SS, ncol = 3, 
                                  nrow = 2, common.legend = TRUE, legend = "bottom")

plot_education_patch
```

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

By examining the O-score distribution, it seems that the group most
at-risk of a high curiosity are those with some college or university
experience, but no certificate or degree. Likewise, for the C-score
trait, the same group also has the lowest mean score for organization
and diligence. For the sensation-seeking trait, those who left school at
18 years are also very likely to have a high score on average. For
agreeableness (A score) and neuroticism (N score), those who left school
at 17 years are most at risk of having the lowest and highest scores on
average, respectively.

# Country

``` r
data_country <- data_one |> group_by(Country, user_status_real) |> summarize(count = n()) |>
  na.omit(user_status_real)
```

    ## `summarise()` has grouped output by 'Country'. You can override using the
    ## `.groups` argument.

``` r
data_country_prime <- data_country |> pivot_wider(
  names_from = user_status_real,
  values_from = count,
  values_fill = 0
)

chi_test_result_residence <- chisq.test(data_country_prime[ , -1])
```

    ## Warning in chisq.test(data_country_prime[, -1]): Chi-squared approximation may
    ## be incorrect

``` r
tidy_chi_test_result_residence <- tidy(chi_test_result_residence)

chi_square_residence_summary <- data.frame(
  Statistic = c("Chi-squared", "Degrees of Freedom", "P-Value"),
  Value = c(round(tidy_chi_test_result_residence$statistic, 2),
            round(tidy_chi_test_result_residence$parameter, 2),
            round(tidy_chi_test_result_residence$p.value, 3)
            )
)

kable(chi_square_residence_summary)
```

|           | Statistic          |  Value |
|:----------|:-------------------|-------:|
| X-squared | Chi-squared        | 707.26 |
| df        | Degrees of Freedom | 108.00 |
|           | P-Value            |   0.00 |

``` r
data_one_trans <- data_one |> mutate(log_user_consumption_level = log1p(user_consumption_level))

data_one_trans |> ggplot(aes(x = Country, y = log_user_consumption_level, fill = Country)) + geom_boxplot()
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
data_Oscore_country <- data_one |> select(Country, Oscore)
Oscore_aov_country_model <- aov(Oscore~Country, data = data_Oscore_country)
tidy_anova_Oscore_country <- tidy(Oscore_aov_country_model) |> mutate(model = "O score country")
p1 <- data_Oscore_country |> ggplot(aes(x = Country, y = Oscore, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

data_Cscore_country <- data_one |> select(Country, Cscore)
Cscore_aov_country_model <- aov(Cscore~Country, data = data_Cscore_country)
tidy_anova_Cscore_country <- tidy(Cscore_aov_country_model) |> mutate(model = "C score country")
p2 <- data_Cscore_country |> ggplot(aes(x = Country, y = Cscore, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

data_Escore_country <- data_one |> select(Country, Escore)
Escore_aov_country_model <- aov(Escore~Country, data = data_Escore_country)
tidy_anova_Escore_country <- tidy(Escore_aov_country_model) |> mutate(model = "E score country")
p3 <- data_Escore_country |> ggplot(aes(x = Country, y = Escore, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

data_Ascore_country <- data_one |> select(Country, AScore)
Ascore_aov_country_model <- aov(AScore~Country, data = data_Ascore_country)
tidy_anova_Ascore_country <- tidy(Ascore_aov_country_model) |> mutate(model = "A score country")
p4 <- data_Ascore_country |> ggplot(aes(x = Country, y = AScore, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

data_Nscore_country <- data_one |> select(Country, Nscore)
Nscore_aov_country_model <- aov(Nscore~Country, data = data_Nscore_country)
tidy_anova_Nscore_country <- tidy(Nscore_aov_country_model) |> mutate(model = "N score country")
p5 <- data_Nscore_country |> ggplot(aes(x = Country, y = Nscore, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

data_Impulsivity_country <- data_one |> select(Country, Impulsive)
Impulsivity_aov_country_model <- aov(Impulsive~Country, data = data_Impulsivity_country)
tidy_anova_Impulsivity_country <- tidy(Impulsivity_aov_country_model) |> mutate(model = "Impulsivity score country")
p6 <- data_Impulsivity_country |> ggplot(aes(x = Country, y = Impulsive, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

data_SS_country <- data_one |> select(Country, SS)
SS_aov_country_model <- aov(SS~Country, data = data_SS_country)
tidy_anova_SS_country <- tidy(SS_aov_country_model) |> mutate(model = "N score country")
p7 <- data_SS_country |> ggplot(aes(x = Country, y = SS, fill = Country)) + geom_boxplot() +
  theme(axis.text.x = element_blank())

kable(bind_rows(tidy_anova_Oscore_country, tidy_anova_Cscore_country, 
                tidy_anova_Escore_country, tidy_anova_Ascore_country, 
                tidy_anova_Nscore_country))
```

| term      |   df |      sumsq |     meansq | statistic |  p.value | model           |
|:----------|-----:|-----------:|-----------:|----------:|---------:|:----------------|
| Country   |    6 |  221.16396 | 36.8606597 | 41.974729 | 0.00e+00 | O score country |
| Residuals | 1877 | 1648.31222 |  0.8781631 |        NA |       NA | O score country |
| Country   |    6 |   90.06932 | 15.0115525 | 15.788729 | 0.00e+00 | C score country |
| Residuals | 1877 | 1784.60747 |  0.9507765 |        NA |       NA | C score country |
| Country   |    6 |   30.92140 |  5.1535672 |  5.248226 | 2.26e-05 | E score country |
| Residuals | 1877 | 1843.14566 |  0.9819636 |        NA |       NA | E score country |
| Country   |    6 |   54.12981 |  9.0216357 |  9.307263 | 0.00e+00 | A score country |
| Residuals | 1877 | 1819.39744 |  0.9693114 |        NA |       NA | A score country |
| Country   |    6 |   42.74903 |  7.1248382 |  7.291796 | 1.00e-07 | N score country |
| Residuals | 1877 | 1834.02299 |  0.9771033 |        NA |       NA | N score country |

``` r
plot_country_psychology_patch <- ggarrange(p1, p2, p3,
                                  p4, p5, ncol = 3, 
                                  nrow = 2, common.legend = TRUE, legend = "right")

plot_country_psychology_patch
```

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
kable(bind_rows(tidy_anova_Impulsivity_country, tidy_anova_SS_country))
```

| term | df | sumsq | meansq | statistic | p.value | model |
|:---|---:|---:|---:|---:|---:|:---|
| Country | 6 | 95.87967 | 15.9799458 | 18.51171 | 0 | Impulsivity score country |
| Residuals | 1877 | 1620.29080 | 0.8632343 | NA | NA | Impulsivity score country |
| Country | 6 | 210.95916 | 35.1598592 | 42.92747 | 0 | N score country |
| Residuals | 1877 | 1537.36182 | 0.8190527 | NA | NA | N score country |

``` r
plot_country_psychology_patch_2 <- ggarrange(p6, p7, ncol = 2, 
                                  nrow = 1, common.legend = TRUE, legend = "right")

plot_country_psychology_patch_2
```

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
data_education |> ggplot(aes(x = Education, fill = Education)) + geom_bar() +
  facet_wrap(~Country) + theme(axis.text.x = element_blank())
```

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
model_no_interaction_education_residence <- lm(log1p(user_consumption_level) ~ (Oscore * Cscore *
                                 AScore * Nscore * SS) + Country + Education, data = data_one)

model_no_interaction_education_residence |> summary() |>
  broom::tidy() |>
  filter(p.value < 0.05) |>
  filter(term %in% c("CountryCanada", "CountryOther", "CountryUK",
                     "EducationLeft school at 18 years",
                     "EducationLeft school before 16 years",
                     "EducationSome college or university, no certificate or degree")) |>
  kbl(
      caption     = "Effect of Psychological Predictors on Drug Consumption Levels", 
      col.names   = c("Predictor", "Estimate", "SE", "t-statistic", "p-value"), 
      digits      = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Effect of Psychological Predictors on Drug Consumption Levels
</caption>

<thead>

<tr>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

CountryCanada
</td>

<td style="text-align:right;">

-0.127
</td>

<td style="text-align:right;">

0.061
</td>

<td style="text-align:right;">

-2.062
</td>

<td style="text-align:right;">

0.039
</td>

</tr>

<tr>

<td style="text-align:left;">

CountryOther
</td>

<td style="text-align:right;">

-0.148
</td>

<td style="text-align:right;">

0.059
</td>

<td style="text-align:right;">

-2.522
</td>

<td style="text-align:right;">

0.012
</td>

</tr>

<tr>

<td style="text-align:left;">

CountryUK
</td>

<td style="text-align:right;">

-0.304
</td>

<td style="text-align:right;">

0.050
</td>

<td style="text-align:right;">

-6.069
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

EducationLeft school at 18 years
</td>

<td style="text-align:right;">

0.149
</td>

<td style="text-align:right;">

0.052
</td>

<td style="text-align:right;">

2.856
</td>

<td style="text-align:right;">

0.004
</td>

</tr>

<tr>

<td style="text-align:left;">

EducationLeft school before 16 years
</td>

<td style="text-align:right;">

0.217
</td>

<td style="text-align:right;">

0.076
</td>

<td style="text-align:right;">

2.859
</td>

<td style="text-align:right;">

0.004
</td>

</tr>

<tr>

<td style="text-align:left;">

EducationSome college or university, no certificate or degree
</td>

<td style="text-align:right;">

0.110
</td>

<td style="text-align:right;">

0.042
</td>

<td style="text-align:right;">

2.616
</td>

<td style="text-align:right;">

0.009
</td>

</tr>

</tbody>

</table>

``` r
model_interaction_psychology |> summary() |> broom::glance() |>
  bind_rows(summary(model_no_interaction_education_residence) |> broom::glance()) |>
  mutate(model = c("Psychology Model with Interaction", "Psychology Model with Education and Residence")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of User Consumption from Psychological Traits"
    , col.names = c(
        "Model", "R-squared", "Adj. R-squared"
      , "Sigma", "F-statistic", "p-value", "df", "Residual df", "N"
    )
    , digits    = c(1, 2, 2, 0, 2, 5, 0, 0, 0)
  )
```

<table>

<caption>

Key Statistics for Prediction of User Consumption from Psychological
Traits
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:right;">

R-squared
</th>

<th style="text-align:right;">

Adj. R-squared
</th>

<th style="text-align:right;">

Sigma
</th>

<th style="text-align:right;">

F-statistic
</th>

<th style="text-align:right;">

p-value
</th>

<th style="text-align:right;">

df
</th>

<th style="text-align:right;">

Residual df
</th>

<th style="text-align:right;">

N
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Psychology Model with Interaction
</td>

<td style="text-align:right;">

0.39
</td>

<td style="text-align:right;">

0.37
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

18.58
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

63
</td>

<td style="text-align:right;">

1812
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

Psychology Model with Education and Residence
</td>

<td style="text-align:right;">

0.47
</td>

<td style="text-align:right;">

0.45
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

35.35
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

45
</td>

<td style="text-align:right;">

1830
</td>

<td style="text-align:right;">

1876
</td>

</tr>

</tbody>

</table>

``` r
knitr::kable(AIC(model_interaction_psychology, model_no_interaction_education_residence))
```

|                                          |  df |      AIC |
|:-----------------------------------------|----:|---------:|
| model_interaction_psychology             |  65 | 1646.347 |
| model_no_interaction_education_residence |  47 | 1371.758 |

# Additional Analysis

``` r
t_test_result <- t.test(user_consumption_level~Gender, data = data_one, var.equaql = FALSE, alternative = "two.sided")
print(t_test_result)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  user_consumption_level by Gender
    ## t = -15.014, df = 1839, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group F and group M is not equal to 0
    ## 95 percent confidence interval:
    ##  -11.090030  -8.527473
    ## sample estimates:
    ## mean in group F mean in group M 
    ##        28.44338        38.25213

``` r
data_one |> ggplot(aes(x = Gender, y = user_consumption_level, fill = Gender)) + geom_boxplot()
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
data_ethnicity <- data_one |> group_by(Ethnicity, user_status_real) |> summarize(count = n()) |>
  na.omit(user_status_real)
```

    ## `summarise()` has grouped output by 'Ethnicity'. You can override using the
    ## `.groups` argument.

``` r
data_ethnicity_prime <- data_ethnicity |> pivot_wider(
  names_from = user_status_real,
  values_from = count,
  values_fill = 0
)

chi_test_result_ethnicity <- chisq.test(data_ethnicity_prime[ , -1])
```

    ## Warning in chisq.test(data_ethnicity_prime[, -1]): Chi-squared approximation
    ## may be incorrect

``` r
tidy_chi_test_result_ethnicity <- tidy(chi_test_result_ethnicity)

chi_square_ethnicity_summary <- data.frame(
  Statistic = c("Chi-squared", "Degrees of Freedom", "P-Value"),
  Value = c(round(tidy_chi_test_result_ethnicity$statistic, 2),
            round(tidy_chi_test_result_ethnicity$parameter, 2),
            round(tidy_chi_test_result_ethnicity$p.value, 3)
            )
)

kable(chi_square_ethnicity_summary)
```

|           | Statistic          |  Value |
|:----------|:-------------------|-------:|
| X-squared | Chi-squared        | 292.64 |
| df        | Degrees of Freedom | 108.00 |
|           | P-Value            |   0.00 |

``` r
data_one_trans |> ggplot(aes(x = Ethnicity, y = log_user_consumption_level, fill = Ethnicity)) + geom_boxplot()
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
data_ethnicity_country <- data_one |> group_by(Country, Ethnicity) |> summarize(count = n())
```

    ## `summarise()` has grouped output by 'Country'. You can override using the
    ## `.groups` argument.

``` r
data_ethnicity_country_prime <- data_ethnicity_country |> pivot_wider(
  names_from = Ethnicity,
  values_from = count,
  values_fill = 0
)

chi_test_result_ethnicity_country <- chisq.test(data_ethnicity_country_prime[ , -1])
```

    ## Warning in chisq.test(data_ethnicity_country_prime[, -1]): Chi-squared
    ## approximation may be incorrect

``` r
tidy_chi_test_result_ethnicity_country <- tidy(chi_test_result_ethnicity_country)

chi_square_ethnicity_country_summary <- data.frame(
  Statistic = c("Chi-squared", "Degrees of Freedom", "P-Value"),
  Value = c(round(tidy_chi_test_result_ethnicity_country$statistic, 2),
            round(tidy_chi_test_result_ethnicity_country$parameter, 2),
            round(tidy_chi_test_result_ethnicity_country$p.value, 3)
            )
)

kable(chi_square_ethnicity_country_summary)
```

|           | Statistic          |  Value |
|:----------|:-------------------|-------:|
| X-squared | Chi-squared        | 67.720 |
| df        | Degrees of Freedom | 36.000 |
|           | P-Value            |  0.001 |

``` r
data_age <- data_one |> group_by(Age, user_status_real) |> summarize(count = n()) |>
  na.omit(user_status_real)
```

    ## `summarise()` has grouped output by 'Age'. You can override using the `.groups`
    ## argument.

``` r
data_age_prime <- data_age |> pivot_wider(
  names_from = user_status_real,
  values_from = count,
  values_fill = 0
)

chi_test_result_age <- chisq.test(data_age_prime[ , -1])
```

    ## Warning in chisq.test(data_age_prime[, -1]): Chi-squared approximation may be
    ## incorrect

``` r
print(chi_test_result_age)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  data_age_prime[, -1]
    ## X-squared = 540.9, df = 90, p-value < 2.2e-16

``` r
data_one_trans |> ggplot(aes(x = Age, y = log_user_consumption_level, fill = Age)) + geom_boxplot()
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
model_full <- lm(log1p(user_consumption_level) ~ (Oscore + Cscore +
                                 AScore + Nscore + Impulsive + SS)^6 + Country + Education +
                   Age + Gender + Ethnicity*Country, data = data_one)

knitr::kable(AIC(model_no_interaction_education_residence, model_full))
```

|                                          |  df |      AIC |
|:-----------------------------------------|----:|---------:|
| model_no_interaction_education_residence |  47 | 1371.758 |
| model_full                               | 104 | 1161.297 |

``` r
model_no_interaction_education_residence |> summary() |> broom::glance() |>
  bind_rows(summary(model_full) |> broom::glance()) |>
  mutate(model = c("Psychology Model with Education and Residence", "Best Model")) |>
  relocate(model) |>
  kbl(
    caption     = "Key Statistics for Prediction of User Consumption from Psychological Traits"
    , col.names = c(
        "Model", "R-squared", "Adj. R-squared"
      , "Sigma", "F-statistic", "p-value", "df", "Residual df", "N"
    )
    , digits    = c(1, 2, 2, 0, 2, 5, 0, 0, 0)
  )
```

<table>

<caption>

Key Statistics for Prediction of User Consumption from Psychological
Traits
</caption>

<thead>

<tr>

<th style="text-align:left;">

Model
</th>

<th style="text-align:right;">

R-squared
</th>

<th style="text-align:right;">

Adj. R-squared
</th>

<th style="text-align:right;">

Sigma
</th>

<th style="text-align:right;">

F-statistic
</th>

<th style="text-align:right;">

p-value
</th>

<th style="text-align:right;">

df
</th>

<th style="text-align:right;">

Residual df
</th>

<th style="text-align:right;">

N
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Psychology Model with Education and Residence
</td>

<td style="text-align:right;">

0.47
</td>

<td style="text-align:right;">

0.45
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

35.35
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

45
</td>

<td style="text-align:right;">

1830
</td>

<td style="text-align:right;">

1876
</td>

</tr>

<tr>

<td style="text-align:left;">

Best Model
</td>

<td style="text-align:right;">

0.55
</td>

<td style="text-align:right;">

0.52
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

21.25
</td>

<td style="text-align:right;">

0
</td>

<td style="text-align:right;">

102
</td>

<td style="text-align:right;">

1773
</td>

<td style="text-align:right;">

1876
</td>

</tr>

</tbody>

</table>

``` r
model_full |> summary() |>
  broom::tidy() |>
  filter(p.value < 0.05) |>
  filter(term %in% c("Oscore", "Cscore", "Ascore", "Nscore", "SS",
                     "CountryCanada", "CountryUK", "EducationLeft school at 18 years",
                     "EducationLeft school before 16 years",
                     "Age45-54", "Age55-64", "Age65+", "GenderM")) |>
  kbl(
      caption     = "Effect of Psychological Predictors on Drug Consumption Levels", 
      col.names   = c("Predictor", "Estimate", "SE", "t-statistic", "p-value"), 
      digits      = c(3, 3, 3, 3, 3)
  )
```

<table>

<caption>

Effect of Psychological Predictors on Drug Consumption Levels
</caption>

<thead>

<tr>

<th style="text-align:left;">

Predictor
</th>

<th style="text-align:right;">

Estimate
</th>

<th style="text-align:right;">

SE
</th>

<th style="text-align:right;">

t-statistic
</th>

<th style="text-align:right;">

p-value
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Oscore
</td>

<td style="text-align:right;">

0.075
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

6.778
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Cscore
</td>

<td style="text-align:right;">

-0.067
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

-5.908
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Nscore
</td>

<td style="text-align:right;">

0.031
</td>

<td style="text-align:right;">

0.011
</td>

<td style="text-align:right;">

2.905
</td>

<td style="text-align:right;">

0.004
</td>

</tr>

<tr>

<td style="text-align:left;">

SS
</td>

<td style="text-align:right;">

0.092
</td>

<td style="text-align:right;">

0.013
</td>

<td style="text-align:right;">

7.241
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

CountryCanada
</td>

<td style="text-align:right;">

-0.117
</td>

<td style="text-align:right;">

0.060
</td>

<td style="text-align:right;">

-1.961
</td>

<td style="text-align:right;">

0.050
</td>

</tr>

<tr>

<td style="text-align:left;">

CountryUK
</td>

<td style="text-align:right;">

-0.959
</td>

<td style="text-align:right;">

0.208
</td>

<td style="text-align:right;">

-4.609
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

EducationLeft school at 18 years
</td>

<td style="text-align:right;">

0.107
</td>

<td style="text-align:right;">

0.050
</td>

<td style="text-align:right;">

2.123
</td>

<td style="text-align:right;">

0.034
</td>

</tr>

<tr>

<td style="text-align:left;">

EducationLeft school before 16 years
</td>

<td style="text-align:right;">

0.220
</td>

<td style="text-align:right;">

0.073
</td>

<td style="text-align:right;">

3.024
</td>

<td style="text-align:right;">

0.003
</td>

</tr>

<tr>

<td style="text-align:left;">

Age45-54
</td>

<td style="text-align:right;">

-0.135
</td>

<td style="text-align:right;">

0.026
</td>

<td style="text-align:right;">

-5.175
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Age55-64
</td>

<td style="text-align:right;">

-0.239
</td>

<td style="text-align:right;">

0.038
</td>

<td style="text-align:right;">

-6.299
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

Age65+
</td>

<td style="text-align:right;">

-0.369
</td>

<td style="text-align:right;">

0.083
</td>

<td style="text-align:right;">

-4.418
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

<tr>

<td style="text-align:left;">

GenderM
</td>

<td style="text-align:right;">

0.136
</td>

<td style="text-align:right;">

0.017
</td>

<td style="text-align:right;">

8.086
</td>

<td style="text-align:right;">

0.000
</td>

</tr>

</tbody>

</table>

``` r
check_model(model_full, check = c("linearity", "outliers", "qq", "normality"))
```

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
model_full_nonlinear_no_interaction <- gam(log1p(user_consumption_level) ~ s(Oscore) + s(Cscore) +
                                 s(AScore) + s(Nscore) + s(Impulsive) + s(SS) + Country + Education +
                   Age + Gender + Ethnicity*Country, data = data_one)

model_full_nonlinear_interaction <- gam(log1p(user_consumption_level) ~ s(Oscore) + s(Cscore) +
                                 s(AScore) + s(Nscore) + s(Impulsive) + s(SS) + Country + Education +
                   Age + Gender + Ethnicity*Country +
                     ti(Oscore, Cscore, AScore, Nscore) + ti(Impulsive, SS), data = data_one)

knitr::kable(AIC(model_full, model_full_nonlinear_no_interaction, model_full_nonlinear_interaction))
```

|                                     |        df |      AIC |
|:------------------------------------|----------:|---------:|
| model_full                          | 104.00000 | 1161.297 |
| model_full_nonlinear_no_interaction |  54.59517 | 1167.055 |
| model_full_nonlinear_interaction    |  68.45005 | 1157.174 |

``` r
set.seed(123)

data_one_prime_use_no_use_clean <- data_one_prime_use_no_use |> select(
  Oscore, Cscore, Escore, Ascore, Nscore, Impulsive, SS, Country, Education,
  Age, Gender, Ethnicity, user_consumption_level
)

cv_df =
  crossv_mc(data_one_prime_use_no_use_clean, 20) |> 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))

cv_df = 
  cv_df |> 
  mutate(
    linear_mod  = map(train, \(df) lm(log1p(user_consumption_level) ~ (Oscore + Cscore +
                                 Ascore + Nscore + Impulsive + SS)^6 + Country + Education +
                   Age + Gender + Ethnicity*Country, data = df)),
    non_linear_no_interaction_mod     = map(train, \(df) gam(log1p(user_consumption_level) ~ s(Oscore) + s(Cscore) +
                                 s(Ascore) + s(Nscore) + s(Impulsive) + s(SS) + Country + Education +
                   Age + Gender + Ethnicity*Country, data = df)),
    smooth_mod  = map(train, \(df) gam(log1p(user_consumption_level) ~ s(Oscore) + s(Cscore) +
                                 s(Ascore) + s(Nscore) + s(Impulsive) + s(SS) + Country + Education +
                   Age + Gender + Ethnicity*Country +
                     ti(Oscore, Cscore, Ascore, Nscore) + ti(Impulsive, SS), data = as_tibble(df)))) |> 
  mutate(
    rmse_linear = map2_dbl(linear_mod, test, \(mod, df) rmse(model = mod, data = df)),
    rmse_non_linear_interaction    = map2_dbl(non_linear_no_interaction_mod, test, \(mod, df) rmse(model = mod, data = df)),
    rmse_smooth = map2_dbl(smooth_mod, test, \(mod, df) rmse(model = mod, data = df)))
```

    ## Warning: There were 13 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `rmse_linear = map2_dbl(...)`.
    ## Caused by warning in `predict.lm()`:
    ## ! prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 12 remaining warnings.

``` r
cv_df |> 
  select(starts_with("rmse")) |> 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") |> 
  mutate(model = fct_inorder(model)) |> 
  ggplot(aes(x = model, y = rmse, fill = model)) + geom_violin()
```

![](drug_consumption_analysis_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->
