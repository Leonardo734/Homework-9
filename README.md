# Homework-9
homework9

My group memebers: Leonardo Alcaide, Sun Wu Kim, Arifa Begum, Maria Camlia Vergas, Nene Diallo. 


library(ggplot2)
library(tidyverse)
library(haven)
library(car)  
library(lmtest)  
library(vip)  


load("C:/Users/camil/Downloads/ACS_2021_couples.RData")
acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")

acs2021_couples$educ_numeric <- as.numeric(levels(acs2021_couples$educ_numeric))[acs2021_couples$educ_numeric]

acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                             "0" = "N/A or no schooling",
                                             "2" = "Nursery school to grade 4",
                                             "6.5" = "Grade 5, 6, 7, or 8",
                                             "9" = "Grade 9",
                                             "10" = "Grade 10",
                                             "11" = "Grade 11",
                                             "12" = "Grade 12",
                                             "13" = "1 year of college",
                                             "14" = "2 years of college",
                                             "15" = "3 years of college",
                                             "16" = "4 years of college",
                                             "17" = "5+ years of college")

acs2021_couples$h_educ_numeric <- as.numeric(levels(acs2021_couples$h_educ_numeric))[acs2021_couples$h_educ_numeric]

acs2021_couples$educ_diff <- acs2021_couples$educ_numeric - acs2021_couples$h_educ_numeric
acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")

acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1",
                                     "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4",
                                     "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7",
                                     "two races" = "8",
                                     "three races" = "9")

acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                       "Not Hispanic" = "0",
                                       "Mexican" = "1",
                                       "Puerto Rican" = "2",
                                       "Cuban" = "3",
                                       "Other" = "4")
trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )

trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)
table(trad_data$he_more_than_5yrs_than_her,cut(trad_data$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)
ols_out2 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)
summary(ols_out2)
ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE, data = trad_data)
summary(ols_out3)
ols_out4 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + STATEFIP, data = trad_data)
summary(ols_out4)



model_us <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + FAMSIZE + NCHILD, data = trad_data)
summary(model_us)


library(ggplot2)
library(dplyr)

summary_data <- trad_data %>%
  group_by(FAMSIZE) %>%
  summarise(avg_age_diff = mean(he_more_than_5yrs_than_her, na.rm = TRUE))

colors <- rep(c("purple", "pink"), length.out = nrow(summary_data))

ggplot(summary_data, aes(x = as.factor(FAMSIZE), y = avg_age_diff, fill = as.factor(FAMSIZE))) +
  geom_bar(stat = "identity") +  # Use 'stat = "identity"' for raw values
  scale_fill_manual(values = colors) +  # Apply the alternating colors
  labs(title = "Average Age Difference by Family Size",
       x = "Family Size",
       y = "Average Age Difference (Years)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend


library(ggplot2)
library(dplyr)


trad_data$predicted_prob <- predict(model_us, newdata = trad_data, type = "response")

ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)


pred_vals_ols1 <- predict(ols_out1, trad_data)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))  # You can change the threshold if needed


ols_conf_matrix <- table(pred = pred_model_ols1, true = trad_data$he_more_than_5yrs_than_her)
print("Confusion Matrix for OLS:")
print(ols_conf_matrix)


model_logit1 <- glm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data, family = binomial)


pred_vals_logit <- predict(model_logit1, trad_data, type = "response")
pred_model_logit <- (pred_vals_logit > 0.5)  # You can try different thresholds (e.g., > mean(pred_vals_logit))


logit_conf_matrix <- table(pred = pred_model_logit, true = trad_data$he_more_than_5yrs_than_her)
print("Confusion Matrix for Logit:")
print(logit_conf_matrix)


ols_hypothesis_test <- linearHypothesis(ols_out1, c("educ_hs = 0", "educ_somecoll = 0", "educ_college = 0", "educ_advdeg = 0", "AGE = 0"))
print("OLS Hypothesis Test (all coefficients except intercept):")
print(ols_hypothesis_test)


logit_lr_test <- lrtest(model_logit1)
print("Logit Likelihood Ratio Test:")
print(logit_lr_test)


trad_data$pred_logit <- pred_vals_logit  


ggplot(trad_data, aes(x = pred_logit, color = as.factor(he_more_than_5yrs_than_her))) +
  geom_histogram(binwidth = 0.05, alpha = 0.6, position = "identity") +
  labs(title = "Logit Model Predicted Probabilities vs Actual Outcomes", x = "Predicted Probability", y = "Frequency") +
  theme_minimal()


vip(model_logit1)

library(ggplot2)
library(tidyverse)
library(haven)
load("~/Desktop/ACS_2021_couples.RData")
View(acs2021_couples)
```
```{r}
#I will be examining whether race and/or state the persons lives affects the age gap among couples in the 95th confidence interval.
acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")

acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1",
                                     "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4",
                                     "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7",
                                     "two races" = "8",
                                     "three races" = "9")
acs2021_couples$age_diff <- acs2021_couples$AGE - acs2021_couples$h_age
summary(acs2021_couples$AGE[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$h_age[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
```
```{r}
# OLS Model
Sub1 <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )
Sub1$he_more_than_5yrs_than_her <- as.numeric(Sub1$age_diff < -5)
Sub_model <- lm(he_more_than_5yrs_than_her ~ STATEFIP + h_race + RACE+ AGE, data = Sub1)
summary(Sub_model)
```
```{r}
#Predictted Values: 
pred_vals_Sub1 <- predict(Sub_model, Sub1)
pred_model_Sub1 <- (pred_vals_Sub1 > mean(pred_vals_Sub1))
table(pred = pred_model_Sub1, true = Sub1$he_more_than_5yrs_than_her)
```
```{r}
#The table shows four possible outcomes:
#1: True Negative (FALSE, 0): 183,991 cases where the model correctly predicts the age difference is not more than 5 years.
#2: False Negative (FALSE, 1): 36,867 cases where the model incorrectly predicts the age difference is not more than 5 years, but in reality, it is.
#3: False Positive (TRUE, 0): 147,186 cases where the model incorrectly predicts the age difference is more than 5 years, but in reality, it is not.
#4: True Positive (TRUE, 1): 44,231 cases where the model correctly predicts the age difference is more than 5 years.
# GRAPH
# Assuming conf_matrix is already created as:
conf_matrix <- table(pred = pred_model_Sub1, true = Sub1$he_more_than_5yrs_than_her)
# Convert the table to a data frame for ggplot
conf_matrix_df <- as.data.frame(conf_matrix)
# Plot a simple bar graph
ggplot(conf_matrix_df, aes(x = pred, y = Freq, fill = as.factor(true))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Gap Prediction Outcomes (5+ Years Older)", x = "Prediction", y = "Count") +
  scale_fill_manual(name = "True Value", values = c("blue", "red"))
```
```{r}
#Logistic regression model
model_logit_Sub1 <- glm(he_more_than_5yrs_than_her ~ STATEFIP + h_race + RACE+ AGE, data = Sub1, family = binomial)
summary(model_logit_Sub1)
```
```{r}
#Predicted Values for LR
pred_vals <- predict(model_logit_Sub1, Sub1, type = "response")
pred_model_logit_Sub1 <- (pred_vals > mean(pred_vals))
table(pred = pred_model_logit_Sub1, true = Sub1$he_more_than_5yrs_than_her)
```
```{r}
#1True Positives (TP): 42,123 cases where the model correctly predicts an age gap greater than 5 years.
#2True Negatives (TN): 192,414 cases where the model correctly predicts an age gap of 5 years or less.
#3False Positives (FP): 138,763 cases where the model over-predicts an age gap of more than 5 years.
#4False Negatives (FN): 38,975 cases where the model under-predicts an age gap of 5 years or less.
# Generate the confusion matrix from the new predictions
conf_matrix_logit <- table(pred = pred_model_logit_Sub1, true = Sub1$he_more_than_5yrs_than_her)
# Convert the table to a data frame for ggplot
conf_matrix_logit_df <- as.data.frame(conf_matrix_logit)
# Plot a simple bar chart
ggplot(conf_matrix_logit_df, aes(x = pred, y = Freq, fill = as.factor(true))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Logistic Model Prediction Outcomes (5+ Years Age Gap)",
    x = "Predicted Outcome",
    y = "Count",
    fill = "Actual Outcome"
  ) +
  scale_fill_manual(name = "Actual Outcome", values = c("Purple", "Green")) +
  theme_minimal()
library(ggplot2)
library(tidyverse)
library(haven)

load("/Users/jaydenkim/Desktop/Econometrics/ACS_2021_couples.RData")

```
```{r}
# Filter data for female-male couples and create age difference indicator
trad_data <- acs2021_couples %>% filter((SEX == "Female") & (h_sex == "Male"))
trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)

```

```{r}
# Recode education levels to numeric values for both partners
trad_data$educ_numeric <- fct_recode(trad_data$EDUC,
                                     "0" = "N/A or no schooling",
                                     "2" = "Nursery school to grade 4",
                                     "6.5" = "Grade 5, 6, 7, or 8",
                                     "9" = "Grade 9",
                                     "10" = "Grade 10",
                                     "11" = "Grade 11",
                                     "12" = "Grade 12",
                                     "13" = "1 year of college",
                                     "14" = "2 years of college",
                                     "15" = "3 years of college",
                                     "16" = "4 years of college",
                                     "17" = "5+ years of college")
trad_data$educ_numeric <- as.numeric(levels(trad_data$educ_numeric))[trad_data$educ_numeric]

trad_data$h_educ_numeric <- fct_recode(trad_data$h_educ,
                                       "0" = "N/A or no schooling",
                                       "2" = "Nursery school to grade 4",
                                       "6.5" = "Grade 5, 6, 7, or 8",
                                       "9" = "Grade 9",
                                       "10" = "Grade 10",
                                       "11" = "Grade 11",
                                       "12" = "Grade 12",
                                       "13" = "1 year of college",
                                       "14" = "2 years of college",
                                       "15" = "3 years of college",
                                       "16" = "4 years of college",
                                       "17" = "5+ years of college")
trad_data$h_educ_numeric <- as.numeric(levels(trad_data$h_educ_numeric))[trad_data$h_educ_numeric]


```

```{r}
# Filter rows with NA values in relevant variables
trad_data <- trad_data %>%
  filter(!is.na(educ_numeric), !is.na(h_educ_numeric), !is.na(AGE), !is.na(he_more_than_5yrs_than_her))


```

```{r}
# Linear Model (OLS)
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)

# Predicted values and confusion matrix for OLS
pred_vals_ols1 <- predict(ols_out1, trad_data)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = trad_data$he_more_than_5yrs_than_her)

```

```{r}
# Logistic Model
model_logit1 <- glm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, 
                    data = trad_data, family = binomial)
summary(model_logit1)

```

```{r}
pred_vals <- predict(model_logit1, trad_data, type = "response")
pred_model_logit1 <- (pred_vals > mean(pred_vals))
table(pred = pred_model_logit1, true = trad_data$he_more_than_5yrs_than_her)
```

```{r}
# Predictions and confusion matrix for logistic model
pred_vals_logit <- predict(model_logit1, trad_data, type = "response")
pred_model_logit1 <- (pred_vals_logit > mean(pred_vals_logit))
table(pred = pred_model_logit1, true = trad_data$he_more_than_5yrs_than_her)

```


```{r}
# Probit Model
model_probit1 <- glm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, 
                     data = trad_data, family = binomial(link = "probit"))
summary(model_probit1)

```



```{r}
# Predictions and confusion matrix for probit model
pred_vals_probit <- predict(model_probit1, trad_data, type = "response")
pred_model_probit1 <- (pred_vals_probit > mean(pred_vals_probit))
table(pred = pred_model_probit1, true = trad_data$he_more_than_5yrs_than_her)

```


```{r}
# Plot predicted probabilities for logistic model vs AGE
ggplot(trad_data, aes(x = AGE, y = pred_vals_logit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Logistic Model: Predicted Probability vs Age",
       x = "Age of Female Partner", y = "Predicted Probability") +
  theme_minimal()

```


```{r}
# Plot predicted probabilities for probit model vs AGE
ggplot(trad_data, aes(x = AGE, y = pred_vals_probit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "purple") +
  labs(title = "Probit Model: Predicted Probability vs Age",
       x = "Age of Female Partner", y = "Predicted Probability") +
  theme_minimal()

```


```{r}
# Analysis of false positives and false negatives for logistic model
false_positive_rate <- sum(pred_model_logit1 == 1 & trad_data$he_more_than_5yrs_than_her == 0) / sum(trad_data$he_more_than_5yrs_than_her == 0)
false_negative_rate <- sum(pred_model_logit1 == 0 & trad_data$he_more_than_5yrs_than_her == 1) / sum(trad_data$he_more_than_5yrs_than_her == 1)
print(paste("False Positive Rate for Logit Model:", round(false_positive_rate, 3)))
print(paste("False Negative Rate for Logit Model:", round(false_negative_rate, 3)))


```


```{r}
# Hypothesis Test: Likelihood of significant age difference based on female partner's age
# Null Hypothesis: Older female partners are just as likely to have a significant age difference with their male partner as younger female partners. (age_diff = age)
# Alternative Hypothesis: Older female partners are less likely to have a significant age difference with their male partner. (age_diff < age)
# Confidence Level: 95%, p-value = 0.017, left-tailed test

p_value <- 0.017
alpha <- 0.05

if (p_value < alpha) {
  result <- "Reject the null hypothesis. Older female partners are less likely to have a significant age difference with their male partner than younger female partners."
} else {
  result <- "Fail to reject the null hypothesis. Older female partners are just as likely to have a significant age difference with their male partner as younger female partners."
}

print(result)

```{r}
# Load necessary library
library(ggplot2)
library(dplyr)
# Summarize the data to get the average age difference for each family size
summary_data <- trad_data %>%
  group_by(MORTGAGE) %>%
  summarise(avg_age_diff = mean(he_more_than_5yrs_than_her, na.rm = TRUE))
# Create a vector of alternating colors
colors <- rep(c("purple", "black"), length.out = nrow(summary_data))
# Create the bar plot with alternating colors
ggplot(summary_data, aes(x = as.factor(MORTGAGE), y = avg_age_diff, fill = as.factor(MORTGAGE))) +
  geom_bar(stat = "identity") +  # Use 'stat = "identity"' for raw values
  scale_fill_manual(values = colors) +  # Apply the alternating colors
  labs(title = "Average Age Difference by Family Size",
       x = "Mortgage",
       y = "Average Age Difference (Years)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend
```

library(ggplot2)
library(tidyverse)
library(haven)
library(ggthemes)

load("ACS_2021_couples.RData")
trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )
trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
pred_vals_ols1 <- predict(ols_out1, trad_data)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = trad_data$he_more_than_5yrs_than_her)
model_logit1 <- glm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data, family = binomial)
summary(model_logit1)
pred_vals <- predict(model_logit1, trad_data, type = "response")
pred_model_logit1 <- (pred_vals > mean(pred_vals))
table(pred = pred_model_logit1, true = trad_data$he_more_than_5yrs_than_her)
ggplot(data=ols_out1, mapping = aes(x = AGE, y = educ_hs))+
geom_point(alpha = 0.5) + geom_smooth(method ="gam")+
theme_economist() + scale_x_log10() + labs(title = "Age and High School Education", y="High School degree")
```
```{r cars}
pred_model_logit2 <- (pred_vals > 0.5)
table(pred = pred_model_logit2, true = trad_data$he_more_than_5yrs_than_her)
ggplot(data=ols_out1, mapping = aes(x = AGE, y = educ_advdeg))+
geom_point(alpha = 0.5) + geom_smooth(method ="gam")+
theme_economist()+ scale_x_log10() +labs(title = "Age and Advanced Degree", y = "Advanced Degree")
```
ols_outA <- lm(he_more_than_5yrs_than_her  ~ FOODSTMP + MORTGAGE + AGE + NCHILD, data = trad_data)
pred_vals_olsA <- predict(ols_outA, trad_data)
pred_model_olsA <- (pred_vals_olsA > mean(pred_vals_olsA))
model_logit3 <- glm(he_more_than_5yrs_than_her ~  FOODSTMP + MORTGAGE + AGE, data = trad_data, family = binomial)
table(pred = pred_model_olsA, true = trad_data$he_more_than_5yrs_than_her)
pred_vals2 <- predict(model_logit3, trad_data, type = "response")
pred_model_logit4 <- (pred_vals2 < mean(pred_vals2))
table(pred = pred_model_logit4, true = trad_data$he_more_than_5yrs_than_her)
ggplot(data = ols_outA, mapping = aes(x = AGE, y = FOODSTMP)) +
geom_point(alpha= 0.5) + geom_smooth(method = "gam") +
  theme_economist() + scale_x_log10()+ labs(title = "Age and Food Stamps", y = "Food Stamps")





Articles: 


Card, David. “Immigration and Inequality.” The American Economic Review, vol. 99, no. 2, 2009, pp. 1–21. JSTOR, http://www.jstor.org/stable/25592368. Accessed 14 Nov. 2024. 
This article goes over immigration and inequality. It measures immigration of unskilled immigrants and natives. Most of the data is collected municipalities data in the United States so it is accessible. The Econometrics tools are the data sets of immigrants with their gender, ethnicity, education, and wages as well as scatter plots of immigrant populations. 

Beerli, Andreas, et al. “The Abolition of Immigration Restrictions and the Performance of Firms and Workers: Evidence from Switzerland.” The American Economic Review, vol. 111, no. 3, 2021, pp. 976–1012. JSTOR, https://www.jstor.org/stable/27027278. Accessed 13 Nov. 2024. 
This article goes over experiment in Switzerland had an open border policy with EU citizens. The research found that the open border policy allowed for greater innovation and research for firms without decreasing the wages or employment of native workers. The data is mostly from local municipalities or the EU, which the former would be difficult to access. The Econometrics tools are mean, standard deviations, linear models and whole data sets of the demographics of the migrants given their age and education with the industry of the firm.  
