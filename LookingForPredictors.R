library(openintro)
library(faraway)

## Remove columns with many NAs
loans_nona <- subset(loans_full_schema, 
                     select = -c(num_accounts_120d_past_due,
                                 months_since_last_credit_inquiry,
                                 months_since_90d_late,
                                 months_since_last_delinq,
                                 debt_to_income_joint,
                                 verification_income_joint,
                                 annual_income_joint,
                                 emp_length
                     ))

## Remove unimportant
loans_nona <- subset(loans_nona, 
                     select = -c(
                       state,
                       emp_title,
                       issue_month
                     ))

## Remove 24 entrie with NA dept_to_income
loans_nona <- na.omit(loans_nona)

## Remove some categoricals
loans_nona <- subset(loans_nona, 
                     select = -c(
                       loan_status,
                       initial_listing_status,
                       disbursement_method,
                       grade,
                       sub_grade
                     ))

loans_nona <- subset(loans_nona, 
                     select = -c(
                       homeownership,
                       verified_income,
                       application_type,
                       term
                     ))


dim(loans_nona)

loans_nona$homeownership <- as.factor(loans_nona$homeownership)
loans_nona$verified_income <- as.factor(loans_nona$verified_income)
loans_nona$application_type <- as.factor(loans_nona$application_type)
loans_nona$term <- as.factor(loans_nona$term)
#loans_nona$grade <- as.factor(loans_nona$grade)
#loans_nona$sub_grade <- as.factor(loans_nona$sub_grade)


sapply(loans_nona, function(x) sum(is.na(x)))

idxs <- 1:nrow(loans_nona)
ran_idx = sample(idxs, 2000)

loans_sampled <- loans_nona[ran_idx, ]

small <- lm(log(interest_rate) ~ grade, data = loans_sampled)

additive <- lm(log(interest_rate) ~ ., data = loans_sampled)

additive2 <- lm(interest_rate ~ ., data = loans_sampled)

slct_from_add <- step(additive, k = log(nrow(loans_sampled)), trace = 0)

additive_nograde <- lm(log(interest_rate) ~ . - grade, data = loans_sampled)

slct_from_add_nograde <- step(additive_nograde, k = log(nrow(loans_sampled)), trace = 0)

#inter <- lm(log(interest_rate) ~ .^2, data = loans_sampled)

#slct <- step(model, k = log(nrow(loans_sampled)), trace = 0)

summary(additive2)$r.squared
summary(additive2)$adj.r.squared

vif(slct_from_add_nograde)

## This 2 functions are in the report
diagnostic_graph(additive2)
diagnostics(slct_from_add)

plot(interest_rate ~ debt_to_income, 
     data = loans_sampled, col = "grey", pch = 20,
     main = "Data from Model 1")

#pairs(loans_sampled)









