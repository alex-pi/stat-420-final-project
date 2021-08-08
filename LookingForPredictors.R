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

# Remove due to collinearity
loans_nona <- subset(loans_nona,
                     select = -c(num_satisfactory_accounts,
                                 num_accounts_30d_past_due,
                                 installment,
                                 balance,
                                 num_open_cc_accounts,
                                 num_cc_carrying_balance,
                                 num_historical_failed_to_pay,
                                 paid_total,
                                 paid_principal,
                                 paid_interest,
                                 paid_late_fees,
                                 loan_purpose,
                                 #num_total_cc_accounts
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
#loans_nona$delinq_2y <- as.factor(loans_nona$delinq_2y)
#loans_nona$grade <- as.factor(loans_nona$grade)
#loans_nona$sub_grade <- as.factor(loans_nona$sub_grade)


sapply(loans_nona, function(x) sum(is.na(x)))

idxs <- 1:nrow(loans_nona)
ran_idx = sample(idxs, 7000)

loans_sampled <- loans_nona[ran_idx, ]

small <- lm(log(interest_rate) ~ grade, data = loans_sampled)

additive <- lm(log(interest_rate) ~ ., data = loans_sampled)

slct_from_add <- step(additive, k = log(nrow(loans_sampled)), trace = 0)

vif(slct_from_add)

additive_nograde <- lm(log(interest_rate) ~ . - grade, data = loans_sampled)



slct_from_add_nograde <- step(additive_nograde, k = log(nrow(loans_sampled)), trace = 0)

inter <- lm(log(interest_rate) ~ .^2, data = loans_sampled)

slct_from_inter <- step(inter, k = log(nrow(loans_sampled)), trace = 0)

summary(slct_from_add)$r.squared
summary(slct_from_add)$adj.r.squared

summary(slct_from_add)$r.squared
summary(slct_from_add)$adj.r.squared

anova(slct_from_add, additive)

vif(slct_from_add_nograde)

## This 2 functions are in the report
diagnostic_graph(slct_from_add)
diagnostics(slct_from_add)

plot(interest_rate ~ debt_to_income, 
     data = loans_sampled, col = "grey", pch = 20,
     main = "Data from Model 1")

#pairs(loans_sampled)

##### Trying transforms

inter <- lm(log(interest_rate) ~ 
              (verified_income
            + debt_to_income
            + delinq_2y 
            + inquiries_last_12m 
            + total_credit_lines 
            + total_credit_limit 
            + total_credit_utilized 
            + accounts_opened_24m 
            + num_active_debit_accounts 
            + total_debit_limit 
            + account_never_delinq_percent 
            + balance 
            + paid_total 
            + paid_principal)^2
            , data = loans_sampled)

slct_from_inter <- step(inter, k = log(nrow(loans_sampled)), trace = 0)


poly_all <- lm(log(interest_rate) ~ 
              poly(debt_to_income, degree = 3, raw = TRUE) 
            + poly(delinq_2y, degree = 3, raw = TRUE)
            + poly(inquiries_last_12m, degree = 3, raw = TRUE)
            + poly(total_credit_lines, degree = 3, raw = TRUE)
            + poly(total_credit_limit, degree = 3, raw = TRUE)
            + poly(total_credit_utilized, degree = 3, raw = TRUE)
            + poly(accounts_opened_24m, degree = 3, raw = TRUE)
            + poly(num_active_debit_accounts, degree = 3, raw = TRUE)
            + poly(total_debit_limit, degree = 3, raw = TRUE)
            + poly(account_never_delinq_percent, degree = 3, raw = TRUE)
            + poly(balance, degree = 3, raw = TRUE)
            + poly(paid_total, degree = 3, raw = TRUE)
            + poly(paid_principal, degree = 3, raw = TRUE)
            , data = loans_sampled)




diagnostic_graph(poly1)
diagnostics(poly1)

diagnostic_graph(poly_all)
diagnostics(poly_all)

diagnostic_graph(additive)
diagnostics(additive)

test1 <- lm(log(interest_rate) ~  
            + verified_income
            + debt_to_income
            + delinq_2y
            + inquiries_last_12m
            + total_credit_lines
            + poly(total_credit_limit, degree = 2, raw = TRUE)
            + total_credit_utilized
            + accounts_opened_24m
            + total_debit_limit
            + num_open_cc_accounts
            + num_cc_carrying_balance
            + num_mort_accounts
            + account_never_delinq_percent
            + loan_amount
            + term
            + installment
            + paid_total
            + paid_principal
            , data = loans_sampled)

diagnostic_graph(test1)
diagnostics(test1)

test2 <- lm(log(interest_rate) ~ 
              #poly(total_credit_limit, degree = 4, raw = TRUE)
              exp(total_credit_limit)
            , data = loans_sampled)

diagnostic_graph(test2)
diagnostics(test2)


predict(test1, newdata = data.frame(
  delinq_2y = "14"
))

