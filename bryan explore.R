loans_nona = subset(loans_nona, select = -c(grade, sub_grade))

idxs <- 1:nrow(loans_nona)
ran_idx = sample(idxs, 3000)
loans_sampled = loans_nona[ran_idx,]

loans_num = subset(loans_sampled, select = -c(homeownership, verified_income, loan_purpose, application_type, term))

loans_sampled = subset(loans_sampled, select = -c(current_accounts_delinq, num_accounts_30d_past_due))

round(cor(loans_num), 3)


# [num_satisfactory_accounts, open_credit_lines]
# [num_acc_30d_past_due, curr_accounts_delinq]
# [installment, loan_amount]
# [loan_amount, balance]

loans_sampled = subset(loans_sampled, select = -c(num_satisfactory_accounts, num_accounts_30d_past_due, installment, balance))
loans_num = subset(loans_sampled, select = -c(homeownership, verified_income, loan_purpose, application_type, term))

add_model = lm(interest_rate ~ ., data = loans_sampled)
vif(add_model)[vif(add_model) > 5]

# [num_open_cc_accounts, open_credit_lines]
# [num_total_cc_accounts, num_open_cc_accounts]
# [num_cc_carrying_balance, num_open_cc_accounts]
# [public_record_bankrupt, num_historical_failed_to_pay]
# [num_cc_carrying_balance, num_active_debit_accounts]

loans_sampled = subset(loans_sampled, select = -c(num_open_cc_accounts,
                                                  num_cc_carrying_balance,
                                                  num_historical_failed_to_pay))
add_model = lm(interest_rate ~ ., data = loans_sampled)
round(vif(add_model)[vif(add_model) > 5],2)
length(coef(add_model))

# loans_sampled = subset(loans_sampled, select = -c(total_collection_amount_ever))
# add_model = lm(interest_rate ~ ., data = loans_sampled)
# log_model = lm(log(interest_rate) ~ ., data = loans_sampled)
# 
# vif(add_model)[vif(add_model) > 5]
# vif(log_model)[vif(log_model) > 5]
# 
# loans_sampled = subset(loans_sampled, select = -c(total_debit_limit,
#                                                   account_never_delinq_percent))
# add_model = lm(interest_rate ~ ., data = loans_sampled)
# log_model = lm(log(interest_rate) ~ ., data = loans_sampled)
# 
# loans_sampled = subset(loans_sampled, select = -c(num_mort_accounts))
# add_model = lm(interest_rate ~ ., data = loans_sampled)
# log_model = lm(log(interest_rate) ~ ., data = loans_sampled)
# 
# loans_sampled = subset(loans_sampled, select = -c(num_total_cc_accounts))
# 
# add_model = lm(interest_rate ~ ., data = loans_sampled)
# log_model = lm(log(interest_rate) ~ ., data = loans_sampled)

loans_sampled = subset(loans_sampled, select = -c(loan_purpose))

add_model = lm(interest_rate ~ ., data = loans_sampled)
log_model = lm(log(interest_rate) ~ ., data = loans_sampled)