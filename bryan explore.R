loans_nona = subset(loans_nona, select = -c(grade, sub_grade))

ran_idx = sample(idxs, 3000)
loans_sampled = loans_nona[ran_idx,]

loans_num = subset(loans_sampled, select = -c(homeownership, verified_income, loan_purpose, application_type, term))

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
                                                  num_historical_failed_to_pay,
                                                  paid_total,
                                                  paid_principal,
                                                  paid_interest,
                                                  paid_late_fees))
add_model = lm(interest_rate ~ ., data = loans_sampled)
vif(add_model)[vif(add_model) > 5]
length(coef(add_model))
