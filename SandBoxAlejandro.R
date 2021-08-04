library(MASS)


loans_data <- subset(loans_full_schema, 
           select = c("interest_rate",
                   #"debt_to_income_joint",
                   "annual_income",
                   "inquiries_last_12m",
                   "total_credit_utilized",
                   "current_installment_accounts",
                   "accounts_opened_24m",
                   "num_satisfactory_accounts",
                   "num_active_debit_accounts",
                   "total_debit_limit",
                   "num_open_cc_accounts",
                   "num_cc_carrying_balance",
                   "homeownership",
                   "verified_income"))

loans_data$homeownership <- as.factor(loans$homeownership)

loans_data$verified_income <- as.factor(loans$verified_income)

loans_data <- na.omit(loans_data)

dim(loans_data)

View(loans_data)

sapply(loans_data, function(x) sum(is.na(x)))




# Get some random indexes for observations
idxs <- 1:nrow(loans_data)
ran_idx = sample(idxs, 500)

loans_sampled <- loans_data[ran_idx, ]

pairs(loans_sampled, col = "dodgerblue")


additive_mod <- lm(interest_rate ~ ., data = loans_sampled)
additive_mod <- lm(log(interest_rate) ~ ., data = loans_sampled)

par(mfrow=c(1, 1), bg="ghostwhite")
boxcox(additive_mod, plotit = TRUE, lambda = seq(-0.8, 0.8, by = 0.1))

vif(additive_mod)

summary(additive_mod)$r.squared

diagnostics(additive_mod, testit = FALSE)

