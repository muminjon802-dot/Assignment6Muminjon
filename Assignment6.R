install.packages("dplyr")
library(dplyr)
library(dplyr)
set.seed(123)
customers <- data.frame(
  customer_id = 1:20,
  name = paste("Customer", 1:20),
  age = sample(18:70, 20, replace = TRUE),
  city = sample(c("Seoul", "Busan", "Incheon", "Daegu"), 20, replace = TRUE)
)
transactions <- data.frame(
  transaction_id = 1:50,
  customer_id = sample(1:25, 50, replace = TRUE),
  product = sample(c("Laptop", "Phone", "Tablet", "Headphones"), 50, replace = TRUE),
  amount = runif(50, 50, 500),
  purchase_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by="day"), 50)
)
customers_selected <- customers %>% select(customer_id, name, city)
transactions_selected <- transactions %>% select(transaction_id, customer_id, amount)
high_transactions <- transactions %>% filter(amount > 200)
seoul_customers <- customers %>% filter(city == "Seoul")
transactions_sorted <- transactions %>% arrange(desc(amount))
customers_sorted <- customers %>% arrange(age)
transactions <- transactions %>%
  mutate(discounted_amount = amount * 0.9)

customers <- customers %>%
  mutate(age_group = case_when(
    age < 30 ~ "Young",
    age >= 30 & age < 60 ~ "Middle",
    TRUE ~ "Senior"
  ))
# Average spending per customer
avg_spending <- transactions %>%
  group_by(customer_id) %>%
  summarise(avg_amount = mean(amount))
merged_data <- merge(transactions, customers, by = "customer_id")

city_spending <- merged_data %>%
  group_by(city) %>%
  summarise(total_spending = sum(amount))
merged_data <- merge(transactions, customers, by = "customer_id")
no_transaction_customers <- customers[is.na(match(customers$customer_id,
                                                  transactions$customer_id)), ]

n_no_transactions <- nrow(no_transaction_customers)
total_spending_customer <- transactions %>%
  group_by(customer_id) %>%
  summarise(total = sum(amount)) %>%
  arrange(desc(total))

top_customer <- total_spending_customer[1, ]
top_city <- city_spending %>%
  arrange(desc(total_spending)) %>%
  slice(1)
age_spending <- merged_data %>%
  group_by(age_group) %>%
  summarise(avg_spending = mean(amount))
n_no_transactions
total_before <- sum(transactions$amount)
total_after <- sum(transactions$discounted_amount)

discount_effect <- total_before - total_after
