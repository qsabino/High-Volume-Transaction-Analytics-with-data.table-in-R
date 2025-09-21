

################## Fast Analytics using data.table in R#########################


# Load libraries
if (!require(data.table)) install.packages("data.table")
library(data.table)
library(dplyr)

# Load the CSV file
transactions_dt <- fread("transactions_dt.csv", stringsAsFactors = FALSE)
head(transactions_dt)



#=============== Practice exercises using data.table package ===================


# Add collum monetary to calculate revenue per transaction
transactions_dt[, monetary := price*quantity]

# Most popular product by sale volumn
transactions_dt[,.(total_sold = sum(price*quantity)), by = product_id][order(-total_sold)][1:10]

# Top 3 countries by revenue
transactions_dt[, .(revenue = sum(price*quantity)), by = country][order(-revenue)][1:3]

# Most used payment methods globally
transactions_dt[, .(most_used_method = .N), by = payment_method][order(-most_used_method)][1:3]

# Average basketsize (avg_quantity) per user
transactions_dt[, .(avg_quantity = mean(price*quantity)), by = user_id]

# Monthly revenue trend
transactions_dt[, month := format(timestamp, "%Y-%m")]
monthly_rev <- transactions_dt[, .(revenue = sum(price*quantity)), by = month][order(month)]
# visualize
library(ggplot2)
# Trick to convert to Date-type data
ggplot(monthly_rev, aes(x = as.Date(paste0(month, "-01")), y = revenue)) + 
  geom_line() + 
  labs(title = "Monthly Revenue Trend", x = "month/year") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of unique users per country
transactions_dt[, .(users_per_country = uniqueN(user_id)), by = country]

# Peak shopping hours
transactions_dt[, hour := hour(timestamp)]
transactions_dt[, .(num_transactions = .N), by = hour][order(-num_transactions)][1:5]

# Most loyal customers
transactions_dt[, .N, by = user_id][order(-N)][1:100]

# Product with highest total revenue
transactions_dt[, .(revenue = sum(monetary)), by = product_id][order(-revenue)][1:100]

# Much faster filtering: Find all transactions in 2023 over $300
transactions_dt[year(timestamp) == 2023 & price > 300]

# Load second CSV file
products_dt <- fread("products_dt.csv", stringsAsFactors = FALSE)

# Join with transactions_dt
setkey(transactions_dt, product_id) # for fast joins or lookups
setkey(products_dt, product_id)
join_data <- transactions_dt[products_dt, nomatch = 0] # inner join

# Revenue by category
join_data[, .(revenue = sum(price*quantity)), by = category]



# ============= Cluster users based on their behavior ==========================


user_behavior <- transactions_dt[, .(
  total_spent = sum(price * quantity),
  total_quantity = sum(quantity),
  num_transactions = .N,
  avg_basket_value = sum(price * quantity) / .N,
  unique_products = uniqueN(product_id),
  unique_countries = uniqueN(country),
  unique_payment_methods = uniqueN(payment_method)
), by = user_id]

head(user_behavior)

# Fesatures scaling, remove user_id before scaling
user_matrix <- as.data.frame(user_behavior[,-1])
user_scaled <- scale(user_matrix)

# K-Mean Clustering 
# Elbow method to find k (Can use factoextra or NbClust to find optimal k)
wss <- sapply(1:7, function(k){
  kmeans(user_scaled, centers = k, nstart = 25)$tot.withinss
})

plot(1:7, wss) # chose k = 4

set.seed(79)
kmeans_result <- kmeans(user_scaled, centers = 4, nstart = 25)

# Add cluster labels back to user_behavior data
user_behavior[, cluster := kmeans_result$cluster]

# interpret the clusters, uncovered hidden structure
cluster_summary <- user_behavior[, lapply(.SD, mean), by = cluster, 
              .SDcols = c("total_spent", "total_quantity", "num_transactions", "avg_basket_value")]
cluster_summary

# Visualize the finding
# Normalize each column (min-max scaling)
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
cluster_summary_norm <- cluster_summary[, lapply(.SD, normalize), .SDcols = -1]
cluster_summary_norm[, cluster := cluster_summary$cluster]
cluster_summary_norm

# data.table::melt() to long format
cluster_long <- melt(cluster_summary_norm, id.vars = "cluster")

ggplot(cluster_long, aes(x = variable, y = value, group = factor(cluster), color = factor(cluster))) +
  geom_line() +
  geom_point() +
  theme_minimal()

# Cluster	behavior interpretation:
# Cluster 1	High avg_basket_value, but low transaction count → "Premium Buyers"
# Cluster 2	Highest total spent, quantity, and transactions → "Power Users"
# Cluster 3	Moderate across all features → "Engaged Users"
# Cluster 4	Very few transactions, low spend → "Low-Value Users"


# Add cluster labels to transactions_dt for later use
transactions_dt <- merge(transactions_dt, user_behavior[, .(user_id, cluster)], 
                         by = "user_id", all.x = TRUE) # left join

# Group by cluster
transactions_dt[, .N, by = cluster]

# Distribution by cluster and country
transactions_dt[, .N, by = .(cluster, country)]


# Add cluster labels to join_data
join_data <- merge(join_data, user_behavior[, .(user_id, cluster)],
                   by = "user_id", all.x = TRUE)
# Top product-categories by cluster
top_categories <- join_data[,.(revenue = sum(price*quantity)), by = .(cluster, category)]
top_categories <- top_categories[, cluster := factor(cluster)]

ggplot(top_categories, aes(category, revenue, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster, scale = "free_y") +
  labs(title = "Categories by Revenue per Cluster")
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Time series per cluster, by month
monthly_cluster_rev <- transactions_dt[, .(revenue = sum(price*quantity)), by = .(cluster, month)]
monthly_cluster_rev[, month := as.Date(paste0(month, "-01"))] # Convert month to proper Date for plotting

ggplot(monthly_cluster_rev, aes(x = month, y = revenue, color = as.factor(cluster))) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Revenue by User Cluster",
    x = "Month",
    y = "Revenue",
    color = "Cluster"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# More time series per cluster, by week
transactions_dt[, week := as.Date(cut(timestamp, breaks = "week"))]
weekly_rev <- transactions_dt[, .(revenue = sum(price * quantity)), by = .(cluster, week)]
ggplot(weekly_rev, aes(x = week, y = revenue, color = as.factor(cluster))) +
  geom_line() +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 month") +
  labs(
    title = "Weekly Revenue by Cluster",
    x = "Week",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# More time series per cluster, by quarter
if(!require(zoo)) install.packages("zoo") # for as.yearqtr()

transactions_dt[, quarter := as.yearqtr(timestamp)]
quarterly_rev <- transactions_dt[, .(revenue = sum(price * quantity)), by = .(cluster, quarter)]
ggplot(quarterly_rev, aes(x = quarter, y = revenue, color = as.factor(cluster))) +
  geom_line() +
  labs(
    title = "Quarterly Revenue by Cluster",
    x = "Quarter",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#=========================== Churn Analysis ====================================


# Sort transaction by user and time to use data.table::shift()
setorder(transactions_dt, user_id, timestamp)

# Calculate time gaps between purchases per user
transactions_dt[, days_since_last_purchase := as.numeric(
  difftime(timestamp, shift(timestamp), units = "days")
), by = user_id]

# Get last purchase date per user
last_purchase_dt <- transactions_dt[, .(last_purchase = max(timestamp)), by = user_id]

# Calculate days since last purchase per user
today <- as.POSIXct("2023-12-31")
last_purchase_dt[, days_since_last := as.numeric(difftime(today, last_purchase, units = "days"))]

# Flag users as churned if inactivate for > 60 days
churn_threshold <- 60
last_purchase_dt[, churn_flag := days_since_last > churn_threshold]

last_purchase_dt[, .N, by = churn_flag]

# Merge churn status back into user_behavior data
user_behavior <- merge(user_behavior, last_purchase_dt, by = "user_id", all.x = TRUE)

ggplot(last_purchase_dt, aes(x = days_since_last, fill = churn_flag)) +
  geom_histogram(binwidth = 10, color = "white", alpha = 0.8) +
  labs(title = "Distribution of Days Since Last Purchase", x = "Days Since Last", y = "User Count") +
  theme_minimal()
