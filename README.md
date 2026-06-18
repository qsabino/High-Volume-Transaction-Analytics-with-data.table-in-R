## High-Volume Transaction Analytics with data.table in R


### 1. Why data.table?
Using data.table package allows the project to process millions of transactions quickly, enabling real-time-like analytics even on a personal machine. This approach is ideal for high-volume retail, fintech, or behavioral tracking applications.

### 2. Data Files
- **transactions_dt.csv:** Transactional data (1M+ rows)
- **products_dt.csv:** Product info including categories

### 3. Project Structure
- **Data Loading:**	Reads transaction and product CSV files using fread()
- **Data Wrangling:** Calculates total revenue, monthly trends, top products, etc.
- **Joined Data:** Combines product metadata with transactions for deeper insight
- **Visualization:** Time series plots, bar charts, and cluster patterns
- **Clustering:** Segments users using K-Means based on behavior
- **Churn Analysis:** Flags users who haven't returned in 60 days
