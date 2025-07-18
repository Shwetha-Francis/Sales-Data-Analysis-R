
#  “SALES ANALYTICS USING R”

getwd()                               # Check working directory
list.files(pattern = ".csv$")         # View CSV files
data <- read.csv("sales_dataa.csv")   # Load dataset
head(data)
summary(data)


# Data Cleaning 


#Show & Count All Missing Values
is.na(data)                           # Show NA locations
sum(is.na(data))                      # Total missing values
colSums(is.na(data))                  # Missing by column

#Replace NA in Units_Sold and Unit_Price with Column Mean
data$Units_Sold[is.na(data$Units_Sold)] <- mean(data$Units_Sold, na.rm = TRUE)
data$Unit_Price[is.na(data$Unit_Price)] <- mean(data$Unit_Price, na.rm = TRUE)

#Drop Rows Where Revenue is NA
before <- nrow(data)                  # Before cleaning
data <- data[!is.na(data$Revenue), ]  # Drop NA revenue
after <- nrow(data)                   # After cleaning


#️ Date Features with lubridate

library(lubridate)

#Convert Date to Date type
data$Date <- mdy(data$Date)           # Use mdy() for format like "1/10/2023"

#Add Year, Month (label), Weekday (label)
data$Year <- year(data$Date)
data$Month <- month(data$Date, label = TRUE)
data$Weekday <- wday(data$Date, label = TRUE)

# Month with Highest Revenue
rev_by_month <- aggregate(Revenue ~ Month, data = data, sum)
highest_month <- rev_by_month$Month[which.max(rev_by_month$Revenue)]
cat("Month with highest revenue:", highest_month, "\n")

# Day with Lowest Average Units Sold
avg_units_by_day <- aggregate(Units_Sold ~ Weekday, data = data, mean)
lowest_day <- avg_units_by_day$Weekday[which.min(avg_units_by_day$Units_Sold)]
cat("Day with lowest average units sold:", lowest_day, "\n")

# Loops & Conditional Logic


# For loop to print monthly revenue
cat("\n Monthly Revenue:\n")
months <- unique(data$Month)
for (m in months) {
  total <- sum(data$Revenue[data$Month == m])
  cat(as.character(m), ":", total, "\n")
}

#Create Performance Column
avg_revenue <- mean(data$Revenue)
data$Performance <- ifelse(data$Revenue > avg_revenue, "High", "Low")


#Grouping, Aggregation & Custom Function


# Grouping
# Total Revenue by Region
print(aggregate(Revenue ~ Region, data = data, sum))

# Average Units Sold by Product
print(aggregate(Units_Sold ~ Product, data = data, mean))

# Region with Highest Avg Revenue
avg_rev_region <- aggregate(Revenue ~ Region, data = data, mean)
print(avg_rev_region)
best_region <- avg_rev_region$Region[which.max(avg_rev_region$Revenue)]
cat(" Region with highest avg revenue:", best_region, "\n")

#Custom Function
region_report <- function(region_name) {
  sub <- data[data$Region == region_name, ]
  total_rev <- sum(sub$Revenue, na.rm = TRUE)
  top_product <- names(which.max(tapply(sub$Revenue, sub$Product, sum, na.rm = TRUE)))
  high_rev_days <- sub$Date[sub$Revenue > mean(data$Revenue, na.rm = TRUE)]
  
  return(list(
    Region = region_name,
    Total_Revenue = total_rev,
    Top_Product = top_product,
    High_Revenue_Days = high_rev_days
  ))
}


#Visualizations with ggplot2

library(ggplot2)

#Bar Chart: Region vs Revenue
ggplot(data, aes(x = Region, y = Revenue, fill = Region)) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Total Revenue by Region")

#Line Chart: Date vs Revenue
ggplot(data, aes(x = Date, y = Revenue)) +
  geom_line(color = "steelblue", size = 1) +
  ggtitle("Revenue Over Time")

#Stacked Bar: Product vs Units Sold (fill = Performance)
ggplot(data, aes(x = Product, y = Units_Sold, fill = Performance)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Units Sold by Product (Stacked by Performance)")