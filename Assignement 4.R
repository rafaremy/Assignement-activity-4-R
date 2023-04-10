# Install tidyverse.
install.packages('tidyverse')

# Import tidyverse library.
library(tidyverse) 

# Import the data set (wages_plots.csv).
turtle_sales.csv <- read.csv(file.choose(), header=TRUE) 


# View the data frame.
View(turtle_sales.csv) 
as_tibble(turtle_sales.csv)

# View a summary of the data frame.
summary(turtle_sales.csv)

# Remove redundant columns ranking, year, genre, publisher
turtle_sales2 <- select(turtle_sales.csv, -Ranking, -Year, -Genre, -Publisher)

head(turtle_sales2)

# View summary.
summary(turtle_sales2)

# Create plots to review and determine insights
plot(hist(turtle_sales2$Global_Sales, col = "blue"))

# Create a scatterplot
x <- c('Product')
y <- c('Global_Sales')

# Plot a bar chart by passing the x-variable and data source, then set the geom type:
qplot('Global_Sales', data=turtle_sales2, geom='bar')


# Load the ggplot2 library
library(ggplot2)

# Create the scatterplot with custom formatting
ggplot(turtle_sales2, aes(x = Product, y = Global_Sales)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Global Sales by Product",
       x = "Product",
       y = "Global Sales (in millions)") +
  theme_bw()

# Load the dplyr package
library(dplyr)

# Group the data by product and calculate the mean and median global sales for each product
summary_stats <- turtle_sales2 %>%
  group_by(Product) %>%
  summarise(mean_sales = mean(Global_Sales),
            median_sales = median(Global_Sales))

# View the summary statistics
summary_stats

# Load the dplyr package
library(dplyr)

# Sort the data frame by global sales in ascending order , select 10 
least_sales <- turtle_sales2 %>%
  arrange(Global_Sales) %>%
  slice(1:10)

# View the row with the least global sales
least_sales

# Load the dplyr package
library(dplyr)

# Sort the data frame by global sales in descending order and select the top 10 rows
top_sales <- turtle_sales2 %>%
  arrange(desc(Global_Sales)) %>%
  slice_head(n = 10)

# View the top 10 rows with the most global sales
top_sales


# Load the ggplot2 package
library(ggplot2)

# Create a histogram of global sales
ggplot(turtle_sales2, aes(x = Global_Sales)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(x = "Global Sales", y = "Frequency", title = "Histogram of Global Sales")


################################################
#Summarise

#I've looked for patterns in data using summary statistics.
# With meand mand median on sales vs product, we can see that,
# product: 107, 123, 195, 231, 249 are the top 5 on 
#mean and median sales. The highest one is 107.

# The top 10 products with least sales are PC, GC, GB, PS2
# I would recommend to explore further why they are not selling well,
# I do recommend to consider modifying or discontinuing those products.
# As they're probably old generation.

# The top 10 most sold products are: Wii, NES, GB, DS.

# I would recommend to invest more resources, more marketing campaigns. 

# Monitor and adjust: Monitor the impact of any changes made based on your 
#recommendations and adjust strategies as needed to ensure ongoing success.
