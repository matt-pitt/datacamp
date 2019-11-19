#chapter 1 - We went through the ticket sales data and importing the data from csv.
#used the usual, glimpse, head etc. Also used separate and unite, used lapply and sapply and some handy knowledge 
#on referring to certain sections of a dataset we wish to investigate further (missing values etc.)

library(tidyverse)

#importing data with datacamp
#useful code to delete columns using base

my_df[1:5, ] # First 5 rows of my_df
my_df[, 4]   # Fourth column of my_df

my_df[-(1:5), ] # Omit first 5 rows of my_df
my_df[, -4]     # Omit fourth column of my_df

keep <-
  5:(ncol(sales2) - 15) #keep first 5 and count all columns and minus 15 (to remove the 15 irrelevant columns), this is the
#type of thinking I need to start doing. Instead of counting all the columns and working that way.

sales5 <- separate(sales4,
                   sales_ord_create_dttm,
                   c("ord_create_dt", "ord_create_time"),
                   sep = " ")
#note use of the concatenate to "name" my new columns.

sales3$sales_ord_create_dttm[issues] #I was able to find specific issues. What fooled me was the lack of need to allocate
#an indices to the column portion e.g [, ]. The column has already been referred to with use of "$"

#lapply syntax
lapply(my_data_frame[, cols], function_name)

# Find columns of sales5 containing "dt": date_cols
date_cols <- str_detect(names(sales5), "dt")

# Coerce date columns into Date objects using lapply. I stumbled on the use of date_cols. Also i opened brackets on ymd() which confused lapply.
sales5[, date_cols] <- lapply(sales5[, date_cols], ymd)

#finding missing data using sapply
# Find date columns (don't change)
date_cols <- str_detect(names(sales5), "dt")

# Create logical vectors indicating missing values (don't change)
missing <- lapply(sales5[, date_cols], is.na)

# Create a numerical vector that counts missing values: num_missing
num_missing <- sapply(missing, sum)


#Chapter 2 - 
# Remove rows 1, 7, and 11 of mbta: mbta2
mbta2 <- mbta[-c(1,7,11),]

# Remove the first column of mbta2: mbta3
mbta3 <- mbta2[,-1]

# Gather columns of mbta3: mbta4
mbta4 <- gather(mbta3, key = "month", value = "thou_riders", - mode)

# Spread the contents of mbta4: mbta5
mbta5 <- spread(mbta4, mode, thou_riders)

# Split month column into month and year: mbta6
mbta6 <- separate(mbta5, month, c("year", "month"), sep = "-")

# Find the row number of the incorrect value: i
i <- which(mbta6$Boat > 30)

# Replace the incorrect value with 4
mbta6$Boat[i] <- 4

#Chapter 3
#good way to type in the column names and make a nice piece of code to remove the irrelevant columns
# Define vector of duplicate cols (don't change)

duplicates <- c(4, 6, 11, 13, 15, 17, 18, 20, 22,
                24, 25, 28, 32, 34, 36, 38, 40,44, 46, 48, 51, 54, 65, 158)

# Remove duplicates from food: food2
food2 <- food[, -duplicates]

# Define useless vector (don't change)
useless <- c(1, 2, 3, 32:41)

# Remove useless columns from food2: food3. This removes columns.
food3 <- food2[, -useless]

# Create vector of column indices: nutrition
nutrition <- str_detect(names(food3), "100g")

#Changing the value of NA's to ZERO
# Find indices of sugar NA values: missing
missing <- is.na(food3$sugars_100g)

# Replace NA values with 0
food3$sugars_100g[missing] <- 0

# Create first histogram
hist(food3$sugars_100g, breaks = 100)

# Create food4
food4 <- food3[food3$sugars_100g > 0, ]

# Create second histogram
hist(food4$sugars_100g, breaks = 100)



# counting the number of items which contain the string "plasti"
# Find entries containing "plasti": plastic

plastic <- str_detect(food3$packaging, "plasti")


# Print the sum of plastic

sum(plastic)

#Chapter 4 
# Create remove
remove <- c(3, 56:59)

# Create att2 - this removes rows
att2 <- att[-remove , ]

# Create remove
remove <- c(3, 5, 7, 9, 11, 13, 15, 17)

# Create att3 - This removes the rows.
att3 <- att2[ ,-remove]

#splitting up the dataset, indicator of this is merged cells in xls files
# Subset just elementary schools: att_elem
att_elem <- att3[ , c(1, 6:7)]
# Subset just secondary schools: att_sec
att_sec <- att3[ , c(1, 8:9)]
# Subset all schools: att4
att4 <- att3[, 1:5]


#renaming columns, and removin some more rows
# Define cnames vector (don't change)
cnames <- c("state", "avg_attend_pct", "avg_hr_per_day", 
            "avg_day_per_yr", "avg_hr_per_yr")
# Assign column names of att4
colnames(att4) <- cnames
# Remove first two rows of att4: att5
att5 <- att4[-c(1, 2), ]
# View the names of att5
names(att5)

#Removing all those nonsense full stops the use of "\\." means find any "." and replace with white space
# Remove all periods in state column
att5$state <- str_replace_all(att5$state, "\\.", "")

# Remove white space around state names
att5$state <- str_trim(att5$state)

# Dplyr version to coerce to as.numeric, there is also a way to do it by subsetting using base
# Change columns to numeric using dplyr (don't change)
example <- mutate_at(att5, vars(-state), funs(as.numeric))
# Define vector containing numerical columns: cols
cols <- -1
# Use sapply to coerce cols to numeric
att5[, cols] <- sapply(att5[, cols], as.numeric)