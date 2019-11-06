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







