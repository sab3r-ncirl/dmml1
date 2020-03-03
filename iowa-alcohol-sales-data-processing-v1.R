getwd()
setwd("C:/Users/Soham More/Documents/Github/dmml1/v1_Data_Processing/Iowa_Alcohol_Sales/")


alcohol_df <- read.csv("Iowa_Liquor_Sales_Final.csv", header = TRUE)

library(dplyr)


# Change Date from factor to character so that data can be filtered for specific years.
class(alcohol_df$Date)
alcohol_df$Date <- as.character(alcohol_df$Date)

# Install 'anytime' package to format the Date variables.
install.packages("anytime")
# Check is the date is formatted correctly before applying it to the dataframe.
anytime::anydate(head(alcohol_df$Date)) 

# Transform the Date values so that we can perform operations on it.
alcohol_df$Date <- anytime::anydate(alcohol_df$Date)


# Filter data for years 2015 and 2016.
alcohol_df_2015_2016 <- alcohol_df %>%
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2016-12-31"))

names(alcohol_df_2015_2016)
# Order the data using Date and Store Name
new_alcohol_df <- alcohol_df_2015_2016[order(as.Date(alcohol_df_2015_2016$Date), alcohol_df_2015_2016$Store.Name),]

# Check head and tail if the data is correct.
head(new_alcohol_df)
tail(new_alcohol_df)

# Check for NA values
sapply(new_alcohol_df, function(x) sum(is.na(x))) 
# No NA values

# Check for NaN values
sapply(new_alcohol_df, function(x) sum(is.nan(x)))
# No NaN values

# Check for Blank values
sapply(new_alcohol_df, function(x) sum(grepl("^\\s*$", x)))
# Invoice.Item.Number - 0
# Date - 0
# Store.Number - 0
# Store.Name - 0
# Address - 965
# City - 965
# Zip.Code - 1010
# Store.Location - 965
# County.Number - 70455
# County - 70455
# Category - 1123
# Category.Name - 7562
# Vendor.Number - 2
# Vendor.Name - 0
# Item.Number - 0
# Item.Description - 0
# Pack - 0
# Bottle.Volume..ml.  - 0
# State.Bottle.Cost - 0
# State.Bottle.Retail - 0
# Bottles.Sold - 0
# Sale..Dollars. - 0
# Volume.Sold..Liters. - 0
# Volume.Sold..Gallons. - 0

# Check unique values
sapply(new_alcohol_df, function(x) length(unique(x)))
# Unique Values:
# Invoice.Item.Number - 4464362
# Date - 473
# Store.Number - 1540
# Store.Name - 1645
# Address - 2844
# City - 766 
# Zip.Code - 426
# Store.Location - 3798
# County.Number - 109
# County - 201
# Category - 104
# Category.Name - 129
# Vendor.Number - 193
# Vendor.Name - 288
# Item.Number - 4614
# Item.Description - 3682
# Pack - 22
# Bottle.Volume..ml. - 39
# State.Bottle.Cost - 1742
# State.Bottle.Retail - 1809
# Bottles.Sold - 322
# Sale..Dollars. - 12749
# Volume.Sold..Liters. - 806
# Volume.Sold..Gallons. - 900


# Remove the '$' symbol from the columns 'Sale..Dollars.' 'State.Bottle.Retail' and 'State.Bottle.Cost'
new_alcohol_df$Sale..Dollars. <- as.numeric(gsub("\\$", "", new_alcohol_df$Sale..Dollars.))
new_alcohol_df$State.Bottle.Cost <- as.numeric(gsub("\\$", "", new_alcohol_df$State.Bottle.Cost))
new_alcohol_df$State.Bottle.Retail <- as.numeric(gsub("\\$", "", new_alcohol_df$State.Bottle.Retail))

# Trim White Spaces from all the columns
sapply(new_alcohol_df, function(x) x <- trimws(x))

# Write all the data to a new file.
write.csv(new_alcohol_df, "Iowa_Liquor_Sales_Final_2015_2016.csv")
