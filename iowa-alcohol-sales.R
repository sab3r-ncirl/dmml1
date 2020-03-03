getwd()
setwd("C:/Users/Soham More/Documents/Datasets/v1_Data_Processing/Iowa_Alcohol_Sales/")


alcohol_df <- read.csv("Iowa_Liquor_Sales_Final.csv", header = TRUE)

library(dplyr)

alcohol_df_2015_2016 <- alcohol_df %>%
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2016-12-31"))

tail(alcohol_df)

class(alcohol_df$Date)

alcohol_df$Date <- as.character(alcohol_df$Date)

sapply(alcohol_df, function(x) sum(is.nan(x)))

install.packages("anytime")
anytime::anydate(head(alcohol_df$Date))

alcohol_df$Date <- anytime::anydate(alcohol_df$Date)


write.csv(alcohol_df_2015_2016, "Iowa_Liquor_Sales_Final_2015_2016.csv")

sapply(alcohol_df_2015_2016, function(x) sum(x==''))

alcohol_df_2015_2016 <- read.csv("Iowa_Liquor_Sales_Final_2015_2016.csv", header = TRUE)

str(alcohol_df_2015_2016)
alcohol_df_2015_2016 <- as.character(alc)

head(alcohol_df_2015_2016)

alcohol_df_2015_2016$Date <- as.character(alcohol_df_2015_2016$Date)
alcohol_df_2015_2016$Invoice.Item.Number <- as.character(alcohol_df_2015_2016$Invoice.Item.Number)

alcohol_df_2015_2016$Category.Name <- as.character(alcohol_df_2015_2016$Category.Name)

sum(is.na(alcohol_df_2015_2016$Category))
unique(alcohol_df_2015_2016$Category.Name)
categoryNameFrequency <- table(alcohol_df_2015_2016$Category.Name)
categoryNameFrequency
sort(categoryNameFrequency)
