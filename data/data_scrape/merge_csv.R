
# Load library
library(data.table)

# Get a List of all files in directory named with a key word, say all `.csv` files
filenames <- list.files("data_scrape/", pattern="*.csv", full.names=TRUE)
filenames

# read and row bind all data sets
data <- rbindlist(lapply(filenames,fread))

write.csv(data,
          "reddit_df.csv",
          row.names = TRUE)

# sentiment for merged file
source('functions/sentiment_functions.R') 

compute_sentiment(df = data)
# saved in df_with_sentiment_timestamp.csv and daily_sentiment.csv
