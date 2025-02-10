#PSYC 259 Homework 3 - Data Structure
#For full credit, provide answers for at least 8/11 questions

#List names of students collaborating with: NA

### SETUP: RUN THIS BEFORE STARTING ----------

install.packages("rvest")

#Load packages
library(tidyverse)
library(lubridate)
library(rvest)

# Scrape the data for the new rolling stone top 500 list
url <- "https://stuarte.co/2021/2021-full-list-rolling-stones-top-500-songs-of-all-time-updated/"
rs_new <- url %>% read_html() %>% html_nodes(xpath='//*[@id="post-14376"]/div[2]/div[2]/table') %>% html_table() %>% pluck(1)

# Scrape the data for the old rolling stone top 500 list
url_old <- "https://www.cs.ubc.ca/~davet/music/list/Best9.html"
rs_old <- url_old %>% read_html() %>% html_nodes(xpath='/html/body/table[2]') %>% html_table() %>% pluck(1) %>% 
  select(1, 4, 3, 7) %>% rename(Rank = X1, Artist = X3, Song = X4, Year = X7) %>% filter(Year != "YEAR") 

# If there's a security error, add:
#url %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% read_html()

#OR
load("rs_data.RData")

### Question 1 ---------- 

# Use "full_join" to merge the old and new datasets, rs_new and rs_old,
# by Artist AND Song. Save the results to a dataset called rs_joined_orig
# If the merged worked, each song-artist combination that appears in both
# datasets should now be in a single row with the old/new ranks
# Use the nrow() function to see how many rows of data there are
# In the viewer, take a look at the merge...what kinds of problems are there? Some artists/songs have extra punctuation/spaces, resulting in duplicates 
# Why did some of the artist-song fail to match up? Some artists/songs have extra punctuation/spaces 

#ANSWER
# Merging the datasets using full_join by Artist and Song
rs_joined_orig <- full_join(rs_new, rs_old, by = c("Artist", "Song"))

# View the first few rows of the merged dataset
head(rs_joined_orig)

# Count the number of rows in the merged dataset
nrow(rs_joined_orig)

### Question 2 ---------- 

# To clean up the datasets, it would be more efficient to put them into a single data set
# Add a new variable to each dataset called "Source" with value "New" for rs_new and
# "Old" for rs_old. Then use bind_rows to join the two datasets into a single one called rs_all
# You will run into a problem because the old dataset has rank/year as characters instead of integers
# Make Rank and Year into integer variables for rs_old before binding them into rs_all

#ANSWER
# Convert Rank and Year to integers in rs_old
rs_old <- rs_old %>%
  mutate(Rank = as.integer(Rank),
         Year = as.integer(Year))

# Add a "Source" column to each dataset
rs_new <- rs_new %>%
  mutate(Source = "New")

rs_old <- rs_old %>%
  mutate(Source = "Old")

# Combine both datasets using bind_rows()
rs_all <- bind_rows(rs_new, rs_old)

# View the structure of the combined dataset
str(rs_all)

### Question 3 ----------

# The join in Q1 resulted in duplicates because of differences in how the songs and artists names were written
# Use string_remove_all to remove the word "The" from every artist/song (e.g., Beach Boys should match The Beach Boys)
# Use string_replace_all to replace the "&" with the full word "and" from every artist/song
# Then use string_remove_all to remove all punctuation from artists/songs
# Finally, read the documentation for the functions str_to_lower and str_trim
# Use both functions to make all artists/song lowercase and remove any extra spaces

#ANSWER
# Clean and standardize Artist and Song names
rs_new <- rs_new %>%
  mutate(
    Artist = str_remove_all(Artist, "\\bThe\\b"),  # Remove "The"
    Song = str_remove_all(Song, "\\bThe\\b"),
    Artist = str_replace_all(Artist, "&", "and"),  # Replace "&" with "and"
    Song = str_replace_all(Song, "&", "and"),
    Artist = str_remove_all(Artist, "[[:punct:]]"),  # Remove all punctuation
    Song = str_remove_all(Song, "[[:punct:]]"),
    Artist = str_to_lower(str_trim(Artist)),  # Convert to lowercase and trim spaces
    Song = str_to_lower(str_trim(Song))
  )

rs_old <- rs_old %>%
  mutate(
    Artist = str_remove_all(Artist, "\\bThe\\b"),
    Song = str_remove_all(Song, "\\bThe\\b"),
    Artist = str_replace_all(Artist, "&", "and"),
    Song = str_replace_all(Song, "&", "and"),
    Artist = str_remove_all(Artist, "[[:punct:]]"),
    Song = str_remove_all(Song, "[[:punct:]]"),
    Artist = str_to_lower(str_trim(Artist)),
    Song = str_to_lower(str_trim(Song))
  )

# Now bind the cleaned datasets
rs_all <- bind_rows(rs_new, rs_old)

# View a sample of cleaned data
head(rs_all)

### Question 4 ----------

# Now that the data have been cleaned, split rs_all into two datasets, one for old and one for new
# Each dataset should have 500 observations and 5 variables
# Use full_join again to merge the old and new datasets by artist and song, and save it to rs_joined
# Read about the "suffix" argument in full_join, and use it to append _Old and _New to year and rank
# rather than the default (x and y)
# Did the string cleaning improve matches? If so, there should be fewer rows of data (fewer duplicates)
# in the new rs_joined compared to the original. Use nrow to check (there should be 799 rows)

#ANSWER
# Ensure rs_all has exactly 1000 observations before splitting
rs_all <- rs_all %>% slice(1:1000)  # In case rs_all has extra rows

# Split into rs_old and rs_new (each with 500 rows)
rs_old <- rs_all %>% filter(Source == "Old") %>% select(Artist, Song, Year, Rank, Source) %>% slice(1:500)
rs_new <- rs_all %>% filter(Source == "New") %>% select(Artist, Song, Year, Rank, Source) %>% slice(1:500)

# Perform a full join with custom suffixes
rs_joined <- full_join(rs_old, rs_new, by = c("Artist", "Song"), suffix = c("_Old", "_New"))

# Check the number of rows in rs_joined
nrow(rs_joined)  # Should return 799 if cleaning was successful

# View the first few rows
head(rs_joined)

### Question 5 ----------

# Let's clean up rs_joined with the following steps:
  # remove the variable "Source"
  # remove any rows where Rank_New or Rank_Old is NA (so we have only the songs that appeared in both lists)
  # calculate a new variable called "Rank_Change" that subtracts new rank from old rank
  # sort by rank change
# Save those changes to rs_joined
# You should now be able to see how each song moved up/down in rankings between the two lists

#ANSWER
# Ensure rs_joined contains the necessary columns
rs_joined <- rs_joined %>%
  filter(!is.na(Rank_Old) & !is.na(Rank_New)) %>%  # Keep only songs in both lists
  mutate(Rank_Change = Rank_Old - Rank_New) %>%  # Calculate rank change
  arrange(Rank_Change)  # Sort by rank change

# View cleaned dataset
head(rs_joined)

### Question 6 ----------

# Add a new variable to rs_joined that takes the year and turns it into a decade with "s" at the end
# The new variable should be a factor
# 1971 should be 1970s, 1985 should be 1980s, etc.
# Group by decade and summarize the mean rank_change for songs released in each decade (you don't have to save it)
# Which decade improved the most? The 1990s improved the most

#ANSWER
# Add a new 'Decade' variable
rs_joined <- rs_joined %>%
  mutate(Decade = factor(paste0(floor(Year_Old / 10) * 10, "s")))

# Summarize mean rank change by decade
decade_summary <- rs_joined %>%
  group_by(Decade) %>%
  summarize(Mean_Rank_Change = mean(Rank_Change, na.rm = TRUE)) %>%
  arrange(Mean_Rank_Change)  # Sorting to find the most improved decade

# View the summary
print(decade_summary)

### Question 7 ----------

# Use fct_count to see the number of songs within each decade
# Then use fct_lump to limit decade to 3 levels (plus other), and
# Do fct_count on the lumped factor with the prop argument to see the 
# proportion of songs in each of the top three decades (vs. all the rest)

#ANSWER
# Count the number of songs in each decade
decade_count <- fct_count(rs_joined$Decade)

# Lump the decades into the top 3 (plus 'Other')
rs_joined <- rs_joined %>%
  mutate(Decade_Lumped = fct_lump(Decade, n = 3))

# Count the proportion of songs in the top 3 decades (and 'Other')
decade_lumped_count <- fct_count(rs_joined$Decade_Lumped, prop = TRUE)

# View the proportion of songs in each of the top 3 decades (vs all the rest)
print(decade_lumped_count)

### Question 8 ---------- 

# Read the file "top_20.csv" into a tibble called top20
# Release_Date isn't read in correctly as a date
# Use parse_date_time to fix it

#ANSWER
# Read the CSV file into a tibble
top20 <- read_csv("top_20.csv")

# View the column names to check if 'Release_Date' is correct
colnames(top20)

# If the column name is correct, parse the Release_Date
top20 <- top20 %>%
  mutate(Release = parse_date_time(Release, orders = "mdy"))

# View the top20 tibble to ensure Release_Date is now correctly parsed
head(top20)

### Question 9 --------

# top20's Style and Value are mixing two different variables into one column
# use pivot_wider to fix the issue so that bpm and key are columns
# overwrite top20 with the pivoted data (there should now be 20 rows!)

#ANSWER
# Pivot the 'Style' and 'Value' columns into 'bpm' and 'key'
top20 <- top20 %>%
  pivot_wider(names_from = Style, values_from = Value)

# View the result to ensure the pivot was successful
head(top20)

### Question 10 ---------

# Merge in the data from rs_joined to top20 using left_join by artist and song
# The results should be top20 (20 rows of data) with columns added from rs_joined
# Use the "month" function from lubridate to get the release month from the release date
# and add that as a new variable to top 20. 
# It should be a factor - if you get a number back read the help for ?month to see how to get a factor
# Create a new factor called "season" that collapses each set of 3 months into a season "Winter", "Spring", etc.
# Count the number of songs that were released in each season

#ANSWER
# Merge rs_joined with top20 by artist and song
top20 <- left_join(top20, rs_joined, by = c("Artist" = "Artist", "Song" = "Song"))

# Extract the release month and convert to factor
top20 <- top20 %>%
  mutate(Release_Month = factor(month(Release), levels = 1:12, labels = month.name))

# Create a season factor
top20 <- top20 %>%
  mutate(Season = case_when(
    month(Release) %in%  c(12, 1, 2) ~ "Winter",
    month(Release) %in%  c(3, 4, 5) ~ "Spring",
    month(Release) %in%  c(6, 7, 8) ~ "Summer",
    month(Release) %in%  c(9, 10, 11) ~ "Fall"
  ))

# Count the number of songs released in each season
season_count <- top20 %>%
  count(Season)

# View the count of songs per season
print(season_count)

### Question 11 ---------

# How many songs in the top 20 were major vs. minor? 
# Create a new factor called "Quality" that is either Major or Minor
# Minor keys contain the lowercase letter "m". If there's no "m", it's Major
# Figure out which is the top-ranked song (from Rank_New) that used a minor key

#ANSWER
# Create the 'Quality' factor based on the presence of 'm' in the key
top20 <- top20 %>%
  mutate(Quality = case_when(
    str_detect(Key, "m") ~ "Minor",  # If the key contains 'm', it's Minor
    TRUE ~ "Major"  # Otherwise, it's Major
  ))

# Count how many songs are Major vs. Minor
quality_count <- top20 %>%
  count(Quality)

# View the count of Major vs Minor songs
print(quality_count)

# Find the top-ranked song (lowest Rank_New) that used a Minor key
top_minor_song <- top20 %>%
  filter(Quality == "Minor") %>%
  arrange(Rank_New) %>%
  slice(1)  # Select the top-ranked song

# View the top-ranked minor key song
print(top_minor_song)


