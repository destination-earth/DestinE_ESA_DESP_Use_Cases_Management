library(dplyr)
library(readxl)
library(tidyr)
library(janitor)
library(ggplot2)
library(lubridate)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)

# Set the working directory to the raw_data folder
setwd("/cloud/project/data/raw_data")

# Read data from the 1st DestinE User eXchange registration form
db01 <- readxl::read_excel("20230215_User_eXchange_01.xlsx") %>%
  select(`Date Registered`, `Primary Address - Country`, # mandatory fields
         `First Name`, `Last Name`, `Organization`, `Primary Email`) %>% # temp fields (to remove duplicates)
  rename(Date = `Date Registered`, Country = `Primary Address - Country`,
         First = `First Name`, Last = `Last Name`,
         Org = `Organization`, Email = `Primary Email`) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(Date = as.Date(ymd_hms(Date))) 

# Read data from the Roadshow webinar #1
db02 <- read.csv("20230719_Roadshow_Webinar_01.csv",
                 skip = 6, header = FALSE) %>%
  select(`V6`, `V12`,
         `V1`, `V2`, `V5`, `V3`) %>%
  rename(Date = `V6`, Country = `V12`,
         First = `V1`, Last = `V2`,
         Org = `V5`, Email = `V3`) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(Date = as.Date(parse_date_time(Date, orders = "b d Y H M S")))

# Read data from the Mailjet subscriber database
db03 <- read.csv("20230802_Mailjet_newsletter_subscribers_database.csv") %>%
  select(`subscribedat`, `country`,
         `firstname`, `lastname`, `organisation`, `email`) %>%
  rename(Date = `subscribedat`, Country = `country`,
         First = `firstname`, Last = `lastname`,
         Org = `organisation`, Email = `email`) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(Date = as.Date(ymd_hms(Date)))

# Read data from the 2nd DestinE User eXchange registration list (NO COUNTRY!)
db04 <- readxl::read_excel("20231113-14_User_eXchange_02.xlsx") %>%
  select(`Time & Date of Registration`, `Comment`,
         `Name`, `Surname`, `Organisation`, `Email`) %>%
  mutate(Comment = NA) %>%
  rename(Date = `Time & Date of Registration`, Country = `Comment`,
         First = `Name`, Last = `Surname`,
         Org = `Organisation`) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(Date = as.Date(ymd_hms(Date)))

# Read data from the Roadshow Webinar #2
db05 <- read.csv("20231213_Roadshow_Webinar_02.csv") %>%
  select(`Date`, `Country`,
         `your.name`, `your.surname`, `Organisation`, `your.email`) %>%
  rename(First = `your.name`, Last = `your.surname`,
         Org = `Organisation`, Email = `your.email`) %>%
  mutate(across(everything(), tolower)) %>%
  mutate(Date = as.Date(ymd_hms(Date)))

# Read data from the Community registration form - first version without country
db06 <- read.csv("20240107_Community_Form_no_country.csv") %>%
 select(`Date`, `your.country`,
        `first.name`, `Lastname`, `Organisation`, `your.email`) %>%
 rename(Country = `your.country`,
        First = `first.name`, Last = `Lastname`,
        Org = `Organisation`, Email = `your.email`) %>%
  mutate(across(everything(), tolower)) %>% 
 mutate(Date = as.Date(ymd_hms(Date)))

# Read data from the Community registration form - updated on 20240206
db07 <- read.csv("20240206_Community_Form.csv") %>%
  select(`Date`, `your.country`,
         `first.name`, `Lastname`, `Organisation`, `your.email`) %>%
  rename(Country = `your.country`,
         First = `first.name`, Last = `Lastname`,
         Org = `Organisation`, Email = `your.email`) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(Date = as.Date(ymd_hms(Date)))

# Merge databases
df <- rbind(db01, db02, db03, db04, db05, db06, db07) %>%
  filter(!(is.na(First) | First == "") & !(is.na(Last) | Last == "")) %>% #remove test rows (emptyname/surname)
  distinct(Email, .keep_all = TRUE) %>% # remove duplicate email
  distinct(First, Last, .keep_all = TRUE) # remove duplicate first/last names

# Summarize registrations by date and calculate cumulative registrations
df %>%
  select(Date) %>%
  mutate(Registrations = 1) %>%
  group_by(Date) %>%
  summarise(Registrations = sum(Registrations)) %>%
  arrange(Date) %>%
  mutate(cumulative_registrations = cumsum(Registrations)) %>%
  ggplot(aes(Date, cumulative_registrations)) +
  geom_line() + scale_color_discrete(name = "") + ylab("Registered community members")

# Fix manually missing values ##################################################
# Homogenize missing values
rows_with_missing_values <- with(df, is.na(Country) | Country == "" | is.null(Country))
print(df$Country[rows_with_missing_values])
df$Country[rows_with_missing_values] <- NA
print(df[rows_with_missing_values, ])

# Count characters in email extensions
df$Characters <- nchar(sub(".*\\.", "", df$Email))
# Replace ISO codes - 2 characters
rows_with_iso2c <- with(df, is.na(Country) & Characters == 2)
print(df$Country[rows_with_iso2c])
df$Country[rows_with_iso2c] <- tolower(countrycode(sourcevar = sub(".*\\.", "", 
                                                  df$Email[rows_with_iso2c]),
                                                  origin = "iso2c",
                                                  destination = "country.name"))
print(df$Country[rows_with_iso2c])

# Replace UK with united kingdom
rows_with_uk <- with(df, is.na(Country) & sub(".*\\.", "", Email) == "uk" & Characters == 2)
print(df$Country[rows_with_uk])
df$Country[rows_with_uk] <- "united kingdom"
print(df$Country[rows_with_uk])

# Find rows where 'Email' or 'Org' contains 'nasa' (case-insensitive)
rows_with_us <- with(df, Country == "united states")
df$Country[rows_with_us] <- "united states of america"
rows_with_nasa <- with(df, is.na(Country) & 
                         (grepl("nasa", Email, ignore.case = TRUE) | grepl("nasa", Org, ignore.case = TRUE)))
# Replace missing value
df$Country[rows_with_nasa] <- "united states of america"
# Print to check the result
print(df[rows_with_nasa, ])

# Still missing
rows_with_missing_values <- with(df, is.na(Country))
print(df[rows_with_missing_values, ])
################################################################################

df_country <- df[!(is.na(df$Country) | df$Country==""), "Country"] %>%
  count(Country, sort = TRUE) %>%
  mutate(n = as.numeric(n))

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Make sure the country names match the names in your data frame 'df'
world$admin <- tolower(world$admin)

# Merge your data with the world map
merged_data <- merge(world, df_country, by.x = "admin", by.y = "Country", all.x = TRUE)

ggplot() +
  # Plot filled countries with data
  geom_sf(data = merged_data, aes(fill = n), color = "white", size = 0.1) +
  # Plot outlines for all countries
  geom_sf(data = world, fill = NA, color = "black", size = 0.1) +
  scale_fill_continuous(name = "Number of Entries", na.value = "grey90", low = "blue", high = "red") +
  labs(title = "DestinE community registrations",
       subtitle = "Number of Entries per Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

################################################################################
# WORD CLOUD
install.packages("wordcloud")
install.packages("tm")  # For text mining
library(wordcloud)
library(tm)

# Example vector of text
texts <- c(read.csv("20240206_Community_Form.csv")$Whichtopicsareyouinterestedin,
           read.csv("20240107_Community_Form_no_country.csv")$Whichtopicsareyouinterestedin)

# Create a corpus from the vector
corpus <- Corpus(VectorSource(texts))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Convert the corpus to a plain text document
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)

# Generate the word cloud
wordcloud(words = df_word_freqs$word, freq = df_word_freqs$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
