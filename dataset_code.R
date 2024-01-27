# Script number: 1
# Script name: dataset_code.R
# This script includes:
# - For descriptive analysis of dataset
# - Inspection and subset dataset
# - Bar chart for categories
# - Histogram for word count

# List of required libraries
required_libraries <- c("dplyr","ggplot2")

# Check if libraries are installed, and install if not
for (library_name in required_libraries) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
  }
}

library(dplyr)
library(ggplot2)

# assign variable to dataset
df <- read.csv("MN-DS-news-classification.csv")

# Identify unique entries in the 'Category' column
unique_entries <- unique(df$category_level_2)

# print the result
print(unique_entries)

# sort based on 
count_per_category <- sort(table(df$category_level_2))

# print the result
print(count_per_category)

# Create a bar chart using ggplot2
ggplot(df, aes(x = category_level_1)) +
  geom_bar(fill="#2c2c54")+
  labs(title = "Categories under Category Level 1", 
       x = "Category Level 1", 
       y = "Frequency") +
theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
      axis.text.y = element_text(size=13),
      plot.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(t = 10), size=18),
      axis.title.y = element_text(margin = margin(r = 10), size=18),
      plot.margin = margin(20, 70, 20, 105, unit = "pt"))
ggsave('Plots/catlvl1.png')


# subset politics category
df_politics_tfidf <- subset(df, category_level_1 %in% c("politics"))

# inspection for unique entries
num_unique_entries <- df_politics_tfidf %>% 
  summarize(num_unique_entries = n_distinct(source))
print(num_unique_entries$num_unique_entries)

unique_entries <- unique(df_politics_tfidf$source)
print(unique_entries)

# inspection for number of articles per category
count_per_category <- sort(table(df_politics_tfidf$source))
print(count_per_category)

# word count
df_politics_tfidf <- df_politics_tfidf %>%
  group_by(source) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  filter(row_number() <= 5) %>%
  inner_join(df_politics_tfidf, by = "source")

# function to count words 
count_words <- function(text) {
  words <- unlist(strsplit(as.character(text), "\\s+"))
  return(length(words))
}
# calling function
df_politics_tfidf$word_count <- apply(df_politics_tfidf, 1, function(row) count_words(row["content"]))

# plot histogram for word count
custom_labels <- c("0", "20", "40", "60", "80")
ggplot(df_politics_tfidf, aes(x = word_count)) +
  geom_histogram(data=df_politics_tfidf, aes(y = after_stat(density)),colour ="#f5e3e0", fill="#2c2c54") +
  geom_line(stat = "density", color="#a40e4c") +
  scale_y_continuous(breaks = c(0, 0.00025, 0.00050, 0.00075, 0.00100), labels = custom_labels) +
  labs(title = "Histogram of Word Count", 
       x = "Word Count", 
       y = "Frequency") +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 10), size=18),
        axis.title.y = element_text(margin = margin(r = 10), size=18))
#save pic, uncomment when necessary 
#ggsave('Plots/histogram.png')

