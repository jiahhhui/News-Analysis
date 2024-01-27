# Script number: 3
# Script name: sentiment.R
# This script includes:
# sentiment analysis
# scatter chart: sentiment score vs top tfidf word

required_libraries <- c("tidytext", "sentimentr", "dplyr", "tm", "ggplot2", "stringr")

# Check if libraries are installed, and install if not
for (library_name in required_libraries) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
  }
}

library(tidytext)
library(sentimentr)
library(dplyr)
library(tm)
library(ggplot2)
library(stringr)

# step 1: subset & categorise
df <- read.csv("MN-DS-news-classification.csv")

df_politics_sent <- subset(df, category_level_1 %in% c("politics"))

df_politics_sent <- df_politics_sent %>%
  group_by(source) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  filter(row_number() <= 5) %>%
  inner_join(df_politics_sent, by = "source")


# step 2: preprocess text
clean_text_tm <- function(corpus) {
  
  corpus <- Corpus(VectorSource(corpus)) # convert to corpus
  corpus <- tm_map(corpus, tolower)  # convert to lowercase
  corpus <- tm_map(corpus, removeNumbers) # remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stopwords
  corpus <- tm_map(corpus, removeWords, c("mr", "ms", "said", "say")) # remove custom stopwords
  corpus <- tm_map(corpus, removePunctuation) # remove punctuation
  corpus <- tm_map(corpus, stripWhitespace) # remove whitespace
  corpus <- tm_map(corpus, stemDocument) #stemming
  corpus <- sapply(corpus, as.character) # convert back to a character vector
  
  return(corpus)
}

df_politics_sent <- df_politics_sent %>%
  mutate(cleaned_content = sapply(content, clean_text_tm))

# step 3: tfidf
tidy_text <- df_politics_sent %>% 
  unnest_tokens(word, cleaned_content) %>% 
  count(source, word, sort = TRUE)

total_words <- tidy_text %>% 
  group_by(source) %>% 
  summarize(total = sum(n))

tidy_text <- left_join(tidy_text,total_words) # to get words & count

tidy_text_tf_idf <- tidy_text %>%
  bind_tf_idf(word, source, n) %>% 
  arrange(desc(tf_idf))

result_tfidf <- tidy_text_tf_idf %>%
  group_by(source) %>%
  slice(which.max(tf_idf))

# step 4: sentiment

# finding the index of which the top tf-idf word appeared in the main df
# condition: matching exact word & matching source
indices_list <- lapply(1:nrow(result_tfidf), function(i) {
  pattern <- paste0("\\b", result_tfidf$word[i], "\\b")
  indices <- which(str_detect(df_politics_sent$cleaned_content, pattern) &
                     df_politics_sent$source == result_tfidf$source[i])
})
# flatten the list
indices_list <- data.frame(index = unlist(indices_list))

# create a new dataframe with the source, cleaned_content, 
# top_word, tf-idf score 
df_sentiment <- df_politics_sent[indices_list$index, ]
df_sentiment <- df_sentiment[, c("source", "content", "cleaned_content")]
df_sentiment <- merge(df_sentiment, result_tfidf, by = "source", all.x = TRUE)

sentiment_scores <- data.frame(sentiment_by(df_sentiment$cleaned_content))
df_sentiment$avg_sentiment <- sentiment_scores$ave_sentiment

# step 5: plot

# scatter plot
ggplot(df_sentiment, aes(x = word, y = avg_sentiment, colour = source)) +
  geom_point(size=2.5) +
  geom_text(aes(label = round(avg_sentiment, 2)),  vjust = -0.9, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 5, b=5), face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10, l=5), face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.margin = margin(0, 15, 0, 10)) +
  scale_y_continuous(
    breaks = seq(-0.5, 4.5, by = 0.5),
    limits = c(-0.5, 4.5)) + 
  labs(title = "Sentiment Score of Articles", 
       subtitle = "Of Top TF-IDF Words",
       caption ="Multilabeled News Dataset",
       color = "News Sources",
       x = "Word", y = "Sentiment Score")

# save pic, uncomment when necessary 
# ggsave('Plots/sentiment_1.png', width = 7, height = 6)
