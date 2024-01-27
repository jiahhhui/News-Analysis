# Script number: 4
# Script name: spacyr.R
# This script includes:
# NER vs Sentiment: sentiment score with noun associations in title 
# 3 bar charts - positive, neutral, negative

# Install and load the spacyr package
if (!requireNamespace("spacyr", quietly = TRUE)) {
  install.packages("spacyr")}
library(spacyr)

# Download and load the spaCy model
if (!requireNamespace("en_core_web_sm", quietly = TRUE)) {
  spacy_install("en_core_web_sm")}

if (!requireNamespace("en_core_web_sm", quietly = TRUE)) {
  spacy_initialize(model = "en_core_web_sm")}

# step 1: subset data
# assign variable to dataset
df <- read.csv("MN-DS-news-classification.csv")
# subset
df_politics_spacyr <- subset(df, category_level_1 %in% c("politics"))
df_politics_spacyr <- select(df_politics_spacyr, source, title, content)

df_politics_spacyr <- df_politics_spacyr %>%
  group_by(source) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  filter(row_number() <= 5) %>%
  inner_join(df_politics_spacyr, by = "source")

# step 2: Perform NER 
text <- spacy_parse(df_politics_spacyr$title)
text <- subset(text, pos %in% c("PROPN"))
#get rid of punctuation detected
text <- text[-393,]
text <- text[-1108,]

text_df <- text %>%
  group_by(doc_id) %>%
  summarize(token_list = list(token)) 

text_df <- text_df %>%
  mutate(index = as.numeric(gsub("text", "", doc_id))) %>%
  arrange(index)

# step 3: Perform Sentiment Analysis
# preprocess text
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

df_politics_spacyr <- df_politics_spacyr %>%
  mutate(cleaned_content = sapply(content, clean_text_tm))

# compute sentiment score
sentiment_scores <- data.frame(sentiment_by(df_politics_spacyr$cleaned_content))

# find top 3 documents with the highest positive sentiment score
pos_sent <- sentiment_scores %>% arrange(desc(ave_sentiment)) %>% head(4)
# find top 3 documents with the highest negative sentiment score
neg_sent <- sentiment_scores %>% arrange(ave_sentiment) %>% head(3)
# find top 3 documents with the most neutral sentiment score
neu_sent <- sentiment_scores %>% arrange(abs(ave_sentiment)) %>% head(3)

# extract relevant columns
pos_extract <- df_politics_spacyr[pos_sent$element_id, ]
pos_sent <- cbind(pos_sent, pos_extract)
# check if there were nouns detected in these text
appearances <- pos_sent$element_id %in% text_df$index 
appearances
# result for positive: FALSE  TRUE  TRUE  
# delete first row
pos_sent <- pos_sent[-1,]
# combine to get list of nouns for respective articles
pos_sent <- merge(pos_sent, text_df, by.x = "element_id", by.y = "index")

#repeat for neg 
neg_extract <- df_politics_spacyr[neg_sent$element_id, ]
neg_sent <- cbind(neg_sent, neg_extract)
appearances <- neg_sent$element_id %in% text_df$index 
appearances
# result for positive: TRUE  TRUE  TRUE  
neg_sent <- merge(neg_sent, text_df, by.x = "element_id", by.y = "index")

#repeat for neg & neutral
neu_extract <- df_politics_spacyr[neu_sent$element_id, ]
neu_sent <- cbind(neu_sent, neu_extract)
appearances <- neu_sent$element_id %in% text_df$index 
appearances
# result for positive: TRUE  TRUE  TRUE  
neu_sent <- merge(neu_sent, text_df, by.x = "element_id", by.y = "index")

# combining 3 dataframes into 1 for plotting
pos_sent$sentiment_type <- "positive"
neg_sent$sentiment_type <- "negative"
neu_sent$sentiment_type <- "neutral"

sentiment_spacyr <- rbind(pos_sent, neg_sent, neu_sent)
sentiment_spacyr$token_list <- sapply(sentiment_spacyr$token_list, 
                                      function(words) paste(words, collapse = ", "))
# source rank
sentiment_spacyr$source_no <- match(sentiment_spacyr$source, unique(sentiment_spacyr$source))
sentiment_spacyr <- sentiment_spacyr[order(sentiment_spacyr$ave_sentiment), ]

# step 4: 
# plot
library(ggplot2)
desired_order <- c("positive","neutral","negative")
ggplot(sentiment_spacyr, 
       aes(x = ave_sentiment, fct_reorder(token_list, ave_sentiment),
           fill=factor(sentiment_type, levels = desired_order))) +
  geom_bar(stat = "identity") +
  theme_light() +
  geom_text(aes(label = round(ave_sentiment, 3)), size=3.5) +
  theme(strip.text.x.top = element_text(colour = "black", face = "bold", size = 13),
        plot.title = element_text(face = "bold", size = 16),
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 11),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(face = "bold", size = 13),
        axis.text.y = element_text(size = 11),) +
  facet_wrap(~ sentiment_type, scales = "free_y", ncol = 1, as.table = FALSE) +
  labs(title = "Named Entity Recognition (NER) vs Sentiment Scores", 
       x = "Sentiment Score", 
       y = "Named Entity in Article Titles",
       fill = "Sentiment Type",
       caption ="Multilabeled News Dataset") 

# save pic, uncomment when necessary 
# ggsave('Plots/ner_1.png', width = 11, height = 5)
