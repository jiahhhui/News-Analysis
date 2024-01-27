# Script number: 2
# Script name: tfidf.R
# This script includes:
# tf-idf analysis
# bar chart &
# packed circles

# List of required libraries
required_libraries <- c("dplyr", "tm", "tidytext", "forcats", "ggplot2", "packcircles", "viridis")

# Check if libraries are installed, and install if not
for (library_name in required_libraries) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
  }
}

library(dplyr)
library(tm)
library(tidytext)
library(forcats)
library(ggplot2)
library(packcircles) # for circles generating
library(viridis) # for colour

# step 1: subset & categorise
df <- read.csv("MN-DS-news-classification.csv")

df_politics_tfidf <- subset(df, category_level_1 %in% c("politics"))

df_politics_tfidf <- df_politics_tfidf %>%
  group_by(source) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  filter(row_number() <= 5) %>%
  inner_join(df_politics_tfidf, by = "source")


# step 2: preprocess text
clean_text_tm <- function(corpus) {
  
  corpus <- Corpus(VectorSource(corpus))
  
  # convert to lowercase
  corpus <- tm_map(corpus, tolower)
  
  # remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  
  # remove stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, c("mr", "ms"))
  
  # remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  
  # remove whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  #stemming
  corpus <- tm_map(corpus, stemDocument)
  
  # convert back to a character vector
  corpus <- sapply(corpus, as.character)
  
  return(corpus)
}

df_politics_tfidf <- df_politics_tfidf %>%
  mutate(cleaned_content = sapply(content, clean_text_tm))

tidy_text <- df_politics_tfidf %>% 
  unnest_tokens(word, cleaned_content) %>% 
  count(source, word, sort = TRUE)

total_words <- tidy_text %>% 
  group_by(source) %>% 
  summarize(total = sum(n))

tidy_text <- left_join(tidy_text,total_words)

tidy_text_tf_idf <- tidy_text %>%
  bind_tf_idf(word, source, n) %>% 
  arrange(desc(tf_idf))

# plot 1: bar chart
tidy_text_tf_idf %>%
  group_by(source) %>%
  slice_max(tf_idf, n = 3) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = source)) +
  geom_col(position = position_dodge(width = 1.7)) +
  facet_wrap(~source, ncol = 3, scales = "free") +
  theme(legend.position = "none",
        plot.caption = element_text((hjust = 0.5), margin = margin(b=10)),
        plot.subtitle = element_text(size = 15),
        axis.text.x = element_text(hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 20, face = "bold"),
        strip.text = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10), size=18, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size=18, face = "bold"),
        panel.spacing = unit(2, "lines")) +
  labs(title = "Highest TF-IDF Scored Words",
    subtitle = "Comparison on a Single News Source",
    x = "tf-idf Scores", 
    y = "Word",
    caption ="Multilabeled News Dataset")
ggsave('Plots/tf-idf_1.png', width = 15, height = 7)

# plot 2: circles
# taking top 3 highest tf-idf scored words from each news source
top3_tfidf <- tidy_text_tf_idf %>%
  group_by(source) %>%
  slice_max(tf_idf, n = 3)

# Generate a new datafame with selected cols
data <- data.frame() 
data <- top3_tfidf %>% select(word, tf_idf, source)
colnames(data)[colnames(data) == "tf_idf"] <- "value"

# Generate the layout for circles
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# custom df 
custom_df <- data.frame(id = dat.gg$id)

custom_df <- custom_df %>%
  mutate(source = case_when(
    id %in% 1:4 ~ "21stcenturywire",
    id %in% 5:7 ~ "aljazeera",
    id %in% 8:10 ~ "bbc",
    id %in% 11:13 ~ "birminghammail",
    id %in% 14:17 ~ "sputnik",
    TRUE ~ "unknown"))

dat.gg$source <- custom_df$source

rows_per_category <- 51
cat_df <- data.frame(
  Category = rep(data$word, each = rows_per_category))
dat.gg$group <- cat_df$Category


# plot
ggplot() +
  geom_polygon(data =dat.gg, aes(x, y, group = id, fill = source)) +
  theme_void() +
  geom_text(data=data, aes(x=x, y=y, label = gsub("Group_", "", word)), size=5)+ 
  theme(plot.margin=unit(c(0,1,0,1),"cm"),
        plot.caption = element_text(hjust = 0.5),
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 18),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16)) + 
  coord_equal() +
  labs(
    title ="Highest TF-IDF Scored Words",
    subtitle = "Comparison across News Sources",
    caption ="Multilabeled News Dataset",
    fill = "News sources")
#save pic, uncomment when necessary 
#ggsave('Plots/tf-idf_2.png', width = 10, height = 10)
            