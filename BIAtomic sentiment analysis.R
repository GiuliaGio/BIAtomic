# Install the necessary packages
install.packages(c("dplyr", "tidytext", "sentimentr", "ggplot2"))

# Load libraries
library(dplyr)
library(tidytext)
library(sentimentr)
library(ggplot2)
library(readxl)

####################### GENERAL SENTIMENT ANALYSIS  #########################

#1: Import the dataset
data <- read_excel("C:/Users/ritam/OneDrive/Documenti/Desktop/ExcelMarchi.xlsx")

#2: Select only the relevant column are needed
reviews <- data %>%select(Brand, Rating, Reviews, element_id)

#3: Divide the reviews in phrases
sentences <- get_sentences(reviews$Reviews)

#4: Calculate the sentiment for each review
sentiment_results <- sentiment(reviews$Reviews)

#5: Calculate the aggregated sentiment for each review
aggregated_sentiment <- sentiment_results %>%
  group_by(element_id) %>%    #important step
  summarise(sentiment = mean(sentiment, na.rm = TRUE))

#6: Add the results of step 6 to the reviews dataframe
reviewsSentiment <- left_join(reviews, aggregated_sentiment, by = "element_id")

#7: Add label for the sentiments
reviewsSentiment$sentiment_label <- ifelse(reviewsSentiment$sentiment > 0, "Positive",
                                  ifelse(reviewsSentiment$sentiment < 0, "Negative", "Neutral"))

#8: Create summary for the sentiments
sentiment_summary <- reviewsSentiment %>%
  group_by(sentiment_label) %>%
  summarise(count = n())

#9: Visualize results
ggplot(sentiment_summary, aes(x = sentiment_label, y = count, fill = sentiment_label)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Sentiment") +
  ylab("Number of Reviews") +
  ggtitle("Sentiment Summary of Product Reviews") +
  scale_fill_manual(
    values = c("Positive" = "chartreuse4", "Neutral" = "cornflowerblue", "Negative" = "darkred"))

################ PIE GRAPH ########################

# Calculate the percentage of sentiments
emotion_summary <- reviewsSentiment %>%
  group_by(sentiment_label) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(percent = n / sum(n))


# Pie graph
ggplot(sentiment_summary, aes(x = "", y = count, fill = sentiment_label)) +
  geom_col() +  # Usa geom_col per il grafico a torta
  coord_polar(theta = "y") +  # Trasformare in grafico a torta
  theme_minimal() +
  xlab("") +  # Rimuove l'etichetta dell'asse x
  ylab("Number of Reviews") +
  ggtitle("Sentiment Distribution on a pie graph") +
  scale_fill_manual(
    values = c("Positive" = "chartreuse4", "Neutral" = "cornflowerblue", "Negative" = "darkred")
  ) +
  geom_text(aes(label = paste0(round(count / sum(count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white")



####################### SENTIMENTS BY CATEGORIES #########################

library(tidyverse)

# 1 Calculate the aggregated sentiment for each category
aggregated_sentiment_category <- reviewsSentiment %>%
  group_by(Brand) %>%
  summarise(
    positive = sum(sentiment > 0, na.rm = TRUE),
    negative = sum(sentiment < 0, na.rm = TRUE),
    neutral = sum(sentiment == 0, na.rm = TRUE) )

# 2 Remain to the long format for the graph
sentiment_long <- aggregated_sentiment_category %>%
  pivot_longer(
    cols = c(positive, negative, neutral), 
    names_to = "sentiment_label", 
    values_to = "count")

# 3 Create the graph that visualize the sentiments by category
ggplot(sentiment_long, aes(x = Brand, y = count, fill = sentiment_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Product Category") +
  ylab("Number of Reviews") +
  ggtitle("Sentiment Distribution by Brand") +
  scale_fill_manual(values = c(positive = "chartreuse4", negative = "darkred", neutral = "cornflowerblue"))

# Calculate the aggregated sentiment for each category
aggregated_sentiment_category <- reviewsSentiment %>%
  group_by(Brand) %>%
  summarise(
    positive = sum(sentiment > 0, na.rm = TRUE),
    negative = sum(sentiment < 0, na.rm = TRUE),
    neutral = sum(sentiment == 0, na.rm = TRUE),
    total = n()
  )

# Convert counts to percentages
sentiment_percentage <- aggregated_sentiment_category %>%
  mutate(
    positive_pct = positive / total * 100,
    negative_pct = negative / total * 100,
    neutral_pct = neutral / total * 100
  ) %>%
  select(Brand, positive_pct, negative_pct, neutral_pct)


# Convert counts to percentages
sentiment_percentage <- aggregated_sentiment_category %>%
  mutate(
    positive_pct = positive / total * 100,
    negative_pct = negative / total * 100,
    neutral_pct = neutral / total * 100
  ) %>%
  select(Brand, positive_pct, negative_pct, neutral_pct)

# Stay in long format for the percentage graph
sentiment_percent_long <- sentiment_percentage %>%
  pivot_longer(
    cols = c(positive_pct, negative_pct, neutral_pct),
    names_to = "sentiment_label",
    values_to = "percentage"
  )

# Create the bar graph that visualizes the percentages by category
ggplot(sentiment_percent_long, aes(x = Brand, y = percentage, fill = sentiment_label)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "white", 
            size = 3.5) +  # Adjust size as necessary
  theme_minimal() +
  xlab("Brand") +
  ylab("Percentage of Reviews") +
  ggtitle("Percentage Sentiment Distribution by Brand") +
  scale_fill_manual(values = c(positive_pct = "chartreuse4", negative_pct = "darkred", neutral_pct = "cornflowerblue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####################### FREQUENCE OF WORDS USING STOP WORDS #########################

library(dplyr)
library(tidytext)
library(ggplot2)

# Load the predefined dataset of stop words
data("stop_words")  

# Prepare data for the words analysis excluding stop words
word_counts <- reviews %>%
  unnest_tokens(word, Reviews) %>%  # Tokenization by single words
  anti_join(stop_words) %>%          # Exclude words in stop words
  count(Brand, word, sort = TRUE)    # Count the words by brand

# Calculate sentiment for each word using the Bing lexicon
word_sentiment <- word_counts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%  # Join with Bing sentiment lexicon
  group_by(Brand, word) %>%
  summarise(
    n = sum(n),  # Total count of each word
    sentiment = first(sentiment),  # Get the sentiment (positive/negative)
    .groups = 'drop'  # Drop grouping structure for easier handling
  )

# Get the top 10 words per brand
top_words <- word_sentiment %>%
  group_by(Brand) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%   # Extract the top words
  ungroup() %>%
  arrange(Brand, desc(n)) %>%
  mutate(word = factor(word, levels = unique(word[order(-n)])))  # Order by frequency

# Create the plot showing the top words, their counts, and sentiment
ggplot(top_words, aes(y = n, x = word, fill = sentiment)) + 
  geom_col(alpha = 0.8) + 
  coord_flip() + 
  facet_wrap(~ Brand, scales = "free_y") +  # Use free_y for more space
  theme_linedraw() + 
  xlab(label = "Word") + 
  ylab(label = "Count") +
  ggtitle("Top 10 Most Used Words per Brand") +
  scale_fill_manual(values = c("positive" = "chartreuse4", "negative" = "darkred")) +  # Color by sentiment
  theme(legend.title = element_blank())

# Load libraries
library(wordcloud2)

# Load the predefined dataset of stop words
data("stop_words")

# Prepare data for the words analysis excluding stop words
word_counts <- reviews %>%
  unnest_tokens(word, Reviews) %>%  # Tokenization by single words
  anti_join(stop_words) %>%         # Exclude words in stop words
  count(Brand, word, sort = TRUE)   # Count the words by brand

# Aggregate the counts across all brands for the word cloud
overall_word_counts <- word_counts %>%
  group_by(word) %>%
  summarise(total_count = sum(n), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  head(15)  # Get the top 15 words across all brands

# Create the word cloud
wordcloud2(data = overall_word_counts, size = 1, color = "random-light", backgroundColor = "white")



####################### BIGRAMS-GRAPH #########################
library(igraph)
library(ggraph)
library(tidyr)

# Load the predefined dataset of stop words
data("stop_words")

# Creation of bigrams excluding stop words
biwords_df <- reviews %>%
  unnest_tokens(output = bigram, input = Reviews, token = "ngrams", n = 2) %>%
  # Remove rows with NA bigrams
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Exclude stop words from the bigrams
  anti_join(stop_words, by = c("word1" = "word")) %>%
  anti_join(stop_words, by = c("word2" = "word"))

# Count couples of words by brand
bigram_net <- biwords_df %>%
  group_by(Brand) %>%
  count(word1, word2, sort = TRUE) %>%
  ungroup()

# First 10 top couples of words 
top_bigrams_per_brand <- bigram_net %>%
  group_by(Brand) %>%
  top_n(10, n) %>%
  ungroup()

# Visualization of 10 couples of words
top_bigrams_per_brand %>%
  mutate(bigram = paste(word1, word2)) %>%
  count(bigram, sort = TRUE) %>%
  head(10) %>%
  ggplot() + 
  geom_col(aes(y = n, x = reorder(bigram, n)), fill = "cyan") +
  coord_flip() + 
  theme_linedraw() + 
  xlab(label = "Bigrams") + 
  ggtitle("Top 10 Bigrams")

# Graph
bigram_igraph <- top_bigrams_per_brand %>%
  filter(n > 2) %>%
  graph_from_data_frame()

# Adding the frequency attribute for the node
V(bigram_igraph)$size <- degree(bigram_igraph)
V(bigram_igraph)$color <- ifelse(V(bigram_igraph)$name %in% unique(top_bigrams_per_brand$Brand), "aquamarine3", "deeppink")  # Different color for brands

# Creation of the arrow for the graph
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

# Graph
set.seed(7)
ggraph(bigram_igraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(aes(size = size, color = color), show.legend = FALSE) +  
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void() +
  ggtitle("Graph of the Bigrams per Brand")



################### CORRELATION BETWEEN RATING AND REVIEW ###########################

# Selection of rating and sentiment without NA rows
  correlation_analysis <- reviewsSentiment %>%
    select(Rating, sentiment) %>%
    na.omit() 
  
# Calculate the correlation
  correlation_value <- cor(correlation_analysis$Rating, correlation_analysis$sentiment)
  
  print(paste("Correlation between Rating and Sentiment:", correlation_value))
  
 #graph 
ggplot(correlation_analysis, aes(x = Rating, y = sentiment)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Linear regression
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a horizontal dashed line at y = 0
    theme_minimal() +
    xlab("Rating (Stars' Number)") +
    ylab("Sentiment Score") +
    ggtitle("Correlation between Rating and Sentiment") +
    annotate("text", x = 1, y = 0.5, 
             label = paste("Correlation:", round(correlation_value, 2)), 
             color = "blue", size = 5)

# Selection of rating and sentiment without NA rows
correlation_analysis <- reviewsSentiment %>%
  select(Rating, sentiment) %>%
  na.omit() 

# Calculate the correlation
correlation_value <- cor(correlation_analysis$Rating, correlation_analysis$sentiment)

print(paste("Correlation between Rating and Sentiment:", correlation_value))

# Box plot
ggplot(correlation_analysis, aes(x = factor(Rating), y = sentiment)) +  # Convert Rating to factor for box plot
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +  # Boxplot with customization
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a horizontal dashed line at y = 0
  theme_minimal() +
  xlab("Rating (Stars' Number)") +
  ylab("Sentiment Score") +
  ggtitle("Box Plot of Sentiment Score by Rating") +
  annotate("text", x = 1, y = 0.5, 
           label = paste("Correlation:", round(correlation_value, 2)), 
           color = "blue", size = 5)


######################### NUMBER OF REVIEWS BY BRAND ########################
  
# Summary of sentiments by brand
  sentiment_summary <- reviewsSentiment %>%
    group_by(Brand, sentiment_label) %>%  
    summarise(n = n(), .groups = "drop")    
  
# Graph
  ggplot(sentiment_summary, aes(x = Brand, y = n, color = sentiment_label)) +
    geom_line(aes(group = sentiment_label), alpha = 0.8) +  
    geom_point(size = 3, alpha = 0.8) +                   
    geom_text(aes(label = n), vjust = -0.5, size = 3) +  
    theme_minimal() +
    ylab(label = "Number of Reviews") +
    xlab(label = "Brand") +
    scale_color_manual(values = c("Negative" = "red", "Positive" = "green", "Neutral" = "gray")) +  # Colori
    theme(legend.position = "bottom", 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
    labs(color = "Sentiments") +
    coord_flip() +                                   
    ggtitle("Number of reviews by Brand")
  
  

 



