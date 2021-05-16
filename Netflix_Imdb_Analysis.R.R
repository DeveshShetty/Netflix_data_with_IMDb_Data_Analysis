install.packages("tidyverse")
install.packages("ggplot")
install.packages("plotly")
install.packages("wordcloud")
install.packages('tidytext')
install.packages('caret')
install.packages('caTools')
install.packages('randomForest')

library("tidyverse")
library("plotly")
library('wordcloud')
library('tidytext')
library('caret')
library('caTools')
library('randomForest')

df = read.csv("E:\\ikeee\\A_Rtut Important\\new\\netflix_titles.csv", stringsAsFactors = FALSE)
df$date_added = as.Date(df$date_added, format = "%B %d, %Y")

glimpse(df)
summary(df)

#dataset split to check duration
movies = df %>% 
  select(country, type, duration, rating, title) %>%
  filter(type == "Movie") %>%
  drop_na() %>% 
  mutate(duration_min = parse_number(duration))

tv_show = df %>% 
  select(country, type, duration, rating, title) %>%
  filter(type == "TV Show") %>% 
  drop_na() %>% 
  mutate(duration_season = parse_number(duration))

#movie duration distribution
movies %>%
  plot_ly(
    x = ~ duration_min,
    type = "histogram",
    nbinsx = 40,
    marker = list(
      color = "drakblue",
      line = list(color = "black",
                  width = 1)
    )
  ) %>%
  layout(
    title = "Duration distrbution",
    yaxis = list(title = "Count",
                 zeroline = FALSE),
    xaxis = list(title = "Duration (min)",
                 zeroline = FALSE)
  ) 

#Season distribution
tv_show %>% select(duration_season) %>%
  count(duration_season, sort = TRUE) %>%
  ggplot(aes(
    x = as.factor(duration_season),
    y = n,
    label = n
  )) +
  geom_col(aes(fill = duration_season)) +
  geom_text(vjust = -0.5, size = 3, col = "darkgreen") +
  theme_light() +
  theme(legend.position = "none") +
  labs(x = "Season duration",
       y = "Count",
       title = "Season distrbution",
       subtitle = "Column Plot, Season distrbution",
       fill = ""
  )


#Approx number of titles of each country
df_country = filter(df, nchar(country)>0 & nchar(type)>0)

df_country %>%
  filter(!str_detect(country,',')) %>%
  group_by(country) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot() + geom_col(aes(y = reorder(country,n), x = n)) +
  geom_label(aes(y = reorder(country,n), x = n, label = n)) +
  labs(title = 'Approx. Number of Titles of each Country') +
  theme_minimal()


#Trend of titles every year
df_country %>%
  filter(release_year !=2021) %>%
  group_by(type,release_year) %>%
  count() %>%
  ggplot()+geom_line(aes(x=release_year,y=n,group=type,colour=type)) +
  labs(title = 'Trend of Titles every Year') +
  theme_minimal()

#Are Movies on Netflix more than TV shows?
df %>% count(type, sort = T) %>%
  mutate(prop = paste0(round(n / sum(n) * 100, 0), "%")) %>%
  ggplot(aes(x = "", y = prop, fill = type)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "steelblue",
    size = 1
  ) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(y = prop, label = prop),
    position = position_stack(vjust = 0.5),
    size = 6,
    col = "white",
    fontface = "bold"
  ) +
  scale_fill_manual (values = c('#e41a1c', '#377eb8')) +
  theme_void() +
  labs(
    title = "Are Movies on Netflix more than TV shows?",
    subtitle = "Pie Plot, proportion of Movies to TV shows",
    fill = ""
  )

#Total number of shows and movies for each type
df_country %>%
  count(type) %>%
  ggplot() + geom_col(aes(x = type, y = n, fill = type)) +
  labs(title = "Show Types")+
  theme_minimal()


# Number of titles based on themes / Genre of Titles

df_country %>%
  select(listed_in) %>%
  mutate(listed_in = str_split(listed_in,',')) %>%
  unnest(listed_in) %>%
  mutate(listed_in= trimws(listed_in, which = c("left")))%>%
  group_by(listed_in) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(30) %>%
  ggplot() + geom_col(aes(y = reorder(listed_in,n), x = n)) +
  labs(title = 'Themes / Genre of Titles',
       x = '# of Titles',
       y = 'Theme / Genre') +
  theme_minimal()


#Where do the majority of movies available on Netflix come from?
df_country = df %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country) %>%
  mutate(country = trimws(country, which = c("left")))  #eliminate space on the left side

df_country = df_country %>%
  group_by(country)%>%
  add_tally()

df_country = df_country%>%
  select(country,n,type) %>%
  unique() 

df_country_top = df_country[order(-df_country$n),]
df_country_top = df_country_top[1:35,]

ggplot(df_country_top, aes(x = reorder(country, n), y = n, fill = type))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(title="Content available per country", x = "Amount of content")

#Rating by type
df %>% 
  select(rating, type) %>%
  filter(!is.na(rating)) %>%
  mutate(rating = fct_lump(rating, 5)) %>%
  group_by(rating, type) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ type ,
    y = ~ Count,
    type = "bar",
    color = ~ rating,
    text = ~ Count,
    textposition = 'outside',
    textfont = list(color = '#000000', size = 12)
  ) %>%
  layout(yaxis = list(categoryorder = "array",
                      categoryarray = ~ Count)) %>%
  layout(
    title = "Rating by Type",
    yaxis = list(title = "Count"),
    xaxis = list(title = "Type"),
    legend = list(title = list(text = '<b> Rating </b>'))
  )

#Most frequent word for movies
desc_words_m = df %>% 
              select(type, show_id, description) %>%
              filter(type == "Movie") %>% 
              unnest_tokens(word, description) %>%
              anti_join(stop_words)

count_word = desc_words_m %>%
            filter(nchar(word)>1) %>%
            count(word, sort = TRUE) 


wordcloud(words = count_word$word,  
          freq = count_word$n, 
          min.freq = 50,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2"))

#Most frequent word for tv shows 
desc_words_tv = df %>% 
                select(type, show_id, description) %>%
                filter(type == "TV Show") %>% 
                unnest_tokens(word, description) %>%
                anti_join(stop_words)

count_word = desc_words_tv %>%
            filter(nchar(word)>1) %>%
            count(word, sort = TRUE)


wordcloud(words = count_word$word,  
          freq = count_word$n, 
          min.freq = 30,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2"))


#loading IMDB data sets and merging them
combined_df  <- merge(read.csv("E:\\ikeee\\A_Rtut Important\\new\\IMDb_movies.csv", stringsAsFactors = FALSE) %>% select(imdb_title_id, title),
                      read.csv("E:\\ikeee\\A_Rtut Important\\new\\IMDb_ratings.csv", stringsAsFactors = FALSE) %>% select(imdb_title_id, rating = weighted_average_vote),
                      by = 'imdb_title_id') %>%
              select(-imdb_title_id)

new_df  <- merge(df, combined_df, by = 'title') %>%
  select(type, duration, genre = listed_in, year = release_year, rating = rating.y) %>%
  mutate(duration = parse_number(duration))

new_df <- new_df %>%
  filter(type == 'Movie') %>%
  select(-type)

#create training and validation data from given data
set.seed(88)
split <- sample.split(new_df$rating, SplitRatio = 0.70)

#get training and test data
train <- subset(new_df, split == TRUE)
test <- subset(new_df, split == FALSE)

train = train %>%
  mutate(genre = strsplit(as.character(genre), ",")) %>% 
  unnest(genre) %>%
  mutate(genre = trimws(genre, which = c("left"))) %>%
  mutate(genre = tolower(genre))

test = test %>%
  mutate(genre = strsplit(as.character(genre), ",")) %>% 
  unnest(genre) %>%
  mutate(genre = trimws(genre, which = c("left"))) %>%
  mutate(genre = tolower(genre))

#dummify data
dmy <- dummyVars("~ .", data= train)
train <- data.frame(predict(dmy, newdata = train)) 

dmy <- dummyVars("~ .", data= test)
test <- data.frame(predict(dmy, newdata = test)) 

#Multi Linear regression
model <- lm(rating ~., data=train)
predicted_value <- predict(model, newdata = test)

multi_linear = as.data.frame(cbind(Actual = test$rating, Predicted = predicted_value))
error =  (multi_linear$Actual - multi_linear$Predicted)
multi_linear = as.data.frame(cbind(multi_linear,error))
rmse = sqrt(mean((error)^2))
head(multi_linear)
print(rmse)

#Random Forest
model <- randomForest(rating ~., data=train)
predicted_value <- predict(model, newdata = test)

random_forest = as.data.frame(cbind(Actual = test$rating, Predicted = predicted_value))
error =  (random_forest$Actual - random_forest$Predicted)
random_forest = as.data.frame(cbind(random_forest,error))
rmse = sqrt(mean((error)^2))
head(random_forest)
print(rmse)

