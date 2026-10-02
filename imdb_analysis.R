install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)

basics <- read_tsv("E:/My Projects/Dataset/title.basics.tsv", na = "\\N")
ratings <- read_tsv("E:/My Projects/Dataset/title.ratings.tsv", na = "\\N")

movies <- basics %>%
  filter(titleType == "movie") %>%
  inner_join(ratings, by = "tconst")

glimpse(movies)

# Average rating by genre
movies %>%
  filter(!is.na(genres), numVotes >= 1000) %>%
  separate_rows(genres, sep = ",") %>%
  group_by(genres) %>%
  summarise(
    avg_rating = mean(averageRating),
    total_movies = n()
  ) %>%
  arrange(desc(avg_rating))

movies %>%
  filter(!is.na(genres), numVotes >= 1000) %>%
  separate_rows(genres, sep = ",") %>%
  group_by(genres) %>%
  summarise(
    avg_rating = mean(averageRating),
    total_movies = n()
  ) %>%
  arrange(desc(avg_rating)) %>%
  print(n = 25)

akas <- read_tsv("E:/My Projects/Dataset/title.akas.tsv", na = "\\N")

# Get country of origin for each movie
origin <- akas %>%
  filter(isOriginalTitle == 1, !is.na(region)) %>%
  select(titleId, region) %>%
  rename(tconst = titleId)

# Join with movies
movies_country <- movies %>%
  inner_join(origin, by = "tconst") %>%
  filter(numVotes >= 100, !is.na(averageRating))

# Quick check
movies_country %>%
  count(region, sort = TRUE) %>%
  head(20)

# Check what's in akas
glimpse(akas)

# Check if isOriginalTitle has any 1s
table(akas$isOriginalTitle)

# Get country by taking the first regional entry per title
origin <- akas %>%
  filter(!is.na(region)) %>%
  group_by(titleId) %>%
  slice_min(ordering, n = 1) %>%
  ungroup() %>%
  select(titleId, region) %>%
  rename(tconst = titleId)

# Join with movies
movies_country <- movies %>%
  inner_join(origin, by = "tconst") %>%
  filter(numVotes >= 100, !is.na(averageRating))

# Check top countries
movies_country %>%
  count(region, sort = TRUE) %>%
  head(20)

# Free up memory
rm(akas)
gc()

glimpse(origin)

akas_small <- read_tsv(
  "E:/My Projects/Dataset/title.akas.tsv",
  col_select = c(titleId, ordering, region),
  na = "\\N"
)

# Check sample of region values
akas_small %>%
  filter(!is.na(region)) %>%
  head(20) %>%
  print()

origin <- akas_small %>%
  filter(!is.na(region)) %>%
  group_by(titleId) %>%
  slice_min(ordering, n = 1) %>%
  ungroup() %>%
  rename(tconst = titleId) %>%
  select(tconst, region)

# Check it worked
nrow(origin)

# Join with movies
movies_country <- movies %>%
  inner_join(origin, by = "tconst") %>%
  filter(numVotes >= 100, !is.na(averageRating))

# Check top countries
movies_country %>%
  count(region, sort = TRUE) %>%
  head(20)

# Define Western vs Non-Western countries explicitly
western <- c("US", "GB", "FR", "DE", "IT", "ES", "AU", "CA")
non_western <- c("IN", "BD", "KR", "JP", "TR", "NG", "CN", "IR", "PK", "TH", "ID")

# Get movies for each group
comparison <- movies_country %>%
  filter(region %in% c(western, non_western)) %>%
  mutate(group = ifelse(region %in% western, "Western", "Non-Western"))

# Compare ratings and votes
comparison %>%
  group_by(group) %>%
  summarise(
    avg_rating = round(mean(averageRating), 2),
    median_votes = median(numVotes),
    avg_votes = round(mean(numVotes)),
    total_movies = n()
  )

# Bar chart - ratings comparison
comparison %>%
  group_by(group) %>%
  summarise(avg_rating = mean(averageRating)) %>%
  ggplot(aes(x = group, y = avg_rating, fill = group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(avg_rating, 2)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Non-Western" = "#E63946", "Western" = "#457B9D")) +
  labs(
    title = "Non-Western Films Rate Higher — But Nobody Sees Them",
    subtitle = "Average IMDB rating: Western vs Non-Western cinema",
    x = "", y = "Average Rating",
    caption = "Source: IMDB Public Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Votes comparison - the shocking one
comparison %>%
  group_by(group) %>%
  summarise(avg_votes = mean(numVotes)) %>%
  ggplot(aes(x = group, y = avg_votes, fill = group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(round(avg_votes))), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Non-Western" = "#E63946", "Western" = "#457B9D")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Western Films Get 6x More Votes on IMDB",
    subtitle = "Average number of votes: Western vs Non-Western cinema",
    x = "", y = "Average Number of Votes",
    caption = "Source: IMDB Public Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Country by country comparison
comparison %>%
  group_by(region) %>%
  summarise(
    avg_rating = mean(averageRating),
    avg_votes = mean(numVotes),
    total_movies = n()
  ) %>%
  filter(total_movies >= 20) %>%
  mutate(group = ifelse(region %in% western, "Western", "Non-Western")) %>%
  ggplot(aes(x = reorder(region, avg_rating), y = avg_rating, fill = group)) +
  geom_col() +
  geom_text(aes(label = round(avg_rating, 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("Non-Western" = "#E63946", "Western" = "#457B9D")) +
  labs(
    title = "Average IMDB Rating by Country",
    subtitle = "Non-Western countries highlighted in red",
    x = "Country", y = "Average Rating",
    fill = "",
    caption = "Source: IMDB Public Dataset | Min. 20 movies"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

comparison %>%
  group_by(region) %>%
  summarise(
    avg_rating = mean(averageRating),
    avg_votes = mean(numVotes),
    total_movies = n(),
    group = first(group)
  ) %>%
  filter(total_movies >= 20) %>%
  ggplot(aes(x = avg_votes, y = avg_rating, color = group, label = region)) +
  geom_point(aes(size = total_movies), alpha = 0.8) +
  geom_text(vjust = -1, size = 4, fontface = "bold") +
  scale_color_manual(values = c("Non-Western" = "#E63946", "Western" = "#457B9D")) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Better Ratings, Fewer Votes — The Non-Western Cinema Gap",
    subtitle = "Each dot = one country | Size = number of movies | X-axis on log scale",
    x = "Average Number of Votes (log scale)",
    y = "Average IMDB Rating",
    color = "",
    size = "Movies",
    caption = "Source: IMDB Public Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("chart3_scatter.png", width = 10, height = 7, dpi = 300)

# Regenerate and save chart 1 - ratings
comparison %>%
  group_by(group) %>%
  summarise(avg_rating = mean(averageRating)) %>%
  ggplot(aes(x = group, y = avg_rating, fill = group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(avg_rating, 2)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Non-Western" = "#E63946", "Western" = "#457B9D")) +
  labs(title = "Non-Western Films Rate Higher — But Nobody Sees Them",
       subtitle = "Average IMDB rating: Western vs Non-Western cinema",
       x = "", y = "Average Rating", caption = "Source: IMDB Public Dataset") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("chart1_ratings.png", width = 10, height = 7, dpi = 300)

# Regenerate and save chart 2 - votes
comparison %>%
  group_by(group) %>%
  summarise(avg_votes = mean(numVotes)) %>%
  ggplot(aes(x = group, y = avg_votes, fill = group)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(round(avg_votes))), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Non-Western" = "#E63946", "Western" = "#457B9D")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Western Films Get 6x More Votes on IMDB",
       subtitle = "Average number of votes: Western vs Non-Western cinema",
       x = "", y = "Average Number of Votes", caption = "Source: IMDB Public Dataset") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("chart2_votes.png", width = 10, height = 7, dpi = 300)







