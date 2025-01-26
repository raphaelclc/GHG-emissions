library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Read and preprocess data
read_and_preprocess_data <- function() {
  GHG_per_capita_by_country <- read.csv("../data/GHG_total_by_country.csv")
  Country_Continent <- read.csv("../data/Country_Continent.csv")
  
  worldwide_2023 <- GHG_per_capita_by_country %>% 
    filter(Country == "GLOBAL TOTAL") %>% 
    select(X2023)
  
  df <- inner_join(GHG_per_capita_by_country, Country_Continent, by="Country") %>%
    select(Country, Continent, "X2023") %>%
    mutate(Country_Perc = X2023 / worldwide_2023$X2023 * 100)
  
  list(df = df, worldwide_2023 = worldwide_2023)
}

# Summarize continent data
summarize_continent_data <- function(df, worldwide_2023) {
  df %>% 
    group_by(Continent) %>% 
    summarize(Continent_Perc = sum(X2023) / worldwide_2023$X2023 * 100)
}

# Get top 20 countries by emissions
get_top_20_countries <- function(df) {
  df %>% 
    arrange(desc(Country_Perc)) %>% 
    head(20)
}

# Continents color palette
create_color_palette <- function() {
  scale_fill_manual(values = c(
    "Africa" = "#1f77b4",
    "Asia" = "#ff7f0e",
    "Europe" = "#2ca02c",
    "North America" = "#d62728",
    "Oceania" = "#9467bd",
    "South America" = "#8c564b"
  ))
}

# Pie chart for continents
create_pie_chart <- function(continent_data, color_palette) {
  ggplot(continent_data, aes(x = "", y = Continent_Perc, fill = Continent)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = "GHG Emissions by Continent in 2023", fill = "Continent") +
    color_palette +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}

# Bar chart for top 20 countries
create_bar_chart <- function(top_20_countries, color_palette) {
  ggplot(top_20_countries, aes(x = reorder(Country, -Country_Perc), y = Country_Perc, fill = Continent)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 20 Countries by GHG Emissions in 2023", x = "Country", y = "World Emissions (%)", fill = "Continent") +
    color_palette +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}

# Main
color_palette <- create_color_palette()
data <- read_and_preprocess_data()
df <- data$df
worldwide_2023 <- data$worldwide_2023

continent_data <- summarize_continent_data(df, worldwide_2023)
top_20_countries <- get_top_20_countries(df)

pie_chart <- create_pie_chart(continent_data, color_palette)
bar_chart <- create_bar_chart(top_20_countries, color_palette)

combined_plot <- grid.arrange(pie_chart, bar_chart, ncol = 2)

ggsave("chart3.png", plot = combined_plot, width = 16, height = 8)
print(combined_plot)