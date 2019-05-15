# SPON-Scraper
# Articles featuring "Martin Schulz"

# Load packages
## ---------------------------------------------------------------------- ##

if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, lubridate)

# Approach
## ---------------------------------------------------------------------- ##
# 1. Internetseite kennenlernen
# 2. Import von HTML-Dateien
# 3. Information isolieren
# 4. Iteration (loops)

# First starting with a single page then generalize in a loop
## ---------------------------------------------------------------------- ##

# first page of search results
url <- "https://www.spiegel.de/suche/?suchbegriff=Martin+Schulz&suchzeitraum=ab2005&suchbereich=header%2Ctitle%2Clead&fromDate=01.01.2005&quellenGroup=SPOX&quellenGroup=SP"

# CSS-Selectors (Nodes)
# Date: .search-teaser div
# Title: .search-teaser .headline
# Teaser: .article-intro

# Get the data (single page)
spon.df <- read_html(url) %>%                  # import html page
  html_nodes(css = ".search-teaser div") %>%   # select nodes
  html_text()                                  # transform to text

# Generalize to multiple pages
## ---------------------------------------------------------------------- ##

# Observe the behavior of the URL when further sites are queried
# Here: It just appends the respective page number to the URL
# We can exploit this by creating a vector that holds all page. We then loop
# over all pages and retrieve the respective information. Still, we need to get 
# somewhat abstract (i.e. using functions)

# Create a data frame with all pages
links.df <- tibble(
  links = paste0("https://www.spiegel.de/suche/?suchbegriff=Martin+Schulz&suchbereich=header,title,lead&suchzeitraum=ab2005&fromDate=01.01.2005&quellenGroup=SPOX&quellenGroup=SP&pageNumber=", seq(1:30))
)

# Create a function that extracts the relevant information
spon_scraper <- function(x) {
  page <- read_html(x)
  
  date <- page %>%
    html_nodes(".search-teaser div") %>%
    html_text()
  
  title <- page %>%
    html_nodes(".search-teaser .headline") %>%
    html_text()
  
  df <- tibble(date, title)

  teaser <- page %>%
    html_nodes(".article-intro") %>%
    html_text()
  
  df <- tibble(date, title, teaser)
}

# Get all the beautiful data
spon.df <- map_dfr(
  links.df$links, ~{
    Sys.sleep(sample(seq(0, 3, 0.5), 1))
    spon_scraper(.x)
  })

# Data cleaning
## ---------------------------------------------------------------------- ##
spon.df <- spon.df %>%
  separate(date, into = c("source", "section", "date"), sep = "-") %>%
  mutate_at(c("source", "section", "date"), str_trim, "both") %>%
  .[-c(115, 173, 174, 187, 271, 308, 309, 363, 394, 473, 501, 536, 555, 570, 586),] %>%  # Irregular article headers 
  mutate(date = ymd(parse_date_time(date, "d!.m!*.Y!")))