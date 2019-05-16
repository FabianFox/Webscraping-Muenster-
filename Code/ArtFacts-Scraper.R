# Scraping a dynamic webpage: https://artfacts.net/
## ---------------------------------------------------------------------- ##

# DO NOT RUN / USE API

# Install/load packages
## ---------------------------------------------------------------------- ##
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, RSelenium, WikipediR, rvest)

# Setting up the Docker container
## ---------------------------------------------------------------------- ##

# Note: Docker container (a virtual environment)
# The browser is run within a Docker container. A excellent introduction to
# Docker (in R) is provided by ROpenSciLab: https://ropenscilabs.github.io/r-docker-tutorial/

# Docker needs some commands from within the command line (here: PowerShell)
# Command overview: https://docs.docker.com/engine/reference/commandline/docker/

# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0

# Connecting the Docker container and R
## ---------------------------------------------------------------------- ##
# Initialize the RSelenium server running chrome (Downloading )
remDr <- RSelenium::rsDriver(remoteServerAddr = "localhost", port = 8788L, browser = "firefox")

# Open the client to steer the browser
rD <- remDr[["client"]]

# Steering the container
## ---------------------------------------------------------------------- ##

# Go to ArtFacts
rD$navigate("https://artfacts.net/")

# Find the search window
search_win <- rD$findElement(using = "css selector", "#q")
search_win$clickElement()

# Enter search term (here, we could supply a list of artists, which is consecutively looped over)
search_win$sendKeysToElement(list("Gerhard Richter", key = "enter"))

# Get the respective URL
url <- rD$getPageSource()[[1]]

# Standard rvest from here on
richter.df <- read_html(url) %>%
  html_nodes(".app-js-components-Card-Card__info") %>%
  html_table()

# Now, we create a loop to extract several artists (Source: Wikipedia)
# Package: WikipediR (API)
# Page: https://en.wikipedia.org/wiki/Category:German_contemporary_artists
## ---------------------------------------------------------------------- ##
artists.list <- pages_in_category(language = "en", project = "wikipedia", 
                                  categories = "German contemporary artists", 
                                  limit = 250)

# Annoying lists...
artists.vec <- vector("character", length = length(artists.list$query$categorymembers))

for(i in seq_along(artists.vec)){
  artists.vec[[i]] <- artists.list$query$categorymembers[[i]]$title
}

# Let's have a look
head(artists.vec)

# Back to RSelenium
## ---------------------------------------------------------------------- ##
# Just for demo purpose; there is an API; request access to gather information
# from ArtFacts.net (Link to API: https://github.com/ArtFacts/api)

# Ten random artists
artists.vec10 <- sample(artists.vec, size = 10)

artists.url <- vector("list", length = 10)

rD$navigate("https://artfacts.net/")

for(i in seq_along(artists.vec10)){
  
  # Find the search window
  search_win <- rD$findElement(using = "css selector", "#q")
  search_win$clickElement()
  
  # Enter search key
  search_win$sendKeysToElement(list(artists.vec10[i]))
  
  Sys.sleep(sample(seq(2, 5, 0.5), 1))
  
  search_win$sendKeysToElement(list(key = "enter"))
  
  Sys.sleep(sample(seq(3, 5, 0.5), 1))
  
  # Get the respective URL
  url <- rD$getPageSource()[[1]]
  
  artists.url[[i]] <- url
  
  rD$goBack()
  
  Sys.sleep(sample(seq(0, 5, 0.5), 1))
}

# Use rvest to extract information from the respective artist URLs
## ---------------------------------------------------------------------- ##

# Function that extracts the tables
extract_table <- function(url){
  read_html(url) %>%
    html_nodes(".app-js-components-Card-Card__info") %>%
    html_table()
}

# Transform URLs to vector
artists.url <- unlist(artists.url)

# Loop using purrr::map
artists.df <- map(artists.url, extract_table)

# Stop R connection to Docker / Terminate Docker instance
## ---------------------------------------------------------------------- ##
remDr$server$stop()

# Stop Docker (in PowerShell)
# docker ps
# docker stop NAMEOFINSTANCE