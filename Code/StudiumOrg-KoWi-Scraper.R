# studium.org: Studiengänge
# Kommunikationswissenschaftliche Universitätsstandorte

# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, qdap, janitor, robotstxt)

# General scheme
# (1) Inspect the website
# (a) Is the information available
# (b) Am I allowed to scrape the page
# (2) Retrieve the HTML data
# (3) Parse the data for the interesting chunk of information
# (4) Store the data
# (5) Repeat (loops)
# (6) Clean data

# (1) Inspect the URL
## ---------------------------------------------------------------------- ##

# (a) Is the information available
# Page listing all universities that grant degrees in "sociology" (BA&MA)
uni.url <- "https://www.studium.org/kommunikationswissenschaft/uebersicht-universitaeten"

# Each university has its' own page with additional information; grab all links
# from the parent url
links <- read_html(uni.url) %>%
  html_nodes(".lh1 a") %>%
  html_attr("href")

url <- links %>%
  paste0("http://www.studium.org/", .)

# (b) I scraping allowed/prohibited?
paths_allowed("http://www.studium.org/") # No robots.txt; check "impressum"


# (2) Retrieve the html data / (3) Parse data
## ---------------------------------------------------------------------- ##
# Get the information from all university pages
# Function applicable to all pages
steckbrief_fun <- function(x) {
  read_html(x) %>% # (2) Retrieve
    html_nodes(".right") %>% # (3) Parse
    html_text()
}

# Save scraping
save_steckbrief_fun <- possibly(steckbrief_fun, NA_real_)

# (4) Store the data / (5) Repeat in a loop
## ---------------------------------------------------------------------- ##

# Apply to all pages using purrr::map
steckbrief.info <- map(.x = url, ~ {
  Sys.sleep(3)
  save_steckbrief_fun(.x)
})

# (6) Clean data
## ---------------------------------------------------------------------- ##
# Scrape variable names for the construction of a data frame
# Variables should be constant over pages (better check this assumption)
var.names <- read_html(url[1]) %>%
  html_nodes(".rel.fs4") %>%
  html_text() %>%
  bracketX()

# General data cleaning (regex, tidying)
names(steckbrief.info) <- str_extract_all(links, "(?<=/)[:alpha:].*")

# Make the data tidy
uni.df <- steckbrief.info %>%
  enframe() %>%
  unnest()

# Transform messy numbers to tidy ones
tidy.value <- uni.df$value %>%
  str_extract_all(., "[:digit:]+,|.[:digit:]+") %>%
  map(., paste0, collapse = "") %>%
  unlist() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(., ",", "\\.") %>%
  parse_number()

uni.df <- uni.df %>%
  mutate(
    value = tidy.value,
    var = rep(var.names, length(unique(name)))
  ) %>%
  spread(var, value) %>%
  clean_names() %>%
  mutate(einwohnerzahl_stadt = ifelse(einwohnerzahl_stadt <= 999, # population too small on website
    einwohnerzahl_stadt * 1000,
    einwohnerzahl_stadt
  )) %>%
  select(-v1)

# Rename a "th-koeln" which is wrongly named "onlineredakteur"
uni.df[which(uni.df$name == "onlineredakteur"),]$name <- "th-koeln"

# Visualize
## ---------------------------------------------------------------------- ##

uni.df %>% 
  filter(!is.na(sonnenstunden_pro_jahr)) %>%
  ggplot() +
  geom_point(aes(fct_reorder(name, sonnenstunden_pro_jahr), sonnenstunden_pro_jahr), 
             stat = "identity") +
  labs(title = "Deutsche KoWi-Institute nach Sonnenstunden im Jahr",
       subtitle = paste0("Die Differenz zwischen Berlin und Münster beträgt ", 
                         round(
                           abs(
                             uni.df$sonnenstunden_pro_jahr[uni.df$name == "uni-muenster"] -
                               uni.df$sonnenstunden_pro_jahr[uni.df$name == "udk-berlin"]) /
                             24, 
                           digits = 1),
                         " Sonnentage im Jahr."),
       x = "", y = "", caption = "Quelle: studium.org") +
  geom_point(data = subset(uni.df, name %in% c("uni-muenster", "udk-berlin")),
             aes(fct_reorder(name, sonnenstunden_pro_jahr), sonnenstunden_pro_jahr), 
             stat = "identity", color = "red", size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(size = .5))