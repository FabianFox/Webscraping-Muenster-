# wahlrecht.de-Scraper

# Links to the individual polling institutes
institute.href <- read_html("https://www.wahlrecht.de/umfragen/index.htm") %>%
  html_nodes(".in a") %>%
  html_attr("href") %>%
  tibble(institute = str_extract(., "[:alpha:]+(?=.)"),
         link = paste0("https://www.wahlrecht.de/umfragen/", .)) %>%
  rename("href" = 1)

# For filtering
parties <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD") 

# (2) Create a function that grabs the tables 
poll_scraper <- function(x) {
  read_html(x) %>%
    html_nodes(".wilko") %>%
    html_table(fill = TRUE, header = TRUE) %>%
    .[[1]] %>%
    .[min(                                                                         # Which row is the first one with a date? 
      which(
        str_detect(
          string = .[,1], "[:digit:]+.?[:digit:]+.?[:digit:]+") == TRUE)):nrow(.), 
      c(1, which(colnames(.) %in% parties))] %>%                                   
    rename("Zeitpunkt" = 1) %>%
    mutate(Zeitpunkt = parse_datetime(Zeitpunkt, format = "%d.%m.%Y")) %>%
    mutate_if(is.character, str_extract, pattern = "[:digit:]+,?[:digit:]?") %>%
    mutate_if(is.character, str_replace, pattern = ",", replacement = ".") %>%
    mutate_if(is.character, as.numeric) %>%
    gather(party, vote, -Zeitpunkt)
}

# (3) Grab all polls of all institute
polls <- map(institute.href$link, poll_scraper) %>% 
  setNames(institute.href$institute) # Name the list

# (4) Name lists and combine in a data set
polls.df <- polls %>%
  bind_rows(.id = "institute") 

# (5) Election 2017 (Schulz-Hype)
election2017.df <- polls.df %>%
  filter(institute == "forsa") %>%
  mutate(zeitpunkt = ymd(Zeitpunkt)) %>%
  select(-Zeitpunkt) %>%
  filter(party == c("CDU/CSU", "SPD"),
         between(zeitpunkt, ymd("2017-01-01"), ymd("2018-02-28"))) %>%
  as_tibble()