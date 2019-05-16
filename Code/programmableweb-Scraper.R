# Scraping poll data from programmableweb.com
## ---------------------------------------------------------------------- ##

# Install/load packages
## ---------------------------------------------------------------------- ##
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, lubridate, rvest)

# Scrape
## ---------------------------------------------------------------------- ##

# all pages with information on URLs
all.urls <- map_chr(0:726, ~paste0("https://www.programmableweb.com/category/all/apis?page=", .x))

# Function that grabs the respective pages
api_fun <- function(x) {
  read_html(x) %>%
    html_nodes(".views-table") %>%
    html_table(header = TRUE, fill = TRUE)
}

# Map function over all pages
# (1) Allocate space
api.list <- vector(mode = "list", length = length(all.urls))

# (2) Run (add purrr::possibly)
for(i in seq_along(all.urls)){
  api.list[i] <- api_fun(all.urls[[i]])
  Sys.sleep(sample(seq(0, 3, 0.5), 1))
}
  
# (3) Combine as data frame
api.df <- bind_rows(api.list) %>%
  mutate(Submitted = as.numeric(str_extract(Submitted, "(?<=.)[:digit:]{4}")))

# Data wrangling and visualization
## ---------------------------------------------------------------------- ##
# Prepare data for plotting
api.plot.df <- api.df %>%
  group_by(Submitted) %>%
  count() %>%
  ungroup() %>%
  arrange(Submitted) %>%
  filter(!is.na(Submitted)) %>%
  mutate(cumulative.n = cumsum(n))

# Plot
ggplot(api.plot.df, aes(x = Submitted, y = cumulative.n)) +
  geom_line(stat = "identity") +
  theme_minimal() +
  labs(title = "Anzahl and APIs auf programmableweb.com",
       subtitle = "Eine steigende Anzahl an Unternehmen bietet APIs an.",
       caption = "Quelle: programmableweb.org",
       x = "", 
       y = "")
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks = element_line(size = .5))

  
# Save data
## ---------------------------------------------------------------------- ##
saveRDS(api.list, file = "./Slides/Figures/programmableweb_data.R")  
saveRDS(api.plot.df, file = "./Slides/Figures/api_plot_df.R") 
