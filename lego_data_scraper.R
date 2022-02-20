library(tidyverse)
library(dplyr)

# get system's year to keep data updated
current_year = lubridate::year(Sys.Date())
page = 1
# define url to scrape
url = glue::glue("https://brickset.com/sets/year-{current_year-1}/filter-Released/page-{page}")

# obtain html document from url
lego_data = read_html(url)

# define table attributes; 
# html nodes function is used to select parts of a document using CSS selectors
# html text function is used to extract the text the selected nodes
set_name = lego_data %>%
  html_nodes("div.highslide-caption > h1") %>%
  html_text()

# sapply function is used to keep the data consistent in case of missing values
# the function checks if the html node "rating" exists and if not it adds and NA 
rating = lego_data %>% 
  html_nodes(".meta")

rating = sapply(rating, function(x){x %>% 
    html_nodes(".rating > span") %>%
    html_text() %>% 
    str_trim() %>% as.character()}) %>% 
  sapply(function(x) ifelse(length(x) == 0, NA, x))

pieces = lego_data %>%
  html_elements(xpath ='.//dl[dt/text() = "Pieces"]/dd[1]/a/text()') %>%
  html_text() 
piece = data.frame(pieces)
head(piece)


test = data.frame(set_name, rating)
head(test)

