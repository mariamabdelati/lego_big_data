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
setName = lego_data %>%
  html_nodes("div.highslide-caption > h1") %>%
  html_text()

# sapply function is used to keep the data consistent in case of missing values
# the function checks if the html node "rating" exists and if not it adds and NA 
rating = lego_data %>% 
  html_nodes(".meta")

rating = sapply(rating, function(feature){feature %>% 
    html_nodes(xpath = ".//div[@class='rating']/span/text()") %>%
    html_text() %>% 
    str_trim() %>% as.character()}) %>% 
  sapply(function(feature) ifelse(length(feature) == 0, NA, feature))

features = lego_data %>% 
  html_nodes(".rating+ .col")
pieces = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'Pieces']//following-sibling::dd/a/text()") %>%
    html_text()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

# minifigs = lego_data %>% 
#   html_nodes(".rating+ .col")
minifigs = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'Minifigs']//following-sibling::dd/a/text()") %>%
    html_text()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

RRP = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'RRP']//following-sibling::dd/text()") %>%
    html_text() %>%
    str_trim() %>% as.character()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

PPP = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'PPP']//following-sibling::dd") %>%
    html_text() %>%
    str_trim() %>% as.character()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

packaging = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'Packaging']//following-sibling::dd") %>%
    html_text() %>%
    str_trim() %>% as.character()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

availability = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'Availability']//following-sibling::dd") %>%
    html_text() %>%
    str_trim() %>% as.character()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

instructions = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'Instructions']//following-sibling::dd/a/text()") %>%
    html_text() %>%
    str_trim() %>% as.character()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

setType = sapply(features,function(feature) {feature %>%
    html_nodes(xpath ="./dl/dt[text() = 'Set type']//following-sibling::dd") %>%
    html_text() %>%
    str_trim() %>% as.character()
}) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))

test = data.frame(setName, rating, pieces, minifigs, RRP, PPP, packaging, availability, instructions, setType)
head(test)
