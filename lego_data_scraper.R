library(tidyverse)
library(dplyr)

# get system's year to keep data updated
current_year = lubridate::year(Sys.Date())
page = 1
# define url to scrape
url = glue::glue("https://brickset.com/sets/year-{current_year-1}/filter-Released/page-{page}")

# obtain html document from url
legoData = read_html(url)

# obtain links for nested pages
setLinks = legoData %>% 
  html_nodes("h1 a") %>%
  html_attr("href") %>%
  paste("https://brickset.com/", ., sep = "")

# define table attributes; 
# html nodes function is used to select parts of a document using CSS selectors
# html text function is used to extract the text the selected nodes
setName = legoData %>%
  html_nodes(xpath = ".//h1/a/text()") %>%
  html_text() %>%
  str_trim()

# sapply function is used to keep the data consistent in case of missing values
# the function checks if the html node "rating" exists and if not it adds and NA 
rating = legoData %>% 
  html_nodes(".meta")

rating = sapply(rating, function(feature){feature %>% 
    html_nodes(xpath = ".//div[@class='rating']/span/text()") %>%
    html_text() %>% 
    str_trim() %>% as.character()}) %>% 
  sapply(function(feature) ifelse(length(feature) == 0, NA, feature))

features = legoData %>% 
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

#nested pages attriute retrieval functions 
getSetNumber = function(setLink){
  setPage = read_html(setLink) 
  details = setPage %>% 
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_number = sapply(details,function(feature) {feature %>%
      html_nodes(xpath ="./dl/dt[text() = 'Set number']//following-sibling::dd[1]") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))
  
  return(set_number)
}

getThemeGroup = function(setLink){
  setPage = read_html(setLink) 
  details = setPage %>% 
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_themeGroup = sapply(details,function(feature) {feature %>%
      html_nodes(xpath ="./dl/dt[text() = 'Theme group']//following-sibling::dd[1]") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))
  
  return(set_themeGroup)
}

getTheme = function(setLink){
  setPage = read_html(setLink) 
  details = setPage %>% 
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_theme = sapply(details,function(feature) {feature %>%
      html_nodes(xpath ="./dl/dt[text() = 'Theme']//following-sibling::dd[1]/a/text()") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))
  
  return(set_theme)
}

getSubtheme = function(setLink){
  setPage = read_html(setLink) 
  details = setPage %>% 
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_subtheme = sapply(details,function(feature) {feature %>%
      html_nodes(xpath ="./dl/dt[text() = 'Subtheme']//following-sibling::dd[1]/a/text()") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))
  
  return(set_subtheme)
}

getYear = function(setLink){
  setPage = read_html(setLink) 
  details = setPage %>% 
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_year = sapply(details,function(feature) {feature %>%
      html_nodes(xpath ="./dl/dt[text() = 'Year released']//following-sibling::dd[1]/a/text()") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))
  
  return(set_year)
}

getAge = function(setLink){
  setPage = read_html(setLink) 
  details = setPage %>% 
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_age = sapply(details,function(feature) {feature %>%
      html_nodes(xpath ="./dl/dt[text() = 'Age range']//following-sibling::dd[1]") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature) ifelse(length(feature) == 0, NA, feature))
  
  return(set_age)
}

setNumber = sapply(setLinks, FUN = getSetNumber, USE.NAMES = FALSE)
themeGroup = sapply(setLinks, FUN = getThemeGroup, USE.NAMES = FALSE)
theme = sapply(setLinks, FUN = getTheme, USE.NAMES = FALSE)
subtheme = sapply(setLinks, FUN = getSubtheme, USE.NAMES = FALSE)
year = sapply(setLinks, FUN = getYear, USE.NAMES = FALSE)
age = sapply(setLinks, FUN = getAge, USE.NAMES = FALSE)

test = data.frame(setNumber, setName, age, rating, themeGroup, theme, subtheme, pieces, minifigs, RRP, PPP, year, packaging, availability, instructions, setType, stringsAsFactors = FALSE)
head(test)
