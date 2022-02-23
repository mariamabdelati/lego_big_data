library(tidyverse)
library(dplyr)
library(rvest)

# get system's year to keep data updated
# current_year = lubridate::year(Sys.Date())
# page = 1

#======================================nested pages attribute retrieval functions=============================================
#nested pages attribute retrieval functions
getSetNumber = function(setLink) {
  setPage = read_html(setLink)
  details = setPage %>%
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_number = sapply(details, function(feature) {
    feature %>%
      html_nodes(xpath = "./dl/dt[text() = 'Set number']//following-sibling::dd[1]") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature)
    ifelse(length(feature) == 0, NA, feature))
  
  return(set_number)
}

getThemeGroup = function(setLink) {
  setPage = read_html(setLink)
  details = setPage %>%
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_themeGroup = sapply(details, function(feature) {
    feature %>%
      html_nodes(xpath = "./dl/dt[text() = 'Theme group']//following-sibling::dd[1]") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature)
    ifelse(length(feature) == 0, NA, feature))
  
  return(set_themeGroup)
}

getTheme = function(setLink) {
  setPage = read_html(setLink)
  details = setPage %>%
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_theme = sapply(details, function(feature) {
    feature %>%
      html_nodes(xpath = "./dl/dt[text() = 'Theme']//following-sibling::dd[1]/a/text()") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature)
    ifelse(length(feature) == 0, NA, feature))
  
  return(set_theme)
}

getSubtheme = function(setLink) {
  setPage = read_html(setLink)
  details = setPage %>%
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_subtheme = sapply(details, function(feature) {
    feature %>%
      html_nodes(xpath = "./dl/dt[text() = 'Subtheme']//following-sibling::dd[1]/a/text()") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature)
    ifelse(length(feature) == 0, NA, feature))
  
  return(set_subtheme)
}

getYear = function(setLink) {
  setPage = read_html(setLink)
  details = setPage %>%
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_year = sapply(details, function(feature) {
    feature %>%
      html_nodes(xpath = "./dl/dt[text() = 'Year released']//following-sibling::dd[1]/a/text()") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature)
    ifelse(length(feature) == 0, NA, feature))
  
  return(set_year)
}

getAge = function(setLink) {
  setPage = read_html(setLink)
  details = setPage %>%
    html_nodes(xpath = "//body/div[@class='outerwrap']/div[@class='content']/aside[@role='complementary']/section[2]/div[1]")
  
  set_age = sapply(details, function(feature) {
    feature %>%
      html_nodes(xpath = "./dl/dt[text() = 'Age range']//following-sibling::dd[1]") %>%
      html_text() %>%
      str_trim() %>% as.character()
  }) %>% sapply(function (feature)
    ifelse(length(feature) == 0, NA, feature))
  
  return(set_age)
}
#===================================================================================

#define dataframe
legoDataFrame = data.frame()

for (releaseYear in seq(from = 1953, to = 2022, by = 1)) {
  
  # define url to scrape
  url = glue::glue("https://brickset.com/sets/year-{releaseYear}/filter-Released/page-1")
  
  # obtain html document from url
  lego_data = read_html(url)
  
  # multi-page get last page
  # first get the href of the last page
  lastPageRef = lego_data %>%
    html_nodes(".sets+ .pagination .last a") %>%
    html_attr("href")
  
  if (is_empty(lastPageRef)){
    lastPage = 1
  } else {
  # take the href tag and extract the last page from that
  lastPage = stringr::word(lastPageRef,-1, sep = 'page-') %>% as.numeric()
  }
  
  # for loop is used to loop through the pages
  for (pageResult in seq(from = 1, to = lastPage, by = 1)) {
    #22 year - 2
    pagination_link = glue::glue(
      "https://brickset.com/sets/year-{releaseYear}/filter-Released/page-{pageResult}"
    )
    
    legoData = read_html(pagination_link)
    
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
    
    rating = sapply(rating, function(feature) {
      feature %>%
        html_nodes(xpath = ".//div[@class='rating']/span/text()") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>%
      sapply(function(feature)
        ifelse(length(feature) == 0, NA, feature))
    
    features = legoData %>%
      html_nodes(".rating+ .col")
    pieces = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'Pieces']//following-sibling::dd[1]") %>%
        html_text()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    minifigs = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'Minifigs']//following-sibling::dd[1]") %>%
        html_text()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    RRP = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'RRP']//following-sibling::dd[1]/text()") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    PPP = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'PPP']//following-sibling::dd[1]") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    packaging = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'Packaging']//following-sibling::dd[1]") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    availability = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'Availability']//following-sibling::dd[1]") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    instructions = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'Instructions']//following-sibling::dd[1]") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    setType = sapply(features, function(feature) {
      feature %>%
        html_nodes(xpath = "./dl/dt[text() = 'Set type']//following-sibling::dd[1]") %>%
        html_text() %>%
        str_trim() %>% as.character()
    }) %>% sapply(function (feature)
      ifelse(length(feature) == 0, NA, feature))
    
    setNumber = sapply(setLinks, FUN = getSetNumber, USE.NAMES = FALSE)
    themeGroup = sapply(setLinks, FUN = getThemeGroup, USE.NAMES = FALSE)
    theme = sapply(setLinks, FUN = getTheme, USE.NAMES = FALSE)
    subtheme = sapply(setLinks, FUN = getSubtheme, USE.NAMES = FALSE)
    yearReleased = sapply(setLinks, FUN = getYear, USE.NAMES = FALSE)
    ageGroup = sapply(setLinks, FUN = getAge, USE.NAMES = FALSE)
    
    # rbind is used to append the obtained data after each iteration
    legoDataFrame = rbind(
      legoDataFrame,
      data.frame(
        setNumber,
        setName,
        ageGroup,
        rating,
        themeGroup,
        theme,
        subtheme,
        pieces,
        minifigs,
        RRP,
        PPP,
        yearReleased,
        packaging,
        availability,
        instructions,
        setType,
        stringsAsFactors = FALSE
      )
    )
    
    print(paste("Page:", pageResult))
    Sys.sleep(2)
    # head(legoDataFrame)
  }
  
  print(paste("Finished Year:", releaseYear))
  # Sys.sleep(2)
}

# write data to a LegoSetData.csv file
write.csv(legoDataFrame, file = "LegoSetsData.csv", row.names = FALSE)

# write.table(legoDataFrame, file = "LegoSetsData.csv", sep = ",", append = TRUE, row.names = FALSE, column.names = FALSE)
