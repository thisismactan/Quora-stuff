#### LIBRARIES ####

## Data manipulation
library(tidyverse)
library(Rfast)

## Scraping
library(xml2)
library(rvest)
library(httr)

#### CUSTOM FUNCTIONS ####
getURLs <- function(user_id, sorted = FALSE, topic = NULL) {
  # Get the URLs of a Quora user's answers (up to a point). If sorted = FALSE (default), gets the most recent
  # answers. If sorted = TRUE, then scrapes top most viewed answeres. If topic is not NULL, then it will get
  # only the answers in that topic.
  #
  # Note that user_id should be the person's Quora user ID (names separated by spaces, with number at the end 
  # if applicable). The "topic" argument should follow the same rule.
  require(dplyr)
  require(xml2)
  require(rvest)
  
  # Put together the user's profile URL
  if(sorted) {
    if(!is.null(topic)) {
      profile_url <- paste0("https://quora.com/profile/", user_id, "/answers/", topic, "?sort=views")
    } else {
      profile_url <- paste0("https://quora.com/profile/", user_id, "/answers?sort=views")
    }
  } else {
    if(!is.null(topic)) {
      profile_url <- paste0("https://quora.com/profile/", user_id, "/answers/", topic)
    } else {
      profile_url <- paste0("https://quora.com/profile/", user_id, "/answers")
    }
  }
  
  # Grab the page content
  profile_content <- xml2::read_html(profile_url)
  
  # Find all the hrefs on the page and grab only the ones that are to answers
  profile_urls <- rvest::html_nodes(profile_content, "a") %>%
    rvest::html_attr("href")
  
  answer_urls <- paste0("https://quora.com", grep("/answer/", profile_urls, value = TRUE)) %>%
    unique()
  
  return(answer_urls)
}

scrapeAnswers <- function(urls) {
  # Given a character vector of URLs (conveniently created by the getURLs() function), returns a list with
  # the text of the answers in those URLs.
  require(dplyr)
  require(xml2)
  require(rvest)
  
  # Initialize list
  answer_list <- vector("list", length(urls))
  
  # Loop through URLs
  for(i in 1:length(answer_list)) {
    # Grab the page content
    answer_list[[i]] <- xml2::read_html(urls[i]) %>%
      ## Then get the text
      rvest::html_nodes(xpath = '//*[@class="ui_qtext_expanded"]') %>%
      rvest::html_text()
    
  }
  
  return(answer_list)
}
