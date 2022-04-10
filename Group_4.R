library(magrittr)
library(rvest) 
library(stringr)
library(dplyr)
library(xml2)
library(xlsx)

df_art <- data.frame()
site <- "https://bmcmedgenet.biomedcentral.com/articles"

ReadByYear <- function(Input_year){
  
  # lasted year showing on the website is year 2020 and volume max number is 21
  maxvolume <- 21
  
  # oldest year showing on the website is year 2000
  volume <- Input_year-1999
  sitepage <- paste0(site,"?query=&volume=",volume,"&searchType=&tab=keyword")
  sitepage <- read_html(sitepage)
  artlist <- html_nodes(sitepage,".c-pagination__item") %>% html_nodes("a") %>% html_attr("href")
  
  # get the year of input and years after
  while (volume <= maxvolume) {
    
    for(i in seq_along(artlist)){
      pagelink <- 0
      pagelink <- paste0("https://bmcmedgenet.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&volume=",volume,"&page=",i)
      pagelink <- read_html(pagelink)
      artlinks <- html_nodes(pagelink,".c-listing__title")%>%html_nodes("a")%>%html_attr("href")
      links <- artlinks[grep("/articles/",artlinks)]
    
      for(j in seq_along(links)){
        
        # page of each article
        artpage <- paste0("https://bmcmedgenet.biomedcentral.com",links[j])
        
        # get the titles of each article, always single value
        title_info <- read_html(artpage) %>% html_nodes(".c-article-title") %>% html_text()
        
        # get the authors of each article, combine together if the author's more than one, remove irrelevant space and symbols like '\n' or '$'
        author_info <- read_html(artpage) %>% html_nodes(".c-article-author-list__item") %>% html_text()
        author_info <- gsub("([0-9]|\n).*", '', author_info)
        author_info <- paste(author_info, collapse="\t")
        
        # get the affiliations of each article, combine together if it's more than one and use \t to separate
        affiliations_info <- read_html(artpage) %>% html_nodes(".c-article-author-affiliation__address") %>% html_text()
        affiliations_info <-paste(affiliations_info, collapse="\t")
        
        # get the coauthor of each article, combine together if it's more than one and use \t to separate
        coauthor_info <- read_html(artpage) %>% html_nodes("#corresponding-author-list") %>% html_nodes('a') %>% html_text()      
        coauthor_info <- paste(coauthor_info, collapse = "\t")
        
        # get the emails of each article, combine together if it's more than one and use \t to separate
        Email_info <- read_html(artpage) %>% html_nodes("#corresponding-author-list") %>% html_nodes('a') %>% html_attr('href')
        Email_info <- paste(Email_info, collapse = "\t")
        
        # get the pubilshed date of each article, always single value
        date_info <- read_html(artpage) %>% html_nodes(".c-article-identifiers__item") %>% html_nodes('time') %>% html_text()
        
        # get the abstract of each article, always single value
        abstract_info <- read_html(artpage) %>% html_nodes("#Abs1-section") %>% html_text()
      
        # get the keywords of each article, combine together if it's more than one, remove irrelevant space and symbols like '\n'
        keywords_info <- read_html(artpage) %>% html_nodes(".c-article-subject-list__subject") %>% html_text()
        keywords_info <- gsub(" |\n", "", keywords_info)
        keywords_info <- paste(keywords_info, collapse = "\t")
      
        # get the full paper of each article, combine together each part, remove irrelevant space and '\n'
        fullpaper_info <- read_html(artpage) %>% html_nodes(".c-article-section__content") %>% html_text()
        fullpaper_info <- gsub(" |\n", "", fullpaper_info)
        fullpaper_info <- paste(fullpaper_info, collapse="\t")
      
        # bind every attributes together and pull data into a dataframe
        title_name <- bind_cols(title_info, author_info, affiliations_info, coauthor_info, Email_info, date_info, abstract_info, keywords_info, fullpaper_info)
        colnames(title_name) <- c('Title', 'Authors','Author Affiliations','Correspondence Author','CoAuthor Email','Publish Date','Abstract','Keywords','Full Paper')
        df_art <- rbind( df_art, title_name )
        
      }
    }
    volume <- volume + 1
  }
  # fill NA, after removing space and '\n' for some attribute, cell maybe equal to "", not NA, fill with 'NO' per project request
  df_art[is.na(df_art)] <- 'NO'
  df_art[df_art == ""] <- 'NO'
  return (df_art)
}

# try to run year 2019, result will show everything in and after year 2019
Temp <- ReadByYear(2019)
write.xlsx(Temp,"Summary.xlsx")
