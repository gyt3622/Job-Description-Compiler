library(shiny)
library(DT)
library(rvest)
library(tidyverse)
library(jsonlite)

searchJob <- function(job, n){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Crawling:", value = 0)
  count <- ceiling(n/15)
  job_companies <- c()
  job_titles <- c()
  job_urls <- c()
  job_descs <- c()
  for(i in 0:(count-1)){
    progress$inc((i)/count, detail = paste0("https://www.indeed.com/jobs?q=", URLencode(job), "&start=", i*15))
    print(paste0("https://www.indeed.com/jobs?q=", job, "&start=", i*15))
    page = read_html(paste0("https://www.indeed.com/jobs?q=", URLencode(job), "&start=", i*15))
    jobcards <- html_node(page, "#mosaic-provider-jobcards")
    job_links <- html_nodes(jobcards, 'a[id^="job"]')
    titles <- rep(NA, length(job_links))
    names <-  rep(NA, length(job_links))
    urls <-  rep(NA, length(job_links))
    descs  <- rep(NA, length(job_links))
    for(k in 1:length(job_links)){
      link <- job_links[k]
      h2 <- html_node(link, "h2.jobTitle")
      spans <- html_nodes(h2, "span")
      titles[k] <- html_text(spans[length(spans)])
      names[k] <- link %>% html_node("span.companyName") %>% html_text()
      urls[k] <- paste0("https://www.indeed.com", html_attr(link, "href"))
    }
    for(j in 1:length(urls)){
      page2 <- read_html(urls[j])
      desc <- page2 %>% html_node("div#jobDescriptionText") %>% html_text()
      descs[j] <- desc
    }
    job_companies <- c(job_companies,  names)
    job_titles <- c(job_titles, titles)
    job_urls <- c(job_urls, urls)
    job_descs <- c(job_descs, descs)
  }
  dat <- data.frame(
    "title"=1:length(job_titles),          
    "JobTitle"=job_titles,
    "description"=job_descs,
    "CompanyNames"=job_companies,
    "URLS"=job_urls)
  dat
}
