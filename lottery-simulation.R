library(tidyverse)
library(MASS)
library(matrixStats)
library(XML)
library(dplyr)
library(xml2)
library(rvest)
library(RSelenium)
library(RCurl)
library(jsonlite)
setwd("/Users/shuvomsadhuka/Documents/GitHub/NBAVA")

csvreader <- function(x)
{
  nba <- read.csv(x)
  nba[1,] <- toString(nba[1,])
  
  example_row <- as.vector(unlist(strsplit(as.character(nba[1,]), ",")))
  n = length(example_row)
  
  as.matrix <- matrix(ncol = n)
  for (i in 1:61){
    as.strings[i] <- toString(nba[i,])
    total <-  as.vector(unlist(strsplit(as.character(nba[i,]), ",")))
    as.matrix <- rbind(as.matrix, total)
  }
  as.matrix <- as.matrix[-1,] #delete the first row of the table -- kind of buggy
  colnames(as.matrix) <- as.character(unlist(as.matrix[1,]))
  as.matrix <- as.matrix[-1,] #delete the row we just made the header row
  return(as.matrix)
}

fileList <- list.files(path="/Users/shuvomsadhuka/Documents/GitHub/NBAVA", pattern=".csv")

for(file in fileList){
  csvreader(file)
}

View(csvreader('2011-Draft.csv'))
















total
nba_2018[9,]
shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
# lebron <- "https://www.basketball-reference.com/players/j/jamesle01.html"
# rD <- rsDriver()
# remDr <- rD[["client"]]
# remDr$navigate(lebron)
# lebron_webpage <- read_html(remDr$getPageSource()[[1]])
# lebron_table <- html_table(lebron_webpage, fill = TRUE)

#rtg = PPG + APG + RPG, pulled from https://www.82games.com/nbadraftpicks.htm
#draftpos <- data.frame(pos = c(1:14), rtg = c(27.0, 21.6, 23.8, 22.2, 21.1, ))
url <- "https://www.basketball-reference.com/draft/NBA_2011.html"

draft_2011 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="all_stats"]/div[2]') %>%
  html_table(fill = TRUE)

webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
tbls

tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE, header = NA)

unlisted <- unlist(tbls_ls)
View(unlisted)
new.matrix <- matrix(nrow = 63)
as.vector(unlisted[(0*30 + 1): (1 + 30)])
as.vector(unlisted[(1*30 + 1): (1* 32 + 30)])

for (i in 0:21){
  for (j in 1:60){
    column <- as.vector(unlisted[i*32 + 1: i*j + 30])
    new.matrix <- cbind(new.matrix, column)
  }
}
new.matrix

