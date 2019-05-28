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
library(NLP)
library(naniar)
library(rapport)
library(sjmisc)
library(ggplot2)
library(reticulate)
library(RhpcBLASctl)
##import all libraries necessary for this script -- it's a lot!

# Setup parallel computation - use all cores on our computer.
num_cores = RhpcBLASctl::get_num_cores()
num_cores
options(mc.cores = num_cores)
getOption("mc.cores")


setwd("/Users/shuvomsadhuka/Documents/GitHub/NBAVA")
csvreader <- function(x)
{
  nba <- read.csv(x)
  nba[1,] <- toString(nba[1,])
  
  example_row <- as.vector(unlist(strsplit(as.character(nba[1,]), ",")))
  n = length(example_row)
  
  as.matrix <- matrix(ncol = n)
  for (i in 1:61){
    #as.strings[i] <- toString(nba[i,])
    total <-  as.vector(unlist(strsplit(as.character(nba[i,]), ",")))
    
    total[total==""]  <- NA 
    as.matrix <- rbind(as.matrix, total)
  }
  as.matrix <- as.matrix[-1,] #delete the first row of the table -- kind of buggy
  colnames(as.matrix) <- as.character(unlist(as.matrix[1,]))
  year <- substr(x, 1, 4)
  as.matrix <- as.matrix[-1,] #delete the row we just made the header row
  df <- data.frame(as.matrix)
  df$year <- rep(year, length(df[,1])) #store the year of the draft in a separate column
  df_lottery <- df[1:14, ]
  return(df_lottery)
}

fileList <- list.files(path="/Users/shuvomsadhuka/Documents/GitHub/NBAVA", pattern=".csv")
all_drafts <- matrix(ncol = 22)
df_bind <- data.frame()
for(file in fileList){
  df <- csvreader(file)
  df_bind <- rbind(df_bind, df)
}
rownames(df_bind) <- c()
df_bind
typeof(df_bind$Rk)

new <- df_bind[df_bind$Rk == 4,]
vorp.col = grep("VORP", colnames(new)); bpm.col = grep("BPM", colnames(new));
ws.col = grep("WS.48", colnames(new))
cols <- c(vorp.col, bpm.col, ws.col)

length(new$VORP)

new[cols] <- unname(new[cols])
new$VORP <- as.numeric(as.character(new$VORP))
new$WS.48 <- as.numeric(as.character(new$WS.48))
new$BPM <- as.numeric(as.character(new$BPM))

avg = mean(new$VORP)
var = var(new$VORP)

alpha = avg^2/var
beta = avg/var
mean(new$BPM)



#colnames(mydata) <- paste0("position_", 1:3)
scalar1 <- function(x) {x / sqrt(sum(x^2))}


draft_position <- function(x){
  pos <- df_bind[df_bind$Rk == x,]
  vorp.col = grep("VORP", colnames(pos)); bpm.col = grep("BPM", colnames(pos)); 
  ws.col = grep("WS.48", colnames(pos))
  cols <- c(vorp.col, bpm.col, ws.col)
  
  pos[cols] <- unname(pos[cols])
  pos$VORP <- as.numeric(as.character(pos$VORP))
  pos$WS.48 <- as.numeric(as.character(pos$WS.48))
  pos$BPM <- as.numeric(as.character(pos$BPM))
  
  avg_vorp = mean(pos$VORP, na.rm=TRUE); avg_ws = mean(pos$WS.48, na.rm=TRUE); avg_bpm = mean(pos$BPM, na.rm=TRUE)
  var_vorp = var(pos$VORP, na.rm=TRUE); var_ws = var(pos$WS.48, na.rm=TRUE); var_bpm = var(pos$BPM, na.rm=TRUE)
  
  alpha_vorp = avg_vorp^2/var_vorp; alpha_ws = avg_ws^2/var_ws; alpha_bpm = avg_bpm^2/var_bpm; 
  beta_vorp = avg/var; beta_ws =avg_ws/var_ws; beta_bpm = avg_bpm/var_bpm; 
  
  pos_vector <- c(avg_vorp, var_vorp, alpha_vorp, beta_vorp,
                  avg_ws, var_ws, alpha_ws, beta_ws,
                  avg_bpm, var_bpm, alpha_bpm, beta_bpm)
  
  return(pos_vector)
}
positions <- c(1:14)
parameters <- sapply(positions, draft_position)
rownames(parameters) <- c("avg_vorp", "var_vorp", "alpha_vorp", "beta_vorp",
                          "avg_ws", "var_ws", "alpha_ws", "beta_ws",
                          "avg_bpm", "var_bpm", "alpha_bpm", "beta_bpm")
parameters


lottery_teams <- cbind(c(14,14,14,12.5,10.5,9,6,6,6,3,2,1,1,1), 
                       c(13.4,13.4,13.4,12.2,10.5,9.2,6.3,6.3,6.3,3.3,2.2,1.1,1.1,1.1),
                       c(12.7,12.7,12.7,11.9,10.5,9.4,6.7,6.7,6.7,3.6,2.4,1.2,1.2,1.2),
                       c(11.9,11.9,11.9,11.4,10.5,9.6,7.2,7.2,7.2,4.0,2.8,1.4,1.4,1.4),
                       c(47.9,27.8,14.8,7.2,2.2,0,0,0,0,0,0,0,0,0),
                       c(0,20.1,26.0,25.7,19.6,8.6,0,0,0,0,0,0,0,0),
                       c(0,0,7.1,16.8,26.7,29.6,19.7,0,0,0,0,0,0,0),
                       c(0,0,0,2.2,8.8,20.6,37.2,31.2,0,0,0,0,0,0),
                       c(0,0,0,0,0.6,3.8,15.1,34.1,46.4,0,0,0,0,0),
                       c(0,0,0,0,0,0.2,1.6,8.0,24.3,65.9,0,0,0,0),
                       c(0,0,0,0,0,0,0.1,0.5,2.9,18.9,77.6,0,0,0),
                       c(0,0,0,0,0,0,0,0.1,0.1,1.2,12.6,86.1,0,0),
                       c(0,0,0,0,0,0,0,0,0.1,0.1,0.4,9,90.6,0),
                       c(0,0,0,0,0,0,0,0,0,0.1,0.1,0.2,4.6,95.2))

lottery_teams <- lottery_teams/100

teams <- c(1:14)

simulate <- function(){
  picks <- c()
  for(i in 1:14) {
    if (i == 1){
      pick.i <- sample(teams,1,replace = FALSE, prob = scalar1(lottery_teams[,1]))
    }
    else if(i < 4){
      pick.i <- sample(teams[-picks],1,replace = FALSE, prob = scalar1(lottery_teams[,i][-picks])) 
    }
    else{
     remaining <- teams[-picks]
     pick.i <- remaining[1]
    }
    
    picks <- c(picks, pick.i)
  }
  return(picks)
}

drafts <- replicate(10000, simulate()) #each column is a simulation of a draft

mean_var_pos <- function(x){
  positions <- vector()
  for(i in 1:10000){
    pos <- which(drafts[,i] == x)
    positions <- c(positions, pos)
  }
  return(c(mean(positions), var(positions)))
}

for(i in 1:14){
  print(mean_var_pos(i))
}

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

