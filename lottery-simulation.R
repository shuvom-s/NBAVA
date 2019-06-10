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
library(reshape2)
library(manipulate)
##import all libraries necessary for this script -- it's a lot!

# Setup parallel computation - use all cores on our computer.
num_cores = RhpcBLASctl::get_num_cores()
num_cores
options(mc.cores = num_cores)
getOption("mc.cores")



setwd("/Users/shuvomsadhuka/Documents/GitHub/NBAVA/")
samples = 100000

#let's read the CSV into our workspace
csvreader <- function(x)
{
  nba <- read.csv(x)
  nba[1,] <- toString(nba[1,]) #make the first row a header - this is just data cleaning
  
  header_row <- as.vector(unlist(strsplit(as.character(nba[1,]), ","))) #more data cleaning - make each
  #one a header
  n = length(header_row)
  
  as.matrix <- matrix(ncol = n)
  for (i in 1:61){
    total <-  as.vector(unlist(strsplit(as.character(nba[i,]), ","))) #there are 60 picks per draft plus the
    #header column
    
    total[total==""]  <- NA #fill any missing values with NA
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

fileList <- list.files(path="/Users/shuvomsadhuka/Documents/GitHub/NBAVA/drafts/", pattern=".csv") #read all the csv
#files in our working directory
all_drafts <- matrix(ncol = 22)

df_bind <- data.frame()

#iterate over all files in fileList, add to dataframe of all drafts
for(file in fileList){
  setwd("/Users/shuvomsadhuka/Documents/GitHub/NBAVA/drafts/")
  df <- csvreader(file)
  df_bind <- rbind(df_bind, df)
}
rownames(df_bind) <- c()
df_bind <- subset(df_bind, select=-c(WS.48))
typeof(df_bind$Rk)


# new <- df_bind[df_bind$Rk == 4,]
# vorp.col = grep("VORP", colnames(new)); bpm.col = grep("BPM", colnames(new));
# ws.col = grep("WS.48", colnames(new))
# cols <- c(vorp.col, bpm.col, ws.col)
# 
# length(new$VORP)
# 
# new[cols] <- unname(new[cols])
# new$VORP <- as.numeric(as.character(new$VORP))
# new$WS.48 <- as.numeric(as.character(new$WS.48))
# new$BPM <- as.numeric(as.character(new$BPM))
# 
# avg = mean(new$VORP)
# var = var(new$VORP)
# 
# alpha = avg^2/var
# beta = avg/var
# mean(new$BPM)


#function gives all information necessary for a given draft position
draft_position <- function(x){
  pos <- df_bind[df_bind$Rk == x,]
  
  #isolate vorp, ws, bpm
  vorp.col = grep("VORP", colnames(pos)); bpm.col = grep("BPM", colnames(pos)); 
  ws.col = grep("WS", colnames(pos))
  cols <- c(vorp.col, bpm.col, ws.col)
  
  #clean the data, extract each statistic
  pos[cols] <- unname(pos[cols])
  pos$VORP <- as.numeric(as.character(pos$VORP))
  pos$WS <- as.numeric(as.character(pos$WS))
  pos$BPM <- as.numeric(as.character(pos$BPM))
  
  #some essential statistics, such as the avg vorp for a given draft position in the past 22 years
  avg_vorp = mean(pos$VORP, na.rm=TRUE); avg_ws = mean(pos$WS, na.rm=TRUE); avg_bpm = mean(pos$BPM, na.rm=TRUE)
  var_vorp = var(pos$VORP, na.rm=TRUE); var_ws = var(pos$WS, na.rm=TRUE); var_bpm = var(pos$BPM, na.rm=TRUE)
  
  alpha_vorp = avg_vorp^2/var_vorp; alpha_ws = avg_ws^2/var_ws; alpha_bpm = avg_bpm^2/var_bpm; 
  beta_vorp = avg_vorp/var_vorp; beta_ws =avg_ws/var_ws; beta_bpm = avg_bpm/var_bpm; 
  
  #store all the essential statistics in one vector and return
  pos_vector <- c(avg_vorp, var_vorp, alpha_vorp, beta_vorp,
                  avg_ws, var_ws, alpha_ws, beta_ws,
                  avg_bpm, var_bpm, alpha_bpm, beta_bpm)
  
  return(pos_vector)
}

print(parameters)

#example skewness
pos_13 <- df_bind[df_bind$Rk == 13,]
pos_13$WS <- as.double(pos_13$WS)
ggplot(pos_13, aes(x = WS)) + stat_bin(aes(y=..count../sum(..count..)), binwidth = 80) +
  labs(x = "Career Win Shares") + labs(title = "Career Win Shares Density for 8th Overall Pick") +
  labs(y = "Density")
pos_13


#get the information in draft_positions(x) for each lottery position x, rename the rows
positions <- c(1:14)

parameters <- sapply(positions, draft_position)


rownames(parameters) <- c("avg_vorp", "var_vorp", "alpha_vorp", "beta_vorp",
                          "avg_ws", "var_ws", "alpha_ws", "beta_ws",
                          "avg_bpm", "var_bpm", "alpha_bpm", "beta_bpm")

#this matrix gives the probabilities of each team getting any given lottery position
lottery_teams_new <- cbind(c(14,14,14,12.5,10.5,9,6,6,6,3,2,1,1,1), 
                       c(13.4,13.4,13.4,12.2,10.5,9.2,6.3,6.3,6.3,3.3,2.2,1.1,1.1,1.1),
                       c(12.7,12.7,12.7,11.9,10.5,9.4,6.7,6.7,6.7,3.6,2.4,1.2,1.2,1.2),
                       c(11.9,11.9,11.9,11.4,10.5,9.6,7.2,7.2,7.2,4.0,2.8,1.4,1.4,1.4))

lottery_teams_old <- cbind(c(25,19.9,13.8,13.7,8.8,5.3,5.3,2.3,2.2,1.1,0.8,0.7,0.6,0.5), 
                           c(21.48,18.78,14.25,14.16,9.64,6.02,6.02,2.69,2.57,1.3,0.95,0.83,0.71,0.59),
                           c(17.72,17.07,14.54,14.48,10.65,6.96,6.96,3.21,3.08,1.57,1.15,1.01,0.87,0.72))

lottery_teams_new <- lottery_teams_new/100
lottery_teams_old <- lottery_teams_old/100

teams <- c(1:14)

#this is to normalize vectors in te function below
scalar1 <- function(x) {x / sqrt(sum(x^2))}

#let's simulate the lottery
simulate <- function(time){
  picks <- c()
  
  for(i in 1:14) {
    if(time == "old"){
      if (i == 1){
        #the first pick will be made according to the probabilities of the first column of our matrix
        pick.i <- sample(teams,1,replace = FALSE, prob = scalar1(lottery_teams_old[,1]))
      }
      else if(i < 4){
        #the next two picks will be made by omitting the first/second team and rescaling the probabilities
        pick.i <- sample(teams[-picks],1,replace = FALSE, prob = scalar1(lottery_teams_old[,i][-picks])) 
      }
      else{
        #the remaining teams will pick in reverse order of standigns
        remaining <- teams[-picks]
        pick.i <- remaining[1]
      }
    }
    
    else{
      if (i == 1){
        #the first pick will be made according to the probabilities of the first column of our matrix
        pick.i <- sample(teams,1,replace = FALSE, prob = scalar1(lottery_teams_new[,1]))
      }
      else if(i < 5){
        #the next two picks will be made by omitting the first/second team and rescaling the probabilities
        pick.i <- sample(teams[-picks],1,replace = FALSE, prob = scalar1(lottery_teams_new[,i][-picks])) 
      }
      else{
        #the remaining teams will pick in reverse order of standigns
        remaining <- teams[-picks]
        pick.i <- remaining[1]
      }
    }
    
    #store all these picks in a vector
    picks <- c(picks, pick.i)
    
  }
  return(picks)
}

drafts_old <- replicate(samples, simulate("old")) #each column is a simulation of a draft order
drafts_new <- replicate(samples, simulate("new"))

covariance_matrix <- function(x){
  cov_mat_picks <- matrix(,nrow=27)
  for (i in 1:14){
    pos <- df_bind[df_bind$Rk == i,]
    
    #isolate statistic of choice
    x.col = grep(as.character(x), colnames(pos))
    
    #clean the data
    pos[x.col] <- unname(pos[x.col])
    picks <- as.numeric(as.character(unlist(pos[x.col])))

    #clean the data, extract each statistic
    cov_mat_picks <- cbind(cov_mat_picks, picks)
    cov_mat_picks
  }
  cov_mat_picks <- cov_mat_picks[,-1]
  return(cov_mat_picks)
}
#the means and variances for each parameter
mvr_cov_vorp <- covariance_matrix("VORP")
mvr_cov_bpm <- covariance_matrix("BPM")
mvr_cov_ws <- covariance_matrix("WS")

#fill the NaNs with 0
mvr_cov_vorp[is.na(mvr_cov_vorp)] <- 0
mvr_cov_bpm[is.na(mvr_cov_bpm)] <- 0
mvr_cov_ws[is.na(mvr_cov_ws)] <- 0

#convert to cov matrices
mvr_cov_vorp <- cov(mvr_cov_vorp)
mvr_cov_bpm <- cov(mvr_cov_bpm)
mvr_cov_ws <- cov(mvr_cov_ws)

mvr_mean_vorp = parameters[1,]
mvr_mean_bpm = parameters[9,]
mvr_mean_ws = parameters[5,]


simulated_vorp_mvn <- mvrnorm(n = samples, mu = mvr_mean_vorp, Sigma = mvr_cov_vorp, tol = 1e-6)
simulated_vorp_mvn <- t(simulated_vorp_mvn)

simulated_ws_mvn <- mvrnorm(n = samples, mu = mvr_mean_ws, Sigma = mvr_cov_ws, tol = 1e-6)
simulated_ws_mvn <- t(simulated_ws_mvn)

simulated_bpm_mvn <- mvrnorm(n = samples, mu = mvr_mean_bpm, Sigma = mvr_cov_bpm, tol = 1e-6)
simulated_bpm_mvn <- t(simulated_bpm_mvn)

simulated_vorp_gamma <- rgamma(n = samples*14, shape = parameters[3,], rate = parameters[4,])
simulated_vorp_gamma_mat <- matrix(simulated_vorp_gamma, nrow=14)

simulated_ws_gamma <- rgamma(n = samples*14, shape = parameters[7,], rate = parameters[8,])
simulated_ws_gamma_mat <- matrix(simulated_ws_gamma, nrow=14)

old_ws <- matrix(ncol = 14)
new_ws <- matrix(nrow = )
picks_plt <- data.frame(nrow = 14)
picks_exp <- vector()
picks_sd <- vector()

for(i in 1:14){
  temp1 <- paste("draft_picks_mvn_ws_old", i, sep = "")
  temp2 <- paste("draft_picks_gamma_ws_old", i, sep = "")
  temp3 <- paste("draft_picks_mvn_ws_new", i, sep = "")
  temp4 <- paste("draft_picks_gamma_ws_new", i, sep = "")
  temp5 <- paste("draft_picks_mvn_vorp_old", i, sep = "")
  temp6 <- paste("draft_picks_gamma_vorp_old", i, sep = "")
  temp7 <- paste("draft_picks_mvn_vorp_new", i, sep = "")
  temp8 <- paste("draft_picks_gamma_vorp_new", i, sep = "")
  
  
  all_old <- which(drafts_old == i, arr.ind = T)
  all_new <- which(drafts_new == i, arr.ind = T)
  
  get_ws_mvn_old <- simulated_ws_mvn[all_old]
  get_ws_mvn_new <- simulated_ws_mvn[all_new]
  get_ws_gamma_old <- simulated_ws_gamma_mat[all_old]
  get_ws_gamma_new <- simulated_ws_gamma_mat[all_new]
  
  get_vorp_mvn_old <- simulated_vorp_mvn[all_old]
  get_vorp_mvn_new <- simulated_vorp_mvn[all_new]
  get_vorp_gamma_old <- simulated_vorp_gamma_mat[all_old]
  get_vorp_gamma_new <- simulated_vorp_gamma_mat[all_new]
  
  assign(temp1, get_ws_mvn_old)
  assign(temp2, get_ws_gamma_old)
  
  print(paste(mean(get_ws_gamma_old), i, sep = " "))
  print(paste(sd(get_ws_gamma_old), i, sep = " "))
  
  print("gamma_new_ws")
  
  print(paste(mean(get_ws_gamma_new), i, sep = " "))
  print(paste(sd(get_ws_gamma_new), i, sep = " "))
  
  print("gamma_old_vorp")
  
  print(paste(mean(get_vorp_gamma_old), i, sep = " "))
  print(paste(sd(get_vorp_gamma_old), i, sep = " "))
  
  print("gamma_new_vorp")
  
  print(paste(mean(get_vorp_gamma_new), i, sep = " "))
  print(paste(sd(get_vorp_gamma_new), i, sep = " "))
  
  get_ws_gamma_old <- data.frame(simulated_ws_gamma_mat[all_old])
  get_ws_gamma_new <- data.frame(simulated_ws_gamma_mat[all_new])
  names(get_ws_gamma_old)[1] <- "drafts"
  names(get_ws_gamma_new)[1] <- "drafts"
  get_vorp_gamma_new
  
  get_ws_gamma_old$id = 'Old'
  get_ws_gamma_new$id = 'New'
  
  old_v_new <- data.frame(rbind(get_ws_gamma_old, get_ws_gamma_new))
  
  print(ggplot(old_v_new, aes(drafts, fill = id)) + 
          geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + 
          labs(title = paste0("Old vs New Draft WS Values for Team with Seed", i)) +
          labs(x = "Simulated Win Shares"))
}

old <- c(62.94, 53.87, 50.03, 48.12, 47.12, 47.03, 46.06, 48.50, 52.05, 47.34, 37.54, 24.90, 38.18, 20.30)
new <- c(64.11, 63.07, 59.85, 54.87, 47.13, 38.64, 44.31, 39.51, 50.08, 47.31, 36.88, 22.76, 38.07, 18.74)

picks <- c(1:14)

df_picks <- data.frame(picks = picks, Old = old, New = new)
df_picks
ggplot() + 
  geom_line(data = df_picks, aes(x = picks, y = Old, color = "red"), size = 1) +
  geom_line(data = df_picks, aes(x = picks, y = New, color = "blue"), size = 1) +
  xlab('Pick') +
  ylab('Expected Career Win Shares') + scale_color_discrete(name = "Draft System", labels = c("Old", "New"))


cor(as.numeric(df_bind$VORP), as.numeric(df_bind$WS), use = "complete.obs")





mean_var_pos <- function(x){
  positions <- vector()
  for(i in 1:samples){
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

