# install.packages("param_set")
# install.packages("zoo")
# install.packages("tidytext")
# install.packages("tidyverse")
# install.packages("urltools")
# install.packages("tibble")
# install.packages("xlsx")
# install.packages("stringr")
# install.packages("sentimentr")
# install.packages("rvest")
# install.packages("plyr")
# install.packages("countrycode")
# install.packages("rJava")


library(sentimentr)
library(plyr)
library(rvest)
library(zoo)
library(tidytext)
library(tidyverse)
library(urltools)
library(countrycode)
library(tibble)
library(stringr)
library(sentimentr)
library(xlsx)



#############################################################################################
################################ YOUTUBE SCRAPPING ##########################################
#############################################################################################

#------------------------------------------------------------------------------------
#--------------------------Youtube VideoID scrapping----------------------------------------
#------------------------------------------------------------------------------------

#Here in this block of code search keywords are being set according to the years by which the search is to be performed

#Setting the working directory
setwd("D:\\DWBI Final Data")
data_files <- data.frame(matrix(ncol = 1, nrow = 36))
colNames <- c("Video")
colnames(data_files) <- colNames 
data_files$Video <- c("Lenovo%20mobile%20phones%202015","Lenovo%20mobile%20phones%202016","Lenovo%20mobile%20phones%202017","Lenovo%20mobile%20phones%202018","Apple%20mobile%20phones%202015",
                      "Apple%20mobile%20phones%202016",
                      "Apple%20mobile%20phones%202017",
                      "Apple%20mobile%20phones%202018",
                      "LG%20mobile%20phones%202015",
                      "LG%20mobile%20phones%202016",
                      "LG%20mobile%20phones%202017",
                      "LG%20mobile%20phones%202018",
                      "Samsung%20mobile%20phones%202015",
                      "Samsung%20mobile%20phones%202016",
                      "Samsung%20mobile%20phones%202017",
                      "Samsung%20mobile%20phones%202018","Oppo%20mobile%20phones%202015","Oppo%20mobile%20phones%202016","Oppo%20mobile%20phones%202017","Oppo%20mobile%20phones%202018","Alcatel%20mobile%20phones%202015","Alcatel%20mobile%20phones%202016","Alcatel%20mobile%20phones%202017","Alcatel%20mobile%20phones%202018","ZTE%20mobile%20phones%202015","ZTE%20mobile%20phones%202016","ZTE%20mobile%20phones%202017","ZTE%20mobile%20phones%202018","Huawei%20mobile%20phones%202015","Huawei%20mobile%20phones%202016","Huawei%20mobile%20phones%202017","Huawei%20mobile%20phones%202018","Xiaomi%20mobile%20phones%202015","Xiaomi%20mobile%20phones%202016",
                      "Xiaomi%20mobile%20phones%202017","Xiaomi%20mobile%20phones%202018")
data_files$Brand <- c("Lenovo","Apple","LG","Samsung","Oppo","Alcatel","ZTE","Huawei","Xiaomi")
write.csv(data_files, 'SearchKeywordData.csv', row.names=FALSE)


#Load the keywords for youtube search
searchKeywordData <- read.csv('D:\\DWBI Final Data\\SearchKeywordData.csv', stringsAsFactors = F)

for (i in 1:length(searchKeywordData$Video)){
  print(i)
  #Keywordsearch url is being updated with the keywords already defined in the previous block of code
  keywordSearchUrl <- "https://www.googleapis.com/youtube/v3/search?part=snippet%20&maxResults=50&q=Oppo%20mobile%20phones%202016%20&key=AIzaSyAS-uSQhftToHWhbVYh1u5mqjNOvTUrGJ8"
  keywordSearchUrl <- param_set(keywordSearchUrl, key = "q", value = searchKeywordData$Video[i] )
  print(keywordSearchUrl )
  
  
  init_video_results <- httr::content(httr::GET(keywordSearchUrl))
  videoId_data <- init_video_results$items
  
  if(length(videoId_data)!= 0){
    organize_video_data = function(){
      
      sub_data <- lapply(videoId_data, function(x) {
        
        data.frame(
          VideoID = x$id$videoId,
          stringsAsFactors=FALSE)
      })
    }
    
    VideoIdsample <- organize_video_data()
    L <- length(VideoIdsample)
    VideoIdsample <- data.frame(matrix(unlist(VideoIdsample), nrow=L, byrow=T))
    colnames(VideoIdsample) <- c("videoID")
    
    VideoIdsample$videoID <- as.character(VideoIdsample$videoID)
    VideoIdsample$Brand=searchKeywordData$Brand[i]
    VideoIdsample <- VideoIdsample[!VideoIdsample$Brand == 'Brand',]
    write.table(VideoIdsample, "myDF.csv", sep = ",", col.names = T, append = T,row.names = F)
    #Sys.sleep(1)
  }
}
video = read.csv("myDF.csv", stringsAsFactors = F)
str(video)
video = video[!video$Brand == 'Brand',]
write.csv(video, 'VideoId.csv', row.names=FALSE)

#------------------------------------------------------------
#-------------------------------------------------------------

#Here the comments extraction is being done by the used of video ids extracted earlier
videoIdData <- read.csv('D:\\DWBI Final Data\\VideoId.csv', stringsAsFactors = F)
str(videoIdData)

for (i in 1:length(videoIdData$videoID)){
  print(paste("The id is", i))
  
   commentSearchUrl <- "https://www.googleapis.com/youtube/v3/commentThreads?part=snippet%2C+replies&maxResults=100&textFormat=plainText&videoId=iTgmR4pcR9Q%20&fields=items%2CnextPageToken&key=AIzaSyAS-uSQhftToHWhbVYh1u5mqjNOvTUrGJ8"
  commentSearchUrl <- param_set(commentSearchUrl, key = "videoId", value = videoIdData$videoID[i] )
  print(commentSearchUrl)
  
  init_results <- httr::content(httr::GET(commentSearchUrl))
  data <- init_results$items
  
  if(length(data)!= 0){
    
    organize_data = function(){
      
      sub_data <- lapply(data, function(x) {
        data.frame(
          
          Comment = x$snippet$topLevelComment$snippet$textDisplay,
          Date = x$snippet$topLevelComment$snippet$publishedAt,
          stringsAsFactors=FALSE)
      })
    }
    
    sample <- organize_data()
    L <- length(sample)
    sample <- data.frame(matrix(unlist(sample), nrow=L, byrow=T))
    colnames(sample) <- c("Comment","Date")
    sample$Brand <- videoIdData$Brand[i]
    sample$Comment <- gsub("[^[:alnum:] ]", "", sample$Comment)
    
    sample$Comment <- sub("^\\s*<U\\+\\w+>\\s*", "", sample$Comment)
    sampleBrand <-sample[!grep("<U+1798>", sample$Comment),]
    sample$Date <- substring(sample$Date ,1,10)
    sample$Comment <- gsub("[[:punct:]]", "", sample$Comment)
    sample <- sample[sample$Date < "2018-09-01",]
    sample$Comment <- str_replace_all(sample$Comment, "[^[:alnum:]]", " ")
    
    write.table(sample, "Comments.csv", sep = ",", col.names = T, append = T,row.names = F)
  }
}



#-----------------------------xxxx----------------------------------

#To convert date into year and quarter

ReviewsDf <- read.csv('D:\\DWBI Final Data\\Comments.csv', stringsAsFactors = F)

ReviewsDf$Date <- format(as.yearqtr(ReviewsDf$Date, format = "%Y-%m-%d"), format = "%Y-%q")

#To convert Date to Year and quarters in diffrent columns

datetxt <- as.yearqtr(ReviewsDf$Date)

df <- data.frame(Date = datetxt,
                 Year = as.numeric(format(datetxt, format = "%Y")),
                 Quarter = as.numeric(format(datetxt, format = "%q")))


CommentsRevDf <- data.frame(matrix(ncol = 4, nrow = 0))

x <- c("Brand","Year","Quarter","Comments")
colnames(CommentsRevDf) <- x

for(j in 1:(length(ReviewsDf$Comment)))
{
  print(j)
  CommentsRevDf[j, ] = c(ReviewsDf$Brand[j],df$Year[j],df$Quarter[j],ReviewsDf$Comment[j])
  
}

CommentsRevDf <- na.omit(CommentsRevDf)
CommentsRevDf <- CommentsRevDf[CommentsRevDf$Year > "2014",]
write.csv(CommentsRevDf, 'CommentsYearQtr.csv', row.names=FALSE)


#Sentiment Analysis of Comments using afinn


CommentsQtrRev <- read.csv('D:\\DWBI Final Data\\CommentsYearQtr.csv', stringsAsFactors = F)



comment_text_df <- data_frame(year=CommentsQtrRev$Year,quarter=CommentsQtrRev$Quarter ,brand=CommentsQtrRev$Brand, text = CommentsQtrRev$Comments)
comment_text_df <- comment_text_df %>% unnest_tokens(word, text)

afin <- get_sentiments("afinn")
#here the mean of the score is taken and a new column named sentiment is being added
afinSent <- comment_text_df %>%
  inner_join(afin) %>%
  group_by(Brand=brand, Year= year, Quarter= quarter) %>%
  summarise(Sentiment = mean(score))
na.omit(afinSent)
afinSent<-afinSent[!(afinSent$Brand=="Vivo"),]
afinSent<-afinSent[!(afinSent$Year=="2018" & afinSent$Quarter==4),]
afinSent<-afinSent[!(afinSent$Year=="2018" & afinSent$Quarter==3),]

write.csv(afinSent, 'SentimentAnalysis.csv', row.names=FALSE)






#############################################################################################
#################################### SHIPMENT WORLDWIDE #####################################
#############################################################################################3

ShipmentWorldwide = 'D:\\DWBI Final Data\\statistic_id728644_global-smartphone-shipments-by-quarter-2009-2018.xlsx'
df <- read.xlsx(ShipmentWorldwide, sheetIndex =2 , startRow=5)

names(df)[1]<-"Year"
names(df)[2] <-"ShipmentInMillionUnit"

stringSeprator <- str_split_fixed(df$Year, " ", 2)
df <- add_column(df, Quarter=stringSeprator[1:35,1], .after = "Year")


df$Quarter <- gsub("[a-zA-Z ]", "", df$Quarter)


df$Year <- gsub("Q1 '", "20", df$Year)
df$Year <- gsub("Q2 '", "20", df$Year)
df$Year <- gsub("Q3 '", "20", df$Year)
df$Year <- gsub("Q4 '", "20", df$Year)

df<-df[!(df$Year=="2018" & df$Quarter==4),]
df<-df[!(df$Year=="2018" & df$Quarter==3),]
df <- df[df$Year > "2014",]
#quaterString <- gsub( "Q", " ", as.character(df$Quarter))

write.csv(df, file="ShipmentShareWorldwide.csv",row.names=FALSE)
# 
#---------------------------------------------------------------------

########################################################################
######################### WIKIPEDIA SCRAPING ###########################
########################################################################

#scraping html table using rvest

brand_Info <- read_html("https://en.wikipedia.org/wiki/ZTE")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[1]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X4 = as.character(d2$X4)
d2$X10 = as.character(d2$X10)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="ZTE", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL
write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)
#in order to append e have used table
#--------------------------------------------------------------------------------

brand_Info <- read_html("https://en.wikipedia.org/wiki/Apple_Inc.")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[1]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X4 = as.character(d2$X4)
d2$X8 = as.character(d2$X8)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Apple", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------

brand_Info <- read_html("https://en.wikipedia.org/wiki/Lenovo")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[2]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X4 = as.character(d2$X4)
d2$X7 = as.character(d2$X7)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Lenovo", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------

brand_Info <- read_html("https://en.wikipedia.org/wiki/Xiaomi")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[1]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X3 = as.character(d2$X3)
d2$X6 = as.character(d2$X6)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Xiaomi", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------



#----------------------------------------------------------------------------

brand_Info <- read_html("https://en.wikipedia.org/wiki/Alcatel_Mobile")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[1]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X2 = as.character(d2$X2)
d2$X3 = as.character(d2$X3)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Alcatel", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)
#-----------------------------------

#-----------------------------------------

brand_Info <- read_html("https://en.wikipedia.org/wiki/Samsung_Electronics")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[1]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X3 = as.character(d2$X3)
d2$X7 = as.character(d2$X7)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Samsung", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------


brand_Info <- read_html("https://en.wikipedia.org/wiki/Huawei")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[1]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X4 = as.character(d2$X4)
d2$X6 = as.character(d2$X6)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Huawei", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------


brand_Info <- read_html("https://en.wikipedia.org/wiki/LG_Electronics")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[2]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X3 = as.character(d2$X3)
d2$X6 = as.character(d2$X6)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="LG", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------

brand_Info <- read_html("https://en.wikipedia.org/wiki/Oppo")
brand_info_table = html_table(html_nodes(brand_Info, "table")[[2]], fill=TRUE)

brand_info_table = tail(brand_info_table, -1)

brand_info_table = brand_info_table[brand_info_table$X1 %in% c("Type","Founded"),]


d2 <- data.frame(t(brand_info_table))

d2$X3 = as.character(d2$X3)
d2$X5 = as.character(d2$X5)

colnames(d2) <- d2[1, ]
d2 <- d2[-1, ]
d2

df2 <- add_column(d2, Brand="Oppo", .before = "Type")
sampleDf <- df2
sampleDf <- na.omit(sampleDf)
sampleDf$Founded <- NULL


write.table(sampleDf, "BrandInfo.csv", sep = ",", col.names = T, append = T,row.names = F)

#----------------------------------------------------------------------------

BrandInfo = read.csv("BrandInfo.csv", stringsAsFactors = F)
BrandInfo = BrandInfo[!BrandInfo$Type == 'Type',]
write.csv(BrandInfo, 'BrandType.csv', row.names=FALSE)
if (file.exists("BrandInfo.csv")) file.remove("BrandInfo.csv")
#----------------------------------------------------------------------------
###############################################################################################
################################ SCRAPPING FROM QUANDL ########################################
###############################################################################################
setwd("D:\\DWBI Final Data")
#Stocks from Quandl

stock_data_files <- data.frame(matrix(ncol = 2, nrow = 5))
colNames <- c("Url","Brand")
colnames(stock_data_files) <- colNames 


#URls for Stocks Data
stock_data_files$Url <- c("https://www.quandl.com/api/v3/datasets/SSE/APC.csv?api_key=vR8tP4h-fTwBhbc5BUXQ","https://www.quandl.com/api/v3/datasets/SSE/LGLG.csv?api_key=vR8tP4h-fTwBhbc5BUXQ","https://www.quandl.com/api/v3/datasets/SSE/SSU.csv?api_key=vR8tP4h-fTwBhbc5BUXQ","https://www.quandl.com/api/v3/datasets/SSE/FZM.csv?api_key=vR8tP4h-fTwBhbc5BUXQ","https://www.quandl.com/api/v3/datasets/SSE/NOA3.csv?api_key=vR8tP4h-fTwBhbc5BUXQ")
stock_data_files$Brand <- c("Apple","LG","Samsung","ZTE","Alcatel")

for (i in 1:length(stock_data_files$Brand)) {
  
  StocksData <- read.csv(stock_data_files$Url[i])
  StocksData <- StocksData[ , which(names(StocksData) %in% c("Date","Previous.Day.Price"))]
  StocksData$Date <- as.character(StocksData$Date)
  stocksDFrame <- data.frame(matrix(ncol = 3, nrow = 0))
  
  colName <- c("Brand","Date","Close")
  colnames(stocksDFrame) <- colName
  
  for(j in 1:(length(StocksData$Date)))
  {
    stocksDFrame[j, ] = c(stock_data_files$Brand[i],StocksData$Date[j],StocksData$Previous.Day.Price[j])
    
  }
  
  stocksDFrame <- stocksDFrame[stocksDFrame$Date > "2014-12-31",]
  stocksDFrame = stocksDFrame[!stocksDFrame$Date == 'Date',]
  write.table(stocksDFrame, "Stocks.csv", sep = ",", col.names = T, append = T,row.names = F)
  
}

StockDataDf <- read.csv('D:\\DWBI Final Data\\Stocks.csv', stringsAsFactors = F)
StockDataDf = StockDataDf[!StockDataDf$Date == 'Date',]
StockDataDf$Date <- format(as.yearqtr(StockDataDf$Date, format = "%Y-%m-%d"), format = "%Y-%q")

#To convert Date to Year and quarters in diffrent columns

datetxt <- as.yearqtr(StockDataDf$Date)

df <- data.frame(Date = datetxt,
                 Year = as.numeric(format(datetxt, format = "%Y")),
                 Quarter = as.numeric(format(datetxt, format = "%q")))


StockRevDataDf <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Brand","Year","Quarter","Close")
colnames(StockRevDataDf) <- x

for(j in 1:(length(StockDataDf$Brand)))
{
  StockRevDataDf[j, ] = c(StockDataDf$Brand[j],df$Year[j],df$Quarter[j],StockDataDf$Close[j])
}
StockRevDataDf<-StockRevDataDf[!(StockRevDataDf$Year=="2014"),]
StockRevDataDf<-StockRevDataDf[!(StockRevDataDf$Year=="2018" & StockRevDataDf$Quarter==4),]
StockRevDataDf<-StockRevDataDf[!(StockRevDataDf$Year=="2018" & StockRevDataDf$Quarter==3),]
StockRevDataDf <- na.omit(StockRevDataDf)

write.csv(StockRevDataDf, 'StockAnalysis.csv', row.names=FALSE)

df <- read.csv('D:\\DWBI Final Data\\StockAnalysis.csv', stringsAsFactors = F)
df = aggregate(Close~., df, FUN= mean)

write.csv(df, "StockMarketAnalysis.csv", row.names = F)

if (file.exists("StockAnalysis.csv")) file.remove("StockAnalysis.csv")
if (file.exists("Stocks.csv")) file.remove("Stocks.csv")


########################################################################
##########################--MARKET SHARE---#############################
########################################################################

#Merging Market share

setwd("D:\\DWBI Final Data\\Market Share")


combinedMarketShare <- ldply( .data = list.files(pattern="*.csv"),
                              .fun = read.csv,
                              stringsAsFactors = FALSE,
                              header = FALSE,
                              col.names=c("Year", "Quarter", "Country", "Brand", "MarketShare" )
)

combinedMarketShare <- combinedMarketShare[!grepl("Year", combinedMarketShare$Year),]
combinedMarketShare <- combinedMarketShare[!grepl("Vivo", combinedMarketShare$Brand),]
combinedMarketShare<-combinedMarketShare[!(combinedMarketShare$Year=="2018" & combinedMarketShare$Quarter==4),]
combinedMarketShare<-combinedMarketShare[!(combinedMarketShare$Year=="2018" & combinedMarketShare$Quarter==3),]

setwd("D:\\DWBI Final Data")

write.csv(combinedMarketShare, 'MarketShar.csv', row.names=FALSE)


marketSdf <- read.csv('D:\\DWBI Final Data\\MarketShar.csv', stringsAsFactors = F)
#location
column_names <- c("Year","Quarter","Country","Brand","MarketShare")
colnames(marketSdf) <- column_names
marketSdf$Continent <- factor(countrycode(sourcevar = marketSdf[, "Country"],
                                          origin = "country.name",
                                          destination = "continent"))
marketSdf <- marketSdf[!grepl("Vivo", marketSdf$Brand),]
if (file.exists("MarketShar.csv")) file.remove("MarketShar.csv")
write.csv(marketSdf, 'MarketShare.csv', row.names=FALSE)

###################################################################################################
###################################################################################################3

#############################################################################################
#################### CLEANING OF SMARTPHONE SHIPMENT DATA FROM STATISTA #####################
#############################################################################################
setwd("D:\\DWBI Final Data\\")
shipment_statista_filename = 'D:\\DWBI Final Data\\statistic_id271490_global-smartphone-shipments-by-vendor-manufacturer-2009-2018.xlsx'
shipment_dataFrame <- read.xlsx(shipment_statista_filename, sheetIndex =2 , startRow=5)

names(shipment_dataFrame)[1]<-"Year"
names(shipment_dataFrame)[11] <-"Alcatel"
names(shipment_dataFrame)[4] <-"Huawei"
names(shipment_dataFrame)[7] <-"LG"
names(shipment_dataFrame)[8] <-"Lenovo"
names(shipment_dataFrame)[9] <-"ZTE"
names(shipment_dataFrame)[6] <-"Oppo"
names(shipment_dataFrame)[10] <-"Vivo"
shipment_dataFrame$Sony. <- NULL
shipment_dataFrame$RIM. <- NULL
shipment_dataFrame$HTC <- NULL
shipment_dataFrame$Nokia.. <- NULL
shipment_dataFrame$Others <- NULL
stringSeprator <- str_split_fixed(shipment_dataFrame$Year, " ", 2)
shipment_dataFrame <- add_column(shipment_dataFrame, Quarter=stringSeprator[1:36,1], .after = "Year")

#gsub() function replaces all matches of a string
shipment_dataFrame$Quarter <- gsub("[a-zA-Z ]", "", shipment_dataFrame$Quarter)

shipment_dataFrame$Year <- gsub("Q1 '", "20", shipment_dataFrame$Year)
shipment_dataFrame$Year <- gsub("Q2 '", "20", shipment_dataFrame$Year)
shipment_dataFrame$Year <- gsub("Q3 '", "20", shipment_dataFrame$Year)
shipment_dataFrame$Year <- gsub("Q4 '", "20", shipment_dataFrame$Year)
shipment_dataFrame$Year[31] <- "2017"
shipment_dataFrame$Quarter[31] <- "2"
write.csv(shipment_dataFrame, file="ShipmentShare.csv",row.names=FALSE)
getwd()
altered_dataframe <- read.csv("D:\\DWBI Final Data\\ShipmentShare.csv", header=T, na.strings=c("-","NA"))

#Replace NA by Mean
x <- mean(altered_dataframe$Alcatel, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$Alcatel[which(is.na(altered_dataframe$Alcatel))] <- y

x <- mean(altered_dataframe$Huawei, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$Huawei[which(is.na(altered_dataframe$Huawei))] <- y

x <- mean(altered_dataframe$Xiaomi, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$Xiaomi[which(is.na(altered_dataframe$Xiaomi))] <- y

x <- mean(altered_dataframe$Oppo, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$Oppo[which(is.na(altered_dataframe$Oppo))] <- y

x <- mean(altered_dataframe$LG, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$LG[which(is.na(altered_dataframe$LG))] <- y

x <- mean(altered_dataframe$Lenovo, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$Lenovo[which(is.na(altered_dataframe$Lenovo))] <- y

x <- mean(altered_dataframe$ZTE, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$ZTE[which(is.na(altered_dataframe$ZTE))] <- y

x <- mean(altered_dataframe$Vivo, na.rm = T,digits=1, nsmall=1)
y <- format(x, digits=1, nsmall=1)
altered_dataframe$Vivo[which(is.na(altered_dataframe$Vivo))] <- y

altered_dataframe <- altered_dataframe[altered_dataframe$Year > "2014-12-31",]
altered_dataframe<-altered_dataframe[!(altered_dataframe$Year=="2018" & altered_dataframe$Quarter==4),]
altered_dataframe<-altered_dataframe[!(altered_dataframe$Year=="2018" & altered_dataframe$Quarter==3),]

write.csv(altered_dataframe, file="ShipmentV.csv",row.names = F)


#cleaning the data

shipmentDataFrame <- read.csv("D:\\DWBI Final Data\\ShipmentV.csv")
year = shipmentDataFrame$Year

shipment_data_frame <- data.frame(matrix(ncol = 4, nrow= 0))
column_names <- c("Year", "Quarter", "Brand", "Shipment Percentage")
colnames(shipment_data_frame) <- column_names

for (i in 3: ncol(shipmentDataFrame)) {
  lengthOfDataFrame = nrow(shipment_data_frame) 
  for (j in 1 : length(year)) {
    shipment_data_frame[ lengthOfDataFrame + j, ] = c(shipmentDataFrame$Year[j], shipmentDataFrame$Quarter[j],colnames( shipmentDataFrame )[i] , shipmentDataFrame[j, i])
  }
}

shipment_data_frame <- shipment_data_frame[!grepl("Vivo", shipment_data_frame$Brand),]
write.csv(shipment_data_frame, 'ShipmentDataByVendor.csv', row.names=FALSE)

#This is being done inorder to avoid the commit to the same file again and again
if (file.exists("ShipmentV.csv")) file.remove("ShipmentV.csv")
if (file.exists("ShipmentShare.csv")) file.remove("ShipmentShare.csv")



#############################################################################################
#################### CLEANING OF MARKET SHARE DATA #####################
#############################################################################################
#This was done in order to create new files from data and covert the data from the rows to columns

# df1 <- read.csv("C:\\Users\\Yogesh\\Documents\\gsmarena\\CA2018.csv")
# df1$Country <- as.character(df1$Country)
# 
# year = df1$Year
# marketShare_data_frame <- data.frame(matrix(ncol = 5, nrow= 0))
# column_names <- c("Year", "Quarter", "Country", "Brand", "Market Share")
# colnames(marketShare_data_frame) <- column_names
# 
# for (i in 4: ncol(df1)) {
#   lengthOfDataFrame = nrow(marketShare_data_frame) 
#   for (j in 1 : length(year)) {
#     marketShare_data_frame[ lengthOfDataFrame + j, ] = c(df1$Year[j], df1$Quarter[j], df1$Country[j], colnames( df1 )[i] , df1[j, i])
#   }
# }
# CNedit <- CN1
# #1. #remove the col X
# CNedit <- CNedit[ , -which(names(CNedit) %in% c("X"))]
# #2. #add column inbetween two columns
# CNedit <- add_column(CNedit, Quarter=1:4, .after = "Date")
# 
# #remove pattern from columns
# #gsub("([0-9]+)-.*", "\\1", CN1$Date)
# 
# #list2 <- rep("2018",4)
# #3. Remove pattern
# CNedit$Date <- gsub("([0-9]+)-.*", "\\1", CNedit$Date)
# 
# 
# #CN1[,1] <- NULL
# #4.# Rename column name
# names(CNedit)[1]<-"Year"
# 
# 
# CN7 <- CNedit
# 
# #Merging 
# 
# 
# marketShareDf = Reduce(function(x, y) merge(x, y, all=TRUE), list(CN1,CN2,CN3,CN4,CN5,CN6,CN7))
# 
# marketShareDf <- add_column(marketShareDf, Country="Germany", .after = "Quarter")
# 
# write.csv(marketShareDf, file="GermanyMarketShare.csv")
# 

