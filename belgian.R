########## Set Up ########## 
# Set working directory
setwd("~/rbraun/belgian")

# Load packages
packages <- c("xml2","rvest", "dplyr", "ggplot2", "SnowballC", "lubridate", "stringr", "httr", "RSelenium", "XML", "wdman", "Rcrawler")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)

########## Start Selenium Server ########## 
rD <- rsDriver()
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4567L
                      , browserName = "chrome"
)

cDrv <- chrome()
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))
remDr<- remoteDriver(browserName = "chrome", port = 4567L, 
                     extraCapabilities = eCaps)

########### Accessing Page ###############
remDr$open()
remDr$screenshot(display = TRUE)

site <- "http://klm-mraiwp.bh-a.eu/fmi/iwp/cgi?-db=DossiersOfficiers&-loadframes"
remDr$navigate(site)
frames <- remDr$findElements(using = "tag name", "frame")
sapply(frames, function(x){x$getElementAttribute("src")})

remDr$switchToFrame(frames[[1]])  
toggle <- remDr$findElement(using ="id", "toggleButton")
toggle$clickElement()

frames <- remDr$findElements(using = "tag name", "frame")
sapply(frames, function(x){x$getElementAttribute("src")})
remDr$switchToFrame(frames[[1]])  
dropdown <- remDr$findElement(using="xpath", "//*[@id='laypopup']/option[2]")
dropdown$clickElement()

table <- remDr$findElement(using ="id", "tableview-button")
table$clickElement()

############ Scraping ###############
# df <- NULL
for(i in seq(267851, 267856, 50)){
  url <- paste0("http://klm-mraiwp.bh-a.eu/fmi/iwp/cgi?-recordnumber=",i, "&-goto=")
  remDr$navigate(url)
  source <- remDr$getPageSource()[[1]]
  texts <- source %>%
    read_html() %>%
    html_nodes("div div") %>%
    html_text()
  currentlist <- data.frame(matrix(texts, nrow=6, byrow=T))
  df <- rbind(df, currentlist)
}

#save(df, file="list.Rda")
#write.csv(df, file = "list.csv")

