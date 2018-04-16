########## Set Up ########## 
# Set working directory
setwd("~/rbraun/belgian")

# Load packages
packages <- c("xml2","rvest", "dplyr", "ggplot2", "SnowballC", "lubridate", "stringr", "httr", "RSelenium", "XML", "wdman", "jsonlite")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)

########## Page ##########
url <- "https://database.namenlijst.be/api/v1"
json <- '{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1890","month":"01","day":"01"},"dateTo":{"year":"","month":"","day":""}}]},"token":"GUEST"}}'

out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
# g <-content(r, "text", "application/json", encoding="UTF-8")

dat <- content(r, type="application/json")$result$data

b <- as.Date(paste(dat[[1]]$bornDate, collapse="-"), format="%Y-%d-%m")

as_date(b)

as.Date(b, format="%Y-%d-%m")



o <- lapply(dat, function(x){
  df <- data.frame(firstname = x$firstName, lastname = x$familyName, victimtype = x$victimType, borndate = as.Date(paste(x$bornDate, collapse="-"), format="%Y-%d-%m"), dieddate =  as.Date(paste(x$diedDate, collapse="-"), format="%Y-%d-%m"), gender = x$gender, id = x$`_id`)
  return(df)
})


df <- do.call(rbind, lapply(dat, function(x){
  df <- data.frame(firstname = x$firstName, lastname = x$familyName, victimtype = x$victimType, born_year = x$bornDate$year, born_month = x$bornDate$month, born_day = x$bornDate$day, died_year = x$diedDate$year, died_month = x$diedDate$month, died_day = x$diedDate$day, gender = x$gender, id = x$`_id`)
  return(df)
}))

do.call(rbind,o)


ids <- lapply(dat1, function(x){
  x$`_id`
})

a <- paste(ids, collapse='","')
json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',a,'"],"token":"GUEST"}}', sep="")

out2 <- jsonlite::fromJSON(json2)
r2 <- POST(url, body = out2, encode = 'json')
dat2 <- content(r2, type="application/json")
details <- dat2$result

####

json1 <- '{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["000009d1-d2b8-4e8a-9f67-a97c99b2986b","00005c62-1755-481f-b777-e256e0502ad3","000128da-1bab-4164-a6f3-cd509e77a8a2","0001fb0a-16f0-431a-924a-5b35f1b25cbf","000266aa-41a4-4375-86d2-cf8bcda75e81","bc012d0c-989e-4e1d-b3e7-45d096703496","00027b6e-449f-4170-8ed3-3eb75a921382","000344a4-c95b-4fd7-b47d-d86e04da5167","0004fdb3-4ae6-43cb-868d-0bac00af4121","0006cca1-8665-4830-9d5f-5d167f1bf3e6","0007661e-d695-465e-9c44-05715619ed1e","000847b0-1a00-4e39-92f4-f363ea6f49f5","0008a6c2-3ff3-449b-8662-101ee58232ba","00097851-c387-4991-a9b8-9efc0352fc84","0009ef28-6411-4586-b4c5-0a1301d9db88","000ba3ff-d505-415c-aebe-632f2368ba00","000d0bd7-9da1-44cb-8cec-00f8b354d794","000d1e23-95fa-4e9c-a6ee-e73de852f92d","000d98b8-79fa-46be-a5f0-d1894f2601b0","000e264d-4655-44a5-af01-eed081f00352"],"token":"GUEST"}}'

g <- content(r, as="parsed")

dat <- g$result$data

fromJSON(flatten=TRUE) %>%
  as_tibble(g) %>%
  glimpse(dat)

# start with 1800 -1840
####### Loop
url <- "https://database.namenlijst.be/api/v1"
start <- as.Date("01-01-1880",format="%d-%m-%Y")
end   <- as.Date("10-01-1880",format="%d-%m-%Y")
date <- start
df <- NULL

# Loop by date (start here)
while(date <= end){
  # Set Date
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  nextday <- format(date + 1, "%d")
  
  # Paste JSON
  json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', year, '","month":"', month, '","day":"', day, '"},"dateTo":{"year":"', year, '","month":"', month, '","day":"', day, '"}}]},"token":"GUEST"}}')
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result$data
  current_df <- do.call(rbind, lapply(dat, function(x){
    df <- data.frame(firstname = x$firstName, lastname = x$familyName, victimtype = x$victimType, born_year = x$bornDate$year, born_month = x$bornDate$month, born_day = x$bornDate$day, died_year = x$diedDate$year, died_month = x$diedDate$month, died_day = x$diedDate$day, gender = x$gender, id = x$`_id`)
    return(df)
  }))
  df <- rbind(df, current_df)
  date = date + 1
}

####
while(date <= end){
  # Set Date
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  nextday <- format(date + 1, "%d")
  
  # Paste JSON
  json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', year, '","month":"', month, '","day":"', day, '"},"dateTo":{"year":"', year, '","month":"', month, '","day":"', nextday, '"}}]},"token":"GUEST"}}')
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result$data
  current_df <- do.call(rbind, lapply(dat, function(x){
    df <- data.frame(firstname = x$firstName, lastname = x$familyName, victimtype = x$victimType, born_year = x$bornDate$year, born_month = x$bornDate$month, born_day = x$bornDate$day, died_year = x$diedDate$year, died_month = x$diedDate$month, died_day = x$diedDate$day, gender = x$gender, id = x$`_id`)
    return(df)
  }))
 df <- rbind(df, current_df)
 date = date + 1
}
####




content(r)
json <- '{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1800","month":"01","day":"01"},"dateTo":{"year":"1840","month":"1","day":"1"}}]},"token":"GUEST"}}'


