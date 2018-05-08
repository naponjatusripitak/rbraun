########## Set Up ########## 
# Set working directory
setwd("~/rbraun/belgian")

# Load packages
packages <- c("xml2","rvest", "dplyr", "ggplot2", "SnowballC", "lubridate", "stringr", "httr", "RSelenium", "XML", "wdman", "jsonlite", "plyr", "tidyr")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)

### Loop by Year ###
scrape_year <- function(from_date, to_date){
  url <- "https://database.namenlijst.be/api/v1"
  df <- NULL
  
  # Loop by date (start here)
  while(from_date <= to_date){
    # Set Date
    year <- from_date

    # Get Personal Information
    json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', year, '","month":"","day":""},"dateTo":{"year":"', year, '","month":"","day":""}}]},"token":"GUEST"}}')
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result$data
    
    non.null.list <- lapply(dat, Filter, f = Negate(is.null))
    personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
    
    # Get Military Information
    ids <- paste(personal_df$X_id, collapse='","')
    json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
    out2 <- jsonlite::fromJSON(json2)
    r2 <- POST(url, body = out2, encode = 'json')
    dat2 <- content(r2, type="application/json")$result
    
    #Fill in ID for joining
    for(i in 1:length(dat2)){
      dat2[[i]]$X_id <- names(dat2)[i]
    }
    
    non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
    military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
    military_df$X_id <- as.character(military_df$X_id)
    personal_df$X_id <- as.character(personal_df$X_id)
    
    current_df <- full_join(personal_df, military_df, by="X_id")
    df <- rbind.fill(df, current_df)
    
    # Next
    from_date = from_date + 1
  }
  return(df)
}

# Try function
df1 <- scrape_year(1860, 1863)

######### Create Function for Scraping by Date ###
scrape_date <- function(from_date, to_date){
  url <- "https://database.namenlijst.be/api/v1"
  start <- as.Date(from_date,format="%d-%m-%Y")
  end   <- as.Date(to_date,format="%d-%m-%Y")
  date <- start
  df <- NULL
  
  # Loop by date (start here)
  while(date <= end){
    # Set Date
    year <- format(date, "%Y")
    month <- format(date, "%m")
    day <- format(date, "%d")
    nextday <- format(date + 1, "%d")
    
    # Get Personal Information
    json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', year, '","month":"', month, '","day":"', day, '"},"dateTo":{"year":"', year, '","month":"', month, '","day":"', day, '"}}]},"token":"GUEST"}}')
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result$data
    
    non.null.list <- lapply(dat, Filter, f = Negate(is.null))
    
    if(length(non.null.list) == 0) {
      #Next
      date = date + 1
    } else {
      
      personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
      
      # Get Military Information
      ids <- paste(personal_df$X_id, collapse='","')
      json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'", "0"],"token":"GUEST"}}', sep="")
      out2 <- jsonlite::fromJSON(json2)
      r2 <- POST(url, body = out2, encode = 'json')
      dat2 <- content(r2, type="application/json")$result
      
      #Fill in ID for joining
      for(i in 1:length(dat2)){
        dat2[[i]]$X_id <- names(dat2)[i]
      }
      
      non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
      military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
      military_df$X_id <- as.character(military_df$X_id)
      personal_df$X_id <- as.character(personal_df$X_id)
      
      current_df <- full_join(personal_df, military_df, by="X_id")
      df <- rbind.fill(df, current_df)
      
      # Next
      date = date + 1
    }}
  return(df)
}

# Try function
df1 <- scrape_date("01-01-1880", "31-01-1880")


#### 1800 - 1840 #### N = 575
url <- "https://database.namenlijst.be/api/v1"
df <- NULL

# Get Personal Information
json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1800","month":"","day":""},"dateTo":{"year":"1840","month":"","day":""}}]},"token":"GUEST"}}')
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")$result$data

non.null.list <- lapply(dat, Filter, f = Negate(is.null))
personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))

# Get Military Information
ids <- paste(personal_df$X_id, collapse='","')
json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
out2 <- jsonlite::fromJSON(json2)
r2 <- POST(url, body = out2, encode = 'json')
dat2 <- content(r2, type="application/json")$result

#Fill in ID for joining
for(i in 1:length(dat2)){
  dat2[[i]]$X_id <- names(dat2)[i]
}

non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
current_df <- full_join(personal_df, military_df, by="X_id")

df <- rbind.fill(df, current_df)


#### 1841 - 1850 #### N = 767
# Get Personal Information
json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1841","month":"","day":""},"dateTo":{"year":"1850","month":"","day":""}}]},"token":"GUEST"}}')
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")$result$data

non.null.list <- lapply(dat, Filter, f = Negate(is.null))
personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))

# Get Military Information
ids <- paste(personal_df$X_id, collapse='","')
json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
out2 <- jsonlite::fromJSON(json2)
r2 <- POST(url, body = out2, encode = 'json')
dat2 <- content(r2, type="application/json")$result

#Fill in ID for joining
for(i in 1:length(dat2)){
  dat2[[i]]$X_id <- names(dat2)[i]
}

non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
current_df <- full_join(personal_df, military_df, by="X_id")

df <- rbind.fill(df, current_df)

#### 1851 - 1859 #### N = 927
# Get Personal Information
json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1851","month":"","day":""},"dateTo":{"year":"1859","month":"","day":""}}]},"token":"GUEST"}}')
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")$result$data

non.null.list <- lapply(dat, Filter, f = Negate(is.null))
personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))

# Get Military Information
ids <- paste(personal_df$X_id, collapse='","')
json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
out2 <- jsonlite::fromJSON(json2)
r2 <- POST(url, body = out2, encode = 'json')
dat2 <- content(r2, type="application/json")$result

#Fill in ID for joining
for(i in 1:length(dat2)){
  dat2[[i]]$X_id <- names(dat2)[i]
}

non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
current_df <- full_join(personal_df, military_df, by="X_id")

df <- rbind.fill(df, current_df)



#### 1860 - 1869
current_df <- scrapeinfo(1860, 1869)
df <- rbind.fill(df, current_df)
#save(df, file="namelist.Rda")

#### 1870 - 1879

current_df <- scrapeinfo("04-01-1870", "04-01-1870")

#### 1860 - 1864 #### N = 822
# Get Personal Information
json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1860","month":"","day":""},"dateTo":{"year":"1864","month":"","day":""}}]},"token":"GUEST"}}')
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")$result$data

non.null.list <- lapply(dat, Filter, f = Negate(is.null))
personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))

# Get Military Information
ids <- paste(personal_df$X_id, collapse='","')
json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
out2 <- jsonlite::fromJSON(json2)
r2 <- POST(url, body = out2, encode = 'json')
dat2 <- content(r2, type="application/json")$result

#Fill in ID for joining
for(i in 1:length(dat2)){
  dat2[[i]]$X_id <- names(dat2)[i]
}

non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
current_df <- full_join(personal_df, military_df, by="X_id")

df <- rbind.fill(df, current_df)


### In case
# Get Additional Info
non.null.list <- lapply(dat3, Filter, f = Negate(is.null))
additional_info <- rbind.fill(lapply(non.null.list, function(x){
  as.data.frame(t(unlist(x)))
}))

additional_info$X_id<- additional_info$`_id`

#Join
current_df <- left_join(current_df, additional_info, by ="X_id")

