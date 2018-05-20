########## Set Up ########## 
# Set working directory
setwd("~/rbraun/belgian")

# Load packages
packages <- c("xml2","rvest", "dplyr", "ggplot2", "SnowballC", "lubridate", "stringr", "httr", "RSelenium", "XML", "wdman", "jsonlite", "plyr", "tidyr", "zoo")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)

###### Scrape by interval (no looping involved) #######
scrape_interval <- function(from_date, to_date){
  url <- "https://database.namenlijst.be/api/v1"
  df <- NULL
  if(nchar(from_date) == 4){
    from_year <- from_date
    to_year <- to_date
    from_month <- NULL
    to_month <- NULL
    from_day <- NULL
    to_day <- NULL
  } else if(nchar(from_date) == 7){
    from_year <- substr(from_date, start = 4, stop = 8)
    to_year <- substr(to_date, start = 4, stop = 8)
    from_month <- substr(from_date, start = 1, stop = 2)
    to_month <- substr(to_date, start = 1, stop = 2)
    from_day <- NULL
    to_day <- NULL  } else {
      from_year <- substr(from_date, start = 7, stop = 11)
      to_year <- substr(to_date, start = 7, stop = 11)
      from_month <- substr(from_date, start = 4, stop = 5)
      to_month <- substr(to_date, start = 4, stop = 5)
      from_day <- substr(from_date, start = 1, stop = 2)
      to_day <- substr(to_date, start = 1, stop = 2)
    }
    
    # Get Personal Information
    json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', from_year,'","month":"', from_month,'","day":"', from_day,'"},"dateTo":{"year":"', to_year,'","month":"', to_month,'","day":"', to_day,'"}}]},"token":"GUEST"}}', sep="")
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
    df <- current_df
  return(df)
}
###### Loop by Year ######
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
df1 <- scrape_year(1860, 1860)

######### Create Function for Scraping by Date #######
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


####### 1800 - 1840 N = 575 ########
df.1800.1840 <- scrape_interval("1800", "1840")
####### 1841 - 1850 N = 768 ########
df.1841.1850 <- scrape_interval("1841", "1850")
####### 1851 - 1859  N = 927 ########
df.1851.1859 <- scrape_interval("1851", "1859")
####### 1860 - 1864  N = 822 ########
df.1860.1864 <- scrape_interval("1860", "1864")
####### 1865 - 1868  N = 928 ########
df.1865.1868 <- scrape_interval("1865", "1868")

df.1869.1889 <- scrape_monthly("01-01-1869", "31-12-1889")


######## Function for scraping, looping by month #######
scrape_monthly <- function(from_year, to_year){
 months <- format(seq(as.Date(from_year, format = "%d-%m-%Y"), as.Date(to_year, format = "%d-%m-%Y"), by ="month"), format = "%m-%Y")
 df <- sapply(months, function(x){
   scrape_interval(x, x)
 })
 return(df)
}

####### Manual #######
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
################## try going over limit
json <- '{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"born","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"1870","month":"","day":""},"dateTo":{"year":"1873","month":"","day":""}}]},"token":"GUEST"}}'
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")

###### Try improve function #####
scraper <- function(from_date, to_date){
  url <- "https://database.namenlijst.be/api/v1"
  df <- NULL
  cap1000 <- NULL
  if(nchar(from_date) == 4){
    from_year <- from_date
    to_year <- to_date
    from_month <- NULL
    to_month <- NULL
    from_day <- NULL
    to_day <- NULL
  } else if(nchar(from_date) == 7){
    from_year <- substr(from_date, start = 4, stop = 8)
    to_year <- substr(to_date, start = 4, stop = 8)
    from_month <- substr(from_date, start = 1, stop = 2)
    to_month <- substr(to_date, start = 1, stop = 2)
    from_day <- NULL
    to_day <- NULL  } else {
      from_year <- substr(from_date, start = 7, stop = 11)
      to_year <- substr(to_date, start = 7, stop = 11)
      from_month <- substr(from_date, start = 4, stop = 5)
      to_month <- substr(to_date, start = 4, stop = 5)
      from_day <- substr(from_date, start = 1, stop = 2)
      to_day <- substr(to_date, start = 1, stop = 2)
    }
  
  # Get Personal Information
  json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"died","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', from_year,'","month":"', from_month,'","day":"', from_day,'"},"dateTo":{"year":"', to_year,'","month":"', to_month,'","day":"', to_day,'"}}]},"token":"GUEST"}}', sep="")
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result$data
  
  if(length(dat) < 1000){
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
  df <- current_df
  } else {
    months_interval <- format(seq(as.Date(from_date, format = "%d-%m-%Y"), as.Date(to_date, format = "%d-%m-%Y"), by ="month"), format = "%m-%Y")
    pb <- txtProgressBar(min = 0, max = length(months_interval), style = 3)
  for(i in 1:length(months_interval)){
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
    months <- months_interval[i]
  month_month <- substr(months, start = 1, stop = 2)
  month_year <- substr(months, start = 4, stop = 8)
  # Get Personal Information
    json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"died","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', month_year,'","month":"', month_month,'","day":""},"dateTo":{"year":"', month_year,'","month":"', month_month,'","day":""}}]},"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result$data
  
    if(length(dat) < 1000){
    non.null.list <- lapply(dat, Filter, f = Negate(is.null))
    personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
    
    # Get Military Information
    ids <- paste(personal_df$X_id, collapse='","')
    json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
    out2 <- jsonlite::fromJSON(json2)
    r2 <- POST(url, body = out2, encode = 'json')
    dat2 <- content(r2, type="application/json")$result
    if(length(dat2) > 0) {
     
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
    } else {
      current_df <- personal_df
      df <- rbind.fill(df, current_df)
    }
    } else {
      st <- as.Date(paste0("01-", months), format = "%d-%m-%Y")
      en <- seq(st, by ="month", length = 2)[2] - 1
    date_interval <- format(seq(st, en, by = 1), format = "%d-%m-%Y")
    
    for(j in 1:length(date_interval)){
      date <- date_interval[j]
      date_date <- substr(date, start = 1, stop = 2)
      date_month <- substr(date, start = 4, stop = 5)
      date_year <- substr(date, start = 7, stop = 11)
      
      # Get Personal Information
      json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"","mode":"head"},"victim_type":"all","filters":[{"type":"died","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', date_year,'","month":"', date_month,'","day":"', date_date,'"},"dateTo":{"year":"', date_year,'","month":"', date_month,'","day":"', date_date,'"}}]},"token":"GUEST"}}', sep="")
      out <- jsonlite::fromJSON(json)
      r <- POST(url, body = out, encode = 'json')
      dat <- content(r, type="application/json")$result$data
  
      if(length(dat) < 1000){
      non.null.list <- lapply(dat, Filter, f = Negate(is.null))
      personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
      
      # Get Military Information
      ids <- paste(personal_df$X_id, collapse='","')
      json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
      out2 <- jsonlite::fromJSON(json2)
      r2 <- POST(url, body = out2, encode = 'json')
      dat2 <- content(r2, type="application/json")$result
      
      if(length(dat2) > 0) {
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
      } else {
        current_df <- personal_df
        df <- rbind.fill(df, current_df)
      }
      } else if(length(dat) == 0) {
        next} else {
         print(date)
          cap1000 <- c(cap1000, date)
          
          list_date <- lapply(letters, function(x){
            json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"', x,'","mode":"head"},"victim_type":"all","filters":[{"type":"died","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', date_year,'","month":"', date_month,'","day":"', date_date,'"},"dateTo":{"year":"', date_year,'","month":"', date_month,'","day":"', date_date,'"}}]},"token":"GUEST"}}', sep="")
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
            
            if(length(dat2) > 0) {
              #Fill in ID for joining
              for(i in 1:length(dat2)){
                dat2[[i]]$X_id <- names(dat2)[i]
              }
              
              non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
              military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
              military_df$X_id <- as.character(military_df$X_id)
              personal_df$X_id <- as.character(personal_df$X_id)
              
              current_df <- full_join(personal_df, military_df, by="X_id")
            } else {
              current_df <- personal_df
            }
            return(current_df)})
          current_df <- do.call(rbind.fill, list_date)
          df <- rbind.fill(df, current_df)
  }
    }
    } 
    close(pb)
  }
  }
  return(df)
  }

###
from_date <- "01-01-1800"
to_date <- "31-12-1913"
df1800.1913 <- scraper(from_date, to_date)

###
from_date <- "01-01-1914"
to_date <- "31-12-1920"
df1914.1920.2 <- scraper(from_date, to_date)

###
from_date <- "01-01-1921"
to_date <- "31-12-2018"
df1921.2018 <- scraper(from_date, to_date)

thenamelist <- rbind(df1800.1913, df1914.1920.2)
thenamelist <- rbind(thenamelist, df1921.2018)


thenamelist <- unique(thenamelist)

####

from_date <- "17-04-1918"
to_date <- "17-07-1918"
df.try <- scraper(from_date, to_date)


####
date <- "17-04-1918"
date_date <- substr(date, start = 1, stop = 2)
date_month <- substr(date, start = 4, stop = 5)
date_year <- substr(date, start = 7, stop = 11)

list_date <- lapply(letters, function(x){
json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPersonAdvancedPublic","arguments":{"firstName":{"value":"","mode":"head"},"lastName":{"value":"', x,'","mode":"head"},"victim_type":"all","filters":[{"type":"died","id":0,"place":{"value":"","mode":"head"},"dateFrom":{"year":"', date_year,'","month":"', date_month,'","day":"', date_date,'"},"dateTo":{"year":"', date_year,'","month":"', date_month,'","day":"', date_date,'"}}]},"token":"GUEST"}}', sep="")
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

if(length(dat2) > 0) {
  #Fill in ID for joining
  for(i in 1:length(dat2)){
    dat2[[i]]$X_id <- names(dat2)[i]
  }
  
  non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
  military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
  military_df$X_id <- as.character(military_df$X_id)
  personal_df$X_id <- as.character(personal_df$X_id)
  
  current_df <- full_join(personal_df, military_df, by="X_id")
} else {
  current_df <- personal_df
}
return(current_df)})



g <- as.matrix(unlist(list_date))


data.frame(matrix(unlist(list_date), nrow=132, byrow=T))
write.csv(thenamelist, file="thenamelist.csv")
