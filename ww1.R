########## Set Up ########## 
# Set working directory
setwd("~/rbraun/belgian")

# Load packages
packages <- c("xml2","rvest", "dplyr", "ggplot2", "SnowballC", "lubridate", "stringr", "httr", "RSelenium", "XML", "wdman", "jsonlite", "plyr", "tidyr", "zoo", "data.table","pbapply", "parallel")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)


##### Scrape by alphabetical order #####
#df <- NULL
for(i in 1:length(letters)){
  for(j in 1:length(letters)){
    first_name <- letters[i]
    last_name <- letters[j]
    print(paste0("first name: ", first_name))
    print(paste0("last name: ", last_name))
    
    # Get Personal Data
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPerson","arguments":{"firstName":{"value":"', first_name,'","mode":"head"},"lastName":{"value":"', last_name,'","mode":"head"},"bornPlace":{"value":"","mode":"head"},"livingPlace":{"value":"","mode":"head"},"diedFrom":{"year":"","month":"","day":""},"diedTo":{"year":"","month":"","day":""},"memorial":{"value":"","mode":"head"}},"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result$data
    
    # if dat < 1000, proceed as normal
    if(length(dat) < 1000) {
      non.null.list <- lapply(dat, Filter, f = Negate(is.null))
      personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
      
      # Get Military Information
      ids <- paste(personal_df$X_id, collapse='","')
      json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
      out2 <- jsonlite::fromJSON(json2)
      r2 <- POST(url, body = out2, encode = 'json')
      dat2 <- content(r2, type="application/json")$result
      
      # Check if null
      if(length(dat2) > 0) {
        
        #Fill in ID for joining
        for(a in 1:length(dat2)){
          dat2[[a]]$X_id <- names(dat2)[a]
        }
        
        non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
        military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
        military_df$X_id <- as.character(military_df$X_id)
        personal_df$X_id <- as.character(personal_df$X_id)
        
        
        current_df <- full_join(personal_df, military_df, by="X_id")
      } else {
        current_df <- personal_df
      }
    df <- rbind.fill(df, current_df)
    
    # if dat = 0, move to the next iteration
    } else if(length(dat) == 0){
      print("Null result. Moving to next iteration.")
      
    # if dat >= 1000, we have to be more specific
    } else {
      last_name_list <- paste0(letters[j], letters)
      for(k in 1:length(last_name_list)){
        last_name <- last_name_list[k]
        print(paste0("last name: ", last_name))
        # Get Personal Data
        url <- "https://database.namenlijst.be/api/v1"
        json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPerson","arguments":{"firstName":{"value":"', first_name,'","mode":"head"},"lastName":{"value":"', last_name,'","mode":"head"},"bornPlace":{"value":"","mode":"head"},"livingPlace":{"value":"","mode":"head"},"diedFrom":{"year":"","month":"","day":""},"diedTo":{"year":"","month":"","day":""},"memorial":{"value":"","mode":"head"}},"token":"GUEST"}}', sep="")
        out <- jsonlite::fromJSON(json)
        r <- POST(url, body = out, encode = 'json')
        dat <- content(r, type="application/json")$result$data
        
        # if dat < 1000, proceed as normal
        if(length(dat) < 1000) {
          non.null.list <- lapply(dat, Filter, f = Negate(is.null))
          personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
          
          # Get Military Information
          ids <- paste(personal_df$X_id, collapse='","')
          json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
          out2 <- jsonlite::fromJSON(json2)
          r2 <- POST(url, body = out2, encode = 'json')
          dat2 <- content(r2, type="application/json")$result
          
          # Check if null
          if(length(dat2) > 0) {
            
            #Fill in ID for joining
            for(b in 1:length(dat2)){
              dat2[[b]]$X_id <- names(dat2)[b]
            }
            
            non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
            military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
            military_df$X_id <- as.character(military_df$X_id)
            personal_df$X_id <- as.character(personal_df$X_id)
            
            
            current_df <- full_join(personal_df, military_df, by="X_id")
          } else {
            current_df <- personal_df
          }
          df <- rbind.fill(df, current_df)
        } else if(length(dat) == 0){
          print("Null result. Moving to next iteration.")
        } else {
          first_name_list <- paste0(letters[i], letters)
          for(l in 1:length(first_name_list)){
            first_name <- first_name_list[l]
            print(paste0("first name: ", first_name))
            # Get Personal Data
            url <- "https://database.namenlijst.be/api/v1"
            json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPerson","arguments":{"firstName":{"value":"', first_name,'","mode":"head"},"lastName":{"value":"', last_name,'","mode":"head"},"bornPlace":{"value":"","mode":"head"},"livingPlace":{"value":"","mode":"head"},"diedFrom":{"year":"","month":"","day":""},"diedTo":{"year":"","month":"","day":""},"memorial":{"value":"","mode":"head"}},"token":"GUEST"}}', sep="")
            out <- jsonlite::fromJSON(json)
            r <- POST(url, body = out, encode = 'json')
            dat <- content(r, type="application/json")$result$data
            
            # if dat < 1000, proceed as normal
            if(length(dat) < 1000) {
              non.null.list <- lapply(dat, Filter, f = Negate(is.null))
              personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
              
              # Get Military Information
              ids <- paste(personal_df$X_id, collapse='","')
              json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
              out2 <- jsonlite::fromJSON(json2)
              r2 <- POST(url, body = out2, encode = 'json')
              dat2 <- content(r2, type="application/json")$result
              
              # Check if null
              if(length(dat2) > 0) {
                
                #Fill in ID for joining
                for(c in 1:length(dat2)){
                  dat2[[c]]$X_id <- names(dat2)[c]
                }
                
                non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
                military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
                military_df$X_id <- as.character(military_df$X_id)
                personal_df$X_id <- as.character(personal_df$X_id)
                
                
                current_df <- full_join(personal_df, military_df, by="X_id")
              } else {
                current_df <- personal_df
              }
              df <- rbind.fill(df, current_df)
            } else if(length(dat) == 0){
              print("Null result. Moving to next iteration.")
            } else {
              print("Error")
            }
          }
        }
      }
    }
  }
}
      
      
##### Scraping people with empty first name ####
  for(i in 1:length(letters)){
    last_name <- letters[i]
    print(last_name)
    # Get Personal Data
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPerson","arguments":{"firstName":{"value":"","mode":"empty"},"lastName":{"value":"', last_name,'","mode":"head"},"bornPlace":{"value":"","mode":"head"},"livingPlace":{"value":"","mode":"head"},"diedFrom":{"year":"","month":"","day":""},"diedTo":{"year":"","month":"","day":""},"memorial":{"value":"","mode":"head"}},"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result$data
    
    if(length(dat) < 1000) {
      non.null.list <- lapply(dat, Filter, f = Negate(is.null))
      personal_df <- rbind.fill(lapply(non.null.list, as.data.frame))
      
      # Get Military Information
      ids <- paste(personal_df$X_id, collapse='","')
      json2 <- paste('{"jsonrpc":"2.0","method":"getPersonsDetails","id":1,"params":{"ids":["',ids,'"],"token":"GUEST"}}', sep="")
      out2 <- jsonlite::fromJSON(json2)
      r2 <- POST(url, body = out2, encode = 'json')
      dat2 <- content(r2, type="application/json")$result
      
      # Check if null
      if(length(dat2) > 0) {
        
        #Fill in ID for joining
        for(a in 1:length(dat2)){
          dat2[[a]]$X_id <- names(dat2)[a]
        }
        
        non.null.list <- lapply(dat2, Filter, f = Negate(is.null))
        military_df <- rbind.fill(lapply(non.null.list, as.data.frame))
        military_df$X_id <- as.character(military_df$X_id)
        personal_df$X_id <- as.character(personal_df$X_id)
        
        
        current_df <- full_join(personal_df, military_df, by="X_id")
      } else {
        current_df <- personal_df
      }
      df <- rbind.fill(df, current_df)
    } else {
      print("Over thousand")
    }
  }

##### Scraping by date of birth ####
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

##### Total #####
thenamelist <- rbind.fill(df, thenamelist)
thenamelist <- distinct(thenamelist)
#write.csv(thenamelist, "thenamelist.csv")
#save(thenamelist, file="thenamelist2.Rda")
#### Extra Codes ####
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

##### Getting additional info #####

url <- "https://database.namenlijst.be/api/v1"
json <- paste('{"jsonrpc":"2.0","method":"callFunction","id":1,"params":{"name":"FindPerson","arguments":{"firstName":{"value":"', first_name,'","mode":"head"},"lastName":{"value":"', last_name,'","mode":"head"},"bornPlace":{"value":"","mode":"head"},"livingPlace":{"value":"","mode":"head"},"diedFrom":{"year":"","month":"","day":""},"diedTo":{"year":"","month":"","day":""},"memorial":{"value":"","mode":"head"}},"token":"GUEST"}}', sep="")
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")$result$data

personal_id <- "00005c62-1755-481f-b777-e256e0502ad3"

get_additional_info <- function(x){
   lapply(x, function(k){
    personal_id <- k["X_id"]
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result
    return(dat)
  })
  }

j <- get_additional_info(obje)

dat <- lapply(obje, function(k){
  personal_id <- k["X_id"]
  url <- "https://database.namenlijst.be/api/v1"
  json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result
  return(dat)
})

get_additional_info <- function(x){
personal_id <- x["X_id"]
url <- "https://database.namenlijst.be/api/v1"
json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
out <- jsonlite::fromJSON(json)
r <- POST(url, body = out, encode = 'json')
dat <- content(r, type="application/json")$result
return(dat)
}


get_additional_info <- function(x){
  personal_id <- x
  url <- "https://database.namenlijst.be/api/v1"
  json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result
  return(dat)
}

j <- lapply(obje$X_id, get_additional_info)




### start here

j <- lapply(obje$X_id, function(x){
  personal_id <- x
  url <- "https://database.namenlijst.be/api/v1"
  json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result
  return(dat)
})

dfs <- lapply(j, function(x){
  data.frame(as.list(unlist(x)))})

output <- rbind.fill(lapply(dfs, as.data.frame))

### now try part 2
json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
out2 <- jsonlite::fromJSON(json2)
r2 <- POST(url, body = out2, encode = 'json')
dat2 <- content(r2, type="application/json")$result


###
j <- lapply(obje$X_id, function(x){
  personal_id <- x
  url <- "https://database.namenlijst.be/api/v1"
  json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result
  json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
  out2 <- jsonlite::fromJSON(json2)
  r2 <- POST(url, body = out2, encode = 'json')
  dat2 <- content(r2, type="application/json")$result
  output <- list(dat, dat2)
  return(output)
})


#mapply(c, first, second, SIMPLIFY=FALSE)

###
# try making a function with it
newfunc <- function(k){
  dat <- lapply(k$X_id, function(x){
    personal_id <- x
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result
    return(dat)
  })
  
  dfs <- lapply(dat, function(x){
    data.frame(as.list(unlist(x)))})
  output <- rbind.fill(lapply(dfs, as.data.frame))
  return(output)
}

newfunc(obje)

###
get_additional_info <- function(k){
  info <- lapply(k$X_id, function(x){
    personal_id <- x
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result
    json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
    out2 <- jsonlite::fromJSON(json2)
    r2 <- POST(url, body = out2, encode = 'json')
    dat2 <- content(r2, type="application/json")$result
    output <- list(dat, dat2)
    return(output)
  })
  return(info)}

additional_info <- get_additional_info(obje)

##### 9/28
namel <- thenamelist[1:100, ]

extra_info <- pblapply(thenamelist$X_id, function(x){
  personal_id <- x
  url <- "https://database.namenlijst.be/api/v1"
  json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
  out <- jsonlite::fromJSON(json)
  r <- POST(url, body = out, encode = 'json')
  dat <- content(r, type="application/json")$result
  json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
  out2 <- jsonlite::fromJSON(json2)
  r2 <- POST(url, body = out2, encode = 'json')
  dat2 <- content(r2, type="application/json")$result
  output <- list(dat, dat2)
  return(output)
})

### accounting for error
dat_namelist_additional <- NULL
i <- 1
extra_info <- NULL
while (i < 401) {
  print(paste(i, "to", i+99))
  extra_info <- pblapply(thenamelist$X_id[i:(i+99)], function(x){
    personal_id <- x
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result
    json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
    out2 <- jsonlite::fromJSON(json2)
    r2 <- POST(url, body = out2, encode = 'json')
    dat2 <- content(r2, type="application/json")$result
    output <- list(dat, dat2)
    return(output)
  })
dat_namelist_additional <- rbind(dat_namelist_additional, extra_info)
  i = i+100
}


##########
cl <- makeCluster(2) # for parallel processing
dat_namelist_additional <- NULL
i <- 1
extra_info <- NULL
while (i < length(thenamelist$X_id)) {
  print(paste(i, "to", i+9999))
  tryCatch({
  extra_info <- pblapply(thenamelist$X_id[i:(i+9999)], function(x){
    personal_id <- x
    url <- "https://database.namenlijst.be/api/v1"
    json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
    out <- jsonlite::fromJSON(json)
    r <- POST(url, body = out, encode = 'json')
    dat <- content(r, type="application/json")$result
    json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
    out2 <- jsonlite::fromJSON(json2)
    r2 <- POST(url, body = out2, encode = 'json')
    dat2 <- content(r2, type="application/json")$result
    output <- list(dat, dat2)
    return(output)
  }, cl=2)
  }, error=function(e){
    print("error")
    Sys.sleep(sample(60:180, 1))
    extra_info <- pblapply(thenamelist$X_id[i:(i+9999)], function(x){
      personal_id <- x
      url <- "https://database.namenlijst.be/api/v1"
      json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
      out <- jsonlite::fromJSON(json)
      r <- POST(url, body = out, encode = 'json')
      dat <- content(r, type="application/json")$result
      json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
      out2 <- jsonlite::fromJSON(json2)
      r2 <- POST(url, body = out2, encode = 'json')
      dat2 <- content(r2, type="application/json")$result
      output <- list(dat, dat2)
      return(output)
    }, cl=2)})
  dat_namelist_additional <- rbind(dat_namelist_additional, extra_info)
  i = i+10000
}

###Without Trycatch##
#cl <- makeCluster(3) # for parallel processing
#dat_namelist_additional <- NULL
i <- 100001
extra_info <- NULL
while (i < 200001) {
  print(paste(i, "to", i+9999))
    extra_info <- pblapply(thenamelist$X_id[i:(i+9999)], function(x){
      personal_id <- x
      url <- "https://database.namenlijst.be/api/v1"
      json <- paste('{"jsonrpc":"2.0","method":"getPerson","id":1,"params":{"_id":"', personal_id,'","options":["EXTEND_DOCUMENTS","EXTEND_STORIES","EXTEND_REGION","EXTEND_RELATIONS"],"token":"GUEST"}}', sep="")
      out <- jsonlite::fromJSON(json)
      r <- POST(url, body = out, encode = 'json')
      dat <- content(r, type="application/json")$result
      json2 <- paste('{"jsonrpc":"2.0","method":"findEvent","id":1,"params":{"document":{"person_id":"', personal_id,'"},"options":["EXTEND_PLACE","EXTEND_MEMORIAL"],"token":"GUEST"}}', sep="")
      out2 <- jsonlite::fromJSON(json2)
      r2 <- POST(url, body = out2, encode = 'json')
      dat2 <- content(r2, type="application/json")$result
      output <- list(dat, dat2)
      return(output)
    }, cl=3)
  dat_namelist_additional <- rbind(dat_namelist_additional, extra_info)
  i = i+10000
}
#save(dat_namelist_additional, file="thenamelist_additional.Rda")
########