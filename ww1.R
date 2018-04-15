########## Set Up ########## 
# Set working directory
setwd("~/rbraun/belgian")

# Load packages
packages <- c("xml2","rvest", "dplyr", "ggplot2", "SnowballC", "lubridate", "stringr", "httr", "RSelenium", "XML", "wdman")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)
