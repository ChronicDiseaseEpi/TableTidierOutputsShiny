library(tidyverse)
library(jsonlite)
library(rrapply)

## This is a set of functions to read in the json files produced by 
## tabletidier, and work with the resultant objects

## Functions ----
## read collection from json file into R as list objects
ReadCollection <- function(collection_name) {
  ## reads each table and names it according to the tid
  b <- read_json(collection_name, simplifyVector = FALSE)[[1]]
  a <- ConvertInfo(b)
  print(a)
  names(b) <- paste0("TID", a$tid)
  b
}

## Pull terminology for a selected table into a dataframe
ConvertTerminology <- function(mytbl) {
  jsonlite:::simplifyDataFrame(mytbl$metadata, flatten = FALSE)
}

## Pull data for a selected table into a dataframe
ConvertData <- function(mytbl) {
  jsonlite:::simplifyDataFrame(mytbl$tableResults, flatten = FALSE)
}

## Pull info for a selected table into a dataframe
ConvertInfo <- function(collection) {
  map(collection, ~ .x[c("tid", "docid", "page", "collection_id", "doi", "pmid", "url")] %>% as_tibble()) %>% 
    bind_rows()
}

ConvertNotes <- function(collection) {
  map(collection, ~ .x$annotations %>% as_tibble()) %>% 
    bind_rows(.id = "tid")
}

## Get CUI row
GetCuisRows <- function(mytbl, rows, cols, chrctrs) {
  res <- pmap_int(list(rows, cols, chrctrs),
                  function(ro, co, ch){
                    a <- mytbl$posiMapper[[as.character(co)]][[as.character(ro)]][[as.character(ch)]]
                    if(is.null(a)) NA_integer_ else a
                  })
  res
}

## Pull data for a selected table into a dataframe along with selected terminology
# can take a vector of columns want to pull across from terminology
# default is cuis_selected
ConvertDataTerm <- function(mytbl, term_info = "cuis_selected") {
  mydf <- ConvertData(mytbl)
  term <- ConvertTerminology(mytbl)
    if (nrow(term) == 0L) return(mydf)
    for(i in setdiff(names(mydf), 
                     c("row", "col", "value"))) {
        indx <- GetCuisRows(mytbl, mydf$row, mydf$col, mydf[[i]])
        for(j in term_info){
          mydf[, paste0(i, "_", j)] <- term[[j]][indx+1]
        }
    }
    mydf
}

## Get concept IDs for a selected table into for any set of rows and columns
GetCuis <- function(mytbl, row, col) {
  dta <- ConvertData(mytbl)
  print(dta[dta$row == row & dta$col == col , ])
  terminology <- ConvertTerminology(mytbl)
  x <- mytbl$posiMapper[[as.character(col)]][[as.character(row)]]
  print(x)
  terminology[unlist(x) + 1 , ]
}
## Search terminology using either text or tables
SearchConcepts <- function(mytbl,
                           cuis = NA_character_, 
                           cutext = NA_character_) {
  terminology <- ConvertTerminology(mytbl)
  if(!all(is.na(cuis))) {
    a <-  which(terminology$cuis_selected %>% str_detect(cuis)) 
  } else a <- vector(length = 0)
  if(!all(is.na(cutext))) {
    b1 <-  which(terminology$concept %>% str_detect(cutext))
    b2 <-  which(terminology$concept_source %>% str_detect(cutext))
    b3 <-  which(terminology$concept_root %>% str_detect(cutext))
  b <- union(union(b1, b2), b3)
} else b <- vector(length = 0)
union(a, b) - 1
}

## Get data rows and columns a given search. Uses output of SearchConcepts
GetDataRowsCols <- function(mytbl,
                    conceptrows) {
  a  <- rrapply(mytbl$posiMapper, condition = function(x) x %in% conceptrows, how = "melt")
  names(a) <- c("col", "row", "text", "conceptrow")
  a$row <- as.integer(a$row)
  a$col <- as.integer(a$col)
  a
}


## Examples ----
runexamples <- function() {
# Two functions ReadCollection and ConvertInfo work at the collection level
# the rest work at the level of individual tables
## click on raw json file in Rstudio and view
## Read in a collection
clctn <- ReadCollection("collection_132_all.json")
## Get the information for a collection
ConvertInfo(clctn)

## Convert notes into dataframe for an individual table or collection
ConvertNotes(clctn)

## The remaining functions are designed to work on a single table
## As with any R function, they can be applied to all the tables in a collection
## via map faily of functions (or the base R equivalents such as lapply)

## Convert terminology into dataframe for an individual table or collection
ConvertTerminology(clctn$TID12078)

## Convert data into dataframe for an individual table or collection
a <- ConvertData(clctn$TID12078)

## Get the CUIs (concept IDs for a selected table for any given row and column)
ConvertData(clctn$TID12078) %>% 
  filter(row == 18)
GetCuis(clctn$TID12078, row = 18, col = 2)
GetCuis(clctn$TID12078, row = 18, col = 3)

## Search the concepts (concept IDs or original text for a selected table)
SearchConcepts(clctn$TID12078, cuis = "C0001779", cutext = "Age")

# Get relevant rows and columns from a data table, usually after doing a search and can pull relevant 
# data from tables
res <- SearchConcepts(clctn$TID12078, cutext = "Age")
res_r_c <- GetDataRowsCols(clctn$TID12078, res)
mydf <- ConvertData(clctn$TID12078)
mydf %>% 
  semi_join(res_r_c)

# note that it is easy to loop over a collection using map
clctn_plc <- map(clctn, function(each_tbl){
  res <- SearchConcepts(each_tbl, cutext = "Placebo")
res_r_c <- GetDataRowsCols(each_tbl, res)
mydf <- ConvertData(each_tbl)
mydf %>% 
  semi_join(res_r_c)
})

## Read in collection with partially completed tables
clctn2 <- ReadCollection("collection_132_all_additional.json")
terminology <- map(clctn2, ConvertTerminology)
## note no terminology (as expected) in table 1
map_int(terminology, nrow)
terminology <- bind_rows(terminology, .id = "tid")
mydata <- map(clctn2, ConvertData)
map_int(mydata, nrow)
mydata %>% names()
}
