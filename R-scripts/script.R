#----------------------------------------------#
# Economics Bulletin  - European Central Bank  #
#------Download Multiple PDFS files------------#


# Download 2021 pdfs -----------------------------------------------------------


## Packages ----------------------------------------------------------------

library("pdftools")
library("glue")
library("tidyverse")
library("here")


## Set year and month  ----------------------------------------------------------------

year <- as.character(2021)
id<- c("02","03","04","05")

## Creates a String of the URL Addresses -----------------------------------

urls <- 
  tidyr::expand_grid(year, id) %>%
  glue_data("https://www.ecb.europa.eu/pub/pdf/ecbu/eb{year}{id}.en.pdf")


## Creates Names for the PDF Files -----------------------------------------

pdf_names <- 
  tidyr:: expand_grid(year, id) %>% 
  glue_data("eb-{year}-{id}.pdf")

## Download PDFS ----------------------------------------------------------------

safe_download <- safely(~ download.file(.x ,.y, mode = "wb"))
walk2(urls, pdf_names, safe_download)


# Merge PDFS  ---------------------------------------------------------------

## Packages -----------------------------------------------------

library(staplr)
library(here)

## Merge multiple downloaded pdfs into one ##
staple_pdf(
  input_directory = "./", #specify the original path with the pdf files 
  input_files = NULL,
  output_filepath = "./docs/merged.pdf", # saving the merged pdf 
  overwrite = TRUE
)




# WordCloud ---------------------------------------------------------------

# Installing necessary packages -- already done
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("readtext")

## Loading packages -------------------------------------------------------

library(tm)
library(SnowballC)
library(wordcloud)
library(readtext)
library(here)
library(RColorBrewer)

here() # working directory from the R Project

wordbase <- readtext(here("docs", "merged.pdf")) # relative path

# “wordbase” is a variable I’m creating to hold the text from the PDF.
# The variable is actually a data frame (data.frame) with two columns and one row. 
# The first column is the document ID (e.g., “paper.pdf”); the second column is the extracted text


corp <- Corpus(VectorSource(wordbase)) 

## Cleaning the Text with the tm package --------------------------------------------------------


corp <- tm_map(corp, PlainTextDocument)

corp <- tm_map(corp, removePunctuation) # remove punctuation

corp <- tm_map(corp, removeNumbers) 

corp <- tm_map(corp, tolower) # all letters in lowercase
# This is important! Inflation vs inflation -- automatically, it will be converted to inflation 


corp <- tm_map(corp, removeWords, stopwords(kind = "en"))
# ignore words from the english idiom such as  "The", "a","an"


## Removing unwanted words from the cloud -->

corp <- tm_map(corp, removeWords, c("billion","march","also", "period", 
                                    "due", "reached", "months","since",
                                    "may", "month", "previous", "will", "which", "from",
                                    "december", "data", "june", "still", "-", "this","than", "july", "well",
                                    "one",  "january", "based", "current", "quarter", "main",
                                    "last", "september", "however", "important", "grew", "report",
                                    "compared", "scenario", "recent", "quarters", "november","april",
                                    "february","august", "october", "fact","hand","according","_",
                                    "years", "twelvemonth", "despite", "committee", "pace", "copom",
                                    "second", "strong", "regarding", "respectively","even",
                                    "third", "first", "-", "year", "twelve", "two", "index",
                                    "projections", "registered","effects", "new", "less", "next",
                                    "coming", "already","although", "continue", "observed", "figure",
                                    "assumptions", "associated", "economy","average","though",
                                    "comparison","relation","three","forecasts", "especially", "end",
                                    "fourth", "following", "path", "meeting", "the", "and","for", "with",
                                    "that", "are", "was","have","but","such","annual"))
                                      

## Deleting symbols from the PDFS  -----------------------------------------------

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corp <- tm_map(corp, toSpace, "€")
corp <- tm_map(corp, toSpace, "—")
corp <- tm_map(corp, toSpace, "–")
corp <- tm_map(corp, toSpace, "-")


## Create wordcloud and save png ---------------------------------------------

# Vamos usar a paleta de cores do Pacote RColorBrewer:: vantagem: além do tamanho, também há a discriminação por cores de acordo com a frequência das palavras

color <- brewer.pal(8, "Dark2")
# 8  = max.number of colors
# Dark2 -- palete
# Brewer.all is colorblindfriendly!! check display.brewer.all(colorblindFriendly = TRUE) to see all available options


png("wordcloud-R.png", width=900,height=800) 


wordcloud(corp, max.words = 120, random.order = FALSE, colors = color, scale=c(12, .6)) 
# specify the nº max. of words and the scale


dev.off() # saving the png in your R project directory according to the here()package

