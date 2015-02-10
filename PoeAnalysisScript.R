install.packages("wordcloud")
library(tm)
library(wordcloud)

stopWords <- stopwords("en")

urlV <- c("https://www.gutenberg.org/cache/epub/2147/pg2147.txt",
          "https://www.gutenberg.org/cache/epub/2148/pg2148.txt",
          "https://www.gutenberg.org/cache/epub/2149/pg2149.txt",
          "https://www.gutenberg.org/cache/epub/2150/pg2150.txt",
          "https://www.gutenberg.org/cache/epub/2151/pg2151.txt")
urlPoems <- "https://www.gutenberg.org/cache/epub/10031/pg10031.txt"

poev1text <- read_data("PoeV1",urlV[1])

tittlesV1 <- c("THE UNPARALLELED ADVENTURES OF ONE HANS PFAAL",
               "THE GOLD-BUG",
               "FOUR BEASTS IN ONE--THE HOMO-CAMELEOPARD",
               "THE MURDERS IN THE RUE MORGUE",
               "THE MYSTERY OF MARIE ROGET",
               "THE BALLOON-HOAX",
               "MS. FOUND IN A BOTTLE",
               "THE OVAL PORTRAIT",
               "End of Project Gutenberg")

splittingV1 <- parse_tales(poev1text,tittlesV1)
textNoPunct <- gsub("[[:blank:][:punct:]+]", " ", splittingV1[1])
textlow <- tolower(textNoPunct)
'%nin%' <- Negate('%in%')

textWords <- unlist(strsplit(textlow, " "))

textWords <- textWords[textWords %nin% c(stopWords,"")]

wordcloud(textWords,min.freq=2,max.words=70, random.order=T, rot.per=.15,  vfont=c("sans serif","plain"))

substr(splittingV1[1], 1, 2000)
substr(textNoPunct, 1, 2000)

##FUNCTIONS

#Function to download the data -------------------------------------------
read_data <- function(fileName, source_url) {  
  if (!file.exists(fileName)) {
    download.file(source_url, destfile = fileName, method = "curl")
  }
  text <- readLines(fileName,encoding="UTF-8")
  text <- paste(text, collapse=" ")
}

#Function to parse tales -------------------------------------------------
parse_tales <- function(text, titles) {  
  tales <- vector()
  for(title in titles){
    spl <- unlist(strsplit(text,title))
    tales <- c(tales,spl[1])
    text <- spl[2]
  }
  tales[-1] #drop first element
}