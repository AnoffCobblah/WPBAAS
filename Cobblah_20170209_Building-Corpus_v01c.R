#based on this tutorial: http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
#TO DO: Before beginning, choose the directory folders where you will be storing and
  #writing data (datalocation and outputlocation).
rm(list=ls())
datalocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/"
outputlocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output-Texts/"
#install.packages("tm") #only need to do once
#install.packages("SnowballC") #only do once
library(tm)
library(SnowballC)
files <- list.files(path = datalocation, 
                    pattern = "pdf$", full.names = TRUE) #creates vector of pdf file names.
#This is where things get weird.  You have to download an external pdf
#engine in order to use "tm"s "readPDF" function.  I went with "xpdf",
#available here.  I downloaded the precompiled binaries, and installed it.
Rpdf <- readPDF(control = list(text = "-layout"))
datacorpus <- Corpus(URISource(files),
                   readerControl = list(reader = Rpdf))
writeCorpus(datacorpus, path = outputlocation) #Note that you can include a 
  #"filenames = NULL" (or replace NULL with whatever), but that by not doing this,
  #you are telling it to keep the file names as they are.
txtfiles <- list.files(path = outputlocation, 
                    pattern = "txt", full.names = TRUE)
sapply(txtfiles,FUN=function(eachPath){
  file.rename(from=eachPath,to=sub(pattern=".pdf",replacement="",eachPath))
})
txtfiles <- list.files(path = outputlocation, 
                       pattern = "txt", full.names = TRUE)
data.tdm <- TermDocumentMatrix(datacorpus,control = list(removePunctuation = TRUE,
                                                           stopwords = TRUE,
                                                           tolower = TRUE,
                                                           stemming = TRUE,
                                                           removeNumbers = TRUE, #CHANGE IF NECESSARY
                                                           bounds = list(global= c(1,Inf)) #CHANGE IF NECESSARY.  THIS MEANS ANY WORDS THAT APPEAR LESS THAN 3 TIMES WON'T COUNT.
                                                           ))
inspect(data.tdm[1:10,]) #The result should be a list of the terms in the texts,
#with numbers next to them showing how many times the words show up.
