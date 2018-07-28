#To do: This script relies on an excel sheet of gender terms.  Any changes made to that sheet can throw it off.
  #MAKE SURE ALL TERMS ARE LOWER CASE.
#TO DO: Before beginning, choose the directory folders where you will be storing and writing data (datalocation and outputlocation).
#to do: install.packages("RColorBrewer") #only do once

datalocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output-Texts/"
outputlocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output/"
searchterms <- read.csv("D:/OneDrive/Data Mining/R-Scripts/searchterms/genderterms.csv", as.is=TRUE)
library(RColorBrewer)
#Here I begin to set up my counts matrix to summarize my data.
counts <- matrix(,nrow=length(colnames(searchterms)),ncol=4)
counts[,1] <- colnames(searchterms)
colnames(counts) <- c("Category", "Total References in Corpus", "# of Texts in which Category Appears", "% of Texts in which Category Appears")
rm(freq) #Need to start with no "freq" object.
#Since there may be more than one category of search term, we'll need a loop.
#Here I begin to calculate my data
for (i in 1:length(colnames(searchterms))) {
  temp <- subset(searchterms, searchterms[,i] != "")
  temp <- as.vector(temp[,i])
  tempcount <- inspect(DocumentTermMatrix(datacorpus, control = list(removePunctuation = TRUE,
                                                                     removeNumbers = TRUE,
                                                                     stopwords = FALSE,
                                                                     tolower = TRUE,
                                                                     stemming = FALSE,
                                                                     dictionary = temp, 
                                                                     bounds = list(global= c(1,Inf)))))
  assign(paste0("Count", colnames(searchterms)[i]), tempcount) #to do: change to "Count"
  
  #Below is a table to calculate the Total Counts for the entire corpus
  temprefs <- apply(tempcount,1,sum)
  counts[i,2] <- sum(temprefs)
  tempint <- 0
  for(j in 1:length(temprefs)) {
    if(temprefs[j]>0) {
      tempint <- tempint+1 
    }
  }
  counts[i,3] <- tempint
  counts[i,4] <- (tempint / nrow(tempcount)) *100
  
  #Then I add in my data for my frequency of terms plot.  I need to only create
  #"freq" if it does not already exist.
  if(exists("freq") == FALSE) {
    freq <- matrix(,nrow = nrow(tempcount), ncol = 2+length(colnames(searchterms)))
    colnames(freq) <- c("Text", "ID #", colnames(searchterms))
    freq[,1] <- 1:nrow(tempcount)
    freq[,2] <- rownames(tempcount)
    freq[,-(1:2)] <- 0
    freq[,i+2] <- temprefs
  } else {
    freq[,i+2] <- temprefs
  }
}
freq <- t(freq)
dev.off()
pdf(file = file.path(outputlocation,"Gender-Wordsearch.pdf"))
#Can also do jpgs, windows files, postscripts, etc.
m <- rbind(c(1, 2, 3), c(4, 4, 5))
layout(m)
par(mar = c(3, 3, 3, 3))
Plt1 <- barplot(as.numeric(counts[,2]), main= colnames(counts)[2], names.arg=c(counts[,1]), xlab = "Category", ylab = "# of References", cex=.5, cex.axis=.5, cex.main=.7, cex.sub=.5)
Plt2 <- barplot(as.numeric(counts[,3]), main= colnames(counts)[3], names.arg=c(counts[,1]), xlab = "Category", ylab = "# of Texts", cex=.5, cex.axis=.5, cex.main=.7, cex.sub=.5)
Plt3 <- barplot(as.numeric(counts[,4]), main= colnames(counts)[4], names.arg=c(counts[,1]), xlab = "Category", ylab = "% of Texts", cex=.5, cex.axis=.5, cex.main=.7, cex.sub=.5)
brewer.pal(n = nrow(freq), name = "Set2")
palette(brewer.pal(n = nrow(freq), name = "Set2"))
if(nrow(freq)>20) {
  sampleindex <- c(1, sample(2:(ncol(freq)-1),18),ncol(freq))
  sampleindex <- sampleindex[order(sampleindex)]
  Plt4 <- barplot(freq[3:nrow(freq),sampleindex], main="Randomly Sampled Appearance of Terms",
                  xlab="Text", ylab="# of References", col = palette()[3:nrow(freq)],
                  legend = rownames(freq[3:nrow(freq),]), names.arg=c(freq[1,sampleindex]))
  Legend2 <- data.frame(freq[1:2,sampleindex])
} else {
  Plt4 <- barplot(freq[3:nrow(freq),], main="Appearances of Terms",
                  xlab="Text", ylab="# of References", col = palette()[3:nrow(freq)],
                  legend = rownames(freq[3:nrow(freq),]), names.arg=c(freq[1,]))
  Legend2 <- data.frame(freq[1:2,])
}
plot(1, 1, type = "n", xaxt="n", yaxt="n", bty="n")
Legend2 <- t(Legend2)
nametemp <- paste(Legend2[,1],Legend2[,2], sep=" -- ")
Legend2 <- nametemp
title(Legend2, line = -10, cex.main=.5)
dev.off()
counts