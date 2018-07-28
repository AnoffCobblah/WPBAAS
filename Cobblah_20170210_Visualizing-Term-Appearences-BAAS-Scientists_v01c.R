
#########################################ENTIRE-CORPUS############################
rm(list=ls())
#TO DO: change datalocation and outputlocation
#NOTE: TEXTS IN THE datalocation CANNOT CONTAIN "+" OR "()".  THESE DO NOT WORK WELL WITH GREP.  WILL CAUSE DROPS.
#####################DISTANT READING ######################################################
#####################################################################################################
datalocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output-Texts/"
outputlocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output/"
files <- list.files(path = datalocation, 
                    pattern = "txt", full.names = TRUE) #creates vector of txt file names.

tok1 <- "coreNLP"
tok2 <- "tokenizers"
toggletoken <- tok2
searchedtermlist <- c("darwin", "newton", "lyell", "maxwell", "huxley", "joule", "tyndall")
if (toggletoken == tok1) {
  library(coreNLP)
  initCoreNLP()
}
if (toggletoken == tok2) {
  #install.packages("tokenizers") #Only once
  library(tokenizers)
  #install.packages("readr")  #only once
  library(readr)
}
sentences <- list()
lemma <- list()
sentences_perc <- list()
lemma_perc <- list()
data <- matrix(,ncol=12,nrow=1)
tempdata <- matrix(,ncol=12,nrow=1)
colnames(data) <- c("Text","Text_ID", "searchedterm","Lemma","Sentence","Lemma_Perc","Sentence_Perc","searchedterm_ID","category","date","author","Lemma_Length")
outputGraphics <- list()
for (p in 1:length(searchedtermlist)) {
  iter <- 1
  iterp <- 1
  iterpw <- 1
  searchedterm <- searchedtermlist[p]
  tempcategory <- searchedtermlist[p]
  for (i in 1:length(files)) {
    nametemp <- strsplit(files[i],"_")
    tempauthor <- "n.a." #if date is wrong, file path might have a "_" somewhere in it.  Need to be careful.
    nametemp2 <- strsplit(nametemp[[1]][1],"/")
    tempdate <- nametemp2[[1]][6]
    if (toggletoken == tok1) {
      anno <- annotateFile(files[i]) 
      token <- getToken(anno)
      for (j in 1:nrow(token)) {
        #NOTE: I chose to search for the Appearances of the exact word ([j,3]).  A more
        #general method would be to search for appearances of the lemma ([j,4]).
        if(token[j,3] == searchedterm) {
          sentences[[iterp]] <- token[j,1]
          lemma[[iterp]] <- j
          iterp <- iterp+1
        }
      }
      for (k in 1:length(lemma)) {
        sentences_perc[[k]] <- (sentences[[k]] / token[nrow(token),1]) *100
        lemma_perc[[k]] <- (lemma[[k]] / nrow(token)) *100
      }
    }
    if (toggletoken == tok2) {
      fileName <- read_file(files[i])
      #since tokenize_sentences function requires things to be encoded in UTF-8, need to remove some data.
      Encoding(fileName) <- "UTF-8"
      fileName <- iconv(fileName, "UTF-8", "UTF-8",sub='')
      ltoken <- tokenize_words(fileName, lowercase = TRUE, stopwords = NULL, simplify = FALSE)
      stoken <- tokenize_sentences(fileName, lowercase = FALSE, strip_punctuation = FALSE, simplify = FALSE)
      ltoken <- unlist(ltoken)
      stoken <- unlist(stoken)
      for (w in 1:length(ltoken)) {
        #NOTE: I chose to search for the Appearances of the exact word ([j,3]).  A more
        #general method would be to search for appearances of the lemma ([j,4]).
        if(ltoken[w] == searchedterm) {
          lemma[[iterp]] <- w
          iterp <- iterp+1
        }
      }
      for (z in 1:length(stoken)) {
        if(grepl(searchedterm, stoken[z], ignore.case = TRUE)) {
          sentences[[iterpw]] <- z
          iterpw <- iterpw+1
        }
      }
      if (length(lemma) != 0){
        for (k in 1:length(lemma)) {
          lemma_perc[[k]] <- (lemma[[k]] / length(ltoken)) *100
        }
      }
      if (length(sentences) != 0) {
        for (g in 1:length(sentences)) {
          sentences_perc[[g]] <- (sentences[[g]] / length(stoken)) *100
        }
      }
    }
    lemma <- unlist(lemma)
    if (length(lemma) == 0) {
      lemma <- NA
    }
    sentences <- unlist(sentences)
    if (length(sentences) == 0) {
      sentences <- NA
    }
    lemma_perc <- unlist(lemma_perc)
    if (length(lemma_perc) == 0) {
      lemma_perc <- NA
    }
    sentences_perc <- unlist(sentences_perc)
    if (length(sentences_perc) == 0) {
      sentences_perc <- NA
    }
    if (length(lemma) > length(sentences)) {
      mat <- matrix(,ncol=12,nrow=length(lemma))
      mat[,1] <- files[i]
      mat[,2] <- i
      mat[,3] <- searchedterm
      mat[,4] <- lemma
      mat[1:length(sentences),5] <- sentences
      mat[(length(sentences)+1):length(lemma),5] <- NA
      mat[,6] <- lemma_perc
      mat[1:length(sentences),7] <- sentences_perc
      mat[(length(sentences)+1):length(lemma),7] <- NA
      mat[,8] <- p
      mat[,9] <- tempcategory
      mat[,10] <- tempdate
      mat[,11] <- tempauthor
      mat[,12] <- length(ltoken)
    }
    if (length(lemma) < length(sentences)) {
      mat <- matrix(,ncol=12,nrow=length(sentences))
      mat[,1] <- files[i]
      mat[,2] <- i
      mat[,3] <- searchedterm
      mat[1:length(lemma),4] <- lemma
      mat[(length(lemma)+1):length(sentences),4] <- NA
      mat[,5] <- sentences
      mat[1:length(lemma_perc),6] <- lemma_perc
      mat[(length(lemma_perc)+1):length(sentences),6] <- NA
      mat[,7] <- sentences_perc
      mat[,8] <- p
      mat[,9] <- tempcategory
      mat[,10] <- tempdate
      mat[,11] <- tempauthor
      mat[,12] <- length(ltoken)
    }
    if (length(lemma) == length(sentences)) {
      mat <- matrix(,ncol=12,nrow=length(sentences))
      mat[,1] <- files[i]
      mat[,2] <- i
      mat[,3] <- searchedterm
      mat[,4] <- lemma
      mat[,5] <- sentences
      mat[,6] <- lemma_perc
      mat[,7] <- sentences_perc
      mat[,8] <- p
      mat[,9] <- tempcategory
      mat[,10] <- tempdate
      mat[,11] <- tempauthor
      mat[,12] <- length(ltoken)
    }
    tempdata <- rbind(tempdata,mat)
    sentences <- list()
    lemma <- list()
    sentences_perc <- list()
    lemma_perc <- list()
  }
  tempdata <- tempdata[-1,]
  data <- rbind(data,tempdata)
}
data <- data[-1,]
datadf <- as.data.frame(data)
dup <- matrix(,ncol=12,nrow=1)
for (o in 1:length(files)) {
  dupdf <- datadf[grep(files[o],datadf$Text),]
  dupdf <- dupdf[complete.cases(dupdf$Lemma),] #my data method creates a lot of duplicates and empty rows.  These steps erase those.
  dupdf <- dupdf[!duplicated(dupdf$Lemma),]
  dupmat <- as.matrix(dupdf)
  dup <- rbind(dup,dupmat)
}
dup <- dup[-1,]
datadf <- as.data.frame(dup)

#Plotting and data output begin below

write.table(datadf, file.path(outputlocation,"scientist-datadf"))

#Scientist averages
compsci <- matrix(,ncol=9,nrow=1)
for (a in 1:length(searchedtermlist)) {
  fuckit <- datadf[grep(searchedtermlist[a],datadf$searchedterm),]
  analmat1 <- matrix(,ncol=9, nrow=1)
  colnames(analmat1) <- c("date", "Avg._Lemma_Perc", "Avg._Lemma_Perc", "Freq","scientist","freq_per_lemma","NA","Total_Lemma","NA")
  for (m in 1833:1900) {
    analtempmat1 <- matrix(,ncol=9,nrow=1)
    analtempmat1[,1] <- m
    analtempindex1 <- grep(m,fuckit$date)
    analtempdatadf1 <- fuckit[analtempindex1,]
    analtempmat1[,2] <- mean(as.numeric(as.character(analtempdatadf1$Lemma_Perc)), na.rm=TRUE)
    analtempmat1[,5] <- searchedtermlist[a]
    analtempmat1[,4] <- sum(analtempdatadf1$Lemma_Perc[complete.cases(analtempdatadf1$Lemma_Perc)] != "")
    analtempmat1[,6] <- as.numeric(as.character(analtempmat1[,4]))/as.numeric(as.character(analtempdatadf1$Lemma_Length[2]))*100
    analtempmat1[,8] <- sum(as.numeric(as.character(unique(analtempdatadf1$Lemma_Length)))) 
    analmat1 <- rbind(analmat1, analtempmat1)
  }
  analmat1 <- analmat1[-1,]
  compsci <- rbind(compsci,analmat1)
}
compsci <- compsci[-1,]
compscidf <- as.data.frame(compsci)
compscidf <- compscidf[complete.cases(compscidf$freq_per_lemma),]
compscidf <- compscidf[,c(1,5,6)]
darwin <- compscidf[grep("darwin",compscidf$scientist),]
newton <- compscidf[grep("newton",compscidf$scientist),]
lyell <- compscidf[grep("lyell",compscidf$scientist),]
maxwell<- compscidf[grep("maxwell",compscidf$scientist),]
huxley <- compscidf[grep("huxley",compscidf$scientist),]
joule <- compscidf[grep("joule",compscidf$scientist),]
tyndall <- compscidf[grep("tyndall",compscidf$scientist),]


dev.off()
pdf(file = file.path(outputlocation,"HuxleyvTyndall.pdf"))
library(ggplot2)
p <- ggplot(NULL, aes(size=.5)) + geom_line(data = tyndall, aes(y = as.numeric(as.character(freq_per_lemma)), x = as.numeric(as.character(date)), color=scientist)) + geom_line(data = huxley, aes(y = as.numeric(as.character(freq_per_lemma)), x = as.numeric(as.character(date)), color=scientist))
pl <- p +labs(x = "Date", y = "Frequency / Lemma that Year", title = "Average Appearances of Scientists in Corpus")  
pl
dev.off()
