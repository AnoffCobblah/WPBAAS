
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
searchedtermlist <- c("play","playful","playing","amuse","amusement","amusing","recreate","recreation","leisure","entertain","entertaining","diversion","pastime","game","spectacle","pleasure","sport","sporting","toy","fun","competition","compete","work","working","toil","labour","laborious","serious","earnest")
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
  if (p <= 22) {tempcategory <- "play"}
  if (p >= 23) {tempcategory <- "work"}
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

write.table(datadf, file.path(outputlocation,"play-work-datadf"))

#This chart compares the appearance of play and work terms across the corpus
#1. Scatterplot
playdf <- datadf[grep("play",datadf$category),]
workdf <- datadf[grep("work",datadf$category),]
tempplaydf <- cbind(playdf,paste(playdf$author,"play-terms", sep = "-"))
tempworkdf <- cbind(workdf,paste(workdf$author,"work-terms", sep = "-"))
colnames(tempplaydf)[13] <- "corpuslabel"
colnames(tempworkdf)[13] <- "corpuslabel"
corpusdatadf <- rbind(tempplaydf,tempworkdf)
dev.off()
pdf(file = file.path(outputlocation,"All-play-workoutput.pdf"))
library(ggplot2)
p <- ggplot(corpusdatadf, aes((y = as.factor(corpuslabel)), x = as.numeric(as.character(Lemma_Perc)), color = as.factor(category))) + geom_point(size=0.25,pch = 3)
pl <- p + labs(x = "% of Text (by lemma))", y = "Text Terms", title = "Appearances of Play and Work in the Corpus") + theme(text = element_text(size=3), legend.position = "bottom")
pl
dev.off()

#2. Play Binned Plots
histcorpusmat <- matrix(,nrow=1,ncol=9)
for (u in 1:length(files)) {
  blankmat <- matrix(,nrow=20,ncol=9)
  colnames(blankmat) <- c("author", "text", "Text_ID", "date", "bin", "bin_avg", "frequency","category", "freq_per_lemma")
  tempindex <- grep(files[u],playdf$Text)
  tempdf <- playdf[tempindex,]
  bins <- cut(as.numeric(as.character(tempdf$Lemma_Perc)),breaks=seq(0,100,by=5),labels=FALSE, include.lowest = TRUE)
  blankmat[,1] <- as.character(tempdf[2,11])
  blankmat[,2] <- as.character(tempdf[2,1])
  blankmat[,3] <- as.numeric(as.character(tempdf[2,2]))
  var <- droplevels(tempdf[2,10])
  for (k in 1:20) {blankmat[k,4] <- as.numeric(as.character((var)))}
  blankmat[,5] <- c("(0-5)","(5-10)", "(10-15)", "(15-20)", "(20-25)", "(25-30)", "(30-35)", "(35-40)", "(40-45)", "(45-50)", "(50-55)", "(55-60)", "(60-65)", "(65-70)", "(70-75)", "(75-80)", "(80-85)", "(85-90)", "(90-95)", "(95-100)")
  blankmat[,6] <- c(2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5)
  blankmat[,7] <- 0
  for (v in 1:20) {
    mat3 <- as.matrix(table(bins))
    vec3 <- as.numeric(rownames(mat3))
    blankmat[vec3[v],7] <- mat3[v]
  }
  blankmat[,8] <- "play"
  blankmat[,9] <- (as.numeric(as.character(blankmat[,7]))/as.numeric(as.character(tempdf$Lemma_Length[2])))*100 #All values in tempdf$Lemma_Length are the same, so it doesn't matter which cell is used
  histcorpusmat <- rbind(histcorpusmat,blankmat)
}
histcorpusmat <- histcorpusmat[-1,]
histcorpusdf <- as.data.frame(histcorpusmat)
histcorpusdf$bin <- factor(histcorpusdf$bin, levels=histcorpusdf$bin[order(as.numeric(as.character(histcorpusdf$bin_avg)))])
#Play binned, plotting absolute frequency
dev.off()
pdf(file = file.path(outputlocation,"All-play-Date-tile-freq.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(date)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(frequency)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Play in Corpus") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-play-Text-tile-freq.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(frequency)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Play in Corpus") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-play-Date-dot-freq.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(date)), x = as.factor(bin), size = (as.numeric(as.character(frequency))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Play in Corpus") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-play-Text-dot-freq.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin), size = (as.numeric(as.character(frequency))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Play in Corpus") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
#Play binned, plotting frequency / Lemma
#NOTE: it is too difficult, and not really worth it, to normalize a chart that will be arranged by date.  The problem is that ggplot 2
#tries to simply add the percentages together.  Too see how things change by date, look at the averages instead.
#dev.off()
#pdf(file = file.path(outputlocation,"All-play-Date-tile-freqperlemma.pdf"))
#library(ggplot2)
#p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(date)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red")
#pl <- p +labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Play in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
#pl
#dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-play-Text-tile-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Play in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()

#NOTE: it is too difficult, and not really worth it, to normalize a chart that will be arranged by date.  The problem is that ggplot 2
#tries to simply add the percentages together.  Too see how things change by date, look at the averages instead.
#dev.off()
#pdf(file = file.path(outputlocation,"All-play-Date-dot-freqperlemma.pdf"))
#library(ggplot2)
#p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(date)), x = as.factor(bin), size = (as.numeric(as.character(freq_per_lemma))), color = as.factor(category)))
#pl <- p + labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Play in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
#pl
#dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-play-Text-dot-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin), size = (as.numeric(as.character(freq_per_lemma))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Play in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()

#3. Work Binned Plots
histcorpusmat <- matrix(,nrow=1,ncol=9)
for (u in 1:length(files)) {
  blankmat <- matrix(,nrow=20,ncol=9)
  colnames(blankmat) <- c("author", "text", "Text_ID", "date", "bin", "bin_avg", "frequency","category","freq_per_lemma")
  tempindex <- grep(files[u],workdf$Text)
  tempdf <- workdf[tempindex,]
  tempdf <- tempdf[complete.cases(tempdf$Lemma_Perc),] #this step makes sure there are no NAs to throw off cutting.
  bins <- cut(as.numeric(as.character(tempdf$Lemma_Perc)),breaks=seq(0,100,by=5),labels=FALSE, include.lowest = TRUE)
  blankmat[,1] <- as.character(tempdf[2,11])
  blankmat[,2] <- as.character(tempdf[2,1])
  blankmat[,3] <- as.numeric(as.character(tempdf[2,2]))
  var <- droplevels(tempdf[2,10])
  for (k in 1:20) {blankmat[k,4] <- as.numeric(as.character((var)))}
  blankmat[,5] <- c("(0-5)","(5-10)", "(10-15)", "(15-20)", "(20-25)", "(25-30)", "(30-35)", "(35-40)", "(40-45)", "(45-50)", "(50-55)", "(55-60)", "(60-65)", "(65-70)", "(70-75)", "(75-80)", "(80-85)", "(85-90)", "(90-95)", "(95-100)")
  blankmat[,6] <- c(2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5)
  blankmat[,7] <- 0
  for (v in 1:20) {
    mat3 <- as.matrix(table(bins))
    vec3 <- as.numeric(rownames(mat3))
    blankmat[vec3[v],7] <- mat3[v]
  }
  blankmat[,8] <- "work"
  blankmat[,9] <- (as.numeric(as.character(blankmat[,7]))/as.numeric(as.character(tempdf$Lemma_Length[2])))*100 #All values in tempdf$Lemma_Length are the same, so it doesn't matter which cell is used
  histcorpusmat <- rbind(histcorpusmat,blankmat)
}
histcorpusmat <- histcorpusmat[-1,]
histcorpusdf <- as.data.frame(histcorpusmat)
histcorpusdf$bin <- factor(histcorpusdf$bin, levels=histcorpusdf$bin[order(as.numeric(as.character(histcorpusdf$bin_avg)))])
#Plotting by absolute frequency
dev.off()
pdf(file = file.path(outputlocation,"All-work-Date-tile-freq.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(date)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(frequency)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Work in Corpus") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-work-Text-tile-freq.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(frequency)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Work in Corpus") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-work-Date-dot-freq.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(date)), x = as.factor(bin), size = (as.numeric(as.character(frequency))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Work in Corpus") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-work-Text-dot-freq.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin), size = (as.numeric(as.character(frequency))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Text ID", title = "Appearances of Work in Corpus") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
#plotting by frequence / lemma
#NOTE: it is too difficult, and not really worth it, to normalize a chart that will be arranged by date.  The problem is that ggplot 2
#tries to simply add the percentages together.  Too see how things change by date, look at the averages instead.
#dev.off()
#pdf(file = file.path(outputlocation,"All-work-Date-tile-freqperlemma.pdf"))
#library(ggplot2)
#p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(date)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red")
#pl <- p +labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Work in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
#pl
#dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-work-Text-tile-freqperlemma.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histcorpusdf, aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Work in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()

#NOTE: it is too difficult, and not really worth it, to normalize a chart that will be arranged by date.  The problem is that ggplot 2
#tries to simply add the percentages together.  Too see how things change by date, look at the averages instead.
#dev.off()
#pdf(file = file.path(outputlocation,"All-work-Date-dot-freqperlemma.pdf"))
#library(ggplot2)
#library(RColorBrewer)
#p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(date)), x = as.factor(bin), size = (as.numeric(as.character(freq_per_lemma))), color = as.factor(category)))
#pl <- p + labs(x = "% of Text (by lemma)", y = "Date", title = "Appearances of Work in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
#pl
#dev.off()
dev.off()
pdf(file = file.path(outputlocation,"All-work-Text-dot-freqperlemma.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.numeric(as.character(Text_ID)), x = as.factor(bin), size = (as.numeric(as.character(freq_per_lemma))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Text_ID", title = "Appearances of Work in Corpus, Normalized by Number of Lemma") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()

#Histograms of play/work data show the shape of the appearance of play and work terms
#Play histograms
#Pt. 1
dev.off()
pdf(file = file.path(outputlocation,"All-Play-Histograms-Pt1.pdf"))
par(mfrow=c(4,5))
for (z in 1:20) {
  temphistdf <- playdf[grep(files[z],playdf$Text),] #Note that this is different than the others, because grep(1) would grab both 1 and 10,11, etc.
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Play' Density")
  lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()

#Pt. 2
dev.off()
pdf(file = file.path(outputlocation,"All-Play-Histograms-Pt2.pdf"))
par(mfrow=c(4,5))
for (z in 21:40) {
  temphistdf <- playdf[grep(z,playdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Play' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
#dev.off()

#Pt. 3
dev.off()
pdf(file = file.path(outputlocation,"All-Play-Histograms-Pt3.pdf"))
par(mfrow=c(4,5))
for (z in 41:60) {
  
  temphistdf <- playdf[grep(z,playdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Play' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Pt. 4
dev.off()
pdf(file = file.path(outputlocation,"All-Play-Histograms-Pt4.pdf"))
par(mfrow=c(4,5))
for (z in 61:80) {
  
  temphistdf <- playdf[grep(z,playdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Play' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Pt. 5
dev.off()
pdf(file = file.path(outputlocation,"All-Play-Histograms-Pt5.pdf"))
par(mfrow=c(4,5))
for (z in 81:length(files)) {
  temphistdf <- playdf[grep(z,playdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Play' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Work histograms
#Pt. 1
dev.off()
pdf(file = file.path(outputlocation,"All-Work-Histograms-Pt1.pdf"))
par(mfrow=c(4,5))
for (z in 1:20) {
  temphistdf <- workdf[grep(files[z],workdf$Text),] #Note that this is different than the others, because grep(1) would grab both 1 and 10,11, etc.
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Work' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Pt. 2
dev.off()
pdf(file = file.path(outputlocation,"All-Work-Histograms-Pt2.pdf"))
par(mfrow=c(4,5))
for (z in 21:40) {
  temphistdf <- workdf[grep(z,workdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Work' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Pt. 3
dev.off()
pdf(file = file.path(outputlocation,"All-Work-Histograms-Pt3.pdf"))
par(mfrow=c(4,5))
for (z in 41:60) {
  temphistdf <- workdf[grep(z,workdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Work' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Pt. 4
dev.off()
pdf(file = file.path(outputlocation,"All-Work-Histograms-Pt4.pdf"))
par(mfrow=c(4,5))
for (z in 61:80) {
  temphistdf <- workdf[grep(z,workdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Work' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()
#Pt. 5
dev.off()
pdf(file = file.path(outputlocation,"All-Work-Histograms-Pt5.pdf"))
par(mfrow=c(4,5))
for (z in 81:length(files)) {
  temphistdf <- workdf[grep(z,workdf$Text_ID),]
  breakPoints <- quantile(as.numeric(as.character(temphistdf$Lemma_Perc)), prob=seq(0,1,length.out=11),na.rm = TRUE, names=FALSE)
  
  hist(as.numeric(as.character(temphistdf$Lemma_Perc)), breaks=unique(breakPoints), main = z, xlab = "Lemma %", ylab = "'Work' Density")
  if(z %in% c(10,24,26,32,37,38,39,51,79,80,81,82,86)) {box(which = "figure", lty = "solid",lwd=6)}else{box(which = "figure", lty = "solid")};lines(density(as.numeric(as.character(temphistdf$Lemma_Perc))), col="red", lwd=2)
  }
dev.off()

#Averages
analmat1 <- matrix(,ncol=9, nrow=1)
colnames(analmat1) <- c("date", "Avg._Play_Lemma_Perc", "Avg._Work_Lemma_Perc", "Play_Freq","Work_Freq","play_freq_per_lemma","work_freq_per_lemma","Play_Total_Lemma","Work_Total_Lemma")
for (m in 1775:1960) {
  analtempmat1 <- matrix(,ncol=9,nrow=1)
  analtempmat1[,1] <- m
  analtempindex1 <- grep(m,playdf$date)
  analtempplaydf1 <- playdf[analtempindex1,]
  analtempmat1[,2] <- mean(as.numeric(as.character(analtempplaydf1$Lemma_Perc)), na.rm=TRUE)
  analtempindex2 <- grep(m,workdf$date)
  analtempworkdf1 <- workdf[analtempindex2,]
  analtempmat1[,3] <- mean(as.numeric(as.character(analtempworkdf1$Lemma_Perc)), na.rm=TRUE)
  analtempmat1[,4] <- sum(analtempplaydf1$Lemma_Perc[complete.cases(analtempplaydf1$Lemma_Perc)] != "")
  analtempmat1[,5] <- sum(analtempworkdf1$Lemma_Perc[complete.cases(analtempworkdf1$Lemma_Perc)] != "")
  analtempmat1[,6] <- as.numeric(as.character(analtempmat1[,4]))/as.numeric(as.character(analtempplaydf1$Lemma_Length[2]))*100
  analtempmat1[,7] <- as.numeric(as.character(analtempmat1[,5]))/as.numeric(as.character(analtempworkdf1$Lemma_Length[2]))*100
  analtempmat1[,8] <- sum(as.numeric(as.character(unique(analtempplaydf1$Lemma_Length)))) #adds all lemma for play texts that year
  analtempmat1[,9] <- sum(as.numeric(as.character(unique(analtempworkdf1$Lemma_Length)))) #adds all lemma for work texts that year
  analmat1 <- rbind(analmat1, analtempmat1)
}
analmat1 <- analmat1[-1,]
analdf1 <- as.data.frame(analmat1)
playavg1 <- mean(as.numeric(as.character(analdf1$Avg._Play_Lemma_Perc)), na.rm=TRUE)
playfreqavg1 <- mean(as.numeric(as.character(analdf1$Play_Freq)), na.rm=TRUE)
workavg1 <- mean(as.numeric(as.character(analdf1$Avg._Work_Lemma_Perc)), na.rm=TRUE)
workfreqavg1 <- mean(as.numeric(as.character(analdf1$Work_Freq)), na.rm=TRUE)
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Play-Date-freq.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Play_Lemma_Perc)), x = date, fill = as.numeric(as.character(Play_Freq)))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = playavg1) + labs(x = "Date", y = "Average % of Text (by lemma)", title = "Average Appearances of Play in Corpus") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Work-Date-freq.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Work_Lemma_Perc)), x = date, fill = as.numeric(as.character(Work_Freq)))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = workavg1) + labs(x = "Date", y = "Average % of Text (by lemma)", title = "Average Appearances of Work in Corpus") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Play-Date-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Play_Lemma_Perc)), x = date, fill = (as.numeric(as.character(Play_Freq))/as.numeric(as.character(Play_Total_Lemma))*100))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = playavg1) + labs(x = "Date", y = "Average % of Text (by lemma)", title = "Average Appearances of Play in Corpus, Normalized by Text Lemmas") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Work-Date-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Work_Lemma_Perc)), x = date, fill = (as.numeric(as.character(Work_Freq))/as.numeric(as.character(Work_Total_Lemma))*100))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = workavg1) + labs(x = "Date", y = "Average % of Text (by lemma)", title = "Average Appearances of Work in Corpus, Normalized by Text Lemmas") + theme(legend.title = element_blank())
pl
dev.off()

analmat2 <- matrix(,ncol=8, nrow=1)
colnames(analmat2) <- c("Text_ID", "Avg._Play_Lemma_Perc", "Avg._Work_Lemma_Perc", "Play_Freq","Work_Freq","play_freq_per_lemma","work_freq_per_lemma","Lemma_Length")
for (n in 1:length(files)) {
  analtempmat2 <- matrix(,ncol=8,nrow=1)
  analtempmat2[,1] <- n
  analtempindex3 <- grep(files[n],playdf$Text)
  analtempplaydf2 <- playdf[analtempindex3,]
  analtempmat2[,2] <- mean(as.numeric(as.character(analtempplaydf2$Lemma_Perc)), na.rm=TRUE)
  analtempindex4 <- grep(files[n],workdf$Text)
  analtempworkdf2 <- workdf[analtempindex4,]
  analtempmat2[,3] <- mean(as.numeric(as.character(analtempworkdf2$Lemma_Perc)), na.rm=TRUE)
  analtempmat2[,4] <- sum(analtempplaydf2$Lemma_Perc[complete.cases(analtempplaydf2$Lemma_Perc)] != "")
  analtempmat2[,5] <- sum(analtempworkdf2$Lemma_Perc[complete.cases(analtempworkdf2$Lemma_Perc)] != "")
  analtempmat2[,6] <- as.numeric(as.character(analtempmat2[,4]))/as.numeric(as.character(analtempplaydf2$Lemma_Length[2]))*100
  analtempmat2[,7] <- as.numeric(as.character(analtempmat2[,5]))/as.numeric(as.character(analtempworkdf2$Lemma_Length[2]))*100
  analtempmat2[,8] <- as.numeric(as.character(analtempworkdf2$Lemma_Length[2])) #might not be true lemma length, if text contains no work terms
  analmat2 <- rbind(analmat2, analtempmat2)
}
analmat2 <- analmat2[-1,]
analdf2 <- as.data.frame(analmat2)
playavg2 <- mean(as.numeric(as.character(analdf2$Avg._Play_Lemma_Perc)), na.rm=TRUE)
playfreqavg2 <- mean(as.numeric(as.character(analdf2$Play_Freq)), na.rm=TRUE)
workavg2 <- mean(as.numeric(as.character(analdf2$Avg._Work_Lemma_Perc)), na.rm=TRUE)
workfreqavg2 <- mean(as.numeric(as.character(analdf2$Work_Freq)), na.rm=TRUE)
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Play-Text-freq.pdf"))
library(ggplot2)
p <- ggplot(analdf2) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Play_Lemma_Perc)), x = Text_ID, fill = as.numeric(as.character(Play_Freq)))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = playavg2) + labs(x = "Text_ID", y = "Average % of Text (by lemma)", title = "Average Appearances of Play in Corpus") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Work-Text-freq.pdf"))
library(ggplot2)
p <- ggplot(analdf2) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Work_Lemma_Perc)), x = Text_ID, fill = as.numeric(as.character(Work_Freq)))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = workavg2) + labs(x = "Text_ID", y = "Average % of Text (by lemma)", title = "Average Appearances of Work in Corpus") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Play-Text-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(analdf2) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Play_Lemma_Perc)), x = Text_ID, fill = as.numeric(as.character(play_freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = playavg2) + labs(x = "Text_ID", y = "Average % of Text (by lemma)", title = "Average Appearances of Play in Corpus, Normalized Text Lemmas") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Avg.-Work-Text-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(analdf2) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Avg._Work_Lemma_Perc)), x = Text_ID, fill = as.numeric(as.character(work_freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red") + coord_flip()
pl <- p +geom_hline(yintercept = workavg2) + labs(x = "Date", y = "Average % of Text (by lemma)", title = "Average Appearances of Work in Corpus, Normalized by Text Lemmas") + theme(legend.title = element_blank())
pl
dev.off()
avgtab <- matrix(,ncol=4,nrow=2) #Making a table that shows averages by date and text
rownames(avgtab) <- c("Avg. by Date", "Avg. by Text")
colnames(avgtab) <- c("Avg. Play Lemma %", "Avg. Work Lemma %", "Avg. Play Count/Text", "Avg. Work Count/Text")
avgtab[1,] <- c(playavg1, workavg1, playfreqavg1, workfreqavg1)
avgtab[2,] <- c(playavg2, workavg2, playfreqavg2, workfreqavg2)
write.table(avgtab, file.path(outputlocation,"All-Avgs"))
avgtab
