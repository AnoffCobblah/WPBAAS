###The purpose of this script is to create a visualization of the frequency of work and play (normalized by lemma)
###in the reports of the BAAS, based on data collected on 20170210.
rm(list=ls())
datalocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output-Texts/"
outputlocation <- "D:/OneDrive/Journals Collection/Reports of the BAAS/Cobblah_20170712_New-Figure/"
datadf <- read.table("D:/OneDrive/Journals Collection/Reports of the BAAS/Data-Mining-Output/play-work-datadf")

playdf <- datadf[grep("play",datadf$category),]
workdf <- datadf[grep("work",datadf$category),]

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
playfreqavg1 <- mean(as.numeric(as.character(analdf1$Play_Freq)), na.rm=TRUE)
workfreqavg1 <- mean(as.numeric(as.character(analdf1$Work_Freq)), na.rm=TRUE)

####WILL CHANGE BELOW TO INCLUDE WORK AND PLAY LINES IF IT WORKS OUT
dev.off()
pdf(file = file.path(outputlocation,"Play-Date-freq.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Play_Freq)), x = date)) + geom_smooth(aes(y = as.numeric(as.character(Play_Freq)), x = date))
pl <- p  + labs(x = "Date", y = "Count", title = "Appearances of Play in Reports of the BAAS") + theme(legend.title = element_blank())
pl
dev.off()

dev.off()
pdf(file = file.path(outputlocation,"Work-Date-freq.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = as.numeric(as.character(Work_Freq)), x = date))+ geom_smooth(aes(y = as.numeric(as.character(Work_Freq)), x = date))
pl <- p + labs(x = "Date", y = "Count", title = "Appearances of Work in Reports of the BAAS") + theme(legend.title = element_blank())
pl
dev.off()

dev.off()
pdf(file = file.path(outputlocation,"Play-Date-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = (as.numeric(as.character(Play_Freq))/as.numeric(as.character(Play_Total_Lemma))*100), x = date)) + geom_smooth(aes(y = (as.numeric(as.character(Play_Freq))/as.numeric(as.character(Play_Total_Lemma))*100), x = date))
pl <- p + labs(x = "Date", y = "Normalized Frequency", title = "Average Appearances of Play in Reports of the BAAS, Normalized by Text Lemmas") + theme(legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Work-Date-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(analdf1) + geom_bar(stat="identity", aes(y = (as.numeric(as.character(Work_Freq))/as.numeric(as.character(Work_Total_Lemma))*100), x = date)) + geom_smooth(aes(y = (as.numeric(as.character(Work_Freq))/as.numeric(as.character(Work_Total_Lemma))*100), x = date))
pl <- p + labs(x = "Date", y = "Normalized Frequency", title = "Appearances of Work in Reports of the BAAS, Normalized by Text Lemmas") + theme(legend.title = element_blank())
pl
dev.off()

dev.off()
pdf(file = file.path(outputlocation,"WorkandPlay-Date-freqperlemma.pdf"))
library(ggplot2)
analdf1play <- analdf1[grep(".",analdf1$play_freq_per_lemma),] #This works because the frequencies all return decimal numbers
analdf1work <- analdf1[grep(".",analdf1$work_freq_per_lemma),]
pl <- ggplot() + 
  #work plot (blue)
  geom_point(data=analdf1work, aes(y = (as.numeric(as.character(Work_Freq))/as.numeric(as.character(Work_Total_Lemma))*100), x = date), shape=21, color="lightblue", fill="lightblue") + 
  geom_smooth(data=analdf1work, aes(y = (as.numeric(as.character(Work_Freq))/as.numeric(as.character(Work_Total_Lemma))*100), x = date), se=FALSE, fill="blue", colour="darkblue") +
  # play plot (red)
  geom_point(data=analdf1play, aes(y = (as.numeric(as.character(Play_Freq))/as.numeric(as.character(Play_Total_Lemma))*100), x = date), shape=22, color="pink", fill="pink") +
  geom_smooth(data=analdf1play, aes(y = (as.numeric(as.character(Play_Freq))/as.numeric(as.character(Play_Total_Lemma))*100), x = date), se=FALSE, fill="red", colour="red") +
  #lables
  labs(x = "Date", y = "Normalized Frequency", title = "Appearances of Work and Play in Reports of the BAAS, Normalized by Lemmas")
pl
dev.off()