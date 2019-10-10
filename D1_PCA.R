rm(list=ls())
library(ggplot2)
library(foreign)
library(munsell)
library(dplyr)
library(tidyr)
library(scales)
library(Hmisc)
library(corrplot)
library(psych)
library(R.matlab)

setwd("C:\\Users\\lieke.deboer\\Dropbox\\R\\")
#setwd("/home/lieke/Dropbox/R/")

#do all of this ridiculous stuff to merge the three dataframes that make up this GNG data (look into doing something more efficient)

eps                   <- read.csv("pcadata.csv")
gdperf                <- readMat('ll2baxbkwins.mat') #these are the model parameters for the tryout hanneke model (something probably wrong ehre)

df                    <- data.frame(matrix(unlist(c(gdperf$newsb, gdperf$group)), nrow=41, byrow=F),stringsAsFactors=FALSE) #read the mat and get the subj numbers
gdperf                <- cbind(gdperf$params, df) # add the subj names to the parameters
colnames(gdperf)[1:9] <- c('beta_wingd','beta_losegd','alphagd','noisegd','kappagd','gogd','epsgogd','newsub','newgroup') # give the dataframe some column names

rownames(gdperf)      <- gdperf$newsub # make sure the rownames are what they should be
gdperf                <- subset(gdperf, select=-c(newsub)) # take the subject variable out before there are 100000 columns with the same values

gdallvar              <- topl[topl$cluster_GW_lastbin==1 ,] # this selects only those that were in the good performing cluster in the last 15 trials of GW
d                     <- complete.cases(gdallvar[, 1]) # clean up the dataframe so NAs are gone 
gdallvar              <- gdallvar[d ,] 
rownames(gdallvar)    <- gsub('     ', '',gdallvar$subject) # make sure subj names match
mmds                  <- read.csv("MMSE_ds_gnggood.csv", sep = ";") # read third df that includes mmse and digit span
rownames(mmds)        <- mmds$subject # make sure rownames match

gdallvar              <- merge(mmds, gdallvar, by="row.names") # merge the shit
rownames(gdallvar)    <- gdallvar$Row.names #take away variables that otherwise duplicate
gdallvar              <- subset(gdallvar, select=-c(Row.names, subject.x)) #remove superfluous variables

gdallvar              <- merge(gdperf, gdallvar, by="row.names") #merge again! at least you didnt have to copy paste!

# define colour palette for correlatiograms
col2 <- colorRampPalette(c("blue", "green4","hotpink3", "deeppink4"))
col3 <- colorRampPalette(c("blue", "green4","gold", "red2"))

#whatever order you prefer
dadf<-read.csv("pcadata.csv")
colnames(dadf)<-c("index", "age","Caudate", "Putamen", "NAcc" ,"dlPFC/vlPFC: \nBA 9,44,45,46", "medial+lateral \nOFC", "premotor PFC:\nBA 4/6", "parietal cortex")
dadfc<-as.matrix(na.omit(dadf[, 3:9]))
dadfcfig<-rcorr(as.matrix(na.omit(dadf[, 3:9])))


# first for all participants together
pcad1 <- principal(dadfc, nfactors = 3, rotate = "varimax", scores=T)
corrplot(dadfcfig$r, order = "original", col = col3(100), cl.lim=c(0,1),cl.length = 11, method="square",
         addCoef.col = "white", tl.col="black", p.mat = dadfcfig$P, sig.level = 0.001, insig="blank", title = "all", tl.cex = .8)

# then for the two age groups, separately
dadfoc<-rcorr(as.matrix(na.omit(dadf[dadf$age>50, 3:9])))
dadfyc<-rcorr(as.matrix(na.omit(dadf[dadf$age<50, 3:9])))
corrplot(dadfoc$r, order = "original", col = col3(100), cl.lim=c(0,1),cl.length = 11, method="square",
         addCoef.col = "white", tl.col="black", p.mat = dadfoc$P, sig.level = 0.05, title = "old")
corrplot(dadfyc$r, order = "original", col = col3(100), cl.lim=c(0,1),cl.length = 11, method="square",
         addCoef.col = "white", tl.col="black", p.mat = dadfyc$P, sig.level = 0.05, title = "young")
