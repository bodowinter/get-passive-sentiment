## Bodo Winter
## June 21, 2015
## Get-passives full ngram data from COCA

########################################################################
######################## Preprocessing
########################################################################

## Load in data:

setwd("/Users/teeniematlock/Desktop/research/get_passives/analysis")
pass <- read.csv("passives_2grams.csv",quote="")

## Load in libraries:

library(dplyr)
library(ggplot2)
library(grid)

## Clean-up column names:

colnames(pass) <- substr(colnames(pass),3,(nchar(colnames(pass))-1))

## Get rid of extraneous quotation marks:

pass <- as.data.frame(lapply(pass,function(x)gsub("\"","",x)),stringsAsFactors=F)

## Make frequency columns numeric:

pass$freq <- as.numeric(pass$freq)

## Create log frequency column:

pass <- mutate(pass,LogFreq=log10(freq+1))

## Get only those that are actually passives (i.e., second gram == "vvd"):

pass <- pass[grep("vvn",pass$n2_POS),]

## Load in Warriner and combine:

affect <- read.csv("/Users/teeniematlock/Desktop/big_data/warriner_affective_ratings/Ratings_Warriner_et_al.csv")
pass$Valence <- affect[match(pass$n2_lemma,affect$Word),]$V.Mean.Sum
pass$Dominance <- affect[match(pass$n2_lemma,affect$Word),]$D.Mean.Sum



########################################################################
######################## Analysis by lemma:
########################################################################

## Lemmatize into be-verbs and get-verbs:

pass$lemma2 <- pass$lemma
pass[pass$lemma %in% c("are","be","been","being","is","was","were"),]$lemma2 <- "be passive"

## How many rows?

nrow(pass)		# 85,295

## Get rid of valence NAs:

nrow(na.omit(pass))		# 54,450 for which there is a match

## For each passive and n2_lemma, get the averages (so that we don't pseudo-replicate):

pass %>% group_by(lemma2,n2_lemma) %>%
	summarise(Valence=mean(Valence,na.rm=T),Dominance=mean(Dominance,na.rm=T),freq=sum(freq)) -> pass_red

## What's the overall frequency?

aggregate(freq ~ lemma2,pass_red,sum)

## What's the average valence?:

aggregate(Valence ~ lemma2,pass_red,mean)

## What's the average valence of the collocate?

p <- ggplot(pass_red,aes(x=lemma2,y=Valence,fill=lemma2)) + geom_boxplot() + theme_minimal() +
	labs(y="Valence Ratings\n",x="\nPassive Type")
quartz("",7,7)
p + scale_fill_manual(values=c("#669900","#FF8533")) +
	theme_minimal() + theme(plot.margin=unit(c(1,1,1,1.5),"cm"),
		axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

## Is there a frequency/valence interaction?

mutate(pass_red,LogFreq=log10(freq+1)) -> pass_red
quartz("",9,5)
ggplot(pass_red,aes(x=LogFreq,y=Valence,color=lemma2)) + geom_point(shape=16,alpha=45/100) + facet_wrap(~lemma2) + 
	labs(y="Valence Ratings\n",x="\nFrequency (log10)") +
	scale_color_manual(values=c("#669900","#FF8533")) +
	geom_smooth(method="lm") +
	theme_minimal() + theme(plot.margin=unit(c(1,1,1,1.5),"cm"),
		axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave(file="frequency_valence.png")

## Test for the interaction:

pass_red$LogFreq_c <- pass_red$LogFreq-mean(pass_red$LogFreq)
summary(xmdl <- lm(Valence ~ LogFreq_c*lemma2,pass_red))		# super-significant

## What's the average valence?:

aggregate(Dominance ~ lemma2,pass_red,mean)

## What's the average valence of the collocate?

ggplot(pass_red,aes(x=lemma2,y=Dominance,fill=lemma2)) + geom_boxplot()

## Is there a frequency/valence interaction?

ggplot(pass_red,aes(x=LogFreq,y=Dominance,color=lemma2)) + geom_point(shape=16) + facet_wrap(~lemma2) + geom_smooth(method="lm")

## Test for the interaction:

summary(lm(Dominance ~ LogFreq_c*lemma2,pass_red))		# super-significant

## What are the 10 most frequent words for "get" passives and "be" passives?

arrange(pass[pass$lemma2=="get",],LogFreq) %>% select(n2_word,n2_lemma)



########################################################################
######################## Load in sentiment analysis data from Bing Liu
########################################################################

pos <- readLines("opinion_lexicon_positive.txt")
neg <- readLines("opinion_lexicon_negative.txt")

## Take only those lines that matter:

pos <- pos[-c(1:36)]
neg <- neg[-c(1:36)]

## Match:

pass$LiuPosWord <- pos[match(pass$n2_lemma,pos)]
pass$LiuNegWord <- neg[match(pass$n2_lemma,neg)]

## Make a numerical variable:

pass$LiuPosWord01 <- ifelse(is.na(pass$LiuPosWord),0,1)
pass$LiuNegWord01 <- ifelse(is.na(pass$LiuNegWord),0,1)

## Make a logistic regression out of this:

posreg <- glm(LiuPosWord01~lemma2,pass,family="binomial")
summary(posreg)

negreg <- glm(LiuNegWord01~lemma2,pass,family="binomial")
summary(negreg)

## Fit the interaction with frequency:

posreg.freq <- glm(LiuPosWord01~lemma2*LogFreq_c,pass,family="binomial")
summary(posreg.freq)

negreg.freq <- glm(LiuNegWord01~lemma2*LogFreq_c,pass,family="binomial")
summary(negreg.freq)

## Get predictions for plot with positive values:

passive.pred <- predict.glm(lm(LiuPosWord01~lemma2*),newdata=data.frame(LogFreq=seq(0,4.5,0.1),lemma2="be passive"),
	type="response",se.fit=T)

## Make a ggplot out of this:

quartz("",9,5)
ggplot(pass,aes(x=LogFreq,factor,
	y=LiuPosWord01,color=lemma2)) + geom_point(shape=16,alpha=5/100,position=position_jitter(width=0.05,height=0.05)) +
	facet_wrap(~lemma2) + 
	labs(y="Positive Word\n",x="\nFrequency (log10)") +
	scale_color_manual(values=c("#669900","#FF8533")) +
	coord_cartesian(ylim=c(-0.5,1.5)) +
	stat_smooth(method="glm",family="binomial") +
	theme_minimal() + theme(plot.margin=unit(c(1,1,1,1.5),"cm"),
		axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave(file="sentiment.png")

## Zoom in:

quartz("",9,5)
ggplot(pass,aes(x=LogFreq,factor,
	y=LiuPosWord01,color=lemma2)) + geom_point(shape=16,alpha=0,position=position_jitter(width=0.05,height=0.05)) +
	facet_wrap(~lemma2) + 
	labs(y="Positive Word\n",x="\nFrequency (log10)") +
	scale_color_manual(values=c("#669900","#FF8533")) +
	coord_cartesian(ylim=c(-0.05,0.2)) +
	stat_smooth(method="glm",family="binomial") +
	theme_minimal() + theme(plot.margin=unit(c(1,1,1,1.5),"cm"),
		axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave(file="sentiment_logistic.png")

## Make a ggplot out of this:

quartz("",9,5)
ggplot(pass,aes(x=LogFreq,factor,
	y=LiuNegWord01,color=lemma2)) + geom_point(shape=16,alpha=5/100,position=position_jitter(width=0.05,height=0.05)) +
	facet_wrap(~lemma2) + 
	labs(y="Negative Word\n",x="\nFrequency (log10)") +
	scale_color_manual(values=c("#669900","#FF8533")) +
	coord_cartesian(ylim=c(-0.5,1.5)) +
	stat_smooth(method="glm",family="binomial") +
	theme_minimal() + theme(plot.margin=unit(c(1,1,1,1.5),"cm"),
		axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave(file="sentiment_negative.png")

## Zoom in:

quartz("",9,5)
ggplot(pass,aes(x=LogFreq,factor,
	y=LiuNegWord01,color=lemma2)) + geom_point(shape=16,alpha=0,position=position_jitter(width=0.05,height=0.05)) +
	facet_wrap(~lemma2) + 
	labs(y="Negative Word\n",x="\nFrequency (log10)") +
	scale_color_manual(values=c("#669900","#FF8533")) +
	coord_cartesian(ylim=c(0,0.6)) +
	stat_smooth(method="glm",family="binomial") +
	theme_minimal() + theme(plot.margin=unit(c(1,1,1,1.5),"cm"),
		axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave(file="sentiment_logistic_negative.png")




########################################################################
######################## Sentiwordnet analysis
########################################################################

## Load in data:

senti <- read.table("SentiWordNet_3.0.0_20130122.txt",skip=27,quote="")
senti <- senti[,c("V3","V4","V5")]
names(senti) <- c("PosScore","NegScore","Word")

## Merge:

pass <- cbind(pass,senti[match(pass$n2_lemma,senti$Word),c("PosScore","NegScore")])

## Analysis without frequency:

PosScore.mdl <- lm(PosScore ~ lemma2,pass)
NegScore.mdl <- lm(NegScore ~ lemma2,pass)

summary(PosScore.mdl)
summary(NegScore.mdl)

## Analysis with frequency:

PosScore.freq.mdl <- lm(PosScore ~ LogFreq_c*lemma2,pass)
NegScore.freq.mdl <- lm(NegScore ~ LogFreq_c*lemma2,pass)

summary(PosScore.freq.mdl)
summary(NegScore.freq.mdl)



