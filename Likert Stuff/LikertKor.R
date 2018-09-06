library(ggplot2)
library(likert)

#beliefs G0101	G0103	G0104	G0107	G0108	G0110	G0113	G0401	G0404 G0405	G0407	G0408	G0410	
PBsKOR = read.csv("/Users/paulwagner/Desktop/CHFinSK_US/Korea/BeliefsKor.csv", header = TRUE)
#PBsKOR <- PBsKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106), -c(1:2) ]
PBsKOR <- PBsKOR[-c(1, 16, 18, 34, 40, 41, 44, 45, 48, 75, 78, 82, 83, 84, 90, 92, 97, 98, 99, 100,101,102,103,104,105,106), -c(1,2) ]
PBsKOR[PBsKOR== 97] <- 3


#Create PBsKOR Responses
PBsKOR [PBsKOR  == 1] <- "No, totally reject"
PBsKOR [PBsKOR  == 2] <- "Disagree"
PBsKOR [PBsKOR  == 3] <- "Neutral"
PBsKOR [PBsKOR  == 4] <- "Agree"
PBsKOR [PBsKOR  == 5] <- "Strongly Agree"

for(i in 1:ncol(PBsKOR)){
  PBsKOR[,i] <- as.factor(PBsKOR[,i])
}


#G0101	G0103	G0104	G0107	G0108	G0110	G0113	G0401	G0404 G0405	G0407	G0408	G0410	
names(PBsKOR) <- c(
  G0101="Voluntary action by businesses",
  G0102="Emission trading",
  G0104="Expansion of nuclear energy",
  G0107="Use of carbon capture storage technology",
  G0109="Reduction of fuel consumtion in transportation",
  G0110="Private action to minimize the individual ecological footprint",
  G0111="Carbon offsetting",
  G0113="Tax on CO2",
  G0401="My country should take a leading international role in GHG reduction",
  G0402="My government puts too much effort into reducing GHG emission",
  G0404="GHG reduction creates jobs and opportunities for economic growth",
  G0406="The current GHG reduction target of my government is too ambitious",
  G0407="The  transition to renewable energy supply is too costly",
  G0409="Nuclear energy is the most realistic alternative to fossile fuels",
  G0411="The target of my country's government for renewable energy is too ambitious")

PBsKOR[,1] <- factor(PBsKOR[,1], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,2] <- factor(PBsKOR[,2], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,3] <- factor(PBsKOR[,3], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,4] <- factor(PBsKOR[,4], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,5] <- factor(PBsKOR[,5], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,6] <- factor(PBsKOR[,6], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,7] <- factor(PBsKOR[,7], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,8] <- factor(PBsKOR[,8], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,9] <- factor(PBsKOR[,9], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,10] <- factor(PBsKOR[,10], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,11] <- factor(PBsKOR[,11], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,12] <- factor(PBsKOR[,12], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,13] <- factor(PBsKOR[,13], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,14] <- factor(PBsKOR[,10], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsKOR[,15] <- factor(PBsKOR[,11], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)

#str(PBsKOR)

PBsKOR_Result <- likert(PBsKOR)
plot(PBsKOR_Result,type="bar", text.size = 3, wrap =100, plot.percent.high = TRUE, plot.percent.low = TRUE)
