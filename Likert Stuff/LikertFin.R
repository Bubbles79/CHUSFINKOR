library(ggplot2)
library(likert)

#beliefs G0101	G0103	G0104	G0107	G0108	G0110	G0113	G0401	G0404 G0405	G0407	G0408	G0410	
PBsFin = read.csv("/Users/paulwagner/Desktop/ERGMs/Finland/BeliefsFin.csv", header = TRUE, stringsAsFactors=FALSE)
PBsFin <- PBsFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85),-c(1,2,13) ]
PBsFin[PBsFin == 97] <- 3

#Create PBsFin Responses
PBsFin [PBsFin  == 1] <- "No, totally reject"
PBsFin [PBsFin  == 2] <- "Disagree"
PBsFin [PBsFin  == 3] <- "Neutral"
PBsFin [PBsFin  == 4] <- "Agree"
PBsFin [PBsFin  == 5] <- "Strongly Agree"

for(i in 1:ncol(PBsFin)){
  PBsFin[,i] <- as.factor(PBsFin[,i])
}


#G0101	G0103	G0104	G0107	G0108	G0110	G0113	G0401	G0404 G0405	G0407	G0408	G0410	
names(PBsFin) <- c(
  G0101="Voluntary action by businesses",
  G0103="Self-commitment of individual states to reduce GHG-emissions",
  G0104="Expansion of nuclear energy",
  G0107="Use of carbon capture storage technology",
  G0108="Reforestation and avoided deforestation strategies",
  G0110="Private action to minimize the individual ecological footprint",
  G0113="Tax on CO2",
  G0116="National climate law requiring the reduction of Greenhouse Gases",
  G0117="Developing cleantech business solutions",
  G0119="Increasing energy efficiency",
  G0401="My country should take a leading international role in GHG reduction",
  G0402="My government puts too much effort into reducing GHG emission",
  G0404="GHG reduction creates jobs and opportunities for economic growth",
  G0405="Securing the national energy supply is more important than the reducing emission",
  G0407="The  transition to renewable energy supply is too costly",
  G0408="In the long term, energy supply can be secured exclusively by renewable energies",
  G0410="In the long term, the economy profits from the transition to renewable energies")

PBsFin[,1] <- factor(PBsFin[,1], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,2] <- factor(PBsFin[,2], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,3] <- factor(PBsFin[,3], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,4] <- factor(PBsFin[,4], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,5] <- factor(PBsFin[,5], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,6] <- factor(PBsFin[,6], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,7] <- factor(PBsFin[,7], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,8] <- factor(PBsFin[,8], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,9] <- factor(PBsFin[,9], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,10] <- factor(PBsFin[,10], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,11] <- factor(PBsFin[,11], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,12] <- factor(PBsFin[,12], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,13] <- factor(PBsFin[,13], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,14] <- factor(PBsFin[,10], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,15] <- factor(PBsFin[,11], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,16] <- factor(PBsFin[,12], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsFin[,17] <- factor(PBsFin[,13], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)

#str(PBsFin)

PBsFinResult <- likert(PBsFin)
plot(PBsFinResult,type="bar", text.size = 3, wrap =100, plot.percent.high = TRUE, plot.percent.low = TRUE)

