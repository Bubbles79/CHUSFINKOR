

PBsUS = read.csv("/Users/paulwagner/Desktop/CHFinSK_US/US/BeliefsUS.csv", header = TRUE)
#PBsUS <- PBsUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105, 109, 110, 111, 112, 113), -c(1:3,15) ]
PBsUS <- PBsUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105, 109, 110, 111, 112, 113), -c(1,2,3,15) ]
PBsUS[PBsUS== 97] <- 3


#Create PBsKOR Responses
PBsUS [PBsUS  == 1] <- "No, totally reject"
PBsUS [PBsUS  == 2] <- "Disagree"
PBsUS [PBsUS  == 3] <- "Neutral"
PBsUS [PBsUS  == 4] <- "Agree"
PBsUS [PBsUS  == 5] <- "Strongly Agree"

for(i in 1:ncol(PBsUS)){
  PBsUS[,i] <- as.factor(PBsUS[,i])
}


#G0101	G0103	G0104	G0107	G0108	G0110	G0113	G0401	G0404 G0405	G0407	G0408	G0410	
names(PBsUS) <- c(
  G0101="Voluntary action by businesses",
  G0102="Emission trading",
  G0104="Expansion of nuclear energy",
  G0107="Use of carbon capture storage technology",
  G0111="Carbon offsetting",
  G0113="Tax on CO2",
  G0401="My country should take a leading international role in GHG reduction",
  G0402="My government puts too much effort into reducing GHG emission",
  G0404="GHG reduction creates jobs and opportunities for economic growth",
  G0406="The current GHG reduction target of my government is too ambitious",
  G0409="Nuclear energy is the most realistic alternative to fossile fuels")

PBsUS[,1] <- factor(PBsUS[,1], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,2] <- factor(PBsUS[,2], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,3] <- factor(PBsUS[,3], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,4] <- factor(PBsUS[,4], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,5] <- factor(PBsUS[,5], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,6] <- factor(PBsUS[,6], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,7] <- factor(PBsUS[,7], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,8] <- factor(PBsUS[,8], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,9] <- factor(PBsUS[,9], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,10] <- factor(PBsUS[,10], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsUS[,11] <- factor(PBsUS[,11], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
#str(PBsUS)

PBsUS_Result <- likert(PBsUS)
plot(PBsUS_Result,type="bar", text.size = 3, wrap =100, plot.percent.high = TRUE, plot.percent.low = TRUE)
