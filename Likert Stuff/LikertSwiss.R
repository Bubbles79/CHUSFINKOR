
PBsSwiss = read.csv("/users/paulwagner/Desktop/CHFinSK_US/Switzerland/CH_Bfs.csv", header = TRUE, stringsAsFactors=FALSE)
PBsSwiss <- PBsSwiss[,-c(1),]

#Create PBsKOR Responses
PBsSwiss [PBsSwiss  == 1] <- "Disagree"
PBsSwiss [PBsSwiss  == 2] <- "Neutral"
PBsSwiss [PBsSwiss  == 3] <- "Agree"

for(i in 1:ncol(PBsSwiss)){
  PBsSwiss[,i] <- as.factor(PBsSwiss[,i])
}

names(PBsSwiss) <- c(
  G0101="VAs",
  G0102="Krappen",
  G0104="Brenn",
  G0107="Treib",
  G0111="Flex",
  G0113="Strict_target")

PBsSwiss[,1] <- factor(PBsSwiss[,1], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsSwiss[,2] <- factor(PBsSwiss[,2], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsSwiss[,3] <- factor(PBsSwiss[,3], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsSwiss[,4] <- factor(PBsSwiss[,4], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsSwiss[,5] <- factor(PBsSwiss[,5], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBsSwiss[,6] <- factor(PBsSwiss[,6], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)

PBsSwiss_Result <- likert(PBsSwiss)
plot(PBsSwiss_Result,type="bar", text.size = 3, wrap =100, plot.percent.high = TRUE, plot.percent.low = TRUE)

