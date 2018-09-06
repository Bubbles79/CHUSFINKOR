library(sna)
library(ergm)
seed <- 12345
set.seed (seed)

#Import networks and convert to matrices
coopUS <- read.csv(file="/Users/paulwagner/Desktop/CHFinSK_US/US/coopUS.csv",header=TRUE)
SciUS <- read.csv(file= "/Users/paulwagner/Desktop/CHFinSK_US/US/SciUS.csv",header=TRUE)
infUS <- read.csv(file= "/Users/paulwagner/Desktop/CHFinSK_US/US/infUS.csv", header = TRUE)
OrgTypeUS <- read.csv(file="/Users/paulwagner/Desktop/CHFinSK_US/US/OrgTypeUS.csv", header = TRUE)

#Convert to matrices
coopUS=network(coopUS,matrix.type="edgelist",directed=TRUE) 
SciUS=network(SciUS,matrix.type="edgelist",directed=TRUE) 
infUS=network(infUS,matrix.type="edgelist",directed=TRUE) 
coopUS <- as.matrix(coopUS)
SciUS <- as.matrix(SciUS)
infUS <- as.matrix(infUS)
OrgTypeUS <- as.matrix(OrgTypeUS)

#remove self ties
diag (coopUS) <- 0
diag (SciUS)  <- 0
diag (infUS) <- 0

USOrgNames <- as.vector(colnames(coopUS))

#Remove non-respondents
coopUS <- coopUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105, 109, 110, 111, 112, 113),-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105, 109, 110, 111, 112, 113)]
SciUS <- SciUS[-c(3, 6, 11, 12, 13, 14,18,26,27,30,32,33,35,41,42,44,53,58,59,66,70,72,74,76,79,81,83,84,85,86,90,92,93,95,96,100,101,102,103,104),-c(3, 6, 11, 12, 13, 14,18,26,27,30,32,33,35,41,42,44,53,58,59,66,70,72,74,76,79,81,83,84,85,86,90,92,93,95,96,100,101,102,103,104)]
infUS <- infUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72,73,74,76,79,81,82,85,87,89,90,92,93,95,96,97,99,100,102,103,107,108,109,110,111),-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72,73,74,76,79,81,82,85,87,89,90,92,93,95,96,97,99,100,102,103,107,108,109,110,111)]

#4 actor types
OrgTypeUS <- OrgTypeUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105),6]

#for 5 actor types
OrgTypeUS <- OrgTypeUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105),5]

#prepare orgtype data, so that it can be set as an attribute
OrgTypeUS <- as.character(OrgTypeUS)
OrgTypeUS <- as.vector(OrgTypeUS)

#import and calculate beliefs distance matrix. Convert to 3 point Likert scale 
PBsUS = read.csv("/Users/paulwagner/Desktop/CHFinSK_US/US/BeliefsUS.csv", header = TRUE)
#PBsUS <- PBsUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105, 109, 110, 111, 112, 113), -c(1:3,15) ]
rownames(PBsUS) <- USOrgNames
colnames(PBsUS) <- USOrgNames
PBsUS <- PBsUS[-c(3, 6, 11, 12, 13, 14, 17, 19, 27, 28, 31, 33, 34, 36, 42, 43, 45, 48, 55, 60, 61, 68, 72, 73, 74, 75, 77, 79, 80, 81, 83, 84, 87, 89, 91, 92, 94, 95, 98, 99, 101, 102, 104, 105, 109, 110, 111, 112, 113), c(4,9,10) ]
PBsUS[PBsUS== 97] <- 3
PBsUS [PBsUS  == 2] <- 1
PBsUS [PBsUS  == 3] <- 2
PBsUS [PBsUS  == 4] <- 3
PBsUS [PBsUS  == 5] <- 3
Pol.dist.US <- dist(PBsUS, method = "manhattan") #we might want to do Euclidean
PrefSimMatUS <- max(Pol.dist.US) - Pol.dist.US
PBsUS <-as.matrix(PrefSimMatUS)

#order of actor types for the nodefactor terms 
x <- sort(unique(OrgTypeUS))
nodecov <- match(OrgTypeUS,x)
#"BUS" "CIV" "GOV" "NGO" "SCI"
#"BUS" "CIV" "GOV" "SCI"

#set attributes
nw.coopUS <- network(coopUS) # create network object
set.vertex.attribute (nw.coopUS, "OrgTypeUS", OrgTypeUS)
set.vertex.attribute (nw.coopUS, "Influence", degree (infUS, cmode = "indegree"))


#03 Sept 2018
US1 <- ergm(nw.coopUS ~ edges,
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US1)
plot(mcmc.diagnostics(US1))
plot(gof(US1))

US2 <- ergm(nw.coopUS ~ edges + edgecov(PBsUS),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US2)
plot(mcmc.diagnostics(US2))
plot(gof(US2))

US3 <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
              #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
              #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)),
              nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
              nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US3)
plot(mcmc.diagnostics(US3))
plot(gof(US3))

US4a <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
               #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
               #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)) +
               nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
               nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)) +
              nodeicov("Influence") ,
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US4a)
plot(mcmc.diagnostics(US4a))
plot(gof(US4a))

US4b <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
              #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
              #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)) +
               nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
               nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)) +
               edgecov (infUS),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US4b)
plot(mcmc.diagnostics(US4b))
plot(gof(US4b))

US5 <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
              #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
              #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)) +
              nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
              nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)) + 
              edgecov (infUS) + nodeicov("Influence") ,
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US5)
plot(mcmc.diagnostics(US5))
plot(gof(US5))

US6a <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
               #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
               #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)) +
               nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
               nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)) + 
              nodeicov("Influence") + 
              mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US6a)
plot(mcmc.diagnostics(US6a))
plot(gof(US6a))   

US6b <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
               #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
               #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)) +
               nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
               nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)) + 
               edgecov (infUS) + 
             mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US6b)
plot(mcmc.diagnostics(US6b))
plot(gof(US6b))

US7 <- ergm(nw.coopUS ~ edges + edgecov(PBsUS) + 
              #nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
              #nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4)) +
              nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
              nodematch ("OrgTypeUS", diff = T) + nodefactor("OrgTypeUS", base=-c(2,3,4,5)) + 
            edgecov (infUS) + nodeicov("Influence") + 
            mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
            eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US7)
plot(mcmc.diagnostics(US7))
plot(gof(US7))
     
par(mfrow=c(1,1))
par(mfrow=c(2,2))

texreg::screenreg(list(US1,US2,US3,US4a,US4b,US5,US6a,US6b,US7),single.row = T)

#4 actor types
texreg::htmlreg(list(US1,US2,US3,US4a,US4b,US5,US6a,US6b,US7),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/USResults4.html",single.row = T)
#5 actor types
texreg::htmlreg(list(US1,US2,US3,US4a,US4b,US5,US6a,US6b,US7),file="/Users/paulwagner/Desktop/USResults5.html",single.row = T)

#-----------------


#old

#when NGOs and CIV are the same category 14/03/18
m6.US1 <- ergm(nw.coopUS ~ edges + mutual + edgecov(PBsUS) + edgecov(US.bus.civ ) +
                  nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
                  nodeofactor ("OrgTypeUS", base=c(1,3,4)) + nodematch ("OrgTypeUS") + 
                  edgecov (infUS) + nodeicov("Influence") + absdiff ("Influence") +
                  gwodegree(2.5, fixed = TRUE) +
                  twopath + gwesp(1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.US1)
plot(mcmc.diagnostics(m6.US1))
plot(gof(m6.US1))







#when NGOs and CIV are not the same category
m6.US1 <- ergm(nw.coopUS ~ edges + mutual + edgecov(PBsUS) + edgecov(US.bus.ngo) +
                 nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
                 nodeofactor ("OrgTypeUS", base=c(1,2,3,5)) + nodematch ("OrgTypeUS") + 
                 edgecov (infUS) + nodeicov("Influence") + absdiff ("Influence") +
                 gwodegree(2.5, fixed = TRUE) + gwidegree(2.5, fixed = TRUE) +
                 twopath + gwesp(1 , fixed = TRUE ),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.US1)
plot(mcmc.diagnostics(m6.US1))
plot(gof(m6.US1))

texreg::htmlreg(list(m6.Fin1,m6.KOR1,m6.US1),file="/Users/paulwagner/Desktop/ERGMs/CHFinSK_US/Models2a.html",single.row = T)

#-----

#Older models 02/18
#create matrix of ties between bus actors and NGOs (expect a negative paramater estimate)
US.bus.civ <- matrix (0 , nrow = nrow (coopUS) , ncol = ncol (coopUS))
for (i in 1: nrow (US.bus.civ  )) {
  for (j in 1: ncol ( US.bus.civ )) {
    if (( OrgTypeUS [i] == "BUS" && OrgTypeUS [j] == "CIV") ||
        ( OrgTypeUS [i] == "CIV" && OrgTypeUS [j] == "BUS")) {
      US.bus.civ  [i, j] <- 1
      US.bus.civ  [j, i] <- 1
    }
  }
}
#---models---
#"BUS" "CIV" "GOV" "NGO" "SCI"  absdiff ("Influence") + gwidegree(2 , fixed = TRUE ) +  gwodegree(0.5 , fixed = TRUE ) + twopath + gwdsp(0.3 , fixed = TRUE ) + gwesp(0.3 , fixed = TRUE )
# edgecov(SciUS) + 
m1.US <- ergm(nw.coopUS ~ edges,
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m1.US)
plot(mcmc.diagnostics(m1.US))
plot(gof(m1.US))

m2.US <- ergm(nw.coopUS ~ edges + mutual,
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m2.US)
plot(mcmc.diagnostics(m2.US))
plot(gof(m2.US))

m3.US <- ergm(nw.coopUS ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m3.US)
plot(mcmc.diagnostics(m3.US))
plot(gof(m3.US))

m4.US <- ergm(nw.coopUS ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsUS),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m4.US)
plot(mcmc.diagnostics(m4.US))
plot(gof(m4.US))

m5.US <- ergm(nw.coopUS ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsUS) + nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeUS", base=c(1,2,3,5)) + nodematch ("OrgTypeUS"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m5.US)
plot(mcmc.diagnostics(m5.US))
plot(gof(m5.US))

m6.US <- ergm(nw.coopUS ~ edges + mutual + gwidegree(1.5 , fixed = TRUE ) + gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsUS) + edgecov(US.bus.ngo) +
                nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeUS", base=c(1,2,3,5)) + nodematch ("OrgTypeUS") + 
                edgecov (infUS) + nodeicov("Influence") + absdiff ("Influence"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 2500 , MCMC.interval = 2500))
summary(m6.US)
plot(mcmc.diagnostics(m6.US))
plot(gof(m6.US))


texreg::htmlreg(list(m1.US,m2.US,m3.US,m4.US,m5.US,m6.US),file="/Users/paulwagner/Desktop/ERGMs/CHFinSK_US/Korea.html",single.row = T)

texreg::htmlreg(list(m6.Fin,m6.KOR,m6.US),file="/Users/paulwagner/Desktop/ERGMs/CHFinSK_US/Models.html",single.row = T)


#when NGOs and CIV are the same category
m6.US1a <- ergm(nw.coopUS ~ edges + mutual + edgecov(PBsUS) + edgecov(US.bus.civ ) +
                nodeifactor ("OrgTypeUS", base=c(1,2,4)) + 
                nodeofactor ("OrgTypeUS", base=c(1,3,4)) + nodematch ("OrgTypeUS") + 
                edgecov (infUS) + nodeicov("Influence") + absdiff ("Influence") +
                gwodegree(2.5, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.US1)
plot(mcmc.diagnostics(m6.US1))


#when NGOs and CIV are not the same category
m6.US1 <- ergm(nw.coopUS ~ edges + mutual + edgecov(PBsUS) + edgecov(US.bus.ngo) +
                  nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + 
                  nodeofactor ("OrgTypeUS", base=c(1,3,4,5)) + nodematch ("OrgTypeUS") + 
                  edgecov (infUS) + nodeicov("Influence") + absdiff ("Influence") +
                  gwodegree(2.5, fixed = TRUE) +
                  twopath + gwesp(1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.US1)
plot(mcmc.diagnostics(m6.US1))
plot(gof(m6.US1))


palette(c("blue","green","red","yellow"))
plot(nw.coopUS , displaylabels = FALSE , vertex.cex = degree(infUS, cmode = 'indegree') / 20 , vertex.border = "black",edge.col = "black", vertex.col = "OrgTypeUS",pad = 1)





#------ may 13th 
US.m1 <- ergm(nw.coopUS ~ edges + mutual + edgecov(PBsUS) + nodematch ("OrgTypeUS") +
                 nodeifactor ("OrgTypeUS", base=c(1,2,4,5)) + edgecov(US.bus.ngo) +
                 nodeicov("Influence") + absdiff ("Influence") +
                 edgecov(LowHighUS) + edgecov(LowHighUS) +
                 gwodegree(2.5, fixed = TRUE) +
                 twopath + gwesp(0.1 , fixed = TRUE ),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(US.m1)

