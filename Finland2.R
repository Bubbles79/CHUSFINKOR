library(sna)
library(ergm)
seed <- 12345
set.seed (seed)

#Import networks
coopFin <- read.csv(file="/Users/paulwagner/Desktop/ERGMs/Finland/AprilData2018/CollabAprilFin.csv",header=TRUE)
SciFin <- read.csv(file= "/Users/paulwagner/Desktop/ERGMS/Finland/AprilData2018/SciAprilFin.csv",header=TRUE)
infFin <- read.csv(file= "/Users/paulwagner/Desktop/ERGMS/Finland/AprilData2018/InfAprilFin.csv", header = TRUE)
OrgTypeFin <- read.csv(file="/Users/paulwagner/Desktop/ERGMS/Finland/OrgTypeFin.csv", header = TRUE)
coopFin=network(coopFin,matrix.type="edgelist",directed=TRUE) 
SciFin=network(SciFin,matrix.type="edgelist",directed=TRUE)
infFin=network(infFin,matrix.type="edgelist",directed=TRUE) 
coopFin <- as.matrix(coopFin)
SciFin <- as.matrix(SciFin)
infFin <- as.matrix(infFin)
coopFin <- coopFin[c(1:96),c(1:96)]
SciFin <- SciFin[c(1:96),c(1:96)]
infFin <- infFin[c(1:96),c(1:96)]

diag (coopFin) <- 0
diag (SciFin)  <- 0
diag (infFin) <- 0

FinOrgNames <- as.vector(colnames(OrgTypeFin))

coopFin <-coopFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85),-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85)]
SciFin  <-SciFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85),-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85)]
infFin  <-infFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85),-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85)]
OrgTypeFin  <-OrgTypeFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85), ]

coopFin <- as.matrix(coopFin)
infrepFin <- as.matrix(infFin)
SciFin <- as.matrix(SciFin)

#OrgTypeFin <- OrgTypeFin[,6] #4 actor types
OrgTypeFin <- OrgTypeFin[,4]  #5 actor types
OrgTypeFin <- as.character(OrgTypeFin)
OrgTypeFin <- as.vector(OrgTypeFin)

#import and calculate beliefs distance matrix. Convert to 3 point Likert scale 
PBsFin = read.csv("/users/paulwagner/Desktop/CHFinSK_US/Finland/BeliefsFin.csv", header = TRUE, stringsAsFactors=FALSE)
#PBsFin <- PBsFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85),-c(1,2,13) ]
rownames(PBsFin) <- FinOrgNames
colnames(PBsFin) <- FinOrgNames
PBsFin <- PBsFin[-c(4,8,12,25,28,43,44,46,52,53,56,69,73,85),c(3,9,14) ]
PBsFin[PBsFin == 97] <- 3
PBsFin [PBsFin  == 2] <- 1
PBsFin [PBsFin  == 3] <- 2
PBsFin [PBsFin  == 4] <- 3
PBsFin [PBsFin  == 5] <- 3
Pol.dist.Fin <- dist(PBsFin, method = "manhattan")
PrefSimMatFin <- max(Pol.dist.Fin) - Pol.dist.Fin
PBsFin <-as.matrix(PrefSimMatFin)


#order of actor types for the nodefactor terms 
f <- sort(unique(OrgTypeFin))
nodecov <- match(OrgTypeFin,f)
#"BUS" "CIV" "GOV" "NGO" "SCI"
#"BUS" "CIV" "GOV" "SCI"

#set attributes
nw.coopFin <- network(coopFin) # create network object
set.vertex.attribute (nw.coopFin, "OrgTypeFin", OrgTypeFin)
set.vertex.attribute (nw.coopFin, "Influence", degree (infFin, cmode = "indegree"))


#03 Sept 2018--------
Fin1 <- ergm(nw.coopFin ~ edges,
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin1)
plot(mcmc.diagnostics(Fin1))
plot(gof(Fin1))

Fin2 <- ergm(nw.coopFin ~ edges + edgecov(PBsFin),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin2)
plot(mcmc.diagnostics(Fin2))
plot(gof(Fin2))

Fin3 <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
               #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
               #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)),
                nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin3)
plot(mcmc.diagnostics(Fin3))
plot(gof(Fin3))

Fin4a <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
                #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
                #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)) +
                nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)) +
               nodeicov("Influence"),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin4a)
plot(mcmc.diagnostics(Fin4a))
plot(gof(Fin4a))

Fin4b <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
                #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
                #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)) +
                 nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                 nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)) +
               edgecov (infFin),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin4b)
plot(mcmc.diagnostics(Fin4b))
plot(gof(Fin4b))

Fin5 <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
              #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
              #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)) +
               nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
               nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)) + 
               edgecov (infFin) + nodeicov("Influence"),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 5000 , MCMC.interval = 5000))
summary(Fin5)
plot(mcmc.diagnostics(Fin5))
plot(gof(Fin5))

Fin6a <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
                #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
                #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)) +
                 nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                 nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)) + 
                nodeicov("Influence") +  
                mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin6a)
plot(mcmc.diagnostics(Fin6a))
plot(gof(Fin6a))

Fin6b <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
                #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
                #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)) +
                 nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                 nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)) + 
                edgecov (infFin) +  
                mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin6b)
plot(mcmc.diagnostics(Fin6b))
plot(gof(Fin6b))

Fin7 <- ergm(nw.coopFin ~ edges + edgecov(PBsFin) + 
               #nodeifactor ("OrgTypeFin", base=c(1,3,4)) + 
               #nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4)) +
                nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                nodematch ("OrgTypeFin", diff = T) + nodefactor("OrgTypeFin", base=-c(2,3,4,5)) + 
               edgecov (infFin) + nodeicov("Influence") +  
               mutual + gwodegree(2.5, fixed = TRUE) + twopath + gwesp(0.1 , fixed = TRUE),
             eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(Fin7)
plot(mcmc.diagnostics(Fin7))
plot(gof(Fin7))

par(mfrow=c(1,1))
par(mfrow=c(2,2))

texreg::screenreg(list(Fin4a,Fin4b,Fin5,Fin6a,Fin6b,Fin7),single.row = T)
texreg::screenreg(list(Fin1,Fin2,Fin3,Fin4,Fin5),single.row = T)

#4 actor types
texreg::htmlreg(list(Fin1,Fin2,Fin3,Fin4a,Fin4b,Fin5,Fin6a,Fin6b,Fin7),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/Fin_Rslts03Sept4.html",single.row = T)

#5 actor types
texreg::htmlreg(list(Fin1,Fin2,Fin3,Fin4a,Fin4b,Fin5,Fin6a,Fin6b,Fin7),file="/Users/paulwagner/Desktop/CHFinSK_US/CHFinSKUS Code 01_09/Results/Fin_Rslts03Sept5.html",single.row = T)



#-----------------





#when NGOs and CIV are the same category 14/03/18
m6.Fin1 <- ergm(nw.coopFin ~ edges + mutual + edgecov(PBsFin) + edgecov(fin.bus.civ) +
                  nodeifactor ("OrgTypeFin", base=c(1,2,4)) + 
                  nodeofactor ("OrgTypeFin", base=c(1,3,4)) + nodematch ("OrgTypeFin") + 
                  edgecov (infFin) + nodeicov("Influence") + absdiff ("Influence") + 
                  gwodegree(3, fixed = TRUE) +
                  twopath + gwesp(0.1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.Fin1)
plot(mcmc.diagnostics(m6.Fin1))
plot(gof(m6.Fin1))

#when NGOs and CIV are not the same category 14/03/18
m6.Fin1 <- ergm(nw.coopFin ~ edges + mutual + edgecov(PBsFin) + edgecov(fin.bus.ngo) +
                  nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                  nodeofactor ("OrgTypeFin", base=c(1,2,3,5)) + nodematch ("OrgTypeFin") + 
                  edgecov (infFin) + nodeicov("Influence") + absdiff ("Influence") + 
                  gwodegree(3, fixed = TRUE) +
                  twopath + gwesp(0.1 , fixed = TRUE ),
                eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 2500 , MCMC.interval = 2500))
summary(m6.Fin1)
plot(mcmc.diagnostics(m6.Fin1))
plot(gof(m6.Fin1))

#Older models 02/18
#"BUS" "CIV" "GOV" "NGO" "SCI"
# edgecov(SciFin) +
#absdiff ("Influence") + gwidegree(2 , fixed = TRUE ) +  gwodegree(0.5 , fixed = TRUE ) + twopath + 
m1.Fin <- ergm(nw.coopFin ~ edges + mutual + nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                 nodeofactor ("OrgTypeFin", base=c(1,2,3,5)) + nodematch ("OrgTypeFin") + 
                 edgecov (fin.bus.ngo) + edgecov (infFin) + nodeicov("Influence")  + 
                 edgecov(PBsFin) + absdiff ("Influence") + twopath + 
                 gwidegree(1.4, fixed = TRUE ) + gwodegree(3, fixed = TRUE ) + 
                 gwesp(0.2 , fixed = TRUE ),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m1.Fin)
plot(mcmc.diagnostics(m1.Fin))
plot(gof(m1.Fin))


#----------models -----------------------------------
m1.Fin  <- ergm(nw.coopFin  ~ edges,
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m1.Fin )
plot(mcmc.diagnostics(m1.Fin ))
plot(gof(m1.Fin ))

m2.Fin  <- ergm(nw.coopFin  ~ edges + mutual,
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m2.Fin)
plot(mcmc.diagnostics(m2.Fin))
plot(gof(m2.Fin))

m3.Fin  <- ergm(nw.coopFin  ~ edges + mutual + gwidegree(1.4 , fixed = TRUE ) + gwodegree(3, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m3.Fin)
plot(mcmc.diagnostics(m3.Fin))
plot(gof(m3.Fin))

m4.Fin  <- ergm(nw.coopFin  ~ edges + mutual + gwidegree(1.4 , fixed = TRUE ) + gwodegree(3, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsFin),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m4.Fin)
plot(mcmc.diagnostics(m4.Fin))
plot(gof(m4.Fin))

m5.Fin <- ergm(nw.coopFin ~ edges + mutual + gwidegree(1.4 , fixed = TRUE ) + gwodegree(3, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsFin) + nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeFin", base=c(1,2,3,5)) + nodematch ("OrgTypeFin"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m5.Fin)
plot(mcmc.diagnostics(m5.Fin))
plot(gof(m5.Fin))

m6.Fin <- ergm(nw.coopFin ~ edges + mutual + gwidegree(1.4 , fixed = TRUE ) + gwodegree(3, fixed = TRUE) +
                twopath + gwesp(1 , fixed = TRUE ) + edgecov(PBsFin) + edgecov(fin.bus.ngo) +
                 nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + 
                nodeofactor ("OrgTypeFin", base=c(1,2,3,5)) + nodematch ("OrgTypeFin") + 
                edgecov (infFin) + nodeicov("Influence") + absdiff ("Influence"),
              eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 2500 , MCMC.interval = 2500))
summary(m6.Fin)
plot(mcmc.diagnostics(m6.Fin))
plot(gof(m6.Fin))

texreg::htmlreg(list(m1.Fin,m2.Fin,m3.Fin,m4.Fin,m5.Fin,m6.Fin),file="/Users/paulwagner/Desktop/ERGMs/CHFinSK_US/US.html",single.row = T)




texreg::htmlreg(list(m6.Fin1,m6.KOR1,m6.US1),file="/Users/paulwagner/Desktop/ERGMs/CHFinSK_US/Models1.html",single.row = T)

#"BUS" "CIV" "GOV" "NGO" "SCI"
palette(c("blue","green","red","yellow", "orange"))
plot(nw.coopFin , displaylabels = FALSE , vertex.cex = degree(infFin, cmode = 'indegree') / 20 , vertex.border = "black", edge.col = "black", vertex.col = "OrgTypeFin",pad = 1)





#------ may 13th 
Fin.m1 <- ergm(nw.coopKor ~ edges + mutual + edgecov(PBsFin) + nodematch ("OrgTypeFin") +
                 nodeifactor ("OrgTypeFin", base=c(1,2,4,5)) + edgecov(Fin.bus.ngo) +
                 nodeicov("Influence") + absdiff ("Influence") +
                 edgecov(LowHighFin) + edgecov(LowHighFin) +
                 gwodegree(2.5, fixed = TRUE) +
                 gwdsp(0.1 , fixed = TRUE ) + gwesp(0.1 , fixed = TRUE ),
               eval.loglik = TRUE, check.degeneracy = TRUE, control = control.ergm ( seed = seed , MCMC.samplesize = 1000 , MCMC.interval = 1000))
summary(m6.Kor1)
