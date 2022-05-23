################
### BASELINE ###
################

#simulate 100 times, takes into account parameter uncertainty
simulLength <- 1000
store0 <- list()
for (i in 1:simulLength){
  #base parameters 
  params <-   list(
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity -- fecundity [[from digitized paper]]
    PhiU  =  runif(1,76.18577,81.52174),  
    PhiW1 = runif(1,83.89328,88.04348), 
    PhiW2 = runif(1,93.97233,99.30830), 
    #per capita death rates -- survivalTimes [[from digitized paper]]
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.19423,36.91445),#0.061, female Wild
    MiuMU  = 1/runif(1,12.56200,35.70369),#0.068, male Wild
    MiuFW1 = 1/runif(1,16.12368,42.58042),#0.068, female wAlb
    MiuMW1 = 1/runif(1,19.39853,48.04934),#0.068, male wAlb
    MiuFW2 = 1/runif(1,16.56467,42.88247),#0.068, female wMel
    MiuMW2 = 1/runif(1,18.19171,46.50823),#0.068, male wMel
    #population carrying capacity
    Ka     = 10000) 
  
  # baseline cases, no Wolb releases
  store0[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=0,
                                   MW=0,
                                   release="sustained",
                                   relError=0, 
                                   intervention="na", 
                                   intError=0, 
                                   interventionStart=0, 
                                   interventionStop=0,
                                   intFreq=0,
                                   intIntensity=0,
                                   releaseStart=0,
                                   releaseStop=0,
                                   releaseFreq=0,
                                   releaseIntensity=0,
                                   Params=params, 
                                   SIT=F)}


#plot and show stochasticity for one case, recommend to make into seperate function 
femaleMosU_baseline <- lapply(store0,function(x)x$Fu)
femaleMosU_baseline <- do.call(cbind,femaleMosU_baseline)
maleMosU_baseline <- lapply(store0,function(x)x$Mu)
maleMosU_baseline <- do.call(cbind,maleMosU_baseline)

femaleMosW1_baseline <- lapply(store0,function(x)x$Fw1)
femaleMosW1_baseline <- do.call(cbind,femaleMosW1_baseline)
maleMosW1_baseline <- lapply(store0,function(x)x$Mw1)
maleMosW1_baseline <- do.call(cbind,maleMosW1_baseline)

femaleMosW2_baseline <- lapply(store0,function(x)x$Fw2)
femaleMosW2_baseline <- do.call(cbind,femaleMosW2_baseline)
maleMosW2_baseline <- lapply(store0,function(x)x$Mw2)
maleMosW2_baseline <- do.call(cbind,maleMosW2_baseline)

###################
### SUPPRESSION ###
###################

# with suppression strategy, no intervention

#simulate 100 times, takes into account parameter uncertainty
store1 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store1[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="na", 
                                   intError=0, 
                                   interventionStart=0, 
                                   interventionStop=0,
                                   intFreq=0,
                                   intIntensity=0,
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params, 
                                   SIT=F)}

femaleMosU_supp<- lapply(store1,function(x)x$Fu)
femaleMosU_supp <- do.call(cbind,femaleMosU_supp)
maleMosU_supp<- lapply(store1,function(x)x$Mu)
maleMosU_supp <- do.call(cbind,maleMosU_supp)

femaleMosW1_supp<- lapply(store1,function(x)x$Fw1)
femaleMosW1_supp <- do.call(cbind,femaleMosW1_supp)
maleMosW1_supp<- lapply(store1,function(x)x$Mw1)
maleMosW1_supp <- do.call(cbind,maleMosW1_supp)

femaleMosW2_supp<- lapply(store1,function(x)x$Fw2)
femaleMosW2_supp <- do.call(cbind,femaleMosW2_supp)
maleMosW2_supp<- lapply(store1,function(x)x$Mw2)
maleMosW2_supp <- do.call(cbind,maleMosW2_supp)

##################################################################
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W ERROR) ###
##################################################################

#simulate 100 times, takes into account parameter uncertainty
store2 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store2[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="otherStrain", #otherStrain or overflooding
                                   intError=1/1000, #assume same as relError
                                   interventionStart=1700, 
                                   interventionStop=2400, #int Time (Days) of 1800 days
                                   intFreq=3.5,
                                   intIntensity=6,
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params, 
                                   SIT=F)}

femaleMosU_2ndstrainwerror<- lapply(store2,function(x)x$Fu)
femaleMosU_2ndstrainwerror <- do.call(cbind,femaleMosU_2ndstrainwerror)
maleMosU_2ndstrainwerror<- lapply(store2,function(x)x$Mu)
maleMosU_2ndstrainwerror <- do.call(cbind,maleMosU_2ndstrainwerror)

femaleMosW1_2ndstrainwerror<- lapply(store2,function(x)x$Fw1)
femaleMosW1_2ndstrainwerror <- do.call(cbind,femaleMosW1_2ndstrainwerror)
maleMosW1_2ndstrainwerror<- lapply(store2,function(x)x$Mw1)
maleMosW1_2ndstrainwerror <- do.call(cbind,maleMosW1_2ndstrainwerror)

femaleMosW2_2ndstrainwerror<- lapply(store2,function(x)x$Fw2)
femaleMosW2_2ndstrainwerror <- do.call(cbind,femaleMosW2_2ndstrainwerror)
maleMosW2_2ndstrainwerror<- lapply(store2,function(x)x$Mw2)
maleMosW2_2ndstrainwerror <- do.call(cbind,maleMosW2_2ndstrainwerror)

#################################################################
#  intINTENSITY = 4 (base=6)
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W ERROR) ###
##################################################################

#simulate 100 times, takes into account parameter uncertainty
store2_1.3 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store2_1.3[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="otherStrain", #otherStrain or overflooding
                                       intError=1/1000, #assume same as relError
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=4,
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=F)}

femaleMosU_2ndstrainwerror_1.3 <- lapply(store2_1.3,function(x)x$Fu)
femaleMosU_2ndstrainwerror_1.3 <- do.call(cbind,femaleMosU_2ndstrainwerror_1.3)
maleMosU_2ndstrainwerror_1.3 <- lapply(store2_1.3,function(x)x$Mu)
maleMosU_2ndstrainwerror_1.3 <- do.call(cbind,maleMosU_2ndstrainwerror_1.3)

femaleMosW1_2ndstrainwerror_1.3 <- lapply(store2_1.3,function(x)x$Fw1)
femaleMosW1_2ndstrainwerror_1.3 <- do.call(cbind,femaleMosW1_2ndstrainwerror_1.3)
maleMosW1_2ndstrainwerror_1.3 <- lapply(store2_1.3,function(x)x$Mw1)
maleMosW1_2ndstrainwerror_1.3 <- do.call(cbind,maleMosW1_2ndstrainwerror_1.3)

femaleMosW2_2ndstrainwerror_1.3 <- lapply(store2_1.3,function(x)x$Fw2)
femaleMosW2_2ndstrainwerror_1.3 <- do.call(cbind,femaleMosW2_2ndstrainwerror_1.3)
maleMosW2_2ndstrainwerror_1.3 <- lapply(store2_1.3,function(x)x$Mw2)
maleMosW2_2ndstrainwerror_1.3 <- do.call(cbind,maleMosW2_2ndstrainwerror_1.3)




#################################################################
# intINTENSITY  = 8 (base=6)
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W ERROR) ###
##################################################################

#simulate 100 times, takes into account parameter uncertainty
store2_1.6 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store2_1.6[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="otherStrain", #otherStrain or overflooding
                                       intError=1/1000, #assume same as relError
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=8,
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=F)}

femaleMosU_2ndstrainwerror_1.6 <- lapply(store2_1.6,function(x)x$Fu)
femaleMosU_2ndstrainwerror_1.6 <- do.call(cbind,femaleMosU_2ndstrainwerror_1.6)
maleMosU_2ndstrainwerror_1.6 <- lapply(store2_1.6,function(x)x$Mu)
maleMosU_2ndstrainwerror_1.6 <- do.call(cbind,maleMosU_2ndstrainwerror_1.6)

femaleMosW1_2ndstrainwerror_1.6 <- lapply(store2_1.6,function(x)x$Fw1)
femaleMosW1_2ndstrainwerror_1.6 <- do.call(cbind,femaleMosW1_2ndstrainwerror_1.6)
maleMosW1_2ndstrainwerror_1.6 <- lapply(store2_1.6,function(x)x$Mw1)
maleMosW1_2ndstrainwerror_1.6 <- do.call(cbind,maleMosW1_2ndstrainwerror_1.6)

femaleMosW2_2ndstrainwerror_1.6 <- lapply(store2_1.6,function(x)x$Fw2)
femaleMosW2_2ndstrainwerror_1.6 <- do.call(cbind,femaleMosW2_2ndstrainwerror_1.6)
maleMosW2_2ndstrainwerror_1.6 <- lapply(store2_1.6,function(x)x$Mw2)
maleMosW2_2ndstrainwerror_1.6 <- do.call(cbind,maleMosW2_2ndstrainwerror_1.6)






#####################################################################
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W NO ERROR) ###
#####################################################################

#simulate 100 times, takes into account parameter uncertainty
store3 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store3[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="otherStrain", #otherStrain or overflooding
                                   intError=0, #no error
                                   interventionStart=1700, 
                                   interventionStop=2400, #int Time (Days) of 1800 days
                                   intFreq=3.5,
                                   intIntensity=6,
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params, 
                                   SIT=F)}

femaleMosU_2ndstrainnoerror<- lapply(store3,function(x)x$Fu)
femaleMosU_2ndstrainnoerror <- do.call(cbind,femaleMosU_2ndstrainnoerror)
maleMosU_2ndstrainnoerror<- lapply(store3,function(x)x$Mu)
maleMosU_2ndstrainnoerror <- do.call(cbind,maleMosU_2ndstrainnoerror)

femaleMosW1_2ndstrainnoerror<- lapply(store3,function(x)x$Fw1)
femaleMosW1_2ndstrainnoerror <- do.call(cbind,femaleMosW1_2ndstrainnoerror)
maleMosW1_2ndstrainnoerror<- lapply(store3,function(x)x$Mw1)
maleMosW1_2ndstrainnoerror <- do.call(cbind,maleMosW1_2ndstrainnoerror)

femaleMosW2_2ndstrainnoerror<- lapply(store3,function(x)x$Fw2)
femaleMosW2_2ndstrainnoerror <- do.call(cbind,femaleMosW2_2ndstrainnoerror)
maleMosW2_2ndstrainnoerror<- lapply(store3,function(x)x$Mw2)
maleMosW2_2ndstrainnoerror <- do.call(cbind,maleMosW2_2ndstrainnoerror)


#################################################################
#  intINTENSITY  = 4 (base=6)
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W NO ERROR) ###
#####################################################################

#simulate 100 times, takes into account parameter uncertainty
store3_1.3 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store3_1.3[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="otherStrain", #otherStrain or overflooding
                                       intError=0, #no error
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=4,
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params,
                                       SIT=F)}

femaleMosU_2ndstrainnoerror_1.3 <- lapply(store3_1.3,function(x)x$Fu)
femaleMosU_2ndstrainnoerror_1.3 <- do.call(cbind,femaleMosU_2ndstrainnoerror_1.3)
maleMosU_2ndstrainnoerror_1.3 <- lapply(store3_1.3,function(x)x$Mu)
maleMosU_2ndstrainnoerror_1.3 <- do.call(cbind,maleMosU_2ndstrainnoerror_1.3)

femaleMosW1_2ndstrainnoerror_1.3 <- lapply(store3_1.3,function(x)x$Fw1)
femaleMosW1_2ndstrainnoerror_1.3 <- do.call(cbind,femaleMosW1_2ndstrainnoerror_1.3)
maleMosW1_2ndstrainnoerror_1.3 <- lapply(store3_1.3,function(x)x$Mw1)
maleMosW1_2ndstrainnoerror_1.3 <- do.call(cbind,maleMosW1_2ndstrainnoerror_1.3)

femaleMosW2_2ndstrainnoerror_1.3 <- lapply(store3_1.3,function(x)x$Fw2)
femaleMosW2_2ndstrainnoerror_1.3 <- do.call(cbind,femaleMosW2_2ndstrainnoerror_1.3)
maleMosW2_2ndstrainnoerror_1.3 <- lapply(store3_1.3,function(x)x$Mw2)
maleMosW2_2ndstrainnoerror_1.3 <- do.call(cbind,maleMosW2_2ndstrainnoerror_1.3)



#################################################################
# intINTENSITY = 8 (base=6)
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W NO ERROR) ###
#####################################################################

#simulate 100 times, takes into account parameter uncertainty
store3_1.6 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store3_1.6[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="otherStrain", #otherStrain or overflooding
                                       intError=0, #no error
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=8,
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=F)}

femaleMosU_2ndstrainnoerror_1.6 <- lapply(store3_1.6,function(x)x$Fu)
femaleMosU_2ndstrainnoerror_1.6 <- do.call(cbind,femaleMosU_2ndstrainnoerror_1.6)
maleMosU_2ndstrainnoerror_1.6 <- lapply(store3_1.6,function(x)x$Mu)
maleMosU_2ndstrainnoerror_1.6 <- do.call(cbind,maleMosU_2ndstrainnoerror_1.6)

femaleMosW1_2ndstrainnoerror_1.6 <- lapply(store3_1.6,function(x)x$Fw1)
femaleMosW1_2ndstrainnoerror_1.6 <- do.call(cbind,femaleMosW1_2ndstrainnoerror_1.6)
maleMosW1_2ndstrainnoerror_1.6 <- lapply(store3_1.6,function(x)x$Mw1)
maleMosW1_2ndstrainnoerror_1.6 <- do.call(cbind,maleMosW1_2ndstrainnoerror_1.6)

femaleMosW2_2ndstrainnoerror_1.6 <- lapply(store3_1.6,function(x)x$Fw2)
femaleMosW2_2ndstrainnoerror_1.6 <- do.call(cbind,femaleMosW2_2ndstrainnoerror_1.6)
maleMosW2_2ndstrainnoerror_1.6 <- lapply(store3_1.6,function(x)x$Mw2)
maleMosW2_2ndstrainnoerror_1.6 <- do.call(cbind,maleMosW2_2ndstrainnoerror_1.6)


#################################################################
# intINTENSITY = 2 (base=6), 10 000 releases per week
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W NO ERROR) ###
#####################################################################

#simulate 100 times, takes into account parameter uncertainty
store3_10k <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store3_10k[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="otherStrain", #otherStrain or overflooding
                                       intError=0, #no error
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=2,
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=F)}

femaleMosU_2ndstrainnoerror_10k <- lapply(store3_10k,function(x)x$Fu)
femaleMosU_2ndstrainnoerror_10k <- do.call(cbind,femaleMosU_2ndstrainnoerror_10k)
maleMosU_2ndstrainnoerror_10k <- lapply(store3_10k,function(x)x$Mu)
maleMosU_2ndstrainnoerror_10k <- do.call(cbind,maleMosU_2ndstrainnoerror_10k)

femaleMosW1_2ndstrainnoerror_10k <- lapply(store3_10k,function(x)x$Fw1)
femaleMosW1_2ndstrainnoerror_10k <- do.call(cbind,femaleMosW1_2ndstrainnoerror_10k)
maleMosW1_2ndstrainnoerror_10k <- lapply(store3_10k,function(x)x$Mw1)
maleMosW1_2ndstrainnoerror_10k <- do.call(cbind,maleMosW1_2ndstrainnoerror_10k)

femaleMosW2_2ndstrainnoerror_10k <- lapply(store3_10k,function(x)x$Fw2)
femaleMosW2_2ndstrainnoerror_10k <- do.call(cbind,femaleMosW2_2ndstrainnoerror_10k)
maleMosW2_2ndstrainnoerror_10k <- lapply(store3_10k,function(x)x$Mw2)
maleMosW2_2ndstrainnoerror_10k <- do.call(cbind,maleMosW2_2ndstrainnoerror_10k)




##########################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W ERROR) ###
#########################################################

#simulate 100 times, takes into account parameter uncertainty
store4 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store4[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="overflooding", #overflooding
                                   intError=1/1000, #same error as release
                                   interventionStart=1700, 
                                   interventionStop=2400, #int Time (Days) of 1800 days
                                   intFreq=3.5,
                                   intIntensity=6, # overflooding ratio of 6:1
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params,
                                   SIT=F)}

femaleMosU_overfloodingwerror<- lapply(store4,function(x)x$Fu)
femaleMosU_overfloodingwerror <- do.call(cbind,femaleMosU_overfloodingwerror)
maleMosU_overfloodingwerror<- lapply(store4,function(x)x$Mu)
maleMosU_overfloodingwerror <- do.call(cbind,maleMosU_overfloodingwerror)

femaleMosW1_overfloodingwerror<- lapply(store4,function(x)x$Fw1)
femaleMosW1_overfloodingwerror <- do.call(cbind,femaleMosW1_overfloodingwerror)
maleMosW1_overfloodingwerror<- lapply(store4,function(x)x$Mw1)
maleMosW1_overfloodingwerror <- do.call(cbind,maleMosW1_overfloodingwerror)

femaleMosW2_overfloodingwerror<- lapply(store4,function(x)x$Fw2)
femaleMosW2_overfloodingwerror <- do.call(cbind,femaleMosW2_overfloodingwerror)
maleMosW2_overfloodingwerror<- lapply(store4,function(x)x$Mw2)
maleMosW2_overfloodingwerror <- do.call(cbind,maleMosW2_overfloodingwerror)


#################################################################
# intINTENSITY = 4 (base=6)
### SUPPRESSION + INTERVENTION (OVERFLOODING W ERROR) ###
#########################################################

#simulate 100 times, takes into account parameter uncertainty
store4_1.3 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store4_1.3[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=1/1000, #same error as release
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=4, #5*2 overflooding ratio of 5:1
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params,
                                       SIT=F)}

femaleMosU_overfloodingwerror_1.3 <- lapply(store4_1.3,function(x)x$Fu)
femaleMosU_overfloodingwerror_1.3 <- do.call(cbind,femaleMosU_overfloodingwerror_1.3)
maleMosU_overfloodingwerror_1.3 <- lapply(store4_1.3,function(x)x$Mu)
maleMosU_overfloodingwerror_1.3 <- do.call(cbind,maleMosU_overfloodingwerror_1.3)

femaleMosW1_overfloodingwerror_1.3 <- lapply(store4_1.3,function(x)x$Fw1)
femaleMosW1_overfloodingwerror_1.3 <- do.call(cbind,femaleMosW1_overfloodingwerror_1.3)
maleMosW1_overfloodingwerror_1.3 <- lapply(store4_1.3,function(x)x$Mw1)
maleMosW1_overfloodingwerror_1.3 <- do.call(cbind,maleMosW1_overfloodingwerror_1.3)

femaleMosW2_overfloodingwerror_1.3 <- lapply(store4_1.3,function(x)x$Fw2)
femaleMosW2_overfloodingwerror_1.3 <- do.call(cbind,femaleMosW2_overfloodingwerror_1.3)
maleMosW2_overfloodingwerror_1.3 <- lapply(store4_1.3,function(x)x$Mw2)
maleMosW2_overfloodingwerror_1.3 <- do.call(cbind,maleMosW2_overfloodingwerror_1.3)



#################################################################
#  intINTENSITY = 8 (base=6)
### SUPPRESSION + INTERVENTION (OVERFLOODING W ERROR) ###
#########################################################

#simulate 100 times, takes into account parameter uncertainty
store4_1.6 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store4_1.6[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=1/1000, #same error as release
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=8,
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=F)}

femaleMosU_overfloodingwerror_1.6 <- lapply(store4_1.6,function(x)x$Fu)
femaleMosU_overfloodingwerror_1.6 <- do.call(cbind,femaleMosU_overfloodingwerror_1.6)
maleMosU_overfloodingwerror_1.6 <- lapply(store4_1.6,function(x)x$Mu)
maleMosU_overfloodingwerror_1.6 <- do.call(cbind,maleMosU_overfloodingwerror_1.6)

femaleMosW1_overfloodingwerror_1.6 <- lapply(store4_1.6,function(x)x$Fw1)
femaleMosW1_overfloodingwerror_1.6 <- do.call(cbind,femaleMosW1_overfloodingwerror_1.6)
maleMosW1_overfloodingwerror_1.6 <- lapply(store4_1.6,function(x)x$Mw1)
maleMosW1_overfloodingwerror_1.6 <- do.call(cbind,maleMosW1_overfloodingwerror_1.6)

femaleMosW2_overfloodingwerror_1.6 <- lapply(store4_1.6,function(x)x$Fw2)
femaleMosW2_overfloodingwerror_1.6 <- do.call(cbind,femaleMosW2_overfloodingwerror_1.6)
maleMosW2_overfloodingwerror_1.6 <- lapply(store4_1.6,function(x)x$Mw2)
maleMosW2_overfloodingwerror_1.6 <- do.call(cbind,maleMosW2_overfloodingwerror_1.6)








############################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR) ###
############################################################

#simulate 100 times, takes into account parameter uncertainty
store5 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="overflooding", #overflooding
                                   intError=0, #zero error, irradiated males
                                   interventionStart=1700, 
                                   interventionStop=2400, #int Time (Days) of 1800 days
                                   intFreq=3.5,
                                   intIntensity=6, # overflooding ratio of 5:1
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params, 
                                   SIT=F)}

femaleMosU_overfloodingnoerror<- lapply(store5,function(x)x$Fu)
femaleMosU_overfloodingnoerror <- do.call(cbind,femaleMosU_overfloodingnoerror)
maleMosU_overfloodingnoerror<- lapply(store5,function(x)x$Mu)
maleMosU_overfloodingnoerror <- do.call(cbind,maleMosU_overfloodingnoerror)

femaleMosW1_overfloodingnoerror<- lapply(store5,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror <- do.call(cbind,femaleMosW1_overfloodingnoerror)
maleMosW1_overfloodingnoerror<- lapply(store5,function(x)x$Mw1)
maleMosW1_overfloodingnoerror <- do.call(cbind,maleMosW1_overfloodingnoerror)

femaleMosW2_overfloodingnoerror<- lapply(store5,function(x)x$Fw2)
femaleMosW2_overfloodingnoerror <- do.call(cbind,femaleMosW2_overfloodingnoerror)
maleMosW2_overfloodingnoerror<- lapply(store5,function(x)x$Mw2)
maleMosW2_overfloodingnoerror <- do.call(cbind,maleMosW2_overfloodingnoerror)


#################################################################
#  intINTENSITY = 4 (base=6)
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR) ###
############################################################

#simulate 100 times, takes into account parameter uncertainty
store5_1.3 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5_1.3[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=0, #zero error, irradiated males
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=4, # 
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=F)}

femaleMosU_overfloodingnoerror_1.3 <- lapply(store5_1.3,function(x)x$Fu)
femaleMosU_overfloodingnoerror_1.3 <- do.call(cbind,femaleMosU_overfloodingnoerror_1.3)
maleMosU_overfloodingnoerror_1.3 <- lapply(store5_1.3,function(x)x$Mu)
maleMosU_overfloodingnoerror_1.3 <- do.call(cbind,maleMosU_overfloodingnoerror_1.3)

femaleMosW1_overfloodingnoerror_1.3 <- lapply(store5_1.3,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror_1.3 <- do.call(cbind,femaleMosW1_overfloodingnoerror_1.3)
maleMosW1_overfloodingnoerror_1.3 <- lapply(store5_1.3,function(x)x$Mw1)
maleMosW1_overfloodingnoerror_1.3 <- do.call(cbind,maleMosW1_overfloodingnoerror_1.3)

femaleMosW2_overfloodingnoerror_1.3 <- lapply(store5_1.3,function(x)x$Fw2)
femaleMosW2_overfloodingnoerror_1.3 <- do.call(cbind,femaleMosW2_overfloodingnoerror_1.3)
maleMosW2_overfloodingnoerror_1.3 <- lapply(store5_1.3,function(x)x$Mw2)
maleMosW2_overfloodingnoerror_1.3 <- do.call(cbind,maleMosW2_overfloodingnoerror_1.3)


#################################################################
#  intINTENSITY = 8 (base=6)
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR) ###
############################################################

#simulate 100 times, takes into account parameter uncertainty
store5_1.6 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5_1.6[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=0, #zero error, irradiated males
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=8, # overflooding ratio of 8:1
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params,
                                       SIT=F)}

femaleMosU_overfloodingnoerror_1.6 <- lapply(store5_1.6,function(x)x$Fu)
femaleMosU_overfloodingnoerror_1.6 <- do.call(cbind,femaleMosU_overfloodingnoerror_1.6)
maleMosU_overfloodingnoerror_1.6 <- lapply(store5_1.6,function(x)x$Mu)
maleMosU_overfloodingnoerror_1.6 <- do.call(cbind,maleMosU_overfloodingnoerror_1.6)

femaleMosW1_overfloodingnoerror_1.6 <- lapply(store5_1.6,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror_1.6 <- do.call(cbind,femaleMosW1_overfloodingnoerror_1.6)
maleMosW1_overfloodingnoerror_1.6 <- lapply(store5_1.6,function(x)x$Mw1)
maleMosW1_overfloodingnoerror_1.6 <- do.call(cbind,maleMosW1_overfloodingnoerror_1.6)

femaleMosW2_overfloodingnoerror_1.6 <- lapply(store5_1.6,function(x)x$Fw2)
femaleMosW2_overfloodingnoerror_1.6 <- do.call(cbind,femaleMosW2_overfloodingnoerror_1.6)
maleMosW2_overfloodingnoerror_1.6 <- lapply(store5_1.6,function(x)x$Mw2)
maleMosW2_overfloodingnoerror_1.6 <- do.call(cbind,maleMosW2_overfloodingnoerror_1.6)



############################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR) ###
###################### SIT ##################################

#simulate 100 times, takes into account parameter uncertainty
store5_sit <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5_sit[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=0, #zero error, irradiated males
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1800 days
                                       intFreq=3.5,
                                       intIntensity=6, 
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=T)}

femaleMosU_overfloodingnoerror_sit <- lapply(store5_sit,function(x)x$Fu)
femaleMosU_overfloodingnoerror_sit <- do.call(cbind,femaleMosU_overfloodingnoerror_sit)
maleMosU_overfloodingnoerror_sit <- lapply(store5_sit,function(x)x$Mu)
maleMosU_overfloodingnoerror_sit <- do.call(cbind,maleMosU_overfloodingnoerror_sit)

femaleMosW1_overfloodingnoerror_sit <- lapply(store5_sit,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror_sit <- do.call(cbind,femaleMosW1_overfloodingnoerror_sit)
maleMosW1_overfloodingnoerror_sit <- lapply(store5_sit,function(x)x$Mw1)
maleMosW1_overfloodingnoerror_sit <- do.call(cbind,maleMosW1_overfloodingnoerror_sit)

femaleMosW2_overfloodingnoerror_sit <- lapply(store5_sit,function(x)x$Fw2)
femaleMosW2_overfloodingnoerror_sit <- do.call(cbind,femaleMosW2_overfloodingnoerror_sit)
maleMosW2_overfloodingnoerror_sit <- lapply(store5_sit,function(x)x$Mw2)
maleMosW2_overfloodingnoerror_sit <- do.call(cbind,maleMosW2_overfloodingnoerror_sit)

#################################################################
#  intINTENSITY = 4 (base=6)
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR)  SIT ###
############################################################

###########################
#simulate 100 times, takes into account parameter uncertainty
store5_sit_1.3 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5_sit_1.3[[i]]   <- mozzieModelStoc(t=3500,             
                                           FW=5,
                                           MW=5000, #5000 biweekly meaning 10000 per week
                                           release="sustained",
                                           relError=1/1000, #release error rate of 1/1000
                                           
                                           intervention="overflooding", #overflooding
                                           intError=0, #zero error, irradiated males
                                           interventionStart=1700, 
                                           interventionStop=2400, #int Time (Days) of 1800 days
                                           intFreq=3.5,
                                           intIntensity=4, 
                                           
                                           releaseStart=20,
                                           releaseStop=1820,
                                           releaseFreq=3.5,#biweekly release; twice a week
                                           releaseIntensity=1,
                                           Params=params, 
                                           SIT=T)}

femaleMosW1_overfloodingnoerror_sit_1.3 <- lapply(store5_sit_1.3,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror_sit_1.3 <- do.call(cbind,femaleMosW1_overfloodingnoerror_sit_1.3)


#################################################################
#  intINTENSITY = 8 (base=6)
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR)  SIT ###
############################################################

###########################
#simulate 100 times, takes into account parameter uncertainty
store5_sit_1.6 <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5_sit_1.6[[i]]   <- mozzieModelStoc(t=3500,             
                                           FW=5,
                                           MW=5000, #5000 biweekly meaning 10000 per week
                                           release="sustained",
                                           relError=1/1000, #release error rate of 1/1000
                                           
                                           intervention="overflooding", #overflooding
                                           intError=0, #zero error, irradiated males
                                           interventionStart=1700, 
                                           interventionStop=2400, #int Time (Days) of 1800 days
                                           intFreq=3.5,
                                           intIntensity=8, 
                                           
                                           releaseStart=20,
                                           releaseStop=1820,
                                           releaseFreq=3.5,#biweekly release; twice a week
                                           releaseIntensity=1,
                                           Params=params, 
                                           SIT=T)}

femaleMosW1_overfloodingnoerror_sit_1.6 <- lapply(store5_sit_1.6,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror_sit_1.6 <- do.call(cbind,femaleMosW1_overfloodingnoerror_sit_1.6)


#################################################################
#  intINTENSITY = 2 (base=6), to simulate 10000 per week only
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR)  SIT ###
############################################################

###########################
#simulate 100 times, takes into account parameter uncertainty
store5_sit_10k <- list()
for (i in 1:simulLength){
  #base parameters 
  #please check parameter ranges
  params <-   list(#parameters, take from Table 2
    #Fraction of unviable offspring due to cytoplasmic incompatibility
    C_uw1 = 1,      
    C_uw2 = 1,   
    C_w1u = runif(1,0.337,0.417),       
    C_w2u = runif(1,0.182,0.262),       
    C_w1w1 = runif(1,0.165,0.285),       
    C_w2w2 = runif(1,0.12,0.2),       
    #fraction female births
    Bf = 0.5, #runif(1,0.34,0.6)
    Bm = 0.5,
    #Per capita development rate of mosquito eggs
    Psi  = 0.01,
    #Fraction of infected mosquito eggs produced by infected female mosquitoes
    Vw = 0.9, #runif(1,0,1)
    Vu = 1 - 0.9, #1  - Vw
    #egg laying capacity
    PhiU  =  runif(1,76.18577,81.52174),  #from digitized paper
    PhiW1 = runif(1,83.89328,88.04348), #from digitized paper
    PhiW2 = runif(1,93.97233,99.30830), #from digitized paper
    #per capita death rates
    MiuA   = 0.02,
    MiuFU  = 1/runif(1,14.74923,56.49701),#0.061, 
    MiuMU  = 1/runif(1,22.95297,69.31650),#0.068, 
    MiuFW1 = 1/runif(1,20.16475,68.65655),#0.068, 
    MiuMW1 = 1/runif(1,17.13717,61.62285),#0.068,
    MiuFW2 = 1/runif(1,18.76872,62.53389),#0.068, 
    MiuMW2 = 1/runif(1,19.68899,63.44167),#0.068, 
    #population carrying capacity
    Ka     = 10000) 
  
  store5_sit_10k[[i]]   <- mozzieModelStoc(t=3500,             
                                           FW=5,
                                           MW=5000, #5000 biweekly meaning 10000 per week
                                           release="sustained",
                                           relError=1/1000, #release error rate of 1/1000
                                           
                                           intervention="overflooding", #overflooding
                                           intError=0, #zero error, irradiated males
                                           interventionStart=1700, 
                                           interventionStop=2400, #int Time (Days) of 1800 days
                                           intFreq=3.5,
                                           intIntensity=2, 
                                           
                                           releaseStart=20,
                                           releaseStop=1820,
                                           releaseFreq=3.5,#biweekly release; twice a week
                                           releaseIntensity=1,
                                           Params=params, 
                                           SIT=T)}

femaleMosW1_overfloodingnoerror_sit_10k <- lapply(store5_sit_10k,function(x)x$Fw1)
femaleMosW1_overfloodingnoerror_sit_10k <- do.call(cbind,femaleMosW1_overfloodingnoerror_sit_10k)



#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
##############################   PLOTS  ###############################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

########################
### WILD TYPE FEMALE ###
########################
col2rgb("black")

#baseline
par(mar = c(3.5, 6, 0.5, 4))  
plot(x=c(0,3500),y=range(femaleMosU_baseline), #range(femaleMosU_supp)
     col="white", las=1, xlab="", ylab="")
title(ylab="Wild Female Population", line=3.1)
title(xlab="Time (Days)", line=2.2)

for (i in 1:ncol(femaleMosU_baseline))
{lines(x=c(0:3500),y=femaleMosU_baseline[,i], col=rgb(1,0,0,alpha=0.03))}

femaleMosU_baseline_median = apply(femaleMosU_baseline, 1, median, na.rm=T)
lines(femaleMosU_baseline_median, col="red3")

#suppresion
for (i in 1:ncol(femaleMosU_supp))
{lines(x=c(0:3500),y=femaleMosU_supp[,i], col=rgb(0,0,0,alpha=0.04))}
femaleMosU_supp_median = apply(femaleMosU_supp, 1, median, na.rm=T)
lines(femaleMosU_supp_median, col="black")


#second strain with no error (intIntensity=4)
for (i in 1:ncol(femaleMosU_2ndstrainnoerror_1.3))
{lines(x=c(0:3500),y=femaleMosU_2ndstrainnoerror_1.3[,i],
       col=rgb(60/255,179/255,113/255,alpha=0.03))}
femaleMosU_2ndstrainnoerror_1.3_median = apply(femaleMosU_2ndstrainnoerror_1.3, 1, median, na.rm=T)
lines(femaleMosU_2ndstrainnoerror_1.3_median, col="lightgreen")

#second strain with no error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosU_2ndstrainnoerror))
{lines(x=c(0:3500),y=femaleMosU_2ndstrainnoerror[,i],
       col=rgb(144/255,238/255,14/255,alpha=0.03))}
femaleMosU_2ndstrainnoerror_median = apply(femaleMosU_2ndstrainnoerror, 1, median, na.rm=T)
lines(femaleMosU_2ndstrainnoerror_median, col="mediumseagreen")

#second strain with no error (intIntensity=8)
for (i in 1:ncol(femaleMosU_2ndstrainnoerror_1.6))
{lines(x=c(0:3500),y=femaleMosU_2ndstrainnoerror_1.6[,i],
       col=rgb(0/255,100/255,0/255,alpha=0.03))}
femaleMosU_2ndstrainnoerror_1.6_median = apply(femaleMosU_2ndstrainnoerror_1.6, 1, median, na.rm=T)
lines(femaleMosU_2ndstrainnoerror_1.6_median, col="darkgreen")



#overflooding with no error (intIntensity=4)
for (i in 1:ncol(femaleMosU_overfloodingnoerror_1.3))
{lines(x=c(0:3500),y=femaleMosU_overfloodingnoerror_1.3[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosU_overfloodingnoerror_1.3_median = apply(femaleMosU_overfloodingnoerror_1.3, 1, median, na.rm=T)
lines(femaleMosU_overfloodingnoerror_1.3_median, col="orange")

#overflooding with no error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosU_overfloodingnoerror))
{lines(x=c(0:3500),y=femaleMosU_overfloodingnoerror[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosU_overfloodingnoerror_median = apply(femaleMosU_overfloodingnoerror, 1, median, na.rm=T)
lines(femaleMosU_overfloodingnoerror_median, col="darkorange")

#overflooding with no error (intIntensity=8)
for (i in 1:ncol(femaleMosU_overfloodingnoerror_1.6))
{lines(x=c(0:3500),y=femaleMosU_overfloodingnoerror_1.6[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosU_overfloodingnoerror_1.6_median = apply(femaleMosU_overfloodingnoerror_1.6, 1, median, na.rm=T)
lines(femaleMosU_overfloodingnoerror_1.6_median, col="orangered")


####################
### wAlbB FEMALE ###
####################
#baseline
par(mar = c(3.5, 6, 0.5, 4))  
plot(x=c(0,3500),y=range(femaleMosW1_supp),
     col="white", las=1, xlab="", ylab="")
title(ylab="wAlbB Female Population", line=3.1)
title(xlab="Time (Days)", line=2.2)

femaleMosW1_baseline_median = apply(femaleMosW1_baseline, 1, median, na.rm=T)
lines(femaleMosW1_baseline_median, col="red3")

#suppresion
for (i in 1:ncol(femaleMosW1_supp))
{lines(x=c(0:3500),y=femaleMosW1_supp[,i], col=rgb(0,0,0,alpha=0.04))}

femaleMosW1_supp_median = apply(femaleMosW1_supp, 1, median, na.rm=T)
lines(femaleMosW1_supp_median, col="black")


#second strain with error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_2ndstrainwerror_1.3))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainwerror_1.3[,i],
       col=	rgb(65/255,105/255,225/255,alpha=0.03))}
femaleMosW1_2ndstrainwerror_1.3_median = apply(femaleMosW1_2ndstrainwerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainwerror_1.3_median, col="cornflowerblue")

#second strain with error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_2ndstrainwerror))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainwerror[,i],
       col=	rgb(100/255,149/255,237/255,alpha=0.03))}
femaleMosW1_2ndstrainwerror_median = apply(femaleMosW1_2ndstrainwerror, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainwerror_median, col="royalblue")

#second strain with error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_2ndstrainwerror_1.6))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainwerror_1.6[,i],
       col=	rgb(0/255,0/255,128/255,alpha=0.03))}
femaleMosW1_2ndstrainwerror_1.3_median = apply(femaleMosW1_2ndstrainwerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainwerror_1.3_median, col="navyblue")


#second strain with no error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror_1.3))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror_1.3[,i],
       col=rgb(60/255,179/255,113/255,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_1.3_median = apply(femaleMosW1_2ndstrainnoerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_1.3_median, col="lightgreen")

#second strain with no error  (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror[,i],
       col=rgb(144/255,238/255,14/255,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_median = apply(femaleMosW1_2ndstrainnoerror, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_median, col="mediumseagreen")

#second strain with no error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror_1.6))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror_1.6[,i],
       col=rgb(0/255,100/255,0/255,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_1.6_median = apply(femaleMosW1_2ndstrainnoerror_1.6, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_1.6_median, col="darkgreen")

#second strain with no error (intIntensity=2), 10k release for sz
for (i in 1:ncol(femaleMosW1_2ndstrainnoerror_10k))
{lines(x=c(0:3500),y=femaleMosW1_2ndstrainnoerror_10k[,i],
       col=rgb(139/255,139/255,0/255,alpha=0.03))}
femaleMosW1_2ndstrainnoerror_10k_median = apply(femaleMosW1_2ndstrainnoerror_10k, 1, median, na.rm=T)
lines(femaleMosW1_2ndstrainnoerror_10k_median, col="yellow4")


legend(x="center",legend=c("Suppression","Baseline", 
                           "Supression with second strain - lower intensity (no error)", 
                           "Supression with second strain - baseline (no error)", 
                           "Supression with second strain - higher intensity (no error)"),
       bty='n',
       col=c('black','red3', 'lightgreen', 'mediumseagreen', 'darkgreen'),
       pch=15, cex=0.75)


#overflooding with error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_overfloodingwerror_1.3))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror_1.3[,i],
       col=rgb(139/255,71/255,137/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_1.3_median = apply(femaleMosW1_overfloodingwerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_1.3_median, col="orchid4")

#overflooding with error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_overfloodingwerror))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror[,i],
       col=rgb(139/255,71/255,137/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_median = apply(femaleMosW1_overfloodingwerror, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_median, col="orchid4")

#overflooding with error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_overfloodingwerror_1.6))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingwerror_1.6[,i],
       col=rgb(139/255,71/255,137/255,alpha=0.03))}
femaleMosW1_overfloodingwerror_1.6_median = apply(femaleMosW1_overfloodingwerror_1.6, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingwerror_1.6_median, col="orchid4")



#overflooding with no error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_1.3))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_1.3[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_1.3_median = apply(femaleMosW1_overfloodingnoerror_1.3, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_1.3_median, col="orange")

#overflooding with no error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_median = apply(femaleMosW1_overfloodingnoerror, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_median, col="darkorange")

#overflooding with no error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_1.6))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_1.6[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_1.6_median = apply(femaleMosW1_overfloodingnoerror_1.6, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_1.6_median, col="orangered")

legend(x="topright",legend=c("Suppression","Baseline", 
                   "Supression with overflooding - lower intensity (no error)",
                   "Supression with overflooding - baseline (no error)", 
                   "Supression with overflooding - higher intensity (no error)"),
       bty='n',
       col=c('black','red3', 'orange', 'darkorange', 'orangered'),
       pch=15, cex=0.75)


#overflooding with no error (intIntensity=4)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit_1.3))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit_1.3[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_1.3_median = apply(femaleMosW1_overfloodingnoerror_sit_1.3, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_1.3_median, col="darkorange")

#overflooding with no error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_median = apply(femaleMosW1_overfloodingnoerror_sit, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_median, col="orange")

#overflooding with no error (intIntensity=8)
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit_1.6))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit_1.6[,i],
       col=rgb(255/255,165/255,0,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_1.6_median = apply(femaleMosW1_overfloodingnoerror_sit_1.6, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_1.6_median, col="orangered")

#overflooding with no error (intIntensity=2), 10k release for sz
for (i in 1:ncol(femaleMosW1_overfloodingnoerror_sit_10k))
{lines(x=c(0:3500),y=femaleMosW1_overfloodingnoerror_sit_10k[,i],
       col=rgb(238/255,162/255,173/225,alpha=0.03))}
femaleMosW1_overfloodingnoerror_sit_10k_median = apply(femaleMosW1_overfloodingnoerror_sit_10k, 1, median, na.rm=T)
lines(femaleMosW1_overfloodingnoerror_sit_10k_median, col="lightpink2")


plot(x=c(0,3500),y=range(femaleMosW2_2ndstrainwerror),
     col="white", las=1, xlab="", ylab="")

#
legend(x="topright",legend=c("Baseline", 
                             expression(paste("Current suppression approach, Error = ","10"^"-3")), 
                             "SIT (wAlbB irradiation) - 40k/week, Error = 0",
                             "SIT (wAlbB irradiation) - 60k/week, Error = 0", 
                             "SIT (wAlbB irradiation) - 80k/week, Error = 0"),
       bty='n',
       col=c('red3','black', 'orange', 'darkorange', 'orangered'),
       pch=15, cex=0.75)



legend(x="center",legend=c("Baseline", 
                           expression(paste("Current suppression approach, Error = ","10"^"-3")),
                           "wMel introduction - 40k/week, Error = 0", 
                           "wMel introduction - 60k/week, Error = 0", 
                           "wMel introduction - 80k/week, Error = 0"),
       bty='n',
       col=c('red3','black', 'lightgreen', 'mediumseagreen', 'darkgreen'),
       pch=15, cex=0.75)



####################
### wMel FEMALE ###
####################
#baseline
par(mar = c(3.5, 6, 0.5, 4))  
plot(x=c(0,3500),y=range(femaleMosW2_2ndstrainwerror),
     col="white", las=1, xlab="", ylab="")
title(ylab="wMel Female Population", line=3.1)
title(xlab="Time (Days)", line=2.2)

femaleMosW2_baseline_median = apply(femaleMosW2_baseline, 1, median, na.rm=T)
lines(femaleMosW2_baseline_median, col="red3")

#suppresion
femaleMosW2_supp_median = apply(femaleMosW2_supp, 1, median, na.rm=T)
lines(femaleMosW2_supp_median, col="black")


#second strain with error (intIntensity=4)
for (i in 1:ncol(femaleMosW2_2ndstrainwerror_1.3))
{lines(x=c(0:3500),y=femaleMosW2_2ndstrainwerror_1.3[,i],
       col=	rgb(65/255,105/255,225/255,alpha=0.03))}
femaleMosW2_2ndstrainwerror_1.3_median = apply(femaleMosW2_2ndstrainwerror_1.3, 1, median, na.rm=T)
lines(femaleMosW2_2ndstrainwerror_1.3_median, col="cornflowerblue")

#second strain with error (baseline, intIntensity=6)
for (i in 1:ncol(femaleMosW2_2ndstrainwerror))
{lines(x=c(0:3500),y=femaleMosW2_2ndstrainwerror[,i],
       col=	rgb(100/255,149/255,237/255,alpha=0.03))}
femaleMosW2_2ndstrainwerror_median = apply(femaleMosW2_2ndstrainwerror, 1, median, na.rm=T)
lines(femaleMosW2_2ndstrainwerror_median, col="royalblue")

#second strain with error (intIntensity=8)
for (i in 1:ncol(femaleMosW2_2ndstrainwerror_1.6))
{lines(x=c(0:3500),y=femaleMosW2_2ndstrainwerror_1.6[,i],
       col=	rgb(0/255,0/255,128/255,alpha=0.03))}
femaleMosW2_2ndstrainwerror_1.6_median = apply(femaleMosW2_2ndstrainwerror_1.6, 1, median, na.rm=T)
lines(femaleMosW2_2ndstrainwerror_1.6_median, col="navyblue")

par(mar = c(3.5, 2, 0.5, 4))  
plot(x=c(0,3500),y=range(femaleMosW2_2ndstrainwerror),
     col="white", las=1, xlab="", ylab="")
legend(x="bottomright",legend=c("Suppression","Baseline", 
                                "Supression with wMel - 40k/week (Error = )", 
                                "Supression with wMel - 60k/week (EHI's error)", 
                                "Supression with wMel - 80k/week (EHI's error)"),
       bty='n',
       col=c('black','red3', 'cornflowerblue', 'royalblue', 'navyblue'),
       pch=15, cex=0.75)


########################
### WILD TYPE MALE ###
########################

########################
### wAlbB MALE ###
########################

########################
### wMel MALE ###
########################





