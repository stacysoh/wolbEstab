################
### BASELINE ###
################

#simulate 1000 times, takes into account parameter uncertainty
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
    #Per capita development rate of mosquito eggs (eclosion rate)
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

####################################
### SUPPRESSION  w EHI's error ###
##################################

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

###################################################
### SUPPRESSION with verily's smaller error ###
###################################################

# with suppression strategy, no intervention

#simulate 100 times, takes into account parameter uncertainty
store1_v <- list()
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
  
  store1_v[[i]]   <- mozzieModelStoc(t=3500,             
                                     FW=5,
                                     MW=5000, #5000 biweekly meaning 10000 per week
                                     release="sustained",
                                     relError=1e-09, #release error rate of 1/1000
                                     
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

femaleMosU_supp_v<- lapply(store1_v,function(x)x$Fu)
femaleMosU_supp_v <- do.call(cbind,femaleMosU_supp_v)
femaleMosW1_supp_v<- lapply(store1_v,function(x)x$Fw1)
femaleMosW1_supp_v <- do.call(cbind,femaleMosW1_supp_v)



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
                                   interventionStop=2400, #int Time (Days) of 1700 days
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
                                   interventionStop=2400, #int Time (Days) of 1700 days
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

##########################################################################
### SUPPRESSION + INTERVENTION (SECOND STRAIN RELEASE W VERILY ERROR) ###
#########################################################################

#simulate 100 times, takes into account parameter uncertainty
store6 <- list()
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
  
  store6[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="otherStrain", #otherStrain or overflooding
                                   intError=1e-09, #verily
                                   interventionStart=1700, 
                                   interventionStop=2400, #int Time (Days) of 1700 days
                                   intFreq=3.5,
                                   intIntensity=6,
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params,
                                   SIT=F)}

femaleMosU_2ndstrainwerror_verily<- lapply(store6,function(x)x$Fu)
femaleMosU_2ndstrainwerror_verily <- do.call(cbind,femaleMosU_2ndstrainwerror_verily)
maleMosU_2ndstrainwerror_verily<- lapply(store6,function(x)x$Mu)
maleMosU_2ndstrainwerror_verily <- do.call(cbind,maleMosU_2ndstrainwerror_verily)

femaleMosW1_2ndstrainwerror_verily<- lapply(store6,function(x)x$Fw1)
femaleMosW1_2ndstrainwerror_verily <- do.call(cbind,femaleMosW1_2ndstrainwerror_verily)
maleMosW1_2ndstrainwerror_verily<- lapply(store6,function(x)x$Mw1)
maleMosW1_2ndstrainwerror_verily <- do.call(cbind,maleMosW1_2ndstrainwerror_verily)

femaleMosW2_2ndstrainwerror_verily <- lapply(store6,function(x)x$Fw2)
femaleMosW2_2ndstrainwerror_verily <- do.call(cbind,femaleMosW2_2ndstrainwerror_verily)
maleMosW2_2ndstrainwerror_verily<- lapply(store6,function(x)x$Mw2)
maleMosW2_2ndstrainwerror_verily <- do.call(cbind,maleMosW2_2ndstrainwerror_verily)



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
                                   interventionStop=2400, #int Time (Days) of 1700 days
                                   intFreq=3.5,
                                   intIntensity=6, 
                                   
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
                                   interventionStop=2400, #int Time (Days) of 1700 days
                                   intFreq=3.5,
                                   intIntensity=6, 
                                   
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



################################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W VERILY ERROR) ###
################################################################

#simulate 100 times, takes into account parameter uncertainty
store7 <- list()
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
  
  store7[[i]]   <- mozzieModelStoc(t=3500,             
                                   FW=5,
                                   MW=5000, #5000 biweekly meaning 10000 per week
                                   release="sustained",
                                   relError=1/1000, #release error rate of 1/1000
                                   
                                   intervention="overflooding", #overflooding
                                   intError=1e-09, #verily error
                                   interventionStart=1700, 
                                   interventionStop=2400, #int Time (Days) of 1700 days
                                   intFreq=3.5,
                                   intIntensity=6, 
                                   
                                   releaseStart=20,
                                   releaseStop=1820,
                                   releaseFreq=3.5,#biweekly release; twice a week
                                   releaseIntensity=1,
                                   Params=params,
                                   SIT=F)}

femaleMosU_overfloodingwerror_verily<- lapply(store7,function(x)x$Fu)
femaleMosU_overfloodingwerror_verily <- do.call(cbind,femaleMosU_overfloodingwerror_verily)
maleMosU_overfloodingwerror_verily<- lapply(store7,function(x)x$Mu)
maleMosU_overfloodingwerror_verily <- do.call(cbind,maleMosU_overfloodingwerror_verily)

femaleMosW1_overfloodingwerror_verily <- lapply(store7,function(x)x$Fw1)
femaleMosW1_overfloodingwerror_verily <- do.call(cbind,femaleMosW1_overfloodingwerror_verily)
maleMosW1_overfloodingwerror_verily<- lapply(store7,function(x)x$Mw1)
maleMosW1_overfloodingwerror_verily <- do.call(cbind,maleMosW1_overfloodingwerror_verily)

femaleMosW2_overfloodingwerror_verily<- lapply(store7,function(x)x$Fw2)
femaleMosW2_overfloodingwerror_verily <- do.call(cbind,femaleMosW2_overfloodingwerror_verily)
maleMosW2_overfloodingwerror_verily<- lapply(store7,function(x)x$Mw2)
maleMosW2_overfloodingwerror_verily <- do.call(cbind,maleMosW2_overfloodingwerror_verily)

########################################################################
### IRRADIATION THAT STERILISES MALES AS WELL 
### SIT = T 
#########################################################################

##########################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W ERROR) ###
#########################################################
simulLength <- 1000
#simulate 100 times, takes into account parameter uncertainty
store4_sit <- list()
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
  
  store4_sit[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=1/1000, #same error as release
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1700 days
                                       intFreq=3.5,
                                       intIntensity=6, 
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params, 
                                       SIT=T)}

femaleMosU_overfloodingwerror_sit<- lapply(store4_sit,function(x)x$Fu)
femaleMosU_overfloodingwerror_sit <- do.call(cbind,femaleMosU_overfloodingwerror_sit)
maleMosU_overfloodingwerror_sit <- lapply(store4_sit,function(x)x$Mu)
maleMosU_overfloodingwerror_sit <- do.call(cbind,maleMosU_overfloodingwerror_sit)

femaleMosW1_overfloodingwerror_sit <- lapply(store4_sit,function(x)x$Fw1)
femaleMosW1_overfloodingwerror_sit <- do.call(cbind,femaleMosW1_overfloodingwerror_sit)
maleMosW1_overfloodingwerror_sit <- lapply(store4_sit,function(x)x$Mw1)
maleMosW1_overfloodingwerror_sit <- do.call(cbind,maleMosW1_overfloodingwerror_sit)

femaleMosW2_overfloodingwerror_sit <- lapply(store4_sit,function(x)x$Fw2)
femaleMosW2_overfloodingwerror_sit <- do.call(cbind,femaleMosW2_overfloodingwerror_sit)
maleMosW2_overfloodingwerror_sit <- lapply(store4_sit,function(x)x$Mw2)
maleMosW2_overfloodingwerror_sit <- do.call(cbind,maleMosW2_overfloodingwerror_sit)

############################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W NO ERROR) ###
############################################################

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
                                       interventionStop=2400, #int Time (Days) of 1700 days
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



################################################################
### SUPPRESSION + INTERVENTION (OVERFLOODING W VERILY ERROR) ###
################################################################

#simulate 100 times, takes into account parameter uncertainty
store7_sit <- list()
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
  
  store7_sit[[i]]   <- mozzieModelStoc(t=3500,             
                                       FW=5,
                                       MW=5000, #5000 biweekly meaning 10000 per week
                                       release="sustained",
                                       relError=1/1000, #release error rate of 1/1000
                                       
                                       intervention="overflooding", #overflooding
                                       intError=1e-09, #verily error
                                       interventionStart=1700, 
                                       interventionStop=2400, #int Time (Days) of 1700 days
                                       intFreq=3.5,
                                       intIntensity=6, 
                                       
                                       releaseStart=20,
                                       releaseStop=1820,
                                       releaseFreq=3.5,#biweekly release; twice a week
                                       releaseIntensity=1,
                                       Params=params,
                                       SIT=T)}

femaleMosU_overfloodingwerror_verily_sit <- lapply(store7_sit,function(x)x$Fu)
femaleMosU_overfloodingwerror_verily_sit <- do.call(cbind,femaleMosU_overfloodingwerror_verily_sit)
maleMosU_overfloodingwerror_verily_sit <- lapply(store7_sit,function(x)x$Mu)
maleMosU_overfloodingwerror_verily_sit <- do.call(cbind,maleMosU_overfloodingwerror_verily_sit)

femaleMosW1_overfloodingwerror_verily_sit <- lapply(store7_sit,function(x)x$Fw1)
femaleMosW1_overfloodingwerror_verily_sit <- do.call(cbind,femaleMosW1_overfloodingwerror_verily_sit)
maleMosW1_overfloodingwerror_verily_sit <- lapply(store7_sit,function(x)x$Mw1)
maleMosW1_overfloodingwerror_verily_sit <- do.call(cbind,maleMosW1_overfloodingwerror_verily_sit)

femaleMosW2_overfloodingwerror_verily_sit <- lapply(store7_sit,function(x)x$Fw2)
femaleMosW2_overfloodingwerror_verily_sit <- do.call(cbind,femaleMosW2_overfloodingwerror_verily_sit)
maleMosW2_overfloodingwerror_verily_sit <- lapply(store7_sit,function(x)x$Mw2)
maleMosW2_overfloodingwerror_verily_sit <- do.call(cbind,maleMosW2_overfloodingwerror_verily_sit)

