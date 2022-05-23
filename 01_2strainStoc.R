# Implements 1 strain wildtype, optional single strain release, optional second strain intervention
# t denotes time point of simulation
# FW number of seed Wolbachia females #Wolbachia suppression strategy if high value. 
# MW number of seed Wolbachia males #Wolbachia replacement strategy if high value. Is the error rate under suppression approach
# release = "sustained" denotes sustained release of male Wolbachia Mosquitoes
# relError denotes female release error rate, higher values means higher error (as proportion of seed males)
# intervention="otherStrain" or "overflooding", denotes whether intervention takes release of second Wolb strain or overflood the same strain
# interventionStart denotes start time of intervention for second strain
# interventionStop denotes stop time of intervention for second strain
# intFreq denotes frequency of intervention
# intIntensity denotes intensity of intervention (as proportion of releaseIntensity)
# releaseFreq denotes frequency of first strain release
# releaseIntensity denotes intensity of first strain release
# releaseStart denotes start time of release for first strain
# releaseStop denotes stop time of release for first strain
# SIT denotes whether the released male and female wolbachia mosquitoes are sterile
mozzieModelStoc <- function(t,             
                        FW,
                        MW,
                        release="sustained",
                        relError=0.1, 
                        intervention="otherStrain", 
                        intError=0, 
                        interventionStart=200, 
                        interventionStop=400,
                        intFreq=3.5,
                        intIntensity=1,
                        releaseStart=20,
                        releaseStop=500,
                        releaseFreq=3.5,
                        releaseIntensity=1,
                        Params=params,
                        SIT=F){

  list2env(Params,globalenv())
  #storage vectors for transitions between components (denoted d)
  #storage vectors for compartments 
  #w1 denotes vectors for first strain
  #w2 denotes vectors for second strain
  #u denotes uninfected/wildtype mosquitoes
  
  d_Au <- Au <- c();   #uninfected aquatic stage
  d_Aw1 <- Aw1 <- c(); #infected aquatic stage
  d_Aw2 <- Aw2 <- c(); #infected aquatic stage
  d_Fu <- Fu <- c();   #uninfected female 
  d_Fw1 <- Fw1 <- c(); #infected female 
  d_Fw2 <- Fw2 <- c(); #infected female 
  d_Mu <- Mu <- c();   #uninfected male
  d_Mw1 <- Mw1 <- c(); #infected male 
  d_Mw2 <- Mw2 <- c(); #infected male 
  
  mu   <- mw1 <- mw2 <-  c();
  Na <- c();
  Buu <- Buw1 <- Bw1u <- Bw1w1 <- Buw2 <- Bw2u <- Bw2w2 <- c();
  fu  <- fw1 <- fw2 <- c(); #to allow us to look at proportions

  #initial conditions
  Au[1] <- 0
  Aw1[1] <- 0
  Aw2[1] <- 0
  Mu[1] <- 1
  Fu[1] <- 1
  Mw1[1] <- MW  #Wolbachia suppression strategy if high value. 
  Fw1[1] <- FW  #Wolbachia replacement strategy if high value. Is the error rate under suppression approach if unintended
  Mw2[1] <- 0
  Fw2[1] <- 0
  
  tot <-  Mu[1] + Mw1[1] + Mw2[1] 
  mu[1] <- Mu[1]/(tot)
  mw1[1] <- Mw1[1]/(tot)
  mw2[1] <- Mw2[1]/(tot)
  
  tot_f <- Fu[1] + Fw1[1] + Fw2[1]
  fu[1] <- Fu[1]/(tot_f)
  fw1[1] <- Fw1[1]/(tot_f)
  fw2[1] <- Fw2[1]/(tot_f)
  
  Na[1] <- Au[1] + Aw1[1] + Aw2[1]
  xi <- 1- (Na[1]/Ka)
  
  Buu[1]  <- PhiU*Fu[1]*mu[1]*xi
  Buw1[1]  <- (1-C_uw1)*PhiU*Fu[1]*mw1[1]*xi
  Bw1u[1]  <- (1-C_w1u)*PhiW1*Fw1[1]*mu[1]*xi
  Bw1w1[1] <- (1-C_w1w1)*PhiW1*Fw1[1]*mw1[1]*xi
  
  Buw2[1] <- (1-C_uw1)*PhiU*Fu[1]*mw2[1]*xi
  Bw2u[1] <- (1-C_w2u)*PhiW2*Fw2[1]*mu[1]*xi
  Bw2w2[1] <- (1-C_w2w2)*PhiW2*Fw2[1]*mw2[1]*xi
  
  if (release=="sustained"){
    releaseInd <- round(seq(releaseStart,releaseStop,by=releaseFreq)) 
    releaseM   <- releaseIntensity*MW
    releaseF   <- releaseIntensity*MW*relError 
    j<-1
  }
  
  if (!is.na(intervention)){1
    IntReleaseInd <- round(seq(interventionStart,interventionStop,by=intFreq))  
    IntReleaseM   <- intIntensity*MW
    IntReleaseF   <- intIntensity*MW*intError
    j1<-1
  }
  
  
  for (i in 1:t){ 
    #may need to add that stages can only die if >0 in previous time
    d_Au[i+1]  <- Buu[i] + Vu*(Bw1u[i]+Bw1w1[i]+Bw2u[i]+Bw2w2[i]) - MiuA*Au[i] - Psi*Au[i]
    d_Aw1[i+1] <- Vw*(Bw1u[i]+Bw1w1[i]) - MiuA*Aw1[i] - Psi*Aw1[i]
    d_Aw2[i+1] <- Vw*(Bw2u[i]+Bw2w2[i]) - MiuA*Aw2[i] - Psi*Aw2[i]
    
    d_Fu[i+1]  <- Bf*Psi*Au[i] 
    d_Fw1[i+1] <- Bf*Psi*Aw1[i]
    d_Fw2[i+1] <- Bf*Psi*Aw2[i]
    d_Mu[i+1]  <- Bm*Psi*Au[i] 
    d_Mw1[i+1] <- Bm*Psi*Aw1[i] 
    d_Mw2[i+1] <- Bm*Psi*Aw2[i] 
    
    #mosquito can only die if >=0
    if (Fu[i]>=0){d_Fu[i+1]   <- d_Fu[i+1] - MiuFU*Fu[i]}
    if (Fw1[i]>=0){d_Fw1[i+1] <- d_Fw1[i+1] - MiuFW1*Fw1[i]}
    if (Fw2[i]>=0){d_Fw2[i+1] <- d_Fw2[i+1] - MiuFW2*Fw2[i]}
    if (Mu[i]>=0){d_Mu[i+1]   <- d_Mu[i+1] - MiuMU*Mu[i]}
    if (Mw1[i]>=0){d_Mw1[i+1] <- d_Mw1[i+1] - MiuMW1*Mw1[i]}
    if (Mw2[i]>=0){d_Mw2[i+1] <- d_Mw2[i+1] - MiuMW2*Mw2[i]}
    
    #update compartments
    Au[i+1]  <- Au[i] + d_Au[i+1] 
    Aw1[i+1] <- Aw1[i] + d_Aw1[i+1] 
    Aw2[i+1] <- Aw2[i] + d_Aw2[i+1] 
    Fu[i+1]  <- Fu[i] + d_Fu[i+1] 
    Fw1[i+1] <- Fw1[i] + d_Fw1[i+1] 
    Fw2[i+1] <- Fw2[i] + d_Fw2[i+1] 
    Mu[i+1]  <- Mu[i] + d_Mu[i+1] 
    Mw1[i+1] <- Mw1[i] + d_Mw1[i+1] 
    Mw2[i+1] <- Mw2[i] + d_Mw2[i+1] 
    
    #release of predominant strain
    if (release=="sustained"){
      if (j<=length(releaseInd)){
        if (releaseInd[j]==i){
          Mw1[i+1] <- Mw1[i+1] + releaseM
          Fw1[i+1] <- Fw1[i+1] + releaseF 
          j <- j+1}
      }
    }
    
    #release of other strain as intervention
    if (intervention=="otherStrain"){
      if (j1<=length(IntReleaseInd)){
        if (IntReleaseInd[j1]==i){
          Mw2[i+1] <- Mw2[i+1] + IntReleaseM
          Fw2[i+1] <- Fw2[i+1] + IntReleaseF 
          j1 <- j1+1}
      }
    }
    
    #overflooding of predominant strain 
    if (intervention=="overflooding"){
      if (j1<=length(IntReleaseInd)){
        if (IntReleaseInd[j1]==i){
          Mw1[i+1] <- Mw1[i+1] + IntReleaseM
          Fw1[i+1] <- Fw1[i+1] + IntReleaseF 
          j1 <- j1+1}
      }
    }
    
    
    #next time point births
    tot <-  Mu[i+1] + Mw1[i+1] + Mw2[i+1] 
    mu[i+1] <- Mu[i+1]/tot
    mw1[i+1] <- Mw1[i+1]/tot 
    mw2[i+1] <- Mw2[i+1]/tot
    
    tot_f <- Fu[i+1] + Fw1[i+1] + Fw2[i+1]
    fu[i+1] <- Fu[i+1]/tot_f
    fw1[i+1] <- Fw1[i+1]/tot_f
    fw2[i+1] <- Fw2[i+1]/tot_f
    
    Na[i+1] <- Au[i+1] + Aw1[i+1] + Aw2[i+1]
    xi <- 1 - Na[i+1]/Ka
    
    if (SIT==T & i >= interventionStart){C_w1w1 <- 1}
    
    Buu[i+1] <- PhiU*Fu[i+1]*mu[i+1]*xi
    Buw1[i+1] <- (1-C_uw1)*PhiU*Fu[i+1]*mw1[i+1]*xi
    Bw1u[i+1] <- (1-C_w1u)*PhiW1*Fw1[i+1]*mu[i+1]*xi
    Bw1w1[i+1] <- (1-C_w1w1)*PhiW1*Fw1[i+1]*mw1[i+1]*xi
    
    Buw2[i+1] <- (1-C_uw1)*PhiU*Fu[i+1]*mw2[i+1]*xi
    Bw2u[i+1] <- (1-C_w2u)*PhiW2*Fw2[i+1]*mu[i+1]*xi
    Bw2w2[i+1] <- (1-C_w2w2)*PhiW2*Fw2[i+1]*mw2[i+1]*xi
    
    #control for neg birth
    if (Buu[i+1]<0) Buu[i+1] <- 0
    if (Buw1[i+1]<0) Buw1[i+1] <- 0
    if (Bw1u[i+1]<0) Bw1u[i+1] <- 0
    if (Bw1w1[i+1]<0) Bw1w1[i+1] <- 0
    if (Buw2[i+1]<0) Buw2[i+1] <- 0
    if (Bw2u[i+1]<0) Bw2u[i+1] <- 0
    if (Bw2w2[i+1]<0) Bw2w2[i+1] <- 0
  }
  
  out <- data.frame(d_Au=d_Au,
                    d_Aw1=d_Aw1,
                    d_Aw2=d_Aw2,
                    d_Fu=d_Fu,
                    d_Fw1=d_Fw1,
                    d_Fw2=d_Fw2,
                    d_Mu=d_Mu,
                    d_Mw1=d_Mw1,
                    d_Mw2=d_Mw2,
                    Au=Au,
                    Aw1=Aw1, 
                    Aw2=Aw2,
                    Fu=Fu,
                    Fw1=Fw1,
                    Fw2=Fw2,
                    Mu=Mu,
                    Mw1=Mw1,
                    Mw2=Mw2,
                    mu=mu,
                    mw1=mw1,
                    mw2=mw2,
                    fu=fu,
                    fw1=fw1,
                    fw2=fw2,
                    Buu=Buu,
                    Buw1=Buw1,
                    Buw2=Buw2,
                    Bw1u=Bw1u,
                    Bw2u=Bw2u,
                    Bw1w1=Bw1w1,
                    Bw2w2=Bw2w2)
  
  return(out)
}

