#### Functions ####
############################################################################
## FUNCTIONS TO COMPUTE KHAT AND Q
############################################################################
## Function to compute khat and Q from obs. design
computeQkhat <- function(data, subjects = "subjects", raters = "raters"){
  k <- length(unique(data[[raters]]))
  share <- 0 # Proportion shared raters
  
  RR <- data[[raters]]
  SS <- data[[subjects]]
  tabRxS <- table(RR, SS)
  uSub <- ncol(tabRxS) # Number of unique subjects
  khat <- uSub/ sum(1/colSums(tabRxS)) # harmonic mean number of raters per subject
  for(i in 1:uSub){
    k_s <- colSums(tabRxS)[i]
    
    for(j in (1:uSub)[-i]){
      k_sprime <- colSums(tabRxS)[j]
      k_s.sprime <- sum(RR[SS == i] %in% RR[SS == j])
      share <- share + (k_s.sprime / (k_s*k_sprime))/(uSub * (uSub-1))
    }
  }
  Q <- round(1/khat - share, 3)
  names(Q) <- "Q"
  
  return(list(Q = Q, khat = khat))
}

## Function to compute only khat (saves time when Q is not needed)
computeKhat <- function(data, subjects = "subjects", raters = "raters"){
  k <- length(unique(data[[raters]]))
  share <- 0 # Proportion shared raters
  
  RR <- data[[raters]]
  SS <- data[[subjects]]
  tabRxS <- table(RR, SS)
  uSub <- ncol(tabRxS) # Number of unique subjects
  khat <- uSub/ sum(1/colSums(tabRxS)) # harmonic mean number of raters per subject
  
  return(khat)
}

estICCs <- function(data, 
                    Y = "Y", 
                    subjects = "subjects", 
                    raters = "raters", 
                    level = .95,
                    k = NA, 
                    khat = NA, 
                    Q = NA, 
                    estimator = "MLE",
                    response = "continuous") {
  
  ## Number of raters
  if(is.na(k)){
    k <- length(unique(data[[raters]]))
  }
  
  ## Check type of design
  # Balanced or unbalanced
  if(length(unique(rowSums(table(data[[subjects]], by = data[[raters]])))) == 1){
    balanced <- "TRUE"
  } else {
    balanced <- "FALSE"
  }
  # Complete or incomplete
  if(all(colSums(table(data[[subjects]], by = data[[raters]])) == 
         length(unique(data[[subjects]])))){
    complete <- "TRUE" 
  } else {
    complete <- "FALSE"
  }
  # One-Way or Two-Way
  if(all(rowSums(table(data[[raters]], by = data[[subjects]])) == 1)){
    twoWay <- "FALSE"
  } else {
    twoWay <- "TRUE"
  }
  
  if(is.na(khat) | is.na(Q)){
    ## Decide on values for khat and q 
    if(balanced == T & complete == T){ 
      khat <- k
      Q <- 0 
    } else {
      if(balanced == T & complete == F){
        khat <- unique(rowSums(table(data[[subjects]], by = data[[raters]])))
        Q <- computeQkhat(data, subjects = subjects, raters = raters)$Q[["Q"]]
      } else {
        if(balanced == F & complete == F){
          Qkhat <- computeQkhat(data, subjects = subjects, raters = raters)
          khat <- Qkhat$khat
          Q <- Qkhat$QQ[["Q"]]
        } else {
          if(twoWay == F){
            khat <- computeKhat(data, subjects = subjects, raters = raters)
            Q <- 1/k # But not needed, since sigmaR cannot be distinguished
          }
        }
      }
    }
  }
  
  
  ### ESTIMATE ICCs: Continuous data
  if(response == "continuous"){
    if(estimator == "MCMC"){
      ### Estimate two-way model using MCMC with BRMS, silent = TRUE)
      OUT <- estICC_MCMC(data = data, Y = Y, subjects = subjects, raters = raters, 
                         level = level, k = k, khat = khat, Q = Q)
    }
    if(estimator == "MLE"){
      ### MLE with lme4 (random-effects model)
      OUT <- estICC_LME4(data = data, Y = Y, subjects = subjects, raters = raters, 
                         level = level, k = k, khat = khat, Q = Q)
    }
  }
  
  ### ESTIMATE ICCs: Binary data
  if(response == "binary"){
    if(estimator == "MCMC"){
      ### MCMC with BRMS
      OUT <- estICC_MCMC_bin(data = data, Ybin = Ybin, subjects = subjects, raters = raters, 
                             level = level, k = k, khat = khat, Q = Q)
    }
    if(estimator == "MLE"){
      ### MLE with lme4 (random-effects model)
      OUT <- estICC_LME4_bin(data = data, Ybin = Ybin, subjects = subjects, raters = raters, 
                             level = level, k = k, khat = khat, Q = Q)
    }
  }
  return(OUT)
  
}
