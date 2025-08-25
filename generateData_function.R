
############################################################################
## Function To Generate Mock Data to est. Qkhat for new design 04-09-2023
############################################################################
## Mock data 
generateData <- function(Subjects = Subjects, 
                         Raters = Raters, 
                         RatersSub = RatersSub, 
                         Design = Design){
  
  dat <- as.data.frame(matrix(rep(NA, Subjects*RatersSub)))
  dat$subject <- rep(1:Subjects, each = RatersSub)
  
  ## Loop to create design matrix 
  # In case of random rater assignment 
  if(Design == "random"){
    # Randomly sample Rs raters for each subject
    raters <- matrix(rep(1:Raters), nrow = Subjects, 
                     ncol = Raters, byrow = TRUE)
    dat$rater <- as.vector(apply(raters, 1, sample, size = RatersSub)) 
  } else {
    # In case of block designs 
    if(Design== "block"){
      # Determine N blocks 
      blocks <- Raters / RatersSub
      # Assign raters to blocks 
      raterBlocks <- matrix(1:Raters, ncol = blocks)
      # Assign blocks to subjects 
      for(j in 1:blocks){
        if(j == 1){
          raters <- rep(raterBlocks[,j], 
                        times = Subjects/blocks)
        } else {
          raters <- c(raters, rep(raterBlocks[,j], 
                                  times = Subjects/blocks))
        }
      }
      dat$rater <- raters
    } else {
      # In case of anker-rater designs
      if(Design== "anchor"){
        # Assign anker rater to each subject
        dat$rater <- NA
        dat$rater[seq(1, nrow(dat), by = RatersSub)] <- 1
        # Randomly sample Rs - 1 raters for each subject
        raters <- matrix(rep(2:Raters), nrow = Subjects, 
                         ncol = Raters-1, byrow = TRUE)
        dat$rater[is.na(dat$rater)] <- as.vector(apply(raters, 1, sample, size = RatersSub - 1))
      }}}
  
  dat <- dat[ ,c("subject", "rater")]
  
  return(dat)
}

