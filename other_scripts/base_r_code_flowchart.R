
# Flowchart ---------------------------------------------------------------
ICCs  <-  c("ICC(C,1)", "ICC(Q,1)", "ICC(C,k)", "ICC(Q,k)", "ICC(A,1)", "ICC(A,k)", "ICC(A,khat)", "ICC(1)", "ICC(k)", "ICC(khat)")

##step1
#cat("Start\n")
step1 <- step2 <- step3 <- step4 <- NULL
sf1 <- function(x) readline("-> Crossed(C) or Nested(N)? and press ENTER: ")
sf2 <- function(x) readline("-> Relative(R) or Absolute(A)? and press ENTER: ")
sf3 <- function(x) readline("-> Single(S) or Average(A)? and press ENTER: ")
sf4.1 <- function(x) readline("-> Complete(C) or Incomplete(I)? and press ENTER: ")
sf4.2 <- function(x) readline("-> Balanced(B) or Unbalanced(U)? and press ENTER: ")

flow <- function(x){
  step1 <- sf1()
  if(step1=="N") {
    step3 <- sf3()
    if(step3 == "S") {
      Ans <- ICCs[8]
    } else {
      step4 <- sf4.2()
      Ans <- ifelse(step4 == "B", ICCs[9], ICCs[10]) 
    }
  }
  if(step1 == "C"){
    step2 <- sf2()
    if(step2 == "A"){ # Absolute..
      step3 <- sf3()
      if(step3=="S") {
        Ans <- ICCs[5]
        } else {
          step4 <- sf4.2()
          Ans <- ifelse(step4=="B", ICCs[6], ICCs[7])
        }}
    if(step2 == "R"){ # Relative...
      step3 <- sf3()
      if(step3=="S"){
        step4 <- sf4.1()
        Ans <- ifelse(step4=="C", ICCs[1], ICCs[2])
      } else {
        step4 <- sf4.1()
        Ans <- ifelse(step4=="C", ICCs[3], ICCs[4])
      }
    }
  }
  if(!is.null(step1)) cat(step1,"\n")
  if(!is.null(step2)) cat(step2,"\n")
  if(!is.null(step3)) cat(step3,"\n")
  if(!is.null(step4)) cat(step4,"\n")
  cat("\n Choose:", Ans,"\n")
}

flow()





