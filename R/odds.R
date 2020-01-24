odds<-function(p,name="Prevalence"){ #p is the a probability of success (prevalence, incidence)

  while(sum(p>=1)>0|sum(p<=0)>0){
    message("Prevalence of a disease is a value or a vector of values between 0 and 1","\n")
    pStr<-readline("What is the value/vector of prevalences?\n")
    p<-as.numeric(unlist(strsplit(pStr, ",")))
  }
  message(paste(name, "Odds"),"\n")
  p0=sort(p)
  result<-cbind(p0,p0/(1-p0))

  colnames(result)<-c(paste(name),"Odds")
  return(result)
}
