predictive.value<-function(p,Spe,Sen,plot.it=FALSE){
  while(sum(p>=1)>0|sum(p<=0)>0){
    message("Prevalence of a disease is a value or a vector of values between 0 and 1","\n")
    pStr<-readline("What is the value/vector of prevalences?\n")
    p<-as.numeric(unlist(strsplit(pStr, ",")))
  }

  while(((Sen==0)&(Spe==1))|((Spe==0)&(Sen==1))|(Spe<0)|(Sen<0)|(Spe>1)|(Sen>1))  {
    message("Sensitivity and specificity are probabilities. They take values between 0 an 1","\n")
    Sen<-as.numeric(readline("What is the sensitivity value?\n"))
    Spe<-as.numeric(readline("What is the specificity value?\n"))
  }
  message("Computation of the predictive values (+ and -) from the prevalence","\n")
  p<-sort(p)
  p.p.v<-p*Sen/(p*Sen+(1-p)*(1-Spe))
  p.n.v<-(1-p)*Spe/((1-p)*Spe+p*(1-Sen))

  if((length(p)>1)&(plot.it==TRUE)){
    oldpar<-par(mfrow=c(1,2))
    on.exit(par(oldpar))
    plot(p,p.p.v,xlab="Prevalence",ylab="+ predictive value",pch=19)
    plot(p,p.n.v,pch=19,xlab="Prevalence",ylab="- predictive value")
  }
  result<-cbind(p,p.p.v,p.n.v)
  colnames(result)<-c("Prevalence","+ predictive value","- predictive value")
  return(result)
}

