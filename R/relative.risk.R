relative.risk<-function(A=NULL,show.matrix=FALSE,conf.int=FALSE,level=0.05){   #IC for n large
  c1<-is.matrix(A)
  if(!c1){
    A<-input2()
  }else{
    c2<-nrow(A)==2
    c3<-ncol(A)==2
    if(!c2|!c3){A<-input2()}
  }
  c4<-any(apply(A,2,sum)==0)
  while(c4){
    message("Sum of rows can not be zero",'\n')
    A<-input2()
    c4<-any(apply(A,2,sum)==0)
  }
  message("Relative risk for a 2x2 contingency table",'\n')
  if(show.matrix==TRUE){
    rownames(A)<-c("Presence","Absence")
    colnames(A)<-c("Disease","Without disease")
    print(A)
  }
  R1=A[1,1]/(A[1,1]+A[1,2])
  R2=A[2,1]/(A[2,1]+A[2,2])
  RR=R1/R2
  if(conf.int==TRUE){
    while((level>=1)|(level<=0)){
      message("Level of the confidence interval is a value between 0 and 1",'\n')
      level<-as.numeric(readline("What is the value of the level?\n"))
    }
    LI<-qnorm(level/2)*sqrt((1-R1)/(sum(A[1,])*R1)+(1-R2)/(sum(A[2,])*R2))
    CI<-sort(exp(c(log(RR)-LI, log(RR)+LI)))
    L<-list(RR,CI);names(L)=c("Relative Risk",paste("Confidence Interval of level ",level*100,"%",sep=""))
    return(L)
  }else{
    return(RR)
  }
}
