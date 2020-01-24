
odds.ratio<-function(A=NULL,show.matrix=FALSE,conf.int=FALSE,level=0.05){   #IC for large n
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
  if(show.matrix==TRUE){
    rownames(A)=c("Exposed","Non-exposed")
    colnames(A)=c("Disease","Without disease")
    print(A)
    message('\n')
  }
  OR=A[1,1]*A[2,2]/(A[1,2]*A[2,1])
  if(conf.int==TRUE){
    while((level>=1)|(level<=0)){
      message("Level of the confidence interval is a value between 0 and 1",'\n')
      level<-as.numeric(readline("What is the value of the level?\n"))
    }
    LI<-qnorm(level/2)*sqrt(sum(1/A))
    CI<-sort(exp(c(log(OR)-LI,log(OR)+LI)))
    L<-list(OR,CI)
    names(L)=c("Odds Ratio",paste("Confidence Interval of level ",level*100,"%",sep=""))
    return(L)
  }else{
    message("Odds ratio for a 2x2 contingency table",'\n')
    return(OR)
  }
}
