sensitivity.specificity<-function(A=NULL,show.matrix=FALSE){ #Calculo sensibilidad y especificidad
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
  message("Sensitivity and Specificity of a diagnostic test",'\n')
  if(show.matrix==TRUE){
    rownames(A)=c("+","-")
    colnames(A)=c("Disease","Without disease")
    print(A)
  }
  S<-A[1,1]/sum(A[,1])   #P(+|E)
  E<-A[2,2]/sum(A[,2])   #P(-|S)
  output=c(S,E)
  names(output)=c("Sensitivity","Specificity")
  return(output)
}
