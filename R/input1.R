input1<-function(){
  message("The input of this function must be a non-zero column matrix of dimension 2x2",'\n')
  A<-matrix(0,nrow=2,ncol=2)
  A[1,1]<-as.numeric(readline("People with the disease have a positive result\n"))
  A[1,2]<-as.numeric(readline("People without the disease have a positive result\n"))
  A[2,1]<-as.numeric(readline("People with the disease have a negative result\n"))
  A[2,2]<-as.numeric(readline("People without the disease have a negative result\n"))
  return(A)
}
