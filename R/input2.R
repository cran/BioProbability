
input2<-function(){
	message("The input of this function must be a non-zero column matrix of dimension 2x2",'\n')
	A<-matrix(0,nrow=2,ncol=2)
	A[1,1]<-as.numeric(readline("People have been exposed to the factor and they present the disease\n"))
	A[1,2]<-as.numeric(readline("People have been exposed to the factor and they do not present the disease\n"))
	A[2,1]<-as.numeric(readline("People have not been exposed to the factor and they present the disease\n"))
	A[2,2]<-as.numeric(readline("People have not been exposed to the factor and they do not present the disease\n"))
	return(A)
}
