\name{sensitivity.specificity}
\alias{sensitivity.specificity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Sensitivity and specificity for a diagnostic test.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the sensitivity and specificity for a diagnostic test. Definition of these two concepts can be found in Agresti (2018, ISBN: 978-1-119-40528-3).

}
\usage{
sensitivity.specificity(A, show.matrix = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
a 2x2 matrix object where A[1,1] contains the number of people with the disease and with a positive test result; A[1,2], the number of people without the disease with a positive test result; A[2,1], the number of people with the disease with a negative test result;	A[2,2], the number of people without the disease and with a negative test result.
}
  \item{show.matrix}{
%%     ~~Describe \code{show.matrix} here~~
a logical value indicating whether the matrix A must be shown.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector object of two components: The first one cointains the sensitivity and the second component, the specificity.
}

\references{
%% ~put references to the literature/web site here ~
Agresti, A. (2018). An introduction to categorical data analysis. John Wiley & Sons. ISBN: 978-1-119-40528-3.
}

%\author{

%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
A<-matrix(c(744,231,421,659),nrow=2)
sensitivity.specificity(A,show.matrix=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
