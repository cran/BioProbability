\name{relative.risk}
\alias{relative.risk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Relative risk calculation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the relative risk from a contingency table. Furthermore, a confidence interval for the relative risk is provided. Details on the confidence interval can be found in Agresti (2018, ISBN: 978-1-119-40528-3).
}
\usage{
relative.risk(A, show.matrix = FALSE, conf.int = FALSE, level = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
a 2x2 matrix object where A[1,1] contains the number of people with the disease who have been exposed to some condition; A[1,2], the number of people without the disease who have not been exposed this condition; A[2,1], the number of people with the disease who have been exposed to the condition;	A[2,2], the number of people without the disease who have not been exposed to some condition.}
  \item{show.matrix}{
%%     ~~Describe \code{show.matrix} here~~
a logical value indicating whether the matrix A must be shown.
}
  \item{conf.int}{
%%     ~~Describe \code{conf.int} here~~
a logical value indicating whether a confidence interval for the relative risk must be calculated.
}
  \item{level}{
%%     ~~Describe \code{level} here~~
level of significance for the confidence interval.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~

%}
\value{
If \code{conf.int=TRUE}, a list of length equal to two. The first element of the list \code{Relative Risk} corresponds to the estimation of the relative risk; the second one \code{Confidence Interval of level} contains the corresponding confidence interval.

If \code{conf.int=FALSE}, a numeric value corresponding to the estimation of the relative risk.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Agresti, A. (2018). An introduction to categorical data analysis. John Wiley & Sons. ISBN: 978-1-119-40528-3.
}

%\author{
%%  ~~who you are~~

%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
A<-matrix(c(744,231,421,659),nrow=2)
relative.risk(A,show.matrix=TRUE,conf.int=TRUE)
relative.risk(A,show.matrix=TRUE,conf.int=TRUE,level=0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
