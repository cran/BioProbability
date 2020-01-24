\name{predictive.value}
\alias{predictive.value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Positive and negative predictive values for a diagnostic test.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the positive and negative predictive values for a diagnostic test from the prevalence, the sensitivity and the specificity values using the Bayes' theorem. For more details, see Agresti (2018, ISBN: 978-1-119-40528-3).

}
\usage{
predictive.value(p, Spe, Sen, plot.it = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{p} here~~
a numeric value indicating the prevalence of the disease. It is possible to consider a numeric vector of different values for the prevalence.
}
  \item{Spe}{
%%     ~~Describe \code{Spe} here~~
a numeric value corresponding to the specificity of the diagnostic test.

}
  \item{Sen}{
%%     ~~Describe \code{Sen} here~~
a numeric value corresponding to the sensitivity of the diagnostic test.

}
  \item{plot.it}{
%%     ~~Describe \code{plot.it} here~~
a logical value indicating whether the scatterplots for the prevalence values and the corresponding predictive values for the diagnostic test must be plotted.


}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix of three columns. The first column contains the vector of prevalences \code{p}. The second and third columns contain the corresponding positive and negative predictive values, respectively.

If \code{plot.it=TRUE}, the scatterplots for the prevalence values and the  predictive values is are plotted.
}
\references{
%% ~put references to the literature/web site here ~
Agresti, A. (2018). An introduction to categorical data analysis. John Wiley & Sons. ISBN: 978-1-119-40528-3.
}
%\author{
%%  ~~who you are~~


%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
p<-seq(0.001,0.1,length=10)
predictive.value(p,Spe=0.95,Sen=0.97,plot.it=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
