\name{odds}
\alias{odds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Odds of a success
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the odds of a success from its probability. For more details, see Agresti (2018, ISBN: 978-1-119-40528-3).
}
\usage{
odds(p,name="Prevalence")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{x} here~~
a numeric value indicating the probability of the success. It is possible to consider a numeric vector of different probabilities values.
}
\item{name}{
a character value indicating the name of the success. Possible values for this argument are \code{name="Prevalence"} or \code{name="Incidence"}.
%%
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
A matrix of two columns. The first column contains the vector of probabilities \code{p}. The second column contains the corresponding odds values.
%% ...
}
 %% ~put references to the literature/web site here ~

\references{
Agresti, A. (2018). An introduction to categorical data analysis. John Wiley & Sons. ISBN: 978-1-119-40528-3.

%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
odds(0.09, name="Prevalence")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
