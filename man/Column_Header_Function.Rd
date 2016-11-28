\name{col_head}
\alias{col_head}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Column Header Function
}
\description{
This function takes a googlesheets worksheet (or dataframe) and renames the columns of the object using the first row of the object. It returns a dataframe.
}
\usage{
col_head(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The object for which you want to rename the columns using the first row of data.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alexis Zavez}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(sample.data)
col_head(sample.data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
