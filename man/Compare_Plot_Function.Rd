\name{compare.plot}
\alias{compare.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparison Barplot Function
}
\description{
This function is specifically designed for the URMC fitness dataset. It compares the averages for one group to the averages in a second group.
}
\usage{
compare.plot(x, compare_column, group1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The data frame containting the data}
 \item{compare_column}{
The column of the data frame that contains the data that will be compared.
}
 \item{group1}{
The first comparison group. The remaining data will be the second comparison group.
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
Alexis Zavez
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(sample.data)
compare.plot(sample.data, sample.data$Day, "Wednesday")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
