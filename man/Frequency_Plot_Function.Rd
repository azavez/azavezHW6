\name{freq.plot}
\alias{freq.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequency Barplot Function
}
\description{
This function creates a frequency barplot from a vector.
}
\usage{
freq.plot(x, xtitle, ytitle, mtitle,...)
}
\arguments{
  \item{x}{
The vector for which you want to create a frequency barplot
}
  \item{xtitle}{
Title for the x-axis of the frequency barplot. The default title is "x-axis".
}
  \item{ytitle}{
Title for the y-axis of the frequency barplot. The default title is "y-axis"
}
  \item{mtitle}{
Main title for the frequency barplot. The default title is "main title"
}
  \item{...}{
Additional arguments passed to the barplot function
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
days <- sample.data$Day
freq.plot(days, "", "Number of Visits", "Distribution of Visits by Day of Week")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
