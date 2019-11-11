\name{OSFCOK}
\alias{OSFCOK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functional Cokriging
     %%  ~~function to do ... ~~
}
\description{
     %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
     OSFCOK(SFD,modelo,fixcoords,movcoords, S,xbound,ybound,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{SFD}{is an object of class SpatFD.}
     \item{modelo}{an object of class vgm.}
     \item{fixcoords}{coordinates which can not move.}
     \item{movcoords}{Initial values for the parameters to be optimized over.}
     \item{S}{Coordinates where is wanted more optime prediction}
     \item{xbound}{limits in x axe.}
     \item{ybound}{limits in y axe.}
     \item{...}{Arguments to be passed to control(read optim documentation )}
     %%     ~~Describe \code{x} here~~
}

\details{
     %%  ~~ If necessary, more details than the description above ~~
}
\value{
     \item{newcoords}{new coordinates}
     \item{fixcoords}{fix coordinates}
     %%  ~Describe the value returned
     %%  If it is a LIST, use
     %%  \item{comp1 }{Description of 'comp1'}
     %%  \item{comp2 }{Description of 'comp2'}
     %% ...
}
\references{
     %% ~put references to the literature/web site here ~
}
\author{ Angie Villamil, Martha Bohorquez
     %%  ~~who you are~~
}
\note{
     %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

     \seealso{
          %% ~~objects to See Also as \code{\link{help}}, ~~~
     }
\examples{
     ##---- Should be DIRECTLY executable !! ----
     ##-- ==>  Define data, use random,
     ##--	or do  help(data=index)  for the standard data sets.

     ## The function is currently defined as
     function (x)
     {
     }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
