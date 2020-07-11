\name{FCOK}
\alias{FCOK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functional Cokriging
     %%  ~~function to do ... ~~
}
\description{
        Functional Cokriging
     %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
     FCOK(SFD,newcoords,model,j=1,fill.all=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{SFD}{is an object of class SpatFD.}
     \item{newcoords}{A data fame with two columns indicating X and Y coordinates.}
     \item{model}{An object of class variogramModel or a list of objects of class variogramModel(see vgm from gstat package).}
     \item{j}{a numeric value to specifying the position of the variable or a character value specifying the name of the variable. 1 by default.}
     \item{fill.all}{A boolean value to fit the same model to all scores. TRUE by default. If TRUE, model must be class variogramModel. If FALSE, model must be a list of objects of class variogramModel.}
     %%     ~~Describe \code{x} here~~
}


\value{
     An object of class FCOK, which inherits from list. Its components are:
          \item{X}{the object of class FCOK where are specified the functional predictions.}
     %%  ~Describe the value returned
     %%  If it is a LIST, use
     %%  \item{comp1 }{Description of 'comp1'}
     %%  \item{comp2 }{Description of 'comp2'}
     %% ...
}

\author{ Angie Villamil, Martha Bohorquez
     %%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

     \seealso{\code{\link{FKSK}}, \code{\link{FKCK}}, \code{\link{SpatFD}}
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
\keyword{Functional Cokriging}% use one of  RShowDoc("KEYWORDS")
