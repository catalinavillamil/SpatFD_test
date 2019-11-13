\name{crossvalSFD}
\alias{crossvalSFD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ cross validation 
     %%  ~~function to do ... ~~
}
\description{
        cross validation of predictions
     %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
     crossvalSFD(data,coords,basis="Bsplines",nbasis=4,lambda=0,nharm=NULL,vp=NULL,add=NULL,fn="FKSK",model,j=1,fill.all=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
     \item{data}{ Is a matrix, data frame or fdata with the observed data
          %%     ~~Describe \code{x} here~~
     }
     \item{coords}{A data frame or a matrix with two columns indicating X and Y coordinates}
     \item{basis}{A character specifying the basis functions, ``Bsplines'' or ``Fourier''}
     \item{nbasis}{An integer variable specifying the number of basis functions. 4 by default. }
     \item{lambda}{A non negative real number specifying the amount of smoothing to be applied to the estimated functional parameter. 0 by default}
     \item{nharm}{The number of harmonics or principal components to compute.}
     \item{vp}{The percentage of variance to calculate the number of harmonics or principal components to compute.}
     \item{add}{An object of class SpatFD to be added.}
     \item{fn}{method to apply for prediction. ``FKSK'', ``FKCK'' or ``FCOK''. ``FKSK'' by default.}
     \item{model}{an object of class vgm.}
     \item{j}{a numeric value to specifying the position of the variable or a character value specifying the name of the variable. 1 by default.}
     \item{fill.all}{A boolean value to fit the same model to all scores. TRUE by default.}
     \item{...}{Arguments to be passed to the basis function creation (see create.bsplines.basis or create.fourier.basis).}
     
     %%     ~~Describe \code{x} here~~
}


\value{
     \item{prediction}{Predictions.}
     \item{data}{Real data.}
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
     
     \seealso{\code{\link{SpatFD}}, \code{\link{FKSK}}, \code{\link{FCOK}}, \code{\link{FKCK}}
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
\keyword{Cross Validation}% use one of  RShowDoc("KEYWORDS")
