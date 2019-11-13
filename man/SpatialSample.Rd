\name{SpatialSample}
\alias{SpatialSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Spatial Sample
     %%  ~~function to do ... ~~
}
\description{ Optime predictions from sample point locations within a square area, a grid, a polygon, or on a spatial line, using regular or random sampling methods; the methods used assume that the geometry used is not spherical, so objects should be in planar coordinates
     %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
     SpatialSample(x,n,type,r,SFD,modelo,fixcoords,S,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
        \item{x}{Spatial object; spsample(x,...) is a generic method for the existing sample.Xxx functions}
        \item{n}{(approximate) sample size}
        \item{type}{character; "random" for completely spatial random; "regular" for regular (systematically aligned) sampling; "stratified" for stratified random (one single random location in each "cell"); "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates); "hexagonal" for sampling on a hexagonal lattice; "clustered" for clustered sampling; "Fibonacci" for Fibonacci sampling on the sphere (see references).}
        \item{r}{number of coordinates to be added}
        \item{SFD}{is an object of class SpatFD.}
        \item{modelo}{an object of class vgm.}
        \item{fixcoords}{coordinates which can not move.}
        \item{S}{Coordinates where is wanted more optime prediction}
        \item{...}{optional arguments, passed to the appropriate sample.Xxx functions; see NOTES for nclusters and iter}
        
        %%     ~~Describe \code{x} here~~
}


\value{
        \item{newcoords}{new coordinates}
        \item{fixscoords}{fix coordinates}
        \item{S}{S}
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
     
     \seealso{\code{\link{OSFCOK}}
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
\keyword{Spatial Sampling}% use one of  RShowDoc("KEYWORDS")
