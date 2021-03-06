% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stim.M.R
\name{stim.M}
\alias{stim.M}
\title{Build buffer zone to M}
\usage{
stim.M(occs, radio, bgeo = NULL, ...)
}
\arguments{
\item{occs}{data.frame of ocurrence data (longitude/latitude).}

\item{radio}{radio of buffer.}

\item{bgeo}{Biogeographical layer. Categorical values.}

\item{...}{Optional features of buffer}
}
\value{
SpatialPolygons* object
}
\description{
Returns buffer zone based on ocurrence data
}
\details{
To define calibration area is crucial step (Barve et al., 2011),
even more with incomplete sample data sometime is
complicated, because to get complete sample within geography space
is dificult, in these cases is apropiate define M with buffer zone
 (Peterson et al., 2017); and in other cases it helps to cut the
ends of the calibration area based on the maximum dispersion capacity
(Atauchi et al., 2018).
}
\examples{

# Phytotoma ocurrence data
data(phytotoma)

# Build buffer zone
buf_M <- stim.M(occs=phytotoma[,2:3], 100)

# Add points
points(phytotoma[,2:3])

}
\references{
Atauchi et al. (2018). Species distribution models for
Peruvian Plantcutter improve with consideration of biotic
interactions. \emph{J. avian biology 2018: e01617}. <doi:http://10.1111/jav.01617.>

Barve et al. (2011) The crucial role of the accessible area in
ecological niche modeling and species distribution modeling.
 \emph{Ecol. Mod}. 222:1810–1819.

Peterson et al.(2017) Influences of climate change on the potential
distribution of \emph{Lutzomyia longipalpis} sensu lato (Psychodidae:
Phlebotominae). \emph{International journal for parasitology}.
45(10-11): 667–674.
}
