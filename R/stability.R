#' Stability of ecological niche models

stability <- function(current = NULL, future = NULL, thr.value = NULL,
                      charname = NULL)
#'
#' Returns stability niche based on species distribution models and their projections.
#'
#'
#' @param current Raster* objet of present distribution.
#' Raster has continue values.
#' @param future RasterStack* object of future distributions.
#' Must have three models with continue values.
#' @param thr.value Cut value (0 — 1) of threshold in order to
#'  species distribution.
#'
#' @param charname \code{character-Class}, name of the species.
#' (\code{default = NULL}).
#'
#' @return An object of class 'StabEcodist'
#'
#' \bold{Method S4 print}
#'
#' Return table with these features: \code{Models} and \code{nPixel}
#' (frequency of pixel with that feature).
#'
#' \enumerate{
#'
#' \item{\bold{Lost area}
#'
#' {code \code{Models}:
#'
#'  \code{code 0} mention the area with species absence.
#'
#'  \code{code 14} mention the lost area current in the future projections}
#' }
#'
#' \item{\bold{Gain area}
#'
#' {code \code{Models}:
#'
#'  \code{code 1} mention the gain area acording with one model.
#'
#'  \code{code 2} acording with two models.
#'
#'  \code{code 3} acording with three models. }
#' }
#'
#' \item{\bold{Stable}
#'
#' {code \code{Models}:
#'
#'  \code{code 15} mention the stable area with one model.
#'
#'  \code{code 16} mention the stable area with two models
#'
#'  \code{code 17} mention the stable area with three models.}
#' }
#'
#' }
#'
#' \bold{Method S4 plot}
#'
#' Return maps of stability niche
#'
#'
#' @references
#' Peterson et al., (2017) Influences of climate change on the potential
#' distribution of Lutzomyia longipalpis sensu lato (Psychodidae: Phlebotominae).
#'  International Journal for Parasitology. 47(10–11):667–74.
#'
#' @export



{
  if(is.null(list(current, future, thr.value)) == TRUE){
    stop('specifc arguments')
  }
  if(is.null(charname) == TRUE && is.character(charname) == TRUE){
    'sp'
  }
  if(nlayers (current) != 1 & nlayers (future) != 3){
    message('Review the layer numbers')
  }

  if ((thr.value >= 0 & thr.value <= 1) == FALSE) {
    message('You need to put a value of thr.value')
  }else{
    # Build stability analysis
    dfa <- stabl(current, future, thr.value)
    # data.frame of stability
    stab.an <- rasterToPoints(dfa)
    stab.an <- as.data.frame(stab.an)

    # show statistics
    stab.an <- as.data.frame(table(stab.an$layer))
    names(stab.an) <- c("Models", "nPixel")
    stin <- stab.an
  }

  output <- new("StabEcodist", df = stin, map = dfa)
  return(output)

}

