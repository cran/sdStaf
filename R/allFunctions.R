
#' @importFrom graphics par strwidth
#' @importFrom stats cor na.omit reorder filter

##################  AUTO-CORRELATION   #############
# Create function to make histogram with density superimposed
panel.hist <- function(x, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  par(new=TRUE)
  # Do not start new plot
  hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="",
       main="", col="lightgray")
  lines(density(x, na.rm=TRUE))
  # Add density curve
}
# Create function to compute and print R^2
panel.r2 <- function(x, y, digits=2, cex.cor, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="complete.obs")**2 # Compute R^2
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


stabl <- function(p, q, thr.value)

{
  # build threshould
  t <- as.numeric(thr.value)
  t <- c(0, t, 0, t, 1, 1)
  pm <- c(0, 0, 0,  0, 1, 100)

  # build matrix
  mr <- matrix(t, ncol = 3, byrow = TRUE)
  rcls <- matrix(pm, ncol=3, byrow=TRUE)
  p <- reclassify(p, mr)
  pmf <- reclassify(p, rcls)

  # renames
  names(q) <- c("q1","q2","q3")

  q1 <- reclassify(q[[1]], mr)
  q2 <- reclassify(q[[2]], mr)
  q3 <- reclassify(q[[3]], mr)

  # Build stability
  stab <- pmf + (q1 + q2 + q3)
  #mapp <- rasterToPoints(stab)

  #df <- data.frame(mapp)
  #colnames(df) <- c("Longitude", "Latitude", "MODEL")
  return(stab)
}


