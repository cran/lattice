\name{panel.levelplot}
\alias{panel.levelplot}
\title{ Default Panel Function for levelplot }
\description{
  This is the default panel function for \code{levelplot}.
}
\usage{
panel.levelplot(x, y, z, zcol,
                subscripts,
                at = mean(z),
                shrink,
                labels = NULL,
                label.style = c("mixed", "flat", "align"),
                contour = TRUE,
                region = TRUE,
                col = add.line$col,
                lty = add.line$lty,
                lwd = add.line$lwd,
                cex = add.text$cex,
                font = add.text$font,
                col.text = add.text$col,
                \dots,
                col.regions)
}
\arguments{
  \item{x, y, z}{ variables defining the plot}
  \item{zcol}{color numbers for each x,y combination}
  \item{subscripts}{which points to draw}
  \item{at}{cut points}
  \item{shrink}{ either a numeric vector of length 2, or a list with
    components x and y which are numeric vectors of length 2.}
  \item{labels}{contour labels}
  \item{label.style}{}
  \item{contour}{logical}
  \item{region}{logical}
  \item{col, lty, lwd}{ graphical parameters for contour lines}
  \item{cex, font, col.text}{ graphical parameters for contour labels}
  \item{\dots}{extra parameters }
  \item{col.regions}{colors indexed by zcol} 
}
\details{
  The same function is used for both \code{levelplot} and
  \code{contourplot} (which differ only in default values of some
  arguments). Most arguments are self-evident. \code{label.style}
  controls choice of how label positions are determined (this is not
  very sophisticated yet). The \code{shrink} argument allows the
  rectangles to be resized, proportional to the z-value. The
  specification can be made separately for widths and heights, in terms
  of a length 2 numeric vector, which gives the minimum and maximum
  proportion of shrinkage (corresponding to min and max of z).
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{levelplot}}}
\keyword{dplot}
















