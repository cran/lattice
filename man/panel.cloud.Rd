\name{panel.cloud}
\alias{panel.cloud}
\alias{panel.wireframe}
\alias{panel.3dscatter.old}
\alias{panel.3dscatter}
\alias{panel.3dwire}
\alias{panel.3dwire.slow}
\alias{palette.shade}
\alias{ltransform3dMatrix}
\alias{ltransform3dto3d}
\title{ Default Panel Function for cloud }
\description{
  These are (related to) the default panel functions for \code{cloud}
  and \code{wireframe}.
}
\usage{
panel.cloud(x, y, z, subscripts,
            groups = NULL,
            distance, xlim, ylim, zlim,
            panel.3d.cloud = "panel.3dscatter",
            panel.3d.wireframe = "panel.3dwire",
            rot.mat, aspect,
            par.box = NULL,
            xlab, ylab, zlab, scales.3d,
            proportion = 0.6, wireframe = FALSE,
            scpos,
            \dots,
            col.at,
            col.regions)
panel.wireframe(\dots)
ltransform3dMatrix(screen, R.mat)
ltransform3dto3d(x, R.mat, za = 1 , zb = 0, zmin, zmax, dist)
panel.3dscatter.old(x, y, z, rot.mat = diag(4), za, zb,
             zback, zfront, distance,
             groups = NULL,
             subpanel = if (is.null(groups)) "panel.xyplot"
             else "panel.superpose",
             ...)
panel.3dscatter(x, y, z, rot.mat = diag(4), za, zb,
             zback, zfront, distance,
             zlim, zero,
             groups = NULL, subscripts = TRUE,
             type = 'p',
             col, col.point, col.line, lty, lwd, cex, pch, ...)
panel.3dwire(x, y, z, rot.mat = diag(4), za, zb,
             minz = 0, maxz = 1,
             col.at, col.regions,
             shade = FALSE,
             shade.colors = palette.shade,
             light.source = c(1, 0, 0),
             col = "black",
             col.groups = superpose.line$col,
             polynum = 100,
             ...)
panel.3dwire.slow(x, y, z, rot.mat = diag(4), za, zb,
             minz = 0, maxz = 1,
             col.at, col.regions,
             shade = FALSE,
             shade.colors = palette.shade,
             light.source = c(1, 0, 0),
             col = "black",
             col.groups = superpose.line$col,
             ...)
palette.shade(cosangle, height, saturation = .3, ...)
}
\arguments{
  \item{x, y, z}{ numeric vectors (or possibly factors etc for
    \code{panel.cloud}), not necessarily of the same
    length. \code{x} can be a numeric matrix with 3 rows for
    \code{ltransform3dto3d}}
  \item{subscripts}{which points to draw}
  \item{distance, par.box}{described in \code{\link{cloud}}}
  \item{xlim, ylim, zlim}{limits}
  \item{panel.3d.cloud}{function that draws the data-driven part of the
    plot (as opposed to the bounding box and scales) in \code{cloud} }
  \item{panel.3d.wireframe}{the same for \code{wireframe}}
  \item{aspect}{aspect as in \code{cloud}}
  \item{xlab, ylab, zlab}{ Labels, have to be lists}
  \item{scales.3d}{ list defining the scales}
  \item{proportion}{ Gives the length of arrows as a proportion of the
    sides
  }
  \item{scpos}{A list with three components x, y and z, describing which
    of the 12 sides of the cube the scales should be drawn. The
    defaults should be OK. Valid values are x: 1, 3, 9, 11; y: 8, 5, 7,
    6 and z: 4, 2, 10, 12. See comments in panel.cloud
  }
  \item{wireframe}{logical, indicating whether this is a wireframe plot}
  \item{col.at, col.regions}{deals with specification of colors when
    \code{drape = TRUE} in \code{wireframe}}
  \item{subpanel}{ the function used to plot the points in
    \code{panel.3dscatter} once coordinates of their projections are
    computed.  \code{subpanel="panel.superpose"} along with the
    \code{groups} argument can be used to create grouped displays.}
  \item{screen}{list, as described in \code{\link{cloud}}}
  \item{R.mat, rot.mat}{4x4 transformation matrix in homogeneous
    coordinates}
  \item{za, zb, zmin, zmax, dist}{controls transformation to account for
    perspective viewing}
  \item{zback, zfront, zero}{ same as previous, not for regular use yet}
  \item{type}{ character string, specifying type of cloud plot. Can be
    ``p'', ``l'', ``h''}
  \item{groups}{specification of a grouping variable, passed down from
    the high level functions.}
  \item{shade}{logical, whether lighting is to be used}
  \item{shade.colors}{a function that takes 2 arguments, the cosine of
    twice the angle of between the viewing direction and the direction
    of the light source, and a height between 0 and 1, and returns an
    appropriate color }
  \item{cosangle, height}{as described above}
  \item{saturation}{used as \code{s} in \code{hsv} inside
    \code{palette.shade}}
  \item{light.source}{any vector (cartesian coordinates) in the
    direction of the light source. This is relative to the viewing point
    in the direction of (0,0,1)}
  \item{polynum}{ number of quadrilateral faces to draw
    simultaneously. Trade-off between speed and memory usage.}
  \item{minz, maxz, col.groups}{miscellaneous other arguments.}
  \item{col, col.point, col.line, lty, lwd, cex, pch}{graphical parameters}
  \item{\dots}{ other parameters, passed down when appropriate}
}
\details{
  These functions together deal with the content drawn inside each panel
  in \code{cloud} and \code{wireframe}. \code{panel.wireframe} is the
  same as \code{panel.cloud}, which does the actual work.

  \code{panel.cloud} is responsible for drawing the content that does
  not depend on the data, namely, the bounding box, the arrows/scales,
  etc. At some point, depending on whether \code{wireframe} is TRUE, it
  calls either \code{panel.3d.wireframe} or \code{panel.3d.cloud}, which
  draws the data-driven part of the plot.

  The arguments accepted by these two functions are different, since
  they have essentially different purposes. For cloud, the data is
  unstructured, and \code{x, y, z} are all passed to the
  \code{panel.3d.cloud} function. For wireframe, on the other hand,
  \code{x} and \code{y} are increasing vectors with unique values,
  defining a rectangular. \code{z} must be a matrix with \code{length(x)
    * length(y)} rows, and as many columns as the number of groups.

  \code{panel.3dscatter} is the default \code{panel.3d.cloud} function,
  which computes the projection of the points and calls the
  \code{subpanel} function to draw the points in 2-D. (It's a very
  simple function, and could serve as a model for customizing the
  contents of the panels in \code{cloud}.)

  \code{panel.3dscatter.new} is intended as a replacement for
  \code{panel.3dscatter} in future versions. Has a \code{type} argument
  to control type of plot. Has some nice properties, but is still in an
  experimental stage.
  
  \code{panel.3dwire} is the default \code{panel.3d.wireframe} function,
  and \code{panel.3dwire.slow} is its older, slower cousin. The two
  differ only in that the slower version draws the wireframe facets one
  at a time, whereas the newer version waits till it has collected
  information about \code{polynum} facets, and draws them all at
  once. This avoids the overhead of drawing \code{grid.polygon}
  repeatedly, speeding up the rendering considerably. If \code{shade =
    TRUE}, these attempt to color the surface as being illuminated from
  a light source at infinite distance, in the direction given by
  \code{light.source}. \code{palette.shade} is a simple function that
  returns the apropriate colors required for shading given the cosine of
  twice the angle of between the viewing direction and the direction of
  the light source, and a height between 0 and 1. (No shadows are
  formed, though.)  Multiple surfaces are drawn if \code{groups} is
  non-null in the call to \code{wireframe}, however, the algorithm is
  not sophisticated enough to render intersecting surfaces correctly.

  \code{ltransform3dMatrix} and \code{ltransform3dto3d} are utility
  functions to help computation of projections. The first function takes
  a list of the form of the \code{screen} argument in \code{cloud} and
  \code{wireframe} and a \code{R.mat}, a 4x4 transformation matrix in
  homogeneous coordinates, to return a new 4x4 transformation matrix that
  is the  result of applying \code{R.mat} followed by the rotations in
  \code{screen}. The second function applies a 4x4 transformation matrix
  in homogeneous coordinates to a 3xn matrix representing points in 3-D
  space, and optionally does some perspective computations. (There has
  been no testing with non-trivial transformation matrices, and my
  knowledge of the homogeneous coordinate system is very limited, so
  there might be bugs here.)
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{cloud}}}
\keyword{dplot}
















