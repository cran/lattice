\name{latticeParseFormula}
\title{Parse Trellis formula}
\usage{
latticeParseFormula(model, data, dimension = 2)
}
\synopsis{ Parses the formula argument of trellis functions.}
\alias{latticeParseFormula}
\arguments{
  \item{model}{ the model/formula to be parsed.
    This can be in either of two possible forms, one for 2d and one for
    3d formulas, determined by the \code{dimension} argument. The 2d
    formulas are of the form \code{y~x|g_1*\dots*g_n}, and the 3d
    formulas are of the form \code{z~x*y|g_1*\dots*g_n}. In the first
    form, \code{y} may be omitted. The conditioning variables \code{g_1,
      \dots,g_n} can be omitted in either case.
    }
    \item{data}{ the environment/dataset where the variables in the 
	formula are evaluated. By default, the environment where the
	function was called from.
    }
    \item{dimension}{dimension of the model, see above}
}
\value { returns a list with components \code{left, right, left.name, 
	right.name, condition} for 2d, and \code{left, right.x, right.y,
	left.name, right.x.name, right.y.name, condition} for 3d.
}
\description{ this function is used in trellis functions. Might
    be useful by itself. \code{y} and \code{g_1,\dots,\g_n} may be missing.
}
\seealso{
    \code{\link{xyplot}}, \code{\link{Lattice}}
}
\author{Saikat DebRoy \email{saikat@stat.wisc.edu}}
\keyword{utilities}
