\name{latticeParseFormula}
\title{Parse Trellis formula}
\usage{
latticeParseFormula(model, data)
}
\synopsis{ Parses the formula argument of trellis functions.}
\alias{latticeParseFormula}
\arguments{
    \item{model}{ the model/formula to be parsed, of the form
	\code{y~x|g_1*\dots *g_n}
    }
    \item{data}{ the environment/dataset where the variables in the 
	formula are evaluated. By default, the environment where the
	function was called from.
    }
}
\value { returns a list with components \code{left, right, left.name, 
	right.name, condition}. (More later)
}
\description { this function is used in trellis functions. Might
    be useful by itself. \code{x} and \code{g_1,\dots,\g_n} may be missing.
}
\seealso{
    \code{\link{trellis.args}}, \code{\link{Lattice}}
}
\author{Saikat DebRoy \email{saikat@stat.wisc.edu}}
\keyword{utilities}
