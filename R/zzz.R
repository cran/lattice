
### Copyright 2000-2003 Deepayan Sarkar <deepayan@stat.wisc.edu>,
###
### This file is part of the lattice library for R.  It is made
### available under the terms of the GNU General Public License,
### version 2, or at your option, any later version, incorporated
### herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA



.onLoad <- function(lib, pkg) {
  library.dynam(pkg, pkg, lib )
  #import(grid) # will this do ?  apparently not
  ## do we want grid functions visible
  ##if (!require(grid))
  ##    stop("lattice requires grid, but grid couldn't be loaded")
}

.LatticeEnv <- new.env()

## Need global variable to handle more in print.trellis
assign(".lattice.print.more", FALSE, env = .LatticeEnv)
assign("lattice.theme", list(), env = .LatticeEnv)

.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lattice", libpath)









# old (pre NAMESPACE version)
#.First.lib <- function(lib, pkg) {
#  library.dynam(pkg, pkg, lib )
#  if (!require(grid))
#      stop("lattice requires grid, but grid couldn't be loaded")
#}

## Need global variable to handle more in print.trellis
#.lattice.print.more <- FALSE



