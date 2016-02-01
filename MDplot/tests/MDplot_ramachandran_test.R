# load required libraries
library( "MDplot" )
#########

# get values and pre-define regions
# (which are lists, containing a name and four points in consecutive order)
MAT_dihedrals <- MDplot::load_ramachandran( system.file( "extdata/ramachandran_example.txt",
                                                          package = "MDplot" ),
                                            angleColumns = c( 1, 2 ) )
LIST_alpha <- list( "alpha", c( -160,  -70 ), c( -160,   40 ), c(  -40,   40 ), c(  -40,  -70 ) )
LIST_beta  <- list(  "beta", c( -100,   90 ), c( -180,   90 ), c( -180,  180 ), c( -100,  180 ) )
LIST_PII   <- list(   "PII", c(  -30,   90 ), c( -100,   90 ), c( -100,  180 ), c(  -30,  180 ) )
MDplot::ramachandran( MAT_dihedrals, xbins = 600, ybins = 600,
                      structureAreas = list( LIST_alpha, LIST_beta, LIST_PII ),
                      main = "Random Ramachandran plot" )
#########