# load required libraries
library( "MDplot" )
#########

# get values and pre-define regions
# (which are lists, containing a name and four points in consecutive order)
MAT_dihedrals <- MDplot_load_ramachandran( STRING_file = "./data/MDplot_ramachandran_example.txt",
                                           VEC_columns = c( 3, 4 ) )
LIST_alpha <- list( "alpha", c( -160,  -70 ), c( -160,   40 ), c(  -40,   40 ), c(  -40,  -70 ) )
LIST_beta  <- list(  "beta", c( -100,   90 ), c( -180,   90 ), c( -180,  180 ), c( -100,  180 ) )
LIST_PII   <- list(   "PII", c(  -30,   90 ), c( -100,   90 ), c( -100,  180 ), c(  -30,  180 ) )
MDplot_ramachandran( MAT_dihedrals, xbins = 600, ybins = 600,
                     LIST_areas = list( LIST_alpha, LIST_beta, LIST_PII ),
                     plotTitle = "Random Ramachandran plot" )
#########