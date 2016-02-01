# load required libraries
library( "MDplot" )
#########

# load cluster data and set trajectory names if necessary
MAT_clusters <- MDplot::load_clusters( system.file( "extdata/clusters_example.txt",
                                                    package = "MDplot" ) )
rownames( MAT_clusters ) <- c( "WT", "test1", "test2",
                               "otherseries1", "otherseries2", "somethingelse" )
MDplot::clusters( MAT_clusters,
                  clustersNumber = 7 )
#########