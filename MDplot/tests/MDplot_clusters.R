# load required libraries
library( "MDplot" )
#########

# load cluster data and set trajectory names if necessary
MAT_clusters <- MDplot_load_clusters( "./data/MDplot_clusters_example.txt" )
colnames( MAT_clusters ) <- c( "WT", "test1", "test2",
                               "otherseries1", "otherseries2", "somethingelse" )
MDplot_clusters( MAT_clusters,
                 INT_maximum_number = 7,
                 main = "Random cluster plot",
                 BOOL_trajectory_names = TRUE )
#########