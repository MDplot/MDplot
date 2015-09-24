install.packages( "./MDplot_1.0.tar.gz", dependencies = TRUE,
                  repos = NULL, type = "source", INSTALL_opts = c( "--no-lock" ) )
if( require( MDplot ) )
  print( "Installation of MDplot version 1.0 succeeded!" )
if( !require( MDplot ) )
  print( "Installation of MDplot could not be completed successfully!" )