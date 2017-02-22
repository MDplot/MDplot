install.packages( "MDplot", dependencies = TRUE,
                  INSTALL_opts = c( "--no-lock" ),
                  repos = "http://cran.wu.ac.at" )
if( require( MDplot ) )
{
  print( paste( "Installation of MDplot version ",
                packageVersion( "MDplot" ),
                " succeeded!", sep = "" ) )
}else
{
  print( "Installation of MDplot could not be completed successfully!" )
}