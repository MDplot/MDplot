# load the tpc (total patch curvature) input
load_mem_tpc <- function( path,
                          format = "MANTICORE" )
{
  format <- toupper( format )
  if( format != "MANTICORE" )
    stop( paste( "The specified 'format', set to ", format, " is unknown.", sep = "" ) )
  
  if( format == "MANTICORE" )
  {
    # fill
  }
}

# tpc (total patch curvature) plot
mem_tpc <- function( tpcValues,
                     printLegend = TRUE,
                     barePlot = FALSE,
                     ... )
{
  # fill
}