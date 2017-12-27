# load the tmc (total membrane curvature) input
load_mem_tmc <- function( path,
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

# tmc (total membrane curvature) plot
mem_tmc <- function( tmcValues,
                     printLegend = TRUE,
                     barePlot = FALSE,
                     ... )
{
  # fill
}