# load the loc (local curvature) input
load_mem_loc <- function( path,
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

# loc (local curvature) plot
mem_loc <- function( locValues,
                     printLegend = TRUE,
                     barePlot = FALSE,
                     ... )
{
  # fill
}

# load the loc (local curvature) timeseries input
load_mem_loc_ts <- function( path,
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

# loc (local curvature) timeseries plot
mem_loc_ts <- function( locValues,
                        printLegend = TRUE,
                        timeUnit = "ns",
                        barePlot = FALSE,
                        ... )
{
  # fill
}