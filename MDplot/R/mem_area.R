# load the area (local orthogonal displacement) input
load_mem_area <- function( path,
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

# membrane area plot
mem_area <- function( triangleData,
                      printLegend = TRUE,
                      spaceUnit = "nm",
                      barePlot = FALSE,
                      ... )
{
  # fill
}

# load the area timeseries input
load_mem_area_ts <- function( path,
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

# membrane area timeseries plot
mem_area_ts <- function( triangleData,
                         printLegend = TRUE,
                         spaceUnit = "nm",
                         timeUnit = "ns",
                         barePlot = FALSE,
                         ... )
{
  # fill
}