# load the lod (local orthogonal displacement) input
load_mem_lod <- function( path,
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

# lod (local orthogonal displacement) plot
mem_lod <- function( lodValues,
                 printLegend = TRUE,
                 spaceUnit = "nm",
                 barePlot = FALSE,
                 ... )
{
  # fill
}

# load the lod (local orthogonal displacement) timeseries input
load_mem_lod_ts <- function( path,
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

# lod (local orthogonal displacement) timeseries plot
mem_lod_ts <- function( lodValues,
                    printLegend = TRUE,
                    spaceUnit = "nm",
                    timeUnit = "ns",
                    barePlot = FALSE,
                    ... )
{
  # fill
}