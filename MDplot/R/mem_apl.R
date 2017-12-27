# load the area per lipid input
load_mem_apl <- function( path,
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

# area per lipid plot
mem_apl <- function( triangleData,
                     printLegend = TRUE,
                     spaceUnit = "nm",
                     barePlot = FALSE,
                     ... )
{
  # fill
}