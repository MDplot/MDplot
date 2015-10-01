# load and return input
MDplot_load_DSSP_summary <- function( STRING_input )
{
  return( TABLE_input <- read.table( STRING_input ) )
}

# plot the summary over residues and values (selected)
MDplot_DSSP_summary <- function( TABLE_datainput,
                                 BOOL_printLegend = FALSE,
                                 BOOL_useOwnLegend = FALSE,
                                 COLOURS_DSSP_summary = NULL,
                                 VEC_showValues = NULL,
                                 VEC_showResidues = NULL,
                                 ... )
{
  
  # parse input table and get all values in a matrix
  VEC_residues <- TABLE_datainput[ , 1 ]
  TABLE_datainput <- TABLE_datainput[ , -1 ]
  MAT_data <- as.matrix( TABLE_datainput[ , c( F, T ) ] )
  MAT_buffer <- MAT_data
  #########
  
  # if certain range of residues is to be shown, remove the rest
  if( !is.null( VEC_showResidues ) )
  {
    for( i in nrow( MAT_buffer ):1 )
    {
      if( !( i %in% VEC_showResidues ) )
      {
        #use "drop = FALSE" to avoid dimension reduction
        MAT_buffer <- MAT_buffer[ -i, , drop = FALSE ]
      }
    }
  }
  MAT_data <- MAT_buffer
  #########
  
  # if no colour vector has been supplied, create one now
  if( is.null( COLOURS_DSSP_summary ) )
  {
    PALETTE_DSSP_summary_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    COLOURS_DSSP_summary <- PALETTE_DSSP_summary_colours( ncol( MAT_data ) )
  }
  #########
  
  # if certain range of values is to be shown, remove the rest
  if( is.null( VEC_showValues  ) )
  {
    VEC_showValues = rep( 1:ncol( MAT_data ) )
  }
  MAT_buffer <- MAT_data
  for( i in ncol( MAT_buffer ):1 )
  {
    if( !( i %in% VEC_showValues ) )
    {
      # use "drop = FALSE" to avoid dimension reduction
      MAT_buffer <- MAT_buffer[ , -i, drop = FALSE ]
    }
  }
  MAT_data <- MAT_buffer
  #########
  
  # specify graphical settings, in case a legend has been requested (or not)
  if( BOOL_printLegend )
  {
    par( mar = c( 4.5, 4.5, 2.5, 12 ) )
  }
  else
  { 
    par( mar = c( 4.5, 4.5, 2.5, 2.5 ) )
  }
  #########
  
  # plot the values of the first secondary structure kind, followed
  # by the others
  plot( rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ), MAT_data[ 1, ],
        xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), col = COLOURS_DSSP_summary, 
        xlab = "residues", ylab = "fractions [%]",  xaxs = "i", yaxs = "i", 
        cex = 0.9, pch = 19, ... )
  if( nrow( MAT_data ) > 1 )
  {
    for( i in 2:nrow( MAT_data ) )
    {
      par( new = TRUE )
      plot( rep( VEC_residues[[ i ]], each = ncol( MAT_data ) ), MAT_data[ i, ], col = COLOURS_DSSP_summary, 
            xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
            xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), cex = 0.75, pch = 19 )
    }
  }
  #########
  
  # print legend, if flag is set
  if( BOOL_printLegend )
  {
    VEC_namesLegend = c( "3-Helix", "4-Helix", "5-Helix",
                         "Turn", "B-strand", "B-Bridge",
                         "Bend", rep( NA, 16 ) )
    
    # in case, no own order has been specified
    if( !BOOL_useOwnLegend )
      colnames( MAT_data ) <- VEC_namesLegend[ 1:ncol( MAT_data ) ]
    #########
    
    par( xpd = TRUE )
    legend( 110,
            75,
            legend = colnames( MAT_data ),
            col = COLOURS_DSSP_summary,
            lty = 0, lwd = 0,
            pch = 19, cex = 1 )
    par( xpd = FALSE )
  }
  #########
}

# load the time-series files
MDplot_load_DSSP_timeseries <- function( STRING_folder )
{
  VEC_gromos_names <- c( "3-Helix", "4-Helix", "5-Helix",
                         "Bend", "Beta-Bridge", "Beta-Strand",
                         "Turn" )
  STRING_gromos_postfix <- ".out"
  LIST_return <- list()
  for( i in 1:length( VEC_gromos_names ) )
  {
    STRING_file <- paste( STRING_folder,
                          "/",
                          VEC_gromos_names[ i ],
                          STRING_gromos_postfix, sep = "" )
    if( !file.exists( STRING_file ) )
      next
    TABLE_current <- read.table( STRING_file )
    LIST_current <- list( name = VEC_gromos_names[ i ],
                          values = TABLE_current )
    LIST_return[[ length( LIST_return ) + 1 ]] <- LIST_current
  }
  return( LIST_return )
}

# plot the time-series files, that are specified
MDplot_DSSP_timeseries <- function( LIST_timeseries,
                                    BOOL_printLegend = TRUE,
                                    VEC_timeBoundaries = NULL,
                                    VEC_residueBoundaries = NULL,
                                    BOOL_printNanoseconds = FALSE,
                                    REAL_snapshotsPerNS = 1000,
                                    ... )
{
  STRING_time_unit <- "snapshots"
  if( BOOL_printNanoseconds )
  {
    for( i in 1:length( LIST_timeseries ) )
    {
      LIST_timeseries[[ i ]][[ "values" ]][ 1 ] <- LIST_timeseries[[ i ]][[ "values" ]][ 1 ] / REAL_snapshotsPerNS
    }
    STRING_time_unit <- "ns"
    VEC_timeBoundaries <- VEC_timeBoundaries / REAL_snapshotsPerNS
  }
  if( is.null( VEC_timeBoundaries ) )
    VEC_timeBoundaries <- c( min( unlist( lapply( LIST_timeseries,
                                   function( x ) x[[ "values" ]][ 1 ] ) ) ),
                              max( unlist( lapply( LIST_timeseries,
                                   function( x ) x[[ "values" ]][ 1 ] ) ) ) )
  if( is.null( VEC_residueBoundaries ) )
    VEC_residueBoundaries <- c( min( unlist( lapply( LIST_timeseries,
                                      function( x ) x[[ "values" ]][ 2 ] ) ) ),
                                 max( unlist( lapply( LIST_timeseries,
                                      function( x ) x[[ "values" ]][ 2 ] ) ) ) )
  PALETTE_DSSP_timeseries_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  VEC_colours <- PALETTE_DSSP_timeseries_colours( length( LIST_timeseries ) )
  
  # specify graphical settings, in case a legend has been requested (or not)
  if( BOOL_printLegend )
  {
    par( mar = c( 4.5, 4.5, 2.5, 12 ) )
  }
  else
  { 
    par( mar = c( 4.5, 4.5, 2.5, 2.5 ) )
  }
  #########
  for( i in 1:length( LIST_timeseries )  )
  {
    if( i < 2 )
    {
      plot( LIST_timeseries[[ i ]][[ "values" ]],
            xlim = VEC_timeBoundaries,
            ylim = VEC_residueBoundaries,
            xaxs = "i", yaxs = "i",
            xlab = paste( "time [", STRING_time_unit, "]", sep = "" ),
            ylab = "residue number",
            pch = 22, col = VEC_colours[ i ], bg = VEC_colours[ i ], cex = 0.25,
            ... )
    }
    else
    {
      par( new = TRUE )
      plot( LIST_timeseries[[ i ]][[ "values" ]],
            xlim = VEC_timeBoundaries,
            ylim = VEC_residueBoundaries,
            xaxs = "i", yaxs = "i",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            pch = 22, col = VEC_colours[ i ], bg = VEC_colours[ i ], cex = 0.25 )
    }
  }
  
  # print legend, if flag is set
  if( BOOL_printLegend )
  {
    par( xpd = TRUE )
    legend( "topright", inset = c( -0.325, 0.4 ),
            legend = unlist( lapply( LIST_timeseries,
                                     function( x ) x[[ "name" ]] ) ),
            pch = 22, col = VEC_colours, pt.bg = VEC_colours, pt.cex = 1.25,
            lty = 0, lwd = 0,
            cex = 1,
            bty = "n" )
    par( xpd = FALSE )
  }
  #########
}