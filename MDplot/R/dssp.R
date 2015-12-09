# plot multiple DSSP plots
dssp_summary_multi <- function( LIST_input,
                                STRING_selectedMotif,
                                BOOL_printLegend = FALSE,
                                VEC_colours = NA,
                                VEC_showResidues = NA,
                                BOOL_barePlot = FALSE,
                                ... )
{
  if( length( LIST_input ) < 2 )
    stop( "Error occured since the length of the input list is less than two." )
  
  # specify graphical settings and colours, in case a legend has been requested (or not)
  if( BOOL_printLegend )
    layout( matrix( 1:2, ncol = 2 ), width = c( 0.825, 0.175 ), height = c( 1, 1 ) )
  if( all( is.na( VEC_colours ) ) )
  {
    PALETTE_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    VEC_colours <- PALETTE_colours( length( LIST_input ) )
  }
  #########
  MDplot_DSSP_summary( LIST_input[[ 1 ]][[ "matrix" ]],
                       VEC_selectedMotifs = c( STRING_selectedMotif ),
                       BOOL_barePlot = BOOL_barePlot,
                       STRING_plotType = "curves",
                       VEC_colours = c( VEC_colours[ 1 ] ),
                       VEC_showResidues = VEC_showResidues,
                       ... )
  for( i in 2:length( LIST_input ) )
  {
    par( new = TRUE )
    MDplot_DSSP_summary( LIST_input[[ i ]][[ "matrix" ]],
                         VEC_selectedMotifs = c( STRING_selectedMotif ),
                         BOOL_barePlot = TRUE,
                         STRING_plotType = "curves",
                         VEC_colours = c( VEC_colours[ i ] ),
                         VEC_showResidues = VEC_showResidues,
                         ... )
  }
  
  # print legend, if flag is set
  if( BOOL_printLegend )
  {
    plot.new()
    legend( "right",
            legend = unlist( lapply( LIST_input, function( x ) x[[ "name" ]] ) ),
            col = VEC_colours,
            lty = 0, lwd = 0, bty = "n",
            pch = 19, cex = 1, xpd = TRUE )
  }
  #########
}

# get average summary table
averaging_dssp_summary <- function( VEC_files )
{
  if( length( VEC_files ) < 2 )
    stop( "Error because no input files (two at least) have been specified!" )
  MAT_average <- MDplot_load_DSSP_summary( VEC_files[ 1 ] )
  for( i in 2:length( VEC_files ) )
    MAT_average <- MAT_average +
                   MDplot_load_DSSP_summary( VEC_files[ i ] )
  MAT_average <- MAT_average / length( VEC_files )
  colnames( MAT_average ) <- c( "residuenumber", "# 3-Helix", "3-Helix",
                                "# 4-Helix", "4-Helix", "# 5-Helix",
                                "5-Helix", "# Turn", "Turn",
                                "# B-Strand", "B-Strand", "# B-Bridge",
                                "B-Bridge", "# Bend", "Bend" )
  return( MAT_average )
}

# load and return input
load_dssp_summary <- function( STRING_input )
{
  return( TABLE_input <- read.table( STRING_input ) )
}

# plot the summary over residues and values (selected)
# WARNING because residues are renumbered if selected
dssp_summary <- function( TABLE_datainput,
                          BOOL_printLegend = FALSE,
                          BOOL_useOwnLegend = FALSE,
                          VEC_namesLegend = NA,
                          VEC_colours = NA,
                          VEC_showValues = NA,
                          VEC_showResidues = NA,
                          STRING_plotType = "dots",
                          VEC_selectedMotifs = NA,
                          BOOL_barePlot = FALSE,
                          xlab = "residues",
                          ylab = "occurences [%]",
                          ... )
{
  
  # parse input table and get all values in a matrix
  VEC_residues <- TABLE_datainput[ , 1 ]
  TABLE_datainput <- TABLE_datainput[ , -1 ]
  MAT_data <- as.matrix( TABLE_datainput[ , c( F, T ) ] )
  colnames( MAT_data ) <- c( "3-Helix", "4-Helix", "5-Helix",
                             "Turn", "B-Strand", "B-Bridge",
                             "Bend" )
  #########
  
  # check plot type, delete non-selected motifs and check legend names
  if( STRING_plotType != "dots" &&
      STRING_plotType != "bars" &&
      STRING_plotType != "curves" )
    stop( paste( "Error while analysing plot type in 'MDplot_DSSP_summary()', type ",
                 STRING_plotType,
                 " is not known!",
                 sep = "" ) )
  if( all( is.na( VEC_selectedMotifs ) ) )
    VEC_selectedMotifs <- colnames( MAT_data )
  MAT_data <- MAT_data[ ,
                        ifelse( colnames( MAT_data ) %in% VEC_selectedMotifs,
                                TRUE,
                                FALSE ),
                        drop = FALSE ]
  if( BOOL_useOwnLegend )
    if( all( is.na( VEC_namesLegend ) ) ||
        length( VEC_namesLegend ) != ncol( MAT_data ) )
      stop( "Error while trying to name the columns in the input matrix ",
            "according to specification, since either no 'VEC_namesLegend' has been ",
            "supplied or it has the wrong length!",
            sep = "" )
    else
      colnames( MAT_data ) <- VEC_namesLegend
  #########
  
  # if certain range of residues is to be shown, remove the rest
  MAT_buffer <- MAT_data
  if( !all( is.na( VEC_showResidues ) ) )
    for( i in nrow( MAT_buffer ):1 )
      if( !( i %in% VEC_showResidues[ 1 ]:VEC_showResidues[ 2 ] ) )
        #use "drop = FALSE" to avoid dimension reduction
        MAT_buffer <- MAT_buffer[ -i, , drop = FALSE ]
  MAT_data <- MAT_buffer
  #########
  
  # if no colour vector has been supplied, create one now
  if( all( is.na( VEC_colours ) ) )
  {
    PALETTE_DSSP_summary_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    VEC_colours <- PALETTE_DSSP_summary_colours( ncol( MAT_data ) )
  }
  #########
  
  # if certain range of values is to be shown, remove the rest
  if( all( is.na( VEC_showValues  ) ) )
    VEC_showValues = rep( 1:ncol( MAT_data ) )
  MAT_buffer <- MAT_data
  for( i in ncol( MAT_buffer ):1 )
    if( !( i %in% VEC_showValues ) )
      # use "drop = FALSE" to avoid dimension reduction
      MAT_buffer <- MAT_buffer[ , -i, drop = FALSE ]
  MAT_data <- MAT_buffer
  #########
  
  # specify graphical settings, in case a legend has been requested (or not)
  par( mar = c( 4.5, 4.5, 2.5, ifelse( BOOL_printLegend,
                                       7.5,
                                       2.5 ) ) )
  #########
  
  # dots plot variant
  if( STRING_plotType == "dots" )
  {
    plot( rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ), MAT_data[ 1, ],
          xlim = c( 1, nrow( MAT_data ) ),
          xlab = ifelse( BOOL_barePlot, "", "residues" ),
          xaxs = "i", xaxt = ifelse( BOOL_barePlot, "n", "s" ),
          ylim = c( 0, 100 ),
          ylab = ifelse( BOOL_barePlot, "", "occurences [%]" ),
          yaxs = "i", yaxt = ifelse( BOOL_barePlot, "n", "s" ),
          col = VEC_colours, 
          cex = 0.9, pch = 19, bty = ifelse( BOOL_barePlot, "n", "o" ) )
    if( nrow( MAT_data ) > 1 )
      for( i in 2:nrow( MAT_data ) )
    {
      par( new = TRUE )
      plot( rep( VEC_residues[[ i ]], each = ncol( MAT_data ) ),
            MAT_data[ i, ],
            col = VEC_colours, 
            xaxs = "i", xaxt = "n", xlab = "", xlim = c( 1, nrow( MAT_data ) ),
            yaxs = "i", yaxt = "n", ylab = "", ylim = c( 0, 100 ),
            cex = 0.9, pch = 19, bty = "n" )
      }
  }
  #########
  
  # bars plot variant
  if( STRING_plotType == "bars" )
  {
    stop( "Type 'bars' is not yet implemented, sorry!" )
    #plot( rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ), MAT_data[ 1, ],
    #      xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), col = VEC_colours, 
    #      xlab = "residues", ylab = "occurences [%]",  xaxs = "i", yaxs = "i", 
    #      cex = 0.9, pch = 19, ... )
    #if( nrow( MAT_data ) > 1 )
    #  for( i in 2:nrow( MAT_data ) )
    #  {
    #    par( new = TRUE )
    #    plot( rep( VEC_residues[[ i ]], each = ncol( MAT_data ) ),
    #          MAT_data[ i, ],
    #          col = VEC_colours, 
    #          xaxs = "i", xaxt = "n", xlab = "", xlim = c( 1, nrow( MAT_data ) ),
    #          yaxs = "i", yaxt = "n", ylab = "", ylim = c( 0, 100 ),
    #          cex = 0.75, pch = 19 )
    #  }
  }
  #########
  
  # curves plot variant
  if( STRING_plotType == "curves" )
  {
    plot( MAT_data[ , 1 ],
          xlim = c( 1, nrow( MAT_data ) ),
          xlab = ifelse( BOOL_barePlot, "", "residues" ),
          xaxs = "i", xaxt = ifelse( BOOL_barePlot, "n", "s" ),
          ylim = c( 0, 100 ),
          ylab = ifelse( BOOL_barePlot, "", "occurences [%]" ),
          yaxs = "i", yaxt = ifelse( BOOL_barePlot, "n", "s" ),
          col = VEC_colours[ 1 ],
          type = "l",
          bty = ifelse( BOOL_barePlot, "n", "o" ), ... )
    if( ncol( MAT_data ) > 1 )
      for( i in 2:ncol( MAT_data ) )
      {
        par( new = TRUE )
        plot( MAT_data[ , i ],
              col = VEC_colours[ i ], 
              xaxs = "i", xaxt = "n", xlab = "", xlim = c( 1, nrow( MAT_data ) ),
              yaxs = "i", yaxt = "n", ylab = "", ylim = c( 0, 100 ),
              type = "l", bty = "n" )
      }
  }
  #########
  
  # print legend, if flag is set
  if( BOOL_printLegend )
  {    
    legend( 110,
            75,
            legend = colnames( MAT_data ),
            col = VEC_colours,
            lty = 0, lwd = 0, bty = "n",
            pch = 19, cex = 1, xpd = TRUE )
  }
  #########
}

# load the time-series files
load_dssp_ts <- function( STRING_folder )
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
    if( file.info( STRING_file )$size == 0 )
      next
    TABLE_current <- read.table( STRING_file )
    LIST_current <- list( name = VEC_gromos_names[ i ],
                          values = TABLE_current )
    LIST_return[[ length( LIST_return ) + 1 ]] <- LIST_current
  }
  return( LIST_return )
}

# BUG: time in nanoseconds does not work!
# plot the time-series files, that are specified
dssp_ts <- function( LIST_timeseries,
                     BOOL_printLegend = TRUE,
                     VEC_timeBoundaries = NA,
                     VEC_residueBoundaries = NA,
                     BOOL_printNanoseconds = FALSE,
                     REAL_snapshotsPerNS = 1000,
                     ... )
{
  STRING_time_unit <- "snapshots"
  if( BOOL_printNanoseconds )
  {
    for( i in 1:length( LIST_timeseries ) )
      LIST_timeseries[[ i ]][[ "values" ]][ 1 ] <- LIST_timeseries[[ i ]][[ "values" ]][ 1 ] /
                                                   REAL_snapshotsPerNS
    STRING_time_unit <- "ns"
    VEC_timeBoundaries <- VEC_timeBoundaries / REAL_snapshotsPerNS
  }
  if( is.na( VEC_timeBoundaries ) )
    VEC_timeBoundaries <- c( min( unlist( lapply( LIST_timeseries,
                                                  function( x ) x[[ "values" ]][ 1 ] ) ) ),
                             max( unlist( lapply( LIST_timeseries,
                                                  function( x ) x[[ "values" ]][ 1 ] ) ) ) )
  if( is.na( VEC_residueBoundaries ) )
    VEC_residueBoundaries <- c( min( unlist( lapply( LIST_timeseries,
                                                     function( x ) x[[ "values" ]][ 2 ] ) ) ),
                                max( unlist( lapply( LIST_timeseries,
                                                     function( x ) x[[ "values" ]][ 2 ] ) ) ) )
  PALETTE_DSSP_timeseries_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  VEC_colours <- PALETTE_DSSP_timeseries_colours( length( LIST_timeseries ) )
  
  # specify graphical settings, in case a legend has been requested (or not)
  par( mar = c( 4.5, 4.5, 2.5, ifelse( BOOL_printLegend, 10.0, 2.5 ) ) )

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
    legend( "topright", inset = c( -0.325, 0.3 ),
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