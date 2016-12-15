# plot multiple DSSP plots
dssp_summary_multi <- function( dsspSumMultInput,
                                selectedElements,
                                printLegend = FALSE,
                                colours = NA,
                                showResidues = NA,
                                barePlot = FALSE,
                                ... )
{
  if( length( dsspSumMultInput ) < 2 )
    stop( "Error occured since the length of the input list is less than two." )
  
  # specify graphical settings and colours, in case a legend has been requested (or not)
  if( printLegend )
    layout( matrix( 1:2, ncol = 2 ), widths = c( 0.825, 0.175 ), heights = c( 1, 1 ) )
  if( all( is.na( colours ) ) )
  {
    PALETTE_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    colours <- PALETTE_colours( length( dsspSumMultInput ) )
  }
  #########
  MDplot::dssp_summary( dsspSumMultInput[[ 1 ]][[ "matrix" ]],
                        selectedElements = c( selectedElements ),
                        barePlot = barePlot,
                        plotType = "curves",
                        colours = c( colours[ 1 ] ),
                        showResidues = showResidues,
                        ... )
  for( i in 2:length( dsspSumMultInput ) )
  {
    par( new = TRUE )
    MDplot::dssp_summary( dsspSumMultInput[[ i ]][[ "matrix" ]],
                          selectedElements = c( selectedElements ),
                          barePlot = TRUE,
                          plotType = "curves",
                          colours = c( colours[ i ] ),
                          showResidues = showResidues,
                          ... )
  }
  
  # print legend, if flag is set
  if( printLegend )
  {
    plot.new()
    legend( "right",
            legend = unlist( lapply( dsspSumMultInput, function( x ) x[[ "name" ]] ) ),
            col = colours,
            lty = 0, lwd = 0, bty = "n",
            pch = 19, cex = 1.0, xpd = TRUE )
  }
  #########
}

# get average summary table
averaging_dssp_summary <- function( VEC_files )
{
  if( length( VEC_files ) < 2 )
    stop( "Error because no input files (two at least) have been specified!" )
  MAT_average <- MDplot::load_dssp_summary( VEC_files[ 1 ] )
  for( i in 2:length( VEC_files ) )
    MAT_average <- MAT_average +
                   MDplot::load_dssp_summary( VEC_files[ i ] )
  MAT_average <- MAT_average / length( VEC_files )
  colnames( MAT_average ) <- c( "residuenumber", "# 3-Helix", "3-Helix",
                                "# 4-Helix", "4-Helix", "# 5-Helix",
                                "5-Helix", "# Turn", "Turn",
                                "# B-Strand", "B-Strand", "# B-Bridge",
                                "B-Bridge", "# Bend", "Bend" )
  return( MAT_average )
}

# load and return GROMACS input
load_dssp_GROMACS <- function( path )
{
  inputData <- readLines( path,
                          warn = FALSE )[ -1 ]
  MAT_dataBuffer <- NA
  for( i in 1:length( inputData ) )
    if( all( is.na( MAT_dataBuffer ) ) )
    {
      MAT_dataBuffer <- matrix( unlist( strsplit( inputData[ i ],
                                                  split = "" ) ),
                                nrow = 1 )
    }
  else
  {
    MAT_dataBuffer <- rbind( MAT_dataBuffer,
                             unlist( strsplit( inputData[ i ],
                                               split = "" ) ) )
  }
  return( MAT_dataBuffer )
}

# load and return input
load_dssp_summary <- function( path,
                               mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" &&
      mdEngine != "GROMACS" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )
  if( mdEngine == "GROMOS" )
  {
    dsspData <- read.table( path )
    MAT_data <- as.matrix( dsspData[ , -1][ , c( F, T ) ] )
    MAT_data <- cbind( dsspData[ , 1 ],
                       MAT_data )
    colnames( MAT_data ) <- c( "residue", "3-Helix", "4-Helix",
                               "5-Helix", "Turn", "B-Strand",
                               "B-Bridge", "Bend" )
    return( MAT_data )
  }
  if( mdEngine == "GROMACS" )
  {
    
    MAT_dataBuffer <- load_dssp_GROMACS( path )
    MAT_data <- matrix( c( 1:ncol( MAT_dataBuffer ),
                           rep( 0,
                               times = ncol( MAT_dataBuffer ) * 8 ) ),
                        ncol = 9, byrow = FALSE )
    VEC_abbrev <- c( "I", "G", "~",
                     "S", "T", "H",
                     "B", "E" )
    colnames( MAT_data ) <- c( "residue", "5-Helix", "3-Helix",
                               "Coil", "Bend", "Turn",
                               "4-Helix", "B-Bridge", "B-Strand" )
    for( i in 1:length( VEC_abbrev ) )
      for( j in 1:ncol( MAT_dataBuffer ) )
      {
        INT_occ <- sum( sapply( gregexpr( VEC_abbrev[ i ],
                                          MAT_dataBuffer[ , j ],
                                          fixed = TRUE ),
                                function( x ) sum( x > -1 ) ) )
        MAT_data[ j, i + 1 ] <- INT_occ
      }
    MAT_data <- round( MAT_data / nrow( MAT_dataBuffer ) * 100,
                       digits = 2 )
    MAT_data[ , 1 ] <- 1:ncol( MAT_dataBuffer )
    return( MAT_data )
  }
}

# plot the summary over residues and values (selected)
# WARNING because residues are renumbered if selected
dssp_summary <- function( dsspData,
                          printLegend = FALSE,
                          useOwnLegend = FALSE,
                          elementNames = NA,
                          colours = NA,
                          showValues = NA,
                          showResidues = NA,
                          plotType = "dots",
                          selectedElements = NA,
                          barePlot = FALSE,
                          ... )
{
  
  # parse input table and get all values in a matrix
  VEC_residues <- dsspData[ , 1 ]
  MAT_data <- dsspData[ , -1 ]
  #########
  
  # check plot type, delete non-selected motifs and check legend names
  if( plotType != "dots" &&
      plotType != "bars" &&
      plotType != "curves" )
    stop( paste( "Error while analysing plot type in 'MDplot_DSSP_summary()', type ",
                 plotType,
                 " is not known!",
                 sep = "" ) )
  if( all( is.na( selectedElements ) ) )
    selectedElements <- colnames( MAT_data )
  MAT_data <- MAT_data[ ,
                        ifelse( colnames( MAT_data ) %in% selectedElements,
                                TRUE,
                                FALSE ),
                        drop = FALSE ]
  if( useOwnLegend )
    if( all( is.na( elementNames ) ) ||
        length( elementNames ) != ncol( MAT_data ) )
      stop( "Error while trying to name the columns in the input matrix ",
            "according to specification, since either no 'elementNames' has been ",
            "supplied or it has the wrong length!",
            sep = "" )
    else
      colnames( MAT_data ) <- elementNames
  #########
  
  # if certain range of residues is to be shown, remove the rest
  MAT_buffer <- MAT_data
  INT_resStart <- 1 # plot all residues
  if( !all( is.na( showResidues ) ) )
  {
    for( i in nrow( MAT_buffer ):1 )
      if( !( i %in% showResidues[ 1 ]:showResidues[ 2 ] ) )
        MAT_buffer <- MAT_buffer[ -i, , drop = FALSE ] #use "drop = FALSE" to avoid dimension reduction
    INT_resStart<- showResidues[ 1 ]
  }
  MAT_data <- MAT_buffer
  #########
  
  # if no colour vector has been supplied, create one now
  if( all( is.na( colours ) ) )
  {
    PALETTE_DSSP_summary_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    colours <- PALETTE_DSSP_summary_colours( ncol( MAT_data ) )
  }
  #########
  
  # if certain range of values is to be shown, remove the rest
  if( all( is.na( showValues  ) ) )
    showValues = rep( 1:ncol( MAT_data ) )
  MAT_buffer <- MAT_data
  for( i in ncol( MAT_buffer ):1 )
    if( !( i %in% showValues ) )
      # use "drop = FALSE" to avoid dimension reduction
      MAT_buffer <- MAT_buffer[ , -i, drop = FALSE ]
  MAT_data <- MAT_buffer
  #########
  
  # specify graphical settings, in case a legend has been requested (or not) and "barePlot" is set to TRUE or FALSE
  if( !barePlot )
  {
    par( mar = c( 4.5, 4.5, 2.5, 1.0 ) )
    if( printLegend )
      layout( matrix( c( 1, 2 ), nrow = 1, byrow = TRUE ),
              widths = c( 0.825,
                          0.175 ),
              heights = c( 1.0, 1.0 ) )
  }
  #########
  
  defaultArguments <- list( xlab = ifelse( barePlot,
                                           "",
                                           "residues" ),
                            ylab = ifelse( barePlot,
                                           "",
                                           "occurences [%]" ),
                            main = "" )
  ellipsis <- list( ... )
  defaultArguments[ names( ellipsis ) ] <- ellipsis
  ellipsis[ names( defaultArguments ) ] <- defaultArguments  
  # dots plot variant
  if( plotType == "dots" )
  {
    do.call( what = plot,
             c( list( x = rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ),
                      y = MAT_data[ 1, ],
                      xlim = c( 1, nrow( MAT_data ) ),
                      xaxs = "i",
                      xaxt = ifelse( barePlot, "n", "s" ),
                      ylim = c( 0, 100 ),
                      yaxs = "i",
                      yaxt = ifelse( barePlot, "n", "s" ),
                      cex = 0.9,
                      pch = 19,
                      col = colours,
                      bty = ifelse( barePlot, "n", "o" ) ),
                ellipsis ) )
    if( nrow( MAT_data ) > 1 )
      for( i in 2:nrow( MAT_data ) )
    {
      par( new = TRUE )
      plot( rep( VEC_residues[[ i ]], each = ncol( MAT_data ) ),
            MAT_data[ i, ],
            col = colours, 
            xaxs = "i", xaxt = "n", xlab = "",
            xlim = c( 1, nrow( MAT_data ) ),
            yaxs = "i", yaxt = "n", ylab = "", ylim = c( 0, 100 ),
            cex = 0.9, pch = 19, bty = "n" )
      }
  }
  #########
  
  # bars plot variant
  if( plotType == "bars" )
  {
    PLOT_bar <- do.call( what = barplot,
                         c( list( height = t( MAT_data ),
                                  xaxs = "i",
                                  xaxt = ifelse( barePlot, "n", "s" ),
                                  ylim = c( 0, 100 ),
                                  yaxs = "i",
                                  yaxt = ifelse( barePlot, "n", "s" ),
                                  col = colours,
                                  bty = ifelse( barePlot, "n", "o" ) ),
                         ellipsis ) )
    if( !barePlot )
    {
      VEC_atTicks <- PLOT_bar[ c( F, F, F, F, T ) ]
      VEC_atLabels <- seq( 5, by = 5, length.out = length( VEC_atTicks ) )
      axis( side = 1,
            at = VEC_atTicks,
            labels = VEC_atLabels )
    }
  }
  #########
  
  # curves plot variant
  if( plotType == "curves" )
  {
    do.call( what = plot,
             c( list( x = MAT_data[ , 1 ],
                      xlim = c( 1, nrow( MAT_data ) ),
                      xaxs = "i",
                      xaxt = ifelse( barePlot, "n", "s" ),
                      ylim = c( 0, 100 ),
                      yaxs = "i",
                      yaxt = ifelse( barePlot, "n", "s" ),
                      col = colours[ 1 ],
                      type = "l",
                      lwd = 2.0,
                      bty = ifelse( barePlot, "n", "o" ) ),
                ellipsis ) )
    if( ncol( MAT_data ) > 1 )
      for( i in 2:ncol( MAT_data ) )
      {
        par( new = TRUE )
        plot( MAT_data[ , i ],
              col = colours[ i ], 
              xaxs = "i", xaxt = "n", xlab = "", xlim = c( 1, nrow( MAT_data ) ),
              yaxs = "i", yaxt = "n", ylab = "", ylim = c( 0, 100 ),
              type = "l", bty = "n", lwd = 2.0 )
      }
  }
  #########
  
  # print legend, if flag is set
  if( printLegend && !barePlot )
  {
    plot.new()
    legend( "right",
            legend = colnames( MAT_data ),
            col = colours,
            lty = 0, lwd = 0, bty = "n",
            pch = 15, cex = 0.9, xpd = TRUE,
            pt.cex = 1.45 )
  }
  #########
  
  return( MAT_data )
}

# load the time-series files
load_dssp_ts <- function( folder,
                          filenames = NA,
                          stride = 1,
                          mdEngine = "GROMOS" )
{
  mdEngine <- toupper( mdEngine )
  if( mdEngine != "GROMOS" &&
      mdEngine != "GROMACS" )
    stop( paste( "The specified 'mdEngine', set to ", mdEngine, " is unknown.", sep = "" ) )
  LIST_return <- list()
  if( mdEngine == "GROMOS" )
  {
    VEC_gromos_names <- filenames
    if( is.na( filenames ) )
      VEC_gromos_names <- c( "3-Helix", "4-Helix", "5-Helix",
                             "Bend", "Beta-Bridge", "Beta-Strand",
                             "Turn" )
    STRING_gromos_postfix <- ".out"
    for( i in 1:length( VEC_gromos_names ) )
    {
      STRING_file <- paste( folder,
                            "/",
                            VEC_gromos_names[ i ],
                            STRING_gromos_postfix, sep = "" )
      if( !file.exists( STRING_file ) )
        next
      if( file.info( STRING_file )$size == 0 )
        next
      TABLE_current <- read.table( STRING_file )
      if( stride != 1 )
        TABLE_current <- TABLE_current[ c( T, rep( F, times = stride - 1 ) ), ]
      LIST_current <- list( name = VEC_gromos_names[ i ],
                            values = TABLE_current )
      LIST_return[[ length( LIST_return ) + 1 ]] <- LIST_current
    }
    return( LIST_return )
  }
  if( mdEngine == "GROMACS" )
  {
    MAT_dataBuffer <- load_dssp_GROMACS( paste( folder, "/", filenames, sep = "" ) )
    VEC_abbrev <- c( "I", "G", "~",
                     "S", "T", "H",
                     "B", "E" )
    VEC_types <- c( "5-Helix", "3-Helix", "Coil",
                    "Bend", "Turn", "4-Helix",
                    "B-Bridge", "B-Strand" )
    for( i in 1:length( VEC_abbrev ) )
    {
      VEC_tableColumnResidues <- NA
      VEC_tableColumnSnapshot <- NA
      for( j in 1:ncol( MAT_dataBuffer ) )
        for( k in 1:nrow( MAT_dataBuffer ) )
        {
          if( MAT_dataBuffer[ k, j ] == VEC_abbrev[ i ] )
          {
            VEC_tableColumnSnapshot <- c( VEC_tableColumnSnapshot,
                                          k )
            VEC_tableColumnResidues <- c( VEC_tableColumnResidues,
                                          j )
          }
        }
      if( !all( is.na( VEC_tableColumnResidues ) ) )
        LIST_return[[ length( LIST_return ) + 1 ]] <- list( name = VEC_types[ i ],
                                                            values = list( x=VEC_tableColumnSnapshot[ -1 ],
                                                                           y=VEC_tableColumnResidues[ -1 ] ) )
    }
    return( LIST_return )
  }
}

# BUG: time in nanoseconds does not work!
# plot the time-series files, that are specified
dssp_ts <- function( tsData,
                     printLegend = TRUE,
                     timeBoundaries = NA,
                     residueBoundaries = NA,
                     timeUnit = NA,
                     snapshotsPerTimeInt = 1000,
                     barScaleFactor = 0.25,
                     barePlot = FALSE,
                     ... )
{
  STRING_time_unit <- "snapshots"
  if( all( !is.na( timeUnit ) ) )
  {
    for( i in 1:length( tsData ) )
      tsData[[ i ]][[ "values" ]][ 1 ] <- tsData[[ i ]][[ "values" ]][ 1 ] /
                                                   snapshotsPerTimeInt
    STRING_time_unit <- timeUnit
    timeBoundaries <- timeBoundaries / snapshotsPerTimeInt
  }
  if( all( is.na( timeBoundaries ) ) )
    timeBoundaries <- c( min( unlist( lapply( tsData,
                                                  function( x ) x[[ "values" ]][ 1 ] ) ) ),
                             max( unlist( lapply( tsData,
                                                  function( x ) x[[ "values" ]][ 1 ] ) ) ) )
  if( all( is.na( residueBoundaries ) ) )
    residueBoundaries <- c( min( unlist( lapply( tsData,
                                                     function( x ) x[[ "values" ]][ 2 ] ) ) ),
                                max( unlist( lapply( tsData,
                                                     function( x ) x[[ "values" ]][ 2 ] ) ) ) )
  PALETTE_DSSP_timeseries_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  colours <- PALETTE_DSSP_timeseries_colours( length( tsData ) )
  
  # specify graphical settings, in case a legend has been requested (or not)
  if( !barePlot )
  {
    par( mar = c( 4.5, 4.5, 2.5, 0.0 ) )
    if( printLegend )
      layout( matrix( c( 1, 2 ), nrow = 1, byrow = TRUE ),
              widths = c( 0.8,
                          0.2 ),
              heights = c( 1.0, 1.0 ) )
  }
  #########
  
  for( i in 1:length( tsData )  )
  {
    if( i < 2 )
    {
      plot( tsData[[ i ]][[ "values" ]],
            xlim = timeBoundaries,
            ylim = residueBoundaries,
            xaxs = "i", yaxs = "i",
            xaxt = ifelse( barePlot, "n", "s" ),
            yaxt = ifelse( barePlot, "n", "s" ),
            xlab = ifelse( barePlot,
                           "",
                           paste( "time [", STRING_time_unit, "]", sep = "" ) ),
            ylab = ifelse( barePlot,
                           "",
                           "residue number" ),
            pch = 22, col = colours[ i ], bg = colours[ i ], cex = barScaleFactor,
            ... )
    }
    else
    {
      par( new = TRUE )
      plot( tsData[[ i ]][[ "values" ]],
            xlim = timeBoundaries,
            ylim = residueBoundaries,
            xaxs = "i", yaxs = "i",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            pch = 22, col = colours[ i ], bg = colours[ i ], cex = barScaleFactor )
    }
  }
  
  # print legend, if flag is set
  if( printLegend && !barePlot )
  {
    par( mar = c( 4.5, 0.0, 2.5, 1.0 ) )
    plot.new()
    legend( "left",
            legend = unlist( lapply( tsData,
                                     function( x ) x[[ "name" ]] ) ),
            pch = 15, col = colours, pt.bg = colours, pt.cex = 1.25,
            lty = 0, lwd = 0,
            cex = 0.75,
            bty = "n" )
  }
  #########
}