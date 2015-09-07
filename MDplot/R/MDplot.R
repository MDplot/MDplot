# load required packages
library( MASS )
library( RColorBrewer )
library( gplots )
library( calibrate )
library( gtools )
library( grDevices )
# definitions
VEC_xTicks <- c( -179.9, -90, 0, 90, 180 )
VEC_xLabels <- c( -180, -90, 0, 90, 180 )
PALETTE_histogramColours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )

integrate_curve <- function( MAT_input )
{
  if( ncol( MAT_input ) < 2 )
  {
    stop( paste( "Error: Number of columns in matrix not 2 or higher, but ",
                 ncol( MAT_input ), "!" ) )
  }
  REAL_error <- NA
  if( ncol( MAT_input ) > 2 )
  {
    REAL_error <- 0.0
  }
  REAL_integral <- 0.0
  for( i in 2:nrow( MAT_input ) )
  {
    REAL_integral <- REAL_integral + ( ( MAT_input[ i, 1 ] - MAT_input[ i - 1, 1 ] ) *
                     ( MAT_input[ i, 2 ] + MAT_input[ i - 1, 2 ] ) ) / 2
    if( ncol( MAT_input ) > 2 )
    {
      REAL_error <- REAL_error + ( ( MAT_input[ i, 1 ] - MAT_input[ i - 1, 1 ] ) *
                                     ( MAT_input[ i, 3 ] + MAT_input[ i - 1, 3 ] ) ) / 2
    }
  }
  return( list( integral = REAL_integral, error = REAL_error ) )
}

MDplot_load_TIcurve <- function( STRING_path )
{
  return( as.matrix( read.table( STRING_path ) ) )
}

plot_segments <- function( MAT_values, VEC_spread, REAL_difference = 0.1 )
{
  par( new = TRUE )
  segments( MAT_values[ , 1 ], MAT_values[ , 2 ] - VEC_spread, MAT_values[ , 1 ],
            MAT_values[ , 2 ] + VEC_spread )
  segments( MAT_values[ , 1 ] - REAL_difference, MAT_values[ , 2 ] - VEC_spread,
            MAT_values[ , 1 ] + REAL_difference, MAT_values[ , 2 ] - VEC_spread )
  segments( MAT_values[ , 1 ] - REAL_difference, MAT_values[ , 2 ] + VEC_spread,
            MAT_values[ , 1 ] + REAL_difference, MAT_values[ , 2 ] + VEC_spread )
}

MDplot_TIcurve <- function( MAT_input, STRING_main_title = "Thermodynamic integration" )
{
  par( oma = c( 0.35, 1.75, 0.45, 0.0 ) )
  TIplot <- plot( MAT_input, ylim = c( min( MAT_input[ , 2 ] ), max( MAT_input[ , 2 ] ) ),
                  xaxs = "i", pch = 19, cex = 1.00, xlab = "", ylab = "",
                  main = STRING_main_title )
  plot_segments( MAT_input[ , 1:2 ], MAT_input[ , 3 ], 0.01 )
  mtext( side = 1, text = expression( lambda ), line = 3, cex = 1.45 )
  mtext( side = 2, 
         text = expression( frac( paste( partialdiff, "H" ), paste( partialdiff, lambda ) ) ), 
         line = 2.4, cex = 1.45, las = 1 )
  par( new = TRUE )
  plot( MAT_input, ylim = c( min( MAT_input[ , 2 ] ), max( MAT_input[ , 2 ] ) ), xaxs = "i",
        type = "l", lwd = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "" )
  REAL_integral <- round( unlist( integrate_curve( MAT_input )[ "integral" ] ), 2 )
  REAL_error <- round( unlist( integrate_curve( MAT_input )[ "error" ] ), 2 )
  mtext( side = 1, line = 3.0, cex = 1.25,
         adj = 1,
         text = substitute( paste( Delta, "G = ", REAL_integral, " \u00b1 ", REAL_error,
                                   " [kJ/mol]" ) ) )
  abline( h = 0, lwd = 1, lty = 3 )
}

split_equidistant <- function( VEC_values, n = 5 )
{
  lower_bound = round( VEC_values[[ 1 ]], digits = - ( log10( VEC_values[[ 2 ]] ) - 1 ) )
  upper_bound = round( VEC_values[[ 2 ]], digits = - ( log10( VEC_values[[ 2 ]] ) - 1 ) )
  VEC_return <- c( lower_bound )
  delta <- upper_bound - lower_bound
  it <- delta / ( n - 1 )
  for( i in 1:( n - 2 ) )
  {
    VEC_return <- c( VEC_return, as.integer( lower_bound + it * i ) )
  }
  VEC_return <- c( VEC_return, upper_bound )
  return( VEC_return )
}

calculate_mid <- function( LIST_points )
{
  REAL_x = 0
  REAL_y = 0
  for( i in 1:length( LIST_points ) )
  {
    REAL_x <- REAL_x + LIST_points[[ i ]][[ 1 ]]
    REAL_y <- REAL_y + LIST_points[[ i ]][[ 2 ]]
  }
  return( c( ( REAL_x / length( LIST_points ) ), ( REAL_y / length( LIST_points ) ) ) )
}

MDplot_ramachandran <- function( MAT_dihedrals, xbins = 450, ybins = 450, heatFun = "log", 
                                 plotTitle = "Ramachandran plot", LIST_areas = c( ), ... )
{
  FUN_heatFun = function( x ) log( length( x ) )
  if( heatFun == "log" )
  {}
  else if( heatFun == "norm" )
  {
    FUN_heatFun = function( x ) x
  }
  else
  {
    stop( paste( "Error: the function '", heatFun, "' for parameter 'heatFun' is not defined.", sep = "" ) )
  }
  hist2d( MAT_dihedrals, nbins = c( xbins, ybins ), same.scale = FALSE, na.rm = TRUE, 
          show = TRUE, col = PALETTE_histogramColours( 15 ), xlab = "", ylab = "", xaxs = "i", 
          yaxs = "i", ann = TRUE, xaxt = "n", yaxt = "n", FUN = FUN_heatFun, ... )
  if( length( LIST_areas ) > 0 )
  {
    for( i in 1:length( LIST_areas ) )
    {
      if( length( LIST_areas[[ i ]] ) < 3 )
      {
        next
      }
      for( j in 2:length( LIST_areas[[ i ]] ) )
      {
        if( length( LIST_areas[[ i ]] ) > j )
        {
          segments( LIST_areas[[ i ]][[ j ]][[ 1 ]], LIST_areas[[ i ]][[ j ]][[ 2 ]], 
                    LIST_areas[[ i ]][[ j + 1 ]][[ 1 ]], LIST_areas[[ i ]][[ j + 1 ]][[ 2 ]],
                    col = "black", lwd = 2 )
        }
        else
        {
          segments( LIST_areas[[ i ]][[ j ]][[ 1 ]], LIST_areas[[ i ]][[ j ]][[ 2 ]], 
                    LIST_areas[[ i ]][[ 2 ]][[ 1 ]], LIST_areas[[ i ]][[ 2 ]][[ 2 ]],
                    col = "black", lwd = 2 )
        }
      }
      cen <- calculate_mid( LIST_areas[[ i ]][ -1 ] )
      text( cen[[ 1 ]], cen[[ 2 ]], labels = LIST_areas[[ i ]][[ 1 ]], cex = 1.5 )
    }
  }
  axis( 1, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1 )
  axis( 2, at = VEC_xTicks, labels = VEC_xLabels, cex.axis = 1)
  mtext( side = 1, text = expression( paste( phi, " [", degree, "]" ) ), line = 3, cex = 1.75 )
  mtext( side = 2, text = expression( paste( psi, " [", degree, "]" ) ), line = 2.4, cex = 1.75 )
  title( plotTitle )
}

MDplot_RMSD <- function( MAT_datainput, BOOL_frax = TRUE, REAL_division_factor = 1, plotTitle = "RMSD plot", xunit = "ns", ... )
{
  if( !BOOL_frax )
  {
    newRow <- seq( 1:nrow( MAT_datainput ) )
    MAT_datainput <- cbind( newRow, MAT_datainput )
  }
  PALETTE_RMSD_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_RMSD <- PALETTE_RMSD_colours( ( ncol( MAT_datainput ) - 1 ) )
  TS_datainput <- as.ts( MAT_MDplot_RMSD_example[ , -1 ] )
  plot.ts( TS_datainput, col = COLOURS_RMSD, plot.type = "single", xaxs = "i", xaxt = "n", xlab = "", ylab = "", ... )
  axis( 1, at = split_equidistant( c( 1, nrow( MAT_datainput ) ), 7 ), labels = split_equidistant( c( 1, ( nrow( MAT_datainput ) / REAL_division_factor ) ), 7 ), cex.axis = 1 )
  mtext( side = 1, text = paste( "time [", xunit, "]" ), line = 3, cex = 1.75 )
  mtext( side = 2, text = "RMSD", line = 2.4, cex = 1.75 )
  title( plotTitle )
  legend( "topright", legend = colnames( MAT_MDplot_RMSD_example[ , -1 ] ), col = COLOURS_RMSD, lty = 1, cex = 1 )
}

MDplot_load_rmsf <- function( VEC_files )
{
  LIST_return <- list()
  for( i in 1:length( VEC_files ) )
  {
    TABLE_input <- read.table( VEC_files[ i ] )
    if( length( LIST_return ) == 0 )
    {
      LIST_return <- list( TABLE_input[ , 1 ], TABLE_input[ , 2 ] )
    }
    else
    {
      LIST_return[[ length( LIST_return ) + 1 ]] <- TABLE_input[ , 1 ]
      LIST_return[[ length( LIST_return ) + 1 ]] <- TABLE_input[ , 2 ]
    }
  }
  return( LIST_return )
}

MDplot_RMSF <- function( LIST_datainput, plotTitle = "RMSF plot", xunit = "ns", 
                         VEC_colors = NULL, BOOL_residuewise = FALSE, INT_number_xticks = 7, 
                         ... )
{
  PALETTE_RMSF_colors <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  if( is.null( VEC_colors ) )
  {
    VEC_colors <- PALETTE_RMSF_colors( length( LIST_datainput ) / 2 )
  }
  par( new = FALSE )
  REAL_max_RMSF = max( sapply( LIST_datainput[ c( F, T ) ], max ) )
  INT_max_atomnumber = max( sapply( LIST_datainput[ c( T, F ) ], max ) )
  # find better solution for "every second column as i"
  for( i in 1:length( LIST_datainput ) )
  {
    if( i %% 2 == 1 )
    {
      plot( LIST_datainput[[ i ]], LIST_datainput[[ ( i + 1 ) ]],
            type = "l", col = VEC_colors[ ceiling( i / 2 ) ], xaxs = "i", yaxs = "i",
            xaxt = "n",  xlab = "", ylab = "",
            ylim = c( 0, REAL_max_RMSF ),
            xlim = c( 0, INT_max_atomnumber ),
            ... )
      par( new = TRUE )
    }
  }
  mtext( side = 2, text = "RMSF", line = 2.4, cex = 1.75 )
  VEC_atomnumbers <- 0:INT_max_atomnumber
  if( !BOOL_residuewise )
  {
    mtext( side = 1, text = "atoms", line = 3, cex = 1.75 )
    axis( 1, at = split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks ),
          label = split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks ) )
  }
  else
  {
    mtext( side = 1, text = "residues", line = 3, cex = 1.75 )
    axis( 1, at = split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks ),
          label = as.integer( split_equidistant( c( 0, INT_max_atomnumber ), INT_number_xticks )
                              / 3 ) )
  }
  title( plotTitle )
  #legend( "topright", legend = colnames( XYZ ),
  #        col = COLOURS_RMSF, lty = 1, cex = 1 )
}

MDplot_DSSP_summary <- function( TABLE_datainput, BOOL_printLegend = FALSE, COLOURS_DSSP_summary = NULL, VEC_showValues = NULL,
                                 VEC_show_residues = NULL )
{
  VEC_residues <- TABLE_datainput[ , 1 ]
  TABLE_datainput <- TABLE_datainput[ , -1 ]
  MAT_data <- as.matrix( TABLE_datainput[ , c( F, T ) ] )
  MAT_buffer <- MAT_data
  if( !is.null( VEC_show_residues ) )
  {
    for( i in nrow( MAT_buffer ):1 )
    {
      if( !( i %in% VEC_show_residues ) )
      {
        MAT_buffer <- MAT_buffer[ -i, , drop = FALSE ]
      }
    }
  }
  MAT_data <- MAT_buffer
  if( is.null( COLOURS_DSSP_summary ) )
  {
    PALETTE_DSSP_summary_colours <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    COLOURS_DSSP_summary <- PALETTE_DSSP_summary_colours( ncol( MAT_data ) )
  }
  if( is.null( VEC_showValues  ) )
  {
    VEC_showValues = rep( 1:ncol( MAT_data ) )
  }
  MAT_buffer <- MAT_data
  for( i in ncol( MAT_buffer ):1 )
  {
    if( !( i %in% VEC_showValues ) )
    {
      MAT_buffer <- MAT_buffer[ , -i, drop = FALSE ]
    }
  }
  MAT_data <- MAT_buffer
  if( BOOL_printLegend )
  {
    par( mar = c( 4.5, 4.5, 2.5, 12 ) )
  }
  else
  { 
    par( mar = c( 4.5, 4.5, 2.5, 2.5 ) )
  }
  plot( rep( VEC_residues[[ 1 ]], each = ncol( MAT_data ) ), MAT_data[ 1, ],
        xlim = c( 1, nrow( MAT_data ) ), ylim = c( 0, 100 ), col = COLOURS_DSSP_summary, 
        xlab = "residues", ylab = "fractions [%]",  xaxs = "i", yaxs = "i", 
       cex = 0.75, pch = 19 )
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
  if( BOOL_printLegend )
  {
    par( xpd = TRUE )
    legend( 110, 75, legend = colnames( MAT_data ), col = COLOURS_DSSP_summary, lty = 1, cex = 1 )
    par( xpd = FALSE )
  }
}

MDplot_DSSP_timeseries <- function( VEC_timeseries )
{
  
}

MDplot_load_clusters <- function( STRING_path )
{
  MAT_pre <- as.matrix( read.table( STRING_path ) )[ , -1  ]
  MAT_pre <- MAT_pre[ , ( ( ncol( MAT_pre ) / 2 ) + 1 ):ncol( MAT_pre ) ]
  return( MAT_pre )
}

MDplot_clusters <- function( MAT_clusters, INT_maximum_number = 0, STRING_legend_title = "trajectories", 
                             ylab = "# of cluster members", xlab = "# cluster", main = "cluster plot", 
                             ... )
{
  if( INT_maximum_number != 0 )
  {
    MAT_clusters <- MAT_clusters[ 1:INT_maximum_number, ]
  }
  MAT_clusters <- t( MAT_clusters )
  colnames( MAT_clusters ) <- 1:ncol( MAT_clusters )
  PALETTE_clusters <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
  COLOURS_CLUSTERS <- PALETTE_clusters( nrow( MAT_clusters ) )
  names( MAT_clusters ) <- 1:nrow( MAT_clusters )
  barplot( MAT_clusters, col = COLOURS_CLUSTERS, ylab = ylab, xlab = xlab, main = main, ... )
  legend( "topright", inset = 0.045, legend = 1:nrow( MAT_clusters ), title = STRING_legend_title, box.lwd = 0, col = COLOURS_CLUSTERS, pch = 19, cex = 1.25 )
}

MDplot_load_hbond <- function( STRING_path )
{
  TABLE_input <- read.table( STRING_path, skip = 22 )
  TABLE_input <- TABLE_input[ , c( -19, -18, -17, -16, -14, -13, -11, -10, -6, -5, -2, -1 ) ]
  colnames( TABLE_input ) <- c( "ResDonor", "ResDonorName", "ResAcceptor", "ResAcceptorName", "AtomDonor", "AtomH", "AtomAcceptor", "Percentage" )
  return( TABLE_input )
}

MDplot_hbond <- function( TABLE_input, STRING_what_to_plot = "residue-wise", VEC_acceptorRange = NULL,
                          VEC_donorRange = NULL, BOOL_print_legend = TRUE )
{
  if( STRING_what_to_plot == "residue-wise" )
  {
    TABLE_result <- as.data.frame( NULL )
    VEC_boundariesDonor <- c( min( TABLE_input[ , 1 ] ), max( TABLE_input[ , 1 ] ) )
    VEC_boundariesAcceptor <- c( min( TABLE_input[ , 3 ] ), max( TABLE_input[ , 3 ] ) )
    for( i in 1:nrow( TABLE_input ) )
    {
      BOOL_found = FALSE
      if( nrow( TABLE_result ) != 0 )
      {
        for( j in 1:nrow( TABLE_result ) )
        {
          if( TABLE_result[ j, 1 ] == TABLE_input[ i, 1 ] &&
              TABLE_result[ j, 3 ] == TABLE_input[ i, 3 ] )
          {
            TABLE_result[ j, 8 ] <- TABLE_result[ j, 8 ] + TABLE_input[ i, 8 ]
            BOOL_found = TRUE
            break
          }
        }
      }
      if( !BOOL_found )
      {
        TABLE_result <- rbind( TABLE_result, TABLE_input[ i, ] )
      }
    }
    TABLE_result <- TABLE_result[ , c( -7, -6, -5, -4, -2 ) ]
    TABLE_result <- TABLE_result[ order( TABLE_result[ , 1 ], TABLE_result[ , 2 ] ), ]
    PALETTE_residuewise <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    if( !is.null( VEC_acceptorRange ) )
    {
      TABLE_result <- TABLE_result[ TABLE_result[ , 2 ] >= VEC_acceptorRange[ 1 ], , drop = FALSE ]
      TABLE_result <- TABLE_result[ TABLE_result[ , 2 ] <= VEC_acceptorRange[ 2 ], , drop = FALSE ]
    }
    if( !is.null( VEC_donorRange ) )
    {
      TABLE_result <- TABLE_result[ TABLE_result[ , 1 ] >= VEC_donorRange[ 1 ], , drop = FALSE ]
      TABLE_result <- TABLE_result[ TABLE_result[ , 1 ] <= VEC_donorRange[ 2 ], , drop = FALSE ]
    }
    VEC_normalized <- lapply( as.list( TABLE_result[ , 3 ] ), function( x ) x / 100 )
    layout( matrix( 1:2, ncol = 2 ), width = c( 2, 1 ), height = c( 1, 1 ) )
    PALETTE_colors <- colorRampPalette( brewer.pal( 11, 'Spectral' ) )
    PALETTE_colors_rev <- colorRampPalette( rev( brewer.pal( 11, 'Spectral' ) ) )
    plot( TABLE_result[ , 1:2 ],
          col = PALETTE_colors_rev( 10 )[ as.numeric( cut( as.numeric( VEC_normalized ), breaks = 10 ) )  ],
          pch = 19,
          cex = 1.25 )
    if( BOOL_print_legend )
    {
      legend_image <- as.raster( matrix( PALETTE_colors( 10 ), ncol = 1 ) )
      plot( c( 0, 2 ), c( 0, 1 ), type = 'n', axes = F, xlab = '', ylab = '', main = 'Color legend [%]' )
      text( x = 1.5, y = seq( 0, 1, l = 5 ),
            labels = seq( 0, 100, l = 5 ) )
      rasterImage( legend_image, 0, 0, 1, 1 )
    }
  }
  else
  {
    stop( paste( "Error: plot method ", STRING_what_to_plot, " is unknown. Process aborted!" ) )
  }
}