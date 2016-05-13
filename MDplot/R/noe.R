# load noe data
load_noe <- function( file )
{
  VEC_fileRead <- scan( file, what = "", sep = "\n",
                        quiet = TRUE )
  MAT_violationDistribution <- NULL
  INT_pos <- which( VEC_fileRead == "VIOLATION DISTRIBUTION" ) + 2
  for( i in INT_pos:length( VEC_fileRead ) )
  {
    if( VEC_fileRead[ i ] != "END" )
    {
      VEC_split <- unlist( strsplit( VEC_fileRead[ i ], "[[:space:]]+" ) )
      VEC_split <- as.numeric( VEC_split[ VEC_split != "" ] )
      if( is.null( MAT_violationDistribution ) )
        MAT_violationDistribution <- matrix( VEC_split,
                                             ncol = 2 )
      else
        MAT_violationDistribution <- rbind( MAT_violationDistribution,
                                            VEC_split )
    }
    else
      break
  }
  colnames( MAT_violationDistribution ) <- c( "violation", "count" )
  rownames( MAT_violationDistribution ) <- c( rep( "", times = nrow( MAT_violationDistribution ) ) )
  return( MAT_violationDistribution )
}

# plot noe data
noe <- function( noeData,
                 totalNOEs,
                 printPercentages = TRUE )
{
  noeData <- noeData[ noeData[ , 1 ] > 0, ]
  VEC_yLimits <- c( 0, sum( noeData[ , 2 ] ) * 1.05 )
  
  # remove "negative NOEs"
  if( printPercentages )
  {
    noeData[ , 2 ] <- noeData[ , 2 ] / totalNOEs * 100
    VEC_yLimits <- c( 0, 50 )
  }
  
  # plot histogram now
  barplot( t( noeData ),
           ylim = VEC_yLimits,
           xaxs = "i",
           xlab = "", xaxt = "n",
           space = 0 )
  axis( 1,
        at = seq( from = 0, to = nrow( noeData ) - 1, by = 1 ),
        labels = c( noeData[ , 1 ] ),
        las = 3 )
  
  # plot in same device
  par( new = TRUE )
  
  # calculate NOE curve
  MAT_theCurve <- matrix( c( noeData[ , 1 ],
                             rep( NA, times = nrow( noeData ) ) ),
                          nrow = nrow( noeData ), byrow = FALSE )
  for( i in 1:nrow( MAT_theCurve ) )
    MAT_theCurve[ i, 2 ] <- sum( noeData[ 1:i, 2 ] )
  MAT_theCurve[ , 1 ] <- seq( from = 0, to = nrow( noeData ) - 1, by = 1 ) + 0.5

  # add addition curve
  plot( MAT_theCurve,
        type = "l", xaxs = "i", xaxt = "n",
        yaxt = "n", xlab = "", ylab = "",
        xlim = c( 0, nrow( MAT_theCurve ) + 1 ), ylim = VEC_yLimits,
        lty = 1 )
  return( noeData )
}