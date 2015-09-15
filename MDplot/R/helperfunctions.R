library( MASS )
library( RColorBrewer )
library( gplots )
library( calibrate )
library( gtools )
library( grDevices )
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