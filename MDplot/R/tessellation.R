# function, to plot reference points, the tessellated surface and the curvature scores of a membrane
plotMembrane <- function( inputResult,
                          surfaces = c( 1, 2 ),
                          selectSnapshot = NA,
                          appearance = NA,
                          interactive = FALSE,
                          plotPoints = TRUE,
                          plotEdges = TRUE,
                          plotCurvature = TRUE,
                          curvatureType = "unsigned",
                          colPoints = list( by = "surface", '1' = "red", '2' = "blue" ),
                          colEdges = list( by = "surface", '1' = "red", '2' = "blue" ),
                          paletteCurvature = NA,
                          paletteRange = NA,
                          PNGsnapshot = NA,
                          PDFsnapshot = NA )
{
  # check the input
  if( !is.list( inputResult ) || !all( c( "list", "MANTICORE::result" ) %in% class( inputResult ) ) )
    stop( "The input does not seem to be a proper MANTICORE membrane tessellation result." )
  if( length( inputResult ) < 1 )
    stop( "The input result must be of at least a length of one." )
  if( all( is.na( selectSnapshot ) ) )
    selectSnapshot <- 1
  if( is.na( paletteCurvature ) )
    paletteCurvature <- colorRampPalette( c( "white", "blue", "orange", "red", "black" ) )
  if( all( is.na( paletteRange ) ) )
    paletteRange <- range( as.vector( unlist( lapply( X = inputResult, FUN = function( surface ) lapply( X = surface, FUN = function( x ) x$localCurvature ) ) ) ) )
  if( length( paletteRange ) != 2 )
    stop( "The range of the pallette must be a vector of length two exactly." )
  if( plotPoints && is.null( inputResult[[ 1 ]]$referencePointsDF ) )
    stop( "In order to plot the points, the element \"referencePointsDF\" must be part of the result input (see parameter of the same name in the analysis function of the MANTICORE package)." )
  if( plotEdges && is.null( inputResult[[ 1 ]][[ paste0( "surface_", surfaces[ 1 ] ) ]]$edgeIndices ) )
    stop( "In order to plot the edges, the element \"edgeIndices\" must be part of the input (see parameter \"verbose\" in the analysis function of the MANTICORE package)." )
  if( plotCurvature && is.null( inputResult[[ 1 ]][[ paste0( "surface_", surfaces[ 1 ] ) ]]$localCurvature ) )
    stop( "In order to plot the curvature as the colour of the tiles, the element \"localCurvature\" must be part of the result input (see parameter of the same name in the analysis function of the MANTICORE package)." )
  if( !is.list( colPoints ) || !is.list( colEdges ) || !( colPoints$by %in% c( "surface", "molecule" ) ) || !( colEdges$by %in% c( "surface" ) ) )
    stop( "Parameters \"colPoints\" and \"colEdges\" must be lists with certain elements, please consult the manual." )
  if( ( plotPoints && colPoints$by == "surface" && length( colPoints ) - 1 < length( surfaces ) ) ||
      ( plotEdges && colEdges$by == "surface" && length( colEdges ) - 1  < length( surfaces ) ) )
    stop( "If the colouring lists are set to hold information by surface, colours for every surface have to be specified." )
  if( is.na( curvatureType ) || is.null( curvatureType ) || !( curvatureType %in% c( "unsigned", "signed" ) ) )
    stop( "Parameter \"curvatureType\" must be either \"unsigned\" or \"signed\"." )
  if( any( is.na( appearance ) ) )
    appearance <- list( userMatrix = matrix( c(  0.6380738,  0.6212445, -0.4548815, 0,
                                                -0.2128165,  0.7100536,  0.6712176, 0,
                                                 0.7399803, -0.3314803,  0.5852774, 0,
                                                 0.0000000,  0.0000000,  0.0000000, 1 ),
                                             nrow = 4,
                                             byrow = TRUE ),
                        zoom = 1,
                        windowRect = c( 0, 0, 900, 900 ) )

  # require the library
  require( rgl )

  # set the scene
  rgl::open3d( zoom = appearance$zoom, windowRect = appearance$windowRect, userMatrix = appearance$userMatrix )

  # plot the points
  if( plotPoints )
    for( iSurface in 1:length( surfaces ) )
    {
      referencePointsDF <- inputResult[[ selectSnapshot ]]$referencePointsDF
      listReferencePointsDF <- list()
      colVector <- c()
      if( colPoints$by == "surface" ) {
        colVector <- colPoints[[ as.character( surfaces[ iSurface ] ) ]]
        listReferencePointsDF[[ 1 ]] <- referencePointsDF
      } else {
        allMolecules <- unique( referencePointsDF$moleculeType )
        for( iMolecule in 1:length( allMolecules ) )
        {
          listReferencePointsDF[[ iMolecule ]] <- referencePointsDF[ referencePointsDF$moleculeType == allMolecules[ iMolecule ], , drop = FALSE ]
          colVector <- c( colVector, colPoints[[ allMolecules[ iMolecule ] ]] )
        }
      }
      for( iSurfacePart in 1:length( listReferencePointsDF ) )
      {
        referencePointsDF <- listReferencePointsDF[[ iSurfacePart ]]
        if( nrow( referencePointsDF ) == 0 )
          next
        plot3d( x = referencePointsDF[ referencePointsDF[ , "clusterNumber" ] == surfaces[ iSurface ], "refPointX" ],
                y = referencePointsDF[ referencePointsDF[ , "clusterNumber" ] == surfaces[ iSurface ], "refPointY" ],
                z = referencePointsDF[ referencePointsDF[ , "clusterNumber" ] == surfaces[ iSurface ], "refPointZ" ],
                type = 's',
                size = 0.525,
                add = TRUE,
                xlim = range( referencePointsDF[ , "refPointX" ] ),
                ylim = range( referencePointsDF[ , "refPointY" ] ),
                zlim = range( referencePointsDF[ , "refPointZ" ] ),
                col = colVector[ iSurfacePart ] )
      }
    }

  # plot the edges
  if( plotEdges )
    for( iSurface in 1:length( surfaces ) )
    {
      referencePointsDF <- inputResult[[ selectSnapshot ]]$referencePointsDF
      colVector <- NULL
      if( colEdges$by == "surface" )
        colVector <- colEdges[[ as.character( surfaces[ iSurface ] ) ]]
      edgeIndices <- inputResult[[ selectSnapshot ]][[ paste0( "surface_", surfaces[ iSurface ] ) ]]$edgeIndices
      matEdges <- getEdgeCoordinates( referencePointsDF = referencePointsDF[ referencePointsDF[ , "clusterNumber" ] == surfaces[ iSurface ], ],
                                      edgeIndices = edgeIndices )
      xVec <- c()
      yVec <- c()
      zVec <- c()
      for( iEdge in 1:nrow( matEdges ) )
      {
        xVec <- c( xVec, matEdges[ iEdge, c( "refPointX_1", "refPointX_2" ), drop = TRUE ] )
        yVec <- c( yVec, matEdges[ iEdge, c( "refPointY_1", "refPointY_2" ), drop = TRUE ] )
        zVec <- c( zVec, matEdges[ iEdge, c( "refPointZ_1", "refPointZ_2" ), drop = TRUE ] )
      }
      segments3d( x = xVec,
                  y = yVec,
                  z = zVec,
                  lwd = 1.25,
                  col = colVector )
    }

  # plot the triangles
  curColIndex <- 1
  if( plotCurvature )
    for( iSurface in 1:length( surfaces ) )
    {
      matTriangleIndices <- inputResult[[ selectSnapshot ]][[ paste0( "surface_", surfaces[ iSurface ] ) ]]$triangleIndices
      vecLocalCurvature <- NULL
      if( curvatureType == "unsigned" )
        vecLocalCurvature <- inputResult[[ selectSnapshot ]][[ paste0( "surface_", surfaces[ iSurface ] ) ]]$localCurvature
      else
        vecLocalCurvature <- inputResult[[ selectSnapshot ]][[ paste0( "surface_", surfaces[ iSurface ] ) ]]$signedLocalCurvature
      matTriangleCoordinates <- getCoordinatesFromPointIDs( referencePointsDF = inputResult[[ selectSnapshot ]]$referencePointsDF,
                                                            indices = matTriangleIndices[ , c( "ind1", "ind2", "ind3" ), drop = FALSE ] )
      colRange <- paletteCurvature( 10 )[ as.numeric( cut( c( paletteRange, vecLocalCurvature ), breaks = 10 ) ) ]
      colRange <- colRange[ c( -2, -1 ) ]
      vecColours <- c()
      xVec <- c()
      yVec <- c()
      zVec <- c()
      for( iTriangle in 1:nrow( matTriangleCoordinates ) )
      {
        xVec <- c( xVec, matTriangleCoordinates[ iTriangle, c( "refPointX_1", "refPointX_2", "refPointX_3" ), drop = TRUE ] )
        yVec <- c( yVec, matTriangleCoordinates[ iTriangle, c( "refPointY_1", "refPointY_2", "refPointY_3" ), drop = TRUE ] )
        zVec <- c( zVec, matTriangleCoordinates[ iTriangle, c( "refPointZ_1", "refPointZ_2", "refPointZ_3" ), drop = TRUE ] )
        vecColours <- c( vecColours, rep( colRange[ iTriangle ], times = 3 ) )
      }
      triangles3d( x = xVec,
                   y = yVec,
                   z = zVec,
                   col = vecColours )
      curColIndex <- curColIndex + nrow( matTriangleCoordinates )
    }

  # do the rotation stuff, if required
  if( interactive )
  {
    # hold, until the user specifies to go on
    cat( "Please orient the scene as seems fit and then press [Enter] to proceed" )
    line <- readline()

    # use the user matrix to orient the scene
    appearance <- list( userMatrix = par3d()$userMatrix,
                        zoom = par3d()$zoom,
                        windowRect = par3d()$windowRect )
  }

  # render images, if requested
  if( !is.na( PNGsnapshot ) )
  {
    if( !dir.exists( dirname( PNGsnapshot ) ) )
      stop( "Directory for file specified in parameter \"PNGsnapshot\" does not exist." )
    rgl::rgl.snapshot( filename = PNGsnapshot )
  }
  if( !is.na( PDFsnapshot ) )
  {
    if( !dir.exists( dirname( PDFsnapshot ) ) )
      stop( "Directory for file specified in parameter \"PDFsnapshot\" does not exist." )
    rgl::rgl.postscript( filename = PDFsnapshot, fmt = "pdf" )
  }

  # return the translation and zooming stuff
  return( appearance )
}

# function to generate a movie from PNG snapshots
shootMovieMembrane <- function( inputResult, outputMovie, interactive = TRUE, snapshots = NA, ... )
{
  # check input
  if( is.null( inputResult ) || is.null( outputMovie ) )
    stop( "Parameters \"inputResult\" and \"outputMovie\" are mandatory." )
  if( is.na( snapshots ) )
    snapshots <- 1:length( inputResult )

  # generate a new temporary directory for the snapshots and set it to be the current directory
  tempDir <- paste0( tempdir(), '/', "shootMembraneMovie" )
  if( dir.exists( paths = tempDir ) )
    unlink( x = tempDir, recursive = TRUE )
  dir.create( path = tempDir )
  workingDir <- getwd()
  setwd( tempDir )

  # generate the snapshots
  appearance <- NULL
  for( iSnapshot in 1:length( snapshots ) )
  {
    # set the variables
    curFile <- paste0( tempDir, "/snapshot_", sprintf( "%04d", iSnapshot ), ".png" )

    # generate the snapshot
    clear3d()
    if( iSnapshot == 1 )
      appearance <- MDplot::plotMembrane( inputResult = inputResult, selectSnapshot = iSnapshot, PDFsnapshot = NA,
                                          PNGsnapshot = curFile, appearance = NA, interactive = interactive, ... )
    else
      appearance <- MDplot::plotMembrane( inputResult = inputResult, selectSnapshot = iSnapshot, PDFsnapshot = NA,
                                          PNGsnapshot = curFile, appearance = appearance, interactive = FALSE, ... )
    rgl.close()
  }

  # generate the movie
  system2( command = "ffmpeg",
           args = c( "-framerate 12",
                     paste0( "-i ", tempDir, "/snapshot_%04d.png" ),
                     "-c:v libx264",
                     "-r 24",
                     "-pix_fmt yuv420p",
                     "-vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\"",
                     outputMovie ),
           input = 'y',
           wait = TRUE )

  # switch back the directory
  if( dir.exists( workingDir ) )
    setwd( workingDir )
}