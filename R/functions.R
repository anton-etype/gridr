
#' Create grid
#'
#' @param gdbPath The file path (dsn) to the file containing the polygons.
#' @param lyrName The layer name for the polygon layer.
#' @param xdistGrid Distance in meters between the points in east/west direction.
#' @param ydistGrid Distanve in meters between the points in north/south direction.
#' @param displacementXY A numeric vector (\code{c(x, y)}), for the displacement of the points.
#' The origin is at the south west corner of the bounding box of each polygon.
#'
#' @return A simple feature data frame with with a grid layser of points inside the polygons.
#' @export
#'
#' @examples
#' grid <- createGrid(gridr_example("best.gpkg"), "best", 20, 20, c(-5, -2))
createGrid <- function(gdbPath, lyrName, xdistGrid,
                       ydistGrid, displacementXY = c(0,0)){

  fc_inp <- sf::st_read(dsn=gdbPath,layer=lyrName, promote_to_multi = FALSE)

  crs <- sf::st_crs(fc_inp) # To save for result export

  fc <- sf::st_transform(fc_inp, 3006) # Best distance for Sweden

  coords <- list()
  id <- c()

  for (i in 1:nrow(fc)) {

    p <- fc[i,]

    xmin <- sf::st_bbox(p)$xmin + displacementXY[1]
    xmax <- sf::st_bbox(p)$xmax
    ymin <- sf::st_bbox(p)$ymin + displacementXY[2]
    ymax <- sf::st_bbox(p)$ymax

    xcoords <- seq(from = xmin, to = xmax, by = xdistGrid)
    ycoords <- seq(from = ymin, to = ymax, by = ydistGrid)

    pCoords <- matrix(
      c(
        rep(xcoords, times = length(ycoords)),
        rep(ycoords, each = length(xcoords))
      ),
      ncol = 2)

    #Creates a list of points a vector of ids
    for(r in 1:nrow(pCoords)){
      coords[[length(coords) +1 ]] <- sf::st_point(pCoords[r,])
      id[[length(id) +1 ]] <- i
    }

  }

  # Build a dataframe with point geom and id
  ids <- matrix(id, ncol = 1)
  colnames(ids) <- "id"

  ids <- as.data.frame(ids)

  geom <- sf::st_sfc(coords, crs = 3006)

  points <- sf::st_sf(cbind(ids, geom))

  points$id <- unlist(points$id)

  # Tell sf that the attributes are constant throughout the gemometries
  # It removes a warning in the intersection
  sf::st_agr(fc) = "constant"
  sf::st_agr(points) = "constant"

  # Cut the points to intersect the polygons. Also adds polygon data to the points
  res <- sf::st_intersection(points, fc)

  res <- sf::st_transform(res, crs)

  return(res)

}


#' Title
#'
#' @param grid A simple feature point dataframe, probably created by (\code{createGrid()})
#'
#' @import dplyr
#'
#' @return The point data set, together with a column with the distance from one
#' random chosen point in each polygon and a column with a rank number for the distance.
#' @export
#'
#' @examples
#' grid <- createGrid(gridr_example("best.gpkg"), "best", 20, 20, c(-5, -2))
#' sg <- getSubsample(grid)
getSubsample <- function(grid){

  samplepoints <- grid %>%
    dplyr::group_by(id) %>%
    dplyr::slice_sample(n=1) %>%
    dplyr::mutate(geom_s = geometry) %>%
    dplyr::select(id, geom_s)

  samplepoints <- sf::st_drop_geometry(samplepoints)

  out <- dplyr::left_join(grid, samplepoints, by = "id") %>%
    dplyr::mutate(dist = sf::st_distance(geometry, geom_s, by_element = TRUE))

  out <- out %>% dplyr::group_by(id) %>%
    dplyr::mutate(dist_rank = rank(dist, ties.method = "random")) %>%
    dplyr::arrange(dist_rank, .by_group = TRUE)

  return(out[,colnames(out) != "geom_s"])

}
