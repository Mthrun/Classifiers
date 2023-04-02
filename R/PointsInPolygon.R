PointsInPolygon=function(Points,Polygon,PlotIt=FALSE,...){
#V=PointsInPolygon(Points, Polygon, BMUorProjected = F,PlotIt = FALSE, main = "Projected Points")
# Defines a Cls based on points in a given polygon.
#
# INPUT
# Points          [1:n,1:2] xy cartesian coordinates of a projection
# Polygon   numerical matrix of 2 columns defining a closed polygon
# PlotIt          TRUE: Plots marked points
# ...             Further Plotting Arguments,xlab etc used in \code{\link[DataVisualizations]{Classplot}}
  
# OUTPUT
# Cls                Numerical classification vector Cls with 1=outside polygon and 2= inside polygon
#
# DETAILS
#  We assume that polygon is closed, i.e., that the last point connects to the fist point
#  Michael Thrun
# 
#     XY=cbind(runif(100,min = -1,max = 1),rnorm(100))
#     #closed polygon
#     polymat <- cbind(x = c(0,1,1,0), y = c(0,0,1,1))
#     Cls=PointsInPolygon(XY,polymat,PlotIt = TRUE)

  
  #if (!requireNamespace("secr", quietly = TRUE)) {
  #  message("Subordinate 'secr' is missing. No computations are performed.\n Please install the package which is defined in \"Suggests\".")
  #  return(Cls="Subordinate clustering package (secr) is missing.\nPlease install the package which is defined in 'Suggests'.")
  #}

  if (!is.matrix(Points))
    stop('ProjectedPoints has to be a matrix')
  
  #secr failes to laod with 
  #Error: package or namespace load failed for ‘secr’:  package slot missing from signature for generic ‘coerce’
  pointsInPolygon=function (xy, poly, logical = TRUE) 
  {
  #author: Murray Efford, GPL License
    xy <- matrix(unlist(xy), ncol = 2)
    if (inherits(poly, "SpatialPolygons")) {
      xy <- SpatialPoints(xy)
      proj4string(poly) <- CRS()
      poly <- as(poly, "SpatialPolygons")
      OK <- sp::over(xy, poly)
      !is.na(OK)
    }
    else if (inherits(poly, "mask")) {
      if (ms(poly)) 
        stop("multi-session masks not supported")
      sp <- spacing(poly)
      minx <- min(poly$x, na.rm = TRUE)
      miny <- min(poly$y, na.rm = TRUE)
      mask <- sweep(poly, MARGIN = 2, FUN = "+", STATS = c(-minx, 
                                                           -miny))
      mask <- round(mask/sp) + 1
      xy <- sweep(xy, MARGIN = 2, FUN = "+", STATS = c(-minx, 
                                                       -miny))
      xy <- round(xy/sp) + 1
      xy[xy <= 0] <- NA
      xy[, 1][xy[, 1] > max(mask$x, na.rm = TRUE)] <- NA
      xy[, 2][xy[, 2] > max(mask$y, na.rm = TRUE)] <- NA
      maskmatrix <- matrix(0, ncol = max(mask$y, na.rm = TRUE), 
                           nrow = max(mask$x, na.rm = TRUE))
      maskmatrix[as.matrix(mask)] <- 1:nrow(mask)
      inside <- maskmatrix[as.matrix(xy)]
      inside[is.na(inside)] <- 0
      if (logical) 
        inside <- inside > 0
      inside
    }
    else {
      checkone <- function(xy1) {
        insidecpp(xy1, 0, np - 1, as.matrix(poly))
      }
      poly <- matrix(unlist(poly), ncol = 2)
      np <- nrow(poly)
      apply(xy, 1, checkone)
    }
  }
  
  dots=list(...)
  
  #in case very, very high dim (d>20k,and high amount of points, set eppochs manually lower)
  if(is.null(dots[["main"]]))
    main="Projected Points with Polygon"
  else
    main=NULL
  
  if(is.null(dots[["BMUorProjected"]]))
    BMUorProjected=FALSE
  else
    BMUorProjected=dots$BMUorProjected
  
  #checkmate::assert_named(dots)
  #return(dots)
  #new.args=dots
  #new.args <- lapply(dots, '^', 2) 
  
  #names(new.args)=names(dots)
  
  # new.args[["name"]] <- name
  
  
  c = ncol(Points)
  if (c > 3 | c < 2)
    stop(paste0('Wrong number of Columns of ProjectedPoints: ', c))
  if (c == 3) {
    #With Key
    if (BMUorProjected) {
      Y = Points[, 2]
      X = Points[, 3]
    } else{
      X = Points[, 2]
      Y = Points[, 3]
    }
  } else{
    #Without Key
    if (BMUorProjected) {
      Y = Points[, 1]
      X = Points[, 2]
    } else{
      X = Points[, 1]
      Y = Points[, 2]
    }
  }
  Polygon=rbind(Polygon,head(Polygon,1))
  
  #Cls=secr::pointsInPolygon(xy = cbind(X,Y), poly = Polygon)+1
  Cls=pointsInPolygon(xy = cbind(X,Y), poly = Polygon)+1
  if(isTRUE(PlotIt)){
    if (!requireNamespace("DataVisualizations", quietly = TRUE)) {
      message("Subordinate 'DataVisualizations' is missing. No computations are performed.\n Please install the package which is defined in \"Suggests\".Plotting is disabled")
    }else{
      if(!is.null(main))
        DataVisualizations::Classplot(X = X,Y = Y,Cls = Cls,Plotter = "native",main = main,...)
      else
        DataVisualizations::Classplot(X = X,Y = Y,Cls = Cls,Plotter = "native",...)
      lines(Polygon)
    }
  }
  return(Cls)
}