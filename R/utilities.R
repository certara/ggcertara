#' Default labels
default.labels <- list(
  dv    = "Observed Value",
  pred  = "Population Prediction",
  ipred = "Individual Prediction",
  cwres = "Conditional Weighted Residual",
  iwres = "Individual Weighted Residual",
  time  = "Time",
  tad   = "Time After Dose"
)

.onLoad <- function(libname, pkgname) {
  op <- list(
    gof.label.dv    = default.labels$dv,
    gof.label.pred  = default.labels$pred,
    gof.label.ipred = default.labels$ipred,
    gof.label.cwres = default.labels$cwres,
    gof.label.iwres = default.labels$iwres,
    gof.label.time  = default.labels$time,
    gof.label.tad   = default.labels$tad,
    gof.units.dv    = NA,
    gof.units.time  = NA,
    gof.units.tad   = NA,
    gof.scale.dv    = "linear",
    gof.scale.time  = "linear",
    gof.scale.tad   = "linear"
  )
  toset <- !(names(op) %in% names(options()))
  if (any(toset)) options(op[toset])
  invisible()
}

#' Labels for GOF plots
#'
#' @param ... Named character arguments will override the defaults.
#'
#' @details This function can be used to get and set the axis labels for GOF
#' plots.  The default labels are taken from \code{\link{options}} that start
#' with the prefix \code{gof.label.}, for example \code{gof.label.dv},
#' \code{gof.label.ipred} and so on.  This function extracts those options into
#' a named list (without the prefix), and allows any number of them to be
#' overridden.
#'
#' @return A named list.
#'
#' @examples
#' gof_labels()
#'
#' # Override a label
#' gof_labels(tad="Time After Start of Infusion")
#'
#' \dontrun{
#' # Change the default
#' options(gof.label.tad="Time After Start of Infusion")
#' gof_labels()
#' }
#' @export
gof_labels <- function(...) {
  l <- options()[grepl("^gof\\.label\\.", names(options()))]
  names(l) <- sub("^gof\\.label\\.", "", names(l))
  args <- list(...)
  l[names(args)] <- args
  l
}

# This is an internal function that constructs labels for plots.  If it
# recognizes certain variable names, it will add units from global options.
get_label <- function(x, labels=gof_labels()) {
  x <- rlang::enquo(x)
  xlb <- tryCatch(rlang::eval_tidy(x, data=labels), error=function(e) NULL)
  if (!is.null(xlb)) {
    xnm <- tryCatch(rlang::as_label(x), error=function(e) NULL)
    if (!is.null(xnm)) {
      op <- switch(xnm,
                   dv    = "gof.units.dv",
                   pred  = "gof.units.dv",
                   ipred = "gof.units.dv",
                   time  = "gof.units.time",
                   tad   = "gof.units.tad",
                   NULL)
      if (!is.null(op)) {
        unit <- getOption(op)
        if (!is.null(unit) && length(unit) > 0) {
          if (length(unit) > 1) {
            warning(sprintf("%s should be length 1, using first element", op))
            unit <- unit[[1]]
          }
          if (is.expression(unit) || (!is.na(unit) && unit != "")) {
            if (is.expression(xlb) || is.expression(unit)) {
              if (!is.expression(unit)) {
                unit <- paste0("\"", unit, "\"")
              }
              if (!is.expression(xlb)) {
                xlb <- paste0("\"", xlb, "\"")
              }
              xlb <- parse(text=paste0(xlb, "~(", unit, ")"))
            } else {
              xlb <- sprintf("%s (%s)", xlb, unit)
            }
          }
        }
      }
    }
  } else {
    xlb <- tryCatch(rlang::as_label(x), error=function(e) NULL)
  }
  xlb
}


#' Customized geoms
#' @name certara_geom
NULL

#' @rdname certara_geom
#' @inheritParams ggplot2::geom_point
#' @export
geom_point_c <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointC,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname certara_geom
#' @format NULL
#' @usage NULL
#' @export
GeomPointC <- ggproto("GeomPointC", GeomPoint,
                      default_aes = aes(
                        shape = 19, colour = "#9DA1BD", size = 1.5, fill = NA,
                        alpha = 0.4, stroke = 0
                      )
)

#' @rdname certara_geom
#' @inheritParams ggplot2::geom_line
#' @export
geom_fitline_c <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, orientation = NA,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFitlineC,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname certara_geom
#' @format NULL
#' @usage NULL
#' @export
GeomFitlineC <- ggproto("GeomFitlineC", GeomLine,

                        required_aes = c("x", "y"),

                        default_aes = aes(
                          fitcolour = "#ee3124", fitsize = 1,
                          fitalpha = NA, fitlinetype = 1
                        ),

                        handle_na = function(self, data, params) {
                          data2 <- data[!(names(data) %in% c("colour", "color", "size", "alpha", "linetype"))]
                          nm <- names(data2)
                          nm <- ifelse(nm=="fitcolour",   "colour",   nm)
                          nm <- ifelse(nm=="fitcolor",    "color",    nm)
                          nm <- ifelse(nm=="fitsize",     "size",     nm)
                          nm <- ifelse(nm=="fitalpha",    "alpha",    nm)
                          nm <- ifelse(nm=="fitlinetype", "linetype", nm)
                          names(data2) <- nm
                          data3 <- self$super()$handle_na(data=data2, params=params)
                          nm <- names(data3)
                          nm <- ifelse(nm=="colour",   "fitcolour",   nm)
                          nm <- ifelse(nm=="color",    "fitcolor",    nm)
                          nm <- ifelse(nm=="size",     "fitsize",     nm)
                          nm <- ifelse(nm=="alpha",    "fitalpha",    nm)
                          nm <- ifelse(nm=="linetype", "fitlinetype", nm)
                          names(data3) <- nm
                          data3
                        },

                        draw_panel = function(self, data, ...) {
                          data2 <- data[!(names(data) %in% c("colour", "color", "size", "alpha", "linetype"))]
                          nm <- names(data2)
                          nm <- ifelse(nm=="fitcolour",   "colour",   nm)
                          nm <- ifelse(nm=="fitcolor",    "color",    nm)
                          nm <- ifelse(nm=="fitsize",     "size",     nm)
                          nm <- ifelse(nm=="fitalpha",    "alpha",    nm)
                          nm <- ifelse(nm=="fitlinetype", "linetype", nm)
                          names(data2) <- nm
                          self$super()$draw_panel(data=data2, ...)
                        }
)

#' @rdname certara_geom
#' @param geom Which underlying \code{geom} to use to do the actual rendering.
#' @inheritParams stats::scatter.smooth
#' @export
geom_loess_c <- function(mapping = NULL, data = NULL,
                         geom = "fitline_c", position = "identity",
                         ...,
                         span = 2/3,
                         degree = 1,
                         family = "symmetric",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatLoessC,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      span = span,
      degree = degree,
      family = family,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname certara_geom
#' @export
stat_loess_c <- geom_loess_c

#' @rdname certara_geom
#' @format NULL
#' @usage NULL
#' @export
StatLoessC <- ggproto("StatLoessC", Stat,
                      required_aes = c("x", "y"),

                      compute_group = function(data, scales, span = 2/3, degree = 1, family = "symmetric", na.rm = FALSE) {
                        tryCatch({
                          fit <- loess.smooth(data$x, data$y, span=span, degree=degree, family=family)
                          data.frame(x=fit$x, y=fit$y)
                        }, warning=function(e) {
                          warning("Warnings occurred in LOESS computation. Usually OK to ignore.", call.=FALSE)
                          suppressWarnings({
                            fit <- loess.smooth(data$x, data$y, span=span, degree=degree, family=family)
                            data.frame(x=fit$x, y=fit$y)
                          })
                        }, error=function(e) {
                          warning("Unable to compute LOESS smooth.")
                          data.frame(x=numeric(0), y=numeric(0))
                        })
                      }
)

#' A coordinate system that is symmetric about \eqn{x=0}.
#'
#' This is a Cartesian coordinate system that is centered vertically at \eqn{x=0}.
#' @inheritParams ggplot2::coord_cartesian
#' @export
coord_symm_x <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordSymmX,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' @rdname coord_symm_x
#' @format NULL
#' @usage NULL
#' @export
CoordSymmX <- ggproto("CoordSymmX", CoordCartesian,
                      setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                        scale_x$range$range <- c(-1, 1)*max(abs(scale_x$range$range))
                        parent <- ggproto_parent(CoordCartesian, self)
                        parent$setup_panel_params(scale_x, scale_y, params)
                      }
)

#' A coordinate system that is symmetric about \eqn{y=0}.
#'
#' This is a Cartesian coordinate system that is centered vertically at \eqn{y=0}.
#' @inheritParams ggplot2::coord_cartesian
#' @export
coord_symm_y <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordSymmY,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' @rdname coord_symm_y
#' @format NULL
#' @usage NULL
#' @export
CoordSymmY <- ggproto("CoordSymmY", CoordCartesian,
                      setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                        scale_y$range$range <- c(-1, 1)*max(abs(scale_y$range$range))
                        parent <- ggproto_parent(CoordCartesian, self)
                        parent$setup_panel_params(scale_x, scale_y, params)
                      }
)

#' A coordinate system that is symmetric about \eqn{y=x}.
#'
#' This is a Cartesian coordinate system that has the same limits in both \eqn{x} and \eqn{y}.
#' @inheritParams ggplot2::coord_cartesian
#' @export
coord_symm_xy <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordSymmXY,
          limits = list(x = xlim, y = ylim),
          expand = expand,
          clip = clip
  )
}

#' @rdname coord_symm_xy
#' @format NULL
#' @usage NULL
#' @export
CoordSymmXY <- ggproto("CoordSymmXY", CoordCartesian,
                       setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                         lim_xy <- c(
                           min(scale_x$range$range[1], scale_y$range$range[1]),
                           max(scale_x$range$range[2], scale_y$range$range[2]))
                         scale_x$range$range <- lim_xy
                         scale_y$range$range <- lim_xy
                         parent <- ggproto_parent(CoordCartesian, self)
                         parent$setup_panel_params(scale_x, scale_y, params)
                       }
)

#' Use log scales
#'
#' @param g A \code{ggplot2} object.
#' @param limits Limits to use for the scale. See \code{\link[ggplot2]{scale_x_continuous}}.
#' @return A \code{ggplot2} object with log scales in \eqn{x}, \eqn{y} or both.
#' @name logscales
NULL

#' @rdname logscales
#' @export
log_x <- function(g, limits=NULL) {
  .x <- NULL
  g + scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = limits
  ) +
    annotation_logticks(sides="b")
}

#' @rdname logscales
#' @export
log_y <- function(g, limits=NULL) {
  .x <- NULL
  g + scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = limits
  ) +
    annotation_logticks(sides="l")
}

#' @rdname logscales
#' @export
log_xy <- function(g, limits=NULL) {
  .x <- NULL
  g + scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = limits
  ) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      limits = limits
    ) +
    annotation_logticks(sides="bl", size=0.3)
}
