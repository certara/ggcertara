#' @import ggplot2
#' @import patchwork
#' @importFrom rlang .data
NULL

#' Default labels
#' @export
default.labels=list(
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
    gof.labels = default.labels,
    gof.units.dv = NA,
    gof.units.time = NA,
    gof.units.tad = NA
  )
  toset <- !(names(op) %in% names(options()))
  if (any(toset)) options(op[toset])
  invisible()
}

# This is an internal function that constructs labels for plots.  If it
# recognizes certain variable names, it will add units from global options.
get_label <- function(x, labels=getOption("gof.labels")) {
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
          if (!is.na(unit) && unit != "") {
            xlb <- sprintf("%s (%s)", xlb, unit)
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
#' @inheritParams ggplot2::geom_point
#' @name certara_geom
NULL

#' @rdname certara_geom
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
    shape = 19, colour = "#2b398b", size = 1.5, fill = NA,
    alpha = 0.3, stroke = 0.5
  )
)

#' @rdname certara_geom
#' @inheritParams stats::scatter.smooth
#' @export
geom_loess_c <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
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
    stat = stat,
    geom = GeomLoessC,
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
#' @format NULL
#' @usage NULL
#' @export
GeomLoessC <- ggproto("GeomLoessC", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(
    fitcolour = "#ee3124", fitsize = 1,
    fitalpha = NA, fitlinetype = 1
    ),

  draw_panel = function(data, panel_params, coord, span = 2/3, degree = 1, family = "symmetric", na.rm = FALSE) {

    coords <- coord$transform(data, panel_params)

    fit <- loess.smooth(coords$x, coords$y, span=span, degree=degree, family=family)
    col <- alpha(coords$fitcolour, coords$fitalpha)
    if (length(unique(col)) == 1) {
      col <- col[1]
    } else {
      col <- "black"
    }

    grid::linesGrob(
      fit$x, fit$y,
      default.units = "native",
      gp = grid::gpar(
        col = col,
        lwd = coords$fitsize[1] * .pt,
        lty = coords$fitlinetype[1]
      )
    )
  },

  draw_key = draw_key_smooth
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
#' @return g A \code{ggplot2} object with log scales in \eqn{x}, \eqn{y} or both.
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

#' Read gof data
#'
#' @param rundir A directory in which to search.
#' @param file A filename to read the data from.
#' @return A \code{data.frame}.
#' @export
gof_read_data <- function(rundir=getwd(), file=NULL) {

  read.wrapper <- function(file, nrows) {
    if (file == "sdtab") {
      x <- utils::read.table(file, header=TRUE, check.names=FALSE, skip=1, na.strings=c(".", ""))
    } else {
      x <- utils::read.csv(file, header=TRUE, check.names=FALSE, na.strings=c(".", ""))
    }
    stats::setNames(x, tolower(names(x)))
  }

  if (is.null(file)) {
    if (rundir == getwd()) {
      message("Searching for data in current working directory")
    } else {
      message(sprintf("Searching for data in %s", rundir))
    }

    # search in rundir for csv files that contain dv, pred, ipred, cwres, etc.
    ll <- list.files(path=rundir, pattern="*.csv", full.names=TRUE)
    ll <- c(ll, list.files(path=rundir, pattern="sdtab", full.names=TRUE))

    data <- NULL
    for (l in ll) {
      data <- read.wrapper(l, nrows=1)  # Read just the first line to check the column names
      if (all(c("dv", "pred", "ipred") %in% names(data))) {
        message(sprintf("...Using %s", l))
        data <- read.wrapper(l) # Read the whole file
        break
      }
    }
    if (is.null(data)) {
      stop("No data found.")
    }
  } else {
      data <- read.wrapper(file)
  }

  if (!is.null(data$mdv)) {
    data <- data[data$mdv==0,]
  }

  return(data)
}

#' Base plot for gof plots
#'
#' This is the default base plot for goodness-of-fit diagnostic plots. It
#' produces a scatterplot with a LOESS smoother. The points are blue, and the
#' LOESS smoother is red. The plot has a square aspect ratio. It uses the
#' Certara theme. Other elements (e.g. reference lines) can be added to the
#' plot afterwards. The default aesthetics (x and y) and labels are also added
#' subsequently. The base plot can be overridden using the \code{baseplot}
#' parameter of the specific GOF function.
#'
#' @param data A \code{data.frame}.
#' @param highlight (Optional) A 2-level \code{factor}, evaluated within
#' \code{data}, indicating a subset of points that are to be highlighted on the
#' plot.  Typically, these may be outliers, or a specific subset of interest.
#' Those points belonging to the subset will be drawn with a different color
#' and symbol, and a legend will appear as well.
#' @param ... Additional arguments (ignored).
#' @return  A \code{ggplot} object.
#' @export
gof_baseplot <- function(data, highlight, ...) {
  g <- ggplot(data) +
    geom_point_c() +
    geom_loess_c() +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)

  if (!missing(highlight)) {
    gl <- guide_legend("Legend", title.position="top")
    g <- g + aes(color={{ highlight }}, shape={{ highlight }}, size={{ highlight }}, alpha={{ highlight }}) +
      guides(colour=gl, shape=gl, size=gl, alpha=gl) +
      scale_colour_manual(values=c("#2b398b", "#ee3124")) +
      scale_shape_manual(values=c(19, 13)) +
      scale_size_manual(values=c(1.5, 3)) +
      scale_alpha_manual(values=c(0.3, 1))
  }
  g
}

#' A generic function for residual plots
#'
#' @param data A \code{data.frame}.
#' @param x,y Numeric vectors, evaulated within \code{data}.
#' @param labels A named \code{list} of labels.
#' @param baseplot A function that returns a \code{ggplot} object to use as the
#' base.
#' @param log_x If \code{TRUE} then log-scale will be used for the x-axis.
#' @param ... Additional arguments, passed to \code{baseplot}.
#' @return  A \code{ggplot} object.
#' @export
gof_residual <- function(data, x, y, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  xlb <- get_label({{ x }}, labels)
  ylb <- get_label({{ y }}, labels)
  g <- baseplot(data, ...) +
    aes(x={{ x }}, y={{ y }}) +
    labs(x=xlb, y=ylb) +
    coord_symm_y() +
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8) +
    theme(panel.grid.major.y=element_line(colour="grey80", size=0.3))
  if (isTRUE(log_x)) {
    g <- log_x(g)
  }
  g
}

#' @rdname gof_residual
#' @export
gof_absresidual <- function(data, x, y, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  xlb <- get_label({{ x }}, labels)
  ylb <- get_label({{ y }}, labels)
  g <- baseplot(data, ...) +
    aes(x={{ x }}, y=abs({{ y }})) +
    labs(x=xlb, y=sprintf("|%s|", ylb)) +
    expand_limits(y=0) +
    #geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8) +
    theme(panel.grid.major.y=element_line(colour="grey80", size=0.3))
  if (isTRUE(log_x)) {
    g <- log_x(g)
  }
  g
}

#' A generic function for identity plots
#'
#' Identity plots have symmetric x- and y-axes, a reference line at \eqn{y=x}, and
#' a fixed aspect ration. Typical examples are DV vs. PRED and DV vs. IPRED.
#'
#' @param data A \code{data.frame}.
#' @param x,y Numeric vectors, evaulated within \code{data}.
#' @param labels A named \code{list} of labels.
#' @param baseplot A function that returns a \code{ggplot} object to use as the
#' base.
#' @param log_xy If \code{TRUE} then log-scale will be used for both x- and y-axes.
#' @return  A \code{ggplot} object.
#' @param ... Additional arguments, passed to \code{baseplot}.
#' @export
gof_identity <- function(data, x, y, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_xy=F, ...) {
  xlb <- get_label({{ x }}, labels)
  ylb <- get_label({{ y }}, labels)
  g <- baseplot(data, ...) +
    aes(x={{ x }}, y={{ y }}) +
    labs(x=xlb, y=ylb) +
    coord_symm_xy() +
    geom_abline(slope=1, color="black", linetype="dashed", size=0.8)
  if (isTRUE(log_xy)) {
    g <- log_xy(g)
  }
  g
}

#' Basic gof plots
#'
#' @param data A \code{data.frame}.
#' @param labels A named \code{list} of labels.
#' @param baseplot A function that returns a \code{ggplot} object to use as the
#' base for all scatterplots (histograms and QQ-plots are unaffected).
#' @param log_x If \code{TRUE} then log-scale will be used for the x-axis.
#' @param log_xy If \code{TRUE} then log-scale will be used for both x- and y-axes.
#' @return  A \code{ggplot} object.
#' @param ... Additional arguments, passed to \code{baseplot}.
#' @name gofplots
NULL

#' Plot CWRES vs. PRED
#' @rdname gofplots
#' @export
gof_cwres_vs_pred <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  gof_residual(data, x=.data$pred, y=.data$cwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot CWRES vs. TIME
#' @rdname gofplots
#' @export
gof_cwres_vs_time <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  gof_residual(data, x=.data$time, y=.data$cwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot CWRES vs. TAD
#' @rdname gofplots
#' @export
gof_cwres_vs_tad <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  gof_residual(data, x=.data$tad, y=.data$cwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot |IWRES| vs. IPRED
#' @rdname gofplots
#' @export
gof_absiwres_vs_ipred <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  gof_absresidual(data, x=.data$ipred, y=.data$iwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot |IWRES| vs. TIME
#' @rdname gofplots
#' @export
gof_absiwres_vs_time <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  gof_absresidual(data, x=.data$time, y=.data$iwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot |IWRES| vs. TAD
#' @rdname gofplots
#' @export
gof_absiwres_vs_tad <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_x=F, ...) {
  gof_absresidual(data, x=.data$tad, y=.data$iwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot DV vs. IPRED
#' @rdname gofplots
#' @export
gof_dv_vs_ipred <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_xy=F, ...) {
  gof_identity(data, x=.data$ipred, y=.data$dv, labels=labels, baseplot=baseplot, log_xy=log_xy, ...)
}

#' Plot DV vs. PRED
#' @rdname gofplots
#' @export
gof_dv_vs_pred <- function(data, labels=getOption("gof.labels"), baseplot=gof_baseplot, log_xy=F, ...) {
  gof_identity(data, x=.data$pred, y=.data$dv, labels=labels, baseplot=baseplot, log_xy=log_xy, ...)
}

#' A generic function for histograms
#' @param data A \code{data.frame}.
#' @param x A numeric vector, evaulated within \code{data}.
#' @param labels A named \code{list} of labels.
#' @param symm_x A number of \code{NULL}. If a number, the x-axis will be symmetric, centered around this number.
#' @param log_x If \code{TRUE} then log-scale will be used for the x-axis.
#' @return  A \code{ggplot} object.
#' @export
gof_histogram <- function(data, x, labels=getOption("gof.labels"), symm_x=if (isTRUE(log_x)) 1 else 0, log_x=F) {
  xlb <- get_label({{ x }}, labels)
  density <- NULL
  g <- ggplot(data, aes(x={{ x }})) +
    labs(x=xlb, y="Density") +
    geom_histogram(aes(y=stat(density)), color="gray80", fill="gray80", bins=20) +
    stat_density(geom="line", col="#ee3124", size=1) +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)
  if (isTRUE(log_x)) {
    if (!is.null(symm_x))  {
      g <- log_x(g, limits=function(x) symm_x*10^(c(-1, 1)*max(abs(log10(x/symm_x)), na.rm=T))) +
        geom_vline(xintercept=symm_x, col="gray50")
    } else {
      g <- log_x(g)
    }
    g <- g + stat_function(fun=stats::dlnorm, color="black", linetype="dashed", size=0.8)
  } else {
    if (!is.null(symm_x))  {
      g <- g +
        scale_x_continuous(limits=function(x) symm_x + c(-1, 1)*max(abs(x - symm_x), na.rm=T)) +
        geom_vline(xintercept=symm_x, col="gray50")
    }
    g <- g + stat_function(fun=stats::dnorm, color="black", linetype="dashed", size=0.8)
  }
  g
}

#' Histogram of CWRES
#' @rdname gofplots
#' @export
gof_cwres_histogram <- function(data, labels=getOption("gof.labels")) {
  gof_histogram(data, x=.data$cwres, labels=labels, log_x=F)
}

#' A generic function for QQ-plots
#' @param data A \code{data.frame}.
#' @param x A numeric vector, evaulated within \code{data}.
#' @param labels A named \code{list} of labels.
#' @return  A \code{ggplot} object.
#' @export
gof_qqplot <- function(data, x, labels=getOption("gof.labels")) {
  xlb <- get_label({{ x }}, labels)
  g <- ggplot(data, aes(sample={{ x }})) +
    labs(x="Theoritical Quantile", y="Sample Quantile") +
    coord_symm_xy() +
    stat_qq(color="#2b398b", alpha=0.3) +
    stat_qq_line(col="#ee3124", size=1) +
    geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)
  g
}

#' QQ-plot of CWRES
#' @rdname gofplots
#' @export
gof_cwres_qqplot <- function(data, labels=getOption("gof.labels")) {
  gof_qqplot(data, x=.data$cwres, labels=labels)
}

#' List of gof plots
#' @rdname gof
#' @export
gof_list <- function(data=NULL,
                labels=getOption("gof.labels"),
                baseplot=gof_baseplot,
                rundir=getwd(),
                ...)
{
  if (is.null(data)) {
    data <- gof_read_data(rundir)
  }

  p <- list()

  # Histogram of CWRES
  p[["cwres_histogram"]] <- gof_cwres_histogram(data, labels)

  # QQ-plot of CWRES
  p[["cwres_qqplot"]] <- gof_cwres_qqplot(data, labels)

  # DV vs. IPRED linear scale
  p[["dv_vs_ipred"]] <- gof_dv_vs_ipred(data, labels, baseplot, ...)

  # DV vs. PRED linear scale
  p[["dv_vs_pred"]] <- gof_dv_vs_pred(data, labels, baseplot, ...)

  # DV vs. IPRED log scale
  p[["dv_vs_ipred_log"]] <- gof_dv_vs_ipred(data, labels, baseplot, log_xy=TRUE, ...)

  # DV vs. PRED log scale
  p[["dv_vs_pred_log"]] <- gof_dv_vs_pred(data, labels, baseplot, log_xy=TRUE, ...)

  # CWRES vs. PRED
  p[["cwres_vs_pred"]] <- gof_cwres_vs_pred(data, labels, baseplot, ...)

  # CWRES vs. TIME
  p[["cwres_vs_time"]] <- gof_cwres_vs_time(data, labels, baseplot, ...)

  # CWRES vs. TAD
  p[["cwres_vs_tad"]] <- gof_cwres_vs_tad(data, labels, baseplot, ...)

  # |IWRES| vs. IPRED
  p[["absiwres_vs_ipred"]] <- gof_absiwres_vs_ipred(data, labels, baseplot, ...)

  # |IWRES| vs. TIME
  p[["absiwres_vs_time"]] <- gof_absiwres_vs_time(data, labels, baseplot, ...)

  # |IWRES| vs. TAD
  p[["absiwres_vs_tad"]] <- gof_absiwres_vs_tad(data, labels, baseplot, ...)

  structure(p, class="gof_list")
}

#' @export
print.gof_list <- function(x, ...) {
  cat("List of gof plots:\n\n")
  for (i in 1:length(x)) {
    cat(sprintf("  %d. %s\n", i, names(x)[i]))
  }
  invisible(x)
}

#' Layout gof plots
#' @rdname gof
#' @export
gof_layout <- function(p, layout=c(ceiling(length(p)/2), 2))
{
  p <- purrr::reduce(p, `+`)
  if (!is.null(layout)) {
    p <- p + patchwork::plot_layout(nrow=layout[1], ncol=layout[2], guides="collect")
  }
  p
}

#' Goodness-of-fit diagnostic plots
#'
#' @inheritParams gofplots
#' @inheritParams gof_read_data
#' @param panels A \code{numeric} vector specifying the panels desired.
#' @param p A \code{list} of `ggplot` objects to be laid out.
#' @param layout A \code{numeric} vector of length 2 giving the number of rows
#' and column in which the panels are to be layed out (for multiple panels).
#' @param ... Additional arguments, passed to \code{baseplot}.
#' @export
gof <- function(data=NULL,
                panels=c(3, 4, 7, 8, 9, 10),
                layout=c(ceiling(length(panels)/2), 2),
                labels=getOption("gof.labels"),
                baseplot=gof_baseplot,
                rundir=getwd(),
                ...)
{

  p <- gof_list(data=data, labels=labels, baseplot=baseplot, rundir=rundir, ...)

  if (length(panels) == 1) {
    p <- p[[panels]]
  } else {
    p <- p[panels]
    p <- gof_layout(p, layout)
  }
  p
}

# vim: ts=2 sw=2 et
