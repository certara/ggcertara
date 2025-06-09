#' @import ggplot2
#' @import patchwork
#' @importFrom rlang .data
#' @importFrom dplyr filter %>%
NULL


#' Read gof data
#'
#' @param file A filename to read the data from. If not specified, a simple
#'   heuristic search in \code{rundir} is performed.
#' @param rundir A directory in which to search.
#' @return A \code{data.frame}.
#' @export
gof_read_data <- function(file=NULL, rundir=getwd()) {

  read.wrapper <- function(file, nrows) {
    tryCatch({
      l1 <- readLines(file, n=1)
      if (grepl("^TABLE NO\\.", l1)) {
        x <- utils::read.table(file, header=TRUE, check.names=FALSE, skip=1, na.strings=c(".", ""))
      } else {
        x <- utils::read.csv(file, header=TRUE, check.names=FALSE, na.strings=c(".", ""))
      }
      stats::setNames(x, tolower(names(x)))
    }, error=function(e) NULL)
  }

  if (is.null(file)) {
    if (rundir == getwd()) {
      message("Searching for data in current working directory")
    } else {
      message(sprintf("Searching for data in %s", rundir))
    }

    # search in rundir for csv files that contain dv, pred, ipred, cwres, etc.
    sdtabfiles <- list.files(path=rundir, pattern="sdtab*", full.names=TRUE)
    csvfiles <- list.files(path=rundir, pattern="*.csv", full.names=TRUE)
    ll <- c(sdtabfiles, csvfiles)

    data <- NULL
    for (l in ll) {
      data <- read.wrapper(l, nrows=1)  # Read just the first line to check the column names
      if (!is.null(data) & all(c("dv", "pred") %in% names(data))) {
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

  if (!is.null(data) & !is.null(data$mdv)) {
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
#' @param loess.args A \code{list} of arguments passed to \code{\link{geom_loess_c}}.
#' @return  A \code{ggplot} object.
#' @export
gof_baseplot <- function(data, highlight, ..., loess.args=list()) {
  g <- ggplot(data) +
    geom_point_c() +
    do.call(geom_loess_c, c(list(mapping=aes(group=NA)), as.list(loess.args))) +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)

  if (!missing(highlight)) {
    gl1 <- guide_legend("", title.position="top")
    gl2 <- guide_legend("", title.position="top", override.aes=list(size=3))
    gl3 <- guide_legend("", title.position="top", override.aes=list(alpha=0.7))
    g <- g + aes(color={{ highlight }}, shape={{ highlight }}, size={{ highlight }}, alpha={{ highlight }}) +
      guides(colour=gl1, shape=gl1, size=gl2, alpha=gl3) +
      scale_colour_manual(values=c(GeomPointC$default_aes$colour, "#ee3124")) +
      scale_shape_manual(values=c(19, 16)) +
      scale_size_manual(values=c(1.5, 2)) +
      scale_alpha_manual(values=c(0.3, 0.5))
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
gof_residual <- function(data, x, y, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
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
gof_absresidual <- function(data, x, y, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
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
gof_identity <- function(data, x, y, labels=gof_labels(), baseplot=gof_baseplot, log_xy=F, ...) {
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
gof_cwres_vs_pred <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
  gof_residual(data, x=.data$pred, y=.data$cwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot CWRES vs. TIME
#' @rdname gofplots
#' @export
gof_cwres_vs_time <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
  gof_residual(data, x=.data$time, y=.data$cwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot CWRES vs. TAD
#' @rdname gofplots
#' @export
gof_cwres_vs_tad <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
  gof_residual(data, x=.data$tad, y=.data$cwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot |IWRES| vs. IPRED
#' @rdname gofplots
#' @export
gof_absiwres_vs_ipred <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
  gof_absresidual(data, x=.data$ipred, y=.data$iwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot |IWRES| vs. TIME
#' @rdname gofplots
#' @export
gof_absiwres_vs_time <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
  gof_absresidual(data, x=.data$time, y=.data$iwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot |IWRES| vs. TAD
#' @rdname gofplots
#' @export
gof_absiwres_vs_tad <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_x=F, ...) {
  gof_absresidual(data, x=.data$tad, y=.data$iwres, labels=labels, baseplot=baseplot, log_x=log_x, ...)
}

#' Plot DV vs. IPRED
#' @rdname gofplots
#' @export
gof_dv_vs_ipred <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_xy=F, ...) {
  gof_identity(data, x=.data$ipred, y=.data$dv, labels=labels, baseplot=baseplot, log_xy=log_xy, ...)
}

#' Plot DV vs. PRED
#' @rdname gofplots
#' @export
gof_dv_vs_pred <- function(data, labels=gof_labels(), baseplot=gof_baseplot, log_xy=F, ...) {
  gof_identity(data, x=.data$pred, y=.data$dv, labels=labels, baseplot=baseplot, log_xy=log_xy, ...)
}

#' A generic function for histograms
#' @param data A \code{data.frame}.
#' @param x A numeric vector, evaulated within \code{data}.
#' @param bins Number of bins to use for the histogram.
#' @param labels A named \code{list} of labels.
#' @param symm_x A number of \code{NULL}. If a number, the x-axis will be symmetric, centered around this number.
#' @param log_x If \code{TRUE} then log-scale will be used for the x-axis.
#' @return  A \code{ggplot} object.
#' @export
gof_histogram <- function(data, x, bins=20, labels=gof_labels(), symm_x=if (isTRUE(log_x)) 1 else 0, log_x=F) {
  xlb <- get_label({{ x }}, labels)
  density <- NULL
  g <- ggplot(data, aes(x={{ x }})) +
    labs(x=xlb, y="Density") +
    geom_histogram(aes(y=after_stat(density)), color=NA, fill="#9DA1BD", alpha=0.3, bins=bins) +
    stat_density(geom="fitline_c") +
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
gof_cwres_histogram <- function(data, labels=gof_labels()) {
  gof_histogram(data, x=.data$cwres, labels=labels, log_x=F)
}

#' A generic function for QQ-plots
#' @param data A \code{data.frame}.
#' @param x A numeric vector, evaulated within \code{data}.
#' @param labels A named \code{list} of labels.
#' @return  A \code{ggplot} object.
#' @export
gof_qqplot <- function(data, x, labels=gof_labels()) {
  xlb <- get_label({{ x }}, labels)
  g <- ggplot(data, aes(sample={{ x }})) +
    labs(x="Theoretical Quantile", y="Sample Quantile") +
    coord_symm_xy() +
    stat_qq(geom="point_c") +
    stat_qq_line(geom="fitline_c") +
    geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)
  g
}

#' QQ-plot of CWRES
#' @rdname gofplots
#' @export
gof_cwres_qqplot <- function(data, labels=gof_labels()) {
  gof_qqplot(data, x=.data$cwres, labels=labels)
}

#' The default GOF panels
#'
#' @param ... Ignored.
#'
#' @details
#' The default output of \code{\link{gof}} is consists of 6 panels arranged in
#' a 3-by-2 grid. These are:
#'
#' \enumerate{
#'   \item dv_vs_ipred
#'   \item dv_vs_pred
#'   \item cwres_vs_pred
#'   \item cwres_vs_time
#'   \item cwres_vs_tad
#'   \item absiwres_vs_ipred
#' }
#'
#' Most panels come a both a linear and log variant. Normally, the linear
#' variant will be chosen, but in some cases it may make more sense for the log
#' variants to be the default for certain panels. This can be achieved using
#' \code{\link{options}} \code{gof.scale.*} (see Examples).
#'
#' @return A vector of panel numbers.
#'
#' @examples
#' gof_default_panels()
#'
#' \dontrun{
#' # Change the default scale to log for dv/pred/ipred
#' options(gof.scale.dv="log")
#' gof_default_panels()
#'
#' # Change the default scale to log for time and tad
#' options(gof.scale.time="log", gof.scale.tad="log")
#' gof_default_panels()
#' }
#' @export
gof_default_panels <- function(...) {
  m1 <- (-1)^(getOption("gof.scale.dv")=="log")
  m2 <- (-1)^(getOption("gof.scale.time")=="log")
  m3 <- (-1)^(getOption("gof.scale.tad")=="log")
  c(
    3*m1, # dv_vs_ipred"
    6*m2, # cwres_vs_time"
    7*m3, # cwres_vs_tad"
    4*m1, # dv_vs_pred"
    5*m1, # cwres_vs_pred"
    8*m1  # absiwres_vs_ipred"
  )
}

#' List of gof plots
#' @param empty Return an empty list.
#' @param all Return a list of all panels.
#' @rdname gof
#' @export
gof_list <- function(data=NULL,
                panels=gof_default_panels(),
                empty=FALSE,
                all=FALSE,
                labels=gof_labels(),
                baseplot=gof_baseplot,
                rundir=getwd(),
                ...)
{

  if (isTRUE(empty)) {
    return(structure(list(), class="gof_list"))
  }

  p <- list()

  if (is.null(data)) {
    data <- gof_read_data(rundir=rundir)
  }

  # Histogram of CWRES
  p[["cwres_histogram"]] <- gof_cwres_histogram(data, labels)

  # QQ-plot of CWRES
  p[["cwres_qqplot"]] <- gof_cwres_qqplot(data, labels)

  # DV vs. IPRED
  p[["dv_vs_ipred"]] <- gof_dv_vs_ipred(data, labels, baseplot, ...)

  # DV vs. PRED
  p[["dv_vs_pred"]] <- gof_dv_vs_pred(data, labels, baseplot, ...)

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

  # DV vs. IPRED log scale
  p[["dv_vs_ipred_log"]] <- gof_dv_vs_ipred(data, labels, baseplot, log_xy=TRUE, ...)

  # DV vs. PRED log scale
  p[["dv_vs_pred_log"]] <- gof_dv_vs_pred(data, labels, baseplot, log_xy=TRUE, ...)

  # CWRES vs. PRED log scale
  p[["cwres_vs_pred_log"]] <- gof_cwres_vs_pred(data, labels, baseplot, log_x=TRUE, ...)

  # CWRES vs. TIME log scale
  p[["cwres_vs_time_log"]] <- gof_cwres_vs_time(data, labels, baseplot, log_x=TRUE, ...)

  # CWRES vs. TAD log scale
  p[["cwres_vs_tad_log"]] <- gof_cwres_vs_tad(data, labels, baseplot, log_x=TRUE, ...)

  # |IWRES| vs. IPRED log scale
  p[["absiwres_vs_ipred_log"]] <- gof_absiwres_vs_ipred(data, labels, baseplot, log_x=TRUE, ...)

  # |IWRES| vs. TIME log scale
  p[["absiwres_vs_time_log"]] <- gof_absiwres_vs_time(data, labels, baseplot, log_x=TRUE, ...)

  # |IWRES| vs. TAD log scale
  p[["absiwres_vs_tad_log"]] <- gof_absiwres_vs_tad(data, labels, baseplot, log_x=TRUE, ...)

  if (!isTRUE(all)) {
    if (is.numeric(panels)) {
      panels <- paste0(names(p)[abs(panels)], ifelse(panels <= -3, "_log", ""))
    }
    p <- p[panels]
  }

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
#' @inheritParams patchwork::plot_layout
#' @seealso \code{\link[patchwork]{plot_layout}}
#' @export
gof_layout <- function(p, nrow=NULL, ncol=NULL, byrow=TRUE, ...)
{
  if (is.null(nrow) & is.null(ncol)) {
    lp <- length(p)
    ncol <- ifelse(lp==1, 1, ifelse(lp %in% c(2, 4), 2, 3))
  }
  defaults <- list(nrow=nrow, ncol=ncol, byrow=byrow, guides="collect")
  args <- list(...)
  args <- args[names(args) %in% names(formals(patchwork::plot_layout))]
  toset <- !(names(defaults) %in% names(args))
  args <- c(args, defaults[toset])
  purrr::reduce(p, `+`) + do.call(patchwork::plot_layout, args)
}

#' Goodness-of-fit diagnostic plots
#'
#' @inheritParams gofplots
#' @inheritParams gof_read_data
#' @param panels A \code{numeric} vector specifying the panels desired.
#' @param p A \code{list} of `ggplot` objects to be laid out.
#' @param layout A \code{list} of arguments to pass to \code{\link{gof_layout}} (for multiple panels).
#' @param ... Additional arguments passed to other methods (e.g. \code{baseplot}).
#' @export
gof <- function(data=NULL,
                panels=gof_default_panels(),
                layout=NULL,
                labels=gof_labels(),
                baseplot=gof_baseplot,
                rundir=getwd(),
                ...)
{

  p <- gof_list(data=data, panels=panels, labels=labels, baseplot=baseplot, rundir=rundir, ...)

  if (length(p) == 1) {
    p[[1]]
  } else {
    do.call(gof_layout, c(list(p), as.list(layout)))
  }
}


#' Generate Individual Plots
#'
#' Observations (DV), individual predictions (IPRED), and population predictions (PRED) plotted against the independent variable for each individual.
#'
#' @param data A \code{data.frame} containing the dataset.
#' @param labels A named \code{list} of labels for plot annotations. Default values are provided by \code{gof_labels()}.
#' @param x The name of the independent variable column. Default value is "time".
#' @param y The name of the dependent variable column. Default value is "dv".
#' @param ipred The name of the individual prediction variable column. Default value is "ipred".
#' @param pred The name of the population prediction variable column. Default value is "pred".
#' @param color_color A named vector specifying colors for different elements (dv, ipred, pred, outliers). Should be a vector of 4 colors.
#' @param shape_shape A named vector specifying shapes for different elements. When not specified, defaults are used.
#' @param fill_fill A named vector specifying fill colors for elements when uncertainty is plotted. Specifically for 'ipred' when \code{uncertainty} is TRUE.
#' @param alpha_fill Transparency level for the filled area representing the confidence interval, when \code{uncertainty} is TRUE.
#' @param line_linetype A named vector specifying line types for different elements (dv, outliers, ipred, pred).
#' @param line_size Line width for the line layer. Default is 1.
#' @param shape_size Size for the shape layer. Default is 1.
#' @param strati1 The variable used to stratify the plots. Default is "id".
#' @param strati2 An optional variable for an additional layer of stratification. Default is NULL.
#' @param ip_ncol Number of columns per plot. Default is 4. Adjust if encountering issues with incomplete first rows on the last page due to \code{ggforce::facet_wrap_paginate}.
#' @param ip_nrow Number of rows per plot. Default is 4. Adjust similarly to \code{ip_ncol} for pagination issues.
#' @param cwres.outliers If \code{TRUE}, only profiles of individuals with outlier observations are displayed. Default is FALSE.
#' @param limit.outliers Threshold for CWRES to identify outliers. Default is 5.
#' @param uncertainty If \code{TRUE}, plots a confidence interval around individual predictions. Default is FALSE.
#' @param uncert.ci Confidence interval level when \code{uncertainty} is TRUE. Default is 0.95.
#' @param ... Additional arguments passed to lower-level functions.
#'
#' @examples
#' \dontrun{
#' indiv_plot(data = dat,
#'            x = "time",
#'            y = "dv",
#'            ipred = "ipred",
#'            pred = "pred",
#'            strati1 = "id",
#'            ip_ncol = 3,
#'            ip_nrow = 4,
#'            uncertainty = FALSE)
#' }
#'
#' # Example with individuals with high cwres values
#' \dontrun{
#' indiv_plot(data = dat,
#'            x = "time",
#'            y = "dv",
#'            ipred = "ipred",
#'            pred = "pred",
#'            strati1 = "id",
#'            cwres.outliers = TRUE,
#'            limit.outliers = 5,
#'            ip_ncol = 3,
#'            ip_nrow = 4,
#'            uncertainty = FALSE)
#' }
#'
#' # Example to apply layers with the ampersand sign & (example to change legend.position)
#' \dontrun{
#' indiv_plot(data = dat,
#'            x = "time",
#'            y = "dv",
#'            ipred = "ipred",
#'            pred = "pred",
#'            strati1 = "id") &
#' theme(legend.position = "bottom")
#' }
#' @export
indiv_plot <- function(data,
                       x,
                       y,
                       ipred,
                       pred,
                       labels = gof_labels(),
                       ip_ncol = 4,
                       ip_nrow = 4,
                       scale_facet = 'fixed',
                       labelfacet = "label_value",
                       strati1 = id,
                       strati2 = NULL,
                       color_color = c(
                         'dv' = ggcertara::certara_pal()[1],
                         'outliers' = ggcertara::certara_pal()[2],
                         'ipred' = ggcertara::certara_pal()[4],
                         'pred' = ggcertara::certara_pal()[7]
                       ),
                       fill_fill = c('ipred' = ggcertara::certara_pal()[4]),
                       alpha_fill = 0.5,
                       line_linetype = c(
                         'dv' = 'blank',
                         'outliers' = 'blank',
                         'ipred' = 'solid',
                         'pred' = 'dashed'
                       ),
                       line_size = 1,
                       shape_shape = c(
                         'dv' = 16,
                         'outliers' = 10,
                         'ipred' = NA,
                         'pred' = NA
                       ),
                       shape_size = 1,
                       cwres.outliers = FALSE,
                       limit.outliers = 5,
                       uncertainty = FALSE,
                       uncert.ci = 0.95,
                       ...) {
  strati1 <- enquo(strati1)
  strati2 <- enquo(strati2)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)


  filt_y <- deparse(substitute(y))
  filt_ipred <- deparse(substitute(ipred))
  filt_pred <- deparse(substitute(pred))

  xlb <- get_label(!!x, labels)
  ylb <- get_label(!!y, labels)

  if (cwres.outliers == TRUE & uncertainty == TRUE)
    stop('Both cwres.outliers and uncertainty arguments cannot be TRUE simultaneously')

  if (uncert.ci >= 1)
    stop('uncert.ci cannot be greater or equal to 1')

  # Rest of the code for different cases
  # ...

  if (cwres.outliers == FALSE & uncertainty == FALSE) {
    data2    <- data %>%
      tidyr::pivot_longer(cols = c({
        {
          y
        }
      }, {
        {
          pred
        }
      }, {
        {
          ipred
        }
      }),
      names_to = "variable",
      values_to = "value")

    ip_npage <- ceiling(
      data %>%
        dplyr::group_by({
          {
            strati1
          }
        }) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        nrow() / (ip_ncol * ip_nrow)
    )

    remainder_id <- data %>%
      dplyr::group_by({
        {
          strati1
        }
      }) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      nrow() %% (ip_ncol * ip_nrow)

    if (remainder_id < ip_ncol &
        remainder_id > 0)
      print(
        'Due to a bug in ggforce::facet_wrap_paginate, you might get an error and be asked to change ip_ncol and/or ip_nrow'
      )


    g1 <- ggplot2::ggplot(data2) +
      ggplot2::aes(x = {{x}}, y = value) +

      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable ==  filt_y),
        aes(
          x =  {
            {
              x
            }
          },
          y = value,
          shape = 'dv',
          colour = 'dv'
        ),
        size = shape_size,
        show.legend = TRUE
      ) +
      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable ==  filt_ipred),
        aes(
          x =  {
            {
              x
            }
          },
          y = value,
          shape = 'ipred',
          colour = 'ipred'
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable ==  filt_pred),
        aes(
          x =  {
            {
              x
            }
          },
          y = value,
          shape = 'pred',
          colour = 'pred'
        ),
        show.legend = TRUE
      ) +

      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable ==  filt_y),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'dv',
          colour = 'dv'
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable ==  filt_ipred),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'ipred',
          colour = 'ipred'
        ),
        size = line_size,
        show.legend = TRUE
      ) +
      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable ==  filt_pred),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'pred',
          colour = 'pred'
        ),
        size = line_size,
        show.legend = TRUE
      ) +
      ggplot2::scale_linetype_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV'
        ),
        limits = c("dv", 'ipred', 'pred'),
        values = line_linetype
      ) +
      ggplot2::scale_shape_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV'
        ),
        limits = c("dv", 'ipred', 'pred'),
        values = shape_shape
      ) +
      ggplot2::scale_color_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV'
        ),
        limits = c("dv", 'ipred', 'pred'),
        values = color_color
      ) +

      ggcertara::theme_certara(base_size = 11) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::theme(
        legend.direction = "vertical",
        legend.position = "right",
        legend.title = element_blank()
      ) +
      ggplot2::labs(
        x = xlb,
        y = ylb,
        title = 'Individual plot title',
        subtitle = 'Individual plot subtitle'
      )



    g = fwp(
      g1,
      ncol = ip_ncol,
      nrow = ip_nrow,
      npg = ip_npage,
      strat1 = strati1,
      strat2 = strati2,
      scale_facet2 = scale_facet,
      labelfacet2 = labelfacet
    )

    print(g)
    return(g)


  }



  if (cwres.outliers == TRUE & uncertainty == FALSE) {
    if (is.null(data$cwres)) {
      stop("No CWRES variable found.")
    }

    datcwres <-
      data %>% dplyr::filter(abs(cwres) > limit.outliers) %>%
      dplyr::select({
        {
          strati1
        }
      }) %>%
      dplyr::pull() %>% unique()


    data2 <- data %>%
      tidyr::pivot_longer(cols = c({
        {
          y
        }
      }, {
        {
          pred
        }
      }, {
        {
          ipred
        }
      }),
      names_to = "variable",
      values_to = "value")


    ip_npage <- ceiling(
      data %>% dplyr::group_by({
        {
          strati1
        }
      }) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>% nrow() / (ip_ncol *  ip_nrow)
    )

    remainder_id <- data %>%  dplyr::group_by({
      {
        strati1
      }
    }) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>% nrow() %% (ip_ncol *  ip_nrow)

    if (remainder_id < ip_ncol &
        remainder_id > 0)
      print(
        'Due to a bug in ggforce::facet_wrap_paginate, you might get an error and be asked to change ip_ncol and/or ip_nrow'
      )



    g1 <- ggplot2::ggplot(data2) +
      ggplot2::aes(x = {
        {
          x
        }
      }, y = value) +

      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable == filt_y &
                                         abs(cwres) < limit.outliers),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          shape = 'dv',
          colour = 'dv'
        ),
        size = shape_size,
        show.legend = TRUE
      ) +
      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable == filt_y &
                                         abs(cwres) >= limit.outliers),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          shape = 'outliers',
          colour = 'outliers'
        ),
        size = shape_size,
        show.legend = TRUE
      ) +

      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable ==  filt_ipred),
        aes(
          x =  {
            {
              x
            }
          },
          y = value,
          shape = 'ipred',
          colour = 'ipred'
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_point(
        data = data2 %>% dplyr::filter(variable ==  filt_pred),
        aes(
          x =  {
            {
              x
            }
          },
          y = value,
          shape = 'pred',
          colour = 'pred'
        ),
        show.legend = TRUE
      ) +


      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable ==  filt_y),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'dv',
          colour = 'dv'
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable ==  filt_y &
                                         abs(cwres) >= limit.outliers),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'outliers',
          colour = 'outliers'
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable == filt_ipred),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'ipred',
          colour = 'ipred'
        ),
        size = line_size,
        show.legend = FALSE
      ) +
      ggplot2::geom_line(
        data = data2 %>% dplyr::filter(variable == filt_pred),
        aes(
          x = {
            {
              x
            }
          },
          y = value,
          linetype = 'pred',
          colour = 'pred'
        ),
        size = line_size,
        show.legend = FALSE
      ) +

      ggplot2::scale_linetype_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV',
          'outliers' = paste0('|CWRES| \U2265 ', limit.outliers)
        ),
        limits = c("outliers", "dv", 'ipred', 'pred'),
        values = line_linetype
      ) +
      ggplot2::scale_shape_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV',
          'outliers' = paste0('|CWRES| \U2265 ', limit.outliers)
        ),
        limits = c("outliers", "dv", 'ipred', 'pred'),
        values = shape_shape
      ) +
      ggplot2::scale_color_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV',
          'outliers' = paste0('|CWRES| \U2265 ', limit.outliers)
        ),
        limits = c("outliers", "dv", 'ipred', 'pred'),
        values = color_color
      ) +
      ggplot2::labs(
        x = xlb,
        y = ylb,
        title = 'Individual plot title',
        subtitle = paste0('Only Ids with |CWRES|\U2265 ', limit.outliers, ' are displayed')
      ) +


      ggcertara::theme_certara(base_size = 11) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::theme(
        legend.direction = "vertical",
        legend.position = "right",
        legend.title = element_blank()
      )




    g <-
      fwp(
        g1,
        ncol = ip_ncol,
        nrow = ip_nrow,
        npg = ip_npage,
        strat1 = strati1,
        strat2 = strati2,
        scale_facet2 = scale_facet,
        labelfacet2 = labelfacet
      )

    print(g) #calls print.fwp because class(x) is "fwp"
    return(g) #implicity calls print.fwp
  }


  if (isTRUE(uncertainty) & cwres.outliers == FALSE) {
    ip_npage <- ceiling(
      data %>% dplyr::group_by({
        {
          strati1
        }
      }) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>% nrow() / (ip_ncol *  ip_nrow)
    )

    remainder_id <- data %>%  dplyr::group_by({
      {
        strati1
      }
    }) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>% nrow() %% (ip_ncol *  ip_nrow)

    if (remainder_id < ip_ncol &
        remainder_id > 0)
      print(
        'Due to a bug in ggforce::facet_wrap_paginate, you might get an error and be asked to change ip_ncol and/or ip_nrow'
      )


    data2 <- data %>%
      tidyr::pivot_longer(cols = c({
        {
          y
        }
      } , {
        {
          pred
        }
      } , {
        {
          ipred
        }
      }),
      names_to = "variable",
      values_to = "value")

    if (is.null(data2$ipred_se)) {
      stop("No IPRED_SE variable found. Uncertainty cannot be computed")
    }


    low <- (1 - uncert.ci) / 2
    up  <-  1 - low
    ci  <- (uncert.ci) * 100



    data3 <-  data2 %>%
      dplyr::filter(variable == filt_ipred) %>%

      dplyr::mutate(ci.low = value + qnorm(low) * ipred_se,
                    ci.up  = value + qnorm(up)  * ipred_se)

    g1 <-   ggplot2::ggplot() +
      ggplot2::aes(x = {
        {
          x
        }
      }, y = value) +

      ggplot2::geom_point(
        data = data2 %>% filter(variable ==  filt_y),
        aes(
          x =  {
            {
              x
            }
          },
          y = value,
          shape = 'dv',
          colour = 'dv'
        ),
        size = shape_size,
        show.legend = TRUE
      ) +

      ggplot2::scale_shape_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV'
        ),
        values = shape_shape
      ) +
      ggplot2::scale_color_manual(
        name = 'variable',
        labels = c(
          'ipred' = 'IPRED',
          'pred' = 'PRED',
          'dv' = 'DV'
        ),
        values = color_color
      ) +

      ggplot2::geom_ribbon(
        data = data3,
        aes(
          x = {
            {
              x
            }
          },
          ymin = ci.low,
          ymax = ci.up,
          fill = 'ipred'
        ),
        alpha = alpha_fill
      ) +
      ggplot2::scale_fill_manual(
        name = 'variable',
        labels = c('ipred' = paste0(ci, '% CI')),
        values = fill_fill
      ) +
      #guides(fill = guide_legend(title = 'Uncertainty on IPRED'))+

      ggplot2::labs(
        x = xlb,
        y = ylb,
        title = 'Individual plot title',
        subtitle = 'Individual plot with CI around IPRED'
      ) +
      ggcertara::theme_certara(base_size = 11) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::theme(
        legend.direction = "vertical",
        legend.position = "right",
        legend.title = element_blank()
      )

    g = fwp(
      g1,
      ncol = ip_ncol,
      nrow = ip_nrow,
      npg = ip_npage,
      strat1 = strati1,
      strat2 = strati2,
      scale_facet2 = scale_facet,
      labelfacet2 = labelfacet
    )

    print(g) #
    return(g) #

  }

}


## utils function #1
fwp <-
  function(g,
           ncol = ip_ncol,
           nrow = ip_nrow,
           npg = ip_npage,
           strat1 = NULL,
           strat2 = NULL,
           scale_facet2 = scale_facet,
           labelfacet2 = labelfacet) {
    l = list()


    for (i in 1:(npg)) {
      l[[length(l) + 1]] = g + ggforce::facet_wrap_paginate(
        vars(!!strat1, !!strat2),
        ncol = ncol,
        nrow = nrow,
        page = i,
        scales = scale_facet2,
        labeller = labelfacet2
      ) + ggplot2::labs(caption = paste0("Page ", i, " out of ", npg))

    }

    structure(l, class = c("gg", 'fwp'))
  }


## utils function #2
#' @exportS3Method print fwp
print.fwp <- function(x) {
  for (i in seq_along(x))
    print(x[[i]])
}
###############################

## utils function #3
#' @exportS3Method `&` gg
"&.gg" <- function (e1, e2) {
  for (i in seq_along(e1))
    e1[[i]] = e1[[i]] + e2

  e1
}
