#' @import ggplot2
#' @import patchwork
#' @importFrom rlang .data
NULL

#####################################################
#' List of default plot labels.
#' 
#' This function specifies the default plot labels. The labels can be overridden 
#' using the \code{baseplot} parameter of the specific DGOF function.
#' @param dv The label for dependent variable.
#' @param pred The label for population predictions.
#' @param ipred The label for individual predictions.
#' @param cwres The label for conditional weighted residuals.
#' @param iwres The label for individual weighted residuals.
#' @param time The label for time.
#' @param tad The label for time after dose.
#' @export
default.labels=list(
  dv    = "Observed Concentration",
  pred  = "Population Predicted Concentration",
  ipred = "Individual Predicted Concentration",
  cwres = "Conditional Weighted Residuals",
  cwres = "Conditional Weighted Residuals",
  iwres = "Individual Weighted Residuals",
  time  = "Time",
  tad   = "Time After Dose"
)


#####################################################
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

#####################################################
#' Use log scales
#'
#' This function puts the respective axes in log scale.
#' @param g A \code{ggplot2} object.
#' @name logscales
NULL

#' @rdname logscales
#' @export
log_x <- function(g) {
  .x <- NULL
  g + scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
    annotation_logticks(sides="b")
}

#' @rdname logscales
#' @export
log_y <- function(g) {
  .x <- NULL
  g + scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
    annotation_logticks(sides="l")
}

#' @rdname logscales
#' @export
log_xy <- function(g) {
  .x <- NULL
  g + scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    annotation_logticks(sides="bl")
}

#####################################################
#' Read gof data
#'
#' This function reads in data files with the .csv file extension and files containing 
#'  the string "sdtab" from the current working directory. Names are set to lower case
#'  and data items are checked to verify that "dv", "pred", and "ipred" are present.
#' @return A \code{data.frame}. 
#' @export
gof_read_data <- function(rundir=getwd()) {
  if (rundir == getwd()) {
    message("Searching for data in current working directory")
  } else {
    message(sprintf("Searching for data in %s", rundir))
  }
  
  # search in rundir for csv files that contain dv, pred, ipred, cwres, etc.
  ll <- list.files(path=rundir, pattern="*.csv", full.names=TRUE)
  ll <- c(ll, list.files(path=rundir, pattern="sdtab", full.names=TRUE))
  
  read.wrapper <- function(file, nrows) {
    if (file == "sdtab") {
      x <- utils::read.table(file, header=TRUE, check.names=FALSE, skip=1, na.strings=c(".", ""))
    } else {
      x <- utils::read.csv(file, header=TRUE, check.names=FALSE, na.strings=c(".", ""))
    }
    stats::setNames(x, tolower(names(x)))
  }
  
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
  
  if (!is.null(data$mdv)) {
    data <- data[data$mdv==0,]
  }
  
  return(data)
}

#####################################################
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
#' @return  A \code{ggplot} object.
#' @export
gof_baseplot <- function(data) {
  ggplot(data) +
    geom_point_c() +
    geom_loess_c() +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)
}

#####################################################
#####################################################
#' Basic gof plots
#'
#' @param data A \code{data.frame}.
#' @param labels A named \code{list} of labels.
#' @param baseplot A function that returns a \code{ggplot} object to use as the
#' base for all scatterplots (histograms and QQ-plots are unaffected).
#' @param log_xy If \code{TRUE} then log-scale will be used for both x- and y- axes.
#' @name gofplots
NULL

#####################################################
#' Plot CWRES vs. PRED
#' @rdname gofplots
#' @export
gof_cwres_vs_pred <- function(data, labels=default.labels, baseplot=gof_baseplot) {
  baseplot(data) +
    aes(x=.data$pred, y=.data$cwres) +
    labs(x=labels$pred, y=labels$cwres) +
    geom_blank(aes(x=.data$pred, y=-.data$cwres)) + # Trick to force symmetry
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8)
}

#####################################################
#' Plot CWRES vs. TIME
#' @rdname gofplots
#' @export
gof_cwres_vs_time <- function(data, labels=default.labels, baseplot=gof_baseplot) {
  baseplot(data) +
    aes(x=.data$time, y=.data$cwres) +
    labs(x=labels$time, y=labels$cwres) +
    geom_blank(aes(x=.data$time, y=-.data$cwres)) + # Trick to force symmetry
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8)
}

#####################################################
#' Plot CWRES vs. TAD
#' @rdname gofplots
#' @export
gof_cwres_vs_tad <- function(data, labels=default.labels, baseplot=gof_baseplot) {
  baseplot(data) +
    aes(x=.data$tad, y=.data$cwres) +
    labs(x=labels$tad, y=labels$cwres) +
    geom_blank(aes(x=.data$tad, y=-.data$cwres)) + # Trick to force symmetry
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8)
}

#####################################################
#' Plot |IWRES| vs. IPRED
#' @rdname gofplots
#' @export
gof_absiwres_vs_ipred <- function(data, labels=default.labels, baseplot=gof_baseplot) {
  baseplot(data) +
    aes(x=.data$ipred, y=abs(.data$iwres)) +
    labs(x=labels$ipred, y=sprintf("|%s|", labels$iwres)) +
    expand_limits(y=0) +
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8)
}

#####################################################
#' Plot |IWRES| vs. TIME
#' @rdname gofplots
#' @export
gof_absiwres_vs_time <- function(data, labels=default.labels, baseplot=gof_baseplot) {
  baseplot(data) +
    aes(x=.data$time, y=abs(.data$iwres)) +
    labs(x=labels$time, y=sprintf("|%s|", labels$iwres)) +
    expand_limits(y=0) +
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8)
}

#####################################################
#' Plot |IWRES| vs. TAD
#' @rdname gofplots
#' @export
gof_absiwres_vs_tad <- function(data, labels=default.labels, baseplot=gof_baseplot) {
  baseplot(data) +
    aes(x=.data$tad, y=abs(.data$iwres)) +
    labs(x=labels$tad, y=sprintf("|%s|", labels$iwres)) +
    expand_limits(y=0) +
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.8)
}

#####################################################
#' Plot DV vs. IPRED
#' @rdname gofplots
#' @export
gof_dv_vs_ipred <- function(data, labels=default.labels, baseplot=gof_baseplot, log_xy=F) {
  g <- baseplot(data) +
    aes(x=.data$ipred, y=.data$dv) +
    labs(x=labels$ipred, y=labels$dv) +
    geom_blank(aes(x=.data$dv, y=.data$ipred)) + # Trick to force symmetry
    geom_abline(slope=1, color="black", linetype="dashed", size=0.8)
  if (isTRUE(log_xy)) {
    g <- log_xy(g)
  }
  g
}

#####################################################
#' Plot DV vs. PRED
#' @rdname gofplots
#' @export
gof_dv_vs_pred <- function(data, labels=default.labels, baseplot=gof_baseplot, log_xy=F) {
  g <- baseplot(data) +
    aes(x=.data$pred, y=.data$dv) +
    labs(x=labels$pred, y=labels$dv) +
    geom_blank(aes(x=.data$dv, y=.data$pred)) + # Trick to force symmetry
    geom_abline(slope=1, color="black", linetype="dashed", size=0.8)
  if (isTRUE(log_xy)) {
    g <- log_xy(g)
  }
  g
}

#####################################################
#' Histogram of CWRES
#' @rdname gofplots
#' @export
gof_cwres_histogram <- function(data, labels=default.labels) {
  density <- NULL
  ggplot(data, aes(x=.data$cwres)) +
    labs(x=labels$cwres, y="Density") +
    geom_blank(aes(x= -.data$cwres)) + # Trick to force symmetry
    geom_histogram(aes(y=stat(density)), color="gray80", fill="gray80", bins=20) +
    geom_vline(xintercept=0, col="gray50") +
    stat_density(geom="line", col="#ee3124", size=1) +
    stat_function(fun=stats::dnorm, color="black", linetype="dashed", size=0.8) +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)
}

#####################################################
#' QQ-plot of CWRES
#' @rdname gofplots
#' @export
gof_cwres_qqplot <- function(data, labels=default.labels) {
  lim.qq <- with(stats::qqnorm(data$cwres, plot.it=FALSE), range(c(x, y)))
  ggplot(data, aes(sample=.data$cwres)) +
    labs(x="Theoritical Quantile", y="Sample Quantile") +
    coord_fixed(ratio=1, xlim=lim.qq, ylim=lim.qq) +
    stat_qq(color="#2b398b", alpha=0.3) +
    stat_qq_line(col="#ee3124", size=1) +
    geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
    theme_certara(base_size=11) +
    theme(aspect.ratio=1)
}

#####################################################
#' List of gof plots
#' 
#' \code{gof_list}  geneates a numbered list of gof plots.
#' @rdname gof
#' @export
gof_list <- function(data=NULL,
                     labels=default.labels,
                     baseplot=gof_baseplot,
                     rundir=getwd())
{
  if (is.null(data)) {
    data <- gof_read_data(rundir)
  }
  
  p <- list()
  
  # Histogram of CWRES
  p[[1]] <- gof_cwres_histogram(data, labels)
  
  # QQ-plot of CWRES
  p[[2]] <- gof_cwres_qqplot(data, labels)
  
  # DV vs. IPRED linear scale
  p[[3]] <- gof_dv_vs_ipred(data, labels)
  
  # DV vs. PRED linear scale
  p[[4]] <- gof_dv_vs_pred(data, labels)
  
  # DV vs. IPRED log scale
  p[[5]] <- gof_dv_vs_ipred(data, labels, log_xy=TRUE)
  
  # DV vs. PRED log scale
  p[[6]] <- gof_dv_vs_pred(data, labels, log_xy=TRUE)
  
  # CWRES vs. PRED
  p[[7]] <- gof_cwres_vs_pred(data, labels)
  
  # CWRES vs. TIME
  p[[8]] <- gof_cwres_vs_time(data, labels)
  
  # CWRES vs. TAD
  p[[9]] <- gof_cwres_vs_tad(data, labels)
  
  # |IWRES| vs. IPRED
  p[[10]] <- gof_absiwres_vs_ipred(data, labels)
  
  # |IWRES| vs. TIME
  p[[11]] <- gof_absiwres_vs_time(data, labels)
  
  # |IWRES| vs. TAD
  p[[12]] <- gof_absiwres_vs_tad(data, labels)
  
  p
}

#####################################################
#' Layout gof plots
#' 
#' \code{gof_layout} allows the user to modify the plot layout.
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

#####################################################
#' Goodness-of-fit diagnostic plots
#' 
#' \code{gof} generats a set of panels of goodness-of-fit diagnostic plots.
#' @inheritParams gofplots
#' @inheritParams gof_read_data
#' @param panels A \code{numeric} vector specifying the panels desired.
#' @param p A \code{list} of `ggplot` objects to be laid out.
#' @param layout A \code{numeric} vector of length 2 giving the number of rows
#' and column in which the panels are to be layed out (for multiple panels).
#' @section Details:
#'  The default plot contains the following 6 panels in a 3x2 matrix:
#'   \itemize{
#'   \item \code{\link{gof_dv_vs_ipred}}
#'   \item \code{\link{gof_dv_vs_pred}}
#'   \item \code{\link{gof_cwres_vs_pred}}
#'   \item \code{\link{gof_cwres_vs_time}}
#'   \item \code{\link{gof_cwres_vs_tad}}
#'   \item \code{\link{gof_absiwres_vs_ipred}}
#'   }
#' @export
gof <- function(data=NULL,
                panels=c(3, 4, 7, 8, 9, 10),
                layout=c(ceiling(length(panels)/2), 2),
                labels=default.labels,
                baseplot=gof_baseplot,
                rundir=getwd())
{
  
  p <- gof_list(data=data, labels=labels, rundir=rundir)
  
  if (length(panels) == 1) {
    p <- p[[panels]]
  } else {
    p <- p[panels]
    p <- gof_layout(p, layout)
  }
  p
}

# vim: ts=2 sw=2 et
