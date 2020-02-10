library(rlang)
library(purrr)
library(magrittr)
library(ggplot2)
library(scales)
library(patchwork)

default.labels=list(
    dv    = "Observed Concentration",
    pred  = "Population Predicted Concentration",
    ipred = "Individual Predicted Concentration",
    cwres = "Conditional Weighted Residuals")

lowernames <- function(x) {
    setNames(x, tolower(names(x)))
}

geom_pointC <- function(mapping = NULL, data = NULL,
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

GeomPointC <- ggproto("GeomPointC", GeomPoint,
  default_aes = aes(
    shape = 19, colour = "#2b398b", size = 1.5, fill = NA,
    alpha = 0.3, stroke = 0.5
  )
)

geom_loess <- function(mapping = NULL, data = NULL,
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
    geom = GeomLoess,
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

GeomLoess <- ggproto("GeomLoess", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(
    refcolour = "#ee3124", refsize = 1,
    refalpha = NA, reflinetype = 1
  ),

  draw_panel = function(data, panel_params, coord, span = 2/3, degree = 1, family = "symmetric", na.rm = FALSE) {

    coords <- coord$transform(data, panel_params)

    fit <- loess.smooth(coords$x, coords$y, span=span, degree=degree, family=family)
    col <- alpha(coords$refcolour, coords$refalpha)
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
        lwd = coords$refsize[1] * .pt,
        lty = coords$reflinetype[1]
      )
    )
  },

  draw_key = draw_key_smooth
)

gof_read_data <- function(rundir=getwd()) {
    if (rundir == getwd()) {
        message("Searching for data in current working directory")
    } else {
        message(sprintf("Searching for data in %s", rundir))
    }

    # search in rundir for csv files that contain dv, pred, ipred, cwres, etc.
    ll <- list.files(path=rundir, patter="*.csv")

    data <- NULL
    for (l in ll) {
        data <- read.csv(l, nrows=1, header=T) %>% lowernames()
        if (all(c("dv", "pred", "ipred", "cwres") %in% names(data))) {
            message(sprintf("...Using %s", l))
            data <- read.csv(l, header=T) %>% lowernames()
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

# DV vs. IPRED linear scale
gof_dv_vs_ipred_linear <- function(data, labels=default.labels) {
    ggplot(data, aes(x=ipred, y=dv)) +
        geom_blank(aes(x=dv, y=ipred)) + # Trick to force symmetry
        labs(x=labels$ipred, y=labels$dv) +
        coord_fixed(ratio=1) +
        geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
        geom_pointC() +
        geom_loess() +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(aspect.ratio=1)
}

# DV vs. PRED linear scale
gof_dv_vs_pred_linear <- function(data, labels=default.labels) {
    ggplot(data, aes(x=pred, y=dv)) +
        geom_blank(aes(x=dv, y=pred)) + # Trick to force symmetry
        labs(x=labels$pred, y=labels$dv) +
        coord_fixed(ratio=1) +
        geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
        geom_pointC() +
        geom_loess() +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(aspect.ratio=1)
}

# DV vs. IPRED log scale
gof_dv_vs_ipred_log <- function(data, labels=default.labels) {
    ggplot(data, aes(x=ipred, y=dv)) +
        geom_blank(aes(x=dv, y=ipred)) + # Trick to force symmetry
        labs(x=labels$ipred, y=labels$dv) +
        coord_fixed(ratio=1) +
        scale_x_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
            ) +
        scale_y_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
            ) +
        annotation_logticks(sides="bl") +
        geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
        geom_pointC() +
        geom_loess() +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(aspect.ratio=1)
}

# DV vs. PRED log scale
gof_dv_vs_pred_log <- function(data, labels=default.labels) {
    ggplot(data, aes(x=pred, y=dv)) +
        geom_blank(aes(x=dv, y=pred)) + # Trick to force symmetry
        labs(x=labels$pred, y=labels$dv) +
        coord_fixed(ratio=1) +
        scale_x_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
            ) +
        scale_y_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
            ) +
        annotation_logticks(sides="bl") +
        geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
        geom_pointC() +
        geom_loess() +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(aspect.ratio=1)
}

# Histogram of CWRES
gof_cwres_histogram <- function(data, labels=default.labels) {
    ggplot(data, aes(x=cwres)) +
        geom_blank(aes(x= -cwres)) + # Trick to force symmetry
        labs(x=labels$cwres, y="Density") +
        geom_histogram(aes(y=stat(density)), color="gray80", fill="gray80", bins=20) +
        geom_vline(xintercept=0, col="gray50") +
        stat_density(geom="line", col="#ee3124", size=1) +
        stat_function(fun=dnorm, color="black", linetype="dashed", size=0.8) +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(aspect.ratio=1)
}

# QQ-plot of CWRES
gof_cwres_qqplot <- function(data, labels=default.labels) {
    lim.qq <- with(qqnorm(data$cwres, plot.it=F), range(c(x, y)))
    ggplot(data, aes(sample=cwres)) +
        labs(x="Theoritical Quantile", y="Sample Quantile") +
        coord_fixed(ratio=1, xlim=lim.qq, ylim=lim.qq) +
        stat_qq(color="#2b398b", alpha=0.3) +
        stat_qq_line(col="#ee3124", size=1) +
        geom_abline(slope=1, color="black", linetype="dashed", size=0.8) +
        theme_bw() +
        theme(panel.grid=element_blank()) +
        theme(aspect.ratio=1)
}

gof <- function(
    data=NULL,
    panels=c(1, 2, 3, 4, 5, 6),
    layout=c(3, 2),
    labels=default.labels,
    rundir=getwd())
{
    if (is.null(data)) {
        data <- gof_read_data(rundir)
    }

    p <- list()

    # DV vs. IPRED linear scale
    p[[1]] <- gof_dv_vs_ipred_linear(data, labels)

    # DV vs. PRED linear scale
    p[[2]] <- gof_dv_vs_pred_linear(data, labels)

    # DV vs. IPRED log scale
    p[[3]] <- gof_dv_vs_ipred_log(data, labels)

    # DV vs. PRED log scale
    p[[4]] <- gof_dv_vs_pred_log(data, labels)

    # Histogram of CWRES
    p[[5]] <- gof_cwres_histogram(data, labels)

    # QQ-plot of CWRES
    p[[6]] <- gof_cwres_qqplot(data, lables)

    purrr::reduce(p, `+`) + plot_layout(nrow=layout[1], ncol=layout[2])
}

#data <- gof_read_data()
#gof_dv_vs_ipred_linear(data)# + aes(color=dummy) + facet_wrap(~ dummy)

#gof()

#gof(obs)

#gof(obs) & aes(color=studyid)

#gof(obs) + plot_layout(nrow=2, ncol=3)


