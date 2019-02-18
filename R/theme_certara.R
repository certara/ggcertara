#' A ggplot2 theme for Certara CSC-iDD
#'
#' @inheritParams ggplot2::theme_grey
#' @inheritParams ggplot2::continuous_scale
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}()} or
#' \code{\link[ggplot2]{continuous_scale}()}.
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#' @import ggplot2
#' @export
#' @examples
#' 
#' \donttest{
#' library(ggplot2)
#' 
#' set.seed(123)
#' x <- rnorm(100) + 3
#' g <- rep(0:5, len=100)
#' y <- exp(rnorm(100, 0.33*x + 0.2*g, 0.1))
#' g <- factor(g, labels=c("xxx", "yyy", "zzz", "aaa", "bbb", "ccc"))
#'
#' theme_set(theme_certara())
#' scale_colour_discrete <- function(...) scale_color_certara(...)
#' scale_fill_discrete <- function(...) scale_fill_certara(...)
#'
#' gl <- guide_legend("Legend", title.position="top")
#' p <- ggplot(data.frame(x, y, g), aes(x=x, y=y, color=g, shape=g, linetype=g, fill=g)) +
#'     geom_point() +
#'     geom_smooth(method="lm", se=T) +
#'     labs(title="Random Data", subtitle="test-test") +
#'     guides(colour=gl, fill=gl, shape=gl, linetype=gl)
#' p
#'
#' p + theme_certara_grid()
#' p + scale_y_continuous(position="right")
#'
#' p + facet_wrap(~ g)
#' p + scale_y_log10() + annotation_logticks(sides="l")
#' p + scale_y_log10() + annotation_logticks(sides="l") + theme_certara_grid()
#'
#' v <- ggplot(faithfuld) + geom_tile(aes(waiting, eruptions, fill=density))
#' v + scale_fill_certara_c()
#'
#' dat <- data.frame(x=1:17)
#' p <- ggplot(dat) +
#'     geom_vline(aes(xintercept=x, color=factor(x)), size=10) +
#'     labs(color="")
#' p
#'
#' }
theme_certara <- function(base_size=16, base_family="",
    base_line_size=base_size/22, base_rect_size=base_size/22) {

    half_line <- base_size/2

    theme(
        line=element_line(
            colour="black",
            size=base_line_size, 
            linetype=1,
            lineend="butt"),

        rect=element_rect(
            fill="white", 
            colour="black",
            size=base_rect_size,
            linetype=1), 

        text=element_text(
            family=base_family,
            face="plain", 
            colour="black",
            size=base_size,
            lineheight=0.9, 
            hjust=0.5,
            vjust=0.5,
            angle=0,
            margin=margin(), 
            debug=FALSE),

        axis.line=element_blank(),
        axis.line.x=NULL, 
        axis.line.y=NULL,

        axis.text=element_text(
            size=rel(0.7), 
            colour="grey30"),
        
        axis.text.x=element_text(
            margin=margin(t=0.8*half_line/2),
            vjust=1),

        axis.text.x.top=element_text(
            margin=margin(b=0.8*half_line/2),
            vjust=0),
        
        axis.text.y=element_text(
            angle=90, 
            margin=margin(r=0.8*half_line/2),
            hjust=0.5),

        axis.text.y.right=element_text(
            angle=-90, 
            margin=margin(l=0.8*half_line/2),
            hjust=0.5),
        
        axis.ticks=element_line(colour="grey20"), 

        axis.ticks.length=unit(half_line/2, "pt"),
        
        axis.title.x=element_text(
            margin=margin(t=half_line/2), 
            vjust=1),
        
        axis.title.x.top=element_text(
            margin=margin(b=half_line/2),
            vjust=0),
        
        axis.title.y=element_text(
            angle=90, 
            margin=margin(r=half_line/2),
            vjust=1),
        
        axis.title.y.right=element_text(
            angle=-90, 
            margin=margin(l=half_line/2),
            vjust=0),
        
        legend.background=element_rect(colour=NA), 

        legend.spacing=unit(2*half_line, "pt"),
        legend.spacing.x=unit(1.2, "lines"), 
        legend.spacing.y=NULL,
        
        legend.margin=margin(half_line, half_line, half_line, half_line),
        
        legend.key=element_rect(
            fill="white", colour="NA"),
        
        legend.key.size=unit(1.2, "lines"), 
        legend.key.height=NULL,
        legend.key.width=NULL,
        
        legend.text=element_text(size=rel(1.0)), 
        legend.text.align=NULL,

        legend.title=element_text(hjust=0), 
        legend.title.align=NULL,
        
        legend.position="bottom", 
        legend.direction="horizontal",
        legend.justification="left", 

        legend.box=NULL,
        legend.box.margin=margin(0, 0, 0, 0, "cm"),
        legend.box.background=element_blank(), 
        legend.box.spacing=unit(2 * half_line, "pt"),
        
        panel.background=element_rect(
            fill="white", 
            colour=NA),
        
        panel.border=element_rect(
            fill=NA,
            colour="grey20"),

        panel.grid=element_blank(), 
        panel.grid.minor=element_blank(),

        panel.spacing=unit(half_line, "pt"),
        panel.spacing.x=NULL,
        panel.spacing.y=NULL, 

        panel.ontop=FALSE,
        
        strip.background=element_rect(
            fill="grey90", colour="grey20"),

        strip.text=element_text(
            colour="grey30",
            size=rel(0.8),
            margin=margin(0.3*half_line, 0.3*half_line, 0.5*half_line, 0.3*half_line)), 

        strip.text.x=NULL,
        strip.text.y=element_text(angle=-90), 

        strip.placement="inside",
        strip.placement.x=NULL, 
        strip.placement.y=NULL,

        strip.switch.pad.grid=unit(half_line/2, "pt"),
        strip.switch.pad.wrap=unit(half_line/2, "pt"),

        plot.background=element_rect(colour="white"), 

        plot.title=element_text(
            size=rel(1.2),
            hjust=0.5, 
            vjust=1,
            margin=margin(b=half_line)),
        
        plot.subtitle=element_text(
            hjust=0.5, 
            vjust=1,
            margin=margin(b=half_line)),
        
        plot.caption=element_text(
            size=rel(0.8), 
            hjust=1,
            vjust=1,
            margin=margin(t=half_line)), 

        plot.tag=element_text(
            size=rel(1.2),
            hjust=0.5, 
            vjust=0.5),
        
        plot.tag.position="topleft",
        
        plot.margin=margin(half_line, half_line, half_line, half_line),
        
        complete=TRUE)
}

#' @rdname theme_certara
#' @export
theme_certara_grid <- function(base_size=16, base_family="",
    base_line_size=base_size/22, base_rect_size=base_size/22) {
    theme_certara(
        base_size=base_size,
        base_family=base_family,
        base_line_size=base_line_size,
        base_rect_size=base_rect_size) %+replace% theme(
        panel.grid=element_line(colour="grey92"), 
        panel.grid.minor=element_line(size=rel(1.0)))
}

#' Certara color palette
#'
#' @param n The number of colors requested in the palette.
#' @return A character vector of hex codes for the colors in the palette.
#' @export
#' @examples
#' certara_pal(8)
certara_pal <- function(n=17) {
    # Certara corporate branded colors
    cols <- c(
        "#4982ac",
        "#ee3124",
        "#fdbb30",
        "#6d405d",
        "#007f97",
        "#666691",
        "#0a7bc1",
        "#971b22",
        "#69899e",
        "#877e4b",
        "#f26522",
        "#932784",
        "#2b398b",
        "#279594",
        "#d80b8c",
        "#e31e30",
        "#7159a6")

    if (n == 0) {
        stop("Must request at least one color.")
    }
    if (length(cols) < n)
        cols <- rep(cols, length.out=n) # Recycle colors

    cols[1:n]
}

#' @rdname theme_certara
#' @export
scale_colour_certara <- function(...) {
    ggplot2::discrete_scale(
        aesthetics="colour",
        scale_name="certara",
        palette=certara_pal, ...)
}

#' @rdname theme_certara
#' @export
scale_color_certara <- scale_colour_certara

#' @rdname theme_certara
#' @export
scale_fill_certara <- function(...) {
    ggplot2::discrete_scale(
        aesthetics="fill",
        scale_name="certara",
        palette=certara_pal, ...)
}

#' @rdname theme_certara
#' @export
scale_colour_certara_c <- function(..., guide="colourbar") {
    ggplot2::continuous_scale(
        aesthetics="colour",
        scale_name="certara_c",
        palette=scales::gradient_n_pal(certara_pal(4)[c(1,4,3)]),
        guide=guide, ...)
}

#' @rdname theme_certara
#' @export
scale_fill_certara_c <- function(..., guide="colourbar") {
    ggplot2::continuous_scale(
        aesthetics="fill",
        scale_name="certara_c",
        palette=scales::gradient_n_pal(certara_pal(4)[c(1,4,3)]),
        guide=guide, ...)
}

## TEST
if (FALSE) {
    source("theme_certara.R")
    theme_set(theme_certara())
    #theme_set(theme_certara_grid())
    scale_colour_discrete <- function(...) scale_color_certara(...)
    scale_fill_discrete <- function(...) scale_fill_certara(...)

    set.seed(123)
    x <- rnorm(100)
    g <- rep(0:5, len=100)
    y <- rnorm(100, 0.33*x + 0.2*g, 0.1)
    g <- factor(g, labels=c("xxx", "yyy", "zzz", "aaa", "bbb", "ccc"))

    gl <- guide_legend(title.position="top")
    p <- ggplot(data.frame(x, y, g), aes(x=x, y=y, color=g, shape=g, linetype=g, fill=g)) +
        geom_point() +
        geom_smooth(method="lm", se=T) +
        labs(title="TEST", subtitle="test-test") +
        guides(colour=gl, fill=gl)
    p

    p + theme_certara_grid()
    p + scale_y_continuous(position = "right")

    p + facet_wrap(~ g)

    v <- ggplot(faithfuld) + geom_tile(aes(waiting, eruptions, fill=density))
    v + scale_fill_certara_c()
}
