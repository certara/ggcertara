#' A ggplot2 theme for Certara CSC-iDD
#'
#' @inheritParams ggplot2::theme_grey
#' @inheritParams ggplot2::continuous_scale
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}()} or
#' \code{\link[ggplot2]{continuous_scale}()}.
#' @details There are 3 variants of the theme: no grid
#' (\code{theme_certara()}), full grid (\code{theme_certara_grid()}), and
#' horizontal grid lines only (\code{theme_certara_hgrid()}).
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
#' g <- factor(g, labels=do.call(paste0, rep(list(letters[1:6]), 3)))
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
#' dat <- data.frame(x=1:8)
#' p <- ggplot(dat) +
#'     geom_col(aes(x=x, y=1, fill=factor(x)), color=NA) +
#'     labs(fill="")
#' p
#' p + scale_fill_certara(palnum=2)
#' p + scale_fill_certara(palnum=3)
#' p + scale_fill_certara(palnum=4)
#' p + scale_fill_certara(palnum=5)
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
            colour="grey40"),
        
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
        
        axis.ticks=element_line(colour="grey40", size=0.3), 

        axis.ticks.length=unit(half_line, "pt"),
        axis.ticks.length.x=unit(half_line, "pt"),
        axis.ticks.length.x.top=unit(half_line, "pt"),
        axis.ticks.length.x.bottom=unit(half_line, "pt"),
        axis.ticks.length.y=unit(half_line, "pt"),
        axis.ticks.length.y.left=unit(half_line, "pt"),
        axis.ticks.length.y.right=unit(half_line, "pt"),

        axis.title=element_text(colour="grey40"),
        
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
            colour="grey60",
            size=0.3),

        panel.grid=element_blank(), 
        panel.grid.major=element_blank(), 
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
        panel.grid.major=element_line(colour="grey90", size=0.3), 
        panel.grid.minor=element_line(colour="grey90", size=0.3))
}

#' @rdname theme_certara
#' @export
theme_certara_hgrid <- function(base_size=16, base_family="",
    base_line_size=base_size/22, base_rect_size=base_size/22) {
    theme_certara(
        base_size=base_size,
        base_family=base_family,
        base_line_size=base_line_size,
        base_rect_size=base_rect_size) %+replace% theme(
        panel.grid.major.y=element_line(colour="grey90", size=0.3), 
        panel.grid.minor.y=element_line(colour="grey90", size=0.3))
}

#' Certara color palettes
#'
#' @param palnum Choose from 1 of 5 different color palettes with 8 colors each.
#' @return A palette function, that returns a vector of hex codes for the
#' colors in the palette when called with a single integer argument.
#' @export
#' @examples
#' certara_pal(1)(8)
certara_pal <- function(palnum=1) {

    pal.list <- list(

        # Palette 1
        c("#29398c",  # dark blue
          "#b8a394",  # beige
          "#f98068",  # salmon
          "#72cbed",  # light blue
          "#7059a6",  # purple
          "#b7a148",  # yellow-green
          "#067f97",  # dark cyan
          "#475c6b"), # charcoal

        # Palette 2
        c("#4682ac",  # blue
          "#ee3124",  # red
          "#fdbb2f",  # yellow-gold
          "#6d405d",  # burgundy?
          "#093b6d",  # dark blue
          "#2f71fd",  # bright blue
          "#336343",  # dark green
          "#52ccbb"), # mint

        # Palette 3
        c("#093b6d",  # dark blue
          "#5cc8f7",  # light blue
          "#d64d20",  # reddish orange
          "#32a17e",  # sort of green
          "#d89a17",  # darkish yellow
          "#7059a6",  # purple
          "#336343",  # dark green
          "#9c8777"), # beige?

        # Palette 4
        c("#279594",  # teal
          "#e07070",  # pink
          "#5cc8f7",  # light blue
          "#ef761b",  # orange
          "#113df2",  # bright blue
          "#f2c611",  # yellow
          "#52ccbb",  # mint
          "#a52f43"), # dark red

        # Palette 5
        c("#29398C",  # dark blue
          "#336343",  # dark green
          "#803333",  # dark red
          "#B35D1B",  # dark orange
          "#067F97",  # dark cyan
          "#B7A148",  # dark yellow
          "#7059A6",  # purple
          "#75604D")  # brown
    )

    cols <- unlist(pal.list[((1:length(pal.list) + (palnum - 2)) %% length(pal.list)) + 1])

    function(n=8) {
        if (n == 0) {
            stop("Must request at least one color.")
        }
        rep(cols, length.out=n) # Recycle colors
    }
}

#' @rdname theme_certara
#' @export
scale_colour_certara <- function(...) {
    ggplot2::discrete_scale(
        aesthetics="colour",
        scale_name="certara",
        palette=certara_pal(...))
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
        palette=certara_pal(...))
}

#' @rdname theme_certara
#' @export
scale_colour_certara_c <- function(..., guide="colourbar") {
    ggplot2::continuous_scale(
        aesthetics="colour",
        scale_name="certara_c",
        palette=scales::gradient_n_pal(certara_pal(1)(3)),
        guide=guide)
}

#' @rdname theme_certara
#' @export
scale_fill_certara_c <- function(..., guide="colourbar") {
    ggplot2::continuous_scale(
        aesthetics="fill",
        scale_name="certara_c",
        palette=scales::gradient_n_pal(certara_pal(1)(3)),
        guide=guide)
}

