suppressPackageStartupMessages({
  library(shiny)
  library(colourpicker)
  #library(extrafont)
  #extrafont::loadfonts(device="win", quiet=T)
  library(ggplot2)
  library(patchwork)
  library(colorblindr) # https://github.com/clauswilke/colorblindr
})

# Palette 1
pal1 <-
c("#4682ac",  # blue
  "#ee3124",  # red
  "#fdbb2f",  # yellow-gold
  "#6d405d",  # burgundy
  "#093b6d",  # dark blue
  "#2f71fd",  # bright blue
  "#336343",  # dark green
  "#803333",  # dark red
  "#279594",  # teal
  "#ef761b")  # orange

# Palette 2
pal2 <-
c("#29398c",  # dark blue
  "#32a17e",  # sort of green
  "#d89a17",  # darkish yellow
  "#d64d20",  # reddish orange
  "#5cc8f7",  # light blue
  "#9c8777",  # beige
  "#7059a6",  # purple
  "#e07070",  # pink
  "#475c6b",  # charcoal
  "#75604D")  # brown

# Palette 3
pal3 <-
c("#067f97",  # dark cyan
  "#b7a148",  # yellow-green
  "#f98068",  # salmon
  "#72cbed",  # light blue
  "#b8a394",  # beige
  "#b35d1b",  # dark orange
  "#a52f43",  # dark red
  "#113df2",  # bright blue
  "#f2c611",  # yellow
  "#52ccbb")  # mint


# Blues & greens
pal4 <-
c("#4682ac",  # blue
  "#2f71fd",  # bright blue
  "#113df2",  # bright blue
  "#5cc8f7",  # light blue
  "#72cbed",  # light blue
  "#52ccbb",  # mint
  "#279594",  # teal
  "#067f97",  # dark cyan
  "#32a17e",  # sort of green
  "#336343")  # dark green

# Reds, oranges & yellow
pal5 <-
c("#803333",  # dark red
  "#a52f43",  # dark red
  "#ee3124",  # red
  "#f98068",  # salmon
  "#d64d20",  # reddish orange
  "#ef761b",  # orange
  "#b35d1b",  # dark orange
  "#d89a17",  # darkish yellow
  "#fdbb2f",  # yellow-gold
  "#f2c611")  # yellow

# Neutral colors (purple, pink, brown and gray)
pal6 <-
c("#b7a148",  # dark yellow
  "#9c8777",  # light beige
  "#b8a394",  # dark beige
  "#75604D",  # brown
  "#6d405d",  # burgundy
  "#e07070",  # pink
  "#7059A6",  # purple
  "#29398c",  # dark blue
  "#093b6d",  # dark blue
  "#475c6b")  # charcoal




pal.init <- pal1

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(width=2,
        sliderInput("ncol", "Number of colors", value=8, min=1, max=10, step=1, ticks=FALSE),
        selectInput("pal", "Reset", choices=c("Group 1"="Pal1", "Group 2"="Pal2", "Group 3"="Pal3"), selected="Pal1"),
        colourInput("col1", NULL, value=pal.init[1], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col2", NULL, value=pal.init[2], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col3", NULL, value=pal.init[3], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col4", NULL, value=pal.init[4], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col5", NULL, value=pal.init[5], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col6", NULL, value=pal.init[6], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col7", NULL, value=pal.init[7], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col8", NULL, value=pal.init[8], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col9", NULL, value=pal.init[9], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        colourInput("col10", NULL, value=pal.init[10], closeOnClick=T, palette="limited", allowedCols=c(pal4, pal5, pal6)),
        radioButtons("selmode", "Color selection mode", c("Pre-specified"="limited", "Arbitrary"="square")),
        selectizeInput("reord", "Reorder colors", choices=1:10, selected=1:10, multiple=T, options=list(plugins=list('drag_drop'))),
        actionButton("swap", "Swap"),
        checkboxInput("colorblind", "Simulate color blindness")),
      mainPanel(
        plotOutput("plot", width="1600px", height="850px"),
        verbatimTextOutput("code", TRUE)))),

  server = function(input, output, session) {

    values <- reactiveValues(plotobj = NULL)

    if (exists(".colorexplorer_plotobj")) {
      values$plotobj <- get(".colorexplorer_plotobj")
    }

    session$onSessionEnded(function() {
      if (exists(".colorexplorer_plotobj")) {
        stopApp()
      }
    })

    observeEvent(input$pal, {
      updateSelectizeInput(session, "reord", selected=1:10)
      pal <- switch(input$pal, Pal1=pal1, Pal2=pal2, Pal3=pal3, Pal4=pal4, Pal5=pal5, Pal6=pal6)
      updateColourInput(session, "col1", value=pal[1])
      updateColourInput(session, "col2", value=pal[2])
      updateColourInput(session, "col3", value=pal[3])
      updateColourInput(session, "col4", value=pal[4])
      updateColourInput(session, "col5", value=pal[5])
      updateColourInput(session, "col6", value=pal[6])
      updateColourInput(session, "col7", value=pal[7])
      updateColourInput(session, "col8", value=pal[8])
      updateColourInput(session, "col9", value=pal[9])
      updateColourInput(session, "col10", value=pal[10])
    })

    palr <- debounce(reactive({
      c(
        input$col1,
        input$col2,
        input$col3,
        input$col4,
        input$col5,
        input$col6,
        input$col7,
        input$col8,
        input$col9,
        input$col10)[as.numeric(input$reord)]
    }), 200)

    observeEvent(input$swap, {
      pal <- palr()
      updateColourInput(session, "col1", value=pal[1])
      updateColourInput(session, "col2", value=pal[2])
      updateColourInput(session, "col3", value=pal[3])
      updateColourInput(session, "col4", value=pal[4])
      updateColourInput(session, "col5", value=pal[5])
      updateColourInput(session, "col6", value=pal[6])
      updateColourInput(session, "col7", value=pal[7])
      updateColourInput(session, "col8", value=pal[8])
      updateColourInput(session, "col9", value=pal[9])
      updateColourInput(session, "col10", value=pal[10])
      updateSelectizeInput(session, "reord", selected=1:10)
    })

    observeEvent(input$selmode, {
        updateColourInput(session, "col1", palette=input$selmode)
        updateColourInput(session, "col2", palette=input$selmode)
        updateColourInput(session, "col3", palette=input$selmode)
        updateColourInput(session, "col4", palette=input$selmode)
        updateColourInput(session, "col5", palette=input$selmode)
        updateColourInput(session, "col6", palette=input$selmode)
        updateColourInput(session, "col7", palette=input$selmode)
        updateColourInput(session, "col8", palette=input$selmode)
        updateColourInput(session, "col9", palette=input$selmode)
        updateColourInput(session, "col10", palette=input$selmode)
    })

    dat <- reactive({

      sim <- function(n=80) {
        set.seed(123)
        x0 <- runif(10, -3, 3)
        b0 <- runif(10, 0, 4)
        b1 <- runif(10, 0.1, 0.5)

        dat <- lapply(1:10, function(i) {
          x <- rnorm(n, x0[i], 1)
          y <- b0[i] + b1[i]*x + rnorm(n, 0, 0.6)
          data.frame(set=factor(paste("Set", i)), x=x, y=y)
        })

        do.call(rbind, dat)
      }

      sim()
    })

    dat1 <- reactive({
      keep <- dat()$set %in% levels(dat()$set)[1:input$ncol]
      dat()[keep,]
    })

    output$plot <- renderPlot(res=200, {

      if (!is.null(values$plotobj)) {
        g <- values$plotobj +
          scale_color_manual(values=palr()) +
          scale_fill_manual(values=palr())

        if (input$colorblind) {
          return(cvd_grid(g))
        } else {
          return(g)
        }
      }

      g1 <- ggplot(dat1(), aes(x=set, y=y, color=set, fill=set)) +
        labs(x="X-Axis Label [units]", y="Y-Axis Label [units]", color=NULL, fill=NULL) +
        geom_jitter(width=0.1, alpha=0.3) +
        geom_boxplot(width=0.6, outlier.shape=NA, alpha=0.1) +
        scale_color_manual(values=palr()) +
        scale_fill_manual(values=palr()) +
        #theme_bw(base_size=11, base_family="Arial Narrow") +
        theme_bw(base_size=11) +
        theme(
          legend.position="bottom", 
          axis.text=element_text(color="gray40"),
          axis.title=element_text(color="gray40"),
          axis.ticks=element_line(color="grey40", size=0.3), 
          panel.border=element_rect(fill=NA, color="grey60", size=0.3),
          panel.grid.major.y=element_line(colour="grey90", size=0.3), 
          panel.grid.minor.y=element_line(colour="grey90", size=0.3),
          panel.grid.major.x=element_blank(), 
          panel.grid.minor.x=element_blank())

      g2 <- ggplot(dat1(), aes(x=x, y=y, color=set, fill=set)) +
        labs(x="X-Axis Label [units]", y="Y-Axis Label [units]", color=NULL, fill=NULL) +
        geom_point(size=2, alpha=0.3) +
        geom_smooth(method="lm", formula=y~x, size=0.8, se=FALSE) +
        scale_color_manual(values=palr()) +
        #theme_bw(base_size=11, base_family="Arial Narrow") +
        theme_bw(base_size=11) +
        theme(
          legend.position="bottom", 
          axis.text=element_text(color="gray40"),
          axis.title=element_text(color="gray40"),
          axis.ticks=element_line(color="grey40", size=0.3), 
          panel.border=element_rect(fill=NA, color="grey60", size=0.3),
          panel.grid.major=element_line(colour="grey90", size=0.3), 
          panel.grid.minor=element_line(colour="grey90", size=0.3))

      if (input$colorblind) {
        return(cvd_grid(g1 + labs(x=NULL, y=NULL)))
      }

      g1 + g2 + patchwork::plot_layout(nrow=1, guides=NULL)
    })

    output$code <- renderText({
      #values <- palr()[1:input$ncol]
      values <- palr()
      values <- paste0("\"", values, "\"", collapse=", ")

      ggcolor <- sprintf("scale_color_manual(values=c(%s))", values)
      ggfill <- sprintf("scale_fill_manual(values=c(%s))", values)
      basegfx <- sprintf("palette(c(%s))", values)

      txt <- ""
      txt <- paste0(txt, "# ggplot:\n")
      txt <- paste0(txt, ggcolor, "\n")
      txt <- paste0(txt, ggfill, "\n")
      txt <- paste0(txt, "\n")
      txt <- paste0(txt, "# Base graphics:\n")
      txt <- paste0(txt, basegfx, "\n")
    })
  }
)

# vim: ts=2 sw=2 et
