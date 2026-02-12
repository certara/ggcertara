library(testthat)
library(ggcertara)

# Helper: fully render a ggplot object to trigger any render-time warnings
render_plot <- function(p) {
  grob <- ggplot2::ggplotGrob(p)
  invisible(grob)
}

# --- Theme functions ---
test_that("theme_certara() produces no deprecation warnings", {
  expect_no_warning(theme_certara())
  expect_no_warning(theme_certara_grid())
  expect_no_warning(theme_certara_hgrid())
})

# --- Scale functions ---
test_that("scale functions produce no deprecation warnings", {
  dat <- data.frame(x = 1:5, y = 1:5, g = factor(letters[1:5]))

  expect_no_warning({
    p <- ggplot(dat, aes(x, y, colour = g)) +
      geom_point() +
      scale_color_certara()
    render_plot(p)
  })

  expect_no_warning({
    p <- ggplot(dat, aes(x, y, fill = g)) +
      geom_col() +
      scale_fill_certara()
    render_plot(p)
  })

  expect_no_warning({
    p <- ggplot(dat, aes(x, y, colour = y)) +
      geom_point() +
      scale_colour_certara_c()
    render_plot(p)
  })

  expect_no_warning({
    p <- ggplot(dat, aes(x, y, fill = y)) +
      geom_tile() +
      scale_fill_certara_c()
    render_plot(p)
  })
})

# --- GOF plots with line-based geoms ---
test_that("GOF residual/identity plots produce no deprecation warnings", {
  data(nmtable)
  colnames(nmtable) <- tolower(colnames(nmtable))

  # gof_residual uses geom_hline + element_line
  expect_no_warning({
    p <- gof_cwres_vs_pred(nmtable)
    render_plot(p)
  })

  # gof_identity uses geom_abline
  expect_no_warning({
    p <- gof_dv_vs_pred(nmtable)
    render_plot(p)
  })

  # gof_identity with log scale uses annotation_logticks
  # Muffle expected data warnings (log-10, non-finite rows); deprecation warnings still propagate
  expect_no_warning({
    withCallingHandlers({
      p <- gof_dv_vs_pred(nmtable, log_xy = TRUE)
      render_plot(p)
    }, warning = function(w) {
      if (!grepl("deprecated|lifecycle", conditionMessage(w), ignore.case = TRUE))
        invokeRestart("muffleWarning")
    })
  })
})

test_that("GOF histogram plots produce no deprecation warnings", {
  data(nmtable)
  colnames(nmtable) <- tolower(colnames(nmtable))

  # gof_histogram uses stat_function
  expect_no_warning({
    p <- gof_cwres_histogram(nmtable)
    render_plot(p)
  })
})

# --- Custom geoms ---
test_that("geom_fitline_c and geom_loess_c produce no deprecation warnings", {
  dat <- data.frame(x = 1:20, y = cumsum(rnorm(20)))

  expect_no_warning({
    p <- ggplot(dat, aes(x, y)) +
      geom_point_c() +
      geom_fitline_c() +
      theme_certara()
    render_plot(p)
  })

  expect_no_warning({
    p <- ggplot(dat, aes(x, y)) +
      geom_point_c() +
      geom_loess_c() +
      theme_certara()
    render_plot(p)
  })
})

# --- Theme grid variants ---
test_that("theme grid variants produce no deprecation warnings when rendered", {
  dat <- data.frame(x = 1:10, y = rnorm(10))

  expect_no_warning({
    p <- ggplot(dat, aes(x, y)) +
      geom_point() +
      theme_certara_grid()
    render_plot(p)
  })

  expect_no_warning({
    p <- ggplot(dat, aes(x, y)) +
      geom_point() +
      theme_certara_hgrid()
    render_plot(p)
  })
})
