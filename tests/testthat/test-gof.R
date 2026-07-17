library(testthat)
library(ggcertara)

test_that("GOF plotting functions return ggplot objects", {
  # Load the built-in dataset
  data(nmtable)
  colnames(nmtable) <- tolower(colnames(nmtable))
  # Test basic GOF plots
  expect_s3_class(gof_cwres_vs_pred(nmtable), "gg")
  expect_s3_class(gof_cwres_vs_time(nmtable), "gg")
  expect_s3_class(gof_dv_vs_pred(nmtable), "gg")
  expect_s3_class(gof_dv_vs_ipred(nmtable), "gg")
  expect_s3_class(gof_absiwres_vs_ipred(nmtable), "gg")
  expect_s3_class(gof_absiwres_vs_time(nmtable), "gg")

  # Test with log scales
  expect_s3_class(gof_cwres_vs_pred(nmtable, log_x = TRUE), "gg")
  expect_s3_class(gof_dv_vs_pred(nmtable, log_xy = TRUE), "gg")

  # Test with custom labels
  custom_labels <- gof_labels(
    dv = "Observed",
    pred = "Predicted",
    ipred = "Individual Predicted",
    cwres = "CWRES"
  )
  expect_s3_class(gof_cwres_vs_pred(nmtable, labels = custom_labels), "gg")

  # Test with highlighting
  nmtable$highlight <- factor(ifelse(abs(nmtable$cwres) > 2, "Outlier", "Normal"))
  expect_s3_class(gof_cwres_vs_pred(nmtable, highlight = highlight), "gg")
})

test_that("GOF plotting functions handle errors gracefully", {
  # Test with missing required columns
  bad_data <- data.frame(time = 1:10)
  expect_error(print(gof_cwres_vs_pred(bad_data)), regexp = "`pred` not found")
  expect_error(print(gof_dv_vs_pred(bad_data)),  regexp = "`pred` not found")
})

test_that("indiv_plot renders DV points from the correct column", {
  # Regression: enquo(y) previously clobbered the y promise before
  # substitute(y) ran, so the DV filter matched zero rows and no DV
  # points/lines were drawn (see report of "Removed N rows" warnings).
  data(nmtable)
  colnames(nmtable) <- tolower(colnames(nmtable))
  nmtable <- dplyr::filter(nmtable, id < 10)

  # The interactive print(g) side effect is absorbed by the temp graphics
  # device set up in setup.R. The off-page point removal warnings are
  # inherent to the paginated free-y facets, so they are silenced here to
  # keep the assertion focused on the DV data.
  p <- suppressWarnings(
    indiv_plot(
      nmtable,
      x = time,
      y = dv,
      ipred = ipred,
      pred = pred,
      scale_facet = "free_y",
      strati1 = id,
      ip_ncol = 3,
      ip_nrow = 2
    )
  )

  # indiv_plot returns a paginated 'fwp' list of ggplot pages.
  expect_s3_class(p, "fwp")

  # The first geom_point layer is the DV series; after the fix it must
  # contain the observed DV values rather than being filtered to zero rows.
  built <- ggplot2::ggplot_build(p[[1]])
  dv_layer <- built$data[[1]]
  expect_gt(sum(is.finite(dv_layer$y)), 0)
})

