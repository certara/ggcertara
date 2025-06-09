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

