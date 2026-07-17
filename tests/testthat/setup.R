# Route graphics produced while printing plots in tests (e.g. indiv_plot's
# internal print(), or expect_error(print(gof_*))) to a temporary file so the
# suite never leaves an Rplots.pdf artifact behind in tests/testthat/.
plot_device_file <- tempfile(fileext = ".pdf")
grDevices::pdf(plot_device_file)
withr::defer(
  {
    grDevices::dev.off()
    unlink(plot_device_file)
  },
  teardown_env()
)
