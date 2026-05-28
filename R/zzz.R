#' @keywords internal
#' @importFrom rlang .data :=
#' @importFrom dplyr `%>%`
#' @importFrom utils getS3method
NULL

# R CMD check sees dplyr/tidyr/ggplot2 column names referenced inside
# rlang-quoted expressions and flags them as undefined globals. Registering
# them silences those notes without disabling NSE.
utils::globalVariables(c(
  ".data", ".",
  # ggplot2 aesthetics used unquoted inside aes()
  "x", "y", "id", "fun",
  "var1", "var2", "other",
  "IC", "Index", "eigenfun", "selected", "outlier",
  # used inside our dd / dispatch bodies
  "ddobj"
))
