#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[fda]{mean.fd}}
#' @rdname mean
#' @export
#' @importFrom fda mean.fd
mean.dd <- function(ddobj, ...) {
  return_list <- FALSE
  if (inherits(ddobj, "list")) {
    ddobj <- merge.dd(ddobj)
    return_list <- TRUE
  }
  meandd <- dd(clr = fda::mean.fd(ddobj), ...)
  if (return_list) list(meandd) else meandd
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddlist PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean
#' @export
mean.ddl <- function(ddlist, ...) as.list(mean(c(ddlist), ...))

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname center
#' @export
center <- function(...) UseMethod("center")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddobj PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[fda]{center.fd}}
#' @rdname center
#' @export
#' @importFrom fda center.fd
center.dd <- function(ddobj, ...) {
  dd(clr = fda::center.fd(ddobj), ...)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddlist PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname center
#' @export
center.ddl <- function(ddlist, ...) as.list(center(c(ddlist), ...))

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gmean
#' @export
gmean <- function(...) UseMethod("gmean")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddlist PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gmean
#' @export
gmean.ddl <- function(ddlist, ...) as.list(gmean(c(ddlist), ...))

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddlist PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gmean
#' @export
gmean.dd <- function(ddlist, ...) mean.dd(ddobj, normalize = FALSE)

#' Bayes space operations
#' @rdname operations
#' @export
`+.dd` <- function(...) dd(clr = fda::plus.fd(...))
#' @rdname operations
#' @export
`-.dd` <- function(...) dd(clr = fda::minus.fd(...))
#' @rdname operations
#' @export
`*.dd` <- function(...) dd(clr = fda::times.fd(...))
#' @rdname operations
#' @export
`[.dd` <- function(...) dd(clr = fda::`[.fd`(...))
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname relative
#' @export
relative <- function(...) UseMethod("relative")
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[fda]{arithmetic.fd}}
#' @rdname relative
#' @export
#' @importFrom fda minus.fd
relative.dd <- function(...) dd(clr = fda::minus.fd(...), normalize = FALSE)

#' @rdname operations
#' @export
`+.ddl` <- function(ddl1, ddl2, ...) as.list(`+.dd`(c(ddl1), c(ddl2), ...))
#' @rdname operations
#' @export
`-.ddl` <- function(ddl1, ddl2, ...) as.list(`-.dd`(c(ddl1), c(ddl2), ...))
#' @rdname operations
#' @export
`*.ddl` <- function(ddl1, ddl2, ...) as.list(`*.dd`(c(ddl1), c(ddl2), ...))
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ddl1 PARAM_DESCRIPTION
#' @param ddl2 PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname relative
#' @export
relative.ddl <- function(ddl1, ddl2, ...) as.list(relative.dd(c(ddl1, normalize = FALSE), c(ddl2, normalize = FALSE), ...))


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION, Default: NULL
#' @param na.rm PARAM_DESCRIPTION, Default: FALSE
#' @param use PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname var
#' @export
var.default <- var

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname var
#' @export
var <- function(...) UseMethod("var")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[fda]{var.fd}}
#' @rdname var
#' @export
#' @importFrom fda var.fd
var.dd <- function(...) structure(fda::var.fd(...), class = "bidd")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION, Default: NULL
#' @param use PARAM_DESCRIPTION, Default: 'everything'
#' @param method PARAM_DESCRIPTION, Default: c("pearson", "kendall", "spearman")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cov
#' @export
cov.default <- cov

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cov
#' @export
cov <- function(...) UseMethod("cov")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[fda]{var.fd}}
#' @rdname cov
#' @export
#' @importFrom fda var.fd
cov.dd <- var.dd
