block_to_json = function(x){
  UseMethod("block_to_json")
}

#' @export
block_to_json.matrix <- function(x){
  dat = vector('list', NROW(x)*NCOL(x))
  m = 1
  for (j in 1:NCOL(x)){
    for (i in 1:NROW(x)){
      dat[[m]] <- list(i, j, x[i, j])
      m <- m + 1
    }
  }
  return(dat)
}

#' @export
block_to_json.data.frame <- function(x){
  dat = vector('list', NROW(x)*NCOL(x))
  m = 1
  for (j in 1:NCOL(x)){
    for (i in 1:NROW(x)){
      dat[[m]] <- list(i, j, x[i, j])
      m <- m + 1
    }
  }
  return(dat)
}

#' @export
block_to_json.block <- function(x){
  NextMethod()
}

#' @export
display_d3 <- function(block){
  if (!('block' %in% class(block))){
    block = make_block(block)
  }
  d1 <- rCharts$new()
  d3block = system.file('d3block', package = 'rblocks')
  d1$setLib(d3block)
  d1$setTemplate(afterScript = "<p></p>")
  alpha = ifelse(is.data.frame(block), 1.3, 1)
  d1$set(data = block_to_json(block), alpha = alpha)
  d1$field('lib', 'd3block')
  d1
}

#' Hook to crop png using imagemagick convert
#'
#'
hook_crop_png = function(before, options, envir){
  if (before){
    return()
  }
  ext = tolower(options$fig.ext)
  if (ext != "png") {
    warning("this hook only works with PNG at the moment")
    return()
  }
  if (!nzchar(Sys.which("convert"))) {
    warning("cannot find convert; please install and put it in PATH")
    return()
  }
  paths = knitr:::all_figs(options, ext)
  lapply(paths, function(x) {
    message("optimizing ", x)
    x = shQuote(x)
    cmd = paste("convert", if (is.character(options$convert))
      options$convert, x, x)
    if (.Platform$OS.type == "windows")
      cmd = paste(Sys.getenv("COMSPEC"), "/c", cmd)
    system(cmd)
  })
  return()
}
