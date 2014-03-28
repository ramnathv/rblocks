make_block <- function(x, ...){
  UseMethod('make_block')
}

#' @export
make_block.list <- function(x, ...){
  as.block(lapply(x, function(xi){
    rep(get_fill(xi), length(xi))
  }))
}

#' @export
make_block.matrix <- function(x, ...){
  as.block(apply(x, 2, function(xi){
    rep(get_fill(xi), length(xi))
  }))
}

#' @export
make_block.data.frame = function(x, ...){
  as.block(as.data.frame(lapply(x, function(xi){
    rep(get_fill(xi), length(xi))
  }), stringsAsFactors = F))
}

# make_block.default = function(x){
#   as.block(rep(get_fill(mode(xi)), length(x)))
# }

#' @export
make_block.default = function(x, ...){
  if (length(x) > 1){
    return(as.block(rep(get_fill(x), length(x))))
  }
  dotlist = list(nrow = x, ...)
  if (!("type" %in% names(dotlist))){
    dotlist$type = infer_type(x)
  }
  if (dotlist$type == 'vector'){
    dotlist$ncol = x
  }
  do.call('block_grid', dotlist)
}



fill_by <- function(f){
  function(x){
    switch(f(x), 'numeric' = "#a6cee3", 'logical' = "#1f78b4", "#b2df8a")
  }
}


get_fill = function(x){
  getOption('fill_by', function(x){return("#b2df8a")})(x)
}


infer_type = function(x){
  if (is.data.frame(x)){
    'data.frame'
  } else if (is.matrix(x)){
    'matrix'
  } else if (is.list(x)) {
    'list'
  } else {
    'vector'
  }
}
