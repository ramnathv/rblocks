make_block <- function(x, ...){
  UseMethod('make_block')
}

make_block.list <- function(x, ...){
  as.block(lapply(x, function(xi){
    rep(get_fill(mode(xi)), length(xi))
  }))
}

make_block.matrix <- function(x, ...){
  as.block(apply(x, 2, function(xj){
    rep(get_fill(mode(xi)), length(xj))
  }))
}

make_block.data.frame = function(x, ...){
  as.block(as.data.frame(lapply(x, function(xi){
    rep(get_fill(mode(xi)), length(xi))
  }), stringsAsFactors = F))
}

# make_block.default = function(x){
#   as.block(rep(get_fill(mode(xi)), length(x)))
# }

make_block.default = function(x, ...){
  if (length(x) > 1){
    return(as.block(rep(get_fill(mode(xi)), length(x))))
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

get_fill <- function(x){
  "#7BEA7B"
  # switch(x, 'numeric' = "#BAE4B3", 'logical' = "#74C476", "#31A354")
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
