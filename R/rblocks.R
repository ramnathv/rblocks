#' Creates a block grid of a given data type
#'
#' @param nrow number of rows
#' @param ncol number of columns
#' @param type data type of object
#' @param fill default fill color
#' @export
#' @examples
#' grid1 = block_grid(10, 10)
#' grid1 = block_grid(10, 10, 'matrix')
#' grid1[1]  = 'red'
#' grid1
#' grid1 = block_grid(10, type = 'vector')
#' grid1[1]  = 'red'
#' grid1
block_grid = function(nrow, ncol = nrow, type = 'data.frame', fill = get_fill("")){
  data_ = matrix(fill, nrow, ncol)
  blk = switch(type,
    "data.frame" = as.data.frame(data_, stringsAsFactors = F),
    "matrix" = data_,
    "vector" = rep(fill, nrow)
  )
  as.block(blk)
}

#' Print a block object as raw data
#'
#' @param x object to print
#' @export
print_raw = function(x){
  class(x) = class(x)[-1]
  print(x)
}

#' Add block class to an object
#'
#' @param x object to add block class to
#' @keywords internal
#' @export
as.block = function(x){
  class(x) = c('block', class(x))
  return(x)
}

#' @export
print.block = function(x, ...){
  display(x, ...)
}

#' Display a block grid
#'
#' The implementation here is borrowed from sna::plot.sociomatrix
#'
#' @param block an object of block class
#' @export
display = function(block, show_values = F){
  values = block
  if (!('block' %in% class(block))){
    block = make_block(block)
  }
  gap = 0.5
  if (!is.atomic(block) && is.null(dim(block))){
    maxLen = max(sapply(block, length))
    data = as.data.frame(matrix('white', maxLen, length(block)))
    datavalues = as.data.frame(matrix('', maxLen, length(block)))
    if (!is.null(names(block))) names(data) = names(block)
    for (i in seq_along(block)){
      data[i] <- c(
        block[[i]],
        rep('white', maxLen - length(block[[i]]))
      )
      datavalues[i] <- c(
        values[[i]],
        rep("", maxLen - length(values[[i]]))
      )
    }
  } else if (length(dim(block)) < 2){
    data <- matrix('white', length(block), length(block))
    datavalues <- matrix('', length(block), length(block))
    data[1,] = block
    datavalues[1,] = values
  } else {
    data = block
    datavalues = values
  }
  n = dim(data)[1]; o = dim(data)[2]
  drawlines = TRUE
  cur_mar = par('mar')
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  plot(1, 1, xlim = c(0, o + 1), ylim = c(n + 1, 0), type = "n",
       axes = FALSE, xlab = "", ylab = "", asp = 1
  )
  if (is.data.frame(data)){
    segments(1, 0, o, 0, col = 'darkgray')
    # points(1:n, rep(0, o), pch = 16)
    text(1:o, rep(0, o), labels = names(data), font = 2)
    # points(1:o, rep(0, o), pch = 21)
    segments(1:o, 0.1, 1:o, 0.5, col = 'darkgray')
    gap = 0.4
  }
  for (i in 1:n){
    for (j in 1:o) {
      rect(j - gap, i + 0.5, j + gap, i - 0.5,
        col = data[i, j], xpd = TRUE, border = 'white'
      )
      # text(i, j, labels = paste("(", i, ",", j, ")"))
      # text(j, i, labels = paste("(", j, ",", i, ")"))
    }
  }

  if (show_values){
    for (i in 1:NCOL(datavalues)){
      for (j in 1:NROW(datavalues)) {
        text(i, j, labels = substr(datavalues[j, i], 1, 2))
      }
    }
  }

  rect(0.5, 0.5, o + 0.5, n + 0.5, col = NA, xpd = TRUE, border = 'white')
  par(mar = cur_mar)
}
