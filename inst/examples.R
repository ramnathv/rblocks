## Data Frames
d1 = block_grid(5, 5)

# full data frame
d1

# data frame with one column
d1[1]

# data frame with one row
d1[1,]

# vector
display(d1[[1]])

# select first column
d1[1] <- 'red'
d1

# select first row
d1[1,] <- 'blue'
d1

# select first column
d1[[1]] <- 'orange' 
d1
d1[,1] <- 'darkorange'
d1


## Matrix
m1 = block_grid(5, 5, type = 'matrix')

## full matrix
m1

## ERROR!!
display(m1[1])
display(m1[[1]])

## vector 
display(m1[1,])

# select element (1, 1)
m1[1] <- 'red'
m1

# select first row
m1[1,] <- 'blue'
m1

# select first column
m1[,1] <- 'orange'
m1

