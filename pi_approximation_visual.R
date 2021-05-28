require(plotrix)
require(grid)

# reading data from the data file
setwd('C:/Users/Ahmad/Desktop/Ahmad/python/pi_approximation_visual')
pi_data <- read.csv('./data/data.csv', row.names=1)
pi_data['pi_appx'][is.na(pi_data['pi_appx'])] <- 0
pi_data


# create the visual of a circle of radius 1 inscribed inside of a square of side
# 2, both centered at the origin
plot(c(-1, 1), c(-1,1), type = "n", asp=1)
rect( -1., -1., 1., 1., border = "blue", lwd = 2)
draw.circle( 0, 0, 1., border = "red", lwd = 2 )

for (row in 1:nrow(pi_data)) {
  x <- pi_data[row, "x"]
  y  <- pi_data[row, "y"]
  in_circle  <- pi_data[row, "in_circle"]
  
  if(in_circle==1) {
    points(x, y, col = "red", lwd = 2 )
  }
  else {
    points(x, y, col = "blue", lwd = 2 )
  }
}
