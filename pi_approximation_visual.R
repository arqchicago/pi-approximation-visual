require(plotrix)
require(grid)
require(ggplot2)

# reading data from the data file
setwd('C:/Users/Ahmad/Desktop/Ahmad/python/pi_approximation_visual')
pi_data <- read.csv('./data/data.csv', row.names=1, header=TRUE)
pi_data['pi_appx'][is.na(pi_data['pi_appx'])] <- 0
pi_data$iter <- rownames(pi_data)
pi_data$iter <- as.numeric(pi_data$iter)
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


#=============================================================
require(ggplot2)

getCircle <- function(center = c(0,0), diameter = 1, npoints = 100){
  seq <- seq(0, 2*pi, length.out = npoints)
  x_seq <- center[1] + diameter / 2 * cos(seq)
  y_seq <- center[2] + diameter / 2 * sin(seq)
  return(data.frame(x = x_seq, y = y_seq))
}


circle_data <- getCircle(diameter=2)
ggplot(circle_data, aes(x, y)) + geom_path(size=1, col='maroon') +
  geom_rect(xmin=-1, xmax=1, ymin=-1, ymax=1, size=1, col='darkblue', alpha=0)

pi_data2 <- pi_data[1:2000,]
ggplot(pi_data2, aes(iter, pi_appx)) + geom_point() + 
  geom_hline(yintercept=3.14159, col='red')