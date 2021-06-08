require(plotrix)
require(grid)
require(ggplot2)
library(gganimate)

#=========================================================================================================
# read data

# reading data from the data file
setwd('C:/Users/Ahmad/Desktop/Ahmad/python/pi_approximation_visual')
pi_data <- read.csv('./data/data.csv', row.names=1, header=TRUE)
pi_data['pi_appx'][is.na(pi_data['pi_appx'])] <- 0
pi_data$iter <- rownames(pi_data)
pi_data$iter <- as.integer(as.numeric(pi_data$iter))
pi_data

#=========================================================================================================
# prepare visuals

# show simulation setup in a visual
getCircle <- function(center = c(0,0), diameter = 1, npoints = 100){
  seq <- seq(0, 2*pi, length.out = npoints)
  x_seq <- center[1] + diameter / 2 * cos(seq)
  y_seq <- center[2] + diameter / 2 * sin(seq)
  return(data.frame(x = x_seq, y = y_seq))
}


circle_data <- getCircle(diameter=2)
ggplot(circle_data, aes(x, y)) + geom_path(size=1, col='maroon') +
  geom_rect(xmin=-1, xmax=1, ymin=-1, ymax=1, size=1, col='darkblue', alpha=0)
ggsave(filename="./output/image1_setup.png", plot=last_plot(), width = 15, height = 15, units='cm')



# animating the simulation
animateplot <- ggplot(pi_data, aes(x, y)) + 
  geom_rect(xmin=-1, xmax=1, ymin=-1, ymax=1, size=1, col='darkblue', alpha=0) +
  geom_path(data=circle_data, aes(x,y)) +
  geom_point(size=0.1, aes(color=factor(in_circle))) + 
  labs(title = "iter: {frame_time}\n", x = "x-coordinate", y = "y-coordinate", color = "Points\n") +
  scale_color_manual(labels = c("outside circle", "inside circle"), values = c("red", "blue")) +
  transition_time(iter) + shadow_mark()

#animate(animateplot, width = 600, height = 500)
anim_save("./output/simulation.gif", animate(animateplot, width = 600, height = 500))




# show simulation results in a visual
pi_data2 <- pi_data[1:2000,]
ggplot(pi_data2, aes(iter, pi_appx)) + 
  geom_line(aes(color='simulation appx'), linetype = 1, size = 1.5) + 
  geom_hline(aes(yintercept = 3.14159, color = "exact value"), linetype = 1, size = 1) +
  scale_colour_manual(values = c("blue", "red")) +
  ggtitle("Monte Carlo Simulation to approximate Pi") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Iteration",
       y = "Pi Approximation",
       color = "Pi")

ggsave(filename="./output/simulation_results.png", plot=last_plot()) #width=10, height=5, units="cm"