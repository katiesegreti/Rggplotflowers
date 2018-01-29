#PHYLLOTAXIS

# This sets plot images to a nice size.
options(repr.plot.width = 4, repr.plot.height = 4)

library(ggplot2)

t <- seq(0, 2*pi, length.out = 50)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point(size = 8, alpha = 0.5, color = "darkgreen") +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())


# Defining the number of points
points <- 500

# Defining the Golden Angle
#angle <- pi * (3 - sqrt(5))
angle <- pi * (3 * sqrt(5))

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
#p <- ggplot(df, aes(x*t, y*t))

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(size = 8, alpha = 0.5, color = "purple", shape = 18) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

#dandelion
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(alpha = 0.5, color = "purple", shape = 8, aes(size = t)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")

#sunflower
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(alpha = 0.5, color = "yellow", shape = 17, aes(size = t)) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "darkmagenta"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")



#challenge

angle <- 14 * pi / 180
points <- 5000

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)


df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(color = "purple", shape = 1, alpha = 0.5, size = 8) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")