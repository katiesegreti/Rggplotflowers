##crochet theme
crochet_theme <- theme(panel.grid = element_blank(), 
                       panel.background = element_rect(fill = "#D8D8D8"),
                       axis.text = element_blank(), 
                       axis.ticks = element_blank(), 
                       axis.title = element_blank(),
                       legend.position = "none")

##plotting circles

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

stitches <- c(6, 12, 18, 24, 30, 36, 42, 48,
              54, 60, 66, 72)
make_circles <- function(stitchcounts) {
  circlez <- data.frame()
  for(i in 1:length(stitchcounts)) {
    newcircle <- circle_points(radius = i, npoints = stitchcounts[i])
    circlez <- rbind(circlez, newcircle)
  }
  return(circlez)
}

circle0 <- make_circles(stitches)


ggplot() + geom_point(data = circle0, 
                      aes(x = x, y = y), 
                      color = "#DF01D7", size = 3) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "#F7F8E0"),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.position = "none")


##plotting spirals
spiral_points <- function(a = 1, b = 1, n){
  theta <- seq(0,n*pi,0.03)
  r <- a + b * theta
  df <- data.frame(x = r*cos(theta), y = r*sin(theta))
  return(df)
}
## evenly spaced points on a spiral
spiral_points1 <- function(arc = 1, separation = 1, n) {
  r <- arc
  b <- separation / (2*pi)
  phi <- r / b
  xs <- c()
  ys <- c()
  while(n > 0) {
    xs <- c(xs, r * cos(phi))
    ys <- c(ys, r * sin(phi))
    phi <- phi + (arc / r)
    r <- b * phi
    n <- n - 1
  }
  df <- data.frame(x = xs, y = ys)
}


spiral1 <- spiral_points1(n = 468)

ggplot(spiral1, aes(x,y)) + geom_point(color = "#6A0888", 
                                       size = 10, shape = 18) +
  crochet_theme


spiral2 <- spiral_points(n = 4)

ggplot(spiral2, aes(x,y)) + geom_point(color = "#6A0888", 
                                       size = 10, shape = 18) +
  crochet_theme
