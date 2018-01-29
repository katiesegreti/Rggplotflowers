library(ggplot2)


#set number of spokes
spokes <- seq(0.1, 2*pi + 0.1, length.out = 15)
#set coordinates for spokes
x <- sin(spokes)
y <- cos(spokes)
def <- data.frame(spokes, x, y)

xs <- c()
ys <- c()
for(i in 1:nrow(def)) {
  new_x <- seq(0, def$x[i], length.out = 6)
  xs <- c(xs, new_x)
  new_y <- seq(0, def$y[i], length.out = 6)
  ys <- c(ys, new_y)
}
xs <- c(xs, x)
ys <- c(ys, y)
def1 <- data.frame(xs, ys)  

q <- ggplot(def1, aes(xs, ys))
q +  geom_point(size = 12, alpha = 1, fill = "darkblue", color = "red", shape = 9) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "cyan"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")
