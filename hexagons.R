library(ggplot2)
library(dplyr)


crochet_theme <- theme(panel.grid = element_blank(), 
                       panel.background = element_rect(fill = "white"),
                       axis.text = element_blank(), 
                       axis.ticks = element_blank(), 
                       axis.title = element_blank(),
                       legend.position = "none")

##THIS ONE IS GOOD
hex_points1 <- function(n, distance = 0.5) {
  sides <- 6
  t <- distance * .75
  r <- t * (sqrt(3) / 2)
  x <- c(0)
  y <- c(0)
  rnd <- c(0)
  pos <- c(0)
  for(i in 1:n) {
    rnd <- c(rnd, rep(i, i*sides))
    pos <- c(pos, 1:(i*sides))
    for(j in 1:sides) {
      xx <- c(0, r, r, 0, -(r), -(r), 0)
      yy <- c(t, (t / 2), -(t / 2), -(t), -(t / 2), (t / 2), t)
      newx <- seq(xx[j], xx[j+1], length.out = i+1)
      newy <- seq(yy[j], yy[j+1], length.out = i+1)
      x <- c(x, newx[-1])
      y <- c(y, newy[-1])
      t <- t + distance / sides
      r <- t * (sqrt(3) / 2)
    }
  }
  stitch <- 0:(length(x)-1)
  return(data.frame(cbind(stitch, x, y, rnd, pos)))
}


##just x and y
hex_points0 <- function(n, distance = 0.5) {
  sides <- 6
  t <- distance
  r <- t * (sqrt(3) / 2)
  x <- c(0)
  y <- c(0)
  for(i in 1:n) {
    for(j in 1:sides) {
      xx <- c(0, r, r, 0, -(r), -(r), 0)
      yy <- c(t, (t / 2), -(t / 2), -(t), -(t / 2), (t / 2), t)
      newx <- seq(xx[j], xx[j+1], length.out = i+1)
      newy <- seq(yy[j], yy[j+1], length.out = i+1)
      x <- c(x, newx[-1])
      y <- c(y, newy[-1])
      t <- t + distance / sides
      r <- t * (sqrt(3) / 2)
    }
  }
  return(data.frame(cbind(x, y)))
}

newhex1 <- hex_points1(12)

newhex00 <- newhex1 %>%
  mutate(inc = ifelse(pos %% rnd == 0 & rnd > 0, 1, 0))

newhex00$rnd <- factor(newhex00$rnd)
newhex00$inc <- factor(newhex00$inc)
newhex00$pos <- factor(newhex00$pos)

cbPalette0 <- c("#4FC3F7", "#999999", "#C2185B")
cbPalette <- rep(cbPalette0, 7)
rndShapes0 <- c(19, 17, 15)
rndShapes <- rep(rndShapes0, 5)

ggplot(newhex00, aes(x = x, y = y, col = inc, shape = rnd)) + 
  geom_point(size = 6) + scale_colour_manual(values=cbPalette) +
  scale_shape_manual(values=rndShapes) +
  crochet_theme + 
  ggtitle("spiral hexagon")





#add other columns to spiral circle
spiral01 <- cbind(spiral00, rnd = newhex00$rnd, pos = newhex00$pos,
                  inc = newhex00$inc)

ggplot(spiral01, aes(x = x, y = y, shape = inc)) + 
  geom_point(size = 7) + 
  crochet_theme 

?count
count(newhex00, c("rnd", "pos"))

rnd_count <- newhex00 %>%
  count(rnd) %>%
  arrange(desc(n))
rnd_count$n


rnd_count %>%
  mutate(share = n / sum(n),
         rnd = reorder(rnd, share))
?reorder

# Creating a bar plot from `bb_count`
bb_count %>%
  slice(1:20) %>%
  mutate(share = n / sum(n),
         chord = reorder(chord, share)) %>%
  ggplot(aes(x = chord, y = share, fill = chord)) +
  geom_col() +
  coord_flip() +
  xlab("Chord") +
  ylab("Share of total chords") 



## part 2: HEXAGON BUGGALOO
##NO SPIRAL - NESTED HEXAGONS
hex_points2 <- function(n, distance = 0.5) {
  sides <- 6
  t <- 0.1
  r <- t * (sqrt(3) / 2)
  x <- c(0)
  y <- c(0)
  rnd <- c(0)
  pos <- c(0)
  hexion <- c(0)
  for(i in 1:n) {
    rnd <- c(rnd, rep(i, i*sides))
    pos <- c(pos, 1:(i*sides))
    for(j in 1:sides) {
      xx <- c(0, r, r, 0, -(r), -(r), 0)
      yy <- c(t, (t / 2), -(t / 2), -(t), -(t / 2), (t / 2), t)
      newx <- seq(xx[j], xx[j+1], length.out = i+1)
      newy <- seq(yy[j], yy[j+1], length.out = i+1)
      hexion <- c(hexion, rep(j, (i * sides / 6)))
      x <- c(x, newx[-1])
      y <- c(y, newy[-1])
    }
    t <- t + distance / sides
    r <- t * (sqrt(3) / 2)
  }
  stitch <- 0:(length(x)-1)
  return(data.frame(cbind(stitch, x, y, rnd, pos, hexion)))
}


hexes <- hex_points2(12)
newhex2 <- hexes %>%
  mutate(rnd2 = ifelse(rnd %% 2 == 0, 1, 0.8))

newhex2$rnd <- factor(newhex2$rnd)

newhex2$pos <- factor(newhex2$pos)
newhex2$hexion <- factor(newhex2$hexion)
#newhex2$rnd2 <- factor(newhex2$rnd2)

hexPalette0 <- c( "#8ffb65", "#ff55f8", "#428789")
hexPalette <- c("black", rep(hexPalette0, 2))
ggplot(newhex2, aes(x = x, y = y, col = hexion, alpha = rnd2)) + 
  geom_point(size = 9) + scale_colour_manual(values=hexPalette) +
  crochet_theme  + 
  scale_alpha(range = c(0.5, 1.0))


