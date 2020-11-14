library(gridExtra)
library(grid)
library(rcartocolor)

N <- 8
N1 <- 8
N2 <- 15
d <-
  matrix(sample(seq(1, N, by = 2) ^ 2, N1 * N2, replace = T), N1, N2)

myfun <- function(label, ...) {
  i <- as.numeric(label)
  n <- sqrt(i)
  arrangeGrob(
    grobs = replicate(i, rectGrob(
      gp = gpar(
        # Carto color palettes: https://carto.com/carto-colors/
        fill = sample(carto_pal(7, "Burg"), 1),
        alpha = runif(1),
        col = 'white',
        lwd = 1
      )
    ),
    simplify = FALSE),
    widths = unit(rep(1 / n, n), "snpc"),
    heights = unit(rep(1 / n, n), "snpc")
  )
}

g <- tableGrob(d,
               theme = ttheme_minimal(core = list(fg_fun = myfun),
                                      padding = unit(c(10, 10), 'mm')))

tw <- convertWidth(sum(g$widths), 'in', valueOnly = TRUE)
th <- convertHeight(sum(g$heights), 'in', valueOnly = TRUE)
ggplot2::ggsave('table.png',
                g,
                width = tw,
                height = th,
                dpi = 300)

# Reference:
# https://gist.github.com/baptiste/ee048e288f1165699dcf131b4e9be21c
