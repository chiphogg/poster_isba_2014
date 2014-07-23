# Show one shortcoming of CI approach: loses "horizontal" information.

x <- seq(from=0, to=10, by=0.1)

require(ggplot2)
size <- 1
colour <- '#22aa33'
p <- (ggplot(data=data.frame(x=x, y=0, dy=1), aes(x=x))
      + geom_ribbon(aes(ymin=y - dy, ymax=y + dy), fill=colour, alpha=0.3)
      + geom_line(aes(y=y), size=size, colour=colour)
      + geom_line(aes(y=y + dy), size=size * 0.5, colour=colour)
      + geom_line(aes(y=y - dy), size=size * 0.5, colour=colour)
      + scale_y_continuous(limits=3*c(-1, 1))
      + theme(legend.position='none', text=element_text(size=20),
              axis.text=element_blank(), axis.title=element_blank(),
              axis.line=element_blank(), axis.ticks=element_blank())
      )

require(Cairo)
require(gridExtra)
width <- 1000
height <- 600
output.dir <- paste(sep='/', getwd(), 'custom')
base.name <- 'two_models'
base.title <- 'Model'
CairoPNG(filename=paste0(base.name, '.png'), width=width, height=height)
Title <- function(x) ggtitle(paste(base.title, x))
grid.arrange(p + Title('A'), p + Title('B'), ncol=2)
dev.off()

L <- function(ell, x) {
  K <- outer(x, x, function(a, b) exp(-((a - b) / ell) ^ 2))
  diag(K) <- diag(K) + 1e-6
  t(chol(K))
}
L.short <- L(1, x)
L.long <- L(10, x)

# Animate individual draws.
source('./interp.R')
N.points <- length(x)
seed.matrix <- matrix(rnorm(n=N.frames * N.points), nrow=N.frames)
N.out <- 300
dt <- N.frames / N.out
t.out <- seq(from=dt, to=N.frames, by=dt)
random.matrix <- MyMatrix(t.out, N.frames / 2) %*% seed.matrix
AddPlot <- function(y) {
  geom_line(data=data.frame(x=x, y=y), aes(x=x, y=y))
}

require(animation)
saveGIF({
  for (i in 1:length(t.out)) {
    grid.arrange(ncol=2
      , p + Title('A') + AddPlot(L.short %*% random.matrix[i, ])
      , p + Title('B') + AddPlot(L.long %*% random.matrix[i, ])
      )
  }
}, movie.name=paste(sep='_', base.name, 'draws.gif'),
outdir=output.dir,
clean=TRUE, interval=0.05, ani.dev='CairoPNG',
ani.width=width, ani.height=height)
