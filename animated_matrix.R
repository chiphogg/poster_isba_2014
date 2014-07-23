set.seed(5)
n.frames <- 300  # Number of animation frames in final slide
n.reduce <- 35  # Number of points
x.reduce <- seq(from=0, to=5, length.out=n.reduce)
small.noise <- 1e-6
#cov <- CovarianceSE(ell=1, sigma.f=1)
#K <- cov$K.specific(X=x.reduce)
K <- outer(x.reduce, x.reduce, function(a, b) dnorm(a - b))
L <- t(chol(K + (small.noise ^ 2) * diag(n.reduce)))
y.noisy.animate <- BubblingRandomMatrix(n.pts=n.reduce, N=8, n.times=n.frames)
y.smooth.animate <- L %*% y.noisy.animate
title.smooth <- 'f: Smooth function'
title.L <- 'L: Cholesky(Cov)'
title.noisy <- 'n: noisy points'
library(animation)
library(ggplot2)
this.theme <- theme(legend.position='none', text=element_text(size=20),
  axis.text=element_blank(), axis.title=element_blank(),
  axis.line=element_blank(), axis.ticks=element_blank())

PointVectorPlot <- function(y, title='') {
  (ggplot(data=data.frame(x=x.reduce, y=y), aes(x=x, y=y))
       + geom_point()
       + this.theme
       + ggtitle(title)
       + scale_y_continuous(limits=3*c(-1, 1))
       )
}

# The plot objects for the i'th frame.
AnimatedMatrixCells <- function(i) {
  require(reshape2)
  list(smooth = PointVectorPlot(y.smooth.animate[, i], title.smooth),
       L = (ggplot(data=melt(L), aes(x=Var2, y=Var1, fill=value))
            + geom_raster()
            + scale_y_reverse()
            + scale_fill_gradient2()
            + this.theme
            + ggtitle(title.L)
            ),
       noisy = PointVectorPlot(y.noisy.animate[, i], title.noisy))
}
