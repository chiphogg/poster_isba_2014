#!/usr/bin/R

# Fit some data and generate some animations.
#
# The first run should take a long time (maybe hours), because it needs to train
# some models.  Subsequent runs should be faster.

source('./train_models.R')
load('custom/trained.RO')
cat("Models done training.  Constructing animations...")

library(rgl)
library(Cairo)
library(gridExtra)

# Initial setup.
source('./interp.R')
M.aniso$SetNoiseBounds(sigma.vals=2e-6)
m.post <- M.aniso$PosteriorMean(d=d.strain, X.out=x.grid)
L.strain <- M.aniso$L(d=d.strain, X.out=x.grid)

# Function to create a single steel strain animation.
SteelStrainAnimation <- function(random.matrix, label) {
  width <- 1000  # pixels
  height <- 600  # pixels
  anim.dir <- sprintf('custom/steel_strain/%s', label)
  dir.create(anim.dir, recursive=TRUE)

  time.mat <- (L.strain %*% random.matrix) + as.vector(m.post)
  open3d()
  par3d(windowRect = 50 + c(0, 0, width, height))
  view3d(theta=45, phi=25, zoom=0.4)
  N.frames <- ncol(random.matrix)
  PlotFrame <- function(i, show.true) {
    d.strain$Plot2D(dist.factor=0.15, max.points=Inf, Y.scale=500, clear=TRUE)
    if (show.true) {
      d.gap$Plot2D(dist.factor=0.15, max.points=Inf, Y.scale=500, clear=FALSE,
                   color='blue')
    }
    PlotSurface(X=x.grid, Y=time.mat[, i])
    tag <- ifelse(show.true, '_true', '')
    filename <- sprintf('%s/steel_strain%s_%04d.png', anim.dir, tag, i)
    rgl.snapshot(filename=filename, top=TRUE)
  }
  for (i in 1:N.frames) {
    PlotFrame(i, TRUE)
    PlotFrame(i, FALSE)
  }
  rgl.close()
}

# Get the times straight.
N.frames <- 200
N.keyframes <- 10
N.smoothseeds <- N.keyframes / 2
total.time <- N.keyframes
dt <- total.time / N.frames
t.out <- seq(from=dt, to=total.time, by=dt)

# Seeds for "basis function" type matrices.
N.points <- ncol(L.strain)
seed.matrix <- matrix(rnorm(n=N.keyframes * N.points), nrow=N.keyframes)

# Arbitrary GP matrices.
RandomMatrix <- function(N.points, t, FUN) {
  N.frames <- length(t)
  K <- outer(t, t, FUN)
  L.t <- chol(K + diag(1e-8, nrow=N.frames))
  matrix(nrow=N.points, rnorm(n=N.points * N.frames)) %*% L.t
}
Brownian <- function(sigma) function(x, y) exp(-abs(x - y) / sigma)
SE <- function(sigma) function(x, y) exp(-((x - y) / sigma) ^ 2)

## Construct the animations.
#SteelStrainAnimation(
#    t(InterpESG(t.out=t.out, N.frames=N.keyframes) %*% seed.matrix), 'esg')
#SteelStrainAnimation(
#    t(MyMatrix(t=t.out, N=N.smoothseeds) %*% seed.matrix), 'smooth')
#SteelStrainAnimation(RandomMatrix(N.points, t.out, Brownian(0.5)), 'brownie')
#SteelStrainAnimation(RandomMatrix(N.points, t.out, SE(0.5)), 'se')

# Common output directory for 2D figures.
anim.gif.dir <- paste(sep='/', getwd(), 'custom')

## Sliding Covariance figure.
#source('./sliding_figure.R')
#set.seed(1)
#library(animation)
#n_frames <- 50
#N <- 200  # Number of points.
#X <- seq(from=0, to=1, length.out=N)
#di_min <- N * 0.05
#offset <- floor(0.5 * di_min)
#distance <- ((0.5 * (N - di_min) - 1)
#  * sin(pi * seq(from=1 / n_frames, to=1, length.out=n_frames)) ^ 2)
#i_mid <- round(N / 2)
#i <- data.frame(i1=i_mid - offset - distance, i2=i_mid + offset + distance)
#saveGIF({
#  for (a in 1:nrow(i)) {
#    sv <- SlidingVariables(n_lines=12, n_points=1000, x=X, i1=i$i1[a], i2=i$i2[a])
#    grid.arrange(ncol=2
#      , sv$lines
#      , sv$scatter
#      )
#  }
#}, movie.name='slide_and_scatter.gif',
#outdir=anim.gif.dir,
#clean=TRUE, interval=0.1, ani.dev='CairoPNG',
#ani.width=150 * 6.5, ani.height=150 * 3.5)

# Animated matrix multiplication figure.
source('./animated_matrix.R')
theme_set(theme_classic(40))
saveGIF({
  for (i in 1:n.frames) {
    plots <- AnimatedMatrixCells(i)
    grid.arrange(ncol=3
      , plots$smooth
      , plots$L
      , plots$noisy
      )
  }
}, movie.name='animated_matrix.gif',
outdir=anim.gif.dir,
clean=TRUE, interval=0.05, ani.dev='CairoPNG',
ani.width=150 * 9, ani.height=150 * 3)

cat("done!\n")
