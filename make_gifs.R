#!/usr/bin/R

# Fit some data and generate some animations.
#
# The first run should take a long time (maybe hours), because it needs to train
# some models.  Subsequent runs should be faster.

source('./train_models.R')
load('custom/trained.RO')
cat("Models done training.  Constructing animations...")

library(rgl)

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
  for (i in 1:N.frames) {
    d.strain$Plot2D(dist.factor=0.15, max.points=Inf, Y.scale=500, clear=TRUE)
    d.gap$Plot2D(dist.factor=0.15, max.points=Inf, Y.scale=500, clear=FALSE,
                 color='blue')
    PlotSurface(X=x.grid, Y=time.mat[, i])
    rgl.snapshot(filename=sprintf('%s/steel_strain_%04d.png', anim.dir, i),
      top=TRUE)
  }
  rgl.close()
}

# Get the times straight.
N.frames <- 100
N.keyframes <- 8
N.smoothseeds <- N.keyframes / 2
total.time <- N.keyframes
dt <- total.time / N.frames
t.out <- seq(from=dt, to=total.time, by=dt)

# Construct the matrices.
N.points <- ncol(L.strain)
seed.matrix <- matrix(rnorm(n=N.keyframes * N.points), nrow=N.keyframes)

# Construct the animations.
SteelStrainAnimation(
    t(InterpESG(t.out=t.out, N.frames=N.keyframes) %*% seed.matrix), 'esg')
SteelStrainAnimation(
    t(MyMatrix(t=t.out, N=N.smoothseeds) %*% seed.matrix), 'smooth')

cat("done!\n")
