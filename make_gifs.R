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
width <- 1000  # pixels
height <- 600  # pixels
n.frames <- 200
time.mat <- (L.strain %*% BubblingRandomMatrix(
    n.pts=nrow(x.grid), N=N.frames, n.times=n.frames))
time.mat <- time.mat + as.vector(m.post)

# Construct the animations.
anim.dir <- 'custom/steel_strain/plain'
dir.create(anim.dir, recursive=TRUE)
open3d()
par3d(windowRect = 50 + c(0, 0, width, height))
view3d(theta=45, phi=25, zoom=0.4)
for (i in 1:n.frames) {
  d.strain$Plot2D(dist.factor=0.15, max.points=Inf, Y.scale=500, clear=TRUE)
  d.gap$Plot2D(dist.factor=0.15, max.points=Inf, Y.scale=500, clear=FALSE,
               color='blue')
  PlotSurface(X=x.grid, Y=time.mat[, i])
  rgl.snapshot(filename=sprintf('%s/steel_strain_%04d.png', anim.dir, i),
    top=TRUE)
}
rgl.close()

cat("done!\n")
