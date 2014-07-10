#!/usr/bin/R

# Fit some data for animations.
#
# Since it's quite slow (hours?), it only runs if this file's timestamp is
# newer than the file with the output.

library(gppois)

modified_time <- file.info("./train_models.R")$mtime

# Analyze the surface (but only if necessary).
flag_surface <- '.surface'
surface_time <- file.info(flag_surface)$mtime
rerun_surface <- (is.na(surface_time) || surface_time < modified_time)
if (rerun_surface) {
  data(steelStrain)

  d.strain <- Dataset(id="steel.strain", data=steelStrain, X.names=c("X", "Y"),
    column="exx", data.offset=0)
  d.gap <- Dataset(id="gap.points", data=steelStrainGap, X.names=c("X", "Y"),
    column="exx", data.offset=0)
  M.aniso <- Model(id="aniso")
  ell.bounds <- c(0.1, 10)
  sigma.f.relative <- c(0.1, 10)
  sigma.n.bounds <- diff(range(d.strain$dpts)) * c(1e-7, 1e-3)
  Cov.2d <- CovarianceSEAniso2D(id='signal', theta.1=0,
    ell.1.bounds=ell.bounds, ell.2.bounds=ell.bounds,
    sigma.f.bounds=sigma.f.relative * sd(d.strain$dpts))
  M.aniso$AddCovariance(Cov.2d)
  M.aniso$SetNoiseBounds(sigma.n.bounds)
  cat("Training model...");
  M.aniso$Train(d=d.strain)
  cat("done!\n");

  # Generate a grid of data points for plotting the surface.
  cat("Generating grid...");
  x.grid <- GriddedConvexHull(X=d.strain$X, spacing=0.5)
  cat("done!\n");

  # Save the file for speedy access later.
  save(x.grid, d.strain, d.gap, M.aniso, file='custom/trained.RO')

  # We just need to save *anything* here.
  xyz <- 123
  save(xyz, file=flag_surface)
}

