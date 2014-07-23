SlidingVariables <- function(n_lines, n_points,
    x=seq(from=0, to=6, length.out=200), i1=round(length(x) * 0.2), i2) {
  # Plots n_lines "random functions", and a scatterplot (with n_points points)
  # of function values at x1 and x2.
  set.seed(2)
  require(gppois)
  require(ggplot2)
  N <- length(x)
  Cov <- CovarianceSE(ell=0.3 * diff(range(x)), sigma.f=1)
  K <- Cov$K.specific(X=x) + 1e-8 * diag(N)
  L <- t(chol(K))
  seeds <- matrix(rnorm(n=N*n_lines), nrow=N)
  d_lines <- data.frame(y=as.vector(L %*% seeds), x=x, i=rep(1:n_lines, each=N))
  xi <- c(x[i1], x[i2])
  d_vlines <- data.frame(x=xi)

  # Scatterpoints sampled from the lines.
  d_sampled_pts <- data.frame(i=1:n_lines
    , x1=d_lines$y[which(d_lines$x == x[i1])]
    , x2=d_lines$y[which(d_lines$x == x[i2])]
    )
  # Generate more scatterpoints with the same covariance.
  seeds.many <- matrix(rnorm(n=N * n_points), nrow=N)
  d_many_points <- data.frame(t(L %*% seeds.many)[, c(i1, i2)])
  point_base_size <- 1

  common_options <- list(scale_colour_brewer("", palette="Set3")
    , theme(legend.position='none')
    )
  p_lines <- (ggplot(data=d_lines)
    + geom_line(aes(y=y, x=x, group=i, colour=as.factor(i)), size=2)
    + geom_vline(data=d_vlines, aes(xintercept=x))
    + scale_x_continuous("", breaks=d_vlines$x, labels=c('X1', 'X2'))
    + scale_y_continuous("", limits=3*c(-1, 1))
    + common_options
    )
  p_scatter <- (ggplot(data=d_many_points, aes(x=x1, y=x2))
    + geom_point(aes(x=X1, y=X2), size=point_base_size, colour='grey70')
    + geom_point(data=d_sampled_pts
      , colour='black'
      , size=5.5 * point_base_size
      )
    + geom_point(data=d_sampled_pts
      , aes(colour=as.factor(i))
      , size=4.0 * point_base_size
      )
    + scale_x_continuous("X1", limits=3*c(-1, 1))
    + scale_y_continuous("X2", limits=3*c(-1, 1))
    + coord_fixed(ratio=1)
    + common_options
    )
  return (list(lines=p_lines, scatter=p_scatter))
}
