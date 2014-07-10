N.out <- 400
N.frames <- 8
t.frames <- 1:N.frames
t.out <- seq(from=0, to=N.frames, length=N.out)
weight <- function(t.in, t.out, FUN) {
  ifelse(abs(t.in - t.out) > 1, 0, FUN(t.in, t.out))
}
periodic_weight <- function(t.in, t.out, period, FUN) {
  # Dirty hack to make the basis functions periodic (instead of always 0 at
  # t=0).
  pmax(weight(t.in, t.out, FUN), weight(t.in, t.out - period, FUN))
}
MyMatrix <- function(t, N) {
  cbind(
    outer(t, 1:N, function(t, i) sin(pi * i * t / N)),
    outer(t, N:1, function(t, i) cos(pi * i * t / N))) / sqrt(N)
}
InterpMatrix <- function(N.frames, t.out, FUN) {
  t.frames <- 1:N.frames
  outer(t.out, t.frames, FUN)
}
InterpLinear <- function(t.out, N.frames) {
  InterpMatrix(N.frames, t.out, 
               function(x, y) {
                 periodic_weight(x, y, N.frames,
                                 function(a, b) 1 - abs(a - b))
               })
}
InterpESG <- function(t.out, N.frames) {
  InterpMatrix(N.frames, t.out, 
               function(x, y) {
                 periodic_weight(x, y, N.frames,
                                 function(a, b) cos((a - b) * pi / 2))
               })
}
interp.linear <- InterpLinear(t.out, N.frames)
interp.esg <- InterpESG(t.out, N.frames)
interp.smooth <- MyMatrix(t.out, max(t.frames) / 2)

