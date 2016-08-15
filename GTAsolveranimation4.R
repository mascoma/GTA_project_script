library(animation)
library(deSolve)
library(plot3D)
outputdir <- "/isi/olga/xin/GTA_project/output/20160807/"
c.array <-c(0.0001, 0.0015, 0.1, 0.2, 0.3, 0.4, 0.5)
K.array <- c(1e6, 1e7, seq(1e8, 1.1e8, length.out = 2), 1.2e8, 2e8, 1e9)
r.array <- c(0.0001, 0.001, 0.01, 0.1, 0.4, 0.7, 1)
N.array <- c(1e-10, 1e-9, 1.05e-9, 1.1e-9, 2e-9, 1e-8, 1e-7)

M <- mesh(c.array, K.array, r.array)


yini <- c(X1 = 1, X2 = 1)
times <- seq(0, 50000, by = 1)

GTAeq <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dX1 <- r * (1-c) * (1-(X1+X2)/K) * X1 - c * X1 + c * N* X1 * X2 # GTA postive
    dX2 <- r * (1-(X1+X2)/K) * X2 - c * N * X1 * X2     # GTA negative
    return(list(c(dX1, dX2)))
  }) 
}

saveGIF({
  ani.options(interval=.3)
  layout(matrix(c(1, 2, 3, 4, rep(5, 4)), 8, 1))
  par(mar=c(4, 4, 2, 1) + 0.1)
  for (i in 1:length(M$x)) {

    for (j in 1:length(N.array)){

      par(fg=1)

      plot(-5, xlim = c(0.0001, 1), ylim = c(0, .3), axes = F, xlab = "c", ylab = "", 
           main = "parameterspace")
      abline(v=M$x[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))

      axis(1)

      plot(-10, xlim = c(1e5, 1e9), ylim = c(0, .3), axes = F, xlab = "K", ylab = "")
      abline(v=M$y[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))

      axis(1)

      plot(-10, xlim = c(0.0001, 1), ylim = c(0, .3), axes = F, xlab = "r", ylab = "")
      abline(v=M$z[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))

      axis(1)

      plot(-10, xlim = c(1e-10, 1e-6), ylim = c(0, .3), axes = F, xlab = "N", ylab = "")
      abline(v=N.array[j], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))

      axis(1)
      
      pars <- c(r = M$z[i], # growth rate
                K = M$y[i], # carry capacity
                c = M$x[i], # cost
                N = N.array[j]) # N = b*i*ro*mu
      
      out <- ode(yini, times, GTAeq, pars, method = "ode45")
      matplot(out[ , 1], out[ , 2:3], type = "l", xlab = "time", xlim = c(0, 55000),
              ylab = "population size", main = " ", lwd = 2)
      legend("topright", c("X+", "X-"), col = 1:2, lty = 1:2)
      print(i)
    }
  }
}, outpath = outputdir, movie.name = "animation-cKrN.gif")
