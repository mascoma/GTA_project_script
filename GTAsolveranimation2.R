library(animation)
library(deSolve)
library(plot3D)
#Set delay between frames when replaying
ani.options(interval=.05)
# one parameter space
#c.array <- c(seq(0.0001, 1, by = 0.001), 1)
#K.array <- c(1e5, 1e6, 1e7, seq(1e8, 2e8, by = 3e6), 3e8, 4e8, 5e8, 6e8, 7e8, 8e8, 9e8, 1e9)
#r.array <- c(seq(0.0001, 0.03, by = 0.001), 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)
#N.array <- c(1e-10, 5e-10, seq(1e-9, 1.1e-9, by = 0.00000000001), 1.5e-9, 1e-8, 1e-7, 1e-6)
#M <- N.array

# two parameters 
#c.array <-c(0.0001, seq(0.0015,0.09, length.out = 9),0.1, 0.2, 0.3, 0.4, 0.5, 
#            0.6, 0.7, 0.8, 0.9, 1)
#K.array <- c(1e5, 1e6, 1e7, seq(1e8, 2e8, length.out = 16), 1e9)
#r.array <- seq(0.0001, 1, length.out = 20)
#N.array <- c(1e-10, 5e-10, seq(1e-9, 1.1e-9, by = 0.00000000001), 1.2e-9, 1.5e-9, 
#             2e-9, 5e-9, 1e-8, 1e-7, 1e-6)

# three parameters
c.array <-c(0.0001, 0.0015, 0.1, 0.2, 0.3, 0.4, 0.5, 
            0.6, 0.7, 0.8, 0.9, 1)
K.array <- c(1e5, 1e6, 1e7, seq(1e8, 1.1e8, length.out = 5),1.2e8, 1.5e8, 2e8, 1e9)
r.array <- seq(0.0001, 1, length.out = 12)
N.array <- c(1e-10, 5e-10, 1e-9, 1.05e-9, 1.1e-9, 1.2e-9, 1.5e-9, 
             2e-9, 5e-9, 1e-8, 1e-7, 1e-6)

M <- mesh(K.array, r.array, N.array)


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
  
  # For the most part, it’s safest to start with graphical settings in
  # the animation loop, as the loop adds a layer of complexity to
  # manipulating the graphs. For example, the layout specification needs to
  # be within animation loop to work properly.
  layout(matrix(c(1,2, 3, rep(4, 3)), 6, 1))
  
  # Adjust the margins a little
  par(mar=c(4,4,2,1) + 0.1)
  
  # Begin the loop that creates the 150 individual graphs
  for (i in 1:length(M$x)) {
    # Reset the color of the top chart every time (so that it doesn’t change as the
    # bottom chart changes)
    par(fg=1)
    
    # Set up the top chart that keeps track of the current frame/iteration
    # Dress it up a little just for fun
    #plot(-5, xlim = c(0.0001, 1), ylim = c(0, .3), axes = F, xlab = "c", ylab = "", 
    #     main = "parameterspace")
    #abline(v=M$x[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    #abline(v=M[i]-0.1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    #abline(v=M[i]-0.2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    
    # Bring back the X axis
    #axis(1)
    
    # Set up the top chart that keeps track of the current frame/iteration
    # Dress it up a little just for fun
    plot(-10, xlim = c(1e5, 1e9), ylim = c(0, .3), axes = F, xlab = "K", ylab = "", 
         main = "parameterspace")
    abline(v=M$x[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    #abline(v=M[i]-0.1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    #abline(v=M[i]-0.2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    
    # Bring back the X axis
    axis(1)
    
    # Set up the top chart that keeps track of the current frame/iteration
    # Dress it up a little just for fun
    plot(-10, xlim = c(0.0001, 1), ylim = c(0, .3), axes = F, xlab = "r", ylab = "")
    abline(v=M$y[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    #abline(v=M[i]-0.1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    #abline(v=M[i]-0.2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    
    # Bring back the X axis
    axis(1)
    
    # Set up the top chart that keeps track of the current frame/iteration
    # Dress it up a little just for fun
    plot(-10, xlim = c(1e-10, 1e-6), ylim = c(0, .3), axes = F, xlab = "N", ylab = "")
    abline(v=M$z[i], lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    #abline(v=M[i]-0.1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    #abline(v=M[i]-0.2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    
    # Bring back the X axis
    axis(1)
    
    # c-K explore 
    
    pars <- c(r = M$y[i], # growth rate
              K = M$x[i], # carry capacity
              c = 0.0015, # cost
              N = M$z[i]) # N = b*i*ro*mu
    
    out <- ode(yini, times, GTAeq, pars, method = "ode45")
    matplot(out[ , 1], out[ , 2:3], type = "l", xlab = "time", xlim = c(0, 53000),
            ylab = "population size", main = " ", lwd = 2)
    legend("topright", c("X+", "X-"), col = 1:2, lty = 1:2)
    print(i)
  }
}, movie.name = "animation-KrN.gif")