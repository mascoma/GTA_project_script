library(plot3D)
example(persp3D)

example(surf3D)
example(slice3D)
example("scatter3D")
example("segments3D")
example(image2D)
example("image3D")
example("contour3D")
example("colkey")
example(jet.col)
example(perspbox)

example(mesh)
example("trans3D")
example(plot.plist)
example(ImageOcean)
example(Oxsat)

par(mar = c(2,2,2,2))
x <- y <- z <- seq(-1, 1, by = 0.1)
grid   <- mesh(x, y, z)
colvar <- with(grid, x*exp(-x^2 - y^2 - z^2))
slice3D  (x, y, z, colvar = colvar, theta = 60)
slicecont3D (x, y, z, ys = seq(-1, 1, by = 0.5), colvar = colvar, theta = 60, border = "black")
slice3D  (x, y, z, xs = c(-1, -0.5, 0.5), ys = c(-1, 0, 1),   zs = c(-1, 0), colvar = colvar, theta = 60, phi = 40)

t = test1[,1]
x1 = test1[,2]


 
