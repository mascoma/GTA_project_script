library(ggplot2)
# GTA selfish model 

b <- c(50, 100, 200) # various b
c <- c(0.0015, 0.01, 0.03) # various c
ro <- c(0.00001, 0.0001) # various ro
mu.b <- c(2.3e-5, 1.1e-5, 7e-6)
mu.c <- c(1.1e-5, 1.1e-5, 1.3e-5)
mu.ro <- c(0.000107, 1.1e-5)
mu.b.df <- as.data.frame(cbind(mu.b, b))
g <- ggplot(mu.b.df, aes(x = b, y = mu.b)) + 
  geom_point(size = 3, shape = 21, fill = "lightpink3") +
  geom_line() + ylab("mu") + xlab("b") + 
  ggtitle("Minimum mu changes given the various b")
g
ggsave(plot = g, file = "/Users/Xin/Desktop/projects/GTA_project/output/20160407/mu_b.jpeg")

mu.c.df <- as.data.frame(cbind(mu.c, c))
g <- ggplot(mu.c.df, aes(x = c, y = mu.c)) + 
  geom_point(size = 3, shape = 21, fill = "deepskyblue2") +
  geom_line() + ylab("mu") + xlab("c") + 
  ggtitle("Minimum mu changes given the various c")
g
ggsave(plot = g, file = "/Users/Xin/Desktop/projects/GTA_project/output/20160407/mu_c.jpeg")

mu.ro.df <- as.data.frame(cbind(mu.ro, ro))
g <- ggplot(mu.ro.df, aes(x = ro, y = mu.ro)) + 
  geom_point(size = 3, shape = 21, fill = "darkseagreen") +
  geom_line() + ylab("mu") + xlab("ro") + 
  ggtitle("Minimum mu changes given the various ro")
g
ggsave(plot = g, file = "/Users/Xin/Desktop/projects/GTA_project/output/20160407/mu_ro.jpeg")


