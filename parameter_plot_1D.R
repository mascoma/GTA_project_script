c.array <- c(0.0001, 0.0002, 0.0005, 0.0008, 0.001, 
             seq(0.0015, 0.1, length.out = 36), 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
             0.8, 0.9, 1)

png("/Users/Xin/Desktop/projects/GTA_project/output/20160722/crange.png", 
    width = 1000, height = 200)
par(mar=c(4, 4, 1, 1))
plot(c.array, seq(1, 50, by = 1), xlab = "c", ylab = "index", pch = 20)
dev.off()

K.array <- c(1e5, 1e6, 1e7, 2e7, 5e7, 8e7, seq(1e8, 1e9, length.out = 44))
png("/Users/Xin/Desktop/projects/GTA_project/output/20160722/Krange.png", 
    width = 1000, height = 200)
par(mar=c(4, 4, 1, 1))
plot(K.array, seq(1, 50, by = 1), xlab = "K", ylab = "index", pch = 20)
dev.off()

r.array <- c(seq(0.0001, 0.03, length.out = 26), 0.05, 0.06, 0.08, 0.09,
             seq(0.1, 1, length.out = 20))
png("/Users/Xin/Desktop/projects/GTA_project/output/20160722/rrange.png", 
    width = 1000, height = 200)
par(mar=c(4, 4, 1, 1))
plot(r.array, seq(1, 50, by = 1), xlab = "r", ylab = "index", pch = 20)
dev.off()

N.array <- c(1e-10, 5e-10, seq(1e-9, 1.5e-9, length.out = 42), 
             2e-9, 5e-9, 8e-9, 1e-8, 1e-7)
png("/Users/Xin/Desktop/projects/GTA_project/output/20160722/Nrange.png", 
    width = 1000, height = 200)
par(mar=c(4, 4, 1, 1))
plot(N.array, seq(1, 49, by = 1), xlab = "N", ylab = "index", pch = 20)
dev.off()

