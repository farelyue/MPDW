#RESPONSI 1 MPDW

##-----------------------TS Plot-----------------------##

#[1.Time Series Plot Data]

#1. Produksi Farmasi (Stasioner)
farmasi <- read.csv("E:/farelyue/IPB University/Asprak/MPDW/1920/CSV/Produk Farmasi.csv", sep = ";")
farmasi_ts <- ts(farmasi$Penjualan, start = 1)

str(farmasi_ts)
class(farmasi_ts)

plot(farmasi_ts,
     col = "navyblue",
     xlab = 'Minggu',
     ylab = 'Penjualan Produk',
     main = 'Penjualan Produk Farmasi')

#2. Produksi Beras (Trend)
beras <- read.csv("E:/farelyue/IPB University/Asprak/MPDW/1920/CSV/Produksi Beras.csv", sep = ";")
beras_ts <- ts(beras$Produksi, start = 2000)

str(beras_ts)
class(beras_ts)

plot(beras_ts,
     col = "navyblue",
     xlab = "Tahun",
     ylab = "Produksi",
     main = "Produksi Beras Nasional 2010-2015")


#3. NetAMS (Seasonal)
net <- read.csv("E:/farelyue/IPB University/Asprak/MPDW/1920/CSV/NetAMS.csv", sep = ";")
net_ts <- ts(net$Passenger, start = c(2013, 1), frequency = 12)

str(net_ts)
class(net_ts)

plot(net_ts,
     col = "navyblue",
     xlab = "Waktu",
     ylab = "Banyak Penumpang",
     main = "Penumpang AMS-AMQ 2013-2019")

##-----------------------Pembangkitan Data-----------------------##

#[2.Pembangkitan Data]

#1, Stasioner, Xt = b + et

b <- 5
et <- rnorm(n = 100, mean = 10, sd = 3)

Xt <- b + et
Xt_ts <- ts(Xt, start = 1)

plot(Xt_ts,
     col = "navyblue",
     xlab = "Waktu",
     ylab = "Nilai",
     main = "Pembangkitan Pola Data Stasioner")

#2. Trend, Xt = b0 + b1*t + et

t <- 1:100
b0 <- 0.2
b1 <- 0.3
et <- rnorm(n = 100, mean = 10, sd = 3)

Xt <- b0 + b1*t + et
Xt_ts <- ts(Xt, start = 1)

plot(Xt_ts,
     col = "navyblue",
     xlab = "Waktu",
     ylab = "Nilai",
     main = "Pembangkitan Pola Data Trend")

#3. Seasonal, Xt = b0 + b1*sin(2*phi*t/d) + b2*cos(2*phi*t/d) + et

t <- 1:100
b0 <- 22
b1 <- 16
b2 <- 17
d <- 17
et <- rnorm(n = 100, mean = 10, sd = 2)

Xt <- b0 + b1*sin((2*pi*t)/d) + b2*cos((2*pi*t)/d) + et
Xt_ts <- ts(Xt, start = 1)

plot(Xt_ts,
     col = "navyblue",
     xlab = "Waktu",
     ylab = "Nilai",
     main = "Pembangkitan Pola Data Seasonal")