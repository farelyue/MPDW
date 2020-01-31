#RESPONSI 2 MPDW

##-----------------------MA-----------------------##

#[1. Single Moving Average]

#Data MA
value <- c(10,9,8,7,3,1,
           2,0,1,5,12,14)

#Membuat Fungsi SMA
sma <- function(value, s1){
  ma_val <- c()
  for (i in 1:length(value)){
    if (i < s1) {
      ma_val[i] <- NA
    } else{
      ma_val[i] <- mean(value[(i-s1+1):i], na.rm = TRUE)
    }
  }
  return(ma_val)
}

#Membuat Fungsi Forecast SMA untuk periode t ke depan
forecast_ma <- function(sma, t = 1){
  val <- sma[length(sma)]
  return(val)
}

#Membuat Fungsi Akurasi Peramalan
#val1 = Nilai Aktual, val2 = Nilai Hasil Forecast
akurasi <- function(val1, val2){
  sse <- sum((val1 - val2)^2, na.rm = TRUE)
  mse <- mean((val1 - val2)^2, na.rm = TRUE)
  mad <- mean(abs(val1 - val2), na.rm = TRUE)
  mape <- mean((abs(val1 - val2)/val1)*100, na.rm = TRUE)
  
  df <- data.frame(SSE = sse, MSE = mse, MAD = mad, MAPE = mape)
  return(df)
}

#Memanggil Fungsi SMA dengan Orde1 = 3 yang disimpan dalam objek ma_Val
ma_val <- sma(value, s1 = 3)

#Membuat vektor hasil forecast yang disimpan dalam objek ft
ft <- c(NA, ma_val[1:(length(ma_val)-1)])

#Membuat Data Frame sMA, 
#Ket : kolom Nilai Aktual (Nilai), Hasil Smoothing (MA), dan Hasil Forecast (Ft)
df_ma <- data.frame(Nilai = value, MA = ma_val, ft = ft)

#Ubah bentuk ke objek timeseries
value_ma <- ts(df_ma$Nilai) 
smoothing_ma <- ts(df_ma$MA)
ft_ma <- ts(df_ma$ft)

#Membuat plot timeseries Nilai Aktual
#Ket : col = warna, main = judul, xlab = nama sumbu-x, ylab = nama sumbu-y
plot(value_ma,
     col = "navyblue",
     main = "MA(3)",
     xlab = "Bulan",
     ylab = "Nilai")

#Menambahkan plot timeseries Nilai Smoothing
lines(smoothing_ma, col = "red")

#Menambahkan plot timeseries Nilai Forecasting
lines(ft_ma, col = "green")

#Menambahkan legend
#Ket : posisi, penamaan garis, bty(boxtype), lwd(line width), col = warna
legend("topleft", 
       c("Smoothing","Actual","Forecast"), 
       bty = "o",
       lwd = 1,
       col = c("red", "blue", "green"))

#Menghitung hasil forecast 2 periode ke depan
forecast_ma(df_ma$MA, t = 2)

#Menghitung akurasi peramalan
akurasi(df_ma$Nilai, df_ma$ft)

##-----------------------DMA-----------------------##

#[2. Double Moving Average]

#Data DMA
value_1 <- c(34,36,38,40,42,
             44,46,48,50,52)

#Membuat Fungsi DMA
dma <- function(value, s1, s2){
  ma_1 <- sma(value, s1) #Melakukan SMA untuk orde 1
  res <- sma(ma_1[s1:length(value)], s2) #Melakukan SMA untuk orde 2
  ma_2 <- c(rep(NA, s2 - 1), res) #Menambahkan NA
  at <- 2*ma_1 - ma_2 #Menghitung nilai at 
  bt <- (2/(s2-1)) * (ma_1 - ma_2) #Menghitung nilai bt
  ft <- at + bt #Menghitung nilai forecast
  df <- data.frame(Nilai = value, #Membuat data frame
                   MA_1 = ma_1, 
                   MA_2 = ma_2,
                   at = at,
                   bt = bt,
                   ft = ft)
  return(df)
}


#Membuat fungsi forecast DMA untuk t periode ke depan
forecast_dma <- function(dma, t = 1){
  a <- dma[nrow(dma),4] #Mengambil baris terakhir, kolom ke empat
  b <- dma[nrow(dma),5] #Mengambil baris terakhir, kolom ke lima
  val <- a+b*t #Menghitung nilai forecast untuk t periode ke depan
  return(val)
}

#Memanggil fungsi DMA dengan orde1 = 3, orde2 = 3
df_dma <- dma(value_1, 3, 3)

#Mengubah ke bentuk objek timeseries
value_dma <- ts(df_dma$Nilai)
smoothing_dma <- ts(df_dma$MA_2)
ft_dma <- ts(df_dma$ft)

#Membuat Plot timeseries
#Penjelasan sama kyak MA
plot(value_dma,
     col = "navyblue",
     main = "DMA(3,3)",
     xlab = "Bulan",
     ylab = "Nilai")

lines(smoothing_dma, col = "red")

lines(ft_dma, col = "green")

legend("topleft", 
       c("Actual", "Smoothing", "Forecast"), 
       bty = "o",
       lwd = 1,
       col = c("red", "blue", "green"))

#Menghitung hasil forecast untuk 3 periode ke depan
forecast_dma(df_dma, t = 3)
