library(xml2)
library(rvest)

#Import data web ke R
alamatweb = 'https://www.transfermarkt.co.id/spieler-statistik/wertvollstespieler/marktwertetop?land_id=0&ausrichtung=alle&spielerposition_id=alle&altersklasse=alle&jahrgang=0&kontinent_id=0&plus=1'
lamanweb = read_html(alamatweb)
lamanweb

# Mengambil informasi 
namapemain_data <- html_nodes(lamanweb,'.inline-table a')
nama_pemain <- html_text(namapemain_data)
nama_pemain
nama_pemain <- nama_pemain[nama_pemain!=""]
nama_pemain

posisipemain_data <- html_nodes(lamanweb,'.inline-table tr+ tr td')
posisi_pemain <- html_text(posisipemain_data)
posisi_pemain

umurpemain_data <- html_nodes(lamanweb,'td:nth-child(3)')
umur_pemain <- html_text(umurpemain_data)
umur_pemain
umur_pemain <- as.numeric(umur_pemain)
umur_pemain

hargapasaran_data <- html_nodes(lamanweb,'.rechts.hauptlink')
harga_pasaran <- html_text(hargapasaran_data)
harga_pasaran
harga_pasaran <- substring(harga_pasaran,3,7) #Menghilangkan Rp dan Mlyr
harga_pasaran <- as.numeric(harga_pasaran)
harga_pasaran <- harga_pasaran*1000
harga_pasaran

banyakpertandingan_data <- html_nodes(lamanweb,'.hauptlink+ .zentriert')
banyak_pertandingan <- html_text(banyakpertandingan_data)
banyak_pertandingan <- as.numeric(banyak_pertandingan)
banyak_pertandingan

gol=lamanweb %>% html_nodes('td:nth-child(8)') %>% html_text() %>% as.numeric()
gol

umpan_gol=lamanweb %>% html_nodes('td:nth-child(10)') %>% html_text() %>% as.numeric()
umpan_gol

kartu_kuning=lamanweb %>% html_nodes('td:nth-child(11)') %>% html_text() %>% as.numeric()
kartu_kuning


# Informasi yang telah didapat dikumpulkan ke dalam dataset
dataset_pemain_bernilai <-data.frame(Nama_Pemain = nama_pemain, Posisi = posisi_pemain, Umur = umur_pemain,
                                Harga_Pasaran_M= harga_pasaran, Pertandingan=banyak_pertandingan,total_Gol=gol,
                                Total_Umpan_Gol=umpan_gol,total_KartuKuning=kartu_kuning)
head(dataset_pemain_bernilai)

# menampilkan informasi tipe data dari dataset
str(dataset_pemain_bernilai) #menampilkan jumlah observasi dan variabel pada data frame yang telah kita buat

# Memastikan tidak ada missing value
colSums(is.na(dataset_pemain_bernilai))

# Mencari tahu ada tidaknya outlier
boxplot(dataset_pemain_bernilai$Umur, main='Boxplot variabel Umur',
        col='white',border='darkblue')
boxplot(dataset_pemain_bernilai$Harga_Pasaran_M, main='Boxplot variabel Harga Pasaran Pemain(M)',
        col='lightgrey', border='blue')
boxplot(dataset_pemain_bernilai$Pertandingan, main='Boxplot variabel Jumlah Pertandingan', 
        col='white',border='darkred')
boxplot(dataset_pemain_bernilai$total_Gol, main='Boxplot variabel Total Goal',
        col='lightgrey',border='red')
boxplot(dataset_pemain_bernilai$Total_Umpan_Gol, main='Boxplot variabel Total Umpan Goal',
        col='white',border='darkgreen')
boxplot(dataset_pemain_bernilai$total_KartuKuning, main='Boxplot variabel Total Kartu Kuning',
        col='lightgrey',border='orange')


# Analisis deskriptif
library(summarytools)
descr(dataset_pemain_bernilai)

#Modus dari data
modus<-function(x){
  uniqx<-unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}
modus(dataset_pemain_bernilai$Posisi)
modus(dataset_pemain_bernilai$Umur)
modus(dataset_pemain_bernilai$Harga_Pasaran_M)
modus(dataset_pemain_bernilai$Pertandingan)
modus(dataset_pemain_bernilai$total_Gol)
modus(dataset_pemain_bernilai$Total_Umpan_Gol)
modus(dataset_pemain_bernilai$total_KartuKuning)

# Mengetahui jumlah jenis variabel bertipe karakter (posisi)
library(dplyr)
count(dataset_pemain_bernilai, Posisi)
# mencari jumlah pemain dengan umur kurang dari rata rata
count(dataset_pemain_bernilai, Umur<mean(Umur))
# mencari jumlah pemain dengan harga pasaran > harga pasaran rata rata
count(dataset_pemain_bernilai, Harga_Pasaran_M>mean(Harga_Pasaran_M))


# Mencari angka korelasi antar variabel dalam  data
# Hubungan variabel harga pasaran pemain dengan umur pemain
cor.test(dataset_pemain_bernilai$Harga_Pasaran_M,dataset_pemain_bernilai$Umur,method="pearson")
#Hubungan variabel harga pasaran pemain dengan banyaknya pertandingan yang diikuti
cor.test(dataset_pemain_bernilai$Harga_Pasaran_M, dataset_pemain_bernilai$Pertandingan, method="pearson")
#HUbungan variabel harga pasaran oemain dengan variabel total_Gol (perolehan gol pemain)
cor.test(dataset_pemain_bernilai$Harga_Pasaran_M,dataset_pemain_bernilai$total_Gol,method="pearson")
# Hubungan variabel harga pasaran pemain dengan variabel total_umpan_gol
cor.test(dataset_pemain_bernilai$Harga_Pasaran_M,dataset_pemain_bernilai$Total_Umpan_Gol,method="pearson")
# Hubungan variabel harga pasaran pemain dengan variabel total perolehan kartu kuning
cor.test(dataset_pemain_bernilai$Harga_Pasaran_M,dataset_pemain_bernilai$total_KartuKuning,method="pearson")

#Visualisasi data
library(ggplot2)
ggplot(dataset_pemain_bernilai,aes(x=Posisi,y=Umur))+geom_point()
ggplot(dataset_pemain_bernilai,aes(x=Pertandingan,y=total_KartuKuning))+geom_smooth()+geom_point()
ggplot(dataset_pemain_bernilai,aes(x=total_Gol,y=total_KartuKuning))+geom_point()
ggplot(dataset_pemain_bernilai,aes(x=Pertandingan, y=Harga_Pasaran_M))+geom_point()
ggplot(dataset_pemain_bernilai,aes(x=Harga_Pasaran_M,y=Total_Umpan_Gol))+geom_point()
library(lessR)
plot(dataset_pemain_bernilai$Harga_Pasaran_M,dataset_pemain_bernilai$total_Gol,
     main='Plot Hubungan Harga Pasaran Pemain dengan Total gol',
     xlab='harga pasaran pemain(M)',ylab='total gol',
     col='darkred')
BarChart(Harga_Pasaran_M,data=dataset_pemain_bernilai, by=Posisi)
PieChart(Posisi, data=dataset_pemain_bernilai)
