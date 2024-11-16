#===UJIAN TENGAH SEMESTER 2024/2025 ANALISIS DATA===
# Nama: NADIVA FREYA DANELLA
# NIM: 22106010026
# Mata Kuliah / Kelas: UTS Analisis Data / A
# MATEMATIKA SEMESTER 5


#==Soal 2==
#==2b==
library(readr)
getwd()
setwd("D:/Dokumen/prak AnDat")

KinerjaSiswa <- read.csv("Kinerja Siswa.csv", sep=";", header=TRUE )
KinerjaSiswa

y=KinerjaSiswa$math.score
x=KinerjaSiswa$gender

#==2c==
nrow(KinerjaSiswa)
ncol(KinerjaSiswa)
str(KinerjaSiswa)

#==2d==
head(KinerjaSiswa)
tail(KinerjaSiswa)

#==2e==
head(table(KinerjaSiswa$gender))
head(table(KinerjaSiswa$math.score))

library(dplyr)
select(KinerjaSiswa, gender) %>% unique %>% nrow
select(KinerjaSiswa, math.score) %>% unique %>% nrow
unique(KinerjaSiswa$gender)
unique(KinerjaSiswa$math.score)

#==2f==
summary(KinerjaSiswa$gender)
summary(KinerjaSiswa$math.score)

quantile(KinerjaSiswa$math.score, seq(0, 1, 0.1))

#==2g==
par(las = 2, mar = c(10, 4, 2, 2), cex.axis = 0.8)
boxplot(math.score ~ gender, KinerjaSiswa, range = 0, ylab = "Nilai matematika", col = "pink")
library(ggplot2)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
ggplot(KinerjaSiswa, aes(x = gender, y = math.score)) + geom_boxplot()

#==2h==
aggregate(math.score ~ gender, data = KinerjaSiswa, mean)

#==2i==
t.test(math.score ~ gender, data = KinerjaSiswa)

#==Soal 3a==
library(dplyr)
model_exspektasi <- KinerjaSiswa %>% group_by(gender) %>% summarise(mean_math_score = mean(math.score))
model_exspektasi

#==Soal 3b==
#membuat histogram nilai matematika
hist(KinerjaSiswa$math.score, breaks = 10, main = "Histogram nilai matematika",
     xlab = "Nilai matematika", col = "orange", freq = FALSE)
#menambahkan kurva distribusi normal
curve(dnorm(x, mean = mean(KinerjaSiswa$math.score), sd=sd(KinerjaSiswa$math.score)), add = TRUE, col = "blue")

#Cara Lainnya
# Membuat histogram nilai matematika
ggplot(KinerjaSiswa, aes(x = math.score, fill = gender)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  labs(title = "Histogram Nilai Matematika Berdasarkan Jenis Kelamin",
       x = "Nilai Matematika",
       y = "Frekuensi") + theme_minimal()
# Membandingkan dengan histogram distribusi normal
normal_data <- data.frame(score = rnorm(100, mean = mean(KinerjaSiswa$math.score),
                                        sd = sd(KinerjaSiswa$math.score)))
ggplot(normal_data, aes(x = score)) +
  geom_histogram(binwidth = 5, fill = "purple", alpha = 0.6) +
  labs(title = "Histogram Data yang Berdistribusi Normal",
       x = "Nilai", y = "Frekuensi") +theme_minimal()

#==Soal 3c I==
# Uji t untuk menentukan signifikansi perbedaan antara kelompok
t_test <- t.test(math.score ~ gender, data = KinerjaSiswa)
print(t_test)

# Menentukan apakah perlu memperbaiki model ekspektasi
if(t_test$p.value < 0.05) {cat("Ya, perlu memperbaiki model ekspektasi karena perbedaan signifikan.")}else {cat("Tidak, model ekspektasi tidak perlu diperbaiki karena perbedaan tidak signifikan.")}

#==Soal 3c II==
#model regresi yang lebih kompleks
kompleks_model <- lm(math.score ~ gender + I(gender == "Laki-Laki"), data = KinerjaSiswa)
summary(kompleks_model)

