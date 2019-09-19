#install.packages('qrcode')
library(qrcode)

load("students_example.Rda")

mat_num <- students$matrnumber 
dir.create("qr", showWarnings = F)
before <- Sys.time()
for (i in 1:length(mat_num)) {
jpeg(file = paste('qr/', mat_num[i], '.jpeg', sep = ''), width = 500, height = 500)
qrcode_gen(paste(mat_num[i]))
dev.off()
}
after <- Sys.time()

after - before
