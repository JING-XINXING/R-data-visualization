##########################################################################################
# Rscript funtions: Plotting 2D and 3D pie
# Date: 2019/06/23
# Author: jingxinxing
# Version: v1
##########################################################################################
#
## 设置当前的工作目录
setwd("./")

## 导入R包
anp <- c("plotrix", "openxlsx")
sapply(anp, library, character.only = T)

## 读取数据
data <- read.xlsx("Protein quantitation（小鼠脑组织）.xlsx", sheet = 1, startRow = 2, colNames = T, rowNames = F, detectDates = F)
dim(data)

## 数据处理
rownames(data) <- data$Accession
data_new <- data.frame(rownames(data), data$`Coverage.[%]`)
head(data_new)
#   rownames.data. data..Coverage.....
# 1        A3KGU7                 78
# 2        A3KGU5                 78
# 3        Q62261                 66
# 4        Q9JHU4                 47
# 5        Q5SXR6                 61
# 6        Q9QXS1                 45
colnames(data_new) <- c("Accession", "Coverage")
head(data_new)
#   Accession Coverage
# 1    A3KGU7       78
# 2    A3KGU5       78
# 3    Q62261       66
# 4    Q9JHU4       47
# 5    Q5SXR6       61
# 6    Q9QXS1       45

dim(data_new)
# [1] 3881    2
max(data_new$Coverage)
# [1] 94
min(data_new$Coverage)
# [1] 0

A <- NROW(data_new[data_new$Coverage <= 10,])
A
# [1] 1607
# length(data_new[data_new$Coverage < 10,])
# [1] 2
B <- NROW(data_new[data_new$Coverage > 10 & data_new$Coverage <= 20,])
B
# [1] 726
C <- NROW(data_new[data_new$Coverage > 20 & data_new$Coverage <= 30,])
C
# [1] 481
D <- NROW(data_new[data_new$Coverage > 30 & data_new$Coverage <= 40,])
D
# [1] 394
E <- NROW(data_new[data_new$Coverage > 40,])
E
# [1] 673

## 2D
x <- c(A, B, C, D, E)
labels <- c("0-10%", "10-20%", "20-30%", "30-40%",">40%")
pdf("pie_2d.pdf", width = 12, height = 7.5)
pie(x, labels = labels, col = rainbow(length(x)), main = "The rate of peptide coverage(%)")
dev.off()
png("pie_2d.png", width = 1200, height = 750)
pie(x, labels = labels, col = rainbow(length(x)), main = "The rate of peptide coverage(%)")
dev.off()

## 3D
pdf("pie_3d.pdf", width = 12, height = 7.5)
pie3D(x, labels = labels, explode = 0.1, main = "The rate of peptide coverage(%)")
dev.off()
png("pie_3d.png", width = 1200, height = 750)
pie3D(x, labels = labels, explode = 0.1, main = "The rate of peptide coverage(%)")
dev.off()
