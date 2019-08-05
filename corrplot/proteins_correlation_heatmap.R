###################################################################################################
# Rscript functions: Plotting Proteins of Mass spectrometry data correlation coefficents heatmap
# Date: 2019/06/25
# Author: jingxinxing
# Version: v1
###################################################################################################
#
# 用法说明：
# 本脚本绘制比较组差异蛋白的top50的相关性热图，不需要准备任何数据，只需要有“差异蛋白筛选结果.xlsx”文件和本绘图脚本在工作目录中即可
#

## 1.设置当前的工作目录
setwd("./")

## 2.导入R包
analysis_needs_packages <- c("openxlsx", "corrplot")
sapply(analysis_needs_packages, library, character.only = T)

## 3.读取数据
#############################################################################
### 3.1 sample_information
sample_info_1 <- read.xlsx("sample_information.xlsx", sheet = "样品信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
sample_info_2 <- read.xlsx("sample_information.xlsx", sheet = "比较组信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
class(sample_info_1)
class(sample_info_2)
dim(sample_info_1)
dim(sample_info_2)
sample_info_1
#   样品分组 样品编号 作图编号
# 1      Isc     Isc1     Isc1
# 2      Isc     Isc2     Isc2
# 3      Isc     Isc3     Isc3
# 4      Con     Con1     Con1
# 5      Con     Con2     Con2
# 6      Con     Con3     Con3
# 7    Isc+A  Isc+A-1  Isc+A-1
# 8    Isc+A  Isc+A-2  Isc+A-2
# 9    Isc+A  Isc+A-3  Isc+A-3
class(sample_info_1$样品编号)
# [1] "character"
samp_numb <- sample_info_1$样品编号
# [1] "Isc1"    "Isc2"    "Isc3"    "Con1"    "Con2"    "Con3"    "Isc+A-1" "Isc+A-2"
# [9] "Isc+A-3"
# samp_numb
# sm <- match(samp_numb, colnames(samp_data_order_pv))
# sm
# class(sm)
# sm[!is.na(sm)]
sample_info_2
#      比较组 交集 物种  韦恩
# 1   Isc/Con   NA   NA 1,2,3
# 2 Isc+A/Isc   NA   NA  <NA>
#   3 Isc+A/Con   NA   NA  <NA>

compa_groups <- 1:length(sample_info_2$比较组)
compa_groups
for (i in compa_groups){
  # print(sample_info[i,1])
  # print(gsub("/","_",sample_info[i,1]))
  class(gsub("/","_",sample_info_2[i,1]))
  new_compa_groups <- gsub("/","_",sample_info_2[i,1])
  class(new_compa_groups)
  # dim(new_compa_groups)
  print(new_compa_groups)
  data <- read.xlsx("差异蛋白筛选结果.xlsx", sheet = new_compa_groups, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
  dim(data)
  class(data)
  rownames(data) <- data[,1]
  samp_data_order_pv <- data[order(data$`P-value`),] # 按照p value值进行升序排序
  dim(samp_data_order_pv)
  # [1] 119  19
  head(colnames(samp_data_order_pv))
  head(rownames(samp_data_order_pv))
  samp_numb <- sample_info_1$样品编号
  sm <- match(samp_numb, colnames(samp_data_order_pv))
  class(sm)
  sm <- as.numeric(sm[!is.na(sm)])
  sm
  # head(samp_data_order_pv)
  # dim(samp_data_order_pv)
  samp_data_order_pv_new <- samp_data_order_pv[,sm]
  head(samp_data_order_pv_new)
  if (NROW(samp_data_order_pv_new) >= 50){
  top50_prot <- data.frame(samp_data_order_pv_new[1:50,]) # 提取样品的top50蛋白质谱数据
  dim(top50_prot)
  # [1] 50  6
  head(top50_prot)
  top50_prot_t <- t(top50_prot) # 数据框转置
  top50_prot_t_corr <- cor(top50_prot_t) # 计算top50蛋白的相关性系数
  top50_prot_t_corr[1:5, 1:5]
  ## 绘图及保存
  col <- colorRampPalette(c("blue","white", "red"))(1000) # 准备绘图颜色
  pdf(paste0(new_compa_groups,"-correlation_coefficients_hclust_top50_dep.pdf"), width = 10, height = 10)
  M <- corrplot(top50_prot_t_corr, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8, col = col, order = "hclust", diag = F)
  dev.off()
  png(paste0(new_compa_groups,"correlation_coefficients_hclust_top50_dep.png"), width = 1000, height = 1000)
  corrplot(top50_prot_t_corr, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8, col = col, order = "hclust", diag = F)
  dev.off()
  ## 数据保存
  write.csv(M, file = paste0(new_compa_groups,"-top50_DEP_correlation_coefficients_hclust.csv"))
  }
}
## 结束
sessionInfo()
