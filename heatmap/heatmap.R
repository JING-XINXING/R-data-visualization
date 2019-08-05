#########################################################################################################
# Rscript function: Plotting heatmap batch, especially for projects with only two sample comparison groups
# Author: jingxinxing
# Date: 2019/06/13
# Version: v1
#########################################################################################################
## 1.设置当前的工作目录
setwd("./")
## 2.导入R包：pheatmap
library("pheatmap")
## 3.批量读取绘图数据
file_names<- list.files("./data")
## 4.查看当前目录中要读取的数据文件个数
print("当前目录中要读取的数据文件个数为：", length(file_names))
class(file_names)
file_names
## 5.绘制聚类热图，保存为pdf和png两种文件格式
for (i in 1:length(file_names)) {
  name <- gsub(".csv","",file_names[i])
  data <- assign(name, read.table(paste0("/public/hstore5/proteome/Personal_dir/jingxinxing/test/heatmap/data/",file_names[i]),sep = ",", header = TRUE, quote = "",stringsAsFactors = FALSE, row.names = 1))
  pdf(paste0("heatmap_", file_names[i], ".pdf"), width = 24, height = 18)
  pheatmap(data,
           cluster_rows = T,
           display_numbers = FALSE,
           number_format = "%.3f",
           show_colnames = TRUE,
           show_rownames = F,
           color=colorRampPalette(c("green","black","red"),bias=1)(100),
           border_color = NA,
           cellwidth = 80,
           cellheight = 0.5,
           fontsize_col = 12,
	   scale="column")
  dev.off()
  png(paste0(file_names[i],".png"), width = 1500, height = 1200)
  pheatmap(data,
           cluster_rows = T,
           display_numbers = FALSE,
           number_format = "%.3f",
           show_colnames = TRUE,
           show_rownames = F,
           color=colorRampPalette(c("green","black","red"),bias=1)(100),
           border_color = NA,
           cellwidth = 80,
           cellheight = 0.5,
           fontsize_col = 12,
	   scale="column")
  dev.off()
}
## 6.输出绘制聚类热图所需的R包和环境信息
sessionInfo()
