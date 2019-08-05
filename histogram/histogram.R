###########################################################################
# Rscript function: 直方分布图绘图脚本
# Date: 2019/06/20
# Author: jingxinxing
# Version: v1
###########################################################################
## 设置工作目录
setwd("./")

## 导入R包
library(ggplot2)
## 磷酸化修饰位点数量分布图

pep_data <- read.table("peptide_data.csv", sep = ',', header = T, row.names = 1)
dim(pep_data2)
ggplot(pep_data2, aes(修饰位点数)) +
  geom_histogram(stat = "bin", binwidth = 1, col = "Black", fill = "DarkGreen", boundary = 1.0, position = "identity") +
  xlim(0,25) +
  xlab(label = "Number of Phosphorylated Sites in a protein") +
  ylab(label = "Phosphorylated Protein numbers") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label=as.character(round(..count..,2))),stat="bin",binwidth=1,vjust=-0.5, position = "identity", check_overlap = T, na.rm = T, show.legend = F, inherit.aes = T, hjust = 1.4) +
  theme(plot.margin = unit(rep(4,4),'lines'), aspect.ratio = 0.5)
ggsave("Peptide_number-proteins.pdf", width = 12, height = 7.5, units = "in")
ggsave("Peptide_number-proteins.png", width = 12, height = 7.5, units = "in")
