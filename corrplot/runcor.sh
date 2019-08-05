#!/usr/bin/bash
date
# nohup 本地投递绘图脚本
Rscript proteins_correlation_heatmap.R
sleep 3
echo "Congratulations plotting finished"
