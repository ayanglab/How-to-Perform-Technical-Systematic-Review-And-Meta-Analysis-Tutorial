# This script analyses the model performance from various studies
root <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/review/'
data_dir <- paste (root, 'review_batch_all', sep='/')
fig_dir <- paste (data_dir, 'final_figures', sep='/')

devtools::load_all ('..', export_all=F)
data (revdat)
library (tidyverse)

# ----------retro- vs pro- spective----------
table (grepl ('retrospective', revdat$testing.mode))
table (grepl ('prospective', revdat$testing.mode))
table (grepl ('prospective', revdat$testing.mode) & grepl ('retrospective', revdat$testing.mode))

revdat$prospective <- grepl ('prospective', revdat$testing.mode)
revdat$prospective <- ifelse (revdat$prospective, 'Prospective', 'Retrospective')
revdat <- revdat [revdat$year != 2021,]
p1 <- barplot_2var_label (revdat, 'year', 'prospective') + ylab ('Publication number')+ 
        theme (aspect.ratio=1.3, legend.position='top') 

p2 <- barplot_1var_overlap (revdat, 'metric', 'year', ID_col='ID')
#multi_freq_over_time_cor (revdat, 'year', 'metric') %>% 
#        write.csv (paste (data_dir, 'analyse_stat/cor_over_time_metric.csv', sep='/') )

revdat$metric_count <- overlap_count (revdat, 'metric', 'ID')
revdat$compare_count <- overlap_count (revdat, 'comparison', 'ID')
p3 <- overlap_count_plot (revdat, 'metric_count', 'year', bw=1)+
        xlab ('Number of metrics') +ylab ('Count')
p5 <- overlap_count_plot (revdat, 'compare_count', 'year', bw=1)+
        xlab ('Number of comparison methods')+ylab ('Count')

# comparison standards
p4 <- barplot_1var_overlap (revdat, 'comparison', 'year', ID_col='ID', show_num=9)
#multi_freq_over_time_cor (revdat, 'year', 'comparison') %>% 
#        write.csv (paste (data_dir, 'analyse_stat/cor_over_time_comparison.csv', sep='/') )

# ----------reported results----------
data (metric_val)
metric_val <- preprocess_acc_mat (metric_val)
p6 <- metric_acc (metric_val, metric_types=c('PSNR', 'SSIM')) + theme (aspect.ratio=2.5)

quantitative <- c('SSIM', 'PSNR', 'MSE', 'NRMSE', 'SNR', 'NMSE', 'HFEN', 'RMSE', 'MAE', 'RLNE', 'MSSIM')
qualitative <- c('sharpness', 'OQ', 'artifact', 'CN', 'CNR')
r2mat <- all_metrics_cor (metric_val, c(quantitative, qualitative) )

p7 <- custom_heat(r2mat, cluster_rows=F, cluster_columns=F, legend_title='R2
                  value', width=5, height=5, grid_height=10, row_names_side='left')
p7_final <- ComplexHeatmap::draw (p7, heatmap_legend_side='top')

# non-concordant metrics
p8 <- plot_PSNR_SSIM (metric_val, color_by='acceleration', label_text=F, 
                         metrics=c('PSNR', 'SSIM'), log_color=T) +
        scale_fill_viridis_c () + theme (legend.position='top', aspect.ratio=1)

grob_list <- list (p1, p2, p3, p6, p8, p7_final, p4+theme (aspect.ratio=0.8), p5 +theme (aspect.ratio=0.8))
lay_mat <- rbind (c(1,1,2,2,3,3), 
                  c(4,4,4,4,5,5),
                  c(4,4,4,4,6,6),
                  c(7,7,7,8,8,8)
)
arrange_plots (grob_list, paste (fig_dir, 'figure5.pdf', sep='/'), lay_mat, plot_width=3.5, plot_height=7)
