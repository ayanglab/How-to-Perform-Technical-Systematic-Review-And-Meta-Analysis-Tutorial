root <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/review/'
data_dir <- paste (root, 'review_batch_all', sep='/')
fig_dir <- paste (data_dir, 'final_figures', sep='/')

devtools::load_all ('..', export_all=F)
data (revdat)
data (metric_val)
library (tidyverse)

metric_val <- preprocess_acc_mat (metric_val)
filtered_df <- filter_main_metric (metric_val)
revdat <- add_impr (revdat, filtered_df, 'SSIM')
revdat <- add_impr (revdat, filtered_df, 'PSNR')

# make sure the colors are consistent despite absence of data points
data (config)
revdat$new_cluster <- factor (revdat$new_cluster)
color_guide <- list (scale_color_manual (breaks=levels(revdat$new_cluster), 
                                         values=config$npg_pal[1:8]),
                     theme (plot.title=element_text (size=config$fontsize, 
                     family=config$font_fam, face='plain'), legend.position='none')
)

# plotting
revdat <- revdat [revdat$year != 2021,]
p1 <- acc_over_feature (revdat, 'SSIM', 'year', 'year', max_y=2, remove_NA=T) + 
        ylab ('SSIM odds ratio')+color_guide[[2]]
p2 <- acc_over_feature (revdat, 'PSNR', 'year', 'year', max_y=1.6, remove_NA=T) + 
        ylab ('PSNR odds ratio')+color_guide[[2]]
p3 <- Deeks_test (revdat, 'SSIM')
p4 <- Deeks_test (revdat, 'PSNR')

p5 <- acc_over_feature (revdat, 'SSIM', 'new_cluster', 'new_cluster',
                        num_rotation=20, rotation=20, max_y=2)+ 
                        ylab ('SSIM odds ratio')+color_guide
p6 <- acc_over_feature (revdat, 'PSNR', 'new_cluster', 'new_cluster',
                        num_rotation=20, rotation=20, max_y=1.6)+ 
                        ylab ('PSNR odds ratio')+color_guide

grob_list <- list (p1+ theme (aspect.ratio=1), p2+ theme (aspect.ratio=1), 
                   p3+ theme (aspect.ratio=1), p4+ theme (aspect.ratio=1), 
                   p5 + theme (aspect.ratio=1), p6+ theme (aspect.ratio=1))
lay_mat <- rbind (c(1,2), 
                  c(3,4),
                  c(5,6) )
arrange_plots (grob_list, paste (fig_dir, 'figure6.pdf', sep='/'), lay_mat, plot_width=7, plot_height=7)

#selected_features <- c('unrolled', 'spatial.domain', 'unet.like', 'PI',
#                       'residual', 'GAN.based', 'supervised', 'complex_inp',
#                       'data.consistency', 'MSE_loss', 'adam',
#                       'parameter.number', 'max_acc', 'new_cluster')
#all_p <- compare_multi_features_multi_metrics (revdat, selected_features, c('SSIM', 'PSNR'))
#all_p %>% arrange (p) %>% write.csv (paste (data_dir, 'analyse_stat/sig_p_val_performance.csv', sep='/'))
