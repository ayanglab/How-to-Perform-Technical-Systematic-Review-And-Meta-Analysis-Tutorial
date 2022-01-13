root <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/review/'
data_dir <- paste (root, 'review_batch_all', sep='/')
fig_dir <- paste (data_dir, 'final_figures', sep='/')

library (tidyverse)
library (mclust)
devtools::load_all ('..', export_all=T)
data (revdat)

revdat$supervised <- ifelse (revdat$supervised, 'Supervised', 'Unsupervised')
revdat$public_data <- ifelse (revdat$public_data, 'Public', 'Private')
revdat$public_code <- ifelse (revdat$public_code, 'Published', 'Not published')
revdat$category <- ifelse (revdat$unrolled, 'Unrolled optimisation', 'End-to-end')

revdat2 <- revdat [revdat$year != 2021,]
p1 <- barplot_2var_label (revdat2, 'year', 'supervised', extend_ratio=0.05) + 
        ylab ('Publication number')+  theme (aspect.ratio=1) 
p2 <- barplot_2var_label (revdat2, 'year', 'category', extend_ratio=0.05) + 
        ylab ('Publication number')+ theme (aspect.ratio=1) 

# architecture and other special design features
plot_fea <- c('model_structure', 'GAN.based', 'data.consistency', 'residual', 'optimiser')
p3 <- freq_bar (revdat, plot_fea, max_num=5) 

plot_fea <- c('spatial.domain', 'PI', 'dimension', 'complex_inp', 'reproducibility')
revdat$reproducibility<- revdat$reproducibility %>% as.character () %>% firstup_vec () %>%
        factor (levels=c('Easy', 'Medium', 'Hard'))
p6 <- freq_bar (revdat, plot_fea, max_num=5) 

# changes in different design features over time
features <- c('supervised', 'unrolled', 'model_structure', 'GAN.based',
              'data.consistency', 'residual', 'optimiser', 'spatial.domain',
              'PI', 'dimension', 'complex_inp', 'public_code', 'public_data',
              'reproducibility')

#multi_freq_over_time_cor (revdat, 'year', features) %>% 
#        write.csv (paste (data_dir, 'analyse_stat/cor_over_time.csv', sep='/') )

p4 <- barplot_1var_overlap (revdat, 'loss', 'year', ID_col='ID')+theme (aspect.ratio=0.7)
revdat$loss_count <- overlap_count (revdat, 'loss', 'ID')
p5 <- overlap_count_plot (revdat, 'loss_count', 'year', bw=1) + 
        xlab ('Number of loss functions') + ylab ('Publication number')+theme (aspect.ratio=0.6)+
        coord_flip ()
#multi_freq_over_time_cor (revdat, 'year', 'loss') %>% 
#        write.csv (paste (data_dir, 'analyse_stat/cor_over_time_loss.csv', sep='/') )

# check parameter number and computation time
data (config)
acc_theme <- list (scale_y_log10(), theme (plot.title=element_text (size=config$fontsize, face='plain'),
                        legend.position='none'))
p7 <- acc_over_feature (revdat2, 'parameter.number', 'year', 'year')+acc_theme
p8 <- acc_over_feature (revdat2, 'computation.time', 'year', 'year')+acc_theme

selected_features <- c('unrolled', 'dimension', 'spatial.domain', 'unet.like', 'PI',
                       'residual', 'complex_inp', 'GAN.based', 'supervised', 'data.consistency',
                        'MSE_loss', 'max_acc')
data (revdat)
p9 <- pca_GMM (revdat [!revdat$revision, ], selected_features,
               cluster_col='new_cluster', move_y=1, length_ratio=0.08)

grob_list <- list (p1, p2, p3, p4, p5, p6, p7, p8, p9)
lay_mat <- rbind (c(1,1,1,2,2,2), 
                  c(3,3,3,3,3,3),
                  c(4,4,4,5,5,5),
                  c(6,6,6,6,6,6),
                  c(7,7,8,8,9,9)
)
arrange_plots (grob_list, paste (fig_dir, 'figure3.pdf', sep='/'), lay_mat, plot_width=3, plot_height=5)
