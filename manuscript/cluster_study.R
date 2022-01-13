root <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/DAGAN/review/'
setwd (paste (root, 'script/utils', sep='/'))
data_dir <- paste (root, 'review_batch_all', sep='/')
fig_dir <- paste (data_dir, 'final_figures', sep='/')
rdata <- read.csv (paste (data_dir, 'all_cleaned.csv', sep='/'), row.names=1)
library (tidyverse)
library (TBdev)
DIR <- modules::use ('dim_red.R')
CU <- modules::use ('clustering.R')
SS <- modules::use ('sum_stat.R')
PL <- modules::use ('plotting.R')

selected_features <- c('unrolled', 'dimension', 'spatial.domain', 'unet.like', 'PI',
                       'residual', 'complex_inp', 'GAN.based', 'supervised', 'data.consistency',
                        'MSE_loss', 'max_acc')
# reassign cluster names

p1 <- DIR$plot_pca (rdata, selected_features, return_sep=T, color_by='new_cluster', move_y=1.5)
ggpubr::ggarrange (plotlist=p1)

mean_clust <- read.csv (paste (data_dir, 'GMM_mean.csv', sep='/'), row.names=1)
var_clust  <- read.csv (paste (data_dir, 'GMM_var.csv',  sep='/'), row.names=1)
p2 <- DIR$pca_GMM (rdata, selected_features, mean_clust, var_clust, cluster_col='new_cluster', move_y=1)
p3 <- CU$hclust_study (rdata, selected_features, group.by='new_cluster')
p4 <- CU$hclust_feature (rdata, selected_features)

grob_list <- list (p2+theme (aspect.ratio=0.8), 
                   p1[[2]]+theme (aspect.ratio=0.8), p3, p4)
lay_mat <- rbind (c(1,1,1,4,4,4), 
                  c(2,2,2,4,4,4),
                  c(3,3,3,3,3,3),
                  c(3,3,3,3,3,3) 
                  )
PL$arrange_plots (grob_list, paste (fig_dir, 'figure4.pdf', sep='/'), lay_mat, plot_width=3, plot_height=6)
#SS$compare_multi_features_multi_metrics (rdata, 'cluster', selected_features) %>% filter (p< 0.05)
