root <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/DAGAN/review/'
data_dir <- paste (root, 'review_batch_all/final_tables', sep='/')

devtools::load_all ('..', export_all=F)
data (revdat)
library (tidyverse)

# ----------Post-processing----------
# For the output from R
# need to do some formating in vim, e.g. removing the quotation marks and blank
# space for better display in latex.
# :%s/"/g 
# :%s/, 0/,0/g

# ----------Table S3: Design traits----------
features <- c('supervised', 'unrolled', 'model_structure', 'GAN.based',
              'data.consistency', 'residual', 'optimiser', 'spatial.domain',
              'PI', 'dimension', 'complex_inp', 'public_code', 'public_data',
              'reproducibility')

multi_freq_over_time_cor (revdat, 'year', features) %>% 
        write.csv (paste (data_dir, 'cor_over_time.csv', sep='/'), row.names=F)

# ----------Table S4: loss functions----------
multi_freq_over_time_cor (revdat, 'year', 'loss') %>% 
        write.csv (paste (data_dir, 'cor_over_time_loss.csv', sep='/'), row.names=F)

# ----------Table S6: Metrics----------
multi_freq_over_time_cor (revdat, 'year', 'metric') %>% 
        write.csv (paste (data_dir, 'cor_over_time_metric.csv', sep='/'), row.names=F)

# ----------Table S7: Comparison methods----------
multi_freq_over_time_cor (revdat, 'year', 'comparison') %>% 
        write.csv (paste (data_dir, 'cor_over_time_comparison.csv', sep='/'), row.names=F)

# ----------TableS8: metric pairwise correlation----------
data (metric_val)
metric_val <- preprocess_acc_mat (metric_val)
quantitative <- c('SSIM', 'PSNR', 'MSE', 'NRMSE', 'SNR', 'NMSE', 'HFEN', 'RMSE', 'MAE', 'RLNE', 'MSSIM')
qualitative <- c('sharpness', 'OQ', 'artifact', 'CN', 'CNR')
r2mat <- all_metrics_cor (metric_val, c(quantitative, qualitative) )
r2mat %>% data.frame () %>% round (2) %>% format (nsmall=2) %>%
        write.csv (paste (data_dir, 'cor_metrics.csv', sep='/'))

# ----------Table S9: model design traits on model performance----------
filtered_df <- filter_main_metric (metric_val)
revdat <- add_impr (revdat, filtered_df, 'SSIM')
revdat <- add_impr (revdat, filtered_df, 'PSNR')

devtools::load_all ('..', export_all=F)
selected_features <- c('unrolled', 'spatial.domain', 'unet.like', 'PI',
                       'residual', 'GAN.based', 'supervised', 'complex_inp',
                       'data.consistency', 'MSE_loss', 'adam',
                       'parameter.number', 'max_acc', 'new_cluster')
all_p <- compare_multi_features_multi_metrics (revdat, selected_features, c('SSIM', 'PSNR'))
#all_p %>% arrange (p) %>% write.csv (paste (data_dir, 'sig_p_val_performance.csv', sep='/'), row.names=F)
# need to do some formating in vim, e.g. removing the quotation marks and blank
# space for better display in latex.
