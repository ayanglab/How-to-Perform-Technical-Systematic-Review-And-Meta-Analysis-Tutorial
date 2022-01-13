# This script analyses the data preprocessing information from various studies
library (tidyverse)
#roxygen2::roxygenise()
devtools::load_all ('..', export_all=F)
data (revdat)
revdat$public_data <- ifelse (revdat$public_data, 'Public', 'Private')

# publication over time
# training and testing sample size changes over time
revdat <- revdat [revdat$year != 2021, ]
p1 <- barplot_2var_label (revdat, 'year', 'public_data', extend_ratio=0.05) + 
        ylab ('Publication number')+ theme (aspect.ratio=1.3) 
acc_theme <- list (scale_y_log10(), theme (plot.title=element_text (size=config$fontsize, face='plain'),
                        legend.position='none'))
p2 <- acc_over_feature (revdat, 'training.number', 'year', 'year') + acc_theme
p3 <- acc_over_feature (revdat, 'testing.number', 'year', 'year') + acc_theme

# image sizes
p4 <- barplot_1var_overlap (revdat, 'augmentation', 'year', ID_col='ID', show_num=8, 
                               extend_ratio=1.5)+ theme (aspect.ratio=0.9)
p5 <- size_freq (revdat, 'input.size', 'year', bw=0.1)+ theme (aspect.ratio=0.7)
revdat$modality <- gsub ('PDFS', 'PD', revdat$modality)
p6 <- barplot_1var_overlap (revdat, 'modality', 'year', ID_col='ID', show_num=8, 
                               extend_ratio=1.5) + theme (aspect.ratio=0.7)
p7 <- barplot_1var_overlap (revdat, 'region', 'year', ID_col='ID', show_num=8, 
                               extend_ratio=1.5)+ theme (aspect.ratio=0.7)

grob_list <- list (p1, p2+theme (aspect.ratio=1.3), 
                   p3+theme (aspect.ratio=1.3), 
                   p4, p5+ scale_x_log10 (breaks=2^c(3:9)), p6, p7)
lay_mat <- rbind (c(1,1,2,2,3,3), 
                  c(4,4,4,5,5,5),
                  c(6,6,6,7,7,7) 
)
fig_dir <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/review/review_batch_all/final_figures'
arrange_plots (grob_list, paste (fig_dir, 'figure2.pdf', sep='/'), lay_mat, 
               plot_width=2.5, plot_height=6)

# pathology
# table (revdat$training.data.pathology, revdat$testing.data.pathology) 
#         FALSE TRUE
#   FALSE    24   10
#   none      1    0
#   TRUE      0   24
