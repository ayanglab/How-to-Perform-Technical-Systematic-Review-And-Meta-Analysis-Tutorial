root <- '/mnt/c/users/Yutong/Documents/bioinformatics/machine_learning/review/'
data_dir <- paste (root, 'review_batch_all', sep='/')
fig_dir <- paste (data_dir, 'final_figures', sep='/')
#setwd ('..')
#roxygen2::roxygenise()
#setwd('manuscript')
devtools::load_all ('..', export_all=F)
data (revdat)
library (tidyverse)

p1 <- ggplot () + theme_void ()
p2 <- read.csv (paste (fig_dir, 'PRISMA_sub.csv', sep='/'))
#grep('\u0007', p2$statistical.test)

revdat <- revdat [revdat$year != '2021',]
p3 <- barplot_2var (revdat, 'year', 'country') + ylab ('Publication number')+ 
        theme (aspect.ratio=1.5) +guides (fill=guide_legend(ncol=3))
p4 <- barplot_1var (revdat, 'institute', 'year', show_num=10)
grob_list <- list (p1, p2, p3, p4+theme (aspect.ratio=1.2))
lay_mat <- rbind (c(1,1,1,3,3), 
                  c(1,1,1,3,3),
                  c(2,2,4,4,4)
                  )
arrange_plots (grob_list, paste (fig_dir, 'figure1.pdf', sep='/'), lay_mat, plot_width=3, plot_height=5.2)
