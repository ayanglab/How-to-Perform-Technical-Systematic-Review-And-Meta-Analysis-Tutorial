# How-to-Perform-Technical-Systematic-Review-And-Meta-Analysis-Tutorial
Here we summarise a tutorial for systematic review and meta analysis for technical development (e.g., using deep learning) for digital healthcare projects.

This is also the official implementation of our systematic review and meta analysis for fast MRI:  

Chen, Yutong, Carola-Bibiane Schönlieb, Pietro Liò, Tim Leiner, Pier Luigi Dragotti, Ge Wang, Daniel Rueckert, David Firmin, and Guang Yang, "AI-Based Reconstruction for Fast MRI—A Systematic Review and Meta-Analysis," in Proceedings of the IEEE, vol. 110, no. 2, pp. 224-245, Feb. 2022, DOI: 10.1109/JPROC.2022.3141367.

Please cite:

```
@ARTICLE{chen2022aifastMRI,
  author={Chen, Yutong and Schönlieb, Carola-Bibiane and Liò, Pietro and Leiner, Tim and Dragotti, Pier Luigi and Wang, Ge and Rueckert, Daniel and Firmin, David and Yang, Guang},
  journal={Proceedings of the IEEE}, 
  title={AI-Based Reconstruction for Fast MRI—A Systematic Review and Meta-Analysis}, 
  year={2022},
  volume={110},
  number={2},
  pages={224-245},
  doi={10.1109/JPROC.2022.3141367}}
```

Use the following command in R to access the meta-analysis data:
```r
# for the design traits of each model
data (revdat, package='deepCSMRI')
# for the performance of each metric
data (metric_val, package='deepCSMRI')
```
You may use the R help page to learn more about the data.
