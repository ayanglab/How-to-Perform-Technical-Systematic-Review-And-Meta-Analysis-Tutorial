# How-to-Perform-Technical-Systematic-Review-And-Meta-Analysis-Tutorial
Here we summarise a tutorial for systematic review and meta analysis for technical development (e.g., using deep learning) for digital healthcare projects.

This is also the official implementation of our systematic review and meta analysis for fast MRI:  

Chen, Yutong, Carola-Bibiane Schönlieb, Pietro Liò, Tim Leiner, Pier Luigi Dragotti, Ge Wang, Daniel Rueckert, David Firmin, and Guang Yang. "AI-based Reconstruction for Fast MRI--A Systematic Review and Meta-analysis." Proceedings of the IEEE, arXiv preprint arXiv:2112.12744 (2021). 

Please cite:

```
@article{chen2021ai,
  title={AI-based Reconstruction for Fast MRI--A Systematic Review and Meta-analysis},
  author={Chen, Yutong and Sch{\"o}nlieb, Carola-Bibiane and Li{\`o}, Pietro and Leiner, Tim and Dragotti, Pier Luigi and Wang, Ge and Rueckert, Daniel and Firmin, David and Yang, Guang},
  journal={arXiv preprint arXiv:2112.12744},
  year={2021}
}
```

Use the following command in R to access the meta-analysis data:
```r
# for the design traits of each model
data (revdat, package='deepCSMRI')
# for the performance of each metric
data (metric_val, package='deepCSMRI')
```
You may use the R help page to learn more about the data.
