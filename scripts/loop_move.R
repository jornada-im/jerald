library(jerald)
setwd('/media/greg/jrn-DataProducts/JORNADA_IM/Core_packages/')
smespkg <- 210086006:210086009
for(p in smespkg){
  old_dir <- dir()[grepl(as.character(p), dir())]
  print(old_dir)
  new_dir <- paste0(old_dir, '_new')
  template_dataset_dir(p)
  migrate_eal_dir(old_dir, new_dir)
}
  