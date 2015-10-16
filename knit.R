setwd('/home/joebrew/Documents/fdi_moz')
require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('paper.Rmd', 'paper.md') # creates md file
markdownToHTML('paper.md', 'paper.html') # creates html file
browseURL(paste('file://', file.path(getwd(),'paper.html'), sep='')) # open file in browser