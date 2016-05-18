tutorial.html: tutorial.Rmd
	Rscript -e "rmarkdown::render('$<')"
