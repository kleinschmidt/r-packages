tutorial.html: tutorial.Rmd
	Rscript -e "rmarkdown::render('$<', encoding='UTF-8')"
