tutorial.html: tutorial.Rmd
	Rscript -e "rmarkdown::render('$<', encoding='UTF-8')"

index.html: tutorial.html
	cp $< $@
