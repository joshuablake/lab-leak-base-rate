site: docs/lab-leak-base-rates.html

docs/lab-leak-base-rates.html: lab-leak-base-rates.Rmd _output.yaml
	Rscript -e 'rmarkdown::render("$<", output_dir = "docs")'