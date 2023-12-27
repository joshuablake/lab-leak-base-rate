site: outputs/lab-leak-base-rates.html

outputs/lab-leak-base-rates.html: lab-leak-base-rates.Rmd _output.yaml
	Rscript -e 'rmarkdown::render("$<", output_dir = "outputs")'