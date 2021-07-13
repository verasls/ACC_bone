## figures    : Generates all figures
figures: figures/fig1.png figures/fig2.png figures/figS1.png figures/figS2.png figures/figS3.png

figures/fig1.png: data/mechanical_load_data.rda figures/fig1.R
	R CMD BATCH figures/fig1.R

figures/fig2.png: output/loocv_data.rda figures/fig2.R
	R CMD BATCH figures/fig2.R

figures/figS1.png: data/mechanical_load_data.rda figures/figS1.R
	R CMD BATCH figures/figS1.R

figures/figS2.png: output/loocv_data.rda figures/figS2.R
	R CMD BATCH figures/figS2.R

figures/figS3.png: output/loocv_data.rda figures/figS3.R
	R CMD BATCH figures/figS2.R

## output     : Generates all output
output: output/loocv_data.rda output/prediction_models.rda

output/loocv_data.rda: data/mechanical_load_data.rda code/03_build_models.R
	R CMD BATCH code/03_build_models.R

output/prediction_models.rda: data/mechanical_load_data.rda code/03_build_models.R
	R CMD BATCH code/03_build_models.R

## data       : Processes raw data
data: data/mechanical_load_data.rda

data/mechanical_load_data.rda: data/anthropometric_data.csv data/jumping_data.csv code/01_tidy_data.R
	R CMD BATCH code/01_tidy_data.R

## install    : Installs all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean      : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata

## cleanall   : Removes auto-generated files, including processed data, figures and the manuscript pdf
cleanall:
	\rm -f *.Rout .Rdata data/*.rda output/* figures/*.png figures/*.tiff

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
