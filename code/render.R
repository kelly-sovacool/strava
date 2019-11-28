library(here)
library(rmarkdown)

if (exists("snakemake")) {
    rmd <- snakemake@input[["rmd"]]
} else {
    rmd <- here::here("code", "report.Rmd")
}
rmarkdown::render(rmd, output_dir=here::here("docs"), output_format="all")
