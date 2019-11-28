library(here)
library(rmarkdown)

if (exists("snakemake")) {
    rmd <- snakemake@input[["rmd"]]
    html <- snakemake@input[["html"]]
} else {
    rmd <- here::here("code", "report.Rmd")
    html <- here::here("docs", "report.html")
}
rmarkdown::render(rmd, output_file=html)
