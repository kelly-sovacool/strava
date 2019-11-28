import datetime
import pathlib

today = datetime.date.today()
years = range(2017, today.year+1)
filename_raw_csv = "data/raw/activities.csv"
csv_path = pathlib.Path(filename_raw_csv)

# forcerun download rule if the raw data file was last modified before today
if csv_path.exists():
    timestamp = datetime.date.fromtimestamp(csv_path.stat().st_mtime)
    if today > timestamp:
        csv_path.touch(exist_ok=True)

rule targets:
    input:
        "docs/report.html",
        "figures/README.md"

rule download:
    input:
        R="code/download.R"
    output:
        csv=filename_raw_csv
    script:
        "{input.R}"

rule process:
    input:
        R="code/process_raw_data.R",
        csv=rules.download.output.csv
    output:
        csv="data/processed/activities.csv"
    script:
        "{input.R}"

rule plot:
    input:
        R="code/make_plots.R",
        csv=rules.process.output.csv
    output:
        line_plot="figures/line_all.png",
        bar_plot_week="figures/bar_all_week.png",
        bar_plot_month="figures/bar_all_month.png",
        bar_plot_day="figures/bar_all_day.png",
        bar_years=expand("figures/bar_{year}.png", year=years)
    params:
        years=years
    script:
        "{input.R}"

rule render_report:
    input:
        R="code/render.R",
        rmd="code/report.Rmd",
        plots=rules.plot.output
    output:
        html="docs/report.html"
    script:
        "{input.R}"

rule cat_figures_readme:
    input:
        figures=rules.plot.output
    output:
        md="figures/README.md"
    run:
        exts = {'svg', 'png', 'jpg'}
        with open(output.md, "w") as outfile:
            outfile.write("# Plots\n\n")
            for plot_fn in sorted(os.listdir("figures/")):
                if plot_fn.split('.')[-1] in exts:
                    outfile.write(f"![]({plot_fn})\n\n")

