import datetime
import pathlib

now = datetime.datetime.now()
update_frequency = datetime.timedelta(hours = 12)
years = range(2017, now.year+1)
filename_raw_csv = 'data/raw/activities.csv'
csv_path = pathlib.Path(filename_raw_csv)
filename_download_code = 'code/download.R'
download_code_path = pathlib.Path(filename_download_code)
# forcerun download rule if the raw data file was last modified before today
if csv_path.exists():
    timestamp = datetime.datetime.fromtimestamp(csv_path.stat().st_mtime)
    if (now - update_frequency) > timestamp:
        download_code_path.touch(exist_ok=True)

rule targets:
    input:
        "docs/report.html",
        "figures/README.md"

rule download:
    input:
        R=filename_download_code
    output:
        csv=filename_raw_csv
    script:
        "{input.R}"

rule process:
    input:
        R="code/process_raw_data.R",
        csv=rules.download.output.csv
    output:
        csv="data/processed/activities.csv",
        sum="data/processed/summary.csv"
    script:
        "{input.R}"

rule plot_activities:
    input:
        R="code/make_plots_activities.R",
        csv=rules.process.output.csv
    output:
        bar_plot_week="figures/bar_all_week.png",
        bar_plot_month="figures/bar_all_month.png",
        bar_plot_day="figures/bar_all_day.png",
        bar_years=expand("figures/bar_{year}.png", year=years),
        box_dist="figures/box_weekday_dist_wrap.png",
        box_time="figures/box_weekday_time.png"
    params:
        years=years
    script:
        "{input.R}"

rule plot_summary:
    input:
        R="code/make_plots_summary.R",
        sum=rules.process.output.sum
    output:
        "figures/bar_sum_dist.png",
        "figures/bar_sum_hrs.png"
    script:
        "{input.R}"

rule render_report:
    input:
        R="code/render.R",
        rmd="code/report.Rmd",
        plots1=rules.plot_activities.output,
        plots2=rules.plot_summary.output
    output:
        html="docs/report.html"
    script:
        "{input.R}"

rule cat_figures_readme:
    input:
        rules.plot_activities.output,
        rules.plot_summary.output
    output:
        md="figures/README.md"
    run:
        exts = {'svg', 'png', 'jpg'}
        with open(output.md, "w") as outfile:
            outfile.write("# Plots\n\n")
            for plot_fn in sorted(os.listdir("figures/")):
                if plot_fn.split('.')[-1] in exts:
                    outfile.write(f"![]({plot_fn})\n\n")

