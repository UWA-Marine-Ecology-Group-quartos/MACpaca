![Alt Text](workflow.png)

# australian-marine-parks

## Steps to use this template for a new marine park:

1.  Copy the `r/TEMPLATE/` folder and save in the `r/` folder as your park name.
2.  Edit `r/[YOUR_PARK]/02_create-report_appendix-A-natural-values/00_config.yml` and `r/[YOUR_PARK]/03_create-report_appendix-B-pressures/00_config.yml` to set your marine park and project name (currently set to "template")
3.  Copy `TEMPLATE/` folders in `data/`, `output/model-output/`, and `plots/` and save as your park name.
4.  Run scripts in number order.
    - The template scripts were duplicated from the Geographe marine park, so they need to be read through and edited to be appropriate for your park.
    - To do this, Look out for TODO notes where specific edits for your park/analysis are required. Run `todor::todor(search_path = "r/[YOUR_PARK]")` to view full list of required edits.
5.  Text in Quarto markdowns need to be edited heavily.
6.  Look out for commits beginning with 'TEMPLATE:', these contain changes to the template scripts and will probably improve your analysis, so you will need to look at what changes were made and copy them into your scripts.

## Website

The site at <https://uwa-marine-ecology-group-quartos.github.io/MACpaca/> is rebuilt automatically by
`.github/workflows/deploy-site.yml` whenever something in `quartos/`, `images/parks/` or `site/` is pushed to `main`.

- **New report for an existing park:** render the Quarto `.html` into that park's `quartos/<folder>/` and push. It is linked automatically.
- **New park:** add a block to `site/parks.yml` (name, network, folders, photo and credit, synthesis IDs). A new `quartos/` folder that isn't listed still gets a basic card.
- **Planned reports** listed under `planned:` show as "In preparation" until the `.html` exists.
- **Test locally:** `pip install pyyaml pillow`, then `python site/build_site.py` and open `_site/index.html`.
