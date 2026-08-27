# Hand-entered lobster data

Files here are **entered by hand and cannot be regenerated** from a raw export.
Unlike everything under `data/`, this folder is tracked by git (see the
`!data/*/manual` exception in `.gitignore`) so the work is not lost if a local
copy is deleted.

## pot_strings.csv

Assigns each western rock lobster pot to the string it was set in. Strings were
repeated between the 2025 and 2026 surveys, so string is the repeated measures
unit in the catch rate models in `06_model_catch-rates.R`.

- **`pot_key`** - join key, `<year>_<date_retrieved>_<pot_number>`. Underscores
  keep Excel from reformatting it as a date or number. Joined onto the pot table
  in `03_combine-data.R`.
- **`string`** - the string identifier, consistent across both years: string 7
  in 2026 is the same line of pots as string 7 in 2025.
- Every other column is a copy of the pot metadata, kept only so the file can be
  read by eye. Nothing downstream uses them.

A string is **one physical line of pots**, kept whole even where it crosses a
marine park zone boundary. Strings 1 to 13 each sit entirely within one zone;
string 14 spans the boundary (11 pots in the National Park Zone, 5 in Special
Purpose) because it was a single deployment.

This means zone is *almost* a between-string factor in the models - string 14 is
the only place zone varies within a string. An earlier version of this file
split string 14 in two at the boundary; that was undone, because the split
asserted two independent deployments where there was one and discarded the only
within-string zone contrast in the dataset. Strings now run 1 to 14, with no 15.

Two 2025 pots have no retrieval time recorded and key as `2025_NA_4` and
`2025_NA_15`.
