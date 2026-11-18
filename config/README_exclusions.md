# How to maintain `exclusions.csv`

Rules 3 (concurrent programme) and 4 (major shock) cannot be automated reliably, so they live in this hand-curated file. Every row removes one country from the donor pool of the listed events and appears in the donor-pool log with its reason.

| Column | Meaning |
|---|---|
| `iso3` | ISO 3166-1 alpha-3 code |
| `applies_to` | Event IDs from `config.R`, separated by `;`, or `all` |
| `rule` | `concurrent_programme` or `shock` |
| `reason` | One line a reviewer can check (programme name and year, or conflict/disaster and year) |
| `status` | `confirmed` or `verify`. Rows marked `verify` are still applied, and the log flags them |

## Filling the shock rows (Rule 4)

1. **Conflict.** Download the UCDP/PRIO Armed Conflict Dataset. List every country-year in the post-treatment window of an event with intensity level 2 (≥1,000 battle-related deaths).
2. **Disasters.** Export EM-DAT records for the same window. Flag any country where total damage in one year exceeds 5% of that year's GDP (WDI series NY.GDP.MKTP.CD).
3. Add one row per country with `rule = shock` and the event IDs whose post-period overlaps the shock year.

## Concurrent programmes still to screen

The starting rows cover the programmes named in the paper. Before the final run, screen the electricity and sanitation donor pools for other national, deadline-driven access drives in the same window, for example community-led sanitation campaigns elsewhere in South Asia or national electrification strategies in sub-Saharan Africa. Record each screening decision, including the ones you decide not to exclude, in the Supplementary Material.
