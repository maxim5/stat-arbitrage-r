# The pipeline

- Download and index the data (can be done via https://github.com/maxim5/quant-data project).
- Run `pairs_selector.R` script.
  It outputs `result-cointegration-pairs.csv`, `result-tradable-pairs.csv`, `result-spreads.csv`, and also
  `pairs.RData` snapshot that can be further used e.g. by `pairs_plot.R`.
- Run `strategy_selector.py`. It outputs `result-strategies.csv`.