# Contributing to gpuinfo

Run the portable checks with:

```sh
R CMD build .
R CMD check --as-cran gpuinfo_*.tar.gz
```

Real-hardware validation must use `tools/validate-hardware.R`. Do not mark a
platform as validated from parser fixtures or a no-GPU CI run. After a real run,
add one concise row to `inst/extdata/validation.csv` and update
`inst/VALIDATION.md` in the same change.

Hardware reports can contain host details. Review diagnostics before sharing
them; gpuinfo never transmits reports automatically.
