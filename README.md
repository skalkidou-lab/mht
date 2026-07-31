# mht

Menopausal hormone therapy (MHT) exposure definitions for Swedish registry data.

`mht` derives MHT exposure from Swedish prescription registry (LMED) data. It
categorises dispensed product names into MHT groups, corrects dispensed
durations for products whose recorded defined daily doses are unreliable,
bridges gaps between consecutive prescriptions, and applies the approach-based
exposure definitions used by the Skalkidou-lab MHT studies.

It operates on person-week skeletons of the form `swereg` produces, but takes
them as plain `data.table` arguments and **does not depend on `swereg`**.

## Installation

```r
pak::pak("skalkidou-lab/mht")
```

## Documentation

Reference documentation: <https://skalkidou-lab.github.io/mht/>

## Migration from swereg

The MHT code was previously part of `swereg`. Function names drop the `mht`
infix, because the package name now supplies it.

| Old name | New name |
|---|---|
| `swereg::x2023_mht_add_lmed` | `mht::x2023_add_lmed` |
| `swereg::x2026_mht_add_lmed` | `mht::x2026_add_lmed` |

Arguments and return values are unchanged.

## License

MIT. See `LICENSE`.
