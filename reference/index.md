# Package index

## MHT exposure entry points

One function per study definition. Each takes a person-week skeleton and
a table of dispensed prescriptions, and writes the derived exposure
columns onto the skeleton BY REFERENCE. They differ in the LMED
identifier column they read, in how the product categories are flagged,
and in whether the `rd_approach*` exposure variables are produced.

- [`add_lmed_v20260828()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20260828.md)
  : Add 2026-08-28 MHT exposure variables to a person-week skeleton
- [`add_lmed_v20250909()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20250909.md)
  : Add 2026 MHT exposure variables to a person-week skeleton
- [`add_lmed_v20230509()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20230509.md)
  : Add 2023 MHT exposure variables to a person-week skeleton

## Synthetic fixtures

Small synthetic tables carrying no real person data, used by the
examples and by the test suite. The prescription tables differ only in
the name of the person identifier column.

- [`fake_lmed_2026`](https://skalkidou-lab.github.io/mht/reference/fake_lmed_2026.md)
  : Synthetic LMED prescriptions in the 2026 column layout
- [`fake_lmed_2023`](https://skalkidou-lab.github.io/mht/reference/fake_lmed_2023.md)
  : Synthetic LMED prescriptions in the 2023 column layout
- [`fake_skeleton_mht`](https://skalkidou-lab.github.io/mht/reference/fake_skeleton_mht.md)
  : Synthetic person-week skeleton
