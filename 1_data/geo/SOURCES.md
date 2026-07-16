# 1_data/geo/ — asset provenance

## pk_districts_adm2.geojson (real ADM2 boundaries, Phase 5B)

| | |
|---|---|
| Source | geoBoundaries (gbOpen), www.geoboundaries.org |
| Dataset | PAK ADM2 "Districts", boundaryID `PAK-ADM2-60131773`, 2019 vintage |
| File | `geoBoundaries-PAK-ADM2_simplified.geojson`, release commit `9469f09` |
| URL | https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/PAK/ADM2/geoBoundaries-PAK-ADM2_simplified.geojson |
| License | **Public Domain** (geoBoundaries open product; safe to track in a public repo) |
| Retrieved | 2026-07-16 (raw sha256 `f274cfc377b3b570b6fd620c2911b5e4acd14460f9cb48551747f1d698566f2e`) |
| Units | 126 districts (126 unique names; Polygon/MultiPolygon; lon 63.3–77.0, lat 24.3–36.2) |

**Transformation applied (2026-07-16, Phase 5B):** properties reduced to a
single `district` field (= geoBoundaries `shapeName`, unchanged values);
geometry untouched; JSON compacted. Nothing else. To reproduce: download the
URL above and rewrite each feature's properties to
`{"district": <shapeName>}`.

Citation (per geoBoundaries request): Runfola D. et al. (2020)
geoBoundaries: A global database of political administrative boundaries.
PLoS ONE 15(4): e0231866.

Note: geoBoundaries names differ from common usage in places (e.g. canonical
`Islamabad Capital Territory`, spelling `Vihari`). `cerp_norm()` /
`cerp_harmonize()` absorb the known cases (suffix stripping resolves ICT →
Islamabad without a crosswalk row); anything unmatched fails loudly at render
time — add a row to `district_lookup.csv` then.

## pk_districts_demo.geojson

Hand-made 5-district stylized demo geometry (in-repo, no external source).
Used by the `choropleth_demo` config entry to exercise messy-name
harmonization.

## pk_districts_adm2.geojson — previous version (pre-5B)

Until 2026-07-16 this path held stylized demo geometry (34 real district
names at approximate shapes, generated in-repo). Recoverable from git history
before the Phase 5B commit.

## district_lookup.csv

Versioned, human-reviewed raw→canonical name crosswalk (header comment in the
file documents versioning + changelog rules).
