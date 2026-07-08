# Raw external data

This folder is where downloaded third-party inputs should live locally (they are not committed to git — see repository `.gitignore` conventions once added for large binary sources):

- PSMSL tide-gauge station records (Charchanga #1016, Cox's Bazar #1397, Hiron Point #203) — https://psmsl.org/
- GADM v4.1 administrative boundaries (Bangladesh, district level) — https://gadm.org/
- WorldPop 2020 gridded population — https://www.worldpop.org/
- SRTM 30 m DEM — https://earthexplorer.usgs.gov/
- IPCC AR6 WGI Chapter 9 SSP sea-level projection tables

A small `00_download_data.R` script that fetches these programmatically (where licensing allows) would be a good addition once scripts 01–06 are assembled.
