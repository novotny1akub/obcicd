# Orienteering Events Viewer

A simple web application to view and filter orienteering events, sourced from the ORIS API. **It is tailored for the CST orienteering club (CST, friends & family) in the Czech Republic.**

## Preview
View the live application here:
[http://raw.githack.com/novotny1akub/obcicd/main/index_api.html](http://raw.githack.com/novotny1akub/obcicd/main/index_api.html)

## Features
- **Data Source**: Fetches event data from the Czech Orienteering Information System (ORIS).
- **Interactive Grid**: Uses AG Grid for sorting, filtering, and displaying event details.
- **Weekend Context**: Automatically labels events relative to the current weekend (e.g., "this PaSoNe", "next PaSoNe", "next2 PaSoNe" for the weekend in 14 days etc.).
- **Distance Calculation**: Shows the distance in km from "Nad Okrouhlíkem" (Prague) to the event location.
- **Map Visualization**: Displays a mini SVG map of the Czech Republic with the event location pinned.
- **Entries & Start Lists**: Shows club entries and start lists with tooltips and modal details (specifically filtering for CST members and tracked individuals).

## Project Structure
- `index_api.html`: The main HTML file containing the application logic and embedded data (generated).
- `ob_api.R`: R script that fetches data from ORIS, processes it, and generates `index_api.html` from the template.
- `docs/index_api_template.html`: The HTML template used by the R script.
