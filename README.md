# Museum Object Flows

This repository contains data concerning the flows of objects from closed museums, and source code for tools used in analysis of the data.

The source code in `sheet_to_graph` provides a tool that validates flow data in spreadsheet format and translates it into a Neo4j database.

The source code in `shiny/mappingmuseums` defines an interactive web app for exploring the data.

## The Dataset

The dataset is contained within `data`. It describes the dispersal of collections away from about 500 UK museums in the wake of each museum's closure.

The dataset details the different types of event that collections and objects are involved in and the different types of actor and location that collections flow between.

## Instructions for Use

- Follow the setup instructions inside `sheet_to_graph/readme.md` and `shiny/mappingmuseums/readme.md` to setup the database and web app hosting.
- From this directory you can run the following commands:
  - `make reset-db` - Clears the contents of your Neo4j database.
  - `make upload-db` - Reads data from the spreadsheet, validates it, and uploads it to the Neo4j database.
  - `make dump-db` - Creates 3 csv files containing all data from the Neo4j database (`dispersal_events.csv`, `event_types.csv`, `actor_types.csv`).
  - `make deploy-app-local` - Deploys the Shiny app and opens it in your default web browser.
  - `make deploy-app` - Deploys the Shiny app to a remote server.

## The Museum Closure Project

The collection of data and development of tools in this repository was undertaken as part of the project ["Museum Closure in the UK 2000–2025"](https://mapping-museums.bbk.ac.uk/museum-closure-in-the-uk-2000-2025/), funded by UKRI-AHRC Grant No. AH/X012816/1, October 2023–September 2025.

The data in this repository was collected and curated by Mark Liebenrood and Fiona Candlin. The tools were designed and implemented by George A. Wright, Andrea Ballatore, Alexandra Poulovassilis, and Peter T. Wood.

The creation of the tools in this repository is described in the paper: Wright, G.A. and Ballatore, A. and Poulovassilis, A. and Wood, P.T. Modelling and visualizing flows of cultural objects (submitted for review).

## See Also

- [Mapping Museums](https://github.com/Birkbeck/mapping-museums)
- [Museums in the Pandemic](https://github.com/Birkbeck/museums-in-the-pandemic)