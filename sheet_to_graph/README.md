# Sheet To Graph

This is a simple utility to upload museum closure data from spreadsheets into a neo4j graph database.

## Setup

Follow the instructions [here](https://neo4j.com/docs/aura/auradb/getting-started/create-database/) to set up a neo4j database online.

To use the sheet_to_graph tool, you need to create a `credentials.json` file with the uri of the database, the username, and the password. Follow the syntax in `example_credentials.json`:

```
{
    "uri": "neo4j+s://xxxxxxxx.databases.neo4j.io",
    "user": "neo4j",
    "password": "xxxxxx"
}
```

You also need to download the [ONS postcode directory](https://geoportal.statistics.gov.uk/datasets/e14b1475ecf74b58804cf667b6740706) in order for postcodes in the spreadsheet to be mapped onto coordinates. Unzip the CSV collection and place it inside the data directory `../data/ONSPD_FEB_2024_UK/`.

## Uploading Data to the Database

Provide details of the Excel Spreadsheets or CSV files where the data you wish to upload is stored in `config.json`. Default values are already filled in with the names of the data files provided in this repository.

Use the command `make upload-db` to upload data into the neo4j database specified in the credentials file.

## Deleting all Data from the Database

Use the command `make reset-db` to wipe all nodes and relationships from the neo4j database specified in the credentials file.

## Downloading Data from the Database

Use the command `make dump-db` to run pre-specified queries that store the contents of the database in csv files in `../data/query_results`.

## Updating Mapping Museums Data

After updating the Mapping Museums data in the `data` directory, update `config.json` with the new file name.

## Troubleshooting

If the Neo4j database has been paused due to inactivity, you will receive `ValueError: Cannot resolve address xxxxxxxx.databases.neo4j.io:xxxx`. Restart the database on the online user interface.

You may then need to flush your DNS cache as your computer may remember that the address is unavailable. On Mac OS:

```
sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder
```