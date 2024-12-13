import json

from sheet_to_graph import Neo4jConnection


if __name__ == "__main__":
    with open("config.json") as f:
        config = json.load(f)
        credentials_file_name = config["credentials_file"]

    with open(credentials_file_name, "r") as f:
        credentials = json.load(f)

    print("Connecting to Neo4j database")
    neo4j_connection = Neo4jConnection(credentials)

    print("Deleting all data in database")
    neo4j_connection.delete_everything()

    print("Complete")
