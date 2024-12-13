import json

from .neo4j_connection import Neo4jConnection


class ConnectionManager:
    """This class manages a connection with a Neo4j database.

    Initialize the class with:
    - credentials_file_name: the name of the file where the database credentials are.
      the file should be in json format with fields uri, user, password
    """

    def __init__(self, credentials_file_name: str = ""):
        self.credentials_file_name = credentials_file_name

    def _initialize_neo4j_connection(self):
        with open(self.credentials_file_name, "r") as f:
            credentials = json.load(f)
        return Neo4jConnection(credentials)
