import csv

from sheet_to_graph import ConnectionManager


class QueryToCsv(ConnectionManager):
    """This class manages a connection from a Neo4j database.
    Use make_queries_and_save_outputs to run Cypher queries,
    and output results as CSV files.

    Initialize with:
    - queries: A list of strings written in Cypher.
    - credentials_file_name: the name of the file where the database credentials are.
      the file should be in json format with fields uri, user, password
    - output_directory_name: the name of the directory where CSVs are saved.
    """

    def __init__(
        self,
        queries: dict,
        credentials_file_name: str = "",
        output_directory_name: str = "",
    ):
        self.queries = queries
        self.credentials_file_name = credentials_file_name
        self.output_directory_name = output_directory_name

    def make_queries_and_save_outputs(self):
        neo4j_connection = self._initialize_neo4j_connection()
        for query_name, query in self.queries.items():
            self._make_query_and_save_output(neo4j_connection, query_name, query)
        print("Finished making queries")

    def _make_query_and_save_output(self, neo4j_connection, query_name, query):
        print(f"Making query: {query_name}")
        query_file_name = f"{self.output_directory_name}/{query_name}.csv"
        records = [item["record"] for item in neo4j_connection.run_query(query)]
        try:
            field_names = records[0].keys()
        except IndexError:
            print("no results")
            return
        with open(query_file_name, "w") as f:
            writer = csv.DictWriter(f, fieldnames=field_names)
            writer.writeheader()
            writer.writerows(records)
