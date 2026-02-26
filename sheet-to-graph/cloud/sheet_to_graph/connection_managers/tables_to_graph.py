from sheet_to_graph.connection_manager import ConnectionManager
from sheet_to_graph.cypher_translator import CypherTranslator
from sheet_to_graph.excel_writer import ExcelWriter


class TablesToGraph(ConnectionManager):
    """This class manages a connection between a list of tables and a Neo4j database.
    Use translate_and_upload to validate the tables, translate them into Cypher,
    upload the data to the database, and then run additional inference_queries.

    Initialize with:
    - tables: a list of objects of the sheet_to_graph.Table class.
    - inference_queries: a list of strings written in Cypher.
    - credentials_file_name: the name of the file where the database credentials are.
      the file should be in json format with fields uri, user, password
    - query_file_name: the name of a file where the generated Cypher queries are saved.
    """

    def __init__(
        self,
        *tables,
        inference_queries: list = None,
        credentials_file_name: str = "",
        query_file_name: str = "queries.txt",
    ):
        self.tables = tables
        self.inference_queries = [] if inference_queries is None else inference_queries
        self.credentials_file_name = credentials_file_name
        self.query_file_name = query_file_name
        self.queries = None

    def translate_and_upload(
        self,
        stop_if_validation_fails: bool = True,
        output_spreadsheet_name: str = None,
    ):
        self.validate_tables(stop_if_validation_fails)
        if output_spreadsheet_name is not None:
            self.save_to_spreadsheet(output_spreadsheet_name)
        self.translate_tables_into_cypher_queries()
        self.upload_to_neo4j_database()

    def validate_tables(self, stop_if_validation_fails: bool):
        print("Validating data")
        for table in self.tables:
            number_of_warnings = len(table.validation_warnings)
            print(f"{number_of_warnings} warnings for table {table.name}")
            if len(table.validation_warnings) > 0:
                print(f"Warnings for table: {table.name}")
                for warning in table.validation_warnings:
                    print(warning)
        validation_fails = False
        for table in self.tables:
            number_of_errors = len(table.validation_errors)
            print(f"{number_of_errors} errors for table {table.name}")
            if len(table.validation_errors) > 0:
                print(f"Errors for table: {table.name}")
                for error in table.validation_errors:
                    print(error)
                validation_fails = True
        if stop_if_validation_fails and validation_fails:
            raise Exception("Not all tables passed validation")

    def save_to_spreadsheet(self, output_spreadsheet_name: str):
        print("Saving Data to Spreadsheet")
        excel_writer = self._initialize_excel_writer(output_spreadsheet_name)
        excel_writer.save_sheets({table.name: table for table in self.tables})

    def translate_tables_into_cypher_queries(self):
        print("Translating tables into cypher queries")
        translator = self._initialize_cypher_translator()
        self.queries = translator.translate_tables(*self.tables)
        number_of_queries = len(self.queries)
        print(f"Generated {number_of_queries} queries")

    def upload_to_neo4j_database(self):
        print("Connecting to Neo4j database")
        neo4j_connection = self._initialize_neo4j_connection()
        print("Uploading data to Neo4j database")
        neo4j_connection.open()
        for query in self.queries + self.inference_queries:
            neo4j_connection.run_query(query)
        neo4j_connection.close()
        print("Upload complete")

    def _initialize_cypher_translator(self):
        return CypherTranslator(self.query_file_name)

    def _initialize_excel_writer(self, output_spreadsheet_name: str):
        return ExcelWriter(output_spreadsheet_name)
