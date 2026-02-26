import json


class CypherTranslator:
    """This class translates objects of the class sheet_to_graph.Table into Cypher queries.
    Use the method transalte_tables to carry out the translation.
    The cypher queries will be stored in self.output_file_name
    """

    def __init__(self, output_file_name):
        self.output_file_name = output_file_name

    def translate_tables(self, *tables) -> list:
        with open(self.output_file_name, "w") as output_file:
            for table in tables:
                self._generate_nodes(table, output_file)
            for table in tables:
                self._generate_relationships(table, output_file)
        with open(self.output_file_name, "r") as output_file:
            return output_file.readlines()

    def _generate_nodes(self, table, output_file):
        for row in table:
            nodes = self._get_nodes_from_row(table, row)
            self._add_properties_and_labels_to_nodes(table, row, nodes)
            self._translate_nodes_to_cypher_queries(nodes, output_file)

    def _generate_relationships(self, table, output_file):
        for row in table:
            relationships = self._get_relationships_from_row(table, row)
            self._add_properties_and_labels_to_relationships(table, row, relationships)
            self._translate_relationships_to_cypher_queries(relationships, output_file)

    def _get_nodes_from_row(self, table, row):
        # nodes = {primary_key: {"type_labels": [...], "properties": {k: v, ...}}}
        nodes = {}
        for column in table.columns.values():
            if column.primary_key:
                node_id = row[column.name]
                if node_id is None or node_id == "":
                    continue
                nodes[node_id] = {}
                nodes[node_id]["type_labels"] = [column.type_label]
                nodes[node_id]["properties"] = {column.name: json.dumps(node_id)}
        return nodes

    def _add_properties_and_labels_to_nodes(self, table, row, nodes):
        for column in table.columns.values():
            if column.ignore:
                continue
            if column.type_label_of is not None:
                type_label = row[column.name]
                node_id = row[column.type_label_of]
                try:
                    nodes[node_id]["type_labels"].append(type_label)
                except KeyError:
                    pass
            if column.property_of is not None:
                property_value = (
                    json.dumps(row[column.name])
                    if row[column.name] is not None
                    else None
                )
                node_id = row[column.property_of]
                try:
                    if property_value is not None:
                        nodes[node_id]["properties"][column.name] = property_value
                except KeyError:
                    pass

    def _translate_nodes_to_cypher_queries(self, nodes, output_file):
        for node_details in nodes.values():
            node_types = ":".join(
                [type_label for type_label in node_details["type_labels"]]
            )
            property_assignments = ", ".join(
                [f"{k}: {v}" for k, v in node_details["properties"].items()]
            )
            cypher_query = f"MERGE (node:{node_types} {{{property_assignments}}})"
            output_file.write(cypher_query + "\n")

    def _get_relationships_from_row(self, table, row):
        # relationships = {col_name: {"type_labels": [], "properties": {}, "from...", "to..."}}
        relationships = {}
        for column in table.columns.values():
            if column.relation_from is not None or column.relation_to is not None:
                relationships[column.name] = {}
                relationships[column.name]["type_labels"] = [column.type_label]
                relationships[column.name]["properties"] = {}
            if column.relation_from is not None:
                # if column is a "relation from" column, from node is in relation_from
                relationships[column.name]["from_type"] = table.columns[
                    column.relation_from
                ].type_label
                relationships[column.name]["from_key"] = column.relation_from
                relationships[column.name]["from_value"] = row[column.relation_from]
                # if column is a "relation from" column, to node is in reference column
                relationships[column.name][
                    "to_type"
                ] = column.reference_column.type_label
                relationships[column.name]["to_key"] = column.reference_column.name
                relationships[column.name]["to_value"] = row[column.name]
            if column.relation_to is not None:
                # if column is a "relation to" column, from node is in reference column
                relationships[column.name][
                    "from_type"
                ] = column.reference_column.type_label
                relationships[column.name]["from_key"] = column.reference_column.name
                relationships[column.name]["from_value"] = row[column.name]
                # if column is a "relation to" column, to node is in relation_to
                relationships[column.name]["to_type"] = table.columns[
                    column.relation_to
                ].type_label
                relationships[column.name]["to_key"] = column.relation_to
                relationships[column.name]["to_value"] = row[column.relation_to]
        return relationships

    def _add_properties_and_labels_to_relationships(self, table, row, relationships):
        for column in table.columns.values():
            if column.ignore:
                continue
            if column.type_label_of is not None:
                type_label = row[column.name]
                relation_id = column.type_label_of
                try:
                    relationships[relation_id]["type_labels"].append(type_label)
                except KeyError:
                    pass
            if column.property_of is not None:
                property_value = row[column.name]
                relation_id = column.property_of
                try:
                    relationships[relation_id]["properties"][
                        column.name
                    ] = property_value
                except KeyError:
                    pass

    def _translate_relationships_to_cypher_queries(self, relationships, output_file):
        for relation_details in relationships.values():
            from_type = relation_details["from_type"]
            from_key = relation_details["from_key"]
            from_value = relation_details["from_value"]
            to_type = relation_details["to_type"]
            to_key = relation_details["to_key"]
            to_value = relation_details["to_value"]
            relation_type = relation_details["type_labels"][0]
            if (
                from_value == ""
                or from_value is None
                or to_value == ""
                or to_value is None
            ):
                continue
            property_assignments = ", ".join(
                [f'{k}: "{v}"' for k, v in relation_details["properties"].items()]
            )
            cypher_query = (
                f'MATCH (from:{from_type} {{{from_key}: "{from_value}"}})'
                + " WITH from"
                + f' MATCH (to:{to_type} {{{to_key}: "{to_value}"}})'
                + f" MERGE (from)-[:{relation_type} {{{property_assignments}}}]->(to)"
            )
            output_file.write(cypher_query + "\n")
