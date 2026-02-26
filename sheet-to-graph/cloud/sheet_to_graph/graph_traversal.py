from functools import lru_cache

import pandas as pd


def make_get_ancestors(lookup_table: dict) -> callable:
    """Create a function that returns all ancestors of a node in a hierarchy.
    Args:
        lookup_table (dict): A dictionary mapping node IDs to their parent IDs.
    Returns:
        callable: A function that takes a node ID and returns a list of its ancestor IDs.
    """

    @lru_cache(None)
    def _get_ancestors(node_id: str) -> list:
        ancestors = []
        current_id = node_id
        while True:
            parent_id = lookup_table.get(current_id)
            if pd.isna(parent_id) or parent_id == "":
                break
            ancestors.append(parent_id)
            current_id = parent_id
        return ancestors

    return _get_ancestors


def make_get_core_type(lookup_table: dict, core_types: list) -> callable:
    """Create a function that returns the core type of a given type ID.
    Args:
        lookup_table (dict): A dictionary mapping type IDs to their parent type IDs.
        core_types (list): A list of core type IDs.
    Returns:
        callable: A function that takes a type ID and returns its core type ID.
    """

    get_ancestors = make_get_ancestors(lookup_table)

    def _get_core_type(type_id: str) -> str:
        if type_id in core_types:
            return type_id
        for ancestor_id in get_ancestors(type_id):
            if ancestor_id in core_types:
                return ancestor_id
        return None

    return _get_core_type


def make_get_ultimate_ancestor(lookup_table: dict) -> callable:
    """Create a function that returns the ultimate ancestor of a node in a hierarchy.
    Args:
        lookup_table (dict): A dictionary mapping node IDs to their parent IDs.
    Returns:
        callable: A function that takes a node ID and returns its ultimate ancestor ID.
    """
    get_ancestors = make_get_ancestors(lookup_table)

    def _get_ultimate_ancestor(node_id: str) -> str:
        ancestors = get_ancestors(node_id)
        if ancestors:
            return ancestors[-1]
        return None

    return _get_ultimate_ancestor
