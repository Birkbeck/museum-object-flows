import requests


class WikidataConnection:
    def __init__(self, email):
        self.session = requests.Session()
        self.session.headers.update(
            {
                "User-Agent": f"museum-object-flows/1.0 (contact: {email})",
                "Accept": "application/json",
            }
        )

    def search_entities(self, search_term, limit=3):
        url = "https://www.wikidata.org/w/api.php"
        params = {
            "action": "wbsearchentities",
            "format": "json",
            "language": "en",
            "search": search_term,
            "limit": limit,
        }
        response = self.session.get(url, params=params, timeout=10)
        response.raise_for_status()
        response_data = response.json()
        return [
            {
                "id": item.get("id"),
                "label": item.get("label"),
                "description": item.get("description"),
            }
            for item in response_data.get("search", [])
        ]

    def get_entity_properties(self, qid):
        url = f"https://www.wikidata.org/wiki/Special:EntityData/{qid}.json"
        response = self.session.get(url)
        response.raise_for_status()
        response_data = response.json()
        entity = response_data["entities"][qid]
        claims = entity.get("claims", {})
        properties = {}
        for pid, statements in claims.items():
            for stmt in statements:
                mainsnak = stmt.get("mainsnak", {})
                if mainsnak.get("datatype") == "wikibase-item":
                    try:
                        value_qid = mainsnak["datavalue"]["value"]["id"]
                        properties[pid] = value_qid
                    except KeyError:
                        continue
                else:
                    try:
                        value = mainsnak["datavalue"]["value"]
                        properties[pid] = value
                    except KeyError:
                        continue
        return properties
