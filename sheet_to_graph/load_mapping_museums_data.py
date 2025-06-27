import json

import pandas as pd


governance_map = {
    "Government-Cadw": "other government",
    "Government-Local_Authority": "local authority",
    "Government-National": "national",
    "Government-Other": "other government",
    "Independent-Not_for_profit": "not-for-profit",
    "Independent-English_Heritage": "English Heritage",
    "Independent-Historic_Environment_Scotland": "Historic Environment Scotland",
    "Independent-National_Trust": "National Trust",
    "Independent-National_Trust_for_Scotland": "National Trust for Scotland",
    "Independent-Private": "private",
    "Independent-Unknown": "unknown governance",
    "University": "university",
    "Unknown": "unknown governance",
}

governance_broad_map = {
    "Government-Cadw": "other government",
    "Government-Local_Authority": "local authority",
    "Government-National": "national",
    "Government-Other": "other government",
    "Independent-Not_for_profit": "independent",
    "Independent-English_Heritage": "independent",
    "Independent-Historic_Environment_Scotland": "independent",
    "Independent-National_Trust": "independent",
    "Independent-National_Trust_for_Scotland": "independent",
    "Independent-Private": "private",
    "Independent-Unknown": "unknown governance",
    "University": "university",
    "Unknown": "unknown governance",
}


def size_map(size):
    if size == "unknown":
        return "unknown size"
    return size


def subject_map(subject):
    return (
        subject.lower()
        .replace("_", " ")
        .replace("-", ": ")
        .replace(" and ", " & ")
        .replace(": large houses", " (large)")
        .replace(": medium houses", " (medium)")
        .replace(": small houses", " (small)")
    )


def subject_broad_map(subject):
    return subject_map(subject).split(":")[0]


def country_map(region):
    if region not in (
        "Channel Islands",
        "Isle of Man",
        "Northern Ireland",
        "Scotland",
        "Wales",
    ):
        return "England"
    return region


def region_map(region):
    if region == "Yorkshire and The Humber":
        return "Yorks & Humber"
    return region


def english_county_map(admin_area):
    try:
        return (
            [area for area in admin_area.split("/") if "(English County)" in area][0]
            .replace("(English County)", "")
            .strip()
        )
    except IndexError:
        pass
    return None


if __name__ == "__main__":
    with open("config.json") as f:
        config = json.load(f)

    mapping_museums_file = config["mapping_museums_file"]
    mapping_museums_data = pd.read_csv(mapping_museums_file)

    museums_data = pd.DataFrame(
        {
            "museum_id": mapping_museums_data["museum_id"],
            "museum_name": mapping_museums_data["Name_of_museum"],
            "governance": mapping_museums_data["Governance"].map(governance_map),
            "governance_broad": mapping_museums_data["Governance"].map(
                governance_broad_map
            ),
            "size": mapping_museums_data["Size"].map(size_map),
            "subject": mapping_museums_data["Subject_Matter"].map(subject_map),
            "subject_broad": mapping_museums_data["Subject_Matter"].map(
                subject_broad_map
            ),
            "accreditation": mapping_museums_data["Accreditation"].map(
                lambda x: x.lower()
            ),
            "english_county": mapping_museums_data["Admin_area"].map(
                english_county_map
            ),
            "region": mapping_museums_data["Region_country"].map(region_map),
            "country": mapping_museums_data["Region_country"].map(country_map),
            "year_opened_1": mapping_museums_data["Year_opened"].map(
                lambda x: x.split(":")[0]
            ),
            "year_opened_2": mapping_museums_data["Year_opened"].map(
                lambda x: x.split(":")[1]
            ),
            "year_closed_1": mapping_museums_data["Year_closed"].map(
                lambda x: x.split(":")[0]
            ),
            "year_closed_2": mapping_museums_data["Year_closed"].map(
                lambda x: x.split(":")[1]
            ),
            "address_1": mapping_museums_data["Address_line_1"],
            "address_2": mapping_museums_data["Address_line_2"],
            "address_3": mapping_museums_data["Address_line_3"],
            "village_town_city": mapping_museums_data["Village,_Town_or_City"],
            "postcode": mapping_museums_data["Postcode"],
        }
    )

    museums_data.to_csv(config["sheets"]["museums"]["file"])
