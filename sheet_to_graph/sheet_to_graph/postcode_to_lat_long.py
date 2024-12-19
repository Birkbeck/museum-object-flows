import csv
import json

from bng_latlon import WGS84toOSGB36


class PostcodeToLatLong:
    """This class is used to define a mapping from postcodes to latitudes, longitudes,
    and other geographic information from the Office for National Statistics.
    The first time this is run, information is loaded from the ONS postcode directory.
    Information used by the sheet is saved in self.saved_geo_info_file_name.
    This saves time in future uploads.

    Provide the location of where the ONS postcode directory is saved on your machine.
    Download from:
    https://geoportal.statistics.gov.uk/datasets/e14b1475ecf74b58804cf667b6740706/about
    """

    regions_map = {
        "E12000001": "North East",
        "E12000002": "North West",
        "E12000003": "Yorkshire and The Humber",
        "E12000004": "East Midlands",
        "E12000005": "West Midlands",
        "E12000006": "East of England",
        "E12000007": "London",
        "E12000008": "South East",
        "E12000009": "South West",
        "S99999999": "Scotland",
        "W99999999": "Wales",
        "N99999999": "Northern Ireland",
        "L99999999": "Channel Islands",
        "M99999999": "Isle of Man",
    }

    def __init__(self, saved_geo_info_file_name: str, postcode_directory_path: str):
        self.saved_geo_info_file_name = saved_geo_info_file_name
        self.postcode_directory_path = postcode_directory_path
        self.saved_geo_info = None
        self._lads_map = None

    def get_latitude(self, postcode: str):
        return self._get_geo_info(postcode)["lat"]

    def get_longitude(self, postcode: str):
        return self._get_geo_info(postcode)["long"]

    def get_bng_x(self, postcode: str):
        return self._get_geo_info(postcode)["bng_x"]

    def get_bng_y(self, postcode: str):
        return self._get_geo_info(postcode)["bng_y"]

    def get_region(self, postcode: str):
        return self._get_geo_info(postcode)["region"]

    def get_local_authority_code(self, postcode: str):
        return self._get_geo_info(postcode)["lad23cd"]

    def get_local_authority_name(self, postcode: str):
        return self._get_geo_info(postcode)["lad23nm"]

    def _get_geo_info(self, postcode: str):
        if self.saved_geo_info is None:
            self._open_saved_geo_info()
        try:
            return self.saved_geo_info[postcode]
        except KeyError:
            self._add_new_postcode(postcode)
            return self._get_geo_info(postcode)

    def _open_saved_geo_info(self):
        try:
            with open(self.saved_geo_info_file_name, "r") as f:
                self.saved_geo_info = json.load(f)
        except (FileNotFoundError, json.decoder.JSONDecodeError):
            self.saved_geo_info = {}

    def _add_new_postcode(self, postcode: str):
        blank_details = {
            "lat": None,
            "long": None,
            "bng_x": None,
            "bng_y": None,
            "region": None,
            "lad23cd": None,
            "lad23nm": None,
        }
        if postcode == "":
            return self._update_saved_geo_info(postcode, blank_details)
        initial_letter = self._get_initial_letters(postcode)
        postcode_file = (
            f"{self.postcode_directory_path}/Data/multi_csv/"
            + f"ONSPD_FEB_2024_UK_{initial_letter}.csv"
        )
        try:
            with open(postcode_file, "r") as f:
                postcode_table = csv.DictReader(f)
                for row in postcode_table:
                    if postcode in {row["pcd"], row["pcd2"], row["pcds"]}:
                        lat = float(row["lat"])
                        lon = float(row["long"])
                        bng = WGS84toOSGB36(lat, lon)
                        return self._update_saved_geo_info(
                            postcode,
                            {
                                "lat": lat,
                                "long": lon,
                                "bng_x": bng[0],
                                "bng_y": bng[1],
                                "region": self.regions_map[row["rgn"]],
                                "lad23cd": row["oslaua"],
                                "lad23nm": self.lads_map.get(row["oslaua"], None),
                            },
                        )
        except FileNotFoundError:
            print(f"No postcode directory found for postcode '{initial_letter}'")
        except Exception as e:
            print(str(e))
        return self._update_saved_geo_info(postcode, blank_details)

    def _update_saved_geo_info(self, postcode: str, geo_info: dict):
        self.saved_geo_info[postcode] = geo_info
        with open(self.saved_geo_info_file_name, "w") as f:
            f.write(json.dumps(self.saved_geo_info))

    def _get_initial_letters(self, postcode: str):
        letters = ""
        for character in postcode:
            if character.isalpha():
                letters += character.upper()
            else:
                return letters

    @property
    def lads_map(self):
        if self._lads_map is None:
            lads_map_file_name = "LAD23_LAU121_ITL321_ITL221_ITL121_UK_LU.csv"
            lads_map_file = (
                f"{self.postcode_directory_path}/Documents/{lads_map_file_name}"
            )
            with open(lads_map_file, "r") as f:
                postcode_table = csv.DictReader(f)
                self._lads_map = {
                    row["\ufeffLAD23CD"]: row["LAD23NM"] for row in postcode_table
                }
        return self._lads_map
