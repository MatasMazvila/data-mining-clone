import json
import os


API_KEY = "your_api_key"
CACHE_DIR = "api_cache"

PUBLISHED_AFTER = "2024-01-01T00:00:00Z"
PUBLISHED_BEFORE = "2024-10-01T00:00:00Z"

THRESHOLD_CHANNEL_SUBS = 10**4
THRESHOLD_CHANNEL_VIEWS = 10**6
THRESHOLD_VIDEO_VIEWS = 10**3

THRESHOLD_PERIOD_VIEWS = 10**5
THRESHOLD_PERIOD_COMMENTS = 10**2

CATEGORIES = {
    "1": "Filmai ir animacija",
    "2": "Automobiliai ir transporto priemonės",
    "10": "Muzika",
    "15": "Augintiniai ir kiti gyvūnai",
    "17": "Sportas",
    "18": "Trumpametražiai filmai",
    "19": "Kelionės ir renginiai",
    "20": "Žaidimai",
    "21": "Vaizdo įrašų tinklaraščio kūrimas",
    "22": "Žmonės ir tinklaraščiai",
    "23": "Komedijos",
    "24": "Pramogos",
    "25": "Naujienos ir politika",
    "26": "Kaip ką nors padaryti ir stiliaus patarimai",
    "27": "Švietimas",
    "28": "Mokslas ir technologijos",
    "30": "Filmai",
    "31": "Anime animacija",
    "32": "Veiksmas nuotykiai",
    "33": "Klasika",
    "34": "Komedijos",
    "35": "Dokumentika",
    "36": "Drama",
    "37": "Šeimai",
    "38": "Užsienio",
    "39": "Siaubo",
    "40": "Mokslinė fantastika maginė fantastika",
    "41": "Trileris",
    "42": "Trumpi filmai",
    "43": "Pramoginės laidos",
    "44": "Anonsai",
}


OUTLIERS = [
    # Ukraine news channels
    "UC7Elc-kLydl-NAV4g204pDQ",
    "UCgxTPTFbIbCWfTR9I2-5SeQ",
    "UCTVk323gzizpujtn2T_BL7w",
    "UCS-cgYslpMpH5FkxJ2e0Vpg",
    "UCjQdM9q_Vd2gBN9Xy_zRDJQ",
]


class Cache:
    def __init__(self, cache_dir_name, cache_id):
        self.cache_dir = os.path.join(CACHE_DIR, cache_dir_name)
        os.makedirs(self.cache_dir, exist_ok=True)
        self.cache_file = os.path.join(self.cache_dir, f"{cache_id}.json")
        self.data = None
        self.cached = os.path.exists(self.cache_file)

    def __enter__(self):
        if self.cached:
            with open(self.cache_file, "r") as f:
                self.data = json.load(f)
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        if not self.cached and self.data is not None:
            with open(self.cache_file, "w") as f:
                json.dump(self.data, f)
