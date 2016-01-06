import requests
import pandas as pd


## get champion data:
url = "https://global.api.pvp.net/api/lol/static-data/euw/v1.2/champion?api_key=cf3bb3f9-cf43-4361-ab6a-9eedbf0904a3&champData=all"
champs_dict = requests.get(url).json()["data"]



def extract_champ(champ):
    champ_data = champs_dict[champ]
    champ_d = {
        "champ_name": champ_data["name"],
        "tag1": champ_data["tags"][0],
        "tag2": champ_data["tags"][1] if len(champ_data["tags"]) > 1 else "",
        "id": champ_data["id"]
        }
    stats = champ_data["info"]
    champ_d.update(stats)
    return(champ_d)



champ_infos = [extract_champ(champ) for champ in champs_dict.keys()]

champ_df = pd.DataFrame(champ_infos)

champ_df.to_csv("champ-info.csv", index=False)



