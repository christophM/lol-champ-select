from pymongo import MongoClient
import pandas as pd
import numpy as np
import requests
import re
import json
from collections import Counter


N_GAMES = 100000000

## connect
client = MongoClient()
lol = client.lol
match_db = lol.match

## Match query
match_query = {"patch": {"$in": ["5.24"]}}
match_projection = {"participants": 1,
                    "teams": 1,
                    "queueType": 1,
                    "region": 1,
                    "season": 1,
                    "matchDuration": 1,
                    "matchVersion": 1}


## create a cursor
cur = match_db.find(match_query, match_projection)

match = cur.next()


## TODO:
## Create patch variable from matchVersion

## get champion data:
url = "https://global.api.pvp.net/api/lol/static-data/euw/v1.2/champion?api_key=cf3bb3f9-cf43-4361-ab6a-9eedbf0904a3"
champs_dict = requests.get(url).json()["data"]
my_champ_dict = {}

for key, value in champs_dict.iteritems():
    my_champ_dict.update({value["id"]: value["name"]})
with open("./app/model/champions.json", "w") as f:
    json.dump(my_champ_dict, f)



champ_role_counter = {
    "TOP_SOLO": Counter(),
    "JUNGLE_NONE": Counter(),
    "MIDDLE_SOLO": Counter(),
    "BOTTOM_DUO_SUPPORT": Counter(),
    "BOTTOM_DUO_CARRY": Counter()
    }


def is_meta_role(lane, role):
    is_meta = True
    if (lane == "BOTTOM" and role in ["NONE", "DUO", "DUO_NONE", "SOLO"]) or (lane in ["MIDDLE", "TOP"] and role in ["NONE", "DUO", "DUO_CARRY", "DUO_SUPPORT"]):
         is_meta = False
    return is_meta


## Function to map match to champions + winner info
def match_to_champ_dict(cur):

    ## TODO:
    ## Filter games that are not meta

    match = cur.next()
    is_meta_game = all([is_meta_role(x["timeline"]["lane"], x["timeline"]["role"]) for x in match["participants"]])
    if not is_meta_game:
        return dict()
    champion_roles = [my_champ_dict[x["championId"]] + "_" + x["timeline"]["lane"] + "_" + x["timeline"]["role"]
                       for x in match["participants"]]

    for participant in match["participants"]:
        role = participant["timeline"]["lane"] + "_" + participant["timeline"]["role"]
        champ_role_counter[role].update([my_champ_dict[participant["championId"]]])

    ## 1 if in team 100, else -1
    team_membership = [str(x["teamId"]) for x in match["participants"]]
    lane_role = [x["timeline"]["lane"] + "_" + x["timeline"]["role"] for x in match["participants"]]
    champ_dict = dict(zip(champion_roles, team_membership))

    ## create matrix (10 X 4) with id, winner, champion, team_indicator
    match_repr = {"match_id": str(match["_id"]),
                  "winner_team100": match["teams"][0]["winner"],
                  "season": match["season"],
                  "queueType": match["queueType"],
                  "region": match["region"],
                  "matchDuration": match["matchDuration"]
                  }

    match_repr.update(champ_dict)

    return match_repr

n_games = min(N_GAMES, cur.count())
print "Processing %i games" % (n_games)
matches = [match_to_champ_dict(cur) for x in range(0,n_games - 1)]


for key, value in champ_role_counter.iteritems():
    champ_role_counter[key] = dict(value)

with open('./app/model/champ_role_counter.json', 'w') as outfile:
     json.dump(champ_role_counter, outfile)






## create pandas data set
matches = pd.DataFrame.from_records(matches)

matches.set_index("match_id", inplace=True)
matches.fillna(0, inplace=True)


non_champion_vars = ["match_id", "winner_team100", "season", "queueType", "region", "matchDuration"]
champion_vars = np.setdiff1d(matches.columns.values, non_champion_vars)

# matches_switched = matches.copy()
# matches_switched["winner_team100"] = matches_switched["winner_team100"].map(lambda x: not x)

# champ_switch = {"100": "200", "200": "100", 0: 0}
# matches_champions_switched = matches_switched[champion_vars].applymap(lambda x: champ_switch[x])
# matches_switched[champion_vars] = matches_champions_switched

## matches_double = pd.concat([matches, matches_switched])

champions_df = pd.get_dummies(matches[champion_vars])

matches.drop(champion_vars, axis=1, inplace=True)
matches = pd.concat([matches, champions_df], axis=1)

## Drop indicator columns with information: No team had a particular champion
zero_cols = [col for col in matches.columns if "_0" in col]
matches.drop(zero_cols, axis=1, inplace=True)

## store on disk
matches.to_csv("champion-wins.csv")
