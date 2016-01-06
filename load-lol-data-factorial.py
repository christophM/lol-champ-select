from pymongo import MongoClient
import pandas as pd
import numpy as np
import requests
import re
import json
from collections import Counter


N_GAMES = 1000000

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

    champ_dict = {}
    for x in match["participants"]:
        role = x["timeline"]["lane"] + "_" + x["timeline"]["role"] + "_" + str(x["teamId"])
        champ = my_champ_dict[x["championId"]]
        champ_dict.update({role: champ})

    for participant in match["participants"]:
        role = participant["timeline"]["lane"] + "_" + participant["timeline"]["role"]
        champ_role_counter[role].update([my_champ_dict[participant["championId"]]])

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



## copy matches
switch1 = ["TOP_SOLO_100", "JUNGLE_NONE_100", "MIDDLE_SOLO_100", "BOTTOM_DUO_CARRY_100", "BOTTOM_DUO_SUPPORT_100"]
switch2 = ["TOP_SOLO_200", "JUNGLE_NONE_200", "MIDDLE_SOLO_200", "BOTTOM_DUO_CARRY_200", "BOTTOM_DUO_SUPPORT_200"]

matches["blue_side"] = True

matches_copy = matches.copy()
winner_team200 = matches.winner_team100.apply(lambda x: not x)
champs1 = matches[switch1]
champs2 = matches[switch2]

matches_copy[switch1] = champs2
matches_copy[switch2] = champs1

matches_copy["match_id"] = matches.match_id + "_copy"
matches_copy["winner_team100"] = winner_team200

matches_copy["blue_side"] = False


matches = matches.append(matches_copy)



matches.set_index("match_id", inplace=True)
matches.fillna(0, inplace=True)

## store on disk
matches.to_csv("./app/matches/champion-wins-factorial.csv")
