library("jsonlite")

MODEL_DIR <- "./model"

ROLE_CHAMP_MINIMUM_THRESHOLD <- 300
LANE_ROLES = c("TOP_SOLO", "JUNGLE_NONE", "MIDDLE_SOLO", "BOTTOM_DUO_CARRY", "BOTTOM_DUO_SUPPORT")
TEAM_LANE_ROLES = c(paste0(LANE_ROLES, "_100"), paste0(LANE_ROLES, "_200"))

champion_lane_combos <- read.csv(sprintf("%s/champion_lane_combos.csv", MODEL_DIR), stringsAsFactor=FALSE)$features

champions = as.vector(unlist(fromJSON(sprintf("%s/champions.json", MODEL_DIR))))
champ_role_counter <-  fromJSON(sprintf("%s/champ_role_counter.json", MODEL_DIR))

max_champ_role_count <- max(unlist(champ_role_counter))

champions_top <- sort(names(champ_role_counter$TOP_SOLO[champ_role_counter$TOP_SOLO > ROLE_CHAMP_MINIMUM_THRESHOLD]))
champions_middle <- sort(names(champ_role_counter$MIDDLE_SOLO[champ_role_counter$MIDDLE_SOLO > ROLE_CHAMP_MINIMUM_THRESHOLD]))
champions_jungle <- sort(names(champ_role_counter$JUNGLE_NONE[champ_role_counter$JUNGLE_NONE> ROLE_CHAMP_MINIMUM_THRESHOLD]))
champions_support <- sort(names(champ_role_counter$BOTTOM_DUO_SUPPORT[champ_role_counter$BOTTOM_DUO_SUPPORT> ROLE_CHAMP_MINIMUM_THRESHOLD]))
champions_carry <- sort(names(champ_role_counter$BOTTOM_DUO_CARRY[champ_role_counter$BOTTOM_DUO_CARRY> ROLE_CHAMP_MINIMUM_THRESHOLD]))

champions_per_pos <- list(champions_top,champions_jungle, champions_middle, 
                       champions_carry, champions_support)
names(champions_per_pos) <- LANE_ROLES
champions_per_team_pos <- c(champions_per_pos, champions_per_pos)
names(champions_per_team_pos) <- TEAM_LANE_ROLES
