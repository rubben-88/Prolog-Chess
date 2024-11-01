import os
import re
import json
import time

##############
### USAGE: ###
############## 

# 1) Install this extension: https://marketplace.visualstudio.com/items?itemName=JustinFrizzell.keyword-context
# 2) Fill in the path to vscode's settings.json and the src-folder with the prolog files.
settings_path = 'C:/Users/vanda/AppData/Roaming/Code/User/settings.json'
src_path = 'C:/Users/vanda/Projects/project2324-rubben-88/src'
# 3) Set how often to check for new predicates (in seconds)
new_check = 60
# 4) Set the end type ("\n" for linux, "\n\r" for windows)
end = "\n\r"
# 5) Run this script. It will provide a tooltip for predicates like following example:
#       :- module(board, [
#           init_board/1,                   % -Board
#           board_positions/1,              % -Positions
#           piece_at/3,                     % ?Position, +Board, ?Piece
#           filter_in_bounds/2,             % +Positions, -Filtered
#           filter_not_blocked_positions/5, % +Board, +AbsPos, +SelfPositions, +OtherPositions, -Filtered
#           if_pawn_add_positions/3,        % AbsPos, Board, Positions
#           positions_of_color/3            % Color, Board, Positions
#       ]).

#######################
### IMPLEMENTATION: ###
####################### 

def find_modules_and_predicates():

    regex_pattern = r':- module\([^\[]*\[([^\]]*)'
    matches = []
    for root, _, files in os.walk(src_path):
        for file in files:
            if file.endswith(".pl"):
                file_path = os.path.join(root, file)
                with open(file_path, "r") as f:
                    content = f.read()
                    matches.extend(re.findall(regex_pattern, content))

    transformed_match = ''.join(matches).replace(" ", "").replace("\t", "").replace("\r", "").split('\n')
    transformed_match = [x for x in transformed_match if x.strip()]

    predicates = []
    for x in transformed_match:
        name_args = x.split('%')
        name = name_args[0].split('/', 1)[0]
        args = " ".join(name_args[1].split(','))
        predicates.append([name, args])

    with open(settings_path, 'r') as file:
        data = json.load(file)
    new_map = [
        {
            "keyword": x[0],
            "tooltip": x[1]
        }
        for x in predicates
    ]

    regex_pattern = r':- module\(([^\[]*\[[^\]]*)'
    matches = []
    for root, _, files in os.walk(src_path):
        for file in files:
            if file.endswith(".pl"):
                file_path = os.path.join(root, file)
                with open(file_path, "r") as f:
                    content = f.read()
                    matches.extend(re.findall(regex_pattern, content))

    separated = [x.split('[') for x in matches]
    module_names = [x[0].split(',')[0] for x in separated]
    module_predicates = [x[1].strip().split('\n') for x in separated]
    module_predicates = [[y.split('/', 1)[0].lstrip() for y in x] for x in module_predicates]

    for i in range(len(module_names)):
        new_map.append({
            "keyword": module_names[i],
            "tooltip": end.join(module_predicates[i])
        })

    data['keyword-context.map'] = new_map
    with open(settings_path, 'w') as file:
        json.dump(data, file, indent=4)


#######################
### MAIN: #############
####################### 

if __name__ == "__main__":
    i = 0
    while(True):
        find_modules_and_predicates()
        print(f'updated ({i})')
        i += 1
        time.sleep(new_check)
