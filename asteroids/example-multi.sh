#!/bin/sh

./playgame.py --verbose --fill --log_input --log_output --log_error --log_dir game_logs --turns 500 --map_file maps/test/multi_p04_01.map "python dist/starter_bots/python/MyBot.py"
