#!/usr/bin/env python
import sys
import traceback
import random

try:
    from sys import maxint
except ImportError:
    from sys import maxsize as maxint

class Asteroids():
    def __init__(self):
        self.turn = None
        self.width = None
        self.height = None
        self.player_id = None
        self.turn_steps = None
        self.m_thrust = None
        self.m_turn = None
        self.ship_radius = None
        self.asteroids = []
        self.bullets = []
        self.ships = []

    def setup(self, data):
        'parse initial input and setup starting game state'
        for line in data.split('\n'):
            line = line.strip().lower()
            if len(line) > 0:
                tokens = line.split()
                key = tokens[0]
                if key == 'turn':
                    self.turn = int(tokens[1])
                elif key == 'width':
                    self.width = int(tokens[1])
                elif key == 'height':
                    self.height = int(tokens[1])
                elif key == 'player_id':
                    self.player_id = int(tokens[1])
                elif key == 'player_seed':
                    random.seed(int(tokens[1]))
                elif key == 'turntime':
                    self.turntime = int(tokens[1])
                elif key == 'loadtime':
                    self.loadtime = int(tokens[1])
                elif key == 'turn_steps':
                    self.turn_steps = int(tokens[1])
                elif key == 'm_thrust':
                    self.m_thrust = float(tokens[1])
                elif key == 'm_turn':
                    self.m_turn = float(tokens[1])
                elif key == 'ship_radius':
                    self.ship_radius = int(tokens[1])

    def update(self, data):
        # clear ant and food data
        self.asteroids = []
        self.bullets = []
        self.ships = []
        self.my_ships = []

        # update map and create new ant and food lists
        for line in data.split("\n"):
            line = line.strip().lower()
            if len(line) > 0:
                tokens = line.split()
                if tokens[0] == "turn":
                    self.turn = int(tokens[1])
                elif len(tokens) >= 3:
                    x = float(tokens[2])
                    y = float(tokens[3])
                    if tokens[0] == "a":
                        self.asteroids.append({
                            "category": int(tokens[1]),
                            "x": float(tokens[2]),
                            "y": float(tokens[3]),
                            "heading": float(tokens[4]),
                            "speed": float(tokens[5])})
                    if tokens[0] == "b":
                        self.bullets.append({
                            "owner": int(tokens[1]),
                            "x": float(tokens[2]),
                            "y": float(tokens[3]),
                            "heading": float(tokens[4]),
                            "speed": float(tokens[5])})
                    elif tokens[0] == "s":
                        ship = { "ship_id": int(tokens[1]),
                                   "x": float(tokens[2]),
                                   "y": float(tokens[3]),
                                   "heading": float(tokens[4]),
                                   "x_speed": float(tokens[5]),
                                   "y_speed": float(tokens[6]),
                                   "owner": int(tokens[7])}
                        if ship["ship_id"] == self.player_id:
                            self.my_ships.append(ship)
                        self.ships.append(ship)

    def issue_order(self, order):
        sys.stdout.write('o %s %s %s %s\n' % (order[0], order[1], order[2], order[3]))
        sys.stdout.flush()

    def finish_turn(self):
        sys.stdout.write('go\n')
        sys.stdout.flush()

    @staticmethod
    def run(bot):
        asteroids = Asteroids()
        map_data = ''
        while(True):
            try:
                # strip new line char
                current_line = sys.stdin.readline().rstrip('\r\n')
                if current_line.lower() == 'ready':
                    asteroids.setup(map_data)
                    asteroids.finish_turn()
                    map_data = ''
                elif current_line.lower() == 'go':
                    asteroids.update(map_data)
                    bot.do_turn(asteroids)
                    asteroids.finish_turn()
                    map_data = ''
                else:
                    map_data += current_line + '\n'
            except EOFError:
                break
            except Exception as e:
                traceback.print_exc(file=sys.stderr)
                break