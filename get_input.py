#! /usr/bin/env python
import sys
import requests

def get_session():
    with open(".session") as f:
        session = f.read()
    return session

def get_input(day):
    session = get_session()
    resp = requests.get(f"https://adventofcode.com/2022/day/{day}/input", cookies={"session": session})
    res = resp.content
    if b"Puzzle inputs differ by user.  Please log in to get your puzzle input." in res:
        raise Exception("Need to update session")
    return resp.content

def main(day):
    with open(f"inputs/day{day}", "wb") as f:
        f.write(get_input(day=day))

day = sys.argv[1]
main(day)

# for i in range(25):
#     main(i)
