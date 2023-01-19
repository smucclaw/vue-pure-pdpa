#!/usr/bin/python3
# ./test.py
import os

try:
  path = "/root/src/vue-pure-pdpa"
  os.chdir(path)
except:
  path = "/Users/maxloo/vue-pure-pdpa/tools"
  os.chdir(path)
read_file = path + "/pdpaQ2.txt"
write_file = path + "/pdpa.txt"

all_lines = ""
with open(read_file, 'r') as f:
  for line in f:
    temp = line.strip()
    temp_split = temp.split()
    if temp_split[0]=='[' and temp_split[1]=='(':
      temp = "".join(temp_split)
    elif temp_split[0]==',' and temp_split[1]=='(':
      temp = "".join(temp_split)
    # print(temp)
    all_lines = all_lines + temp + " "
  f.close()
with open(write_file, 'w') as f:
  f.write(all_lines)
  f.close()
# print(all_lines)
