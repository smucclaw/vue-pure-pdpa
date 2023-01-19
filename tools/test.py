#!/usr/bin/python3
# chmod +x test.py
# ./test.py
import os

try:
  path = "/root/src/vue-pure-pdpa/tools"
  os.chdir(path)
except:
  path = "/Users/maxloo/vue-pure-pdpa/tools"
  os.chdir(path)
read_file = path + "/testQ.txt"
write_file = path + "/split_paintQ.txt"
all_lines = ""

def printString(line, index, count):
  global all_lines
  front_string = line[:index]
  back_string = line[index:]
  for x in range(count-1):
    front_string = "    " + front_string
  # print(index)
  print(count)
  print(front_string)
  all_lines = all_lines + front_string + "\n"
  return back_string

with open(read_file, 'r') as f:
  for line in f:
    count = 0
    while True:
      index = line.find('{', 1)
      index_close = line.find('}', 1)
      print("index: " + str(index))
      print("index_close: " + str(index_close))
      if index > 0 and index < index_close:
        line = printString(line, index, count)
        if count == 5:
          break
        count += 1
      elif index_close > 0 and index_close < index:
        line = printString(line, index_close, count)
        count -= 1
      else:
        line = ""
      if line == "":
        break
  f.close()

with open(write_file, 'w') as f:
  f.write(all_lines)
  f.close()