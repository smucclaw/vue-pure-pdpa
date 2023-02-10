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
read_file = path + "/paintQ.txt"
write_file = path + "/split_paintQ.txt"
all_lines = ""

def printString(line, index, count, symbol):
  global all_lines
  front_string = line[:index]
  back_string = line[index:]
  for x in range(count):
    front_string = "    " + front_string
  print(count)
  print(front_string)
  print(back_string)
  all_lines = all_lines + front_string + "\n"
  return back_string

def parseCurlyBraces(line, count, previous_close):
  global all_lines
  index = line.find('{', 1)
  index_close = line.find('}', 1)
  print("index: " + str(index))
  print("index_close: " + str(index_close))
  print("previous_close: " + str(previous_close))
  if index > 0 and index < index_close:
    line = printString(line, index, count, '{')
    if previous_close == False:
      count += 1
    previous_close = False
    print(count)
  elif index_close > 0 and index_close < index:
    if previous_close == True:
      count -= 1
    line = printString(line, index_close, count, '}')
    previous_close = True
    print(count)
  elif index_close > 0 and index < 0:
    line = printString(line, index_close, count, '}')
    if previous_close == True:
      count -= 1
    previous_close = True
    print(count)
  else:
    # count -= 1
    print(count)
    for x in range(count):
      end_string = "    " + line
    all_lines = all_lines + end_string
    return True, line, count, previous_close
  print()
  return False, line, count, previous_close

with open(read_file, 'r') as f:
  for line in f:
    count = 0
    previous_close = False
    while True:
      stop_curly_braces, line, count, previous_close = parseCurlyBraces(line, count, previous_close)
      if stop_curly_braces:
        break
  f.close()

with open(write_file, 'w') as f:
  f.write(all_lines)
  f.close()