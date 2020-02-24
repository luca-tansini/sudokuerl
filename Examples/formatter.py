# http://www.menneske.no/sudoku/3/eng/random.html
# http://www.menneske.no/sudoku/4/eng/random.html
# http://www.menneske.no/sudoku/5/eng/random.html

import re, sys

file = sys.argv[1]
out = ""
for line in open(file):
    out += " ".join(re.sub(" ", "0", line).split("\t"))

f = open(file, "w")
f.write(out)
f.close
