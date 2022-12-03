library(readr)
data = read_lines("input.txt")
sum = 0
for (d in data) {
	len = nchar(d)
	s1 = substr(d, 0, d/2)
	s2 = substr(d, d/2+1, d)
	# create matrix with priorities
	# name cols
	found = FALSE
	for (i1 in strsplit(s1, "")) {
		for (i2 in strsplit(s2, "")) {
			if (s1 == s2) {
				if (found) {
					print("error")
					return
				} else {
					found = TRUE
					sum = sum + # access col of matrix named like s1 or s2
				}
			}
		}
	}
}
print(sum)
