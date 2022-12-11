#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
# data = read_lines("test.txt")
c = 0
val = 1
signal_str = 0
imp_cycs <- c(20, 60, 100,140,180,220)
for (d in data) {
	command = substring(d, 1, 4)
	if (command == "noop") {
		c = c+1
		if (c %in% imp_cycs) {
			signal_str = signal_str + (c*val)
			# print("------------------")
			# print(paste("Hit important cycle: ", as.character(c)))
			# print(paste("Val added: ", as.character((c*val))))
			# print("------------------")
		}
	} else if (command == "addx") {
		if ((c+1) %in% imp_cycs) {
			signal_str = signal_str + ((c+1)*val)
			# print("------------------")
			# print("Pre-add")
			# print(paste("Hit important cycle: ", as.character(c+1)))
			# print(paste("Val added: ", as.character(((c+1)*val))))
			# print("------------------")
		}
		c = c+2
		if (c %in% imp_cycs) {
			signal_str = signal_str + (c*val)
			# print("------------------")
			# print("post-add")
			# print(paste("Hit important cycle: ", as.character(c)))
			# print(paste("Val added: ", as.character((c*val))))
			# print("------------------")
		}
		val_c = strsplit(d, " ")[[1]][2]
		# print(paste("Value: ", val_c))
		val = val + strtoi(val_c)
		# print(paste("New Sum: ", as.character(val)))
	} else {
		print("fuck you!")
		stop()
	}
}
print(signal_str)
