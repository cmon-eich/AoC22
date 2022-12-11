#!/usr/bin/env Rscript
library(readr)
library(gmp)
options(warn=1)
data = read_lines("input.txt")
monkey <- setClass("monkey", slots=c(
									 num="character",
									 items="list", 
									 op="character", 
									 opval="numeric", 
									 opold="logical",
									 test="numeric",
									 tcond="numeric",
									 fcond="numeric",
									 inspected="numeric"
									 ),
				   prototype=list(inspected=0, opold=FALSE)
)

inspect <- function(old, op, val, opold) {
	v = 0
	if (opold) {
		v = old
	} else {
		v = val
	}
	if (op == "+") {
		return(old+v)
	} else if (op == "*") {
		return(old*v)
	} else {
		print("Can not determine operation!")
		stop()
	}
}

monkeys <- list()
for (d in data) {
	items <- list()
	strings = strsplit(d, " ")[[1]]
	if (length(strings) >= 2 && substring(d, 1, 6) == "Monkey") {
		num = substring(d, 8,8)
		monkeys[[length(monkeys)+1]] <- monkey(num=num, items=list())
	} else if (length(strings) > 2 && strings[3] == "Starting") {
		for (i in strings[5:length(strings)]) {
			# attr(monkeys[[length(monkeys)]], "items")[length(attr(monkeys[[length(monkeys)]], "items"))+1] = strtoi(strsplit(i, ",")[[1]][1])
			items[[length(items)+1]] = strtoi(strsplit(i, ",")[[1]][1])
		}
		attr(monkeys[[length(monkeys)]], "items") <- items
	} else if (length(strings) > 2 && strings[3] == "Operation:") {
		attr(monkeys[[length(monkeys)]], "op") <- strings[(length(strings)-1)]
		if (strings[length(strings)] == "old") {
			attr(monkeys[[length(monkeys)]], "opold") = TRUE
		} else {
			attr(monkeys[[length(monkeys)]], "opval") <- strtoi(strings[length(strings)])
		}
	} else if (length(strings) > 2 && strings[3] == "Test:") {
		attr(monkeys[[length(monkeys)]], "test") <- strtoi(strings[length(strings)])
	} else if (length(strings) > 2 && strings[6] == "true:") {
		attr(monkeys[[length(monkeys)]], "tcond") <- strtoi(strings[length(strings)])
	} else if (length(strings) > 2 && strings[6] == "false:") {
		attr(monkeys[[length(monkeys)]], "fcond") <- strtoi(strings[length(strings)])
	}
}
# print(monkeys)
for (i in 1:20) {
	print(paste("Round: ", as.character(i)))
	for (j in 1:length(monkeys)) {
		print(paste("monkey num: ", attr(monkeys[[j]], "num")))
		print(paste("monkey item count: ", as.character(length(attr(monkeys[[j]], "items")))))
		for (item in attr(monkeys[[j]], "items")) {
			# print(paste("Processing item: ", as.character(item)))
			# monkey inspects item
			worry_level = inspect(item, attr(monkeys[[j]], "op"), attr(monkeys[[j]], "opval"), attr(monkeys[[j]], "opold"))
			worry_level = worry_level %/% 3
			# monkey tests worry level and throws item
			attr(monkeys[[j]], "inspected") = attr(monkeys[[j]], "inspected") +1
			if ((as.bigz(worry_level) %% attr(monkeys[[j]], "test")) == 0) {
				new_m = monkeys[[(attr(monkeys[[j]], "tcond")+1)]]
				attr(new_m, "items")[[length(attr(new_m, "items"))+1]] <- worry_level
				monkeys[[(attr(monkeys[[j]], "tcond")+1)]] = new_m
			} else {
				new_m = monkeys[[(attr(monkeys[[j]], "fcond")+1)]]
				attr(new_m, "items")[[length(attr(new_m, "items"))+1]] <- worry_level
				monkeys[[(attr(monkeys[[j]], "fcond")+1)]] = new_m
			}

		}
		attr(monkeys[[j]], "items") <- list()
	}
}

inspections <- list()
for (m in monkeys) {
	inspections[[length(inspections)+1]] = attr(m, "inspected")
}
inspections <- sort(unlist(inspections), decreasing=TRUE)
print(inspections[1]*inspections[2])


