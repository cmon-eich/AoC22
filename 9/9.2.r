#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
#data = read_lines("test2.txt")
get_tails <- function(heads) {
	tail <- c(0,0)
	tail_pos <- list()
	for (h in heads) {
		if ((abs(h[1]-tail[1]) > 1) && (abs(h[2]-tail[2]) > 1)) {
			d1 = 0
			d2 = 0
			if (h[1]>tail[1]) {
				d1 = -1
			} else if (h[1]<tail[1]) {
				d1 = 1
			}
			if (h[2]>tail[2]) {
				d2 = -1
			} else if (h[2]<tail[2]) {
				d2 = 1
			}
			tail <- c(h[1]+d1, h[2]+d2)
		} else if (abs(h[1]-tail[1]) > 1) {
			if (h[1]>tail[1]) {
				tail <- c(h[1]-1, h[2])
			} else if (h[1]<tail[1]) {
				tail <- c(h[1]+1, h[2])
			}
		} else if (abs(h[2]-tail[2]) > 1) {
			if (h[2]>tail[2]) {
				tail <- c(h[1], h[2]-1)
			} else if (h[2]<tail[2]) {
				tail <- c(h[1], h[2]+1)
			}
		}
		tail_pos[[length(tail_pos)+1]] = tail
	}
	return(tail_pos)
}

head <- c(0,0)
tail <- c(0,0)
head_list <- list()
tail_pos <- list()
x_max = 0
x_min = 0
y_max = 0
y_min = 0
for (d in data) {
	x = strsplit(d, " ")[[1]]
	direction = x[1]
	times = x[2]
	for (i in 1:strtoi(times)) {
		if (direction == "R") {
			head[1] = head[1]+1
			if (abs(head[1]-tail[1])>1) {
				tail <- c(head[1]-1, head[2])
			}
			if (head[1] > x_max) {
				x_max = head[1]
			}
		} else if (direction == "L") {
			head[1] = head[1]-1
			if (abs(head[1]-tail[1])>1) {
				tail <- c(head[1]+1, head[2])
			}
			if (head[1] < x_min) {
				x_min = head[1]
# print(tail_pos)
			}
		} else if (direction == "U") {
			head[2] = head[2]+1
			if (abs(head[2]-tail[2])>1) {
				tail <- c(head[1], head[2]-1)
			}
			if (head[2] > y_max) {
				y_max = head[2]
			}
		} else if (direction == "D") {
			head[2] = head[2]-1
			if (abs(head[2]-tail[2])>1) {
				tail <- c(head[1], head[2]+1)
			}
			if (head[2] < y_min) {
				y_min = head[2]
			}
		}
		head_list[[length(head_list)+1]] = head
		tail_pos[[length(tail_pos)+1]] = tail
	}
}
tail_lists <- list()
tail_lists[[1]] = tail_pos
tail_pos_unique <- list()
for (i in 1:8) {
	tail_lists[[length(tail_lists)+1]] = get_tails(tail_lists[[length(tail_lists)]])
}
for (t in tail_lists[[length(tail_lists)]]) {
	unique = TRUE
	for (tu in tail_pos_unique) {
		if (identical(t, tu)) {
			unique = FALSE
		}
	}
	if (unique) {
		tail_pos_unique[[length(tail_pos_unique)+1]] = t
	}
}
tmp <- list()
# for (k in 1:length(head_list)) {
for (k in 1:length(head_list)) {
	min1 = head_list[[k]][2]
	max1 = head_list[[k]][2]
	min2 = head_list[[k]][1]
	max2 = head_list[[k]][1]

	tmp[[k]] <- list()
	for (tl in tail_lists) {
		tmp[[k]][[length(tmp[[k]])+1]] = tl[[k]]
	}
	m <-matrix(c("."), ncol=abs(x_min-x_max)+1, nrow=abs(y_min-y_max)+1)
	colnames(m) <- c(x_min:x_max)
	rownames(m) <- c(y_max:y_min)
	for (i in 1:length(tmp[[k]])) {
		if (min1 > tmp[[k]][[i]][2]) {
			min1 = tmp[[k]][[i]][2]
		}
		if (max1 < tmp[[k]][[i]][2]) {
			max1 = tmp[[k]][[i]][2]
		}
		if (min2 > tmp[[k]][[i]][1]) {
			min2 = tmp[[k]][[i]][1]
		}
		if (max2 < tmp[[k]][[i]][1]) {
			max2 = tmp[[k]][[i]][1]
		}
		m[
		  as.character(tmp[[k]][[i]][2]),
		  as.character(tmp[[k]][[i]][1]) 
		] = as.character(i)
	}
	m[
	  as.character(head_list[[k]][2]),
	  as.character(head_list[[k]][1]) 
	] = "H"
	m["0","0"] = "S"
	# print(paste(as.character(min1),as.character(max1), as.character(min2),as.character(max2)))
	# print(paste(as.character(min1+abs(y_min)), as.character(max1+abs(y_min)), as.character(min2+abs(x_min)),as.character(max2+abs(x_min))))
	# print((min1+abs(y_min)):(max1+abs(y_min)))
	# print(y_min)
	# print(y_max)
	print(m[(abs(max1-y_max)+1):(abs(min1-y_max)+1), (min2+abs(x_min)+1):(max2+abs(x_min)+1)])
}
# print(tail_pos_unique[[1]])
# print(tail_pos_unique[[length(tail_pos_unique)]])
print(length(tail_pos_unique))
