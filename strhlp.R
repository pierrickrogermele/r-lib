#######################
# WHITESPACE TRIMMING #
#######################

# Trim leading whitespaces
trim.leading <- function (x)  sub("^\\s+", "", x)

# Trim trailing whitespaces
trim.trailing <- function (x) sub("\\s+$", "", x)

# Trim leading and trailing whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
