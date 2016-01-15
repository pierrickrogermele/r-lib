if ( ! exists('load.sdf')) { # Do not load again if already loaded

	#############
	# CONSTANTS #
	#############

	R.LIB.CHEM.FILE.PATH <- parent.frame(2)$ofile
	
	##################
	# LOAD JAVA CHEM #
	##################

	load.java.chem <- function() {
		library(rJava)
		.jinit()
		cmd <- c("mvn", "-f", file.path(dirname(R.LIB.CHEM.FILE.PATH), '..', 'java-chem'), "org.apache.maven.plugins:maven-dependency-plugin:2.10:build-classpath")
		classpath <- system(paste(cmd, collapse = " "), intern = TRUE)
		classpath <- grep("^\\[INFO]", classpath, invert = TRUE, value = TRUE)
		classpath <- strsplit(classpath, split = ':')[[1]] # TODO make it portable (classpath under Windows use ';' instead of ':')
		.jaddClassPath(classpath)
		.jaddClassPath(file.path(dirname(R.LIB.CHEM.FILE.PATH), '..', 'java-chem', 'target', 'java-chem-1.0.jar'))
	}

	#############
	# GET INCHI #
	#############

	get.inchi <- function(mol) {
		load.java.chem()
		cdkhlp <- .jnew('org/openscience/chem/CdkHelper')
		inchi <- .jcall(cdkhlp, 'S', 'getInchi', mol)
		return(inchi)
	}

	# 
	############
	# LOAD SDF #
	############

	load.sdf <- function(file, silent = FALSE) {

		library(stringr)

		# Valid file ?
		if ( ! file.exists(file)) {
			if ( ! silent)
				warning(paste0("SDF File \"", file, "\" does not exist."))
			return(NULL)
		}

		info <- data.frame()

		# Read file line by line
		con <- file(file)
		open(con)
		imol <- 1 # Index of molecule inside the file
		field.name <- NA_character_
		while (TRUE) {

			# Read one line
			line <- readLines(con, n = 1)
			if (length(line) == 0)
				break

			# Field value
			if ( ! is.na(field.name)) {
				info[imol, field.name] <- line
				field.name <- NA_character_
				next
			}

			# Empty line
			if (line == "") {
				field.name <- NA_character_
				next
			}

			# End of molecule
			if (substring(line, 1, 4) == "$$$$") {
				field.name <- NA_character_
				imol <- imol + 1
				next
			}

			# Metadata field
			g <- str_match(line, "^> <(.*)>$")
			if ( ! is.na(g[1,2])) {
				field.name <- g[1,2]
				next
			}
		}
		close(con)

		# Load molecule structures
		load.java.chem()
		cdkhlp <- .jnew('org/openscience/chem/CdkHelper')
		struct <- .jcall(cdkhlp, '[Lorg/openscience/cdk/interfaces/IAtomContainer;', 'loadSdf', file)

		return(list(struct = struct, info = info))
	}

} # end of load safe guard
