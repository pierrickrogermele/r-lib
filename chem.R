if ( ! exists('load.sdf')) { # Do not load again if already loaded

	r.lib.chem.file.path <- parent.frame(2)$ofile
	
	#############
	# GET INCHI #
	#############

	get.inchi <- function(mol) {
		library(rJava)
		.jinit()
		.jaddClassPath(file.path(dirname(r.lib.chem.file.path), 'rlib.jar'))
		.jaddClassPath(file.path(dirname(r.lib.chem.file.path), '..', 'java-cdk', 'cdk-1.4.19.jar'))
		print(.jclassPath())
		inchi <- .jcall('fr/cea/r/lib/Chem', 'S', 'getInchi', mol)
# FIXME error inside .jcall:
#			  java.lang.NoSuchMethodError: org.openscience.cdk.config.IsotopeFactory.getInstance(Lorg/openscience/cdk/interfaces/IChemObjectBuilder;)Lorg/openscience/cdk/config/IsotopeFactory;
# This is because the IChemObjectBuilder found is the one inside rcdklibs loaded by rcdk. cdk-1.4.19.jar is at the end of the classpath. The only solution to put it at the beginning of the class path, is to reinitialize rJava with .jinit(force.init = TRUE), but this reinitialize the JVM and thus erase all current objects.
		print(inchi)
		return(inchi)
	}

	############
	# LOAD SDF #
	############

	load.sdf <- function(file) {

		library(stringr)
		library(rcdk)

		# Valid file ?
		if ( ! file.exists(file)) {
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
		struct <- load.molecules(file)

		return(list(struct = struct, info = info))
	}

} # end of load safe guard
