####################################################################################################################
#
# genConquestBatch
# erzeugt Batch String für Stapelverarbeitungsdatei
#
# Version: 	0.2.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
#
# Change Log:
# * zu 0.2.0 (2011-10-04, NH): kosmetische Korrekturen
#
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 30.05.2011: uses "normalize.path" instead of "normalizePath"
# 17.06.2011: uses relative paths
# 
####################################################################################################################

genConquestBatch <- function (pathConquest, jobName){
    command <- paste('"', normalize.path(pathConquest), '" ', '"' , paste(jobName, ".cqc", sep = ""), '"' , sep = "")
    return(command)
}

