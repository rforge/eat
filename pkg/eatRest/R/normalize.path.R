####################################################################################################################
#
# normalize.path      
# macht dasselbe wie "normalizePath"
# nur daﬂ die Funktion sich versionsinvariant verh‰lt 
#
# Version: 	0.1.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change-Log
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#
####################################################################################################################

### string     ... Pfadname als "R"-string

normalize.path <- function(string)
                  {string <- gsub("//","/",string)
                   string <- gsub("/","//",string)
                   string <- gsub("//","\\\\",string)
                   return(string)}