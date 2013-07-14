

# Mergen ohne Sortierung
# d.h. Sortierung (sowohl Spalten wie auch Zeilen) bleibt so wie in x Datensatz
# y-Spalten werden hinten dran gehangen, oder durch optionalen Parameter reinsortiert


# Code Snippet
# merge.vars <- c ( "th" )
# cn <- colnames ( x )
# x <- merge ( x , y , by = merge.vars )
# new.vars <- colnames(y)[!colnames(y) %in% c ( merge.vars )]
# x <- x [ , c ( cn[!cn %in% new.vars] , new.vars ) ]


