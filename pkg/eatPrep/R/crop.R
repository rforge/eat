# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# crop
# 2011-12-07 SW/MH
# CHANGED: built ultra fast version of crop
# 0000-00-00 AA#
# 03.11.2011 (MH): NA handling geaddet
# 14.10.2011 (MH): gestabled
# 14.10.2011 (SW): Funktion umbenannt in "crop", "trim" gibt es schon
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

crop <- function ( x , char = " " ) {
	if ( char %in% c ( "\\" , "+" , "*" , "." , "(" , ")" , "[" , "]" , "{" , "}" , "|" , "^" , "$" ) ) 
    char <- paste ( "\\" , char , sep = "" )
	gsub ( paste ( "^" , char , "+|" , char , "+$" , sep = "" ) , "" , x )
}

### Beispiele
# ( crop ( "/bla" , "/" ) )
# ( crop ( "bla/" , "/" ) )
# ( crop ( "//bla" , "/" ) )
# ( crop ( "//bla//" , "/" ) )
# ( crop ( "bla//" , "/" ) )
# ( crop ( "//bla/blubb//" , "/" ) )
# ( crop ( "\\bla" , "\\" ) )
# ( crop ( "bla\\" , "\\" ) )
# ( crop ( "\\bla" , "\\" ) )
# ( crop ( "bla\\" , "\\" ) )
# ( crop ( "\\bla/blubb\\" , "\\" ) )
# ( crop ( c ( "\\bla\\" , "\\blubb\\\\" ) , "\\" ) )
# ( crop(c("1 ","5","  12"," 3 ",NA,"12")) )
# ( crop ( "aaaKFLSJFKJaa" , "a" ) )
# ( crop ( "+++sdfa+sdafsdfa++" , "+" ) )
# ( crop ( "^^sdfa+sdafsdfa^^" , "^" ) )


