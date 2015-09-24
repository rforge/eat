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


