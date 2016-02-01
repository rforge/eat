\name{bias.rmse}
\alias{bias.rmse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute bias and RMSE
}
\description{
compute bias and RMSE
}
\usage{
bias.rmse ( true , est , id.col , val.col , repl.col = NULL , group.col = NULL , method = c ( "group" , "repl" ) , verbose = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true}{data frame with true parameters}
  \item{est}{data frame with estimated parameters}
  \item{id.col}{name of id column (must be equal in both data frames)}
  \item{val.col}{name of value column (must be equal in both data frames)}
  \item{repl.col}{name of replication column (in \code{est})}
  \item{group.col}{name of group columns (must be equal in both data frames)}
  \item{method}{either "group" (aggregation over groups) or "repl" (aggregation over replications)}
  \item{verbose}{logical, print information while processing}
}
%% \details{}
\value{
returns a data frame
}
%% \references{}
\author{
Martin Hecht
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning}{
This version is beta. Use with care.
} 

\seealso{
}

\examples{
### Example 1, no replicates
true <- data.frame ( "variable" = c ("item1","item2") ,
					 "value" = c ( 1 , 2 ) ,
					 "group" = rep ( "items" , 2 ) , 
					 stringsAsFactors = FALSE )
est <-  data.frame ( "variable" = c ("item1","item2") ,
					 "value" = rnorm ( 2 , 1 , 1 ) ,
					 stringsAsFactors = FALSE )
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , verbose = TRUE )	


### Example 2, additionally 2 replicates
true <- data.frame ( "variable" = c ("item1","item2") ,
					 "value" = c ( 1 , 2 ) ,
					 "group" = rep ( "items" , 2 ) , 
					 stringsAsFactors = FALSE )
est <-  data.frame ( "variable" = c ("item1","item1","item2","item2") ,
					 "value" = rnorm ( 4 , 1 , 1 ) ,
					 "replicates" = c ( 1 , 2 , 1 , 2 ) , 					 
					 stringsAsFactors = FALSE )

bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , method = "group" , verbose = TRUE)		 
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , method = "repl" , verbose = TRUE)		 
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE)		 

					 
### Example 3, additionally 2 data conditions
true <- data.frame ( "variable" = rep ( c ("item1","item2") , 2 ) ,
					 "value" = 1:4 ,
					 "group" = rep ( rep ( "items" , 2 ) , 2 ) , 
					 "data.cond" = c ( rep ( 1 , 2 ) , rep ( 2 , 2 ) ) ,
					 stringsAsFactors = FALSE )
est <-  data.frame ( "variable" = rep ( c ("item1","item1","item2","item2") , 2 ) ,
					 "value" = rnorm ( 8 , 1 , 1 ) ,
					 "replicates" = rep ( c ( 1 , 2 , 1 , 2 ) ) , 
					 "data.cond" = c ( rep ( 1 , 4 ) , rep ( 2 , 4 ) ) ,
					 stringsAsFactors = FALSE )
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE )		 
	
	
### Example 4, 2 models
true <- data.frame ( "variable" = c ("item1","item2") ,
					 "value" = c ( 1 , 2 ) ,
					 "group" = rep ( "items" , 2 ) , 
					 stringsAsFactors = FALSE )
est <-  data.frame ( "variable" = rep ( c ("item1","item1","item2","item2") , 2 ),
					 "value" = rnorm ( 8 , 1 , 1 ) ,
					 "replicates" = rep ( c ( 1 , 2 , 1 , 2 ) ) , 
					 "model" = c ( rep ( "m1" , 4 ) , rep ( "m2" , 4 ) ) ,
					 stringsAsFactors = FALSE )
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE )		 


### Example 5, 2 data conditions and 2 models
true <- data.frame ( "variable" = rep ( c ("item1","item2") , 2 ) ,
					 "value" = 1:4 ,
					 "group" = rep ( rep ( "items" , 2 ) , 2 ) , 
					 "data.cond" = c ( rep ( 1 , 2 ) , rep ( 2 , 2 ) ) ,
					 stringsAsFactors = FALSE )
est <-  data.frame ( "variable" = rep ( rep ( c ("item1","item1","item2","item2") , 2 ) , 2 ),
					 "data.cond" = rep ( c ( rep ( 1 , 4 ) , rep ( 2 , 4 ) ) , 2 ) ,
					 "value" = rnorm ( 16 , 1 , 1 ) ,
					 "replicates" = rep ( rep ( c ( 1 , 2 , 1 , 2 ) ) , 2 ) , 
					 "model" = c ( rep ( "m1" , 8 ) , rep ( "m2" , 8 ) ) ,
					 stringsAsFactors = FALSE )
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE )


### Example 6, multiple groups
true <- data.frame ( "variable" = c ("item1","item2","person1","person2","randeff1") ,
					 "value" = 1:5 ,
					 "group" = c ( rep ( "items" , 2 ) , rep ( "persons" , 2 ) , NA ) , 
					 stringsAsFactors = FALSE )
est <-  data.frame ( "variable" = c ("item1","item2","person1","person2","randeff1") ,
					 "value" = rnorm ( 5 , 1 , 1 ) ,
					 stringsAsFactors = FALSE )
bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , verbose = TRUE )	

}
