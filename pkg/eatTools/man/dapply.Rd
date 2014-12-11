\name{dapply}
\alias{dapply}

\title{
apply a function on data.frame
}

\description{
split a data.frame, apply a function, and get data.frame with results
}

\usage{
dapply ( data , split.vars = NULL , fun = mean , wide = TRUE , drop = TRUE , all.level = FALSE , verbose = FALSE , ... )
}

\arguments{
  \item{data}{a data frame}
  \item{split.vars}{colnames of variables to split data.frame}
  \item{fun}{a function}
  \item{wide}{logical, if \code{TRUE} (default) a data.frame in wide format is returned, if \code{FALSE} a data.frame in long format is returned}
  \item{drop}{logical, if \code{TRUE} (default) empty combinations will be dropped, if \code{FALSE} all combinations are kept and NAs are generated}
  \item{all.level}{logical, if \code{TRUE} all levels of factors are incorporated, if \code{FALSE} (default) only factor levels existing in the data are used}
  \item{verbose}{logical, if \code{TRUE} progress is printed, if \code{FALSE} (default) no console output is generated}
  \item{...}{arguments to be passed to \code{fun}}
}
\details{
}
\value{
A data frame with results. Depending on \code{wide} a data.frame in either wide or long format.
}

\author{
Martin Hecht
}

\examples{

### example data

x1 <- data.frame ( "sex" = c ( "m" , "f" ) , stringsAsFactors = FALSE ) 
x2 <- data.frame ( "age" = c ( 20 , 30 , 40 ) ) 
d <- merge ( x1 , x2 )
set.seed ( 123 )
d$resp1 <- rnorm ( nrow ( d ) , 0 , 1 )
d$resp2 <- rnorm ( nrow ( d ) , 0 , 1 )


### return results in wide format

# 1 variable
dapply ( data = d[,"resp1",drop= FALSE] , fun = mean )

# 2 variables
dapply ( data = d[,c("resp1","resp2"),drop= FALSE] , fun = mean )

# 2 variables, 2 functions
twofun1 <- c ( mean , median )
names ( twofun1 ) <- c ( "mean" , "median" )
dapply ( data = d[,c("resp1","resp2")] , fun = twofun1 )

# 1 variable, 1 split
dapply ( data = d[,c("sex","resp1")] , split.vars = "sex" , fun = mean )

# 1 variable, 2 splits
dapply ( data = d[,c("sex","age","resp1")] , split.vars = c("sex","age") , fun = mean )

# 2 variable, 2 splits
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = mean )

# 2 variable, 2 splits, 2 functions
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = twofun1 )


### return results in long format

# 1 variable
dapply ( data = d[,"resp1",drop= FALSE] , fun = mean , wide = FALSE )

# 2 variables
dapply ( data = d[,c("resp1","resp2")] , fun = mean , wide = FALSE )

# 2 variables, 2 functions
dapply ( data = d[,c("resp1","resp2")] , fun = twofun1 , wide = FALSE )

# 1 variable, 1 split
dapply ( data = d[,c("sex","resp1")] , split.vars = "sex" , fun = mean , wide = FALSE )

# 1 variable, 2 splits
dapply ( data = d[,c("sex","age","resp1")] , split.vars = c("sex","age") , fun = mean , wide = FALSE )

# 2 variable, 2 splits
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = mean , wide = FALSE )

# 2 variable, 2 splits, 2 functions
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = twofun1 , wide = FALSE )


### function has more than one return when applying data.frame()
# e.g., table has names of category and frequency of category
# .1 is category , .2 is frequency

# 1 variable, 
dapply ( data = d[,"resp1",drop= FALSE] , fun = table , wide = TRUE )
dapply ( data = d[,"resp1",drop= FALSE] , fun = table , wide = FALSE )

# 2 variables
dapply ( data = d[,c("resp1","resp2")] , fun = table , wide = TRUE )
dapply ( data = d[,c("resp1","resp2")] , fun = table , wide = FALSE )

# 2 variables, 2 functions
twofun2 <- c ( table , table )
names ( twofun2 ) <- c ( "table" , "table" )
dapply ( data = d[,c("resp1","resp2")] , fun = twofun2 , wide = TRUE )
dapply ( data = d[,c("resp1","resp2")] , fun = twofun2 , wide = FALSE )

# 1 variable, 1 split
dapply ( data = d[,c("sex","resp1")] , split.vars = "sex" , fun = table , wide = TRUE )
dapply ( data = d[,c("sex","resp1")] , split.vars = "sex" , fun = table , wide = FALSE )

# 1 variable, 2 splits
dapply ( data = d[,c("sex","age","resp1")] , split.vars = c("sex","age") , fun = table , wide = TRUE )
dapply ( data = d[,c("sex","age","resp1")] , split.vars = c("sex","age") , fun = table , wide = FALSE )

# 2 variable, 2 splits
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = table , wide = TRUE )
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = table , wide = FALSE )

# 2 variable, 2 splits, 2 functions
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = twofun2 , wide = TRUE )
dapply ( data = d[,c("sex","age","resp1","resp2")] , split.vars = c("sex","age") , fun = twofun2 , wide = FALSE )

}
