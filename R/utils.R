## taken from "pcts"
.reportClassName <- function(object, class, trailer = "\n"){
    if(class(object) == class)
        cat('An object of class "', class, '"', trailer, sep = "")
    NULL
}
