

#' Flatten Submissions
#'
#' This function will flatten submissions coming from a FormIO list that has nested elements
#' @param x list with responses, typically obtained through the GetResponses() function.
#'
#' @returns list with
#'	* `FlatResponses` which is the entire dataset obtained from GetResponses() but, where all the nested lists are flattened into a bidimensional data frame. nested lists will take their name by using their parent name as a prefix, then a "-" separator, and then their own name.
#'	* `ColumnNames` which is a data frame with column `Number` and column `Names`, listing all the newly created column names in `FlatResponses` and their numerical order. This output can then be passed to other FormIO functions for making easier to subset the dataset
#' @export

#' @examples
#'
#' x<-FoodTypes
#'
#' # Nested Structure
#' head(x)
#'
#' #Flattened Structure
#' xFlat<-FlattenSubmissions(x)
#'
#' xFlat$FlatResponses ## Survey Output
#' xFlat$ColumnNames   ## New Columns Formed



FlattenSubmissions <- function(x) {
	LineData<-list()
	for(j in 1:nrow(x)){
		RespondentLine <- x[j, ]
		LineData[[j]]<-list( tidyr::unnest(RespondentLine, cols = everything(), names_sep="-", keep_empty = T))
	}

	Output<-dplyr::bind_rows(LineData)
	ColumnNames<-data.frame(Number= 1:length(colnames(Output)), Name=  colnames(Output))
	return(list(FlatResponses=Output, ColumnNames=ColumnNames))
}
