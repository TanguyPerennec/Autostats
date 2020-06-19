#' xlsx
#'
#' @param table matrix or dataframe to write in a excel file
#' @param title_sheet character : title of the sheet in the excel file
#'
#' @return
#' @export
#' @import xlsx
#'
#' @examples
xlsx <- function(table,title_sheet=NULL,file_name){
   write.xlsx(table,"Results.xlsx",sheetName = title_sheet,append = FALSE,row.names = FALSE)
}
