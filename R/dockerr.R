#' transform terminal output to a tibble
#' @param stdout output from terminal
#' @importFrom purrr imap set_names 
#' @importFrom tidyr as_tibble
#' @importFrom glue glue
terminal2tbl = function(stdout){
  cols   <- strsplit(stdout[1],"  +")[[1]]
  mapcol <- \(col,last=F){
    idx <- gregexpr(pattern = glue::glue("{col}( |$)+"),text = stdout[1])[[1]]
    end <- \(line){ if(last){ nchar(line) }else{ idx[1]+attr(idx,"match.length")-1 } }
    purrr::map_chr(stdout[-1],~ substr(.,idx[1],end(.))) |> trimws()
  }
  purrr::imap(cols,\(x,i){ mapcol(x,i==length(cols)) }) |> purrr::set_names(cols) |> tidyr::as_tibble()
}

#' list active docker containers
#' @return a tibble output of the docker container ls command
#' @export
containers = function(){
  system("docker container ls",intern = T) |> terminal2tbl()
}
