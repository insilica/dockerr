#' transform terminal output to a tibble
#' @param stdout output from terminal
#' @importFrom purrr imap set_names map_chr
#' @importFrom tidyr as_tibble
#' @importFrom glue glue
terminal2tbl = function(stdout){
  cols   <- strsplit(stdout[1],"  +")[[1]]
  mapcol <- \(col,last=F){
    idx <- gregexpr(pattern = glue::glue("{col}( |$)+"),text = stdout[1])[[1]]
    end <- \(line){ if(last){ nchar(line) }else{ idx[1]+attr(idx,"match.length")-1 } }
    map_chr(stdout[-1],~ substr(.,idx[1],end(.))) |> trimws()
  }
  imap(cols,\(x,i){ mapcol(x,i==length(cols)) }) |> set_names(cols) |> as_tibble()
}

#' list active docker containers
#' @return a tibble output of the docker container ls command
#' @export
containers = function(){
  system("docker container ls",intern = T) |> terminal2tbl()
}

#' list active docker containers
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @param container the container you want to inspect
#' @return a tibble output of the docker container ls command
#' @export
inspect = function(container){
  res <- system(glue("docker inspect {container}"),intern=T,ignore.stderr=T)
  res |> paste(collapse="\n") |> fromJSON()
}