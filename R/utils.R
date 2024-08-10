#' 
#' Collect all targets and lists of targets in the environment
#' 
#' 
all_targets <- function(env = parent.env(environment()), 
                        type = "tar_target", 
                        add_list_names = TRUE) {
  
  ## Function to determine if an object is a type (a target), 
  ## or a list on only that type
  rfn <- function(obj) 
    inherits(obj, type) || (is.list(obj) && all(vapply(obj, rfn, logical(1))))
  
  ## Get the names of everything in the environment 
  ## (e.g. sourced in the _targets.R file)
  objs <- ls(env)
  
  out <- list()
  for (o in objs) {
    obj <- get(o, envir = env)      ## Get each top-level object in turn
    if (rfn(obj)) {                 ## For targets and lists of targets
      out[[length(out) + 1]] <- obj ## Add them to the output
      
      ## If the object is a list of targets, add a vector of the target names 
      ## to the environment So that one can call `tar_make(list_name)` to make 
      ## all the targets in that list
      if (add_list_names && is.list(obj)) {
        target_names <- vapply(obj, \(x) x$settings$name, character(1))
        assign(o, target_names, envir = env)
      }
    }
  }
  return(out)
}



#'
#' Helper function/s to process climate tables
#'
#'

structure_table_single <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
     {
       x[row_index[2], col_index]             <- x[row_index[2], ][x[row_index[2], ] != ""]
       x[row_index[2], 1:(col_index[1] - 1)]  <- x[row_index[1], 1:(col_index[1] - 1)]
       x[row_index[2], (col_index[2] + 1):14] <- x[row_index[1], (col_index[2] + 1):14]
       x
    }
    )()
}


structure_table_double <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
      {
        x[row_index[1], 2:14]      <- x[row_index[1], 1:13]
        x[row_index[1], 1]         <- x[row_index[2], 1]
        x[row_index[2], 2:14]      <- x[row_index[1], 2:14]
        x[row_index[2], col_index] <- x[row_index[3], ][x[row_index[3], ] != ""]
        
        x
      }
    )()
}


structure_table_triple <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
      {
        x[row_index[1], 2:14]      <- x[row_index[1], 1:13]
        x[row_index[1], 1]         <- x[row_index[2], 1]
        
        x[row_index[2], col_index] <- x[row_index[2], ][stringr::str_detect(x[row_index[2], ], paste(toupper(month.abb), collapse = "|"), negate = TRUE)] |> (\(x) x[x != ""])()
        x[row_index[2], which(!1:14 %in% col_index)]  <- x[row_index[1], which(!1:14 %in% col_index)]
        
        x[row_index[3], col_index]             <- x[row_index[3], 1:length(col_index)]
        x[row_index[3], which(!1:14 %in% col_index)]  <- x[row_index[1], which(!1:14 %in% col_index)]
        
        x
      }
    )()
}


structure_table_quadruple <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
      {
        x[row_index[1], 2:14]      <- x[row_index[1], 1:13]
        x[row_index[1], 1]         <- x[row_index[3], 1]
       
        x[row_index[2], col_index]             <- x[row_index[2], ][x[row_index[2], ] != ""]
        x[row_index[2], 1:(col_index[1] - 1)]  <- x[row_index[1], 1:(col_index[1] - 1)]
        x[row_index[2], (col_index[2] + 1):14] <- x[row_index[1], (col_index[2] + 1):14]
       
        x[row_index[3], 2:14]                  <- x[row_index[1], 2:14]
        x[row_index[3], col_index]             <- x[row_index[4], ][x[row_index[4], ] != ""]

        x[row_index[4], ]                      <- x[row_index[1], ]
        x[row_index[4], col_index]             <- x[row_index[5], ][x[row_index[5], ] != ""]
       
        x
      }
    )()
}

remove_table_rows <- function(pdf_tab) {

  stringr::str_detect(pdf_tab[ , 1], pattern = "[A-Z]") |>
    (\(x) pdf_tab[x, ])()
  
}