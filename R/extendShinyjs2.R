.globals <- new.env(parent = emptyenv())
# main powerhorse that takes an R command and translates it to shinyjs JS function
jsFunc <- function(...) {
  params <- eval(substitute(alist(...)))
  
  if (!is.null(names(params)) && any(vapply(names(params), nzchar, 1L) == 0)) {
    errMsg(paste0("you cannot mix named and unnamed arguments in the same function call",
                  " (function: ", as.character(match.call()[1]), ")"))
  }
  
  # evaluate the parameters in the appropriate environment
  parentFrame <- parent.frame(1)
  params <- lapply(params, function(x){ eval(x, envir = parentFrame) })
  
  # figure out what JS function to call, make sure to work with namespacing as well
  pkgName <- "shinyjs"
  extensionName <- "js"
  regex <- sprintf("^(%s:{2,3})?(%s\\$)?((\\w)+)$", pkgName, extensionName)
  fxn <- as.character(as.list(match.call()[1]))
  fxn <- sub(regex, "\\3", fxn)
  
  jsFuncHelper(fxn, params)
}

# similar to jsFunc, but here we already know the function name and parameters
jsFuncHelper <- function(fxn, params) {
  # get the Shiny session
  session <- getSession()
  
  fxn <- paste0("shinyjs-", fxn)
  
  # respect Shiny modules/namespaces
  if (inherits(session , "session_proxy")) {
    if ("id" %in% names(params) && !is.null(params[['id']])) {
      if (!"asis" %in% names(params) || !params[['asis']]) {
        params[['id']] <- session$ns(params[['id']])
      }
    }
  }
  
  # call the javascript function
  session$sendCustomMessage(
    type = fxn,
    message = params)
  
  invisible(NULL)
}
# common way to print error messages
errMsg <- function(x) {
  stop(sprintf("shinyjs: %s", x), call. = FALSE)
}

# get the shiny session object
getSession <- function() {
  session <- shiny::getDefaultReactiveDomain()
  
  if (is.null(session)) {
    errMsg(paste(
      "could not find the Shiny session object. This usually happens when a",
      "shinyjs function is called from a context that wasn't set up by a Shiny session."
    ))
  }
  
  session
}

# set up some javascript functions to work with shinyjs and any other resources
setupJS <- function(jsFuncs, script, text, ...) {
  # add a shiny message handler binding for each supported method
  tpl <- paste0(
    "Shiny.addCustomMessageHandler('shinyjs-%s', function(params) {",
    " shinyjs.debugMessage('shinyjs: calling function \"%s\" with parameters:');",
    " shinyjs.debugMessage(params);",
    " shinyjs.%s(params);",
    "});")
  controllers <-
    lapply(jsFuncs, function(x) {
      sprintf(tpl, x, x, x)})
  controllers <- paste(controllers, collapse = "\n")
  sout(controllers)
  
  # ensure the same scripts don't get added to the HTML twice
  shinyjsContent <-
    shiny::singleton(
      insertHead(
        # add the message handlers
        shiny::tags$script(shiny::HTML(controllers)),
        # add the actual javascript code
        shinyjsInlcudeScript(script),
        shinyjsInlineScript(text),
        # add any extra tags
        ...
      )
    )
  sout(shinyjsContent)
  
  # inject the content via JavaScript if necessary
  if (!is.null(.globals$inject) && .globals$inject) {
    shinyjsContent <- as.character(shinyjsContent)
    session <- getSession()
    session$sendCustomMessage('shinyjs-inject', shinyjsContent)
  } else {
    shinyjsContent
  }
}

# insert content into the <head> tag of the document if this is a proper HTML
# Shiny app, but if it's inside an interactive Rmarkdown document then don't
# use <head> as it won't work
insertHead <- function(...) {
  if (is.null(.globals$astext) || .globals$astext) {
    shiny::tagList(...)
  } else {
    shiny::tags$head(...)
  }
}

# include a JavaScript script
shinyjsInlcudeScript <- function(script) {
  if (missing(script) || is.null(script)) {
    return(NULL)
  } else {
    shiny::tags$script(src = script)
  }
}

# include a JavaScript string
shinyjsInlineScript <- function(text) {
  if (missing(text) || is.null(text)) {
    return(NULL)
  } else {
    shiny::tags$script(shiny::HTML(text))
  }
}

extendShinyjs2 <- function(script, text, functions) {
  if (missing(script) && missing(text)) {
    errMsg("Either `script` or `text` need to be provided.")
  }
  sout("Starting ShinyJs")
  # if V8 is not installed, the user must provide the JS function names
  if (!requireNamespace("V8", quietly = TRUE)) {
    sout("Not using V8")
    if (missing(functions)) {
      errMsg(paste0("In order to use the `extendShinyjs()` function, you must either ",
                    "use the `functions` argument, or install the `V8` package ",
                    "with `install.packages(\"V8\")`."))
    }
    jsFuncs <- functions
  }
  # if V8 is installed (preferable method), parse the input for JS functions
  else {
    # create a js context with a `shinyjs` object that user-defined functions
    # can populate
    ct <- V8::new_context(NULL, FALSE, FALSE)
    ct$assign("shinyjs", c())
    
    # read functions from a script
    if (!missing(script)) {
      if (!file.exists(script)) {
        errMsg(sprintf("Could not find JavaScript file `%s`.", script))
      }
      
      tryCatch({
        ct$source(script)
      }, error = function(err) {
        errMsg(sprintf("Error parsing the JavaScript file: %s.", err$message))
      })
    }
    
    # read functions from in-line text
    if (!missing(text)) {
      tryCatch({
        ct$eval(text)
      }, error = function(err) {
        errMsg(sprintf("Error parsing the JavaScript code provided.", err$message))
      })
    }
    
    # find out what functions the user defined
    jsFuncs <- ct$get(V8::JS("Object.keys(shinyjs)"))
    if (length(jsFuncs) == 0) {
      errMsg(paste0("Could not find any shinyjs functions in the JavaScript file. ",
                    "Did you remember to prepend every function's name with `shinyjs.`?"))
    }
  }
  
  # add all the given functions to the shinyjs namespace so that they can be
  # called as if they were regular shinyjs functions
  lapply(jsFuncs, function(x) {
    assign(x, jsFunc, js)
  })
  sout(jsFuncs)
  # Add the script as a resource
  if (!missing(script)) {
    if (!file.exists(script)) {
      errMsg(sprintf("Could not find JavaScript file `%s`.", script))
    }
    shiny::addResourcePath("shinyjs-extend", dirname(script))
    script <- file.path("shinyjs-extend", basename(script))
  }
  
  # set up the message handlers for all functions
  sout("Calling setupJS")
  sout(jsFuncs)
  setupJS(jsFuncs, script, text)
}


#' Call user-defined JavaScript functions from R
#' @seealso \code{\link[shinyjs]{extendShinyjs}}
#' @export
#' @keywords internal
js <- new.env()