#######################################
# 10/15/2016                          #
# Sven Nelson                         #
# function to package_name::function  #
#######################################
#test
require(stringr)
require(rstudioapi)

addPackageNames <- function() { # include _ and . in func names...
  # from current file (using rstudioapi)
  currentFile <- rstudioapi::getActiveDocumentContext()
  # Parse the file to identify all functions
  parsedFile <- utils::getParseData(parse(currentFile$path))

  # Remove all but Functions and NS_GET + :: from parsedFile  & not base function
  parsedFile <- dplyr::filter(parsedFile, (text=="::" & token=="NS_GET") | token == "SYMBOL_FUNCTION_CALL" & !(text %in% builtins()))

  # Initial pacNamesAdded variable to keep track of number of insertions
  pacNamesAdded <- 0

  # Loop through parsedFile by line
  for (i in 1:length(rownames(parsedFile))) {
    # if (NS_GET and ::) {
    if (parsedFile$token[i] == "NS_GET" & parsedFile$text[i] == "::") {
      # Token for i+1 row append _hasPkgName
      parsedFile$token[i + 1] <- paste(parsedFile$token[i], "_hasPkgName")
    } else if (!exists(parsedFile$text[i])) {
      parsedFile$token[i] <- paste(parsedFile$token[i], "_funcNotFound")
    } else if (parsedFile$token[i] == "SYMBOL_FUNCTION_CALL") { # (is a function w/o pkg name)
      # Add package name at location given as below:
      # get package name (and set length of this name as a variable)
      funcName <- parsedFile$text[i]
      # get the package name associated with that function
      pacName <- utils::packageName(environment(get(funcName))) # gets package name for a given function
      # Append "::" to the end of the output for package name
      pacName <- paste(pacName, "::", sep = "")

      currentLine <- parsedFile$line1[i]
      # create location object for the location to insert "package_name::"
      loc <- rstudioapi::document_position(row = currentLine, column = parsedFile$col1[i])
      pacNamesAdded <- pacNamesAdded + 1

      #pass this info to rstudioapi::insertText
      rstudioapi::insertText(text = pacName, id = currentFile$id, location = loc)

      # if (parsedFile[line1 == line1[i]] > 1) {
       onSameLine <- dplyr::filter(parsedFile, line1 == currentLine)
       if (nrow(onSameLine) > 1) {
         #Add length of packageName to all on the same line
         parsedFile[parsedFile$line1 == currentLine,]$col1 <- parsedFile[parsedFile$line1 == currentLine,]$col1 + nchar(pacName)
         # This line not strictly necessary, but might be usefull if I ever want to use col2
         parsedFile[parsedFile$line1 == currentLine,]$col2 <- parsedFile[parsedFile$line1 == currentLine,]$col2 + nchar(pacName)
         #(doesn't matter that it adds to the previous ones because i is only increasing.)
       }
    }
  }
  # renames functions from the current package, which is probably unnecessary.
  if (pacNamesAdded == 1) {
    writeLines(paste("\nProcess complete!\n", "Package names were inserted for ", pacNamesAdded, " package.", sep = ""))
  } else {
    writeLines(paste("\nProcess complete!\n", "Package names were inserted for ", pacNamesAdded, " packages.", sep = ""))
  }
  #writeLines("Process complete!")
}

addPackageAtCursor <- function() {
  cursorText <- insertAtCursor()
  pacInfo <- insertPackageName(funcName = cursorText$funcName, fileID = cursorText$fileID, loc = cursorText$loc)

  if (is.null(pacInfo$loc)) {
    rstudioapi::insertText(text = pacInfo$pacName, id = pacInfo$fileID)
  } else {
    rstudioapi::insertText(text = pacInfo$pacName, id = pacInfo$fileID, location = pacInfo$loc)
  }
}

insertPackageName <- function(funcName, fileID, loc = NULL) { # funcName, location
  # parse text, text in front of ( (can have 1 space between) from the last space before
  # text must start with a letter of the alphabet (upper or lowercase ok).

  # That should leave only things like if, for...
  if (is.function(get(funcName))) { # Is this a function (includes base and primatives)
    # (Use mget() if funcName is a vector of names)
    if (!(funcName %in% builtins())) { # get rid of primatives like (if, for...) and base funcs
      # get the package name associated with that function
      pacName <- utils::packageName(environment(get(funcName))) # gets package name for a given function
      pacName <- paste(pacName, "::", sep = "")
      if (is.null(loc)) {
        return(list(pacName = pacName, fileID = fileID))
      } else {
        return(list(pacName = pacName, fileID = fileID, loc = loc))
      }
    }
    # ADD?:
    if (is.null(loc)) {
      return(list(pacName = "", fileID = fileID))
    } else {
      return(list(pacName = "", fileID = fileID, loc = loc))
    }
  }
}

insertAtCursor <- function() {
  # Get input from the cursor in the current document
  context <- rstudioapi::getActiveDocumentContext()
  # Get the starting position of the cursor
  cursorPos <- context$selection[[1]]$range$start
  cursorEnd <- context$selection[[1]]$range$end
  if (!identical(cursorEnd,cursorPos)) {
    context$selection[[1]]$range$end <- cursorPos
    writeLines("Inserting package name at beginning of selection.")
  }
  #cursorPos["row"]
  #cursorPos["column"]
  selectedRow <- context$content[cursorPos["row"]]
  # Parse row to identify functions
  parsedText <- utils::getParseData(parse(text = selectedRow))
  functionInfo <- dplyr::filter(parsedText, col1 == cursorPos["column"] & token == "SYMBOL_FUNCTION_CALL")
  funcName <- functionInfo$text
  return(list(funcName = funcName, fileID = context$id, loc = cursorPos))
}
