
# debugR.R, a debugging tool for the R programming language    

# see Known Issues at end of this file

# produces a window into the debuggee's source file(s); cursor will move
# in this window as the user single-steps through the source, etc.; user
# submits comments, e.g. n for next statement, from within this window,
# rarely if ever needing to shift focus to the R window

# usage: (in R terminal) debugR("primary_source_file.R")

# basic idea:  this program starts up the Unix/Linux "screen" utility
# (to be replaced with a pipe version in the near future), then starts
# up R within "screen"; when the user gives a debug command to this
# program, the latter sends it to R via the "-X stuff" command within
# "screen"; R, meanwhile, records debugging information, especially
# current source file name and line number, in a file, which is read by
# this program to update the cursor position

# stored source lines are prepended with (1-based) line number in source
# file, and possibly 'N' to indicate next line to be executed, and/or
# 'D' ("debug") to indicate a line where execution is to be paused

# Author:  Norm Matloff

suppressMessages(library(rcurses))
library(stringr)

# all globals packaged here,
# in an evironment as recommended by CRAN (http://r-pkgs.had.co.nz/check.html)
debugr <- new.env(parent = emptyenv())
debugr$scrn <- NULL  # will point to window object
debugr$row <- NULL  # current row position of cursor within window
debugr$src <- NULL  # handle for the current source file
debugr$srclen <- NULL  # length in lines of the current source file
debugr$srcpanellen <- NULL  # length in lines of the panel for displaying the source code
debugr$winwidth <- NULL  # width in characters of the window
debugr$srclines <- NULL  # contents of source file, list of strings, 1 per src line
debugr$maxdigits <- NULL  # number of digits in the longest line number
debugr$firstdisplayedlineno <- NULL  # source line number now displayed at top of window; starts at 0
debugr$currsrcfilename <- NULL  # name of source file currently in window
debugr$nextlinenum <- NA  # source line number to be executed next; starts at 1
debugr$ftns <- NULL  # dictionary of function line numberss, indexed by function name
debugr$debuggeecall <- NULL  # previous call to run debuggee, e.g. 'mybuggyfun(3)'
debugr$scroll <- 20  # amount to scroll in response to 'up' and 'down' cmds
debugr$papcmd <- ""  # expression to be printed at each pause (after n/s/c cmd)
debugr$helpbarindex <- -1  # 1-based row index saying where to put the helpbar
debugr$userinputindex <- -1  # 1-based row index saying where to put user input
debugr$msgline <- NULL  # 1-based row index saying where to put messages on window
debugr$ds <- NULL  # file handle for dbgsink file
debugr$eds <- NULL  # file handle for dbgerrorsink file
debugr$bpconds <- c()  # dictionary of breakpoints
debugr$prevcmd <- ""  # last user command
debugr$helpfile <- FALSE
debugr$Nplace <- -1
debugr$Dplace <- -1
debugr$isbrowsing <- FALSE  # TRUE if in browser() mode

# debugging function, prints variable name with variable value
p <- function(x) { print(paste0(deparse(substitute(x)),': ',x)) }

# debugging function, prints called function name to debug file
w <- function(x) { write(capture.output(x),append=TRUE) }

# finds the number of decimal digits in n
ndigs <- function(n) {
    # w(match.call()[[1]])
    return(nchar(toString(n)))
}

# writes a row in the screen, in the designated color.
# winrow is 1-based
writeline <- function(winrow,whattopaint,colorpair=NULL) {
    # Pad whattopaint with the right number of trailing spaces
    # to get a full row.
    whattopaint <- stringr::str_c(whattopaint,strrep(' ',debugr$winwidth - nchar(whattopaint)))

    # Paint the line to the console with rcurses.
    rcurses.addstr(debugr$scrn,whattopaint,winrow-1,0,colorpair)
}

# this function displays the current source file, starting at the top of
# the screen, and beginning with the row srcstartrow in debugr$srclines.
# srcstartrow is 1-based.
dispsrc <- function(srcstartrow) {
    rcurses.clear(debugr$scrn)
    winrow <- 1
    nlinestoshow <- min(debugr$srclen - srcstartrow + 1,debugr$srcpanellen)

    # paint each line of the window
    for (i in srcstartrow:(srcstartrow + nlinestoshow - 1)) {
        if (substr(debugr$srclines[i],debugr$Nplace,debugr$Nplace) == 'N') {
            if (substr(debugr$srclines[i],debugr$Dplace,debugr$Dplace) == 'D') {
                writeline(winrow,debugr$srclines[i],rcurses.color_pair(3))
            } else {
                writeline(winrow,debugr$srclines[i],rcurses.color_pair(2))
            }
        } else if (substr(debugr$srclines[i],debugr$Dplace,debugr$Dplace) == 'D') {
            writeline(winrow,debugr$srclines[i],rcurses.color_pair(1))
        } else {
            writeline(winrow,debugr$srclines[i])
        }
        winrow <- winrow + 1
    }
    debugr$firstdisplayedlineno <- srcstartrow
    rcurses.refresh(debugr$scrn)
}

# this function reads in the source file from disk, and copies it to the
# list debugr$srclines, with each source file being prepended by the line
# number
inputsrc <- function(filename) {
    lns <- try(readLines(filename))
    if (class(lns) == "try-error") {
        endscreen()
        cleancursesthings()
        stop("Failed to open file.")
    }
    
    debugr$srclen <- length(lns)

    
    debugr$maxdigits <- ndigs(length(lns) + 1)

    # location of 'N', if any
    debugr$Nplace <- debugr$maxdigits + 2

    # location of 'D', if any
    debugr$Dplace <- debugr$maxdigits + 3

    
    lnno <- 1

    
    debugr$srclines <- c()

    
    for (lineNum in 1:length(lns)) {

        # form the line number, with blanks instead of leading 0s
        ndl <- ndigs(lineNum)

        
        ### tmp <- rep(' ',debugr$maxdigits - ndl)
        ### tmp <- paste0(tmp,toString(lineNum),' ')
        tmp <- sprintf(paste0('%',debugr$maxdigits,'d'),lineNum)

        # add room for N marker for next executed and D/B for breakpoint
        tmp <- paste0(tmp,'   ')

        # now add the source line itself, truncated to fit the window
        # width, if necessary
        tmp <- paste0(tmp,lns[lineNum])

        
        ntrunclinechars <- min(debugr$winwidth,nchar(tmp))

        debugr$srclines <- c(debugr$srclines, substr(tmp,1,ntrunclinechars))
    }

    
    dispsrc(1)
}

# utility; in string s at position k, replace by string r, presumed to
# be the same length as s; new string is returned
rplc <- function(s,k,r) {
    # w(match.call()[[1]])

    # grab first k - 1 characters of string
    front <- substr(s,1,k - 1)

    # grab last characters after r added at position k
    back <- substr(s,k + nchar(r),nchar(s))

    # return concatenation
    return(paste0(front,r,back))
}

# substitutes s starting at linepos in line lineno of debugr$srclines; this
# function does NOT paint the screen, and indeed the given line may be
# currently off the screen; mainly used to add an 'N' or 'D' designation
# in a source line.
# lineno is 1-based.
rplcsrcline <- function(lineno,linepos,s) {
    # add s into source line lineno at position linepos
    debugr$srclines[lineno] <- rplc(debugr$srclines[lineno],linepos,s)
}

# deletes newline character at the end of s, returning result
chop <- function(s) {
    return(stringr::str_sub(s, 1, -2))  # cut off last character
}

# sends the command cmd to the "screen session", thus typically to R
sendtoscreen <- function(cmd) {
    cmd <- paste(cmd, '\n', sep="")
    tosend <- paste('screen -S "rdebug" -X stuff "', cmd, '" ', sep="")
    # R S3 classes use $ for member quantities; must escape it for
    # Unix-family systems; need to expand, checking for non-Unix, for
    # multiple $, etc.; note that R also requires escaping the
    # backslash!
    dollarIndex = stringr::str_locate(tosend, "\\$")[1]
    if (!is.na(dollarIndex)) {
        tosend = stringr::str_c(stringr::str_sub(tosend, 0, dollarIndex-1), "\\",
            stringr::str_sub(tosend,dollarIndex))
    }
    system(tosend)
}

checkdbgerrorsink <- function() {
    # Position the connection to where it's already positioned.
    # Oddly, I seem to have to do this seek() command, or else debugr$eds
    # won't recognize any lines that have been appended to the file since
    # the last time readLines() was called on debugr$eds.
    seek(debugr$eds, where=seek(debugr$eds), origin="start")

    # Should never be more than one line, assuming:
    # a) an R command produces at most one error message.
    # b) the error debug sink is read from each time an R
    # command is sent to screen.
    line = readLines(debugr$eds, n=-1)

    if (length(line) > 0)
        errormsg(line)
}

# initialize various globals dealing with the source file
initsrcthings <- function() {
    debugr$nextlinenum <- 1
    inputsrc(debugr$currsrcfilename)
    dispsrc(debugr$nextlinenum)
}

# initializes debugging operations; tells R to call sink(), setting up a
# duplication of screen output to file output; then tells R to input our
# buggy file
initrdebug <- function() {
    # planned change:  have sink() write to R network connection, to
    # a server that is run here; the throttling then probably won't be
    # necessary, and conditional breakpoint will be faster
    file.create('dbgsink')
    sendtoscreen("sink(\'dbgsink\',type=\'output\',split=T)")
    debugr$ds <- file("dbgsink", "r")

    file.create('dbgerrorsink')
    sendtoscreen("dbgerrorsink <- file(\'dbgerrorsink\',open=\'w\')")
    sendtoscreen("sink(dbgerrorsink,type=\'message\')")
    debugr$eds <- file("dbgerrorsink", "r")
}

# Returns all the latest lines in the sink file that have not yet been
# read through the global connection.
# Used for getting updates on what the user is currently debugging
# (if anything) (e.g. which function is being debugged).
readfromgbds <- function() {
    # Position the connection to where it's already positioned.
    # Oddly, I seem to have to do this seek() command, or else debugr$ds
    # won't recognize any lines that have been appended to the file since
    # the last time readLines() was called on debugr$ds.
    seek(debugr$ds, where=seek(debugr$ds), origin="start")

    lines = readLines(debugr$ds, n=-1)
    return(lines)
}

# find the latest line in the sink file that starts with either 'debug
# at' (pause line) or 'exiting from' (exit R debugger), returning that
# line 
finddebugline <- function() {
    # go back to start of file to read all lines
    # seek(debugr$ds, where=0, origin="start")
    # sinkfilelines <- readLines(debugr$ds, n=-1)
    sinkfilelines <- readfromgbds()

    numlines <- length(sinkfilelines)
    for (i in numlines:1) {
        # Check for line of the form, e.g.:
        # exiting from: g()
        if (!is.na(stringr::str_locate(sinkfilelines[i], "exiting from")[1])) {
            return(c('exiting', sinkfilelines[i]))
        }
        # Check for line of either form, e.g.:
        # debug at test.R#9: {
        # test.R#4
        else if (!is.na(stringr::str_locate(sinkfilelines[i],
            stringr::str_c(debugr$currsrcfilename,"#"))[1])) {
            return(c('debug', sinkfilelines[i]))
        }
    }
    return(NA)
}

# determines if linenum of the current src is in the current window
inwin <- function(linenum) {
    firstdisp = debugr$firstdisplayedlineno
    return (linenum >= firstdisp && linenum < firstdisp + debugr$srcpanellen)
}

# change the highlighting color of a line that's in the current window,
# to reflect that it's the current line or a pause line
# wrow is 1-based.
updatecolor <- function(wrow, linenum) {
    tmp = debugr$srclines[linenum]
    if (stringr::str_sub(tmp, debugr$Nplace, debugr$Nplace) == 'N') {
        if (stringr::str_sub(tmp, debugr$Dplace, debugr$Dplace) == 'D') {
            colorpair = rcurses.color_pair(3)
        } else {
            colorpair = rcurses.color_pair(2)
        }
    } else if (stringr::str_sub(tmp, debugr$Dplace, debugr$Dplace) == 'D') {
        colorpair = rcurses.color_pair(1)
    } else {
        colorpair = rcurses.color_pair(0)
    }
    writeline(wrow,tmp,colorpair)
    rcurses.refresh(debugr$scrn)
}

# update the indicators, e.g. N mark, of where the next line to be
# executed is; newnextlinenum is 1-based
updatenext <- function(newnextlinenum) {
    oldnextlinenum = debugr$nextlinenum
    rplcsrcline(oldnextlinenum,debugr$Nplace,' ')
    if (inwin(oldnextlinenum)) {
        winrow = oldnextlinenum - debugr$firstdisplayedlineno + 1
        updatecolor(winrow,oldnextlinenum)
    }
    debugr$nextlinenum <- newnextlinenum
    rplcsrcline(newnextlinenum,debugr$Nplace,'N')
    debugr$isbrowsing <- TRUE
    if (inwin(newnextlinenum)) {
        winrow = newnextlinenum - debugr$firstdisplayedlineno + 1
        updatecolor(winrow,newnextlinenum)
    } else {
        # If the next line is out of src code view,
        # scroll so that this line is at top of view.
        dispsrc(newnextlinenum)
    }
}

# blank out the given line in the current window
# winrow is 1-based
blankline <- function(winrow) {
    writeline(winrow,stringr::str_dup(' ', debugr$winwidth-1))
}

# when we hit a pause, or exit the R debugger, this function will
# determine what line we paused at, or that we did exit
checkdbgsink <- function() {
    # now must find current src file, line num; works on the basis of the
    # lines in the sink file being of the form, e.g.
    #    debug at test.R#3: for (i in 1:3) {
    found = finddebugline()
    # need the if, as dbgsink may still be empty at this point.
    # use any() to avoid a warning.
    if (!any(is.na(found))) {
        sinkline = found[2]
        colonplace = stringr::str_locate(sinkline, ":")[1]
        if (found[1] == 'debug') {
            linenumstart = stringr::str_locate(sinkline, "#")[1] + 1
            # get file name before # sign
            # srcfile = stringr::str_sub(sinkline, 10, linenumstart-2)
            if (is.na(colonplace))  # if no colon found on this line
                linenum = as.integer(stringr::str_sub(sinkline, linenumstart))
            else
                linenum = as.integer(stringr::str_sub(sinkline, linenumstart,
                    colonplace-1))
            if (iscondbphere(linenum)) {  # if conditional breakpoint
                # Print the condition of the conditional breakpoint so we
                # can check its value (true/false).
                doprint(stringr::str_c('p ',debugr$bpconds[linenum]))

                # go back to start of file to read all lines, so we can read
                # last line (doesn't seem to be a cleaner way).
                # seek(debugr$ds, where=0, origin="start")
                # sinkfilelines <- readLines(debugr$ds, n=-1)
                sinkfilelines <- readfromgbds()
                lastline = sinkfilelines[length(sinkfilelines)]

                # if bp condition doesn't hold, do not stop at it
                if (!is.na(stringr::str_locate(lastline, "FALSE")[1])) {
                    if (debugr$prevcmd != "n") {
                        dostep("c")
                        return()
                    }
                }
            }
            updatenext(linenum)
        } else if (found[1] == 'exiting') {  # debugging stopped due to function end
            linenum = debugr$nextlinenum
            winrow = linenum - debugr$firstdisplayedlineno + 1
            rplcsrcline(linenum,debugr$Nplace,' ')  # there's no longer a "next" line
            debugr$isbrowsing <- FALSE
            writeline(winrow,debugr$srclines[linenum],rcurses.color_pair(0))
            debugr$papcmd <- ''
            blankline(debugr$srcpanellen + 3)
            rcurses.refresh(debugr$scrn)
        }
    } else {  # debugging stopped due to error within function
        # Clear the 'N' on the former next line.
        oldnextlinenum = debugr$nextlinenum
        rplcsrcline(debugr$nextlinenum,debugr$Nplace,' ')
        debugr$isbrowsing <- FALSE
        winrow = oldnextlinenum - debugr$firstdisplayedlineno + 1
        writeline(winrow,debugr$srclines[oldnextlinenum],
            rcurses.color_pair(0))
    }
}

dostep <- function(cmd) {
    if (cmd == 's') {
        # assumes an isolated function call, e.g. not a call within a
        # call, so function name is the first non-whitespace char in the
        # line, and ')' immediately follows the function name
        currline <- debugr$srclines[debugr$nextlinenum]
        currline <- stringr::str_sub(currline, (debugr$Dplace+1))  # remove line number etc.
        ftnpart <- stringr::str_trim(currline, "left")  # remove leading whitespace
        parenplace <- stringr::str_locate(ftnpart, '\\(')[1]
        ftnname <- stringr::str_sub(ftnpart, 1, parenplace-1)
        cmd = stringr::str_c("debugonce(", ftnname, "); c")
    }
    sendtoscreen(cmd)
    Sys.sleep(0.25)
    checkdbgsink()
    if (debugr$papcmd != "") {
        doprint(debugr$papcmd)
    }
}

# Send "f" to screen. Assumes screen is in debugging mode.
dof <- function() {
    sendtoscreen("f")
    Sys.sleep(0.5)
    checkdbgsink()
}

# run the debuggee call
dorun <- function(cmd) {
    # if function to call was specified, run it; otherwise, run the last one
    if (cmd != "rn") {
        debugr$debuggeecall <- stringr::str_split(cmd, " ", simplify=TRUE)[2]
    }
    sendtoscreen(debugr$debuggeecall)
    Sys.sleep(0.5)
    checkdbgsink()
}

# utility: removes the first k nonwhitespace tokens, e.g.
# e.g. with k = 2, inputting 'a + b + c' returns 'b + c'
removefirsttokens <- function(k, s) {
    # Start the substring after the kth whitespace character.
    startspliceindex = stringr::str_locate_all(s, " ")[[1]][k,1] + 1
    return(stringr::str_sub(s, startspliceindex))
}

doprint <- function(cmd) {
    pcmd = stringr::str_split(cmd, " ", simplify=TRUE)[1]
    expressiontoprint = removefirsttokens(1,cmd)
    if (pcmd == 'pc') {
        sendtoscreen(expressiontoprint)
        return()
    }

    # Print the line in screen, then retrieve the line that was printed
    # from the sink.
    tosend = stringr::str_c("cat(", expressiontoprint, ",fill=TRUE)")
    sendtoscreen(tosend)
    Sys.sleep(0.25)  # give time for screen output to be written to dbgsink
    ds = file("dbgsink", "r")
    printedline = tail(readLines(ds, n=-1), 1)
    toprint = stringr::str_c(expressiontoprint, " = ", printedline)
    writeline(debugr$msgline,toprint,rcurses.color_pair(0))
    close(ds)
}

# print R expression after each n or d cmd
dopap <- function(cmd) {
    pcmd = stringr::str_split(cmd," ",simplify=TRUE)[1]
    expressiontoprint = removefirsttokens(1,cmd)
    if (pcmd == 'pcap') {
        debugr$papcmd <- stringr::str_c('pc ', expressiontoprint)
    } else {
        debugr$papcmd <- stringr::str_c('p ', expressiontoprint)
    }
    doprint(debugr$papcmd)
}

# given (1-based) line number in current source file, returns the name
# of the function that begins on that line. if no function there,
# returns NA.
findftnnamebylinenum <- function(linenum) {
    srcline <- debugr$srclines[linenum]
    srcline <- stringr::str_split(srcline, " ", simplify=TRUE)
    fnamepos <- match("<-", srcline) - 1  # func name is 1 token before <-
    if (is.na(fnamepos)) {
        return(NA)
    } else {
        return(srcline[fnamepos])
    }
}

# given name of a function in the current source file, returns the 
# (1-based) number of the line at which it begins. if fail to find
# function, returns NA.
findftnlinenumbyname <- function(fname) {
    for (i in 1:length(debugr$srclines)) {
        possiblefname = findftnnamebylinenum(i)
        if (!is.na(possiblefname)) {  # if there was a function declared on this line
            if (possiblefname == fname) {
                return(i)
            }
        }
    }
    return(NA)  # function not found
}

# given (1-based) line number in current source file, returns the name
# of the function that includes this line; assumes no "functions defined
# within functions"
findenclosingftn <- function(linenum) {
    # Start at given line number and keep going up a line until
    # find name of the enclosing function.
    i = linenum
    while (i > 0) {
        line = debugr$srclines[i]
        # if function on this line
        if (!is.na(stringr::str_locate(line,"<- function")[1])) {
            fname = findftnnamebylinenum(i)
            if (!is.na(fname))
                return(fname)
        }
        i = i - 1  # go up a line
    }
    return(NA)
}

# Returns TRUE if given str starts with number; otherwise, FALSE.
stringstartswithnumber <- function(str) {
    return(grepl("^[0-9].*", str))
}

# call R debug() or undebug() on the given function; specified either by
# line number or function name; for now, assumes blanks surround '<-' in
# the assignment line in which the function is defined
dodf <- function(cmd) {
    cmdparts <- stringr::str_split(cmd, " ", simplify=TRUE)
    fspec <- cmdparts[2]

    # Determine both function line number and name.
    if (stringstartswithnumber(fspec)) {  # if function specified by line number
        fline = as.integer(fspec)
        fname = findftnnamebylinenum(fline)
    } else {  # if function specified by name
        fname = fspec
        fline = findftnlinenumbyname(fname)
        if (is.na(fline)) {  # couldn't find line number of this function
            errormsg(str_c("Nonexistent function name: ",fname))
            return()
        }
    }

    # Update the function's debug flag.
    if (cmdparts[1] == "df") {
        tosend = stringr::str_c("debug(", fname, ")")
    } else {
        tosend = stringr::str_c("undebug(", fname, ")")
    }
    sendtoscreen(tosend)

    # mark the src line D for "debug", blank out the D if undebug
    if (cmdparts[1] == "df") {
        rplcsrcline(fline,debugr$Dplace,'D')
    } else {
        rplcsrcline(fline,debugr$Dplace,' ')
    }

    # if it's currently on the screen, update there
    firstdisp = debugr$firstdisplayedlineno
    if (inwin(fline)) {
        winrow = fline - firstdisp + 1
        if (cmdparts[1] == "df") {
            updatecolor(winrow,fline)
        } else {  # undebug case
            updatecolor(winrow,fline)
        }
    }
}

# call undebug() on all functions currently in debug state
doudfa <- function() {
    for (i in 1:length(debugr$srclines)) {
        if (stringr::str_sub(debugr$srclines[i], debugr$Dplace, debugr$Dplace) == "D") {
            dodf(stringr::str_c("udf ", i))
        }
    }
}

# Returns TRUE if there is conditional breakpoint at given 1-based line number.
iscondbphere <- function(lineno) {
    if (length(debugr$bpconds) > 0)  # if even are any breakpoints
        return(!is.na(debugr$bpconds[lineno]))
    else
        return(FALSE)
}

# setBreakpoint() will be called on the requested source line, specified by
# (1-based) line number in the current source file
dobp <- function(cmd) {
    cmdparts = stringr::str_split(cmd, ' ', simplify=TRUE)
    linenum = cmdparts[2]
    filename = debugr$currsrcfilename
    tosend = stringr::str_c("setBreakpoint(\'", filename, "\',", linenum, ")")
    sendtoscreen(tosend)
    # mark the src line D for "debug"
    fline = as.integer(linenum)
    rplcsrcline(fline,debugr$Dplace,"D")
    # if it's currently on the screen, update there
    if (inwin(fline)) {
        firstdisp = debugr$firstdisplayedlineno
        winrow = fline - firstdisp + 1
        updatecolor(winrow,fline)
    }
    # add to our list of conditional breakpoints
    if (length(cmdparts) > 2)  # if conditional breakpoint (condition is 3rd arg)
        debugr$bpconds[fline] <- removefirsttokens(2,cmd)
}

doubp <- function(cmd) {
    cmdparts = stringr::str_split(cmd, ' ', simplify=TRUE)
    linenum = cmdparts[2]
    ftnname = findenclosingftn(as.integer(linenum))
    tosend = stringr::str_c("untrace(", ftnname, ")")
    # unfortunately, untrace() does an auto undebug(), so need to update
    dodf(stringr::str_c("udf ", ftnname))
    sendtoscreen(tosend)
    fline = as.integer(linenum)
    rplcsrcline(fline,debugr$Dplace,' ')
    # if it's currently on the screen, update there
    if (inwin(fline)) {
        firstdisp = debugr$firstdisplayedlineno
        winrow = fline - firstdisp + 1
        updatecolor(winrow,fline)
    }
    # if there is a conditional breakpoint for this fline
    if (iscondbphere(fline))
        debugr$bpconds[fline] <- NA
}

doreloadsrc <- function(cmd) {
    doudfa()
    loadsrc = stringr::str_c("source(\'", debugr$currsrcfilename, "\')")
    sendtoscreen(loadsrc)
    inputsrc(debugr$currsrcfilename)
}

dodown <- function() {
    newstartline = min(debugr$firstdisplayedlineno+debugr$scroll,debugr$srclen)
    dispsrc(newstartline)
}

doup <- function() {
    newstartline = max(debugr$firstdisplayedlineno-debugr$scroll,1)
    dispsrc(newstartline)
}

dopls <- function() {
    tosend = "ls.str()"
    sendtoscreen(tosend)
}

dopenv <- function(cmd) {
    e = stringr::str_split(cmd," ",simplify=TRUE)[2]  # the environment to print contents of
    if (is.na(e))  {
        # if no environment given, print current environment
        tosend = stringr::str_c("ls.str()")
    } else {
        tosend = stringr::str_c("ls.str(envir=", e, ")")
    }
    sendtoscreen(tosend)
}

doquitbrowser <- function() {
    sendtoscreen('Q')
    oldnextlinenum = debugr$nextlinenum
    if (!is.na(oldnextlinenum)) {
        rplcsrcline(oldnextlinenum,debugr$Nplace,' ')
        debugr$isbrowsing <- FALSE
        if (inwin(oldnextlinenum)) {
            winrow = oldnextlinenum - debugr$firstdisplayedlineno + 1
            updatecolor(winrow,oldnextlinenum)
        }
    }
    debugr$papcmd <- ''
    blankline(debugr$srcpanellen + 3)
}

dohelp <- function() {
    if (!debugr$helpfile) {
        # open this R source file, find the help section, make a tmp
        # file from it, and have R invoke the user's favorite text editor
        # on it.
        hf = system.file('help.txt',package='dbgR') 
        hflines = readLines(hf)
        ## hf = file("R/debugR.R", "r")  # hardcode file name, for now
        hf = system.file("help.txt",package='dbgR')
        hflines = readLines(hf)
        hfout = file("/tmp/debugRhelp","w")
        cat(hflines,sep="\n",file=hfout)
        close(hfout)
        debugr$helpfile <- TRUE
    }
    tosend = "edit(file=\'/tmp/debugRhelp\')"
    sendtoscreen(tosend)
}

# initialize rcurses environment
initcursesthings <- function() {
    # w(match.call()[[1]])

    # initializes the screen for rcurses
    debugr$scrn <- rcurses.initscr()

    # disables line buffering and erase/kill character-processing
    rcurses.cbreak()

    # screen will be cleared on next call to refresh
    rcurses.clear(debugr$scrn)

    # allows support of color attributes on terminals
    rcurses.start_color()

    # initialize color pair for source code line that has a breakpoint
    rcurses.init_pair(1,rcurses.COLOR_BLACK,rcurses.COLOR_RED)

    # initialize color pair for source code line that's the current line
    rcurses.init_pair(2,rcurses.COLOR_BLACK,rcurses.COLOR_GREEN)

    # initialize color pair for source code line that's current and breakpoint
    rcurses.init_pair(3,rcurses.COLOR_BLACK,rcurses.COLOR_YELLOW)

    # initialize color pair for remaining source code
    rcurses.init_pair(8,rcurses.COLOR_BLACK,rcurses.COLOR_WHITE)

    # set background color pair
    rcurses.bkgd(debugr$scrn,' ',rcurses.color_pair(8))

    # leave 3 lines at the bottom.
    debugr$srcpanellen <- rcurses.LINES() - 3
    debugr$helpbarindex <- rcurses.LINES() - 2
    debugr$userinputindex <- rcurses.LINES() - 1
    debugr$msgline <- rcurses.LINES()  # last line

    debugr$winwidth <- rcurses.COLS()

    
    
    rcurses.refresh(debugr$scrn)
}

cleancursesthings <- function() {
    rcurses.nocbreak()
    rcurses.endwin()
}

errormsg <- function(err) {
    blankline(debugr$msgline)
    writeline(debugr$msgline,err)
    rcurses.refresh(debugr$scrn)
}

getusercmd <- function() {
    rcurses.move(debugr$scrn,debugr$userinputindex-1,0)  # rcurses is 0-based, so -1
    cmd <- rcurses.getstr(debugr$scrn)

    # if user simply hits Enter, then re-do previous command
    if (cmd == '' && debugr$prevcmd != "") {
        return(debugr$prevcmd)
    } else {
        return(cmd)
    }
}

setupscreen <- function() {
    # start "screen, with name 'rdebug' for now
    system('xterm -e "screen -S \'rdebug\'" &')
    # start R within screen
    Sys.sleep(1)
    sendtoscreen('R --no-save -q')
}

# Terminates screen terminal.
endscreen <- function() {
    if (debugr$isbrowsing == TRUE)
        sendtoscreen('Q')  # exit browser() mode
    sendtoscreen("sink(type=\'message\')")  # close error sink
    sendtoscreen("close(dbgerrorsink)")  # close error sink file
    sendtoscreen('quit()')
    sendtoscreen('exit')
    system('killall screen')
    system('screen -wipe')
}

# Calls browser() if given condition is false, and allows user to inspect
# the given environment, with some helpful suggestions. A more experienced
# user may prefer to use browser(expr=(!condition)) rather than
# assert(condition,...), but both allow inspection of the execution
# environment of the caller of assert().
#
# condition: only call browser if FALSE.
# env: environment to allow browser() to inspect. As the
# contained call to browser() inspects assert()'s execution environment
# (i.e. condition and env), we need the env parameter so that the
# calling envirnoment can be inspected, so that the caller of assert()
# can figure out what went wrong. Usually, one would pass environment(),
# e.g. assert(someCondition, env=environment()).
assert <- function(condition, env) {
    if (!condition) {
        # Helpful messages for the (assumed beginner) programmer.
        message("Use `where` to see the call stack.")
        message("Use `ls(env)` or `ls.str(env)` to see the variables in \
            the caller of assert().")
        message("Use `env$varName` to see varName's value.")

        browser()
    }
}

# Use this to kill 'screen' sessions, in the case where debugr
# was closed without the 'screen' session dying.
killScreen <- function() {
    system('killall screen')
}

debugR <- function(filename) {
    # check for existing 'screen' sessions with name 'rdebug'
    tmp <- system('screen -ls | grep rdebug')
    if (tmp == 0) {
        cat('rdebug screen running\n')
        cat('kill screen process, then run "screen -wipe"\n')
    }

    setupscreen()
    initcursesthings()

    # save the file name in a global variable
    debugr$currsrcfilename <- filename

    # initialize global variables related to source code
    initsrcthings()

    # have R read in the source file to be debugged
    loadsrc = paste("source(", "\'", debugr$currsrcfilename, "\'", ")", sep="")
    sendtoscreen(loadsrc)

    initrdebug()

    # one iteration of this loop handles one user command, e.g. one
    # "continue" or one "next"
    while (TRUE) {

        # set console
        tmp <- (debugr$winwidth - 1 - nchar(' h for help ')) / 2

        # put the help bar on the screen
        helpbartext <- stringr::str_c(stringr::str_dup(' ',tmp),' h for help ',stringr::str_dup(' ',tmp))
        writeline(debugr$helpbarindex,helpbartext,rcurses.A_REVERSE)

        # clear user's previous input
        writeline(debugr$userinputindex,stringr::str_dup(' ',debugr$winwidth - 1))

        fullcmd <- getusercmd()
        # specifies the command without params
        cmd = stringr::str_split(fullcmd," ",simplify=TRUE)[1]

        # clear error msg after user input (i.e. after they saw it)
        errormsg("")

        # check for Next or Continue
        if (cmd == 'n' || cmd == 's' || cmd == 'c') {
            dostep(fullcmd)
        }

        else if (cmd == 'f') {
            dof()
        }

        # check for Debug Ftn command
        else if (cmd == 'df') {
            dodf(fullcmd)
        }

        # check for UndebugAll Ftn command
        else if (cmd == 'udfa') {
            doudfa()
        }

        # check for Undebug Ftn command
        else if (cmd == 'udf') {
            dodf(fullcmd)
        }

        # check for Set Breakpoint command
        else if (cmd == 'bp') {
            dobp(fullcmd)
        }

        # check for Unset Breakpoint command
        else if (cmd == 'ubp') {
            doubp(fullcmd)
        }

        # check for Run command
        else if (cmd == 'rn') {
            dorun(fullcmd)
        }

        # check for Print at Pause command
        else if (cmd == 'pap') {
            dopap(fullcmd)
        }

        # check for Undo Print at Pause command
        else if (cmd == 'upap') {
            debugr$papcmd <- ''
        }

        else if (cmd == 'pls') {
            dopls()
        }

        else if (cmd == 'penv') {
            dopenv(fullcmd)
        }

        # check for Print command
        else if (cmd == 'p') {
            doprint(fullcmd)
        }

        # check for Print to Console command
        else if (cmd == 'pc') {
            doprint(fullcmd)
        }

        # check for Print to Console at Pause command
        else if (cmd == 'pcap') {
            dopap(fullcmd)
        }

        # check for Undo Print to Console at Pause command
        else if (cmd == 'upcap') {
            debugr$papcmd <- ''
        }

        # check for Source Reload command
        else if (cmd == 'rl') {  # tell R to reload current source file
            doreloadsrc()
        }

        # check for scrolling
        else if (cmd == 'down') {
            dodown()
        }

        
        else if (cmd == 'up') {
            doup()
        }

        # (re)load source file
        else if (cmd == 'ls') {
            cmdsplit = stringr::str_split(cmd, ' ', simplify=TRUE)
            if (length(cmdsplit) > 1) {  # if file name given
                debugr$currsrcfilename <- cmdsplit[2]
            }
            initsrcthings()
            loadsrc = stringr::str_c("source(\'",debugr$currsrcfilename,"\')")
            sendtoscreen(loadsrc)
        }

        # quit R browser
        else if (cmd == 'Q') {
            doquitbrowser()
        }

        # check for End Session command (stops R, screen and exits Python)
        else if (cmd == 'es') {
            endscreen()
            cleancursesthings()
            close(debugr$ds)
            break
        }

        # display help information
        else if (cmd == 'h') {
            dohelp()
        }

        # command not recognized
        else {
            errormsg('no such command')
        }

        # Debug error sink may not yet be set up.
        if (!is.null(debugr$eds)) {
            checkdbgerrorsink()  # report any error in screen to user
        }

        # save previous command
        debugr$prevcmd <- fullcmd
    }
}

# HELP SECTION
#  
# New users scroll down to Quick Start section below.  
#  
# Command List:
#  
# Enter key:  repeat last command (should use this a lot, e.g. for n)
#   
# rn expr:  Run the given expression; if no expression, use the previous Run
#   
# n,s,c:  go to Next line/Step to function/Continue until next pause
#   
# df f, udf f:  Debug/Undebug f()
# udfa:  Undebug all functions
#   
# bp linenum:  set Breakpoint at given line
# bp linenum expr:  set Breakpoint at given line, conditional on expr
# ubp linenum:  cancel Breakpoint at the given line
#   
# p expr:  Print expression
# pap expr:  Print expression at each Pause (only one expression at a time)
# upap:  cancel pap
#   
# pc expr:  Print expression to Console
# pcap expr:  Print expression to Console at each Pause 
# upcap:  cancel pcap
#
# pls: print local variables (including args) of the current function
# penv e: print contents of the environment e
# 
# down: scroll down
# up: scroll down
#   
# Q:  quit R's debugger
# es:  exit debugR program
#   
# ls srcfile:  (re)load source file; if no file given, use the previous one
#   
#   
# Tips:
#
#    (a) Make good use of the Enter command, especially for repeating
#        the Next or Continue command.
#   
#    (b) To print more than one item, use c() or str(), e.g.
#
#        p c(i,j)
#  
#    (c) To print a complicated object, say a matrix, use pc, e.g.
#
#        pc somematrix
#   
#    (d) To print something repeatedly as you step through the code,
#        use pap or pcap.
#    
#    (e) Don't define functions within functions.  The R internal
#        debug operations don't handle this well.
#  
# Quick Start:
#  
# Create a file test.R with contents
#   
#    f <- function() {
#       sum <- 0
#       for (i in 1:3) {
#          sum <- sum + i
#       }
#       sum
#    }
#   
# At the shell command line, type
#   
#    python debugR.py test.R
#   
# Then debugR will appear in your shell window, and it will invoke
# an R session in a new window.  In the debugR window, type
#   
#    df f
#    rn f()
#   
# That says to set the function f() to R debug state, and run f().  Then hit
# n to go from line to line, hitting c to continue, Q to exit the R
# debugger (but not debugR).  Hit es to end this debugR session.
#  
# See the Command List section above for a full list of commands.  Be
# sure to read the Tips section too.
#  

# KNOWN ISSUES

# the s command doesn't work if n has not been used fierst?

# need to implement $ escape for non-Unix family systems
