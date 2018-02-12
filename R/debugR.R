# this is the R version of debugR.py

suppressMessages(library(rcurses))
library(stringr)

# all globals packaged here
gb.scrn <- NULL  # will point to window object
gb.row <- NULL  # current row position of cursor within window
gb.src <- NULL  # handle for the current source file
gb.srclen <- NULL  # length in lines of the current source file
gb.winlen <- NULL  # length in lines of the window
gb.winwid <- NULL  # width in characters of the window
gb.srclines <- NULL  # contents of source file, list of strings, 1 per src line
gb.maxdigits <- NULL  # number of digits in the longest line number
gb.firstdisplayedlineno <- NULL  # source line number now displayed at top of window; starts at 0
gb.currsrcfilename <- NULL  # name of source file currently in window
gb.nextlinenum <- NA  # source line number to be executed next; starts at 1
gb.ftns <- NULL  # dictionary of function line numberss, indexed by function name
gb.debuggeecall <- NULL  # previous call to run debuggee, e.g. 'mybuggyfun(3)'
gb.scroll <- 20  # amount to scroll in response to 'up' and 'down' cmds
gb.papcmd <- ""  # expression to be printed at each pause (after n/s/c cmd)
gb.msgline <- NULL  # line in window where messages are printed
gb.ds <- NULL  # file handle for dbgsink file
gb.bpconds <- NULL  # dictionary of breakpoints
gb.prevcmd <- ""  # last user command
gb.helpfile <- FALSE

# debugging function, prints variable name with variable value
p <- function(x) { print(paste0(deparse(substitute(x)),': ',x)) }

# debugging function, prints called function name to debug file
w <- function(x) { write(capture.output(x),append=TRUE) }

# finds the number of decimal digits in n
ndigs <- function(n) {
    # w(match.call()[[1]])
    return(nchar(toString(n)))
}

# paints a row in the screen, in the designated color
# winrow is 0-based
paintcolorline <- function(winrow,whattopaint,colorpair) {
    whattopaint <- paste0(whattopaint,strrep(' ',gb.winwid - nchar(whattopaint)))
    rcurses.addstr(gb.scrn,whattopaint,winrow,0,colorpair)

    # manadatory return statement
    return(NULL)
}

# this function displays the current source file, starting at the top of
# the screen, and beginning with the row srcstartrow in gb.srclines.
# srcstartrow is 1-based.
dispsrc <- function(srcstartrow) {
    rcurses.clear(gb.scrn)
    winrow <- 0
    nlinestoshow <- min(gb.srclen - srcstartrow + 1,gb.winlen)

    # paint each line of the window
    for (i in srcstartrow:(srcstartrow + nlinestoshow - 1)) {
        if (substr(gb.srclines[i],gb.Nplace,gb.Nplace) == 'N') {
            if (substr(gb.srclines[i],gb.Dplace,gb.Dplace) == 'D') {
                paintcolorline(winrow,gb.srclines[i],rcurses.color_pair(3))
            } else {
                paintcolorline(winrow,gb.srclines[i],rcurses.color_pair(2))
            }
        } else if (substr(gb.srclines[i],gb.Dplace,gb.Dplace) == 'D') {
            paintcolorline(winrow,gb.srclines[i],rcurses.color_pair(1))
        } else {
            rcurses.addstr(gb.scrn,gb.srclines[i],winrow,0)
        }
        winrow <- winrow + 1
    }
    gb.firstdisplayedlineno <<- srcstartrow
    rcurses.refresh(gb.scrn)

    # manadatory return statement
    return(NULL)
}

# this function reads in the source file from disk, and copies it to the
# list gb.srclines, with each source file being prepended by the line
# number
inputsrc <- function(filename) {
    lns <- readLines(filename)

    
    gb.srclen <<- length(lns)

    
    gb.maxdigits <<- ndigs(length(lns) + 1)

    # location of 'N', if any
    gb.Nplace <<- gb.maxdigits + 2

    # location of 'D', if any
    gb.Dplace <<- gb.maxdigits + 3

    
    lnno <- 1

    
    gb.srclines <<- c()

    
    for (lineNum in 1:length(lns)) {

        # form the line number, with blanks instead of leading 0s
        ndl <- ndigs(lineNum)

        
        tmp <- rep(' ',gb.maxdigits - ndl)

        
        tmp <- paste0(tmp,toString(lineNum),' ')

        # add room for N marker for next executed and D/B for breakpoint
        tmp <- paste0(tmp,'   ')

        # now add the source line itself, truncated to fit the window
        # width, if necessary
        tmp <- paste0(tmp,lns[lineNum])

        
        ntrunclinechars <- min(gb.winwid,nchar(tmp))

        gb.srclines <<- c(gb.srclines, substr(tmp,1,ntrunclinechars))
    }

    
    dispsrc(1)

    # manadatory return statement
    return(NULL)
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

# substitutes s starting at linepos in line lineno of gb.srclines; this
# function does NOT paint the screen, and indeed the given line may be
# currently off the screen; mainly used to add an 'N' or 'D' designation
# in a source line
rplcsrcline <- function(lineno,linepos,s) {
    # add s into source line lineno at position linepos
    gb.srclines[lineno] <<- rplc(gb.srclines[lineno],linepos,s)

    # manadatory return statement
    return(NULL)
}

# deletes newline character at the end of s, returning result
chop <- function(s) {
    return(str_sub(s, 1, -2))  # cut off last character
}

# sends the command cmd to the "screen session", thus typically to R
sendtoscreen <- function(cmd) {
    cmd <- paste(cmd, '\n', sep="")
    tosend <- paste('screen -S "rdebug" -X stuff "', cmd, '" ', sep="")
    # R S3 classes use $ for member quantities; must escape it for
    # Unix-family systems; need to expand, checking for non-Unix, for
    # multiple $, etc.; note that R also requires escaping the
    # backslash!
    dollarIndex = str_locate(tosend, "\\$")[1]
    if (!is.na(dollarIndex)) {
        tosend = str_c(str_sub(tosend, 0, dollarIndex-1), "\\",
            str_sub(tosend,dollarIndex))
    }
    system(tosend)
}

# initialize various globals dealing with the source file
initsrcthings <- function() {
    # w(match.call()[[1]])

    
    gb.nextlinenum <<- 1

    
    inputsrc(gb.currsrcfilename)

    
    rplcsrcline(1,gb.Nplace,'N')

    
    dispsrc(gb.nextlinenum)

    # manadatory return statement
    return(NULL)
}

# initializes debugging operations; tells R to call sink(), setting up a
# duplication of screen output to file output; then tells R to input our
# buggy file
initrdebug <- function() {
    # planned change:  have sink() write to R network connection, to
    # a server that is run here; the throttling then probably won't be
    # necessary, and conditional breakpoint will be faster
    file.create('dbgsink')
    sendtoscreen("sink(\'dbgsink\',split=T)")
    gb.ds <<- file("dbgsink", "r")
}

# find the latest line in the sink file that starts with either 'debug
# at' (pause line) or 'exiting from' (exit R debugger), returning that
# line 
finddebugline <- function() {
    # go back to start of file to read all lines
    seek(gb.ds, where=0, origin="start")
    sinkfilelines <- readLines(gb.ds, n=-1)

    numlines <- length(sinkfilelines)
    for (i in numlines:1) {
        if (!is.na(str_locate(sinkfilelines[i], "exiting from")[1])) {
            return(c('exiting', sinkfilelines[i]))
        } else if (!is.na(str_locate(sinkfilelines[i], "debug at")[1])) {
            return(c('debug', sinkfilelines[i]))
        }
    }
    return(NA)
}

# determines if linenum of the current src is in the current window
inwin <- function(linenum) {
    firstdisp = gb.firstdisplayedlineno
    return (linenum >= firstdisp && linenum < firstdisp + gb.winlen)
}

# change the highlighting color of a line that's in the current window,
# to reflect that it's the current line or a pause line
# wrow is 1-based.
updatecolor <- function(wrow, linenum) {
    tmp = gb.srclines[linenum]
    if (str_sub(tmp, gb.Nplace, gb.Nplace) == 'N') {
        if (str_sub(tmp, gb.Dplace, gb.Dplace) == 'D') {
            colorpair = rcurses.color_pair(3)
        } else {
            colorpair = rcurses.color_pair(2)
        }
    } else if (str_sub(tmp, gb.Dplace, gb.Dplace) == 'D') {
        colorpair = rcurses.color_pair(1)
    } else {
        colorpair = rcurses.color_pair(0)
    }
    paintcolorline(wrow-1,tmp,colorpair)
    rcurses.refresh(gb.scrn)
}

# update the indicators, e.g. N mark, of where the next line to be
# executed is; newnextlinenum is 1-based
updatenext <- function(newnextlinenum) {
    oldnextlinenum = gb.nextlinenum
    rplcsrcline(oldnextlinenum,gb.Nplace,' ')
    if (inwin(oldnextlinenum)) {
        winrow = oldnextlinenum - gb.firstdisplayedlineno + 1
        updatecolor(winrow,oldnextlinenum)
    }
    gb.nextlinenum <<- newnextlinenum
    rplcsrcline(newnextlinenum,gb.Nplace,'N')
    if (inwin(newnextlinenum)) {
        winrow = newnextlinenum - gb.firstdisplayedlineno + 1
        updatecolor(winrow,newnextlinenum)
    } else {
        dispsrc(newnextlinenum)
    }
}

# blank out the given line in the current window
blankline <- function(winrow) {
    rcurses.addstr(gb.scrn, str_dup(' ', gb.winwid-1), winrow, 0)
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
        colonplace = str_locate(sinkline, ":")[1]
        if (found[1] == 'debug') {
            linenumstart = str_locate(sinkline, "#")[1] + 1
            # get file name before # sign
            srcfile = str_sub(sinkline, 10, linenumstart-2)
            linenum = as.integer(str_sub(sinkline, linenumstart, colonplace-1))
            # is this a conditional breakpoint?
            fline = linenum
            # if ()
            updatenext(fline)
        } else if (found[1] == 'exiting') {
            linenum = gb.nextlinenum
            winrow = linenum - gb.firstdisplayedlineno + 1
            rplcsrcline(linenum,gb.Nplace,' ')
            paintcolorline(winrow-1,gb.srclines[linenum],rcurses.color_pair(0))
            gb.papcmd <<- ''
            blankline(gb.winlen + 2)
            rcurses.refresh(gb.scrn)
        }
    }
}

dostep <- function(cmd) {
    if (cmd == 's') {
        # assumes an isolated function call, e.g. not a call within a
        # call, so function name is the first non-whitespace char in the
        # line, and ')' immediately follows the function name
        currline <- gb.srclines[gb.nextlinenum]
        currline <- str_sub(currline, (gb.Dplace+1))  # remove line number etc.
        # ftnpart <- trimws(currline, which="left")  # remove leading whitespace
        ftnpart <- str_trim(currline, "left")  # remove leading whitespace
        parenplace <- str_locate(ftnpart, '(')[1]
        ftnname <- str_sub(ftnpart, 1, parenplace-1)
        cmd = str_c("debugonce(", ftnname, "); c")
    }
    sendtoscreen(cmd)
    Sys.sleep(0.25)
    checkdbgsink()
    if (gb.papcmd != "") {
        doprint(gb.papcmd)
    }
}

# run the debuggee call
dorun <- function(cmd) {
    # if function to call was specified, run it; otherwise, run the last one
    if (cmd != "rn") {
        gb.debuggeecall <<- str_split(cmd, " ", simplify=TRUE)[2]
    }
    sendtoscreen(gb.debuggeecall)
    Sys.sleep(0.5)
    checkdbgsink()
}

# utility: removes the first k nonwhitespace tokens, e.g.
# e.g. with k = 2, inputting 'a + b + c' returns 'b + c'
removefirsttokens <- function(k, s) {
    # Start the substring after the kth whitespace character.
    startspliceindex = str_locate_all(s, " ")[[1]][k,1] + 1
    return(str_sub(s, startspliceindex))
}

doprint <- function(cmd) {
    pcmd = str_split(cmd, " ", simplify=TRUE)[1]
    expressiontoprint = removefirsttokens(1,cmd)
    if (pcmd == 'pc') {
        sendtoscreen(expressiontoprint)
        return()
    }

    # Print the line in screen, then retrieve the line that was printed
    # from the sink.
    tosend = str_c("cat(", expressiontoprint, ",fill=TRUE)")
    sendtoscreen(tosend)
    Sys.sleep(0.25)  # give time for screen output to be written to dbgsink
    ds = file("dbgsink", "r")
    printedline = tail(readLines(ds, n=-1), 1)
    toprint = str_c(expressiontoprint, " = ", printedline)
    paintcolorline(gb.msgline,toprint,rcurses.color_pair(0))
    close(ds)
}

dopap <- function(cmd) {

}

# given (1-based) line number in current source file, returns the name
# of the function that begins on that line. if no function there,
# returns NA.
findftnnamebylinenum <- function(linenum) {
    srcline <- gb.srclines[linenum]
    srcline <- str_split(srcline, " ", simplify=TRUE)
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
    for (i in 1:length(gb.srclines)) {
        possiblefname = findftnnamebylinenum(i)
        if (!is.na(possiblefname)) {  # if there was a function declared on this line
            if (possiblefname == fname) {
                return(i)
            }
        }
    }
    return(NA)  # function not found
}

findenclosingftn <- function(linenum) {

}

# Returns TRUE if given str starts with number; otherwise, FALSE.
stringstartswithnumber <- function(str) {
    return(grepl("^[0-9].*", str))
}

# call R debug() or undebug() on the given function; specified either by
# line number or function name; for now, assumes blanks surround '<-' in
# the assignment line in which the function is defined
dodf <- function(cmd) {
    cmdparts <- str_split(cmd, " ", simplify=TRUE)
    fspec <- cmdparts[2]

    # Determine both function line number and name.
    if (stringstartswithnumber(fspec)) {  # if function specified by line number
        fline = as.integer(fspec)
        fname = findftnnamebylinenum(fline)
    } else {  # if function specified by name
        fname = fspec
        fline = findftnlinenumbyname(fname)
    }

    # Update the function's debug flag.
    if (cmdparts[1] == "df") {
        tosend = str_c("debug(", fname, ")")
    } else {
        tosend = str_c("undebug(", fname, ")")
    }
    sendtoscreen(tosend)

    # mark the src line D for "debug", blank out the D if undebug
    if (cmdparts[1] == "df") {
        rplcsrcline(fline,gb.Dplace,'D')
    } else {
        rplcsrcline(fline,gb.Dplace,' ')
    }

    # if it's currently on the screen, update there
    firstdisp = gb.firstdisplayedlineno
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
    for (i in 1:length(gb.srclines)) {
        if (str_sub(gb.srclines[i], gb.Dplace, gb.Dplace) == "D") {
            dodf(str_c("udf ", i))
        }
    }
}

dobp <- function(cmd) {

}

doubp <- function(cmd) {

}

doreloadsrc <- function(cmd) {
    doudfa()
    loadsrc = str_c("source(\'", gb.currsrcfilename, "\')")
    sendtoscreen(loadsrc)
    inputsrc(gb.currsrcfilename)
}

dodown <- function() {
    newstartline = min(gb.firstdisplayedlineno+gb.scroll,gb.srclen)
    dispsrc(newstartline)
}

doup <- function() {
    newstartline = max(gb.firstdisplayedlineno-gb.scroll,1)
    dispsrc(newstartline)
}

doquitbrowser <- function() {
    sendtoscreen('Q')
    oldnextlinenum = gb.nextlinenum
    if (!is.na(oldnextlinenum)) {
        rplcsrcline(oldnextlinenum,gb.Nplace,' ')
        if (inwin(oldnextlinenum)) {
            winrow = oldnextlinenum - gb.firstdisplayedlineno
            updatecolor(winrow,oldnextlinenum)
        }
    }
    gb.papcmd <<- ''
    blankline(gb.winlen + 2)
}

dohelp <- function() {

}

# initialize rcurses environment
initcursesthings <- function() {
    # w(match.call()[[1]])

    # initializes the screen for rcurses
    gb.scrn <<- rcurses.initscr()

    # disables line buffering and erase/kill character-processing
    rcurses.cbreak()

    # screen will be cleared on next call to refresh
    rcurses.clear(gb.scrn)

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
    rcurses.bkgd(gb.scrn,' ',rcurses.color_pair(8))

    # other inits leave 3 lines for console, including border with src panel
    gb.winlen <<- rcurses.LINES - 3

    
    gb.winwid <<- rcurses.COLS

    
    gb.msgline <<- gb.winlen + 2

    
    rcurses.refresh(gb.scrn)

    # manadatory return statement
    return(NULL)
}

cleancursesthings <- function() {
    rcurses.nocbreak()
    rcurses.endwin()
}

errormsg <- function(err) {
    blankline(gb.msgline)
    rcurses.addstr(gb.scrn,err,gb.msgline,0)
    rcurses.refresh(gb.scrn)
}

debugR <- function(filename) {
    tmp <- system('screen -ls | grep rdebug')
    if (tmp == 0) {
        cat('rdebug screen running\n')
        cat('kill screen process, then run "screen -wipe"\n')
        return(NULL)
    }

    # start "screen, with name 'rdebug' for now
    system('xterm -e "screen -S \'rdebug\'" &')
    # start R within screen
    Sys.sleep(1)
    sendtoscreen('R --no-save -q')
    initcursesthings()

    # save the file name in a global variable
    gb.currsrcfilename <<- filename

    # initialize global variables related to source code
    initsrcthings()

    # have R read in the source file to be debugged
    loadsrc = paste("source(", "\'", gb.currsrcfilename, "\'", ")", sep="")
    sendtoscreen(loadsrc)

    initrdebug()

    # one iteration of this loop handles one user command, e.g. one
    # "continue" or one "next"
    while (TRUE) {

        # set console
        tmp <- (gb.winwid - 1 - nchar(' h for help ')) / 2

        # text for the help bar
        helpbar <- str_c(str_dup(' ',tmp),' h for help ',str_dup(' ',tmp))

        # put the help bar on the screen
        rcurses.addstr(gb.scrn,helpbar,gb.winlen,0,rcurses.A_REVERSE)

        
        rcurses.addstr(gb.scrn,str_dup(' ',gb.winwid - 1),gb.winlen + 1,0)

        
        rcurses.move(gb.scrn,gb.winlen + 1,0)

        cmd <- rcurses.getstr(gb.scrn)

        # if user simply hits Enter, then re-do previous command
        if (cmd == '' && gb.prevcmd != "") {
            cmd <- gb.prevcmd
        }

        # check for Next or Continue
        if (substr(cmd,1,1) == 'n' || substr(cmd,1,1) == 's' || substr(cmd,1,1) == 'c') {
            dostep(cmd)
        }

        # check for Debug Ftn command
        else if (substr(cmd,1,2) == 'df') {
            dodf(cmd)
        }

        # check for UndebugAll Ftn command
        else if (substr(cmd,1,4) == 'udfa') {
            doudfa()
        }

        # check for Undebug Ftn command
        else if (substr(cmd,1,3) == 'udf') {
            dodf(cmd)
        }

        # check for Set Breakpoint command
        else if (substr(cmd,1,2) == 'bp') {
            dobp(cmd)
        }

        # check for Unset Breakpoint command
        else if (substr(cmd,1,3) == 'ubp') {
            doubp(cmd)
        }

        # check for Run command
        else if (substr(cmd,1,2) == 'rn') {
            dorun(cmd)
        }

        # check for Print at Pause command
        else if (substr(cmd,1,3) == 'pap') {
            dopap(cmd)
        }

        # check for Undo Print at Pause command
        else if (substr(cmd,1,4) == 'upap') {
            gb.papcmd <<- ''
        }

        # check for Print command
        else if (substr(cmd,1,1) == 'p') {
            doprint(cmd)
        }

        # check for Print to Console command
        else if (substr(cmd,1,2) == 'pc') {
            doprint(cmd)
        }

        # check for Print to Console at Pause command
        else if (substr(cmd,1,4) == 'pcap') {
            dopap(cmd)
        }

        # check for Undo Print to Console at Pause command
        else if (substr(cmd,1,5) == 'upcap') {
            gb.papcmd <<- ''
        }

        # check for Source Reload command
        else if (substr(cmd,1,2) == 'rl') {  # tell R to reload current source file
            doreloadsrc()
        }

        # check for scrolling
        else if (substr(cmd,1,4) == 'down') {
            dodown()
        }

        
        else if (substr(cmd,1,2) == 'up') {
            doup()
        }

        # (re)load source file
        else if (substr(cmd,1,2) == 'ls') {
            cmdsplit = str_split(cmd, ' ', simplify=TRUE)
            if (length(cmdsplit) > 1) {  # if file name given
                gb.currsrcfilename <<- cmdsplit[2]
            }
            initsrcthings()
            loadsrc = str_c("source(\'",gb.currsrcfilename,"\')")
            sendtoscreen(loadsrc)
        }

        # quit R browser
        else if (substr(cmd,1,1) == 'Q') {
            doquitbrowser()
        }

        # check for End Session command (stops R, screen and exits Python)
        else if (substr(cmd,1,2) == 'es') {
            sendtoscreen('quit()')
            sendtoscreen('killall screen')
            sendtoscreen('screen -wipe')
            sendtoscreen('exit')
            cleancursesthings()
            close(gb.ds)
            break
        }

        # display help information
        else if (substr(cmd,1,1) == 'h') {
            dohelp()
        }

        # command not recognized
        else {
            errormsg('no such command')
        }

        # save previous command
        gb.prevcmd <<- cmd
    }
}
