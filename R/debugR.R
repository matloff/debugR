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
gb.nextlinenum <- NULL  # source line number to be executed next; starts at 0
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
paintcolorline <- function(winrow,whattopaint,colorpair) {
    # w(match.call()[[1]])

    whattopaint <- paste0(whattopaint,strrep(' ',gb.winwid - nchar(whattopaint)))

    rcurses.addstr(gb.scrn,whattopaint,winrow,0,colorpair)

    # manadatory return statement
    return(NULL)
}

# this function displays the current source file, starting at the top of
# the screen, and beginning with the row srcstartrow in gb.srclines
dispsrc <- function(srcstartrow) {
    # w(match.call()[[1]])

    # comment
    rcurses.clear(gb.scrn)

    # comment
    winrow <- 0

    # comment
    nlinestoshow <- min(gb.srclen - srcstartrow + 1,gb.winlen)

    # comment
    for (i in srcstartrow:(srcstartrow + nlinestoshow - 1)) {

        # comment
        if (substr(gb.srclines[i],gb.Nplace,gb.Nplace) == 'N') {

            # comment
            if (substr(gb.srclines[i],gb.Dplace,gb.Dplace) == 'D') {
                paintcolorline(winrow,gb.srclines[i],rcurses.color_pair(3))
            }

            # comment
            else {
                paintcolorline(winrow,gb.srclines[i],rcurses.color_pair(2))
            }
        }

        # comment
        else if (substr(gb.srclines[i],gb.Dplace,gb.Dplace) == 'D') {
            paintcolorline(winrow,gb.srclines[i],rcurses.color_pair(1))
        }

        # comment
        else {
            rcurses.addstr(gb.scrn,gb.srclines[i],winrow,0)
        }

        # comment
        winrow <- winrow + 1
    }

    # comment
    gb.firstdisplayedlineno <<- srcstartrow

    # comment
    rcurses.refresh(gb.scrn)

    # manadatory return statement
    return(NULL)
}

# this function reads in the source file from disk, and copies it to the
# list gb.srclines, with each source file being prepended by the line
# number
inputsrc <- function(filename) {
    # w(match.call()[[1]])

    # comment
    lns <- readLines(filename)

    # comment
    gb.srclen <<- length(lns)

    # comment
    gb.maxdigits <<- ndigs(length(lns) + 1)

    # location of 'N', if any
    gb.Nplace <<- gb.maxdigits + 2

    # location of 'D', if any
    gb.Dplace <<- gb.maxdigits + 3

    # comment
    lnno <- 1

    # comment
    gb.srclines <<- c()

    # comment
    for (lineNum in 1:length(lns)) {

        # form the line number, with blanks instead of leading 0s
        ndl <- ndigs(lineNum)

        # comment
        tmp <- rep(' ',gb.maxdigits - ndl)

        # comment
        tmp <- paste0(tmp,toString(lineNum),' ')

        # add room for N marker for next executed and D/B for breakpoint
        tmp <- paste0(tmp,'   ')

        # now add the source line itself, truncated to fit the window
        # width, if necessary
        tmp <- paste0(tmp,lns[lineNum])

        # comment
        ntrunclinechars <- min(gb.winwid,nchar(tmp))

        gb.srclines <<- c(gb.srclines, substr(tmp,1,ntrunclinechars))
    }

    # comment
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
    # w(match.call()[[1]])

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
    system(tosend)
}

# comment
# sendtoscreen <- function(cmd) {
#     w(match.call()[[1]])

#     # illegal parameter(s)
#     if (is.character(cmd) == FALSE) { p(cmd); quit() }

#     system(cmd)

#     # manadatory return statement
#     return(NULL)
# }

# initialize various globals dealing with the source file
initsrcthings <- function() {
    # w(match.call()[[1]])

    # comment
    gb.nextlinenum <<- 1

    # comment
    inputsrc(gb.currsrcfilename)

    # comment
    rplcsrcline(1,gb.Nplace,'N')

    # comment
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
    sendtoscreen("sink(\'dbgsink\',split=T)")
    for (i in 1:20) {
        gb.ds <<- file("dbgsink", "r")
    }
}

# find the latest line in the sink file that starts with either 'debug
# at' (pause line) or 'exiting from' (exit R debugger), returning that
# line 
finddebugline <- function() {
    sinkfilelines <- readLines(gb.ds, n=-1)  # read all lines
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
    paintcolorline(wrow,tmp,colorpair)
    rcurses.refresh(gb.scrn)
}

updatenext <- function(newnextlinenum) {

}

# blank out the given line in the current window
blankline <- function(winrow) {
    rcurses.addstr(gb.scrn, strdup(' ', gb.winwid-1), winrow, 0)
}

# when we hit a pause, or exit the R debugger, this function will
# determine what line we paused at, or that we did exit
checkdbgsink <- function() {

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

removefirsttokens <- function(k, s) {

}

doprint <- function(cmd) {

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
        } else {
            return(NA)
        }
    }
}

findenclosingftn <- function(linenum) {

}

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
        winrow = fline - firstdisp
        if (cmdparts[1] == "df") {
            updatecolor(winrow,fline)
        } else {  # undebug case
            updatecolor(winrow,fline)
        }
    }
}

doudfa <- function() {

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

}

doup <- function() {

}

doquitbrowser <- function() {

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

    # comment
    gb.winwid <<- rcurses.COLS

    # comment
    gb.msgline <<- gb.winlen + 2

    # comment
    rcurses.refresh(gb.scrn)

    # manadatory return statement
    return(NULL)
}

cleancursesthings <- function() {
    rcurses.nocbreak()
    rcurses.endwin()
}

errormsg <- function(err) {

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
    Sys.sleep(3)
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

        # comment
        rcurses.addstr(gb.scrn,str_dup(' ',gb.winwid - 1),gb.winlen + 1,0)

        # comment
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

        # comment
        else if (substr(cmd,1,2) == 'up') {
            doup()
        }

        # (re)load source file
        else if (substr(cmd,1,2) == 'ls') {

            # comment
            cmdsplit <- strsplit(cmd,' ')

            # comment
            if (nchar(cmdsplit) > 1) {
                gb.currsrcfilename <<- strsplit(cmd,' ')[[1]][2]
            }

            # comment
            initsrcthings()

            # comment
            source(gb.currsrcfilename)
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

    # manadatory return statement
    return(NULL)
}
