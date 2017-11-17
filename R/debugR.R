# this is the R version of debugR.py

# all globals packaged here
gb.scrn <- NA  # will point to window object
gb.row <- NA  # current row position of cursor within window
gb.src <- NA  # handle for the current source file
gb.srclen <- NA  # length in lines of the current source file
gb.winlen <- NA  # length in lines of the window
gb.winwid <- NA  # width in characters of the window
gb.srclines <- NA  # contents of source file, list of strings, 1 per src line
gb.maxdigits <- NA  # number of digits in the longest line number
gb.firstdisplayedlineno <- NA  # source line number now displayed at top of window; starts at 0
gb.currsrcfilename <- NA  # name of source file currently in window
gb.nextlinenum <- NA  # source line number to be executed next; starts at 0
gb.ftns <- NA  # dictionary of function line numberss, indexed by function name
gb.debuggeecall <- NA  # previous call to run debuggee, e.g. 'mybuggyfun(3)'
gb.scroll <- 20  # amount to scroll in response to 'up' and 'down' cmds
gb.papcmd <- NA  # expression to be printed at each pause (after n/s/c cmd)
gb.msgline <- NA  # line in window where messages are printed
gb.ds <- NA  # file handle for dbgsink file
gb.bpconds <- NA  # dictionary of breakpoints
gb.prevcmd <- NA  # last user command
gb.helpfile <- FALSE

# comment
debugR <- function(filename) {

    # load 'library'
    source('rcurses.R')

    # initialize rcurses environment
    initcursesthings()

    # save the file name in a global variable
    gb.currsrcfilename <<- filename

    # initialize global variables related to source code
    initsrcthings()

    addstr(25,0,'a')
    quit()

    # one iteration of this loop handles one user command, e.g. one
    # "continue" or one "next"
    while(TRUE) {

        # set console
        tmp <- (gb.winwid - 1 - nchar(' h for help ')) / 2

        # text for the help bar
        helpbar <- paste0(rep(' ',tmp),' h for help ',rep(' ',tmp))

        write(helpbar)

        # put the help bar on the screen
        addstr(gb.winlen,0,helpbar,A_REVERSE())

        # comment
        addstr(gb.winlen + 1,0,rep(' ',gb.winwid - 1))

        # comment
        move(gb.winlen + 1,0)

        #comment
        cmd <- getstr()

        # if user simply hits Enter, then re-do previous command
        if (cmd == '' && is.na(gb.prevcmd) == FALSE) {
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
        else if (cmd[1:3] == 'udf') {
            dodf(cmd)
        }

        # check for Set Breakpoint command
        else if (cmd[1:2] == 'bp') {
            dobp(cmd)
        }

        # check for Unset Breakpoint command
        else if (cmd[1:3] == 'ubp') {
            doubp(cmd)
        }

        # check for Run command
        else if (cmd[1:2] == 'rn') {
            dorun(cmd)
        }

        # check for Print at Pause command
        else if (cmd[1:3] == 'pap') {
            dopap(cmd)
        }

        # check for Undo Print at Pause command
        else if (cmd[1:4] == 'upap') {
            gb.papcmd <<- ''
        }

        # check for Print command
        else if (cmd[1] == 'p') {
            doprint(cmd)
        }

        # check for Print to Console command
        else if (cmd[1:2] == 'pc') {
            doprint(cmd)
        }

        # check for Print to Console at Pause command
        else if (cmd[1:4] == 'pcap') {
            dopap(cmd)
        }

        # check for Undo Print to Console at Pause command
        else if (cmd[1:5] == 'upcap') {
            gb.papcmd <<- ''
        }

        # check for Source Reload command
        else if (cmd[1:2] == 'rl') {  # tell R to reload current source file
            doreloadsrc()
        }

        # check for scrolling
        else if (cmd[1:4] == 'down') {
            dodown()
        }

        # comment
        else if (cmd[1:2] == 'up') {
            doup()
        }

        # (re)load source file
        else if (cmd[1:2] == 'ls') {

            # comment
            cmdsplit <- strsplit(cmd,' ')

            # comment
            if (nchar(cmdsplit) > 1) {
                gb.currsrcfilename <<- strsplit(cmd,' ')[1]
            }

            # comment
            initsrcthings()

            # comment
            source(gb.currsrcfilename)
        }

        # quit R browser
        else if (cmd[1] == 'Q') {
            doquitbrowser()
        }

        # check for End Session command (stops R, screen and exits Python)
        else if (cmd[1:2] == 'es') {
            quit()
        }

        # display help information
        else if (cmd[1] == 'h') {
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
    return(NA)
}

# comment
sendtoscreen <- function(cmd) {
    print('sendtoscreen')

    # illegal parameter(s)
    if (is.character(cmd) == FALSE) { p(cmd); quit() }

    system(cmd)

    # manadatory return statement
    return(NA)
}

# initialize rcurses environment
initcursesthings <- function() {
    print('initcursesthings')

    # initializes the screen for rcurses
    initscr()

    # disables line buffering and erase/kill character-processing
    cbreak()

    # screen will be cleared on next call to refresh
    clear()

    # allows support of color attributes on terminals
    start_color()

    # initialize color pair for source code line that has a breakpoint
    init_pair(1,COLOR_BLACK(),COLOR_RED())

    # initialize color pair for source code line that's the current line
    init_pair(2,COLOR_BLACK(),COLOR_GREEN())

    # initialize color pair for source code line that's current and breakpoint
    init_pair(3,COLOR_BLACK(),COLOR_YELLOW())

    # initialize color pair for remaining source code
    init_pair(8,COLOR_BLACK(),COLOR_WHITE())

    # set background color pair
    bkgd(utf8ToInt(' '),color_pair(8))

    # other inits leave 3 lines for console, including border with src panel
    gb.winlen <<- LINES() - 3

    # comment
    gb.winwid <<- COLS()

    # comment
    gb.msgline <<- gb.winlen + 2

    # comment
    refresh()

    # manadatory return statement
    return(NA)
}

# initialize various globals dealing with the source file
initsrcthings <- function() {
    print('initsrcthings')

    # comment
    gb.nextlinenum <<- 1

    # comment
    inputsrc(gb.currsrcfilename)

    # comment
    rplcsrcline(1,gb.Nplace,'N')

    # comment
    dispsrc(gb.nextlinenum)

    # manadatory return statement
    return(NA)
}

# this function reads in the source file from disk, and copies it to the
# list gb.srclines, with each source file being prepended by the line
# number 
inputsrc <- function(filename) {
    print('inputsrc')

    # comment
    lns <- readLines(filename)

    # comment
    gb.srclen <<- length(lns)

    # comment
    gb.maxdigits <<- ndigs(length(lns) + 1)

    # location of 'N', if any
    gb.Nplace <<- gb.maxdigits + 1

    # location of 'D', if any
    gb.Dplace <<- gb.maxdigits + 2

    # comment
    lnno <- 1

    # comment
    gb.srclines <<- list()

    # comment
    for (l in lns) {

        # form the line number, with blanks instead of leading 0s
        ndl <- ndigs(lnno)

        # comment
        tmp <- rep(utf8ToInt(' '),gb.maxdigits - ndl)

        # comment
        tmp <- paste0(tmp,toString(lnno),' ')

        # add room for N marker for next executed and D/B for breakpoint
        tmp <- paste0(tmp,'   ')

        # now add the source line itself, truncated to fit the window
        # width, if necessary
        tmp <- paste0(tmp,l)

        # comment
        ntrunclinechars <- min(gb.winwid,nchar(tmp))

        # comment
        gb.srclines <<- append(gb.srclines,substr(tmp,1,ntrunclinechars))

        # comment
        lnno <- lnno + 1
    }

    # comment
    dispsrc(1)

    # manadatory return statement
    return(NA)
}

# finds the number of decimal digits in n
ndigs <- function(n) {
    print('ndigs')
    return(nchar(toString(n)))
}

# this function displays the current source file, starting at the top of
# the screen, and beginning with the row srcstartrow in gb.srclines
dispsrc <- function(srcstartrow) {
    print('dispsrc')

    # comment
    clear()

    # comment
    winrow <- 0

    # comment
    nlinestoshow <- min(gb.srclen - srcstartrow + 1,gb.winlen)

    # comment
    for (i in srcstartrow:(srcstartrow + nlinestoshow - 1)) {

        # comment
        if (substr(gb.srclines[[i]],gb.Nplace,gb.Nplace) == 'N') {

            # comment
            if (substr(gb.srclines[[i]],gb.Dplace,gb.Dplace) == 'D') {
                paintcolorline(winrow,gb.srclines[[i]],curses.color_pair(3))
            }

            # comment
            else {
                paintcolorline(winrow,gb.srclines[[i]],curses.color_pair(2))
            }
        }

        # comment
        else if (substr(gb.srclines[[i]],gb.Dplace,gb.Dplace) == 'D') {
            paintcolorline(winrow,gb.srclines[[i]],curses.color_pair(1))
        }

        # comment
        else {
            addstr(winrow,0,gb.srclines[[i]])
        }

        # comment
        winrow <- winrow + 1
    }

    # comment
    gb.firstdisplayedlineno <<- srcstartrow

    # comment
    refresh()

    # manadatory return statement
    return(NA)
}

# paints a row in the screen, in the designated color
paintcolorline <- function(winrow,whattopaint,colorpair) {
    print('paintcolorline')

    # comment
    whattopaint <- paste0(whattopaint,rep(' ',gb.winwid - 1 - nchar(whattopaint)))

    # comment
    addstr(winrow,0,whattopaint,colorpair)

    # manadatory return statement
    return(NA)
}

# substitutes s starting at linepos in line lineno of gb.srclines; this
# function does NOT paint the screen, and indeed the given line may be
# currently off the screen; mainly used to add an 'N' or 'D' designation
# in a source line
rplcsrcline <- function(lineno,linepos,s) {
    print('rplcsrcline')

    # comment
    tmp <- gb.srclines[[lineno]]

    # comment
    gb.srclines[[lineno]] <- rplc(tmp,linepos,s)

    # manadatory return statement
    return(NA)
}

# utility; in string s at position k, replace by string r, presumed to
# be the same length as s; new string is returned
rplc <- function(s,k,r) {
    print('rplc')

    # grab first k - 1 characters of string
    front <- substr(s,1,k - 1)

    # grab last characters after r added at position k
    back <- substr(s,k + nchar(r),nchar(s))

    # return concatenation
    return(paste0(front,r,back))
}

# prints a variable's name followed by its value
p <- function(x) {
    print(paste0(deparse(substitute(x)),': ',x))
}

debugR('a.R')
