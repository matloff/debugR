
# debugR.py, a debugging tool for the R programming language    

# see Known Issues at end of this file

# produces a window into the debuggee's source file(s); cursor will move
# in this window as the user single-steps through the source, etc.; user
# submits comments, e.g. n for next statement, from within this window,
# rarely if ever needing to shift focus to the R window

# usage:  python debugR.py [primary_source_file.R]

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

import curses, os, sys, time

class gb:  # all globals packaged here
   scrn = None  # will point to window object
   row = None  # current row position of cursor within window
   src = None  # handle for the current source file
   srclen = None  # length in lines of the current source file
   winlen = None  # length in lines of the window
   winwid = None  # width in characters of the window
   srclines = None  # contents of source file, list of strings, 1 per src line
   maxdigits = None  # number of digits in the longest line number
   firstdisplayedlineno = None  # source line number now displayed at
                                # top of window; starts at 0
   currsrcfilename = ''  # name of source file currently in window
   nextlinenum = None  # source line number to be executed next; starts at 0
   ftns = []  # dictionary of function line numberss, indexed by function name
   debuggeecall = ''  # previous call to run debuggee, e.g. 'mybuggyfun(3)'
   scroll = 20  # amount to scroll in response to 'up' and 'down' cmds
   papcmd = ''  # expression to be printed at each pause (after n/s/c cmd)
   msgline = None  # line in window where messages are printed
   ds = None  # file handle for dbgsink file
   bpconds = {}  # dictionary of breakpoints
   prevcmd = None  # last user command
   helpfile = False  

def ndigs(n):  # finds the number of decimal digits in n
   return len(str(n))

# paints a row in the screen, in the designated color
def paintcolorline(winrow,whattopaint,colorpair):
   whattopaint = whattopaint + (gb.winwid - 1 - len(whattopaint))*' '
   gb.scrn.addstr(winrow,0,whattopaint,colorpair)

# this function displays the current source file, starting at the top of
# the screen, and beginning with the row srcstartrow in gb.srclines
def dispsrc(srcstartrow):
   gb.scrn.clear()
   winrow = 0
   nlinestoshow = min(gb.srclen-srcstartrow,gb.winlen)
   for i in range(srcstartrow,srcstartrow+nlinestoshow):
      try:
         if gb.srclines[i][gb.Nplace] == 'N':
            if gb.srclines[i][gb.Dplace] == 'D':
               paintcolorline(winrow,gb.srclines[i],curses.color_pair(3))
            else:
               paintcolorline(winrow,gb.srclines[i],curses.color_pair(2))
         elif gb.srclines[i][gb.Dplace] == 'D':
            paintcolorline(winrow,gb.srclines[i],curses.color_pair(1))
         else: gb.scrn.addstr(winrow,0,gb.srclines[i])
      except: pass  # for easier debugging
      winrow += 1
   gb.firstdisplayedlineno = srcstartrow 
   gb.scrn.refresh()

# this function reads in the source file from disk, and copies it to the
# list gb.srclines, with each source file being prepended by the line
# number 
def inputsrc(filename):
   gb.currsrcfilename = filename
   gb.src = open(filename,'r')
   lns = gb.src.readlines()
   gb.srclen = len(lns)
   gb.maxdigits = ndigs(len(lns) + 1)
   gb.Nplace = gb.maxdigits + 1  # location of 'N', if any
   gb.Dplace = gb.maxdigits + 2  # location of 'D', if any
   lnno = 1
   gb.srclines = []
   for l in lns:
      # form the line number, with blanks instead of leading 0s 
      ndl = ndigs(lnno)
      tmp = (gb.maxdigits - ndl) * ' '
      tmp += str(lnno) + ' '
      # add room for N marker for next executed and D/B for breakpoint
      tmp = tmp + '   '
      # now add the source line itself, truncated to fit the window
      # width, if necessary
      tmp += chop(l)
      ntrunclinechars = min(gb.winwid,len(tmp))
      gb.srclines.append(tmp[:ntrunclinechars])
      lnno += 1
   dispsrc(0)

# utility; in string s at position k, replace by string r, presumed to
# be the same length as s; new string is returned
def rplc(s,k,r):
   front = s[:k]
   back = s[k+len(r):]
   return front + r + back

# substitutes s starting at linepos in line lineno of gb.srclines; this
# function does NOT paint the screen, and indeed the given line may be
# currently off the screen; mainly used to add an 'N' or 'D' designation
# in a source line
def rplcsrcline(lineno,linepos,s):
   tmp = gb.srclines[lineno]
   gb.srclines[lineno] = rplc(tmp,linepos,s)

# deletes newline character at the end of s, returning result
def chop(s):  
   return s[:(len(s)-1)]

# sends the command cmd to the "screen session", thus typically to R
def sendtoscreen(cmd):
   cmd += '\n'
   tosend = 'screen -S "rdebug" -X stuff "' + cmd + '" '
   # R S3 classes use $ for member quantities; must escape it for
   # Unix-family systems; need to expand, checking for non-Unix, for
   # multiple $, etc.; note that Python also requires escaping the
   # backslash!
   try:
      dollar = tosend.index('$')
      tosend = tosend[0:dollar] + '\\' + tosend[dollar:]
   except:
      pass
   os.system(tosend)

# initialize various globals dealing with the source file
def initsrcthings():
   # gb.currsrcfilename = sys.argv[1]
   gb.nextlinenum = 0
   inputsrc(gb.currsrcfilename)
   rplcsrcline(0,gb.Nplace,'N')
   dispsrc(gb.nextlinenum)

# initializes debugging operations; tells R to call sink(), setting up a
# duplication of screen output to file output; then tells R to input our
# buggy file
def initrdebug():
   # planned change:  have sink() write to R network connection, to
   # a server that is run here; the throttling then probably won't be
   # necessary, and conditional breakpoint will be faster
   sendtoscreen("sink(\'dbgsink\',split=T)")
   for i in range(20): 
      try:
         gb.ds = open('dbgsink')
         break
      except:
        time.sleep(0.10)
   # have R read in the source file to be debugged

# find the latest line in the sink file that starts with either 'debug
# at' (pause line) or 'exiting from' (exit R debugger), returning that
# line 
def finddebugline():
   sinkfilelines = gb.ds.readlines()
   rng = range(len(sinkfilelines))
   rng.reverse()
   for i in rng:
      if sinkfilelines[i].find('exiting from') >= 0: 
         return ['exiting',sinkfilelines[i]]
      if sinkfilelines[i].find(gb.currsrcfilename+'#') >= 0: 
         # ds.close()
         return ['debug',sinkfilelines[i]]
   return None

# determines if linenum of the current src is in the current window
def inwin(linenum):
   firstdisp = gb.firstdisplayedlineno
   return (linenum >= firstdisp and linenum < firstdisp + gb.winlen)

# change the highlighting color of a line that's in the current window,
# to reflect that it's the current line or a pause line
def updatecolor(wrow,linenum):
   tmp = gb.srclines[linenum]
   if tmp[gb.Nplace] == 'N':
      if tmp[gb.Dplace] == 'D': colorpair = curses.color_pair(3)
      else: colorpair = curses.color_pair(2)
   elif tmp[gb.Dplace] == 'D': colorpair = curses.color_pair(1)
   else: colorpair = curses.color_pair(0)
   paintcolorline(wrow,tmp,colorpair)
   gb.scrn.refresh()

# update the indicators, e.g. N mark, of where the next line to be
# executed is; newnextlinenum is 0-based
def updatenext(newnextlinenum):
   oldnextlinenum = gb.nextlinenum
   if oldnextlinenum != None:  
      rplcsrcline(oldnextlinenum,gb.Nplace,' ')
      if inwin(oldnextlinenum):
         winrow = oldnextlinenum - gb.firstdisplayedlineno
         updatecolor(winrow,oldnextlinenum)
   gb.nextlinenum = newnextlinenum
   rplcsrcline(newnextlinenum,gb.Nplace,'N')
   if inwin(newnextlinenum):
      winrow = newnextlinenum - gb.firstdisplayedlineno
      updatecolor(winrow,newnextlinenum)
   else:
      dispsrc(newnextlinenum)

# blank out the given line in the current window
def blankline(winrow):
   gb.scrn.addstr(winrow,0,(gb.winwid-1) * ' ')

# when we hit a pause, or exit the R debugger, this function will
# determine what line we paused at, or that we did exit
def checkdbgsink():
   # now must find current src file, line num; works on the basis of the
   # lines in the sink file being of the form, e.g.
   #    debug at test.R#3: for (i in 1:3) {
   found = finddebugline()
   if found:  # need the if, as dbgsink may still be empty at this point
      sinkline = found[1]
      colonplace = sinkline.find(':')
      if found[0] == 'debug':
         linenumstart = sinkline.find('#') + 1
         srcfile = sinkline[9:linenumstart-1]
         linenum = int(sinkline[linenumstart:colonplace])
         # is this a conditional breakpoint?
         fline = linenum - 1
         if fline in gb.bpconds.keys():
            doprint('p '+gb.bpconds[fline])
            lastline = gb.ds.readlines()[-1]
            if lastline.find('FALSE') >= 0:  # bp condition doesn't hold
               if gb.prevcmd != 'n': 
                  dostep('c')
                  return
         updatenext(fline)
      elif found[0] == 'exiting':
         linenum = gb.nextlinenum
         winrow = linenum - gb.firstdisplayedlineno
         rplcsrcline(linenum,gb.Nplace,' ')
         # gb.scrn.addstr(winrow,0,gb.srclines[linenum],curses.color_pair(0))
         paintcolorline(winrow,gb.srclines[linenum],curses.color_pair(0))
         gb.papcmd = ''
         blankline(gb.winlen + 2)
         gb.scrn.refresh()

# do a debug step, either Next or Continue
def dostep(cmd):
   if cmd == 's':
      # assumes an isolated function call, e.g. not a call within a
      # call, so function name is the first non-whitespace char in the
      # line, and ')' immediately follows the function name
      currline = gb.srclines[gb.nextlinenum]
      currline = currline[(gb.Dplace+1):]  # remove line number etc.
      ftnpart = currline.lstrip()  # remove leading whitespace
      parenplace = ftnpart.index('(')
      ftnname = ftnpart[:parenplace]
      cmd = 'debugonce(' + ftnname + '); c'
   sendtoscreen(cmd)
   time.sleep(0.25)
   checkdbgsink()
   if gb.papcmd: doprint(gb.papcmd)

# run the debuggee call
def dorun(cmd):
   # if a debuggee call specified, run it; otherwise, run the last one
   if cmd != 'rn':
      gb.debuggeecall = cmd.split(' ')[1]
   sendtoscreen(gb.debuggeecall)
   time.sleep(0.5)
   checkdbgsink()

# utility: removes the first k nonwhitespace tokens, e.g.
# e.g. with k = 1, inputting 'a + b + c' returns 'b + c'
def removefirsttokens(k,s):
   sparts = s.split(' ')
   return reduce(lambda t1,t2:t1+t2,sparts[k:])

# print R expression 
def doprint(cmd):
   pcmd = cmd.split(' ')[0]
   expressiontoprint = removefirsttokens(1,cmd)
   if pcmd == 'pc': 
      sendtoscreen(expressiontoprint)
      return
   tosend = 'cat(' + expressiontoprint + ',fill=TRUE)'
   sendtoscreen(tosend)
   ds = open('dbgsink')
   time.sleep(0.25)
   tail = ds.readlines()[-1]
   tail = tail[:(len(tail)-1)]
   tail = expressiontoprint + ' = ' + tail
   # gb.scrn.addstr(gb.msgline,0,tail)
   paintcolorline(gb.msgline,tail,curses.color_pair(0))

# print R expression after each n or d cmd
def dopap(cmd):
   pcmd = cmd.split(' ')[0]
   expressiontoprint = removefirsttokens(1,cmd)
   if pcmd == 'pcap':
      gb.papcmd =  'pc ' + expressiontoprint
   else:
      gb.papcmd =  'p ' + expressiontoprint
   doprint(gb.papcmd)

# given (0-based) line number in current source file, returns the name
# of the function that begins on that line
def findftnnamebylinenum(linenum):
   srcline = gb.srclines[linenum]
   srcline = srcline.split(' ')
   fnamepos = srcline.index('<-') - 1
   return srcline[fnamepos]

# given name of a function in the current source file, returns the 
# (0-based) number of the line at which it begins
def findftnlinenumbyname(fname):
   for i in range(len(gb.srclines)):
      srcline = gb.srclines[i]
      srcline = srcline.split(' ')
      try: 
         idx = srcline.index('<-') 
         fnamepos = idx - 1
         if srcline[fnamepos] == fname: 
            return i
      except: pass

# given (1-based) line number in current source file, returns the name
# of the function that includes this line; assumes no "functions defined
# within functions"
def findenclosingftn(linenum):
   i = linenum - 1
   while True:
      srcline = gb.srclines[i]
      try:
         tmp = srcline.index('<- function') 
         srcline = srcline.split(' ')
         assignplace = srcline.index('<-')
         return srcline[assignplace-1]
      except:
         i = i - 1

# call R debug() or undebug() on the given function; specified either by
# line number or function name; for now, assumes blanks surround '<-' in
# the assignment line in which the function is defined
def dodf(cmd):
   cmdparts = cmd.split(' ')
   fspec = cmdparts[1]
   try:  # case in which function specified by line number
      fline = int(fspec) - 1
      fname = findftnnamebylinenum(fline)
   except:   # case in which function specified by name
      fname = fspec
      fline = findftnlinenumbyname(fname)
   if cmdparts[0] == 'df': tosend = 'debug(' + fname + ')'
   else: tosend = 'undebug(' + fname + ')'
   sendtoscreen(tosend)
   # mark the src line D for "debug", blank out the D if undebug
   if cmdparts[0] == 'df': rplcsrcline(fline,gb.Dplace,'D')
   else: rplcsrcline(fline,gb.Dplace,' ')
   # if it's currently on the screen, update there
   firstdisp = gb.firstdisplayedlineno
   if inwin(fline):
      winrow = fline - firstdisp
      if cmdparts[0] == 'df': 
         updatecolor(winrow,fline)
      else:  # undebug case
         updatecolor(winrow,fline)

# call undebug() on all functions currently in debug state
def doudfa():
   for i in range(len(gb.srclines)):
      if gb.srclines[i][gb.Dplace] == 'D':
         dodf('udf ' + str(i+1))

# setBreakpoint() will be called on the requested source line, specified by
# (1-based) line number in the current source file
def dobp(cmd):
   cmdparts = cmd.split(' ')
   linenum = cmdparts[1]
   filename = gb.currsrcfilename 
   tosend = 'setBreakpoint(\047' + filename + '\047,' + linenum + ')'
   sendtoscreen(tosend)
   # mark the src line D for "debug"
   fline = int(linenum) - 1
   rplcsrcline(fline,gb.Dplace,'D')
   # if it's currently on the screen, update there
   if inwin(fline):
      firstdisp = gb.firstdisplayedlineno
      winrow = fline - firstdisp
      updatecolor(winrow,fline)
   # add to our list of breakpoints
   if len(cmdparts) > 2:
      gb.bpconds[fline] = removefirsttokens(2,cmd)

# untrace() will be called on the function that contains the requested
# source line, specified by (1-based) line number in the current source
# file
def doubp(cmd):
   cmdparts = cmd.split(' ')
   linenum = cmdparts[1]
   ftnname = findenclosingftn(int(linenum))
   tosend = 'untrace(' + ftnname + ')'
   # unfortunately, untrace() does an auto undebug(), so need to update
   dodf('udf '+ftnname)
   sendtoscreen(tosend)
   fline = int(linenum) - 1
   rplcsrcline(fline,gb.Dplace,' ')
   # if it's currently on the screen, update there
   if inwin(fline):
      firstdisp = gb.firstdisplayedlineno
      winrow = fline - firstdisp
      updatecolor(winrow,fline)
   if fline in gb.bpconds.keys():
      gb.bpconds.pop(fline)

def doreloadsrc():
   doudfa()
   loadsrc = "source(" + "\'" + gb.currsrcfilename + "\'" + ")"
   sendtoscreen(loadsrc)
   inputsrc(gb.currsrcfilename)

def dodown():  
   newstartline = min(gb.firstdisplayedlineno+gb.scroll,gb.srclen-1)
   dispsrc(newstartline)

def doup():  
   newstartline = max(gb.firstdisplayedlineno-gb.scroll,0)
   dispsrc(newstartline)

def doquitbrowser():
   sendtoscreen('Q')
   oldnextlinenum = gb.nextlinenum
   if oldnextlinenum != None:  
      rplcsrcline(oldnextlinenum,gb.Nplace,' ')
      if inwin(oldnextlinenum):
         winrow = oldnextlinenum - gb.firstdisplayedlineno
         updatecolor(winrow,oldnextlinenum)
   gb.papcmd = ''
   blankline(gb.winlen + 2)

def dohelp():
   if not gb.helpfile:
      # open this Python source file, find the help section, make a tmp
      # file from it, and have R invoke the user's favorite text editor
      # on it
      hf = open(sys.argv[0],'r')  
      hflines = hf.readlines()
      for i in range(len(hflines)):
         if hflines[i] == '# HELP SECTION\n': break
      # delete the non-help section
      hflines = hflines[i:]
      # delete the comment signs
      hflines = map(lambda s: s[2:],hflines)
      hfout = open('/tmp/debugRhelp','w')
      hfout.writelines(hflines)
      hfout.close()
      gb.helpfile = True
   tosend = "edit(file=\'/tmp/debugRhelp\')"
   sendtoscreen(tosend)
   # os.system('xterm -e "vi /tmp/debugRhelp" &')

def initcursesthings():
   gb.scrn = curses.initscr()
   curses.cbreak()
   gb.scrn.clear()
   curses.start_color()
   # red if breakpoint
   curses.init_pair(1, curses.COLOR_BLACK, curses.COLOR_RED)
   # green if current line
   curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_GREEN)
   # yellow if both breakpoint and current line
   curses.init_pair(3, curses.COLOR_BLACK, curses.COLOR_YELLOW)
   # overall background for the window, black on white (or off-white)
   curses.init_pair(8, curses.COLOR_BLACK, curses.COLOR_WHITE)
   gb.scrn.bkgd(' ',curses.color_pair(8))
   # other inits 
   # leave 3 lines for console, including border with src panel
   gb.winlen = curses.LINES - 3  
   gb.winwid = curses.COLS
   gb.msgline = gb.winlen + 2
   gb.scrn.refresh()

def cleancursesthings():
   curses.nocbreak()
   curses.endwin()

def errormsg(err):
   blankline(gb.msgline)
   gb.scrn.addstr(gb.msgline,0,err)
   gb.scrn.refresh()

def main():
   # check usage
   # if len(sys.argv) != 2:
   #    print 'usage: python debugR.py debuggee.R'
   #    sys.exit()
   # check for existing 'screen' sessions with name 'rdebug'
   tmp = os.system('screen -ls | grep rdebug')
   if (tmp == 0):
      print 'rdebug screen running'
      print 'kill screen process, then run "screen -wipe"'
      sys.exit()
   # start "screen, with name 'rdebug' for now
   os.system('xterm -e "screen -S \'rdebug\'" &')
   # start R within screen
   time.sleep(3)
   sendtoscreen('R --no-save -q')
   # switch to curses
   initcursesthings()
   if len(sys.argv) == 2: 
      gb.currsrcfilename = sys.argv[1]
      initsrcthings()
      loadsrc = "source(" + "\'" + gb.currsrcfilename + "\'" + ")"
      sendtoscreen(loadsrc)
   initrdebug()
   while True:  # one iteration of this loop handles one user command,
                # e.g. one "continue" or one "next"
      # set console
      tmp = (gb.winwid - 1 - len(' h for help ')) / 2
      helpbar = tmp * ' ' + ' h for help ' + tmp * ' '
      gb.scrn.addstr(gb.winlen,0,helpbar,curses.A_REVERSE)
      gb.scrn.addstr(gb.winlen+1,0,(gb.winwid-1) * ' ')
      gb.scrn.move(gb.winlen+1,0)
      cmd = gb.scrn.getstr()
      # if user simply hits Enter, then re-do previous command
      if cmd == '' and gb.prevcmd: cmd = gb.prevcmd
      # check for Next or Continue
      if cmd == 'n' or cmd == 's' or cmd == 'c':   
         dostep(cmd)
      # check for Debug Ftn command
      elif cmd[:2] == 'df':
         dodf(cmd)
      # check for UndebugAll Ftn command
      elif cmd[:4] == 'udfa':
         doudfa()
      # check for Undebug Ftn command
      elif cmd[:3] == 'udf':
         dodf(cmd)
      # check for Set Breakpoint command
      elif cmd[:2] == 'bp':
         dobp(cmd)
      # check for Unset Breakpoint command
      elif cmd[:3] == 'ubp':
         doubp(cmd)
      # check for Run command
      elif cmd[:2] == 'rn':
         dorun(cmd)
      # check for Print at Pause command
      elif cmd[:3] == 'pap':
         dopap(cmd)
      # check for Undo Print at Pause command
      elif cmd[:4] == 'upap':
         gb.papcmd = ''
      # check for Print command
      elif cmd[:1] == 'p':
         doprint(cmd)
      # check for Print to Console command
      elif cmd[:2] == 'pc':
         doprint(cmd)
      # check for Print to Console at Pause command
      elif cmd[:4] == 'pcap':
         dopap(cmd)
      # check for Undo Print to Console at Pause command
      elif cmd[:5] == 'upcap':
         gb.papcmd = ''
      # check for Source Reload command
      elif cmd == 'rl':  # tell R to reload current source file
         doreloadsrc()
      # check for scrolling
      elif cmd == 'down':
         dodown()
      elif cmd == 'up':
         doup()
      # (re)load source file
      elif cmd[:2] == 'ls':
         cmdsplit = cmd.split(' ')
         if len(cmdsplit) > 1:
            gb.currsrcfilename = cmd.split(' ')[1]
         initsrcthings()
         loadsrc = "source(" + "\'" + gb.currsrcfilename + "\'" + ")"
         sendtoscreen(loadsrc)
      # check for Browser Quit command
      elif cmd == 'Q':  # quit R browser
         doquitbrowser()
      # check for End Session command (stops R, screen and exits Python)
      elif cmd == 'es': 
         sendtoscreen('quit()')
         sendtoscreen('killall screen')
         sendtoscreen('screen -wipe')
         sendtoscreen('exit')
         cleancursesthings()
         break
      elif cmd == 'h':
         dohelp()
      else: errormsg('no such command')
      gb.prevcmd = cmd

if __name__ =='__main__': main()

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

