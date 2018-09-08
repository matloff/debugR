
# dbgR:  a Debugging Tool for R
   
<img src = http://heather.cs.ucdavis.edu/debugRcartoon.png>

This tool, **dbgR**, 
greatly enhances the R debugging process.  R's own
built-in debugging tools are very limited, but **dbgR**
extends them to a rich set of commands, plus visual display of your
execution of the debuggee.  Those who have used GDB for C/C++ will find
many similarities, but no such background is assumed.  (Note:  RStudio,
ESS and statet all have nice debugging tools.)

Note that **an additional of goal of this package is to teach good
debugging habits.**  This feature will be developed over time.

The tool currently needs a Linux or other Unix-family environment, e.g. Macs. 
It should work on Cygwin.  (We may have a Windows version in the future.)
Python is also required.

## Quick Start

<UL>

<li> 

debugR(system.file('examples/test.R',package='dbgR'))

Use your text editor to create a file <strong>test.R</strong> or
copy it from the **examples/** directory in your installed **dbgR**
package.  If you create it yourself, it should have contents

```R
f <- function() {
   sum <- 0
   for (i in 1:3) {
      sum <- sum + i
   }
   sum
}
```
</li> </p> 

<li> In an R console, type

```R
debugR('test.R')
```

This will create a new window, with a new instance of R running in it.
Let's call this new one Window 1, and refer to the original one (from
which you called **debugR**) as Window 0.  During the debugging process,
you will primarily be working with Window 0. 
</li> </p> 

<li> In the command area (space at the bottom of Window 0 type)

```
df f
```

to set the function <strong>f()</strong> to debug status.  
</li> </p> 

<li> Then in the command area, type

```
rn f()
```

which instructs the tool to run ("rn") the expression **f()**.
(No arguments in this particular call, but of course you could have 
some for other functions.  Any R command can be run here.)  
</li> </p> 

<li> You can then type **n** for next line, **c** for continue, etc. 
Type **Q** to quit the browser in the R window, and **es** to 
leave the debugging tool.
</li> </p> 

There are many other commands, e.g. conditional breakpoints, automatic
printing of variables/expressions at each pause, etc.  Type **h** to see
a list of all commands, which are displayed in Window 1, in whatever
text editor your R configuration uses.

## How It Works

Consider what happens if we use R's built-in debugging tool on the
above code:

```R
> debug(f)
> f()
debugging in: f()
debug at test.R#2: {
    sum <- 0
    for (i in 1:3) {
        sum <- sum + i
    }
    sum
}
Browse[2]> n
debug at test.R#3: sum <- 0
Browse[2]> 
```

Here we told R to put **f()** in debug status, so when we then executed the
function, we entered R's browser.  Note how at each pause, the browser 
tells us the current line in our buggy source file, lines 2 and 3 in the
example above.  This is used by **dbgR** to update its display of our
source file, as follows.

The R **sink** function (with the arguments used in **dbgR**) copies
all R output to a file.  (For those who know the Unix **tee** command,
this is similar.)  It is then a simple matter for **dbgR** to read the
file, and then update the cursor in the **dbgR** display accordingly.

Now consider the 'n' command in **dbgR**.  What happens when the
user issues that command is that **dbgR** write the string 'n\n' to
the R window.  This is done via the Unix **screen** utility.

## Command List

Enter key:  repeat last command (should use this a lot, e.g. for n)
  
rn expr:  Run the given R expression; if no expression, use the previous Run
  
n,s,c:  go to Next line/Step into function/Continue until next pause
  
df f, udf f:  Debug/Undebug f()
<br>udfa:  Undebug all functions
  
bp linenum:  set Breakpoint at given line
<br>bp linenum expr:  set Breakpoint at given line, conditional on expr
<br>ubp linenum:  cancel Breakpoint at the given line
  
p expr:  Print expression
<br>pap expr:  Print expression at each Pause (only one expression at a time)
<br>upap:  cancel pap
  
pc expr:  Print expression to Console
<br>pcap expr:  Print expression to Console at each Pause 
<br>upcap:  cancel pcap

pls: print local variables (including args) of the current function
<br>penv e: print contents of the environment e

down: scroll down in debugger window
<br>up: scroll up in debugger window
  
Q:  quit R's debugger
<br>es:  exit debugR program
  
ls srcfile:  (re)load source file; if no file given, use the previous one

