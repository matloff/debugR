
# debugR:  a Debugging Tool for R

This tool, **debugR**, 
greatly enhances the R debugging process.  R's own
built-in debugging tools are very limited, but **debugR**
extends them to a rich set of commands, plus visual display of your
execution of the debuggee.  Those who have used GDB for C/C++ will find
many similarities, but no such background is assumed.  (Note:  RStudio,
ESS and statet all have nice debugging tools.)

The tool currently needs a Linux or other Unix-family environment, e.g. Macs. 
It should work on Cygwin.  (We may have a Windows version in the future.)
Python is also required.

## Quick Start

<UL>

1. Use your text editor to create a file <strong>test.R</strong>, with
contents
</p>

```R
f <- function() {
   sum <- 0
   for (i in 1:3) {
      sum <- sum + i
   }
   sum
}
```

2. In an R console, type

```R
debugR('test.R')

3. In the command area (space at the bottom of the window type)

```
df f
```

to set the function <strong>f()</strong> to debug status.  Then in the
command area, type

```
rn f()
```

to run the function **f**.  (No arguments in this
particular call, but of course you could have some for other functions.)

4. You can then type n for next line, **c**
for continue, etc., rather like GDB.  Type **Q** to quit
the browser, and **es** to leave the debugging tool.

<li> 
There are many other commands, e.g. conditional breakpoints, automatic
printing of variables/expressions at each pause, etc.  Type
<strong>h</strong> to see a list of all commands.

</UL>

<img src = http://heather.cs.ucdavis.edu/debugRcartoon.png>

## How It Works
