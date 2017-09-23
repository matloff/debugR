
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

<li> Use your text editor to create a file <strong>test.R</strong>, with
contents

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

<li> In the command area (space at the bottom of the window type)

```
df f
```

to set the function <strong>f()</strong> to debug status.  
</li> </p> 

<li> Then in the command area, type

```
rn f()
```

to run the function **f**.  (No arguments in this
particular call, but of course you could have some for other functions.
Any R command can be run here.)
</li> </p> 

<li> You can then type **n** for next line, **c**
for continue, etc., rather like GDB.  Type **Q** to quit
the browser in the R window, and **es** to leave the debugging tool.
</li> </p> 

There are many other commands, e.g. conditional breakpoints, automatic
printing of variables/expressions at each pause, etc.  Type **h** to see
a list of all commands, which are displayed in the R window.

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

Here we told R to put **f** in debug status, so when we then executed the
function, we entered R's browser.  Note how at each pause, the browser 
tells us the current line in our buggy source file, lines 2 and 3 in the
example above.  This is used by **debugR** to update its display of our
source file, as follows.

The R **sink** function (with the arguments used in **debugR**) copies
all R output to a file.  (For those who know the Unix **tee** command,
this is similar.)  It is then a simple matter for **debugR** to read the
file, and then update the cursor in the **debugR** display accordingly.

Now consider the **n** command in **debugR**.  What happens when the
user issues that command is that **debugR** write the string 'n\n' to
the R window.  This is done via the Unix **screen** utility.

<img src = http://heather.cs.ucdavis.edu/debugRcartoon.png>

