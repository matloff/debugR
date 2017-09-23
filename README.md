
<h1>debugR:  a Debugging Tool for R</h1>

<p>
This tool, <strong>debugR</strong>, available <a href="debugR.py">here</a>.
greatly enhances the R debugging process.  R's own built-in 
debugging tools are very limited, but <strong>debugR</strong> extends 
them to a rich set of commands, plus visual display of your execution 
of the debuggee.  Those who have used GDB for C/C++ will find many 
similarities, but no such background is assumed.  (Note:  RStudio now
has a built-in debugging tool, as do ESS and statet.)
</p>

<p>
(I am no longer supporting <strong>edtdbg</strong>.)
</p>

<p>
The tool needs a Linux or other Unix-family environment, e.g. Macs.  
It should work on Cygwin.  (I may have a Windows version soon.)
Python is also required.
<p>

<h2>Quick Start:</h2>

<UL>

<li> Use your text editor to create a file <strong>test.R</strong>, with
contents
</p>

<pre>
f <- function() {
   sum <- 0
   for (i in 1:3) {
      sum <- sum + i
   }
   sum
}
</pre>
</li> </p>

<li> In a terminal window, type
</p>

<pre>
python debugR.py test.R
</pre>
</li> </p>

<li> In the command area (space at the bottom of the window type)
</p>

<pre>
df f
</pre>

<p>
to set the function <strong>f()</strong> to debug status.  Then in the
command area, type
</p>

<pre>
rn f()
</pre>

<p>
to run the function <strong>f()</strong>.  (No arguments in this
particular call, but of course you could have some for other functions.)
</p>
</li> </p> 

<li> 
<p>
You can then type <strong>n</strong> for next line, <strong>c</strong>
for continue, etc., rather like GDB.  Type <strong>Q</strong> to quit
the browser, and <strong>es</strong> to leave the debugging tool.
</p>

<li> 
There are many other commands, e.g. conditional breakpoints, automatic
printing of variables/expressions at each pause, etc.  Type
<strong>h</strong> to see a list of all commands.

</UL>

<img src = http://heather.cs.ucdavis.edu/debugRcartoon.png>

