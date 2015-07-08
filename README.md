```
  _____                         __               ____ ____
 / ___/__  __ _  __ _  ___  ___/ /__  _______   / __// / /
/ /__/ _ \/  ' \/  ' \/ _ \/ _  / _ \/ __/ -_) / _ \/_  _/
\___/\___/_/_/_/_/_/_/\___/\_,_/\___/_/  \__/  \___/ /_/  
```

# c64basic
Hacking on http://fssnip.net/le by Phillip Trelford to turn it into Commodore64 Basic.

This is for personal fun... Anyway anyone is invited to participate, just stay close to **C64 Basic** specification.

## Work in progress
This is a work in progress, based on this [wiki](https://www.c64-wiki.com/index.php/C64-Commands). TODO list is [here](https://github.com/gsscoder/c64basic/blob/master/TODO).

```fsharp
#r @"./src/C64Basic/bin/Debug/C64Basic.dll"

open Interpreter

let c64basic = """
5 GOTO 15
10 PRINT "SKIPPED"
15 IF 1 > 2 THEN 20
20 FOR A = 1 TO 20
25 LET B = A + 1
30 PRINT B
40 NEXT
"""

execute c64basic
```
