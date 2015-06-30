# c64basic
Hacking on http://fssnip.net/le by Phillip Trelford to turn it into Commodore64 Basic.

For personal fun...

## Work in progress
_Code is in a messy state full of comments._

```fsharp
open Interpreter

let c64basic = """
10 A = 10
20 B = A + 20
30 GOTO 60
40 B = B + 100
60 TextWindow.WriteLine(B)
70 B = B + 2
"""

execute c64basic
```
