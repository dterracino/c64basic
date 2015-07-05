#r @"P:\C64Basic\C64Basic\bin\Debug\C64Basic.dll";;

open Interpreter;;

let c64basic = """
10 A = 10
20 B = A + 20
30 GOTO 60
40 B = B + 100
60 TextWindow.WriteLine(B)
70 B = B + 2
""";;

let c64basic = """
5 GOTO 20
10 TextWindow.WriteLine("SKIPPED")
20 FOR A = 1 TO 20
30 TextWindow.WriteLine(A)
40 NEXT
""";;