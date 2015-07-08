#r @"P:\C64Basic\src\C64Basic\bin\Debug\C64Basic.dll";;

open Interpreter;;

let c64basic = """
5 GOTO 15
10 PRINT "SKIPPED"
15 IF 1 > 2 THEN 20
20 FOR A = 1 TO 20
25 LET B = A +1
30 PRINT B
40 NEXT
""";;

let c64basic2 = """
5 GOTO 20
10 PRINT "SKIPPED"
20 FOR A = 1 TO 20
30 PRINT A
40 NEXT
""";;

let c64basic3 = """
10 A = 10
20 B = A + 20
30 GOTO 60
40 B = B + 100
60 TextWindow.WriteLine(B)
70 B = B + 2
""";;
