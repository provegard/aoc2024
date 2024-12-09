identification division.
program-id. AOC202404P1.

environment division.
input-output section.
file-control.
       select InputFile assign to 'input'
           organization is line sequential.  

data division.
file section.
FD InputFile
       record varying 1 to 250 depending on wsLineLength.
01 InputRecord pic X(250).

working-storage section.
01 wsLineLength pic 9(3).
01 wsEof pic A(1).
01 wsLines.
       05 wsLine pic X(250) occurs 250 times.
01 wsIdx pic 9(3) value 1.
01 wsIdx2 pic 9(3) value 1.
01 wsLineCount pic 9(3).
01 wsRow pic 9(3).
01 wsCol pic 9(3).
01 wsWordLen pic 9(2) value 4.  *> XMAS
01 wsWord pic a(4).
01 wsXmasCount pic 9(5) value 0.

procedure division.
       open input InputFile

       perform until wsEof = "y"
           read InputFile into InputRecord
               at end move "y" to wsEof
           end-read

           if wsEof not = "y"
               move InputRecord(1:wsLineLength) to wsLine(wsIdx)
               add 1 to wsIdx
           end-if
       end-perform

       close InputFile

       move wsIdx to wsLineCount
       subtract 1 from wsLineCount

       perform varying wsRow from 1 by 1 until wsRow > wsLineCount
           perform varying wsCol from 1 by 1 until wsCol > wsLineLength
               *> horizontal
               if (1 + wsLineLength - wsCol) >= wsWordLen
                   move wsLine(wsRow)(wsCol:wsWordLen) to wsWord
                   if (wsWord = "XMAS") or (wsWord = "SAMX")
                       add 1 to wsXmasCount
                   end-if
               end-if

               *> vertical
               move 1 to wsIdx
               if (1 + wsLineCount - wsRow) >= wsWordLen
                   perform varying wsIdx2 from 0 by 1 until wsIdx2 >= wsWordLen
                       move wsLine(wsRow + wsIdx2)(wsCol:1) to wsWord(wsIdx:1)
                       add 1 to wsIdx
                   end-perform

                   if (wsWord = "XMAS") or (wsWord = "SAMX")
                       add 1 to wsXmasCount
                   end-if
               end-if

               *> diagonal, right down
               move 1 to wsIdx
               if ((1 + wsLineCount - wsRow) >= wsWordLen) and ((1 + wsLineLength - wsCol) >= wsWordLen)
                   perform varying wsIdx2 from 0 by 1 until wsIdx2 >= wsWordLen
                       move wsLine(wsRow + wsIdx2)(wsCol+wsIdx2:1) to wsWord(wsIdx:1)
                       add 1 to wsIdx
                   end-perform

                   if (wsWord = "XMAS") or (wsWord = "SAMX")
                       add 1 to wsXmasCount
                   end-if
               end-if

               *> diagonal, left down
               move 1 to wsIdx
               if ((1 + wsLineCount - wsRow) >= wsWordLen) and (wsCol >= wsWordLen)
                   perform varying wsIdx2 from 0 by 1 until wsIdx2 >= wsWordLen
                       move wsLine(wsRow + wsIdx2)(wsCol+(-wsIdx2):1) to wsWord(wsIdx:1)
                       add 1 to wsIdx
                   end-perform

                   if (wsWord = "XMAS") or (wsWord = "SAMX")
                       add 1 to wsXmasCount
                   end-if
               end-if
           end-perform
       end-perform
       
       *> 2427
       display "Count: " wsXmasCount

       stop run.
