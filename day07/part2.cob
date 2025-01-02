identification division.
program-id. AOC202407P2.

environment division.
input-output section.
file-control.
       select InputFile assign to 'input'
           organization is line sequential.  

data division.
file section.
       FD InputFile
           record varying 1 to 50 depending on wsLineLength.
       01 InputRecord pic X(50).

working-storage section.
       01 wsLineLength pic 9(3).
       01 wsEof pic A(1) value "n".
       01 wsIdx pic 9(3).
       01 wsStart pic 9(3).
       01 wsPart pic X(16).
       01 wsPartIdx pic 9(3).
       01 wsSuccess pic 9(1).
       01 wsSum binary-double value 0.
       01 wsResult binary-double.
       01 wsOperands.
           02 wsOperand binary-double occurs 20 times.

procedure division.
       open input InputFile

       perform until wsEof = "y"
           read InputFile into InputRecord
               at end move "y" to wsEof
           end-read

           if wsEof not = "y"
               perform parse-record

               move 0 to wsSuccess
               call "Calc" using wsResult wsOperands 1 0 "+" wsSuccess
               if wsSuccess = 1
                   add wsResult to wsSum
               end-if
           end-if
       end-perform

       close InputFile

       *> 264184041398847 (slow)
       display "Sum = " wsSum

       stop run.

parse-record.
       move 1 to wsStart
       move 1 to wsPartIdx

       *> initialize operands to -1, so we know where they end
       perform varying wsIdx from 1 by 1 until wsIdx = 20
           move -1 to wsOperand(wsIdx)
       end-perform

       perform varying wsIdx from 1 by 1 until wsIdx > (wsLineLength + 1)
           if wsIdx > wsLineLength or InputRecord(wsIdx:1) = " "
               move InputRecord(wsStart:(wsIdx - wsStart)) to wsPart

               if wsPartIdx = 1
                   *> Strip the trailing ":"
                   move wsPart(1:(wsIdx - wsStart - 1)) to wsResult
               else
                   move wsPart to wsOperand(wsPartIdx - 1)
               end-if
               add 1 to wsIdx giving wsStart
               add 1 to wsPartIdx
           end-if
       end-perform.
end program AOC202407P1.


identification division.
program-id. Calc recursive.

data division.
local-storage section.
       01 lsOperators pic X(3) value "+*|".
       01 lsOperatorIdx pic 9(1).
       01 lsOperand binary-double.
       01 lsValue binary-double.
       01 lsNextOperandIndex binary-short.
       01 lsNextOperator pic X(1).
       01 lsNum1 pic Z(16).
       01 lsNum2 pic Z(16).
       01 lsStrNum1 pic X(25).
       01 lsStrNum2 pic X(25).
       01 lsConcat pic X(25).
linkage section.
       01 lkResult binary-double.
       01 lkOperands.
           02 lkOperand binary-double occurs 20 times.
       01 lkOperandIndex binary-short.
       01 lkPrevValue binary-double.
       01 lkOperator pic X(1).
       01 lkSuccessOut pic 9(1).

procedure division using lkResult lkOperands lkOperandIndex lkPrevValue lkOperator lkSuccessOut.
       move lkOperand(lkOperandIndex) to lsOperand

       if lsOperand < 0
           *> Done
           if lkPrevValue = lkResult
               move 1 to lkSuccessOut
           else
               move 0 to lkSuccessOut
           end-if
           goback
       end-if

       if lkOperator = "+"
           add lsOperand to lkPrevValue giving lsValue
       end-if
       if lkOperator = "*"
           multiply lsOperand by lkPrevValue giving lsValue
       end-if
       if lkOperator = "|"
           *> concatenate
           move lkPrevValue to lsNum1 *> move to PIC Z, without leading zeroes
           move lsOperand to lsNum2   *> move to PIC Z, without leading zeroes
           move function trim(lsNum1) to lsStrNum1 *> trim zeroes
           move function trim(lsNum2) to lsStrNum2 *> trim zeroes
           string
               lsStrNum1 delimited by space
               lsStrNum2 delimited by space
               into lsConcat
           end-string
           move lsConcat to lsValue
       end-if

       add 1 to lkOperandIndex giving lsNextOperandIndex
       perform varying lsOperatorIdx from 1 by 1 until lsOperatorIdx > 3 or lkSuccessOut = 1
           move lsOperators(lsOperatorIdx:1) to lsNextOperator
           call "Calc" using lkResult lkOperands lsNextOperandIndex lsValue lsNextOperator lkSuccessOut
       end-perform
       goback.
end program Calc.
