identification division.
program-id. AOC202405P2.

environment division.
input-output section.
file-control.
       select InputFile assign to 'input'
           organization is line sequential.  

data division.
file section.
FD InputFile
       record varying 1 to 100 depending on wsLineLength.
01 InputRecord pic X(100).

working-storage section.
01 wsState pic A(1) value 'a'.
01 wsLineLength pic 9(3).
01 wsPairs.
       05 wsPair pic X(5) occurs 1500 times indexed by f.
01 wsPageNumbers.
       05 wsPageNumber pic 9(2) occurs 100 times indexed by p, a, b.

01 wsIdx pic 9(4) value 1.
01 wsIdx2 pic 9(4) value 1.
01 wsStart pic 9(3).
01 wsNumCount pic 9(3).
01 wsTmp pic 9(3).
01 wsPairCount pic 9(4).
01 wsTestPair pic X(5).
01 wsOrdered pic X(1).
01 wsCurOrdered pic X(1).
01 wsSum pic 9(5) value 0.
01 wsMiddleIdx pic 9(3).
01 wsSwap pic 9(2).

procedure division.
       open input InputFile

       move 1 to wsIdx
       perform until wsState = "x"
           read InputFile into InputRecord
               at end move "x" to wsState
           end-read

           if wsLineLength = 0
               *> empty line, done reading pairs
               move "b" to wsState
               move wsIdx to wsPairCount
               subtract 1 from wsPairCount
               add 1 to wsIdx
           else if wsState = "a"
               *> line that has a pair
               move InputRecord(1:wsLineLength) to wsPair(wsIdx)
               add 1 to wsIdx
           else if wsState = "b"
               *> line that has a list of numbers
               perform split-numbers

               *> calc the 1-based index of the middle number
               compute wsMiddleIdx = (wsNumCount + 1) / 2

               *> tracks ordered-status of this line
               move "y" to wsOrdered

               *> sort pairs using bubble sort
               perform varying a from 1 by 1 until a > wsNumCount
                   compute wsTmp = a + 1 *> stupid!
                   perform varying b from wsTmp by 1 until b > wsNumCount
                       *> create wsTestPair which contains the pages in the OPPOSITE order.
                       *> if this pair exists in wsPairs, the the line has incorrect order.
                       string wsPageNumber(b) delimited by size
                           "|" delimited by size
                           wsPageNumber(a) delimited by size
                           into wsTestPair
                       end-string

                       move "y" to wsCurOrdered
                       perform varying f from 1 by 1 until (f > wsPairCount) or (wsCurOrdered = "n")
                           if wsPair(f) = wsTestPair
                               *> indicate that this line is unordered
                               move "n" to wsOrdered
                               *> must be swapped
                               move "n" to wsCurOrdered
                           end-if
                       end-perform

                       if wsCurOrdered = "n"
                           move wsPageNumber(a) to wsSwap
                           move wsPageNumber(b) to wsPageNumber(a)
                           move wsSwap to wsPageNumber(b)
                       end-if
                   end-perform
               end-perform

               if wsOrdered = "n"
                   compute wsSum = wsSum + wsPageNumber(wsMiddleIdx)
               end-if

               add 1 to wsIdx
           end-if
       end-perform

       close InputFile

       *> 6257
       display "Sum: " wsSum

       stop run.

split-numbers.
       move 1 to wsStart
       move 1 to p
       move 1 to wsIdx2
       perform until wsIdx2 > wsLineLength
           if InputRecord(wsIdx2:1) = ","
               move InputRecord(wsStart:(wsIdx2 + (-wsStart))) to wsPageNumber(p)
               add 1 to p

               *> new start
               compute wsStart = wsIdx2 + 1
           end-if
           add 1 to wsIdx2
       end-perform
       move p to wsNumCount
       move InputRecord(wsStart:(wsIdx2 + (-wsStart))) to wsPageNumber(p)
       .
