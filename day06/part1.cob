identification division.
program-id. AOC202406P1.

environment division.
input-output section.
file-control.
       select InputFile assign to 'input'
           organization is line sequential.  

data division.
file section.
FD InputFile
       record varying 1 to 131 depending on wsLineLength.
01 InputRecord pic X(131).

working-storage section.
01 wsLineLength pic 9(3).
01 wsLineCount pic 9(3).
01 wsY pic S9(3) value 0.
01 wsX pic S9(3).
01 wsCellIdx pic 9(5).
01 wsEof pic A(1) value 'n'.
01 wsCells.
       05 wsCell pic A(1) occurs 16900 times.
01 wsStartX pic S9(3).
01 wsStartY pic S9(3).
01 wsNextX pic S9(3).
01 wsNextY pic S9(3).
01 wsDirection pic A(1) value 'N'. *> North
01 wsCount pic 9(5) value 0.

procedure division.
       open input InputFile

       perform until wsEof = "y"
           read InputFile into InputRecord
               at end move "y" to wsEof
           end-read

           if wsEof not = "y"
               perform varying wsX from 0 by 1 until wsX = wsLineLength
                   compute wsCellIdx = wsX + (wsY * wsLineLength)
                   move InputRecord((wsX + 1):1) to wsCell(wsCellIdx)
                   if wsCell(wsCellIdx) = "^"
                       move wsX to wsStartX
                       move wsY to wsStartY
                   end-if
               end-perform

               add 1 to wsY
           end-if
       end-perform
       move wsY to wsLineCount *> wsY is 0-based, so this works

       close InputFile

       *> walk
       move wsStartX to wsX
       move wsStartY to wsY
       perform until (wsX < 0) or (wsY < 0) or (wsX >= wsLineLength) or (wsY >= wsLineCount)
           *> "color" the current cell
           compute wsCellIdx = wsX + (wsY * wsLineLength)
           move "X" to wsCell(wsCellIdx)

           perform set-next
           compute wsCellIdx = wsNextX + (wsNextY * wsLineLength)
           if wsCell(wsCellIdx) = "#"
               *> turn right (and stay)
               perform turn-right
           else
               *> go to next cell
               move wsNextX to wsX
               move wsNextY to wsY
           end-if
       end-perform

       *> count
       perform varying wsY from 0 by 1 until wsY = wsLineCount
           perform varying wsX from 0 by 1 until wsX = wsLineLength
               compute wsCellIdx = wsX + (wsY * wsLineLength)
               if wsCell(wsCellIdx) = "X"
                   add 1 to wsCount
               end-if
           end-perform
       end-perform

       *> 5086
       display "Count: " wsCount

       stop run.

set-next.
       if wsDirection = 'N'
           move wsX to wsNextX
           subtract 1 from wsY giving wsNextY
       else if wsDirection = 'E'
           move wsY to wsNextY
           add 1 to wsX giving wsNextX
       else if wsDirection = 'W'
           move wsY to wsNextY
           subtract 1 from wsX giving wsNextX
       else if wsDirection = 'S'
           move wsX to wsNextX
           add 1 to wsY giving wsNextY
       end-if.
turn-right.
       if wsDirection = 'N'
           move "E" to wsDirection
       else if wsDirection = 'E'
           move "S" to wsDirection
       else if wsDirection = 'W'
           move "N" to wsDirection
       else if wsDirection = 'S'
           move "W" to wsDirection
       end-if.
