identification division.
program-id. AOC202408P1.

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
       01 wsLineLength pic 9(2).
       01 wsEof pic A(1) value "n".
       01 wsX binary-short.
       01 wsY binary-short value 0.
       01 wsIdx binary-short.
       01 wsIdxA binary-short.
       01 wsIdxB binary-short.
       01 wsFreq pic A(1).
       *> Counts the unique frequencies found so far.
       01 wsFreqCount binary-short value 0.
       01 wsFrequencies.
           02 wsFreqency pic A(1) occurs 100 times indexed by ixFreq.  *> the frequency
           02 wsAntennaCount binary-short occurs 100 times.            *> number of antennas using this frequency
           02 wsAntennas occurs 100 times.
               03 wsAntennaY binary-short occurs 100 times.
               03 wsAntennaX binary-short occurs 100 times.
       01 wsAntiNodeCount binary-short value 0.
       01 wsAntiNodes.
           02 wsAntiNodeY binary-short signed occurs 10000 times indexed by ixAN.
           02 wsAntiNodeX binary-short signed occurs 10000 times.
       01 wsDX binary-short signed. *> delta-X between antennas
       01 wsDY binary-short signed. *> delta-Y between antennas
       01 wsANX binary-short signed. *> Anti-node X
       01 wsANY binary-short signed. *> Anti-node Y
       01 wsFreqIdx binary-short.
       01 wsOutside binary-short.
       01 wsMul binary-short.

procedure division.
       open input InputFile

       perform until wsEof = "y"
           read InputFile into InputRecord
               at end move "y" to wsEof
           end-read

           if wsEof not = "y"
               perform parse-record
               add 1 to wsY
           end-if
       end-perform

       close InputFile

       perform find-antinodes

       *> input => 1339
       display "RESULT = " wsAntiNodeCount

       stop run.

find-antinodes.
       perform varying wsFreqIdx from 1 by 1 until wsFreqIdx > wsFreqCount
           perform find-for-freq
       end-perform.

find-for-freq.
       *> Go through antennas pairwise
       perform varying wsIdxA from 1 by 1 until wsIdxA > wsAntennaCount(wsFreqIdx)
           add 1 to wsIdxA giving wsIdxB
           perform until wsIdxB > wsAntennaCount(wsFreqIdx)
               subtract wsAntennaY(wsFreqIdx, wsIdxA) from wsAntennaY(wsFreqIdx, wsIdxB) giving wsDY
               subtract wsAntennaX(wsFreqIdx, wsIdxA) from wsAntennaX(wsFreqIdx, wsIdxB) giving wsDX

               *> Start at B, add
               set wsMul to 0
               set wsOutside to 2 *> bogus value, is there do..while?
               perform until wsOutside = 1
                   compute wsANX = wsAntennaX(wsFreqIdx, wsIdxB) + wsMul * wsDX
                   compute wsANY = wsAntennaY(wsFreqIdx, wsIdxB) + wsMul * wsDY
                   perform try-add-antinode
                   add 1 to wsMul
               end-perform

               *> Start at A, subtract
               set wsMul to 0
               set wsOutside to 2 *> bogus value, is there do..while?
               perform until wsOutside = 1
                   compute wsANX = wsAntennaX(wsFreqIdx, wsIdxA) - wsMul * wsDX
                   compute wsANY = wsAntennaY(wsFreqIdx, wsIdxA) - wsMul * wsDY
                   perform try-add-antinode
                   add 1 to wsMul
               end-perform

               add 1 to wsIdxB
           end-perform
       end-perform.

try-add-antinode.
       *> Adds the antinode at WSANY,wsANX, unless it is outside the grid
       set wsOutside to 0
       if wsANX >= 0 and wsANX < wsLineLength and wsANY >= 0 and wsANY < wsY
           *> Cannot use SEARCH, since values beyond wsAntiNodeCount default to 0
           perform varying ixAN from 1 by 1 until ixAN > wsAntiNodeCount
               if wsAntiNodeY(ixAN) = wsANY and wsAntiNodeX(ixAN) = wsANX
                   *> Found existing
                   next sentence
               end-if
           end-perform
           if ixAN > wsAntiNodeCount
               *> New antinode
               add 1 to wsAntiNodeCount
               move wsANY to wsAntiNodeY(wsAntiNodeCount)
               move wsANX to wsAntiNodeX(wsAntiNodeCount)
           end-if
       else
           set wsOutside to 1
       end-if.

parse-record.
       perform varying wsX from 0 by 1 until wsX = wsLineLength
           move InputRecord((wsX + 1):1) to wsFreq
           if wsFreq not = "."
               set ixFreq to 1
               search wsFreqency
                   at end
                       *> New unique frequency
                       add 1 to wsFreqCount
                       move wsFreq to wsFreqency(wsFreqCount)

                       move wsFreqCount to wsFreqIdx
                       perform add-antenna
                   when wsFreqency(ixFreq) = wsFreq
                       move ixFreq to wsFreqIdx
                       perform add-antenna
               end-search
           end-if
       end-perform.

add-antenna.
       add 1 to wsAntennaCount(wsFreqIdx)
       move wsY to wsAntennaY(wsFreqIdx, wsAntennaCount(wsFreqIdx))
       move wsX to wsAntennaX(wsFreqIdx, wsAntennaCount(wsFreqIdx)).

end program AOC202408P1.
