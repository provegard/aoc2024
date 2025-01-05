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
           02 wsAntiNodeY binary-short signed occurs 1000 times indexed by ixAN.
           02 wsAntiNodeX binary-short signed occurs 1000 times.
       01 wsDX binary-short signed. *> delta-X between antennas
       01 wsDY binary-short signed. *> delta-Y between antennas
       01 wsANX binary-short signed. *> Anti-node X
       01 wsANY binary-short signed. *> Anti-node Y
       01 wsFreqIdx binary-short.

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

       *> input => 379
       display "RESULT = " wsAntiNodeCount

       stop run.

find-antinodes.
       perform varying wsIdx from 1 by 1 until wsIdx > wsFreqCount
           perform find-for-freq
       end-perform.

find-for-freq.
       *> Go through antennas pairwise
       perform varying wsIdxA from 1 by 1 until wsIdxA > wsAntennaCount(wsIdx)
           add 1 to wsIdxA giving wsIdxB
           perform until wsIdxB > wsAntennaCount(wsIdx)
               subtract wsAntennaY(wsIdx, wsIdxA) from wsAntennaY(wsIdx, wsIdxB) giving wsDY
               subtract wsAntennaX(wsIdx, wsIdxA) from wsAntennaX(wsIdx, wsIdxB) giving wsDX
               
               *> First antinode, add to B
               compute wsANX = wsAntennaX(wsIdx, wsIdxB) + wsDX
               compute wsANY = wsAntennaY(wsIdx, wsIdxB) + wsDY
               perform try-add-antinode

               *> Second antinode, subtract from A
               compute wsANX = wsAntennaX(wsIdx, wsIdxA) - wsDX
               compute wsANY = wsAntennaY(wsIdx, wsIdxA) - wsDY
               perform try-add-antinode

               add 1 to wsIdxB
           end-perform
       end-perform.

try-add-antinode.
       *> Adds the antinode at WSANY,wsANX, unless it is outside the grid
       if wsANX >= 0 and wsANX < wsLineLength and wsANY >= 0 and wsANY < wsY
           set ixAN to 1
           search wsAntiNodeY
               at end
                   add 1 to wsAntiNodeCount
                   move wsANY to wsAntiNodeY(wsAntiNodeCount)
                   move wsANX to wsAntiNodeX(wsAntiNodeCount)
               when wsAntiNodeY(ixAN) = wsANY and wsAntiNodeX(ixAN) = wsANX
                   continue
           end-search
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
