IDENTIFICATION DIVISION.
PROGRAM-ID. AOC202403P1.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO 'input'
           ORGANIZATION IS LINE SEQUENTIAL.  

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD PIC X(25000).

WORKING-STORAGE SECTION.
01 WS-SUBSTR PIC X(10).
01 WS-IDX PIC 9(5).
01 WS-START PIC 9(5).
01 WS-EOF PIC A(1).
01 WS-STATE PIC X(10).
01 WS-NUM1 PIC 9(3).
01 WS-NUM2 PIC 9(3).

01 WS-PRODUCT PIC 9(10).
01 WS-SUM PIC 9(10) VALUE 0.
PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE

       PERFORM UNTIL WS-EOF='Y'
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE "Y" TO WS-EOF
           END-READ

           IF WS-EOF NOT = "Y"
               PERFORM EXECUTE
            *>    153469856
               DISPLAY "Sum: " WS-SUM
           END-IF
       END-PERFORM

       CLOSE INPUT-FILE

       STOP RUN.

EXECUTE.
       MOVE 1 TO WS-IDX
       PERFORM UNTIL WS-IDX > 25000
           MOVE INPUT-RECORD(WS-IDX:4) TO WS-SUBSTR
           ADD 1 TO WS-IDX
           IF WS-SUBSTR = "mul("
               *> skip "ul("
               ADD 3 TO WS-IDX
               MOVE WS-IDX to WS-START
               MOVE "go" TO WS-STATE
               *> reset numbers so that mul(xxx) just yields 0
               MOVE 0 TO WS-NUM1
               MOVE 0 TO WS-NUM2
               PERFORM UNTIL (WS-IDX > 25000) OR (WS-STATE = "stop")
                   IF INPUT-RECORD(WS-IDX:1) IS NUMERIC
                       *> Digit, continue
                       ADD 1 TO WS-IDX
                   ELSE IF INPUT-RECORD(WS-IDX:1) = ","
                       *> End first number, start second
                       MOVE INPUT-RECORD(WS-START:(WS-IDX - WS-START)) TO WS-NUM1
                       ADD 1 TO WS-IDX
                       MOVE WS-IDX to WS-START
                   ELSE IF INPUT-RECORD(WS-IDX:1) = ")"
                       *> End second number, calculate!
                       MOVE INPUT-RECORD(WS-START:(WS-IDX - WS-START)) TO WS-NUM2
                       MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-PRODUCT
                       ADD WS-PRODUCT TO WS-SUM
                       MOVE "stop" TO WS-STATE
                       ADD 1 TO WS-IDX
                   ELSE
                       MOVE "stop" TO WS-STATE
                   END-IF
               END-PERFORM
           END-IF
       END-PERFORM.
