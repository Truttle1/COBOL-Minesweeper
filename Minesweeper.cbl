      ******************************************************************
      * Author: Truttle1
      * Date: 12-7-2023
      * Purpose: Merry Cobol
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MINESWEEPER.
      *-----------------------
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PLAYFIELD-SIZE PIC 9(2) VALUE IS 10.
       01 TOTAL-MINES PIC 9(4) VALUE IS 10.
       01 MINES.
            05 MINE-X OCCURS 100 TIMES.
               10 MINE-Y OCCURS 100 TIMES.
                   15 MINE PIC X(1) VALUE IS '-'.

       01 SURROUNDING.
            05 SURROUNDING-X OCCURS 100 TIMES.
               10 SURROUNDING-Y OCCURS 100 TIMES.
                   15 SUR PIC 9(1) VALUE IS 0.

       01 VISIBLE.
            05 VISIBLE-X OCCURS 100 TIMES.
               10 VISIBLE-Y OCCURS 100 TIMES.
                   15 VIS PIC 9(1) VALUE IS 0.

       01 X PIC S9(3).
       01 Y PIC S9(3).
       01 XX PIC S9(1).
       01 YY PIC S9(1).

       01 IN-X PIC S9(3).
       01 IN-Y PIC S9(3).

       01 GAME-DONE PIC 9(1) VALUE IS 0.

       01 MINE-COUNT PIC 9(4).

       01 RAND PIC 99.999(3).

       01  DATE-FIELDS.
           05  CURRENT-DATE.
               10  CURRENT-YEAR            PIC  9(4).
               10  CURRENT-MONTH           PIC  9(2).
               10  CURRENT-DAY             PIC  9(2).
           05  CURRENT-TIME.
               10  HOUR            PIC  9(2).
               10  MINUTE          PIC  9(2).
               10  SECOND          PIC  9(2).
               10  MS              PIC  9(2).

       01 SEED                        PIC S9(4) COMP.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           PERFORM GENERATE-MINES.
           PERFORM CALCULATE-SURROUNDING.
           PERFORM GAME UNTIL GAME-DONE EQUALS 1.
           STOP RUN.

       GENERATE-MINES.
           MOVE FUNCTION CURRENT-DATE TO DATE-FIELDS.
           MOVE TOTAL-MINES TO MINE-COUNT.
           COMPUTE SEED = SECOND * MS.
           COMPUTE RAND = FUNCTION RANDOM(SEED).
           PERFORM UNTIL MINE-COUNT EQUALS 0
               COMPUTE RAND = 1 + FUNCTION RANDOM * PLAYFIELD-SIZE
               MOVE RAND TO X
               COMPUTE RAND = 1 + FUNCTION RANDOM * PLAYFIELD-SIZE
               MOVE RAND TO Y
               IF MINE(X, Y) EQUALS '-'
                   SUBTRACT 1 FROM MINE-COUNT
                   MOVE '*' TO MINE(X, Y)
               END-IF
           END-PERFORM.

       CALCULATE-SURROUNDING.
           MOVE 1 TO Y.
           PERFORM UNTIL Y > PLAYFIELD-SIZE
               MOVE 1 TO X
               PERFORM UNTIL X > PLAYFIELD-SIZE
                   IF MINE(X, Y) EQUALS '-' THEN
                       MOVE 0 TO MINE-COUNT
                       MOVE -1 TO YY
                       IF Y EQUALS 1
                           MOVE 0 TO YY
                       END-IF
                       PERFORM UNTIL YY > 1 OR (Y + YY > PLAYFIELD-SIZE)
                           MOVE -1 TO XX
                           IF X EQUALS 1
                               MOVE 0 TO XX
                           END-IF
                           PERFORM UNTIL XX > 1 OR
                               (X + XX > PLAYFIELD-SIZE)
                               IF MINE(X + XX, Y + YY) = '*' THEN
                                   ADD 1 TO MINE-COUNT
                               END-IF
                               ADD 1 TO XX
                           END-PERFORM
                           ADD 1 TO YY
                       END-PERFORM
                       MOVE MINE-COUNT TO SUR(X, Y)
                   ELSE
                       MOVE 9 TO SUR(X, Y)
                   END-IF
                   ADD 1 TO X
               END-PERFORM
               ADD 1 TO Y
           END-PERFORM.


       PRINT-MINES.
           MOVE 1 TO Y.
           MOVE 1 TO X.
           PERFORM UNTIL Y > PLAYFIELD-SIZE
                PERFORM UNTIL X > PLAYFIELD-SIZE
                   DISPLAY MINE(X, Y) WITH NO ADVANCING
                   ADD 1 TO X
                END-PERFORM
                MOVE 1 TO X
                ADD 1 TO Y
                DISPLAY " "
           END-PERFORM.

       PRINT-SURROUNDING.
           MOVE 1 TO Y.
           MOVE 1 TO X.
           PERFORM UNTIL Y > PLAYFIELD-SIZE
                PERFORM UNTIL X > PLAYFIELD-SIZE
                   DISPLAY SUR(X, Y) WITH NO ADVANCING
                   ADD 1 TO X
                END-PERFORM
                MOVE 1 TO X
                ADD 1 TO Y
                DISPLAY " "
           END-PERFORM.

       PRINT-GAME.
           MOVE 1 TO Y.
           MOVE 1 TO X.
           PERFORM UNTIL Y > PLAYFIELD-SIZE
                PERFORM UNTIL X > PLAYFIELD-SIZE
                   IF VIS(X, Y) EQUALS 1
                       IF MINE(X, Y) EQUALS '-'
                           IF SUR(X, Y) EQUALS 0
                               DISPLAY '-' WITH NO ADVANCING
                           ELSE
                               DISPLAY SUR(X, Y) WITH NO ADVANCING
                           END-IF
                       ELSE
                           DISPLAY '*' WITH NO ADVANCING
                       END-IF
                   ELSE
                       DISPLAY '#' WITH NO ADVANCING
                   END-IF
                   ADD 1 TO X
                END-PERFORM
                MOVE 1 TO X
                ADD 1 TO Y
                DISPLAY " "
           END-PERFORM.

       UNCOVER.
           MOVE 1 TO Y.
           PERFORM UNTIL Y > PLAYFIELD-SIZE
               MOVE 1 TO X
               PERFORM UNTIL X > PLAYFIELD-SIZE
                   IF MINE(X, Y) EQUALS '-' THEN
                       MOVE -1 TO YY
                       IF Y EQUALS 1
                           MOVE 0 TO YY
                       END-IF
                       PERFORM UNTIL YY > 1 OR (Y + YY > PLAYFIELD-SIZE)
                           MOVE -1 TO XX
                           IF X EQUALS 1
                               MOVE 0 TO XX
                           END-IF
                           PERFORM UNTIL XX > 1 OR
                               (X + XX > PLAYFIELD-SIZE)
                               IF VIS(X + XX, Y + YY) EQUALS 1
                                   AND SUR(X + XX, Y + YY) EQUALS 0 THEN
                                   MOVE 1 TO VIS(X, Y)
                               END-IF
                               ADD 1 TO XX
                           END-PERFORM
                           ADD 1 TO YY
                       END-PERFORM
                   END-IF
                   ADD 1 TO X
               END-PERFORM
               ADD 1 TO Y
           END-PERFORM.


       CHECK-WIN.
           MOVE 1 TO Y.
           MOVE 1 TO X.
           MOVE PLAYFIELD-SIZE TO MINE-COUNT.
           MULTIPLY 2 BY MINE-COUNT.

           PERFORM UNTIL Y > PLAYFIELD-SIZE
                PERFORM UNTIL X > PLAYFIELD-SIZE
                    IF VIS(X, Y) EQUALS 0 THEN
                        SUBTRACT 1 FROM MINE-COUNT
                    END-IF
                    ADD 1 TO X
                END-PERFORM
                MOVE 1 TO X
                ADD 1 TO Y
           END-PERFORM.

           IF MINE-COUNT EQUALS TOTAL-MINES THEN
               PERFORM PRINT-GAME
               DISPLAY "YOU WIN!"
               MOVE 1 TO GAME-DONE
           END-IF.

       GAME.
           PERFORM PRINT-GAME.
           DISPLAY "Input X Coordinate:".
           ACCEPT IN-X.
           DISPLAY "Input Y Coordinate:".
           ACCEPT IN-Y.
           IF IN-X EQUALS 0
               MOVE 1 TO GAME-DONE
           ELSE
               MOVE 1 TO VIS(IN-X, IN-Y)
               IF MINE(IN-X, IN-Y) EQUALS '*' THEN
                   PERFORM PRINT-GAME
                   DISPLAY "GAME OVER!"
                   MOVE 1 TO GAME-DONE
               END-IF
               PERFORM UNCOVER PLAYFIELD-SIZE TIMES
               PERFORM CHECK-WIN
           END-IF.

       END PROGRAM MINESWEEPER.
