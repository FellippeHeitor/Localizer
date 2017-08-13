OPTION _EXPLICIT

CONST true = -1, false = NOT true

TYPE new_item
    index AS LONG
    selected AS _BYTE
    startPos AS _UNSIGNED LONG
    endPos AS _UNSIGNED LONG
    duplicates AS LONG
    sequential AS LONG
END TYPE

REDIM SHARED strings(1000) AS STRING
REDIM SHARED item(1000) AS new_item
DIM index AS LONG, strIndex AS LONG, totalSelected AS LONG, totalWithDuplicates AS LONG
DIM file$, line$, strAssign AS LONG, endStrAssign AS LONG
DIM thisStr$, u$, s$
DIM i AS LONG, j AS LONG, oldi AS LONG, y AS LONG
DIM k AS LONG, startPos AS _UNSIGNED LONG

PRINT "String Extractor"

DO
    INPUT "Source file: ", file$
    IF file$ = "" THEN SYSTEM
    IF _FILEEXISTS(file$) THEN EXIT DO
    IF _FILEEXISTS(file$ + ".bas") THEN file$ = file$ + ".bas": EXIT DO
LOOP

OPEN file$ FOR BINARY AS #1
DO
    IF EOF(1) THEN EXIT DO
    startPos = SEEK(1)
    LINE INPUT #1, line$

    strAssign = 0
    DO
        strAssign = INSTR(strAssign + 1, line$, CHR$(34)) '"= " + CHR$(34))
        IF strAssign = 0 THEN EXIT DO

        endStrAssign = INSTR(strAssign + 1, line$, CHR$(34))

        IF endStrAssign = strAssign + 1 THEN strAssign = strAssign + 1: endStrAssign = 0

        IF endStrAssign = 0 THEN _CONTINUE

        thisStr$ = MID$(line$, strAssign + 1, (endStrAssign - strAssign) - 1)

        IF LEN(thisStr$) THEN
            index = index + 1
            increaseArrays index

            'look for the same string already in memory and create a pointer
            strIndex = 0
            FOR i = 1 TO index - 1
                IF strings(i) = thisStr$ THEN strIndex = i: EXIT FOR
            NEXT

            IF strIndex = 0 THEN
                strings(index) = thisStr$
            ELSE
                item(strIndex).duplicates = item(strIndex).duplicates + 1
            END IF

            item(index).index = strIndex
            'store absolute address of this string in the file:
            item(index).startPos = startPos + strAssign - 1
            item(index).endPos = startPos + endStrAssign - 1

            strAssign = endStrAssign + 1

            LOCATE , 1
            CONST progressBar = 20
            PRINT "Extracting strings... ["; STRING$((startPos / LOF(1)) * progressBar, 254) + SPACE$(progressBar - ((startPos / LOF(1)) * progressBar)); "] "; USING "###%"; (startPos / LOF(1)) * 100;
        END IF
    LOOP

LOOP
CLOSE #1

IF index = 0 THEN END

u$ = STRING$(LEN(LTRIM$(STR$(index))), "#") + " "

i = 1
j = 1

DO
    oldi = i

    WHILE _MOUSEINPUT
        i = i + _MOUSEWHEEL * 5
    WEND

    IF oldi <> i THEN j = 1

    k = _KEYHIT

    IF k THEN
        _KEYCLEAR
        'Arrows
        IF k = 19200 THEN j = j + (j > 1)
        IF k = 19712 THEN j = j + 1
        IF k = 18432 THEN i = i + (i > 1)
        IF k = 20480 THEN i = i - (i < index)

        'ctrl+a to select all
        IF (k = ASC("a") OR k = ASC("A")) AND (_KEYDOWN(100306) OR _KEYDOWN(100305)) THEN
            totalSelected = 0
            totalWithDuplicates = 0
            FOR y = 1 TO index
                IF item(y).index = 0 THEN
                    item(y).selected = true
                    totalSelected = totalSelected + 1
                    totalWithDuplicates = totalWithDuplicates + item(y).duplicates + 1
                END IF
            NEXT
        END IF

        'ctrl+n to desselect all
        IF (k = ASC("n") OR k = ASC("N")) AND (_KEYDOWN(100306) OR _KEYDOWN(100305)) THEN
            totalSelected = 0
            totalWithDuplicates = 0
            FOR y = 1 TO index
                item(y).selected = false
            NEXT
        END IF

        'enter to confirm/proceed
        IF k = 13 THEN
            EXIT DO
        END IF
    END IF

    IF i + (_HEIGHT - 2) > index THEN i = index - (_HEIGHT - 2)
    IF i < 1 THEN i = 1

    CLS

    FOR y = i TO index
        IF y - i + 1 > _HEIGHT - 1 THEN EXIT FOR
        LOCATE y - i + 1, 1
        COLOR 8
        IF INT(_MOUSEY + .5) = y - i + 1 OR item(y).selected THEN COLOR 15
        PRINT USING u$; y;

        IF item(y).index = 0 THEN strIndex = y ELSE strIndex = item(y).index

        COLOR 7
        IF item(strIndex).selected THEN COLOR 15, 6 ELSE IF INT(_MOUSEY + .5) = y - i + 1 THEN COLOR 15

        IF INT(_MOUSEY + .5) = y - i + 1 THEN
            'IF item(y).selected THEN COLOR 15, 1 ELSE COLOR 0, 7
            IF _MOUSEBUTTON(1) THEN
                WHILE _MOUSEBUTTON(1): WHILE _MOUSEINPUT: WEND: WEND
                IF INT(_MOUSEY + .5) = y - i + 1 THEN
                    item(strIndex).selected = NOT item(strIndex).selected
                    IF item(strIndex).selected THEN
                        totalSelected = totalSelected + 1
                        totalWithDuplicates = totalWithDuplicates + item(strIndex).duplicates + 1
                    ELSE
                        totalSelected = totalSelected - 1
                        totalWithDuplicates = totalWithDuplicates - item(strIndex).duplicates - 1
                    END IF
                END IF
            END IF
        END IF

        PRINT MID$(strings(strIndex), j, 80 - LEN(u$));
        IF item(strIndex).selected THEN IF POS(1) < 80 THEN PRINT SPACE$(81 - POS(1));
        COLOR , 0
    NEXT

    COLOR 3
    _PRINTSTRING (1, 25), STRING$(80, 205)
    s$ = " Total found:" + STR$(index) + " " + CHR$(254) + " Total selected:" + STR$(totalWithDuplicates) + " "
    LOCATE 25, 76 - LEN(s$)
    PRINT s$;

    COLOR 14
    IF i > 1 THEN LOCATE 1, 77: PRINT "(" + CHR$(24) + ")";
    IF i + 25 < index THEN LOCATE 25, 77: PRINT "(" + CHR$(25) + ")";

    _DISPLAY
    _LIMIT 30
LOOP

IF totalSelected = 0 THEN SYSTEM

CLS
COLOR 15
_AUTODISPLAY
DIM newFile$, langFile$, a$, b$
langFile$ = file$ + ".lang"

IF LCASE$(RIGHT$(file$, 4)) = ".bas" THEN
    newFile$ = LEFT$(file$, LEN(file$) - 4) + ".localizer.bas"
ELSE
    newFile$ = newFile$ + ".localizer.bas"
END IF

IF _FILEEXISTS(langFile$) THEN
    'create a backup copy of the existing .lang file
    'before recreating it
    OPEN langFile$ + ".backup" FOR OUTPUT AS #1
    CLOSE #1
    OPEN langFile$ FOR BINARY AS #1
    OPEN langFile$ + ".backup" FOR BINARY AS #2
    a$ = SPACE$(LOF(1))
    GET #1, 1, a$
    CLOSE #1
    PUT #2, 1, a$
    CLOSE #2
END IF

OPEN langFile$ FOR OUTPUT AS #1
PRINT #1, "'This language file was generated by"
PRINT #1, "'Localizer, by @FellippeHeitor - fellippe@qb64.org"
PRINT #1, "'https://github.com/FellippeHeitor/Localizer"
PRINT #1, ""
PRINT #1, "CONST __LocalizerCount = 1"
PRINT #1, ""
PRINT #1, "DIM SHARED __Localizer(1 TO __LocalizerCount, 1 TO" + STR$(totalSelected) + ") AS STRING"
PRINT #1, "DIM SHARED __LocalizerLang AS LONG, __LocalizerIndex AS LONG"
PRINT #1, ""
PRINT #1, "'Use the variable below in your program to alternate between languages:"
PRINT #1, "__LocalizerLang = 1"
PRINT #1, ""
PRINT #1, "'Copy the following block of strings as many times as required."
PRINT #1, "'Every new block is a new language set. Don't forget to"
PRINT #1, "'update the line CONST __LocalizerCount above."
PRINT #1, ""
PRINT #1, "'----------------------------------------------------------------"
PRINT #1, "'Original strings."
PRINT #1, "__LocalizerIndex = __LocalizerIndex + 1"

y = 0
FOR i = 1 TO index
    IF item(i).selected THEN
        y = y + 1
        item(i).sequential = y
        PRINT #1, "__Localizer(__LocalizerIndex," + STR$(y) + ") = " + CHR$(34) + strings(i) + CHR$(34)
    END IF
NEXT
CLOSE #1

'create a new copy of the original source file,
'then output to it with variable replacements
OPEN file$ FOR BINARY AS #1
a$ = SPACE$(LOF(1))
GET #1, 1, a$
CLOSE #1

'Inject Localizer strings and rebuild the rest of the original source:
b$ = LEFT$(a$, item(1).startPos - 1)
FOR i = 1 TO index
    IF item(i).selected THEN
        b$ = b$ + "__Localizer(__LocalizerLang," + STR$(item(i).sequential) + ")"
        IF i < index THEN
            b$ = b$ + MID$(a$, item(i).endPos + 1, item(i + 1).startPos - item(i).endPos - 1)
        ELSE
            b$ = b$ + MID$(a$, item(i).endPos + 1)
        END IF
    ELSEIF item(item(i).index).selected THEN
        b$ = b$ + "__Localizer(__LocalizerLang," + STR$(item(item(i).index).sequential) + ")"
        IF i < index THEN
            b$ = b$ + MID$(a$, item(i).endPos + 1, item(i + 1).startPos - item(i).endPos - 1)
        ELSE
            b$ = b$ + MID$(a$, item(i).endPos + 1)
        END IF
    ELSE
        IF i < index THEN
            b$ = b$ + MID$(a$, item(i).startPos, item(i + 1).startPos - item(i).startPos)
        ELSE
            b$ = b$ + MID$(a$, item(i).startPos)
        END IF
    END IF

    LOCATE , 1
    PRINT "Injecting Localizer code... ["; STRING$((i / index) * progressBar, 254) + SPACE$(progressBar - ((i / index) * progressBar)); "] "; USING "###%"; (i / index) * 100;
NEXT

OPEN newFile$ FOR OUTPUT AS #1: CLOSE #1
OPEN newFile$ FOR BINARY AS #1
a$ = "'$INCLUDE:'" + langFile$ + "'"
$IF WIN THEN
    a$ = a$ + CHR$(13)
$END IF
a$ = a$ + CHR$(10)
PUT #1, 1, a$
PUT #1, , b$
CLOSE #1

PRINT
PRINT "Done."

SUB increaseArrays (upperBoundary AS LONG)

    IF upperBoundary > UBOUND(item) THEN
        REDIM _PRESERVE item(LBOUND(item) TO upperBoundary) AS new_item
        REDIM _PRESERVE strings(LBOUND(item) TO upperBoundary) AS STRING
    END IF

END SUB

