' ________________________________________________________________________
'/Programs Objectives                                                     \
'Purpose: To have a Power Basic program convert all [(file).txt] files in  |
'Folder [HTML Text Files] to html files with a Linked Index added          |
'to each File!                                                             |
'Method:  HTML tags <pre></pre> will be added to the start and end of each |
'TEXT file record and outputted to the [same name .html] file!             |
'\________________________________________________________________________/



#CONSOLE ON                     'if set to 'OFF' program will produce no console output
#COMPILE EXE "Run_Me4Webber"
#REGISTER NONE
#DEBUG ERROR ON                 'checks for array boundary and null-  errors
DEFLNG A-Z                      'defines numeric variables starting with letter of Alfabet as long
''''''''''''''''''''''
MACRO CONST = MACRO
'Color Macros
CONST ForeRG = 0                ' Black
CONST BackRG = 2                ' Green
CONST BorderRG = 8              ' Gray
CONST BackBR = 10               ' Light green
CONST RedRG = 4                 ' message output
CONST RedBR = 12                ' Error_Trap background
CONST YellowRG=14               ' Problem background
CONST white = 7
'OTHER MACROS
CONST PTH = 20
' _____________________________________
'/Type Required by Random File Arrays
TYPE recorded
    record AS STRING * 254
END TYPE
''''''''''''''''''''''''''''''''''''''''''''
DECLARE SUB ATTEMPT_RESTORE_FROM_DELETE
DECLARE SUB COLATE_AS2
DECLARE SUB Frameit (SW,F,B)
DECLARE SUB GET_DIR (WEREDIR$)
DECLARE SUB HEADING (TITLE$)
DECLARE SUB HFRAME_MAIN (FILE$)
DECLARE SUB IMAGE
DECLARE SUB INTERNAL_HEAD (TITLE$)
DECLARE SUB PROBLEM_FILE (TOPIC$, NUMBER)
DECLARE SUB SORT_FILE (FILE$,LAST)
'''''''''''''''''''''''''''''''''''''''''''''''''''
DECLARE FUNCTION BACKUP_TAB (FILE$) AS STRING
DECLARE FUNCTION CONST_INDEX_LINKAGE (PAR$,TCM)
DECLARE FUNCTION CREATE_IMAGE$ (SUPPORT$)
DECLARE FUNCTION Create_Index_Page (FILE$) AS LONG
DECLARE FUNCTION CREATE_LINK$ (SUPPORT$,TCM)
DECLARE FUNCTION CREATE_STYLESHEETS (FILE$) AS LONG
DECLARE FUNCTION DELAY_FOR_ACTION(DELAY) AS STRING
DECLARE FUNCTION DIRCOPY (WERE$,GOING$,OUTFILE$,TXTYPE$) AS LONG
DECLARE FUNCTION DOCTYPE_DECLARE (FILE$) AS LONG
DECLARE FUNCTION DOCTYPE_HEADING (FILE$) AS LONG
DECLARE FUNCTION DPROCESS (PAR$)
DECLARE FUNCTION ENGINEER_OUTPUT
DECLARE FUNCTION ENGINEER_UNDERLINE(FILE$)
DECLARE FUNCTION ENTER (TTT) AS STRING
DECLARE FUNCTION EXENAME(OP AS LONG) AS STRING
DECLARE FUNCTION FETCH_ALL (WERE$,FILE$) AS LONG
DECLARE FUNCTION FETCH_FILE (WERE$,FILE$,FTYPE$) AS LONG
DECLARE FUNCTION FILEMOVESUPPORT(SUPPORT$)
DECLARE FUNCTION FILE_FIX(X$) AS STRING
DECLARE FUNCTION FIXLENGTH$
DECLARE FUNCTION FOLDER_FIX (X$) AS STRING
DECLARE FUNCTION FOLDER_PROCESS$(PR$)
DECLARE FUNCTION GETARRAY (VARNAME$,SX$(),FILE$) AS LONG
DECLARE FUNCTION GET_ENDLINE_NUMBER (WPAR$)
DECLARE FUNCTION HFRAME_HEAD (FILE$) AS LONG
DECLARE FUNCTION HFRAME_TAIL (FILE$) AS LONG
DECLARE FUNCTION HFRAME_THREE (FILE$) AS LONG
DECLARE FUNCTION INSTRUCTIONS (FILE$) AS LONG
DECLARE FUNCTION LICENSE (FILE$) AS LONG
DECLARE FUNCTION NEST_SUBTRACT(TM) AS LONG
DECLARE FUNCTION Create_Setup_File (FILE$) AS LONG
DECLARE FUNCTION SINGLE_QUOTE_REMOVE (PHRASE$) AS STRING
DECLARE FUNCTION SKIP_TAGS (SPAR$,OTAG$,CTAG$,TMPLENGTH) AS LONG
DECLARE FUNCTION TAGS (XX$,Z) AS LONG
DECLARE FUNCTION TCM_FIRST_SET$ (HPAR$,TCM)
DECLARE FUNCTION TCM_SECOND_SET$(TCM)
DECLARE FUNCTION TCM_THIRD_SET$(TCM)
DECLARE FUNCTION Time (x) AS STRING
DECLARE FUNCTION check_enity_marks (nPar$,TCM) AS STRING
DECLARE FUNCTION check_as2 (letter$) AS LONG
DECLARE FUNCTION Help_File(file$) AS LONG
'-------ERROR Variables----------------------------
GLOBAL ERNerror$,ERN AS LONG,NRL AS LONG,gProblem AS LONG
GLOBAL Prm_name$,New_Prg_Name$,Expire$,eMail$,werePROB$,prob AS LONG
'-------Output ------------------------------------------------------------------
GLOBAL Fore AS LONG,Back AS LONG '| COLOR control variables
GLOBAL Cast AS LONG,PX AS LONG,PRElength AS LONG,Format AS LONG,Day$,col_width AS LONG
GLOBAL firstimage$,tailground$,Good$,GoodCopy$,descript$,keyword$
'---------- Program Control Data Folders-----------------------------------------
GLOBAL Trash$,Control$,HTMLfromTEXT$,DownLoad$,HTMLfolders$,WebberText$,HoldFiles$
GLOBAL WebberTextBackup$,MainDummy$
'----------Operating Files and Variables ----------------------------------------
GLOBAL link AS LONG,place AS LONG,indexing AS LONG,Lnumber AS LONG
GLOBAL nNest AS LONG,Index AS LONG,existindex AS LONG,head_int AS LONG
GLOBAL Dest_Folder$,YearFolder$,generator$,author$,spacing$,sortorder AS STRING
GLOBAL action$,opFile$,owner$,par$,New_Dir$,tpFile$,mIndex$,sIndex$
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'___________________________________
FUNCTION PBMAIN 'start of User Code \
    ON ERROR RESUME NEXT
    DESKTOP GET LOC TO x&, y&
    'x&=10: y&=10
    CONSOLE SET LOC x&, y&
    CONSOLE SET SCREEN 25,79    '| no scrol bar setting
    CONSOLE SET FOCUS
    CONSOLE NAME "Run_Me4Webber"
    ON ERROR GOTO error_trap
    Prm_name$="text_webber vs6"
    New_Prg_Name$="Run_Me4Webber"
    Expire$="3000.01"
    eMail$="hapolley@yahoo.ca"
    s$  = DATE$
    mm$ = LEFT$(s$,2)
    DY$ = MID$(s$,4,2)
    Yr$ = RIGHT$(s$,4)
    Day$=Yr$+"." + mm$
    AtDay$=Day$+"."+DY$
    IF Expire$<Day$ THEN ERROR 157
    ON ERROR RESUME NEXT
console_end:
' _________________________________________________________________
'/a. setup Main Folders
    ON ERROR RESUME NEXT
    Trash$ = "@TRASH"
    MKDIR Trash$
    Trash$ = Trash$ + "\"
    KILL Trash$ + "*.*"
    Control$ = "@CONTROL"
    MKDIR Control$            '| common files used to construct other files
    Control$ = Control$ + "\"
    WebberText$ = "@WEBBER_TEXT"
    MKDIR WebberText$         '| stores text files to be processed to !HTML_FROM_TEXT
    WebberText$ = WebberText$ + "\"
    GoodCopy$ = "!GOODCOPY"
    MKDIR GoodCopy$
    GoodCopy$ = GoodCopy$ + "\"
    KILL GoodCopy$ + "*.*"
    HTMLfolders$ = "!HTML_FOLDERS"
    MKDIR HTMLfolders$        '| root directory for website three frames
    HTMLfolders$ = HTMLfolders$ + "\"
' ___________________________________________________________________
'/a.setup second level Folders
    DownLoad$ = HTMLfolders$ + "DOWNLOADS"    '| zipped files for downloading - NOT USED
    MKDIR DownLoad$
    DownLoad$ = DownLoad$ + "\"
    HTMLfromTEXT$ = HTMLfolders$ + "!HTML_FROM_TEXT"    '| converted from text HTML files
    MKDIR HTMLfromTEXT$
    HTMLfromTEXT$ = HTMLfromTEXT$ + "\"
    HoldFiles$ = HTMLfolders$ + "@HTML_HOLD_FILES"    '| converted from text HTML files
    MKDIR HoldFiles$
    HoldFiles$ = HoldFiles$ + "\"
    WebberTextBackup$ = WebberText$ + "@WEBBER_TEXT_BACKUP"
    MKDIR WebberTextBackup$
    WebberTextBackup$ = WebberTextBackup$ + "\"
    MKDIR HTMLfolders$ + "@WORK"
    MainDummy$="MainDummy.txt"
' ___________________________________________________________
'/a.Create Backup Folder From Month Date
    s$ = DATE$: mm$ = MID$(s$,1,2)
    Dy$ = MID$(s$,4,2): yy$ = MID$(s$,7)
    Day$ = yy$ + "-" + mm$ + "-" + Dy$
    'create year folder
    YearFolder$ = WebberTextBackup$ + yy$ + "TEXT_BACKUP"
    MKDIR YearFolder$
    YearFolder$ = YearFolder$ + "\"
    'create Mnoth Folder
    WebberTextBackup$ = WebberTextBackup$ + mm$ + "_TW_BACKUP"
    MKDIR WebberTextBackup$
    WebberTextBackup$ = WebberTextBackup$ + "\"
' ___________________________________________________________
'/a.Clear Temporary Files
    005: NRL = 005
    OPEN trash$ + "temp.tmp" FOR OUTPUT AS 1: CLOSE 1
    KILL trash$ + "*.*"
    '-------------------------------------------------------
    OPEN HTMLfolders$ + "temp.tmp" FOR OUTPUT AS 1: CLOSE 1
    KILL HTMLfolders$ + "*.tmp"
    '-------------------------------------------------------
    010: NRL = 010
    OPEN HTMLfromTEXT$ + "tmp.tmp" FOR OUTPUT AS 1: CLOSE 1
    KILL HTMLfromTEXT$ + "*.*"
    werePROB$ = Control$  + "!Problem.txt"
    '---------------------------------------
    015: NRL = 015
    OPEN werePROB$ FOR OUTPUT AS 1: CLOSE 1
    KILL werePROB$
    '------------------------------------------------------------
    OPEN Control$+"file_not_found.txt" FOR OUTPUT AS 1
        PRINT#1,"'From Folder'",",'To Folder'",",'Not Found'"
        PRINT#1,"=============","=============","============="
    CLOSE 1
    OPEN Control$+"file_found.txt" FOR OUTPUT AS 1
        PRINT#1,"'From Folder'",",'To Folder'",",'File Found'"
        PRINT#1,"=============","=============","============="
    CLOSE 1

' _______________________________________
'/a.Dim Arrays and Random Records
    DIM DUMMYrecord AS GLOBAL recorded
    DIM record(0 TO 2) AS GLOBAL recorded
    REDIM Pathit(PTH) AS GLOBAL STRING
    REDIM Problems(100) AS GLOBAL STRING
    ERRCLEAR
    ON ERROR GOTO error_trap
' ______________________________________
'/a.can be used to control user
    test_checksum:
    expire$ = TRIM$(expire$)
    IF LEN(expire$) = 0 THEN expire$ = day$
' __________________________________________
'/a.Serial Number Generator
    n1$ = "5"                 '| version number
    x$ = "abcdefghijklmnopqrstuvwxyz"
    Image
    '------------------------
    020: NRL = 020
    IF LEN(TRIM$(Owner$))=0 THEN Owner$="Hugh Polley"
    FOR x = 1 TO LEN(owner$)
        Image
        '------------------
        FOR y=1 TO LEN(x$)
            IF LCASE$(MID$(owner$,x,1)) = LCASE$(MID$(x$,y,1)) THEN
                n$ = "0" + TRIM$(STR$(y)): n$ = RIGHT$(n$,2)
                EXIT FOR
            END IF
        NEXT y
        IF y> LEN(x$) THEN n$ = "-"
        n1$ = n1$ + n$
    NEXT x
    generator$=n1$
'  _________________________________________________________
' /                                                         \
'< b.          Main Operating Routines                      >
' \_________________________________________________________/
'
    Frameit(-1,ForeRG,BackRG)
    LOCATE 4,5
    'Colate_AS2
    ON ERROR RESUME NEXT
    '''''''''''''' test program '''''''''''''''''''''''''''''''''
    'KILL Control$+"*.css"   'remove after tests
    'KILL WebberText$+"*.css"   'remove after tests
    'KILL HTMLfolders$+"*.css"   'remove after tests
    'KILL Control$+"html_from_text.txt"   'remove after tests
    'KILL WebberText$+"*.txt"   'remove after tests
    'KILL HTMLfolders$+"*.txt"   'remove after tests
    'KILL HTMLfromTEXT$+"*.txt"  'remove after tests
    'If you remove Control$+"*.css" then have the other CSS replaced by the new one"
    '? "'here remove KILL after tests and fix SHELL!": WAITKEY$
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' _________________________________________________________
    '/c1.SetUP Global Variables
    link = 0
    prob = 0
    Spacing$="&ensp;"
    OPEN Control$ + Prm_name$ + "_PROBLEM.txt" FOR OUTPUT AS 1:CLOSE 1
    KILL Control$ + Prm_name$ + "_PROBLEM.txt"
    ' __________________________________________________
    '/Program Log for Error Checks and Program structure
    OPEN New_Prg_Name$+".LOG" FOR OUTPUT AS 20
    '___________________________________________________________
    LOCATE 4,5: PRINT "  Create Default or Keep DOCUTYPE Files"'\
    LOCATE  ,5: PRINT "  ====================================="
    LOCATE  ,5
    '\__________________________________________________________/

    'b.'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    Sfile$ = Control$+"!Setup.TXT": num=1
    Exists = Create_Setup_File(Sfile$)
    IF ISTRUE Exists THEN x$=" Created!" ELSE x$=" Exists!"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+Sfile$+"] "+x$: LOCATE ,5
    'b.'''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20): Cast=40: PX=16: x$="width PX"
    Exists = getarray(x$,sxx$(),Sfile$)
    IF LEN(TRIM$(sxx$(2)))>0 THEN Cast = VAL(sxx$(2))
    IF LEN(TRIM$(sxx$(3)))>0 THEN PX = VAL(sxx$(3))
    PRElength = 100          '| Controls line length of HTML code
    'b.'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20): Author$ = "Hugh Polley"
    x1 = getarray("Author",sxx$(),Sfile$)
    IF ISTRUE x1 THEN
        Image
        '---------------
        025: NRL = 025
        FOR x = 2 TO 20
            IF LEN(LTRIM$(sxx$(x))) > 0 THEN Author$=sxx$(x)
        NEXT x
    END IF
    Author$ = single_quote_remove(Author$)
    'b.''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20): descript$ = ""
    x1 = getarray("spider description",sxx$(),Sfile$)
    IF ISTRUE x1 THEN
        Image
        '---------------
        FOR x = 2 TO 20
            IF LEN(LTRIM$(sxx$(x))) > 0 THEN
                descript$ = descript$ + "," + TRIM$(sxx$(x))
            END IF
        NEXT x
    END IF
    descript$ = single_quote_remove(descript$)
    'b.'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20) '| serial number from version, registration: name & Date
    x1 = getarray("spider keywords",sxx$(),Sfile$)
    IF ISTRUE x1 THEN
        Image
        '---------------
        030: NRL = 030
        FOR x = 2 TO 20
            IF LEN(LTRIM$(sxx$(x))) > 0 THEN
                keyword$ = keyword$ + "," + TRIM$(sxx$(x))
            END IF
        NEXT x
    END IF
    keyword$ = single_quote_remove(keyword$)
    IF LEN(TRIM$(keyword$))>0 THEN keyword$=n1$+keyword$
    'b.'''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20)
    Exists = getarray("index html",sxx$(),fileF$)
    firstimage$ = "firstimage.jpeg"
    y$ = TRIM$(sxx$(3))
    IF LEN(y$) > 0 THEN firstimage$ = LCASE$(y$)
    'b.''''''''''''''''''''''''''''''''''''''''''''''''''''''
    file$ = Control$ + "DOCTYPE_DECLARE.tmp": num=num+1
    Exists = DOCTYPE_DECLARE (file$)
    IF ISTRUE Exists THEN
        x$=" Created!"
        FILECOPY File$,WebberText$+"DOCTYPE_DECLARE.tmp"
    ELSE
        file$ = WebberText$+"DOCTYPE_DECLARE.tmp"
        Exists = DOCTYPE_DECLARE (file$)
        IF ISTRUE Exists THEN
            FILECOPY Control$ + "DOCTYPE_DECLARE.tmp",File$
            x$=" Created!"
        ELSE
            x$=" Exists!"
        END IF
    END IF
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists));". ["+file$+"] "+x$: LOCATE ,5
    'b.''''''''''''''''''''''''''''''''''''''''''''''''''''''
    file$ = Control$ + "DOCTYPE_heading.tmp": num=num+1
    Exists = DOCTYPE_heading (file$)
    IF ISTRUE Exists THEN
        x$=" Created!"
        FILECOPY File$,WebberText$+"DOCTYPE_heading.tmp"
    ELSE
        file$ = WebberText$+"DOCTYPE_heading.tmp"
        Exists = DOCTYPE_heading (file$)
        IF ISTRUE Exists THEN
            FILECOPY Control$+"DOCTYPE_heading.tmp",File$
            x$=" Created!"
        ELSE
            x$=" Exists!"
        END IF
    END IF
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists));". ["+file$+"] "+x$: LOCATE ,5
    'b.'''''''''''''''''''''''''''''''''''''''''''''''''''''''
    file$ = Control$ + "LICENSE.TXT"
        disclaim = license(file$): num=num+1: Exists=disclaim
    IF ISFALSE Exists THEN
        x$="License Exists!"
    ELSE
        x$="License Created for Tail!"
        FILECOPY File$, WebberText$ + LCASE$("LICENSE.TXT")
    END IF
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists));". ["+file$+"] "+x$: LOCATE ,5
    'b.''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' _______________________________________________
    '/Load and Adjust Style Sheets
    FOR EX=1 TO 3
        num=num+1
        IF EX=1 THEN file$ = "tail.css"
        IF EX=2 THEN file$ = "head.css"
        IF EX=3 THEN file$ = "main.css"
        '''''''''''''''''''''''''''''''''''''
        Exists = CREATE_STYLESHEETS (file$)
        IF ISTRUE Exists THEN
            x$=" Created!"
            FILECOPY Control$+File$,WebberText$+File$
        ELSE
            TRY
                OPEN WebberText$+File$ FOR INPUT AS 1: CLOSE 1
                x$=" Exists!"
            CATCH
                FILECOPY Control$+File$,WebberText$+File$
                x$=" Created!"
            END TRY
        END IF
        IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
        PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5
        '? ' ,TRIM$(STR$(Exists));". ["+file$+"] "+x$+"!"
    NEXT EX
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ON ERROR RESUME NEXT
    num=num+1
    FILECOPY (WebberText$ + "tail.css"),(HTMLfolders$ + "tail.css")
    FILECOPY (WebberText$ + "head.css"),(HTMLfolders$ + "head.css")
    FILECOPY (WebberText$ + "main.css"),(HTMLfolders$ + "main.css")
    FILECOPY (WebberText$ + "tail.css"),(HTMLfromTEXT$ + "tail.css")
    FILECOPY (WebberText$ + "tail.css"),"tail.css"
    x$="Moved CSS files from ["+WebberText$+"]"
    y$="to Root, and ["+HTMLfromTEXT$+"]"
    PRINT RTRIM$(STR$(num))+". "+ x$: LOCATE ,5
    PRINT SPACE$(3)+y$: LOCATE ,5
    ' _____________________________________________
    '/c.Creating Instruction Manual
    file$ = Control$ + "html_from_text.txt": num=num+1
    Exists = instructions(file$)
    x$="INSTRUCTION MANUAL!"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    045: NRL = 045
    ' _____________________________________________
    '/c.Creating help Manual
    file$ = "!Quick Help and Move_Graphics.txt": num=num+1
    Exists = Help_File(file$)
    x$="Quick Help File!"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+file$+"] ": LOCATE ,5
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' ______________________________________________
    '/                                              \
    IF Delay_for_action (10) = "Q" THEN END
    CALL Frameit (-1,ForeRG,BackRG)
    '\______________________________________________/

    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' __________________________________________________
    '/c.Moving IMAGE FILES
    'LOCATE 5,5: PRINT "From Root Folder Move_Graphics is Adjusting Graphic Files !"
    x$="Move_Graphics.EXE"
    SHELL x$,1', EXIT TO exitvar& '| executes separate .exe with focus, exitvar& is returned exit value
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ERRCLEAR
    ON ERROR GOTO error_trap
    ' ___________________________________________
    '/d1.Creating Three Frame File
    ON ERROR GOTO error_trap
    '''''''''''''''''''''''''''''''''''''''''''''
    ' ______________________________________________
    '/                                              \
    Frameit (-1,ForeRG,BackRG)
    '\______________________________________________/
    '_______________________________________________________
    LOCATE 4,7: PRINT "Create Default or Keep Frame Structures"
    LOCATE  ,7: PRINT "======================================="
    LOCATE 7,5
    '\______________________________________________________/

    file$ = WebberText$ + LCASE$("Frame.HTML"): num=num+1
    Exists = hFrame_THREE(file$)
    x$="Frame Structures"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5
    '? ' ,TRIM$(STR$(Exists));". ["+file$+"] "+x$+"!"

    ' __________________________________________________
    '/d2.Default Text of Head Frame File in Text Webber
    file$ = "head": num=num+1
    Exists = hFrame_Head(file$)
    x$="Head TCM File"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5
    '? ' ,TRIM$(STR$(Exists));". ["+file$+"] "+x$+"!"
    ' ___________________________________________
    '/d3.Default Tail Frame File
    file$ = "tail": num=num+1
    Exists = hFrame_Tail(file$)
    x$="Tail Frame to HTML File"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5
    '? ' ,TRIM$(STR$(Exists));". ["+file$+"] "+x$+"!"
    ' ___________________________________________
    '/d4.Default Index HTML Start up PAGE
    file$ = LCASE$("Index.HTML"): num=num+1
    Exists = Create_Index_Page(file$)
    x$="Cover Page Check or Creation"
    IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
    PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5
    '? ' ,TRIM$(STR$(Exists));". ["+file$+"] "+x$+"!"
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' __________________________________________________________
    '/'d5 Transfer Root ZIP files to DOWNLOAD
    zip%=FREEFILE
    file$=Trash$+"ZipFileList.tmp"
    OPEN file$ FOR OUTPUT AS zip%: CLOSE zip%
    '''''''''''''''''''''''''''''''''''''''''
    Exists = fetch_file( "/",file$,".zip")
    ''''''''''''''''''''''''''''''''''''''''
    IF Exists THEN
        CALL Sort_File(file$,last)
        OPEN file$ FOR INPUT AS zip%
        DO UNTIL EOF(zip%)
            LINE INPUT#zip%,xx$: xx$=TRIM$(xx$)
            IF LEN(xx$)>0 THEN
                xx$=xx$+".zip"
                FILECOPY xx$,DownLoad$+xx$
            END IF
        LOOP
        CLOSE zip%
        num=num+1
        x$="Transfer ZIP to DOWNLOADS"
        IF num*Exists<0 THEN Exists=num*Exists ELSE Exists=num
        PRINT RTRIM$(STR$(Exists))+". ["+file$+"] "+x$: LOCATE ,5

    END IF

    x$ = Delay_for_action(15)
    IF x$="Q" THEN CLOSE: END

' _________________________________________________________
'/                                                         \
'<             OPERATION ON TEXT FILES ROUTINES              >
'\_________________________________________________________/
' _________________________________________________________
' ______________________________________________________________
'/E1.load html text files to be processed into file filelist.tmp
    050: NRL = 050
    OPEN Trash$ + "IndexHead.tmp" FOR OUTPUT AS 4: CLOSE 4
    txFile$ = trash$ + LCASE$("FileList.tmp")
    '---------------------------------------
    OPEN txFile$ FOR OUTPUT AS 11: CLOSE 11
    y = fetch_file(WebberText$,txFile$,".txt")
    055: NRL = 055
    CALL Sort_File(txFile$,last)
    IF ERN > 0 THEN ERROR ERN
' __________________________________
'/E2.Set TCM 1 to 9 Basic HTML Tags
    '<pre> exist then TAG(0)=TAG(0)+1
    '</pre> exist then TAG(0)=TAG(0)-1
    'USED BY TCM 1,2
    REDIM TG(10) AS GLOBAL STRING
    TG(1) = "div>"
    TG(2) = "div>"
    TG(6) = "pre>"
    TG(7) = "p>"
    TG(8) = "strong>"
    TG(10) = "div>"
' _________________________________________________________
'/                                                         \
'<             START PROCESSING TEXT FILES                   >
'\_________________________________________________________/
' _________________________________________________________
' _______________________________________________________
'/01a.Process opFile$ text file listed in txFile$
    060: NRL = 060
    OPEN Control$  + "file_required.txt" FOR OUTPUT AS 1: CLOSE 1
    OPEN txFile$ FOR INPUT AS 11
        Image
        '----------------
        DO UNTIL EOF(11)
            Frameit(-1,ForeRG,BackRG)
            LOCATE 5,5
            htmlFile$=""
            New_Dir$=""
            opFile$=""
            Pathit(1)=""
            Image
            '-----------------------------------
            DO UNTIL EOF(11)    '| control loop
                INPUT#11,opFile$
                opFile$ = TRIM$(LCASE$(opFile$))
                head_int = 0
                IF LEN(opFile$) < 1 THEN EXIT DO
                x$="Processing Text File: "+opFile$
                LOCATE ,5: PRINT x$
                LOCATE ,5: PRINT STRING$(LEN(x$),"=")
                File$ = opFile$
                Pathit(1)=backup_tab(File$)
                IF Pathit(1)="-1" THEN EXIT DO    '| Problem with file$ EXIT
                ' _________________________________________________
                '/01b.Index Files set up
                mIndex$ = trash$ + "mIndex.tmp"
                sIndex$ = trash$ + "sIndex.tmp"
                link = 2: Index = 0: existindex = 0
                '-------------------------------------
                065: NRL = 065
                OPEN mIndex$ FOR OUTPUT AS 9: CLOSE 9
                OPEN sIndex$ FOR OUTPUT AS 9: CLOSE 9
                ' _________________________________________________
                '/01c.test opFile$ file for no Data add empty record
                yes = 0
                070: NRL = 070
                TRY
                    '---------------------------
                    OPEN Pathit(1) FOR INPUT AS 3
                        Image
                        '---------------
                        DO UNTIL EOF(3)
                            LINE INPUT#3,x$
                            '---------------
                            IF LEN(TRIM$(x$)) > 0 THEN
                                yes = -1
                                EXIT DO
                            END IF
                        LOOP
                    CLOSE 3
                    '-------
                CATCH
                    CLOSE 3
                    '-------
                END TRY
                IF yes = 0 THEN
                    Topic$ = "1 IN -> [" + Pathit(1) + "]-> Empty File"
                    Problems(1) = "1-No text information in file!"
                    Problems(2) = "Check " + YearFolder$ + " for last good copy!"
                    Problems(3) = "Check " + WebberTextBackup$ + " for last original copy!"
                    Problem_file (Topic$, 3)
                    EXIT DO
                END IF
                ' _______________________________________
                '/take out spaces from name
                DO
                    y = INSTR(opFile$,SPACE$(1))
                    IF y > 0 THEN MID$(opFile$,y,1) = "_"
                LOOP UNTIL y = 0
                ' ________________________________________________
                '/01d.Create Temporary File For Processed Output
                LOCATE ,5: PRINT  "1. Writing results to Temporary File!"
                tpFile$ = trash$ + "process.tmp"
                '----------------------------
                075: NRL = 075
                OPEN tpFile$ FOR OUTPUT AS 2
                OPEN goodCopy$+opFile$+".txt" FOR OUTPUT AS 8
                    ' __________________________________________________________
                    '/01e.Start Processing Text in WebberText$ + opFile$ + ".txt"
                    080: NRL = 080
                    OPEN Pathit(1) FOR INPUT AS 3 '| @TRASH$ + opFile$ + ".tmp"
                    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                        ' ____________________________
                        '/01f.Setup Control variables
                        REDIM nest(100) AS GLOBAL LONG
                        nNest = 0
                        action$ = ""
                        TCM = 0
                        format = 0
                        Lnumber = 0
                        par$ = ""
                        New_Dir$ = HTMLfromTEXT$
                        prelength = 100
                        ' ______________________________
                        '/02a.start of line processing
                        Image
                        DO UNTIL EOF(3)
                            LINE INPUT#3,hPar$
                            '------------------
                            Lnumber = Lnumber + 1
                        LOOP UNTIL LEN(TRIM$(hPar$)) > 0
                        ' ___________________________________
                        '/02b.start of Control processing loop
                         DO UNTIL EOF(3): Image 'hPar$ goes once unless created in third
                            tcm_first_set$ (hPar$,TCM) 'loads a line tests it
                            tcm_second_set$(TCM) 'closes non index lines, executes 5TCM
                            tcm_third_set$(TCM)  'closes indexes
                        LOOP UNTIL EOF(3)
                    CLOSE 3
                    '-------
                    FixLength$
                    par$=RTRIM$(par$)
                    Good$=RTRIM$(Good$)
                    IF LEN(Par$) >0 THEN PRINT#2,Par$
                    IF LEN(Good$)>0 THEN PRINT#8,Good$
                    Par$ = "": Good$ = ""
                CLOSE 8
                '-------
                IF nNest > 0 THEN
                    x$="": bad=0
                    Image
                    '--------------------
                    085: NRL = 085
                    FOR x = -nNest TO -1
                        IF Nest(-x)>0 THEN
                            IF Nest(-x)>2 THEN bad=-1
                            PRINT#2,"</" + TG(Nest(-x))
                            x$=x$+STR$(Nest(-x))+": </" + TG(Nest(-x))
                        END IF
                    NEXT x
                    IF ISTRUE bad THEN
                        Topic$ ="2 IN -> " + opFile$ + "-> NESTING CORRUPT"
                        Problems(1) = "Attempt to close Nests Made!"
                        Problems(2) = "Unclosed Nests Record=[ " + x$ + " ]"
                        Problems(3) = "Bad Nesting can result in unstable HTML!"
                        problem_file(Topic$,3)
                    END IF
                END IF
                CLOSE 2
                '-------
                'end of direct processing and good copy
                'IF ISTRUE existindex THEN const_index_linkage (Par$,-1)
                const_index_linkage (Par$,-1)
                ' ________________________________________
                '/ add sub index to index and close index
                LOCATE ,5: PRINT  "2. Completed processing Text Code Marks!"
                EXIT DO
            LOOP
            ' ______________________________________________
            '/create default or keep html !head
            LOCATE ,5: PRINT  "3. Sub Index added to Main Index!"
            ' __________________________________________________
            '/clear html file and add heading
            LOCATE ,5: PRINT  "4. Markup Heading created!"
            xx$ = LCASE$(opFile$) + ".html"
            ''''''''''''''''''''''''''''''''''''''
            IF New_Dir$ = HTMLfromTEXT$ THEN
                htmlFile$ = HTMLfromTEXT$ + xx$
            ELSE
                htmlFile$ =New_Dir$+xx$
            END IF
            ' _______________________________________
            '/Change file name spaces to underscore
            DO
                y = INSTR(htmlFile$,SPACE$(1))
                IF y > 0 THEN MID$(htmlFile$,y,1) = "_"
            LOOP UNTIL y = 0
            ' ___________________________________________
            '/Clear HTML File and add sections
            LOCATE ,5: PRINT  "5. Creating: "; htmlFile$
            ' ________________________________________________________
            '/Converted Webber Text to be Outputted as htmlFile$ HTML
            'Create HTML Doc Heading if it is not in trash$
            IF ISFALSE head_int THEN
                090: NRL = 090
                CALL heading(opFile$)
                IF ERN > 0 THEN ERROR ERN
            ELSE
                CALL internal_head (opFile$)
                IF ERN > 0 THEN ERROR ERN
            END IF
            '------------------------------
            095: NRL = 095
            OPEN htmlFile$ FOR OUTPUT AS 2
                'Add HTML 'Head' from @TextWebber + Last_head.txt
                ' ____________________________________________________
                '/Create HTML Document Head For Index or OpFile Index
                'Document Head For OpFile Index
                'create HTML for page with top index
                    100: NRL = 100
                    OPEN trash$ + "last_head.HTML" FOR INPUT AS 3
                        Image
                        '---------------
                        DO UNTIL EOF(3)
                            LINE INPUT#3,x$
                            '---------------
                            PRINT#2,x$
                        LOOP
                    CLOSE 3
                    '-------
                ' __________________________________
                '/add Body Style ID to HTML File
                IF LCASE$(opFile$) = "head" THEN
                    PRINT#2,"<body id='headbody'>"
                ELSE
                    PRINT#2,"<body id='tailbody'>"
                END IF
                index = 0
                IF LCASE$(opFile$) <> "head" THEN
                    Ind&=2: x1=0
                    ' ___________________________________________
                    '/CREATE DIVISIONS
                    105: NRL = 105
                    OPEN mIndex$ FOR INPUT AS 3
                        IF NOT(EOF(3)) THEN
                            LINE INPUT#3,x$    '| info or printout line
                            '-------------------------------------------
                            PRINT#Ind&,x$
                            Image
                            '---------------
                            DO UNTIL EOF(3)
                                LINE INPUT#3,ok$
                                '----------------
                                ok$ = TRIM$(ok$)
                                IF LEN(ok$) > 0 THEN
                                    PRINT#Ind&,ok$
                                END IF
                            LOOP
                            IF LEFT$(TRIM$(ok$),4)="<dt>" THEN PRINT#Ind&,"<dd></dd>"
                            PRINT#Ind&,"</dl>"
                        END IF
                    CLOSE 3
                    '-------
                END IF
                IF LCASE$(opFile$) <> "head" THEN PRINT#2,"<div class='text'>"
                ' _______________________________________
                '/Add body of converted text to HTML
                110: NRL = 110
                OPEN tpFile$ FOR APPEND AS 3: CLOSE 3
                '''''''''''''''''''''''''''''''''''''
                OPEN tpFile$ FOR INPUT AS 3
                    Image
                    '---------------
                    DO UNTIL EOF(3)
                        LINE INPUT#3,ok$
                        '----------------
                        PRINT#2,ok$
                    LOOP
                CLOSE 3
                '-------
                'close tag
                IF LCASE$(opFile$) <> "head" THEN PRINT#2,"</div>"
                ' ________________________________
                '/close document
                PRINT#2,"</body>"
                PRINT#2,"</html>"
            CLOSE 2
            '-------
            115: NRL = 115
            LOCATE ,5: PRINT  "6. "; htmlFile$;" Created!"
            ''''''''''''''''''''''''''''''''''''''''''''
            x$ = Delay_for_action(10)
            IF x$ = "Q" THEN CLOSE: END
            ' _______________________________________________________
            '/                                                       \
           '<             HTML DOCUMENT COMPLETED                     >
            '\_______________________________________________________/
        LOOP UNTIL EOF(11)
    CLOSE 11
    '--------
    opFile$=""
    ' ______________________________________________
    '/Put together Head Document
'  _________________________________________________________
' /                                                         \
'<           MOVING FILES RECORDING PROBLEMS                 >
' \_________________________________________________________/
Frameit(-1,ForeRG,BackRG)   'x5
LOCATE 5,5
x$="Moving Required HTML_HOLD files to Folders!
LOCATE ,5: PRINT x$
LOCATE ,5: PRINT STRING$(LEN(x$),"=")
120: NRL = 120

' ____________________________________________________________
'/copy style sheets from Webber Text Folder to directories
x5=0 'trouble with finding files
REDIM YYY$(10)
YYY$(1)="hForeground"
YYY$(2)="tForeground"
YYY$(3)="mForeground"
YYY$(4)="iForeground"
'''''''''''''''''''''''''''''''
fileF$= Control$+"!SetUp.TXT"
'''''''''''''''''''''''''''''''''''''''
FOR XX=1 TO 4
    REDIM sxx$(20): theImage$="noimage.jpeg"    'setup variables
    x1 = getarray(YYY$(XX),sxx$(),fileF$)       'get sxx$(3) image name
    IF x1 THEN                                  'name found
        theImage$=sxx$(3)
        '? ' ,xx,theImage$
        TRY                                     'attempt to move image
           FILECOPY HoldFiles$+theImage$,HTMLfromTEXT$+theImage$
        CATCH
           '-----------------------------------------------------
           '? ' ,0,Control$+"file_not_found.txt ,";HoldFiles$;",";HTMLfromTEXT$;" , ";theImage$
            OPEN Control$+"file_not_found.txt" FOR APPEND AS 10
                PRINT#10,HoldFiles$,","+HTMLfromTEXT$,","+theImage$
                x5=-1
            CLOSE 10
            '--------
        END TRY
    END IF
NEXT xx
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
125: NRL = 125
fd = FREEFILE: fd1 = FREEFILE: fd2 = FREEFILE: fd3 = FREEFILE
' ____________________________________________________________
'/'HTML_FILES Sub Folder Names written to Control/directory.txt
130: NRL = 130
CALL get_dir(HTMLfolders$)
IF ERN > 0 THEN ERROR ERN
' ___________________________________________________________________
'/File names in Folder HTML_HOLD Written to Control/files_in_hold.txt
hdFile$ = Control$ + LCASE$("files_in_hold.txt")
hold = fetch_all(HoldFiles$,hdFile$)
flFile$ = Control$ + LCASE$("files_in_folders.txt")
hold = fetch_all(HTMLfolders$,flFile$)
' ___________________________________________________________
'/open each HTML_FILES Folder check for required file
'1. move css file there
'2. move required file there

'----------------------------------------------
135: NRL = 135
OPEN Trash$ + "directory.txt" FOR APPEND AS fd
    'add HTML Folders to List of Folders to draw from Hold Folder
    PRINT#fd,LEFT$(HTMLfolders$,LEN(HTMLfolders$)-1)
CLOSE fd
'--------
'---------------------------------------------
140: NRL = 140
OPEN Trash$ + "directory.txt" FOR INPUT AS fd
    file2$ = WebberText$ + "tail.css"
    Image
    '----------------
    DO UNTIL EOF(fd)
        141: NRL = 141
        INPUT#fd,FolderName$
        142: NRL = 142

        'were$ equals folder name location
        were$ = HTMLfolders$ + FolderName$ + "\"
        IF HTMLfolders$ = FolderName$ + "\" THEN were$ = HTMLfolders$
        '1. copy css file to directory
        143: NRL = 143

        TRY
            FILECOPY file2$,were$ + "tail.css"
        CATCH
            GOTO 184
        END TRY
        144: NRL = 144
        ' ____________________________________________________________
        '/check all HTML files in folder were$ for required GIF Files
        wFile$ = trash$ + LCASE$("Folders_HTMLFiles.tmp")
        y = fetch_file(were$,wFile$,".html")
        '----------------------------------------
        145: NRL = 145
        OPEN wFile$ FOR APPEND AS fd2: CLOSE fd2
        OPEN wFile$ FOR INPUT AS fd2
            folderHTML$ = ""
            Image
            '-----------------
            DO UNTIL EOF(fd2)
                146: NRL = 146
                INPUT#fd2,folderHTML$    '| html file name
                IF LEN(LTRIM$(folderHTML$)) <> 0 THEN
                    ' _________________________________________________________
                    '/check list of image GIF files required for folder's name
                    fileF$ = Control$ + "file_required.txt"
                    '----------------------------------------
                    150: NRL = 150
                    OPEN fileF$ FOR APPEND AS fd1: CLOSE fd1
                    OPEN fileF$ FOR INPUT AS fd1
                    151: NRL = 151
                        gif$ = "": Html$ = ""
                        Image
                        '-----------------
                        DO UNTIL EOF(fd1)
                            'get HTML file and gif names
                            INPUT#fd1,Html$,gif$
                            152: NRL = 152
                            ' ________________________________________
                            '/is GIF required by Html file Name
                            'required gif$ found
                            IF LCASE$(folderHTML$) = LCASE$(Html$) THEN
                                153: NRL = 153
                                ' ______________________________________________
                                '/check HTML_HOLD for existence of file
                                'If exist copy to were$
                                155: NRL = 155
                                OPEN hdFile$ FOR APPEND AS fd3: CLOSE fd3
                                OPEN hdFile$ FOR INPUT AS fd3
                                156: NRL = 156
                                    x3 = 0
                                    Image
                                    '-----------------
                                    DO UNTIL EOF(fd3)
                                        157: NRL = 157
                                        INPUT#fd3,gif2$
                                        IF LCASE$(gif$) = LCASE$(gif2$) THEN
                                            x3 = -1
                                            EXIT DO
                                        END IF
                                    LOOP
                                CLOSE fd3
                                158: NRL = 158
                                '---------
                                x4=0
                                IF x3=0 THEN
                                    '-----------------------------------------
                                    160: NRL = 160
                                    OPEN flFile$ FOR APPEND AS fd3: CLOSE fd3
                                    OPEN flFile$ FOR INPUT AS fd3
                                        Image
                                        '-----------------
                                        DO UNTIL EOF(fd3)
                                            INPUT#fd3,gif2$
                                            IF LCASE$(gif$) = LCASE$(gif2$) THEN
                                                x4 = -1
                                                EXIT DO
                                            END IF
                                        LOOP
                                    CLOSE fd3
                                    '---------
                                END IF
                                165: NRL = 165
                                'IF RIGHT$(LCASE$(folderHTML$),5) = "dummy" THEN
                                '    xx$ = were$ + folderHTML$ + ".html"
                                '    TRY
                                '        'KILL xx$ 'here
                                '    CATCH
                                '    END TRY
                                'END IF
                                IF ISTRUE x3 THEN
                                    '-------------------------------------------------
                                    170: NRL = 170
                                    FILECOPY HoldFiles$ + gif$ ,were$ + gif$
                                    '--------
                                    OPEN Control$ + "file_found.txt" FOR APPEND AS 10
                                        x$ = HoldFiles$ + "," + were$ + "," + gif$
                                        PRINT#10,x$
                                    CLOSE 10
                                    '--------
                                ELSEIF ISTRUE x4 THEN
                                    175: NRL = 175
                                    x$ = LCASE$(HTMLfolders$ + gif$)
                                    y$ = LCASE$(were$ + gif$)
                                    IF x$ <> y$ THEN
                                        FILECOPY x$,y$
                                    END IF
                                    '-------------------------------------------------
                                    OPEN Control$ + "file_found.txt" FOR APPEND AS 10
                                        x$ = HTMLfolders$ + "," + were$ + "," + gif$
                                        PRINT#10,x$
                                    CLOSE 10
                                    '--------
                                ELSE
                                    '-----------------------------------------------------
                                    180: NRL = 180

                                    OPEN Control$+"file_not_found.txt" FOR INPUT AS 10
                                        F=0
                                        DO UNTIL EOF(10)
                                            TRY
                                                INPUT#10,A$,B$,C$
                                                IF A$=FolderName$ AND B$=folderHTML$ AND C$=gif$ THEN
                                                    F=-1
                                                    EXIT DO
                                                END IF
                                            CATCH
                                            END TRY
                                        LOOP
                                    CLOSE 10
                                    IF F=0 THEN

                                        OPEN Control$+"file_not_found.txt" FOR APPEND AS 10
                                           PRINT#10,FolderName$,","+folderHTML$,","+gif$
                                           x5=-1
                                        CLOSE 10

                                        '--------
                                    END IF
                                   '----------------------
                                END IF
                                '-----------------------
                            END IF
                            '--------------------------
                        LOOP
                        '-----------------------------
                    CLOSE fd1
                    '--------

                END IF
                '--------------------------
            LOOP
            '-----------------------------
        CLOSE fd2
        '---------
        '---------------------------------------------------------------
        184: NRL=184 'bug in PB DIR$ can get File instead of Folder try catch branches here
    LOOP

CLOSE fd
'--------
185: NRL = 185

IF ISTRUE x5 THEN
    Topic$ =" SOME HTML REFERENCED FILES NOT FOUND!"
    Problems(1) = "Check ["+Control$+"file_not_found.txt" + "]
    Problems(2) = "for missing File's!"
    Problem_file(Topic$,2)
    attempt_restore_from_delete
END IF
IF ISTRUE x5 THEN
    Frameit(-1,ForeRG,BackRG)
    LOCATE 5,5
    LOCATE ,5: PRINT  "1. ERRORS! Moving Required HTML_HOLD files to Folders!"
ELSE
    LOCATE ,5: PRINT  "1. Moved Required HTML_HOLD files to Folders!"
END IF
LOCATE ,5: PRINT  "2. Setting Up Web Site!"
LOCATE ,5: PRINT  "3. Standard Files and Folders Created!"
file$ = HTMLfolders$ + LCASE$("Main.HTML")
'-------------------------------------

190: NRL = 190
OPEN file$ FOR APPEND AS 10: CLOSE 10
KILL file$
CALL hFrame_Main(file$)
IF ERN > 0 THEN ERROR ERN
LOCATE ,5: PRINT "4. ";file$;" Created!"
LOCATE ,5: PRINT "  "
LOCATE ,10: PRINT "'Q' TO EXIT WITHOUT SHOWING hTML!"
LOCATE ,5: PRINT "  "
x$ = Delay_for_action(10)
IF x$ = "Q" THEN END
IF ISTRUE disclaim THEN
    file2$ = WebberText$ + LCASE$("LICENSE.TXT")
    KILL file2$
    195: NRL = 195
    TRY
        file2$=WebberText$ + "trial_dummy.txt"
        KILL file2$
    CATCH
    END TRY
    FILECOPY HTMLfromTEXT$ + LCASE$("LICENSE.HTML"),HTMLfolders$ + LCASE$("tail.html")
    x$ = LCASE$("explorer.exe " + HTMLfolders$ + "frame.HTML")
    x& = SHELL(x$)
ELSE
    x$ = "explorer.exe " + LCASE$("index.html")
    x& = SHELL(x$)
END IF
CLOSE 20
EXIT FUNCTION
error_trap:
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    IF ERN =0 THEN ERN = ERR
    ON ERROR RESUME NEXT
    CLOSE
    MSG$ = ERROR$(ERN)
    ' _______________________________________
    '/Record Open Files - 20 is log file
    OPEN New_Prg_Name$+".LOG" FOR APPEND AS 20
    PRINT#20," ______________________________________________________"
    PRINT#20,"/FATAL ERROR REPORT ---- A BUG OR OPERATOR ERROR ------\"
    PRINT#20,"OPEN FILES AT TIME OF CRASH:"
    FOR x = 1 TO PTH '20
        a$="": a$ = FILENAME$(x)
        IF LEN(a$) > 0 THEN PRINT#20,a$
    NEXT x
    PRINT#20,"'''''''''''''''''''''''''''''''''''''''''''''''''''''''"
    PRINT#20," _____________________________________________"
    PRINT#20,"/SITUATION AT TIME OF CRASH:                  \"
    DIM Einfo$(10)
    Einfo$(1)="Program Name; ["+Prm_name$+"]"
    Einfo$(2)="Execution EXE Name: ["+New_Prg_Name$+"]"
    IF gProblem THEN Einfo$(3)="Software Spotted Problem" ELSE Einfo$(3)="Software did not spot problem"
    Einfo$(4)="ERROR Number: ["+STR$(ERN)+"]"
    Einfo$(5)="FUNCTION or SUB: ["+ERNerror$+"]"
    Einfo$(6)="LINE Close to Error: ["+STR$(NRL)+"]"
    FOR x=1 TO 6: PRINT#20, x,TRIM$(Einfo$(x)): NEXT x
    PRINT#20,"'''''''''''''''''''''''''''''''''''''''''''''''''''''''"
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    '-sh is red background +sh yellow background
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM Pathit (20)
    Pathit(0) = "SEE: ["+New_Prg_Name$+".LOG]"
    sh=1
    SELECT CASE ERN
        CASE 57,68,70,71,72,74,75,76

            Pathit(1) = "*****************************************************"
            Pathit(2) = "**           DRIVE OPERATING ERROR                 **"
            Pathit(3) = "*****************************************************"
            Pathit(4) = SPACE$(4)
            Pathit(5) = MSG$
            Pathit(6) = "After (" + TRIM$(STR$(NRL)) + " ) and [" + ERNerror$ + "]"
            n$ = Control$ + UCASE$(New_Prg_Name$+" DRIVE OPERATING ERROR")
            graphic_message(-sh,n$)


        CASE 52,53,54,55,57,58,61,62,63,64,67
            Pathit(1) = "*******************************"
            Pathit(2) = "**  FILE OR PATH ERROR       **"
            Pathit(3) = "*******************************"
            Pathit(4) = SPACE$(4)
            Pathit(5) = MSG$
            Pathit(6) = "After (" + TRIM$(STR$(NRL)) + " ) and [" + ERNerror$ + "]"
            n$ = Control$ + UCASE$(New_Prg_Name$+ "_FILE OR PATH ERROR")
            graphic_message(-sh,n$)

        CASE 151
            Pathit(1) = "**************************************************"
            Pathit(2) = "**           NOT Licensed User?                 **"
            Pathit(3) = "**************************************************"
            Pathit(4) = SPACE$(4)
            Pathit(5) = "Registration not found!"
            Pathit(6) = "Get a licensed product!"
            Pathit(7) = "DID YOU RUN THE WRONG STARTUP PROGRAM?"
            n$ = Control$ + UCASE$(New_Prg_Name$+ "_NOT_Licensed_User")
            graphic_message(-sh,n$)
            ' _____________________________________________________________________________________________
            '/operating errors
            '------------------
        CASE 154
            Pathit(1) = "  ___________________________"
            Pathit(2) = " /                           \"
            Pathit(3) = "<  OPERATING PROBLEMS FOUND   >"
            Pathit(4) = " \___________________________/"
            Pathit(5) = SPACE$(4)
            Pathit(6) = "(1) Open [" + Control$ + "Problem.TXT] with Notepad!"
            Pathit(7) = "(2) If there are a number of errors then"
            Pathit(8) = "    Open Indicated Files!"
            Pathit(9) = "(3) Fix errors outlined in Problem.TXT!"
            Pathit(10) ="    (spelling, omission errors, etc.)"
            Pathit(11) ="(4) Restart Program and continue the process!"
            sh=add_paths_to_Pathit(11)
            n$ = Control$ + UCASE$(New_Prg_Name$+ "_OPERATING_PROBLEMS_FOUND")
            graphic_message(sh,n$)

        CASE 155
            Pathit(1) = "  ___________________________"
            Pathit(2) = " /                            \"
            Pathit(3) = "<  SPECIAL CASE PROBLEMS FOUND >"
            Pathit(4) = " \____________________________/"
            Pathit(5) = SPACE$(4)
            Pathit(6) ="PROGRAM WILL HAVE ATTEMPTED TO FIX ERROR BEFORE ENDING!"
            Pathit(7) ="RE-RUN " + New_Prg_Name$ + " TO SEE RESULT OF REPAIR AFTER"
            Pathit(8) = "CHECKING [" + Control$ + "Problem.TXT] FOR INFORMATION"
            Pathit(9) = "AND CORRECTION SUGGESTIONS!"
            sh=add_paths_to_Pathit(9)
            n$ = Control$ + UCASE$(New_Prg_Name$+ "_SPECIAL_CASE_PROBLEMS_FOUND")
            graphic_message(sh,n$)

        CASE 156
            SLEEP 500
            ON ERROR RESUME NEXT
            NAME (New_Prg_Name$ + ".exe") AS "DELETE ME - VIRUS LIKE ACTIVITY DETECTED.EXE"
            Pathit(1) = "**************************************************"
            Pathit(2) = "**    VIRUS LIKE ACTIVITY DETECTED              **"
            Pathit(3) = "**************************************************"
            Pathit(4) = SPACE$(4)
            Pathit(5) = "A SUSPECT CHANGE DETECTED IN YOUR PROGRAM!"
            Pathit(6) = SPACE$(4) + Old_name$
            Pathit(7) = "USE [SHIFT][ DELETE] TO REMOVE THE PROGRAM!"
            Pathit(8) = "VIRUS CHECK YOUR COMPUTER THEN RE-INSTALL!"
            sh=8
            n$ = Control$ + UCASE$(New_Prg_Name$+ "_VIRUS_ACTIVITY_DETECTED")
            graphic_message(-sh,n$)

        CASE 157
            Pathit(1) = "*******************************"
            Pathit(2) = "**  LICENSE HAS EXPIRED     **"
            Pathit(3) = "*******************************"
            Pathit(4) = "This Program is time limited   "
            Pathit(5) = "and your licence peroid has    "
            Pathit(6) = "run out!                       "
            Pathit(7) = "Go to authors web site and get "
            Pathit(8) = "new demo or registered copy!   "
            sh = 8
            n$ = Control$ + UCASE$(New_Prg_Name$ + "_LICENSE_HAS_EXPIRED")
            graphic_message(-sh,n$)


        CASE 158
            Pathit(1) = "**************************************"
            Pathit(2) = "**         OPERATOR ERROR           **"
            Pathit(3) = "**************************************"
            Pathit(4) = "Direct Execution of Slave Program!    "
            Pathit(5) = "This program not called by its Master!"
            Pathit(6) = "or -----------------------------------"
            Pathit(7) = "   A Program Limit Reached:           "
            Pathit(8) = "   more than 250 letters per line?    "
            n$ = Control$ + UCASE$(New_Prg_Name$ + "_OPERATOR_ERROR")
            graphic_message(sh,n$)



        CASE ELSE:
            Pathit(1) = "*******************************"
            Pathit(2) = "**    UNEXPECTED ERROR!      **"
            IF ERN =200 THEN Pathit(2) = "**    UNANTICIPATED ERROR!   **"
            Pathit(3) = "*******************************"
            Pathit(4) = "If NOT Operator ERROR we have a Bug!"
            Pathit(5) = "Zip Program and its Folders!"
            Pathit(6) = "Create an Email with the programs"
            Pathit(7) = "Name eg. 'Trial4Webber' in Subject line."
            Pathit(8) = "Send to: hap2u@hotmail.com"
            Pathit(9) = "After (" + TRIM$(STR$(NRL)) + " ) and [" + ERNerror$ + "]"
            IF ERN =200 THEN
                n$ = N$ + "_UNANTICIPATED_ERROR"
            ELSE
                n$ =N$ + "_UNEXPECTED_ERROR"
            END IF
            graphic_message(-sh,n$)
    END SELECT
    FOR x=0 TO 20
        IF LEN(TRIM$(Pathit(x)))>0 THEN PRINT#20,TRIM$(Pathit(x))
    NEXT x
    PRINT#20,"'''''''''''''''''''''''''''''''''''''''''''''''''''''''"
END FUNCTION
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''' START OF SUBS AND FUNCTIONS '''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FUNCTION graphic_message(n,n$) AS LONG
    DESKTOP GET LOC TO x&, y&
    SLEEP 250
    LOCAL hWin AS DWORD
    GRAPHIC WINDOW (Trash$+Prm_name$), x&, y&, 600, 300 TO hWin
        GRAPHIC ATTACH hWin, 0, REDRAW
        GRAPHIC SET FOCUS
        GRAPHIC COLOR %BLACK,%BLACK

        GRAPHIC FONT "Lucida Console", 8, 1 '| courier GIVES UNIFORM FRONT
        SLEEP 100

        IF n<0 THEN '| A grave error has occured!
            GRAPHIC BOX (10, 10) - (600-10, 300-10),20,,%RED,0
            GRAPHIC COLOR %WHITE,%RED
        ELSE '| A Problem occured!
            GRAPHIC BOX (10, 10) - (600-10, 300-10),20,,%YELLOW 'yellow
            GRAPHIC COLOR %BLACK,%YELLOW'yellow
        END IF
        SLEEP 100

        GRAPHIC SET POS (10, 50)       '| A. start position referenced by STEP degignator
        GRAPHIC SET POS STEP (50, 1)   '| B. initial position from A.
        GRAPHIC PRINT Pathit(0)
        GRAPHIC PRINT ""
        SLEEP 100
        FOR x = 1 TO ABS(n)
            SLEEP 100
            GRAPHIC SET POS STEP (50+10, 1)  '| A + B gives printing in same coloum no line spacing
            GRAPHIC PRINT Pathit(x)
        NEXT x
        SLEEP 100
        GRAPHIC REDRAW
        SLEEP 100
        GRAPHIC SAVE (Trash$+n$ + ".bmp")
        '| Make Console active screen
        Delay_for_action (15)
    GRAPHIC WINDOW END '| LEFT for program termination
END FUNCTION
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

FUNCTION dProcess ALIAS "z1100" (Par$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1100
    ' ____________________________________________
    '/Add to Action File any Code marks Found
    DO
        IF LEN(TRIM$(Par$)) = 0 THEN EXIT DO
        Par$ = RTRIM$(Par$)
        x1 = INSTR(-1,Par$,":-")
        x2 = INSTR(-1,Par$,":+")
        x3 = LEN(Par$)
        x = ((x2>0) * (x2>x1)*x2) + ((x1>0) * (x1>x2)*x1)
        IF NOT(x3-x = 2 AND x>0) THEN EXIT DO
        'TCM found
        x$ = MID$(Par$,x+1)
        Par$ = LEFT$(Par$,x-1)
        y = VAL(x$)
        t = (y>-10 AND y<0) OR (y>0 AND y<10)
        IF t=-1 THEN
            action$ = ":" + x$ + TRIM$(action$)
        ELSE
            Topic$ = "4 IN -> " + opFile$ + "-> TCM(" + x$ + ") Number Error"
            Problems(1) = "Error occurred in Line Number" + STR$(Lnumber)
            Problems(2) = "line=[" + Par$ + "]"
            Problem_file (Topic$, 2)
        END IF
    LOOP UNTIL x=0
    action$ = TRIM$(Action$)
    ' _______________________________________
    '/order action marks
    IF LEN(action$) > 0 THEN
        order$ = ":+5:-5:-8:-7:-6:+6:+7:+8:-2:+2:-1:-4:+4:-3:+3:+1:+9:-9" 'picks from last to first
        action1$ = action$ + TRIM$(STR$(y1))
        action$ = ""
        last$ = ""
        Image
        '---------------------------------
        FOR x = 1 TO LEN(order$)-2 STEP 3
            x$ = MID$(order$,x,3)
            Image
            '-----------------------------------
            FOR y = 1 TO LEN(action1$)-2 STEP 3
                y$ = MID$(action1$,y,3)
                IF y$ = x$ THEN
                    action$ = action$ + x$
                    last$ = x$
                END IF
            NEXT y
        NEXT x
        action$ = TRIM$(action$)
        action1$ = action$
        x3 = LEN(action$)
        IF x3 = 0 THEN EXIT FUNCTION
        x1 = INSTR(-1,action$,":-")
        x2 = INSTR(-1,action$,":+")
        x = ((x2>0) * (x2>x1)*x2) + ((x1>0) * (x1>x2)*x1)
        IF x3-x = 2 THEN
            x$ = MID$(action$,x+1)
            y = VAL(x$)
            action$ = TRIM$(LEFT$(action$,x-1))
            t = (y>-10 AND y<0) OR (y>0 AND y<10)
            IF t THEN
                dProcess = y
            ELSE
                Topic$ = "5 IN -> " + opFile$ + "-> TCM (" + x$ + ") Error"
                Problems(1) = "Error occurred in Line Number" + STR$(Lnumber)
                Problems(2) = "line=[" + Par$ + "]"
                Problems(3) = "using marks [" + action1$ + "]"
                Problem_file (Topic$, 3)
            END IF
        END IF
    END IF
1100:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION fetch_file ALIAS "z1200" (were$,File$,fType$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1200


    LOCAL fd AS LONG
    LOCAL ff AS LONG,gg AS LONG, fPath AS STRING,hPath AS STRING, fName AS STRING, sBuf AS STRING
    ' __________________________________________________________
    '/construct directory path
    fPath = CURDIR$
    IF RIGHT$(fPath, 1) <> "\" THEN fPath = fPath + "\"
    fPath = fPath + were$     '| adjust path if needed
    IF RIGHT$(fPath, 1) <> "\" THEN fPath = fPath + "\"
    ' _________________________________________________
    '/open text files storage file
    ff =FREEFILE              '| Get free file handle
    '-----------------------------------------------------------------------------
    OPEN File$ FOR OUTPUT AS ff: CLOSE ff    '| clear directory file name records
    ' ___________________________________________
    '/search for file type and add to File
    fName = DIR$(fPath + "*" + fType$)
    fd=0
    WHILE LEN(fName)
        ff =FREEFILE          '| Get free file handle
        '-----------------------------------------------------------------
        OPEN File$ FOR APPEND AS ff    '| open file with directories name
            gg = INSTR(-1, fName, ".")    '| remove period extension
            IF gg THEN fName = LEFT$(fName, gg-1)
            fName = LTRIM$(RTRIM$(fName))
            IF LEN(fName) > 0 THEN
                fName = RTRIM$(fName)
                PRINT#ff,fName
                fd = -1
            END IF
        CLOSE ff              '| close file handle
        '------------------------------------------
        fName = DIR$          '| any more *.txt files
    WEND
    fetch_file = fd
1200:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
SUB heading ALIAS "z1300" (Title$)
    ' ___________________________________________
    '/completes heading started in backup
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 1300
'------------------
    ff = FREEFILE
    '-----------------------------------------------
    OPEN trash$ + "last_head.HTML" FOR APPEND AS ff
        IF opFile$ ="head" THEN
            PRINT#ff,"      <link rel='stylesheet' TYPE='text/css' media='screen' href='head.css'>"
        ELSE
            PRINT#ff,"      <link rel='stylesheet' TYPE='text/css' media='screen' href='tail.css'>"
        END IF
        PRINT#ff,"  </head>"
    CLOSE ff
    '--------
1300:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
FUNCTION CREATE_STYLESHEETS ALIAS "z1400" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1400
'------------------

ff = FREEFILE
TRY 'bring back after tests
    '-------------------------------------
    OPEN Control$ + file$ FOR INPUT AS ff
    CLOSE ff
    '--------
    EXIT FUNCTION
CATCH
    CLOSE ff
    '--------
    CREATE_STYLESHEETS = -1
END TRY


    fileF$ = Control$+"!setup.txt"
    mfile$ = "main.css"
    tfile$ = "tail.css"
    hfile$ = "head.css"
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20)
    IF file$ = mfile$ THEN y$ = "mForeground"
    IF file$ = tfile$ THEN y$ = "tForeground"
    IF file$ = hfile$ THEN y$ = "hForeground"
    x1 = getarray(y$,sxx$(),fileF$)
    IF x1 THEN Foreground$ = sxx$(2) + ";"
    ''''''''''''''''''''''''''''''''''''''''''''''
    'When using the shorthand property the order of the property values is:
    'background-color,background-image,background-repeat,background-attachment,background-position
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20)'
    IF file$ = mfile$ THEN y$ = "mBODY"
    IF file$ = tfile$ THEN y$ = "tBODY"
    IF file$ = hfile$ THEN y$ = "hBODY"
    x1 = getarray(y$,sxx$(),fileF$)
    IF x1 THEN Background$=sxx$(2)+" url("+sxx$(3) + "); "+sxx$(4)+";"+sxx$(5)+";"+sxx$(6)+";"
    '''''''''''''''''''''''''''''''''''''''''''''''''
    x1 = getarray("Link",sxx$(),fileF$)
    IF x1 THEN
        link1$ = sxx$(2) + ";"
        link2$ = sxx$(3) + ";"
        link3$ = sxx$(4) + ";"
    END IF
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    x1 = getarray("Hover",sxx$(),fileF$)
    IF x1 THEN hover1$ = sxx$(2) + ";": hover2$ = sxx$(3) + ";"
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    x1 = getarray("Active",sxx$(),fileF$)
    IF x1 THEN active1$ = sxx$(2) + ";": active2$ = sxx$(3) + ";"
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    x1 = getarray("Offsite",sxx$(),fileF$)
    IF x1 THEN offsite1$ = sxx$(2) + ";": offsite2$ = sxx$(3) + ";"
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    'do for all then insert into style sheet
    '? ' ,"a:            ";link1$,Link2$,Link3$
    '? ' ,"a.hover       ";hover1$,hover2$
    '? ' ,"a.active      ";active1$,active2$
    '? ' ,"a.visited     ";link1$,Link2$
    '? ' ,"a.offsite     ";offsite1$,offsite2$
   ''? ' ,"Background$   ";Background$
    '? ' ,"Foreground$   ";Foreground$
    '? ' ,"'''''''''''''''''''''''''''''''''"

    ' ____________________________________________________________________________________
    '/width relative formulas to Courier letters px = (cast*8), em window width = INT((6.4/100)*cast*8)
    COL_Width = INT((6.4/100)*cast*px)    '| COL_Width em
    E=INT(PX*100/16): EM#=E/100
    IF COL_Width<40 OR EM#<.5 OR  COL_Width>150 OR EM#>3 THEN
         Topic$ = "Column Width or Text PX problem in Setup.TXT"
         Problems(1) = "Cast="+STR$(Cast)+","+"PX="+STR$(px)+", COL_Width="+STR$(COL_Width)+", EM="+STR$(EM#)
         Problems(2) = "Range of Cast is [30 to 120],Range of PX is [8 to 20]"
         Problems(3) = "If values create COL_Width <30 or EM < .5 Display Problems will Occur!"
         problem_file(Topic$,3)
    END IF
    '\___________________________________________________________________________________/

    '? ' ,"COL_Width=";COL_Width, "EM#=";EM#
    '? ' ,"''''''''''''''''''''''''''''''''''''''''''''''''''"
    OPEN Control$ + file$ FOR OUTPUT AS ff
        PRINT#ff,"/* 'TEXT WEBBER' " + File$ + " Style Sheet */"
        PRINT#ff,"/* <link rel='stylesheet' TYPE='text/css' media='screen' href='"+File$+"'> */"
        PRINT#ff,"/* */"
        PRINT#ff,"body                  {"
        PRINT#ff,"      font-family:    'Lucida Console', monospace,'Courier New',  Monaco;"
        PRINT#ff,"      text-align:     left;"    '| right, left, center
        PRINT#ff,"      font-style:     normal;"
        PRINT#ff,"      /* font-variant:   small-caps; */"    '| small-caps, normal
        PRINT#ff,"      font-weight:    normal;"
        PRINT#ff,"      font-size:     "+STR$(EM#)+"em;"
        PRINT#ff,"      overflow: scroll;  /* scroll, visible, hidden, auto */"
        PRINT#ff,"      color:          " + Foreground$
        PRINT#ff,"      background:     " + Background$
        PRINT#ff,"                      }"
        PRINT#ff,"/* '''''''''''''''''''''''''''''''''''''''''''''''''''''' */"
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        PRINT#ff,"em    {"
        PRINT#ff,"      /* font-style:     oblique;  /* normal or italic or oblique */
        PRINT#ff,"      font-size:     "+STR$(INT(10*EM#)/10)+"em;"
        PRINT#ff,"      font-weight:    normal;"    '| bold or normal
        PRINT#ff,"      text-decoration:underline;"    '| sets text to normal size and weight
        PRINT#ff,"      }"
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        PRINT#ff,"  /*          Link Appearance                     */
        PRINT#ff," /* a:hover MUST come after a:link and a:visited */"
        PRINT#ff,"/* a:active MUST come after a:hover             */"
        PRINT#ff,"a         {"
        PRINT#ff,"          color:         " + Link1$
        PRINT#ff,"          background:    " + link2$
        PRINT#ff,"          text-decoration: none;"
        PRINT#ff,"          }"
        PRINT#ff,"a.offsite             {"
        PRINT#ff,"          text-decoration:    none;"
        PRINT#ff,"          font-weight:   normal;"
        PRINT#ff,"          color:         " + offsite1$
        PRINT#ff,"          background:    " + offsite2$
        PRINT#ff,"                      }"
        PRINT#ff,"a:link    {"
        PRINT#ff,"          color:         " + Link1$
        PRINT#ff,"          background:    " + link2$
        PRINT#ff,"          text-decoration: none;"
        PRINT#ff,"          }"
        PRINT#ff,"a:visited {"
        PRINT#ff,"          color:         " + Link1$
        PRINT#ff,"          decoration:    line-through;"
        PRINT#ff,"          }"
        PRINT#ff,"a:hover   {"
        PRINT#ff,"          color:         " + hover1$
        PRINT#ff,"          background:    " + hover2$
        PRINT#ff,"          text-decoration: none;"
        PRINT#ff,"          }"
        PRINT#ff,"a:active  {"
        PRINT#ff,"          color:         " + active2$
        PRINT#ff,"          background:         " + active1$
        PRINT#ff,"          text-decoration: none;"
        PRINT#ff,"          }"
        PRINT#ff,"a:focus   {"
        PRINT#ff,"          color:         " + active1$
        PRINT#ff,"          background:         " + active2$
        PRINT#ff,"          text-decoration: none;"
        PRINT#ff,"          }
        PRINT#ff,"pre   {"
        'PRINT#ff,"      text-align:  left;"
        'PRINT#ff,"      text-indent:  0;"
        'PRINT#ff,"      margin-left:  0;"
        'PRINT#ff,"      margin-bottom: 0;"
        PRINT#ff,"      }"

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        IF file$="tail.css" THEN
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): iForeground$=Foreground$
            x1 = getarray("iForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN iForeground$ = sxx$(2) + ";"
            '''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): iBackground$=Background$
            x1 = getarray("iBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN iBackground$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"/* iBody iForeground Used in Cover and page Index */"
            PRINT#ff,"#body_index_page  {           /* used in Cover Page */"
            PRINT#ff,"      text-align:     center; /* right, left, center */"
            PRINT#ff,"      /* font-variant:   small-caps; */ /* small-caps, normal */"
            PRINT#ff,"      font-size:     "+STR$(2*EM#)+"em;"
            PRINT#ff,"      font-weight:    normal;   /* bold or normal */"
            PRINT#ff,"      color: " + iForeground$
            PRINT#ff,"      background: " + iBackground$
            PRINT#ff,"      }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".Frame_inset {                /* frame around an element */"
            PRINT#ff,"      border-color:  "+Link2$
            PRINT#ff,"      border-width: "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      border-style: inset;"
            PRINT#ff,"             }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".p_in_image {                 /* position text within link graphic */"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 30%;"
            PRINT#ff,"      left: 30%;"
            PRINT#ff,"      float: center;"
            PRINT#ff,"      }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".Frame_scaled {              /* scale an image inside Frame_inset */"
            PRINT#ff,"      width: 100%;"
            PRINT#ff,"      height: auto;"
            PRINT#ff,"            }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".index_line {"
            PRINT#ff,"      padding-left: "+STR$(INT(20*EM#)/10)+"em; /* EM is PX dependent */"
            PRINT#ff,"      border-left:  solid;"
            PRINT#ff,"      border-right: none;"
            PRINT#ff,"      border-top:   none;"
            PRINT#ff,"      border-bottom: none;"
            PRINT#ff,"      border-left-width: thick;"
            PRINT#ff,"      border-color: red;"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      font-size:   "+STR$(INT(8*EM#)/10)+"em;"
            PRINT#ff,"          }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"#index {"
            PRINT#ff,"      float: right;"
            PRINT#ff,"      font-size:  "+STR$(INT(8*EM#)/10)+"em;"
            PRINT#ff,"      left: "+ STR$((col_width)/2) + "em;"
            PRINT#ff,"      padding: "+STR$(INT(10*EM#)/10)+"em "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      margin: "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): tForeground$=Foreground$
            x1 = getarray("tForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN tForeground$ = sxx$(2) + ";"
            '''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): tBackground$=Background$
            x1 = getarray("tBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"/* tBODY and tForeground Used for general tail page layout */"
            PRINT#ff,"#tailbody  {"
            PRINT#ff,"      max-width: " + STR$((col_width)) + "em; /* Determined by [width PX] in [SetUp.txt] */"
            PRINT#ff,"      text-align:     left;       /* right, left, center */"
            PRINT#ff,"      font-style:  normal;        /* normal, oblique */"
            PRINT#ff,"      /* font-variant:   small-caps; */ /* small-caps, normal */"
            PRINT#ff,"      font-size:     "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:    normal;       /* bold or normal */"
            PRINT#ff,"      color: " + tForeground$
            PRINT#ff,"      background: " + tBackground$
            PRINT#ff,"           }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"p {"
            PRINT#ff,"      font-style:  oblique;"
            PRINT#ff,"      font-weight:  normal;"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      font-size: 1em;"
            PRINT#ff,"      padding-right: 5%;"
            PRINT#ff,"      padding-left: 5%;"
            PRINT#ff,"      padding-top: 0;"
            PRINT#ff,"      padding-bottom: 0;"
            PRINT#ff,"      }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"div.text      {"
            PRINT#ff,"      padding: "+STR$(INT(10*EM#)/10)+"em "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      font-size:     "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      color: " + tForeground$
            PRINT#ff,"      font-weight:    normal;       /* bold or normal */"
            PRINT#ff,"      background: " + tBackground$
            PRINT#ff,"              }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".buttonscaled {"
            PRINT#ff,"      width: auto;"
            PRINT#ff,"      height:  "+STR$(INT(20*EM#)/10)+"em;"
            PRINT#ff,"      border-width: .25em;"
            PRINT#ff,"      border-style: outset;"
            PRINT#ff,"            }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"strong    {"
            PRINT#ff,"          font-size:   "+STR$(INT(13*EM#)/10)+"em;"
            PRINT#ff,"          font-weight:  bold;"
            PRINT#ff,"          font-variant:   small-caps; */ /* small-caps, normal
            PRINT#ff,"          }"

         END IF
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        IF file$="head.css" THEN
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): hForeground$=Foreground$
            x1 = getarray("hForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN hForeground$ = sxx$(2) + ";"
            '''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): hBackground$=Background$
            x1 = getarray("hBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"/* hBODY and hForeground Used for head page layout */"
            PRINT#ff,"#headbody  {"
            PRINT#ff,"      clear: both;              /* floating on both sides of element */"
            PRINT#ff,"      text-align:     left;"
            PRINT#ff,"      /* font-variant:   small-caps; */"    '| small-caps, normal
            PRINT#ff,"      font-size:      small;"    '| xx-small,x-small,small,medium,large,x-large,xx-large
            PRINT#ff,"      font-weight:    normal;"    '| font-weight bold or normal
            PRINT#ff,"      color: " + hForeground$
            PRINT#ff,"      background: " + hBackground$
            PRINT#ff,"      }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"div.img       {"
            PRINT#ff,"      border:  solid;"
            PRINT#ff,"      padding: "+STR$(INT(6*EM#)/10)+"em "+STR$(INT(6*EM#)/10)+"em;"
            PRINT#ff,"      margin: "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      float: left;"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"              }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".buttonscaled {"
            PRINT#ff,"      width: auto;"
            PRINT#ff,"      height:  "+STR$(INT(20*EM#)/10)+"em;"
            PRINT#ff,"            }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"div.desc      {"
            PRINT#ff,"      font-size:  "+STR$(INT(8*EM#)/10)+"em;"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"      font-weight: normal;"
            PRINT#ff,"              }"


        END IF
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

        IF file$="main.css" THEN
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            REDIM sxx$(20): Foreground$ = "BLACK;"
            x1 = getarray("mForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN Foreground$ = sxx$(2) + ";"
            REDIM sxx$(20): Background$ = "GRAY;"
            x1 = getarray("mBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"/* mBODY and mForeground Used for main page layout */"
            PRINT#ff,"#mainbody  {"
            PRINT#ff,"      font-family:    'Lucida Console', monospace,'Courier New',  Monaco;"
            PRINT#ff,"      /* font-variant:   small-caps; */"    '| small-caps, normal
            PRINT#ff,"      font-style:     normal;"    '| normal or italic or oblique
            PRINT#ff,"      font-size:      small;"    '| xx-small,x-small,small,medium,large,x-large,xx-large
            PRINT#ff,"      font-weight:    normal;"    '| font-weight bold or normal
            PRINT#ff,"      text-align:     left;"    '| right, left, center
            PRINT#ff,"      background:     " + Background$
            PRINT#ff,"      color:    " + Foreground$
            PRINT#ff,"      }"
            '''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"h1 {"
            PRINT#ff,"      text-align: left;"
            PRINT#ff,"      font-size:    "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"   }"
            PRINT#ff,"h2 {"
            PRINT#ff,"      text-align: left;"
            PRINT#ff,"      font-size:    "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"   }"
            PRINT#ff,"h3 {"
            PRINT#ff,"      text-align: left;"
            PRINT#ff,"      font-size:    "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"   }"
            PRINT#ff,"h4 {"
            PRINT#ff,"      text-align: left;"
            PRINT#ff,"      font-size:   "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"   }"
            PRINT#ff,"h5 {"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"      margin:  "+STR$(INT(10*EM#)/10)+"em;"
            'PRINT#ff,"      margin-bottom:  0;"
            PRINT#ff,"      font-size:    "+STR$(INT(15*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"      color:       "+Link3$
            PRINT#ff,"      background:  "+Link2$
            PRINT#ff,"      }"
            PRINT#ff,"h6 {  /* Statement Heading */"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"      margin:  "+STR$(INT(10*EM#)/10)+"em;"
            'PRINT#ff,"      margin-bottom:  0;"
            PRINT#ff,"      font-size:    "+STR$(INT(15*EM#)/10)+"em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"      color:      "+Link2$
            PRINT#ff,"      background: "+Link3$
            PRINT#ff,"      }"
            PRINT#ff,".subbutton {"
            PRINT#ff,"      margin:  "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"      font-size:   "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      text-decoration: none;"
            PRINT#ff,"      border-width: .3em;"
            PRINT#ff,"      border-style: outset;"
            PRINT#ff,"      border-color:       "+Link2$
            PRINT#ff,"      padding:            "+STR$(INT(4*EM#)/10)+"em;"
            PRINT#ff,"            }"
            PRINT#ff,".mainbutton {"
            PRINT#ff,"      margin:             "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      text-align:         center;"
            PRINT#ff,"      font-size:          "+STR$(INT(10*EM#)/10)+"em;"
            PRINT#ff,"      text-decoration:    none;"
            PRINT#ff,"      border-width:       .5em;"
            PRINT#ff,"      border-style:       outset;"
            PRINT#ff,"      border-color:       "+Link2$
            PRINT#ff,"      padding:            "+STR$(INT(6*EM#)/10)+"em;"
            PRINT#ff,"            }"

            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
         END IF
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
         GOTO exitstyle
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''




            PRINT#ff,"a         {"
            PRINT#ff,"          /* font-variant: small-caps; */"    '| small-caps, normal
            PRINT#ff,"          color:       " + Link1$
            PRINT#ff,"          background: transparent;"
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"
            PRINT#ff,"a:focus  {"
            'PRINT#ff,"          font-size:    1em;"
            PRINT#ff,"          color:        " + active1$
            PRINT#ff,"          background:   " + active2$
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"


        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

         ''''''''''''''''''''''''''''''''''''''''''''''''''''
        PRINT#ff,"#maxarea {"
        IF file$="main.css" THEN
            PRINT#ff,"      position:           absolute;"
            PRINT#ff,"      top:                800px;"
            PRINT#ff,"      left:               1px;"
            PRINT#ff,"      width:              100%;"
            PRINT#ff,"      height:             100%;"
            PRINT#ff,"      padding-right:      5px;"
            PRINT#ff,"      background-color:   green;"
        ELSE
            PRINT#ff,"      position:           absolute;"
            PRINT#ff,"      top:                800px;"
            PRINT#ff,"      width:              auto;"
            PRINT#ff,"      height:             300%;"
            PRINT#ff,"      background-color:   green;"
        END IF
        PRINT#ff,"         }"
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,".head1 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       26%; "
            PRINT#ff,"      width:      70%; "
            PRINT#ff,"      height:     20%;"
            PRINT#ff,"      }"
            PRINT#ff,".head3 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       88%; "
            PRINT#ff,"      width:      10%; "
            PRINT#ff,"      height:     95%;"
            PRINT#ff,"      }"
            PRINT#ff,".main1 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       2%;"
            PRINT#ff,"      width:      22%;"
            PRINT#ff,"      height:     95%;"
            PRINT#ff,"      }"
            PRINT#ff,".main2 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       2%;"
            PRINT#ff,"      width:      22%;"
            PRINT#ff,"      height:     95%;"
            PRINT#ff,"      }"
            PRINT#ff,".main3 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       2%;"
            PRINT#ff,"      width:      22%;"
            PRINT#ff,"      height:     95%;"
            PRINT#ff,"      }"
            PRINT#ff,".tail1 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        26%;"
            PRINT#ff,"      left:       26%;"
            PRINT#ff,"      width:      70%;"
            PRINT#ff,"      height:     71%;"
            PRINT#ff,"      }"
            PRINT#ff,".tail2 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       26%;"
            PRINT#ff,"      width:      70%;"
            PRINT#ff,"      height:     95%;"
            PRINT#ff,"      }"
            PRINT#ff,".tail3 {"
            PRINT#ff,"      position:   absolute;"
            PRINT#ff,"      top:        2%;"
            PRINT#ff,"      left:       26%;"
            PRINT#ff,"      width:      60%;"
            PRINT#ff,"      height:     95%;"
            PRINT#ff,"      }"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            PRINT#ff,"#index {"
            'PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2px;"
            'from cast/courier formula
            PRINT#ff,"      left: "+ STR$((col_width+1)/2) + "em;"
            PRINT#ff,"      width: 30%;"
            PRINT#ff,"      font-size:  .8em;"
            PRINT#ff,"      float: right;"
            PRINT#ff,"      background-color: RGB(200,200,200);"
            PRINT#ff,"      }"
            PRINT#ff,"/* Used in Index.html page */"

            ''''''''''''''''''''''''''''''''''''''''''''''''

        'END IF

            PRINT#ff,".subbutton {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      left: 2em;"
            PRINT#ff,"      font-size:    .8em;"
            PRINT#ff,"      text-decoration: none;"
            'PRINT#ff,"      border-width: .25em;"
            'PRINT#ff,"      border-style: inset;"
            PRINT#ff,"            }"
            PRINT#ff,".mainbutton {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      left: 1em;"
            PRINT#ff,"      font-size:    .9em;"
            PRINT#ff,"      text-decoration: underline;"
            'PRINT#ff,"      border-width: .25em;"
            'PRINT#ff,"      border-style: outset;"
            PRINT#ff,"            }"
            PRINT#ff,"dl    {"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      font-size:    1em;"
            PRINT#ff,"      }"
            PRINT#ff,"dt    {"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      font-size:    1em;"
            PRINT#ff,"      }"
            PRINT#ff,"dd    {"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      font-size:    1em;"
            PRINT#ff,"      }"
           PRINT#ff,"strong    {"
            PRINT#ff,"          font-size:    1.2em;"
            PRINT#ff,"          font-weight:  normal;"
            PRINT#ff,"          }"
            PRINT#ff,"/*          Link Appearance                */"
            Link1$ = "WHITE;":link2$ = "BLUE;":link3$ = "BLACK;"
            hover1$ = Link2$: hover2$ = Link1$
            active1$ = "WHITE;": active2$ = "BLACK;"
            offsite1$ = link3$: offsite2$ = link2$
            REDIM sxx$(20)
            x1 = getarray("Link",sxx$(),fileF$)
            IF ISTRUE x1 THEN
                link1$ = sxx$(2) + ";"
                link2$ = sxx$(3) + ";"
                link3$ = sxx$(4) + ";"
            END IF
            x1 = getarray("Hover",sxx$(),fileF$)
            IF ISTRUE x1 THEN hover1$ = sxx$(2) + ";": hover2$ = sxx$(3) + ";"
            x1 = getarray("Active",sxx$(),fileF$)
            IF ISTRUE x1 THEN active1$ = sxx$(2) + ";": active2$ = sxx$(3) + ";"
            x1 = getarray("Offsite",sxx$(),fileF$)
            IF ISTRUE x1 THEN offsite1$ = sxx$(2) + ";": offsite2$ = sxx$(3) + ";"
            PRINT#ff,"a         {"
            PRINT#ff,"          /* font-variant: small-caps; */"    '| small-caps, normal
            PRINT#ff,"          color:       " + Link1$
            PRINT#ff,"          background: transparent;"
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"
            PRINT#ff,"a:link    {"
            'PRINT#ff,"          font-size:    1em;"
            PRINT#ff,"          color:        " + Link1$
            PRINT#ff,"          background:   " + link2$
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"
            PRINT#ff,"a:visited {"
            'PRINT#ff,"          font-size:    1em;"
            'PRINT#ff,"          color:       " + Link3$
            'PRINT#ff,"          background:   " + link2$
            PRINT#ff,"          text-decoration: line-through;"
            PRINT#ff,"          }"
            PRINT#ff,"a:hover   {"
            'PRINT#ff,"          font-size:    1em;"
            PRINT#ff,"          color:        " + hover1$
            PRINT#ff,"          background:   " + hover2$
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"
            PRINT#ff,"a:active  {"
            'PRINT#ff,"          font-size:    1em;"
            PRINT#ff,"          color:        " + active1$
            PRINT#ff,"          background:   " + active2$
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"
            PRINT#ff,"a:focus  {"
            'PRINT#ff,"          font-size:    1em;"
            PRINT#ff,"          color:        " + active1$
            PRINT#ff,"          background:   " + active2$
            PRINT#ff,"          text-decoration: none;"
            PRINT#ff,"          }"
            PRINT#ff,"      /* Use of div.img */"
            PRINT#ff,"      /* <div class='img'> */"
            PRINT#ff,"      /* <a target='_blank' href='????.html'><img src='klematis_small.jpg' alt='Klematis'></a> */"
            PRINT#ff,"      /* <div class='desc'>description</div> */"
            PRINT#ff,"      /* </div> */"
            PRINT#ff,"      /* ------------------------------------------------------------------------------------- */"
            PRINT#ff,"div.img img   {"
            PRINT#ff,"      display: inline;"
            PRINT#ff,"      margin: 2px;"
            PRINT#ff,"      border: 1px solid #ffffff;"
            PRINT#ff,"              }"
            PRINT#ff,"div.img a:link    {"
            PRINT#ff,"      background: transparent;"
            PRINT#ff,"                  }"
            PRINT#ff,"div.img a:hover img   {"
            PRINT#ff,"      border:1px solid #0000ff;"
            PRINT#ff,"                      }"
            REDIM sxx$(20): Background$ = "GRAY;"
            x1 = getarray("iBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2))
    exitstyle:
    CLOSE ff
    '--------
    'CLOSE: ? "END of style sheet": WAITKEY$
1400:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION license ALIAS "z1500" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1500
'------------------
    ff = FREEFILE
    TRY
        '----------------------------------------------------
        OPEN file$ FOR INPUT AS ff    '| Control license.txt
            Image
            '---------------
            FOR x = 1 TO 20
                LINE INPUT#ff,x$
                '----------------
            NEXT x
        CLOSE ff
        '--------
        EXIT FUNCTION
    CATCH
        license = -1
    END TRY
    '---------------------------
    OPEN file$ FOR OUTPUT AS ff
        PRINT#ff,"'TEXT WEBBER':-1"
        PRINT#ff,"Created by Hugh Polley [hapolley@hotmail.com] (&copy;2005/01/14,&copy;2007,&copy;2009,&copy;2010):-1"
        PRINT#ff,"These terms apply to the software known as 'TEXT WEBBER'."
        PRINT#ff,"Redistribution Or Rental of a registered version is Not Permitted!"
        'PRINT#ff,SPACE$(4)
        PRINT#ff,"BY INSTALLING OR USING 'TEXT WEBBER' (HEREAFTER, THE 'PRODUCT'), THE:+8:+7"
        PRINT#ff,"INDIVIDUAL OR ENTITY LICENSING THE PRODUCT ('LICENSEE' or 'YOU' or 'YOUR')"
        PRINT#ff,"IS CONSENTING TO BE BOUND BY AND IS BECOMING A PARTY TO THIS AGREEMENT."
        PRINT#ff,"IF LICENSEE DOES NOT AGREE TO ALL OF THE TERMS OF THIS AGREEMENT, THE"
        PRINT#ff,"LICENSEE MUST NOT INSTALL OR USE THE SOFTWARE. IF THESE TERMS ARE"
        PRINT#ff,"CONSIDERED AN OFFER, ACCEPTANCE IS EXPRESSLY LIMITED TO THESE TERMS.:-5"
        PRINT#ff,":-7:-8"
        PRINT#ff,"TERMS:-1"
        PRINT#ff,"LICENSE AGREEMENT: As used in this Agreement, 'Hugh' shall mean:-2"
        PRINT#ff,"the programs creator 'Hugh A. Polley'.  The use of any third party"
        PRINT#ff,"software product included in the Product shall be governed by the third"
        PRINT#ff,"party's license agreement and not by this Agreement, whether that license"
        PRINT#ff,"agreement is presented for acceptance the first time that the third party"
        PRINT#ff,"software is invoked, is included in a file in electronic form, or is"
        PRINT#ff,"included in the package in printed form."
        PRINT#ff,"LICENSE GRANT: Hugh grants Licensee a limited, royalty-free,:-2"
        PRINT#ff,"non-exclusive, non-sublicenseable, and non-transferable license to use the"
        PRINT#ff,"executable code version of the Product, provided any copy must contain all"
        PRINT#ff,"of the original proprietary notices.  This license does not entitle"
        PRINT#ff,"Licensee to receive from Hugh hard-copy documentation, technical support,"
        PRINT#ff,"telephone assistance, or enhancements or updates to the Product"
        PRINT#ff,"('Support Programs'), provided, however, that if Licensee DOWNLOADS or"
        PRINT#ff,"otherwise obtains IN ANY manner ANY Support Programs they shall become"
        PRINT#ff,"part OF the Product AND the terms OF this Agreement shall apply."
        PRINT#ff,"Licensee may NOT customize the Product."
        PRINT#ff,"RESTRICTIONS: Except AS otherwise expressly permitted IN this:-2"
        PRINT#ff,"Agreement, Licensee may NOT: (i) modify OR CREATE ANY derivative works OF"
        PRINT#ff,"the Product OR documentation, including translation OR localization;"
        PRINT#ff,"(ii) decompile, disassemble, reverse engineer, OR otherwise attempt TO"
        PRINT#ff,"derive the source code FOR the Product (except TO the extent applicable"
        PRINT#ff,"laws specifically prohibit such restriction); (iii) encumber, sell, rent,"
        PRINT#ff,"lease, sublicense, OR otherwise transfer rights TO the Product; (iv) use"
        PRINT#ff,"the Software FOR timesharing OR service bureau purposes; (v) remove OR"
        PRINT#ff,"alter ANY trademark, logo, copyright OR other proprietary notices,"
        PRINT#ff,"legends, symbols OR labels IN the Product; OR (vi) publish ANY results OF "
        PRINT#ff,"benchmark tests run ON the Product TO a third party without Hugh's prior"
        PRINT#ff,"written consent."
        PRINT#ff,"TERMINATION: Without prejudice to any other rights, Hugh may,:-2"
        PRINT#ff,"at his sole discretion, terminate this Agreement and the license granted herein"
        PRINT#ff,"at any time. Upon termination, Licensee shall destroy all copies of the"
        PRINT#ff,"Product."
        PRINT#ff,"PROPRIETARY RIGHTS: Title, ownership, AND intellectual property:-2"
        PRINT#ff,"rights IN the Product shall remain IN Hugh AND/OR its suppliers. Licensee"
        PRINT#ff,"acknowledges such ownership AND intellectual property rights AND will NOT"
        PRINT#ff,"take ANY action TO jeopardize, limit OR interfere IN ANY manner WITH"
        PRINT#ff,"Hugh's or its suppliers' ownership of or rights with respect to the"
        PRINT#ff,"Product. The Product is protected by copyright and other intellectual"
        PRINT#ff,"property laws and by international treaties. Title and related rights IN"
        PRINT#ff,"the content accessed through the Product is the property OF the"
        PRINT#ff,"applicable content owner AND is protected by applicable law. The license"
        PRINT#ff,"granted under this Agreement gives Licensee no rights TO such content."
        PRINT#ff,"DISCLAIMERS: Hugh DOES NOT VERIFY THE CONTENT OF THE MATERIAL:-2"
        PRINT#ff,"ACCESSED BY YOU THROUGH THE USE OF THE PRODUCT ('CONTENT').  THE PRODUCT AND"
        PRINT#ff,"CONTENT ARE PROVIDED ON AN 'AS IS' BASIS, WITHOUT WARRANTY OF ANY KIND,"
        PRINT#ff,"INCLUDING WITHOUT LIMITATION ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND"
        PRINT#ff,"INCLUDING BUT NOT LIMITED TO WARRANTIES THAT THE PRODUCT OR CONTENT ARE"
        PRINT#ff,"FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE OR"
        PRINT#ff,"NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE"
        PRINT#ff,"PRODUCT AND CONTENT IS BORNE BY LICENSEE. THIS DISCLAIMER OF WARRANTY"
        PRINT#ff,"CONSTITUTES AN ESSENTIAL PART OF THIS AGREEMENT. NO USE OF THE PRODUCT IS"
        PRINT#ff,"AUTHORIZED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.  ALTHOUGH ALL CONTENT"
        PRINT#ff,"IS BELIEVED TO BE RELIABLE, Hugh DOES NOT GUARANTEE THE RELIABILITY,"
        PRINT#ff,"ACCURACY, COMPLETENESS, SAFETY, TIMELINESS, LEGALITY, USEFULNESS,"
        PRINT#ff,"ADEQUACY, OR SUITABILITY OF ANY CONTENT. THE CONTENT MAY BE HARMFUL,"
        PRINT#ff,"UNTIMELY, INCOMPLETE, OR INACCURATE AND, ACCORDINGLY, YOU AGREE TO"
        PRINT#ff,"EXERCISE CAUTION, DISCRETION AND COMMON SENSE WHEN USING THE CONTENT."
        PRINT#ff,"THE PRODUCT MAY CONTAIN SOFTWARE BUGS WHICH COULD CAUSE UNANTICIPATED"
        PRINT#ff,"CONSEQUENCES THROUGH ITS USE. YOUR USE OF THE PRODUCT IS AT YOUR OWN RISK."
        PRINT#ff,"THE PRODUCT AND THE SERVICE ARE PROVIDED 'AS IS' WITHOUT ANY WARRANTIES"
        PRINT#ff,"OR CONDITIONS OF ANY KIND WHATSOEVER.  Hugh DISCLAIMS ANY EXPRESS OR"
        PRINT#ff,"IMPLIED WARRANTIES AS TO THE NONINFRINGEMENT, MERCHANTABILITY,"
        PRINT#ff,"SUITABILITY, RELIABILITY, AVAILABILITY, TIMELINESS, ACCURACY OR FITNESS"
        PRINT#ff,"FOR ANY PURPOSE OF THE PRODUCT AND THE SERVICE OR THAT THE OPERATION OF"
        PRINT#ff,"THE SOFTWARE AND THE PROVISION OF THE SERVICE WILL BE UNINTERRUPTED OR"
        PRINT#ff,"ERROR-FREE."
        PRINT#ff,"LIMITATION OF LIABILITY: TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE:-2 "
        PRINT#ff,"LAW, IN NO EVENT WILL Hugh, ITS LICENSORS, ITS SUPPLIERS OR RESELLERS BE"
        PRINT#ff,"LIABLE FOR ANY INDIRECT, SPECIAL, INCIDENTAL, CONSEQUENTIAL, OR PUNITIVE"
        PRINT#ff,"DAMAGES ARISING OUT OF THE USE OF OR INABILITY TO USE THE PRODUCT OR"
        PRINT#ff,"CONTENT, INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL,"
        PRINT#ff,"WORK STOPPAGE, COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL OTHER"
        PRINT#ff,"COMMERCIAL DAMAGES OR LOSSES, EVEN IF ADVISED OF THE POSSIBILITY THEREOF,"
        PRINT#ff,"AND REGARDLESS OF THE LEGAL OR EQUITABLE THEORY (CONTRACT, TORT OR"
        PRINT#ff,"OTHERWISE) UPON WHICH THE CLAIM IS BASED. IN ANY CASE, Hugh's ENTIRE"
        PRINT#ff,"LIABILITY UNDER ANY PROVISION OF THIS AGREEMENT SHALL NOT EXCEED $10"
        PRINT#ff,"CANADIAN DOLLARS, WITH THE EXCEPTION OF DEATH OR PERSONAL INJURY CAUSED"
        PRINT#ff,"BY THE NEGLIGENCE OF Hugh TO THE EXTENT APPLICABLE LAW PROHIBITS THE"
        PRINT#ff,"LIMITATION OF DAMAGES IN SUCH CASES. SOME JURISDICTIONS DO NOT ALLOW THE"
        PRINT#ff,"EXCLUSION OR LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THIS"
        PRINT#ff,"EXCLUSION AND LIMITATION MAY NOT BE APPLICABLE.  HUGH IS NOT RESPONSIBLE"
        PRINT#ff,"FOR ANY LIABILITY ARISING OUT OF CONTENT PROVIDED BY LICENSEE OR A THIRD"
        PRINT#ff,"PARTY THAT IS ACCESSED THROUGH THE PRODUCT AND/OR ANY MATERIAL LINKED"
        PRINT#ff,"THROUGH SUCH CONTENT. IN ADDITION, LICENSEE SHALL BE SOLELY RESPONSIBLE,"
        PRINT#ff,"AT ITS OWN EXPENSE, FOR ACQUIRING, INSTALLING, MAINTAINING AND UPDATING"
        PRINT#ff,"ALL CONNECTIVITY EQUIPMENT, HARDWARE, SOFTWARE AND OTHER EQUIPMENT AS MAY"
        PRINT#ff,"BE NECESSARY FOR IT TO USE THE PRODUCT."
        PRINT#ff,"RELEASE AND WAIVER: LICENSEE USES Hugh's SERVICES AT HIS OR HER OWN:-2"
        PRINT#ff,"RISK. To the maximum extent permitted by applicable law, You hereby"
        PRINT#ff,"release, and waive all claims against, Hugh and its employees and agents"
        PRINT#ff,"from any and all liability for claims, damages (actual AND consequential),"
        PRINT#ff,"costs AND expenses (including litigation costs AND attorneys' fees) of"
        PRINT#ff,"every nature and kind, arising out of or in any way connected with use of"
        PRINT#ff,"the Site. If You are a California resident, You waive Your rights under"
        PRINT#ff,"California Civil Code Section 1542, which states, 'A general release does"
        PRINT#ff,"not extend to claims which the creditor does not know or suspect to exist"
        PRINT#ff,"in his favor at the time of executing the release, which if known by him "
        PRINT#ff,"must have materially affected his settlement WITH the debtor.' Residents "
        PRINT#ff,"of other states and nations similarly waive their rights under applicable"
        PRINT#ff,"and/or analogous laws, statues, or regulations."
        PRINT#ff,"PRIVACY: Hugh is dedicated TO protecting your personal:-2 "
        PRINT#ff,"information and will only ever release grouped anonymous DATA"
        PRINT#ff,"to third parties without Your permission."
        PRINT#ff,"You agree that IN the event that Hugh as a"
        PRINT#ff,"company is acquired, the acquirer will have ACCESS TO Your Personally"
        PRINT#ff,"Identifiable DATA on the same above terms and conditions."
        PRINT#ff,"MISCELLANEOUS:-2"
        PRINT#ff,"   (a)This Agreement constitutes the entire agreement between the parties"
        PRINT#ff,"concerning the subject matter hereof."
        PRINT#ff,"   (b)This Agreement may be amended only by a writing SIGNED by both parties."
        PRINT#ff,"   (c)Except TO the extent applicable law, IF ANY, provides otherwise, this"
        PRINT#ff,"Agreement shall be governed by the laws OF Vanuatu."
        PRINT#ff,"   (d)This Agreement shall NOT be governed by the United Nations Convention"
        PRINT#ff,"ON Contracts FOR the International Sale OF Goods."
        PRINT#ff,"   (e)IF ANY provision IN this Agreement should be held illegal OR"
        PRINT#ff,"unenforceable by a court having jurisdiction, such provision shall be"
        PRINT#ff,"modified TO the extent necessary TO render it enforceable without losing"
        PRINT#ff,"its intent, OR severed FROM this Agreement IF no such modification is"
        PRINT#ff,"possible, AND other provisions OF this Agreement shall remain IN full"
        PRINT#ff,"force AND effect."
        PRINT#ff,"   (f)The controlling language OF this Agreement is English. IF Licensee has"
        PRINT#ff,"received a translation into another language, it has been provided FOR"
        PRINT#ff,"Licensee's convenience only."
        PRINT#ff,"   (g)A waiver by either party OF ANY term OR condition OF this Agreement OR "
        PRINT#ff,"ANY breach thereof, IN ANY one instance, shall NOT waive such term OR"
        PRINT#ff,"condition OR ANY subsequent breach thereof."
        PRINT#ff,"   (h)The provisions OF this Agreement which require OR contemplate"
        PRINT#ff,"performance after the expiration OR termination OF this Agreement shall"
        PRINT#ff,"be enforceable notwithstanding said expiration OR termination."
        PRINT#ff,"   (i)Licensee may NOT assign OR otherwise transfer by operation OF law OR"
        PRINT#ff,"otherwise this Agreement OR ANY rights OR obligations herein except IN the"
        PRINT#ff,"CASE OF a merger OR the sale OF ALL OR substantially ALL OF Licensee's"
        PRINT#ff,"assets to another entity. Licensor may assign this Agreement to a third"
        PRINT#ff,"party at any time."
        PRINT#ff,"   (j) This Agreement shall be binding upon and shall inure to the benefit of"
        PRINT#ff,"the parties, their successors AND permitted assigns."
        PRINT#ff,"   (k) Neither party shall be IN DEFAULT OR be liable FOR ANY delay, failure"
        PRINT#ff,"IN performance (excepting the obligation TO pay) OR interruption OF"
        PRINT#ff,"service resulting directly OR indirectly FROM ANY cause beyond its"
        PRINT#ff,"reasonable control."
        PRINT#ff,"   (l) The relationship between Hugh AND Licensee is that OF independent"
        PRINT#ff,"contractors AND neither Licensee nor its agents shall have ANY authority"
        PRINT#ff,"TO bind Hugh IN ANY way."
        PRINT#ff,"   (m) IF ANY dispute arises under this Agreement, the prevailing party shall"
        PRINT#ff,"be reimbursed by the other party FOR ANY AND ALL legal fees AND costs"
        PRINT#ff,"associated therewith."
        PRINT#ff,"   (n) IF ANY Hugh professional services are being provided, THEN such"
        PRINT#ff,"professional services are provided pursuant TO the terms OF a separate"
        PRINT#ff,"Professional Services Agreement between Hugh AND Licensee. The parties"
        PRINT#ff,"acknowledge that such services are acquired independently OF the Product"
        PRINT#ff,"licensed hereunder, AND that provision OF such services is NOT essential"
        PRINT#ff,"TO the functionality OF such Product."
        PRINT#ff,"   (o) The headings TO the sections OF this Agreement are used FOR"
        PRINT#ff,"convenience only AND shall have no substantive meaning."
    CLOSE ff
    '--------
1500:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION hFrame_THREE ALIAS "z1600" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1600
'------------------
    REDIM Files$(3)
    Files$(0) = WebberText$ + "frame.html"
    Files$(1) = WebberText$ + "frame1.html"
    Files$(2) = WebberText$ + "frame2.html"
    Files$(3) = WebberText$ + "frame3.html"
    ff = FREEFILE
    gg = FREEFILE
    ' ___________________________________________________
    '/Web Site 3 possible Frames with Spider Descriptions
    fileF$ = Control$  + "!setup.txt"
    REDIM sxx$(20): Title$ = "TEXT WEBBER"
    '''''''''''''''''''''''''''''''''''''''
    x1 = getarray("site title",sxx$(),fileF$)
    IF ISTRUE x1 THEN
        Image
        '---------------
        FOR x = 2 TO 20
            IF LEN(LTRIM$(sxx$(x))) > 0 THEN
                Title$ = sxx$(x)
            END IF
        NEXT x
    END IF
    Title$ = single_quote_remove(Title$)
    Image
    '--------------------
    FOR framing = 1 TO 3
        TRY
            '------------------------------------
            OPEN Files$(framing) FOR INPUT AS ff
                LINE INPUT#ff,x$
                '----------------
            CLOSE ff
            '--------
        CATCH
            CLOSE ff
            '--------
            '3, 'top and 2 col frames
            '2, no top, 2 Col frames
            '1, top and 2 col frames
            OPEN Files$(framing) FOR OUTPUT AS ff
                x$ = WebberText$ + "DOCTYPE_DECLARE.tmp"
                '-----------------------
                OPEN x$ FOR INPUT AS gg
                    Image
                    '----------------
                    DO UNTIL EOF(gg)
                        LINE INPUT#gg,y$
                        '----------------
                        PRINT#ff,y$
                    LOOP
                CLOSE gg
                x$ = WebberText$ + "DOCTYPE_heading.tmp"
                '-----------------------
                OPEN x$ FOR INPUT AS gg
                    Image
                    '----------------
                    DO UNTIL EOF(gg)
                        LINE INPUT#gg,y$
                        '----------------
                        IF INSTR(y$,"</head")>0 THEN
                            PRINT#ff,"    <link rel='stylesheet' TYPE='text/css' media='screen' href='tail.css'>"
                            PRINT#ff,y$
                            EXIT DO
                        ELSEIF INSTR(y$,"<link rel")=0 THEN
                            PRINT#ff,y$
                        END IF
                    LOOP
                CLOSE gg
                '--------
                REDIM sxx$(20):x1 = getarray("frameset cols",sxx$(),fileF$)
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                PRINT#ff,"  <frameset cols='"+sxx$(2)+"%,"+sxx$(3)+"%'>"
                PRINT#ff,"      <frame src='main.html' name='main' scrolling='auto' frameborder='0'>"
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                REDIM sxx$(20):x1 = getarray("frameset rows",sxx$(),fileF$)
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                IF  framing<>2 THEN PRINT#ff,"      <frameset rows='"+sxx$(2)+"%,"+sxx$(3)+"%'>"
                IF  framing<>2 THEN PRINT#ff,"          <frame src='head.html' name='head' scrolling='no' frameborder='0'>"
                PRINT#ff,"          <frame src='tail.html' name='tail' scrolling='auto' frameborder='0'>"
                PRINT#ff,"          <noframes>"
                PRINT#ff,"                  <p>Interactive content requires a Browser with Frames to view this site.</p>"
                PRINT#ff,"          </noframes>"
                IF  framing<>2 THEN PRINT#ff,"      </frameset>"
                PRINT#ff,"  </frameset>"
                PRINT#ff,"</html>"
            CLOSE ff
               '--------
        END TRY
    NEXT framing

    TRY
        '--------------------------
        OPEN File$ FOR INPUT AS ff
            LINE INPUT#ff,x$
            '----------------
        CLOSE ff
       '--------

    CATCH
        CLOSE ff
        '--------
        hFrame_THREE = -1     '| frame.html does not exist

        FILECOPY Files$(1),File$
    FINALLY
        CLOSE ff
        '--------
        SLEEP 250
        FILECOPY File$,HTMLfolders$ + "frame.html"
    END TRY

1600:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION hFrame_Head ALIAS "z1700" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1700
'------------------
ff = FREEFILE

TRY
    test=0: test$=Control$+file$+".txt": Image
    '---------------------------------------------------
    OPEN test$ FOR INPUT AS f1
    '----------------
        DO UNTIL EOF(f1)
            LINE INPUT#f1,x$: x$=TRIM$(x$)
            IF LEN(x$)>0 THEN test=test+1
        LOOP
    CLOSE f1
    IF test<3 THEN
        Topic$ ="IN ["+ Test$+"] Empty File Problem?!"
        Problems(1) ="OPEN '"+test$+".txt'"
        Problems(2) ="Little or no text found!"
        Problems(3) ="add two: <!-- no text wanted --"
        problem_file(Topic$,3)
    END IF
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    TRY
        test=0: test$=WebberText$+file$+".txt": Image
        OPEN test$ FOR INPUT AS f1
        '----------------
            DO UNTIL EOF(f1)
                LINE INPUT#f1,x$: x$=TRIM$(x$)
                IF LEN(x$)>0 THEN test=test+1
            LOOP
        CLOSE f1
        IF test<2 THEN
            Topic$ ="IN ["+ Test$+"] Empty File Problem?!"
            Problems(1) ="OPEN '"+test$+".txt'"
            Problems(2) ="Little or no text found!"
            Problems(3) ="add two: <!-- no text wanted --"
            problem_file(Topic$,3)
        END IF

    CATCH
        SLEEP 250: hFrame_Head = -1
        FILECOPY (Control$+file$+".txt"),(WebberText$+file$+".txt")
    END TRY
    '''''''''''''''''''''''''''''''''''''''''''


CATCH
    CLOSE ff
    hFrame_Head = -1
    '---------------------------
    OPEN (Control$ + file$ + ".txt") FOR OUTPUT AS ff
        x = LEN(HTMLfolders$): x$ = LEFT$(HTMLfolders$,x-1)
        PRINT#ff,x$ + ":-9"
        PRINT#ff,"{tail.html} ~secondimage.jpg~ HOME}:-4:-3"
        PRINT#ff,"{http://validator.w3.org} ~secondimage.jpg~ Validator HTML}:+4:-3"
        PRINT#ff,"{http://jigsaw.w3.org/css-validator/} ~secondimage.jpg~ Validator CSS}:+4:-3"
    CLOSE ff
    SLEEP 250
    FILECOPY (Control$+file$+".txt"),(WebberText$+file$+".txt")
END TRY
'--------
1700:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
SUB hFrame_Main ALIAS "z1800" (file$)
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 1800
'------------------
    fileF$ = Control$  + "!setup.txt"
    LOCAL maxres AS LONG
    maxres=1
    FileGoody$ = File$
    FileStyle1$ = Trash$ + "Style1Inport.tmp"
    FileStyle2$ = Trash$ + "Style2Inport.tmp"
    FileWork$ = Trash$ + "WorkMain.tmp"
    FileStore$ = Trash$ + "SubFolders.tmp"
    '---------------------------------------
'? ' ,"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
'? ' ,"              hFrame_main"
'? ' ,"''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
'? ' ,"    FileGoody$ ="; File$
'? ' ,"    FileStyle1$ ="; Trash$ + "Style1Inport.tmp"
'? ' ,"    FileStyle2$ ="; Trash$ + "Style2Inport.tmp"
'? ' ,"    FileWork$ = ";Trash$ + "WorkMain.tmp"
'? ' ,"    FileStore$ ="; Trash$ + "SubFolders.tmp"
'? ' ,"    OUTPUT", File$
'? ' ,"''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
'? ' ,""
'? ' ,""
'? ' ,""
    OPEN FileGoody$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileStyle1$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileStyle2$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileWork$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileStore$ FOR OUTPUT AS 4: CLOSE 4
    ''''''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20):x1 = getarray("frameset cols",sxx$(),Control$+"!setup.txt")
    'cols$=sxx$(2)+"%,"+sxx$(3)+"%"
    'REDIM sxx$(20):x1 = getarray("frameset rows",sxx$(),Control$+"!setup.txt")
    'rows$=sxx$(2)+"%,"+sxx$(3)+"%"
    '? ' ,"here1"
    '? ' ,cols$
    '? ' ,rows$
    '? ' ,"here2"
    OPEN Trash$ + "maxarea.tmp" FOR OUTPUT AS 4
        PRINT#4,"       width: 80%;"
        PRINT#4,"       height: 100%;"
        PRINT#4,"       margin: 2em;"
        PRINT#4,"border: .5em solid black;"
        PRINT#4,"background-color: lightgrey;"
         PRINT#4,"}"
    CLOSE 4
    '-------
    File$ = FileStyle1$
    f1 = FREEFILE
    f2 = FREEFILE
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    '? ' ,file$,"constructing Main HTML"
    OPEN file$ FOR OUTPUT AS f1
        PRINT#f1,"<!DOCTYPE HTML PUBLIC "+$DQ+"-//W3C//DTD HTML 4.01 Frameset//EN"+$DQ
        PRINT#f1,$DQ+"http://www.w3.org/TR/html4/frameset.dtd"+$DQ+">"
        PRINT#f1,"<html lang=en>"

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        PRINT#f1,"  <head>"
        PRINT#f1,"      <title>main</title>"
        PRINT#f1,"      <meta charset='utf-8' />"
        PRINT#f1,"      <style>"

        x$ = WebberText$ + "main.css"': ? ' ,x$,"inputing from style sheet"
        '-----------------------
        OPEN x$ FOR INPUT AS gg
           Image

           '----------------
           DO UNTIL EOF(gg)
             LINE INPUT#gg,y$
             '----------------
             PRINT#f1,y$
        LOOP
        CLOSE gg
    CLOSE f1
'==============================================================================
    File$ = FileWork$
    '---------------------------
    '? ' ,File$,"Constructing Main - Body Frame -"
    '? ' ,""
    '? ' ,""
    '? ' ,""

    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    OPEN File$ FOR OUTPUT AS f1
        PRINT#f1,SPACE$(4)
        PRINT#f1,"</style>
        PRINT#f1,"</head>
        PRINT#f1,"  <body ID='mainbody'>"
        PRINT#f1,"  <div>"
        ' _________________________________
        '/1. Site Title and Date updated
        s$ = DATE$
        mm$ = MID$(s$,1,2)
        Dy$ = MID$(s$,4,2)
        yy$ = MID$(s$,7)
        yy$ = yy$ + "/" + mm$ + "/" + Dy$
        '''''''''''''''''''''''''''''''''
        REDIM sx$(20): xx$ ="Text Webber"
        x1 = getarray("site title",sx$(),fileF$)
        Image
        '---------------
        FOR x = 2 TO 20
           y$ = TRIM$(sx$(x))
           IF LEN(y$)> 0 THEN xx$ = y$
        NEXT x
        bLink$ = xx$ + " [" + yy$ + "]"
        INCR Link
        mainLink$ = "AAAAA" + TRIM$(STR$(Link))
        mainLink$ = RIGHT$(mainLink$,4) + "top"
        mlk$ = "      <div id='" + mainLink$ + "'><h6>" + bLink$ + "</h6></div>"
        PRINT#f1,mlk$
        ' _____________________________________________________________________
        '/create right frame headings and basic links from directions in fileF$
        REDIM sx$(20): x1 = getarray("Describe",sx$(),fileF$)    '| LEFT LINKS HEADING
        x$ = SPACE$(20)
        IF ISTRUE x1 THEN
            IF LEN(TRIM$(sx$(2))) > 0 THEN
                x$ = "      <div><h5>" + TRIM$(sx$(2)) + "</h5></div>"
                PRINT#f1,x$
            END IF
            IF LEN(TRIM$(sx$(3))) > 0 THEN
                x$ = "      <div><h5>" + sx$(3) + "</h5></div>"
                PRINT#f1,x$
            END IF
        END IF
        REDIM sx$(20): x1 = getarray("EMail",sx$(),fileF$)    '| CONTACT EMAIL
        IF ISTRUE x1 THEN
           sx$(3) = single_quote_remove(sx$(3))
           y$ = TRIM$(sx$(3))
           IF INSTR(y$,"@")>0 THEN
                x$ = "      <div class='subbutton'><a href='mailto:" + LCASE$(y$) + "' target='tail'>" +_
                TRIM$(sx$(2)) + "</a></div><br>"
                PRINT#f1,x$
            END IF
        END IF
        REDIM sx$(20): x1 = getarray("Form",sx$(),fileF$)    '| FREE WEBSITE INFORMATION
        IF ISTRUE x1 THEN
            sx$(3) = single_quote_remove(sx$(3))
            y$ = TRIM$(sx$(3))
            IF LEN(y$)> 0 THEN
              x$ = "      <div class='subbutton'><a href='" + LCASE$(y$) + "' target='tail'>" + TRIM$(sx$(2)) + "</a></div><br>"
              PRINT#f1,x$ 'target='_blank'
            END IF
        END IF
        '\__________________________________________________________________________________/
        ' ___________________________________________________
        '/Output links to files in HTML from TEXT to Page
        a$ = "      <div class='subbutton'><a href='"
        b$ = "' target='tail'>"
        c$ = "</a></div>"
        were$ = HTMLfromTEXT$: nType$ = ".html"
        ' ____________________________________________________
        '/Extract .html Files listed in Folder !HTML_FROM_TEXT
        f2 = FREEFILE
        File$ = trash$ + "FileList.tmp"
        '-------------------------------------
        OPEN File$ FOR OUTPUT AS f2: CLOSE f2
        y = fetch_file(were$,File$,nType$)
        CALL Sort_File(File$,last)
        IF ERN > 0 THEN ERROR ERN
        IF Last > 0 THEN
            '_____________________________________________________
            '/Title of Files created From TCM outputted to Page
            REDIM sx$(20): xx$ = ""
            x1 = getarray("Describe",sx$(),fileF$)
            IF ISTRUE x1 THEN
                y$ = TRIM$(sx$(4))
                IF LEN(y$)> 0 THEN
                    xx$ = "      <h5>" + sx$(4) + "</h5>"
                    PRINT#f1,xx$
                END IF
            END IF
            ' __________________________________________________
            '/output each Processed name link to main page
            OPEN File$ FOR INPUT AS f2
                x$ = ""
                Image
                '----------------
                DO UNTIL EOF(f2)
                   INPUT#f2,x$
                   x$ = LCASE$(TRIM$(x$))
                   IF LEN(x$) > 3  THEN
                      x = LEN(HTMLfolders$)
                      were1$ = MID$(were$,x + 1)
                      IF (x$+nType$)<>"tail.html" THEN 'you can reach it with Home Link
                        lk$ = a$ + were1$ + x$ + nType$ + b$ + x$ + c$ +"<br>"
                        y = 1
                        Image
                        '--------------
                        DO UNTIL y = 0
                            y = INSTR(lk$,"\")
                            IF y > 0 THEN MID$(lk$,y,1) = "/"
                        LOOP
                        PRINT#f1,lk$
                      END IF
                    END IF
                LOOP
            CLOSE f2
            '--------
        END IF
        ' ______________________________________
        '/Output to Main Page Folder List Title
        REDIM sx$(20): x1 = getarray("Describe",sx$(),fileF$)
        IF ISTRUE x1 THEN
           y$ = TRIM$(sx$(5))
           IF LEN(y$) > 0 THEN
              x$ = "      <div><h6>" + sx$(5)+ "</h6></div>"
              PRINT#f1,x$
           END IF
        END IF
        ' _____________________________________________________________________________
        '/Output to main page links to Folders in !HTML FOLDERS
        ' __________________________________________________________
        '/Send to "directory.txt" the Folders names in '!HTML_FOLDERS
        CALL get_dir(HTMLfolders$)    '| Trash$  + "directory.txt"
        IF ERN > 0 THEN ERROR ERN
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        OPEN Trash$  + "directory.txt" FOR INPUT AS f3
                Image
            '----------------
            DO UNTIL EOF(f3)
               ' __________________________________
               '/Input Folder Name Determine Type
               INPUT#f3,y$
               x$ = UCASE$(TRIM$(y$))
               IF x$ = "DOWNLOADS" THEN nType$ = ".zip"  ELSE nType$ = ".html"
               were$ = HTMLfolders$ + x$ + "\"    '| Folder Directory
               IF LEFT$(x$,1) <> "@" AND LEFT$(x$,15) <> "!HTML_FROM_TEXT" THEN
                   INCR Link
                   LastLink$ = "AAAAA" + LTRIM$(STR$(Link))
                   LastLink$ = RIGHT$(LastLink$,4) + x$
                   nextLink = Link + 1
                   nextLink$ = "AAAAA" + LTRIM$(STR$(nextLink))
                   nextLink$ = RIGHT$(nextLink$,4) + x$
                   ' _________________________________
                   '/Folder Button Created
                   bLink$ = x$
                   mlk$ = "<div class='mainbutton'><a id='" + LastLink$ + "'  href='#" + nextLink$ + "' target='main'>" + bLink$ + "</a></div><br>"
                   were$ = HTMLfolders$ + x$ + "\"
                   File$ = trash$ + "FileList.tmp"
                   y = fetch_file(were$,File$,nType$)
                   IF ISTRUE y THEN
                      PRINT#f1,mlk$
                      TRY
                         FILECOPY HTMLfolders$ + tailground$,were$ + tailground$
                      CATCH
                      END TRY
                      CALL Sort_File(File$,last)
                      IF ERN > 0 THEN ERROR ERN
                      were$ = x$ + "\"
                      GOSUB main_folder_pages
                   END IF
                END IF
            LOOP
        CLOSE f3
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        '------------------------------------------
        OPEN FileStore$ FOR APPEND AS f3: CLOSE f3
        OPEN FileStore$ FOR INPUT AS f3
            Image
            '----------------
            DO UNTIL EOF(f3)
               LINE INPUT#f3,x$
               '----------------
               PRINT#f1,x$
            LOOP
        CLOSE f3
        '--------
        PRINT#f1,"  </div>"
        PRINT#f1,"  </body>"
        PRINT#f1,SPACE$(4)
        PRINT#f1,"</html>"
    CLOSE f1

'==============================================================================
    OPEN FileGoody$ FOR OUTPUT AS f1
        OPEN FileStyle1$ FOR INPUT AS f2
           Image
           '----------------
            DO UNTIL EOF(f2)
                LINE INPUT#f2,x$
                '----------------
                PRINT#f1,x$
            LOOP
        CLOSE f2
        '--------------------------------
        OPEN FileStyle2$ FOR INPUT AS f2
           Image
           '----------------
           DO UNTIL EOF(f2)
              LINE INPUT#f2,x$
              '----------------
              PRINT#f1,x$
           LOOP
        CLOSE f2
            '------------------------------
        OPEN FileWork$ FOR INPUT AS f2
            Image
            '----------------
            DO UNTIL EOF(f2)
                LINE INPUT#f2,x$
                '----------------
                PRINT#f1,x$
            LOOP
        CLOSE f2
    CLOSE f1
'===================================================================================
    SLEEP 250

EXIT SUB
main_folder_pages:
    a$ = "      <div class='subbutton'><a href='"
    b$ = "' target='tail'>"
    c$ = "</a></div>"
    f5 = FREEFILE             '| get two file numbers
    f4 = FREEFILE
    ' ______________________________________
    '/open and set up folder link page
    OPEN FileStyle1$ FOR APPEND AS f2
        x$ = TRIM$(STR$(maxres*1100))
        maxarea$ = "maxarea" + x$
        PRINT#f2,SPACE$(8) + "#" + maxarea$ + " {"
        'PRINT#f2,SPACE$(8) + "position: absolute;"
        'x$ = "top: " + x$ + "px;"
        'PRINT#f2,SPACE$(8) + x$
        '------------------------------------------
        OPEN Trash$ + "maxarea.tmp" FOR INPUT AS 4
            Image
            '---------------
            DO UNTIL EOF(4)
                LINE INPUT#4,y$: y$=TRIM$(y$)
                '-----------------------------
                y=(LCASE$(LEFT$(y$,9))="position:") OR (LCASE$(LEFT$(y$,4))="top:")
                IF ISFALSE y THEN
                    PRINT#f2,SPACE$(8) + y$
                END IF
            LOOP
        CLOSE 4
        '-------
    CLOSE f2
    '--------
    maxres=maxres+1
    ' ______________________________________________
    '/Return buttons and sub folder tail file links
    OPEN FileStore$ FOR APPEND AS f4
        PRINT#f4,"<div id='" + maxarea$ + "'>"
        mlk$ = "<div class='mainbutton'><a id='" + nextLink$+ "' href='main.html#" + mainLink$ + "' target='main'>" + UCASE$("return_home") + "</a></div><br>"
        PRINT#f4,mlk$
        mlk$ = "<div class='mainbutton'><a href='main.html#" + LastLink$ + "' target='main'>" + bLink$ + "</a></div><br>"
        PRINT#f4,mlk$
        ' ____________________________________________
        '/For each file found in Folder create a link
        OPEN File$ FOR INPUT AS f5
            Image
            '----------------
            DO UNTIL EOF(f5)
                INPUT#f5,lnk$
                lnk$ = TRIM$(lnk$)
                IF LEN(lnk$) > 0 THEN
                    lnk$ = a$ + were$ + lnk$ + nType$ + b$ + lnk$ + c$+"<br>"
                    PRINT#f4,lnk$
                END IF
            LOOP
        CLOSE f5
        '--------
        PRINT#f4,"  </div>"
    CLOSE f4
    '--------
RETURN
1800:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
FUNCTION hFrame_Tail ALIAS "z1900" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 1900
'------------------
f1 = FREEFILE
    TRY
        test=0: test$=Control$+file$+".txt": Image
    '---------------------------------------------------
        OPEN test$ FOR INPUT AS f1
            '----------------
            DO UNTIL EOF(f1)
                LINE INPUT#f1,x$: x$=TRIM$(x$)
                IF LEN(x$)>0 THEN test=test+1
                IF test>3 THEN EXIT DO
            LOOP
        CLOSE f1
        IF test<3 THEN
            Topic$ ="IN ["+ Test$+"] Empty File Problem?!"
            Problems(1) ="OPEN ["+Test$+"] check contents"
            Problems(2) ="Remove the file to create Default File!"
            Problems(3) ="or add three: <!-- no text wanted --"
            problem_file(Topic$,3)
        END IF
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        TRY
            test=0: test$=WebberText$+file$+".txt": Image
            OPEN test$ FOR INPUT AS f1
                '----------------
                DO UNTIL EOF(f1)
                    LINE INPUT#f1,x$: x$ = TRIM$(x$)
                     IF LEN(x$)>0 THEN test=test+1
                     IF test>3 THEN EXIT DO
                LOOP
            CLOSE f1
            IF test<3 THEN
                Topic$ ="IN ["+ Test$+"] Empty File Problem?!"
                Problems(1) ="OPEN ["+Test$+"] check contents"
                Problems(2) ="Remove the file to create Default File!"
                Problems(3) ="or add three: <!-- no text wanted --"
                problem_file(Topic$,3)
           END IF

        CATCH
            SLEEP 250: hFrame_Tail = -1
            FILECOPY (Control$+file$+".txt"), test$
        END TRY
        '''''''''''''''''''''''''''''''''''''''''''
CATCH
    CLOSE f1
    '--------
    hFrame_Tail = -1
    '----------------------------------------------------
    OPEN test$ FOR OUTPUT AS f1
            PRINT#f1,"!HTML_FOLDERS:-9"
            PRINT#f1,"Navigational Design:-1"
            PRINT#f1,"In the Left Main Frame Clicking a Link will Display the Linked File in the Right Tail Frame."

            PRINT#f1," If the Top Heading Frame remains unchanged, Clicking the Home Link will display this Page."

            PRINT#f1," If you click the last link in this document, travel up the link chain by Clicking each new link you reach."

            PRINT#f1,"Introduction:-1"
            PRINT#f1,Prm_name$+" Framed Web Site creator was written in 2005 by Hugh Polley."
            PRINT#f1,"The Progam Revised in: (&copy;2005/01/14,2007,2009,2010,2014,2021)."

            PRINT#f1,"The Author's E_mail is [hap2u@hotmail.com], TEXTWEBBER must be in subject line!:+1"
            PRINT#f1,"How Index Links Work:-1"
            PRINT#f1,"    The [Index Links] will Navigate to their [Subject Link]!:+1"
            PRINT#f1,"If you click the last link in this document you can travel up the"
            PRINT#f1,"[link chain] by Clicking each new link you reach.:+1"
    CLOSE f1
    '--------
    SLEEP 250
    FILECOPY (Control$+file$+".txt"),(WebberText$+file$+".txt")

END TRY
1900:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION instructions ALIAS "z2000" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2000

f1 = FREEFILE
TRY
        '--------------------------
        OPEN file$ FOR INPUT AS f1
            LINE INPUT#f1,x$
            '----------------
        CLOSE f1
        '--------
CATCH
    CLOSE f1
    '--------
    instructions = -1
    '---------------------------
    OPEN file$ FOR OUTPUT AS f1
        PRINT#f1,HTMLfromTEXT$+":-9:+9:+9"
        PRINT#f1,"1.0 The Technical Manual for Text_Webber!:-1"
        PRINT#f1,"    [Refer to the text file in Folder (@WEBBER_TEXT)] which created these:+1"
        PRINT#f1,"instructions to see an example of how [Text Code Marks (TCM)] work!:+1"
        PRINT#f1,"The program (Run_Me4WEBBER), hereafter referred to as 'The Program', creates a"
        PRINT#f1,"HTML Framed Web Site from its root folder and sub folders. HTML5 does not"
        PRINT#f1,"support Frameset code; therefore, the Program is not practical for creating"
        PRINT#f1,"Web sites on the internet. However, it is still helpful for creating HTML "
        PRINT#f1,"pages and recording Diplomacy Adjudicator results."
        PRINT#f1,"    The best place for The Program's root folder is on your WINDOW'S DESKTOP;"
        PRINT#f1,"a folder name such as TEXT WEBBER will work well. The Program uses HTML,"
        PRINT#f1,"Text-files(.TXT), and CSS files in the sub-folder (@WEBBER_TEXT) to generate"
        PRINT#f1,"your Framed Web Site's HTML documents. By changing these text and data files,"
        PRINT#f1,"you can have the Program change an existing HTML page's location and content "
        PRINT#f1,"or create a NEW page. It will be practical before using the Program to open a"
        PRINT#f1,"'WINDOWS EXPLORER' window. Then in the menu bar click 'VIEW' tab, then check"
        PRINT#f1,"the box for 'File name extensions'."
        PRINT#f1,"    The Text Files in the Folder (@CONTROL) affect the default output of your"
        PRINT#f1,"Web Site. Edit the Text.TXT Files in Folder (@WEBBER_TEXT) to ignore the"
        PRINT#f1,"(@CONTROL) files defaults. Edit the CSS Files in Folder (!HTML_FOLDERS) to"
        PRINT#f1,"ignore the (@WEBBER_TEXT) CSS files. NOTE: Do not edit HTML documents in"
        PRINT#f1,"folder ([!HTML_FOLDERS\!HTML_FROM_TEXT\]) as they will be over written:+1"
        PRINT#f1,"by operations elsewhere! You can place in the Folder (@HTML_HOLD_FILES),"
        PRINT#f1,"material referenced by your Text Code Marks in Folder (@WEBBER_TEXT). In"
        PRINT#f1,"addition the Program will call [Move_Graphic.exe] to copy JPG image files placed:+1"
        PRINT#f1,"in your Root Folder to Folder (@HTML_HOLD_FILES)."
        PRINT#f1,"[The Text Code Marks], hereafter referred to as TCM, allow you to:+7:+1"
        PRINT#f1,"program your text file's essential elements to produce your HTML documents.:-7"
        PRINT#f1,"    You can place HTML code in your text files as long as the sections"
        PRINT#f1,"enclosed by less than and greater than marks [<"
        PRINT#f1,"code>] are on the same line. Otherwise, the HTML code does not work correctly,"
        PRINT#f1,"often adding ENTITY HTML code."
        PRINT#f1,"Finally: you can use programs such as 'Libre Office', 'Grammarly' to:+7"
        PRINT#f1,"turn the HTML documents into PDF documents.:-7"
        PRINT#f1,""
        PRINT#f1,"2.0 The License Agreement:-1"
        PRINT#f1,"    The first time you run the Program, the Tail Frame will show the License"
        PRINT#f1,"Agreement instead of the (tail.txt) file! You will not find the file"
        PRINT#f1,"License.txt in the (@WEBBER_TEXT) because the Program creates it, then deletes"
        PRINT#f1,"it after the first run."
        PRINT#f1,"IF you wish TO use the CODE OR the program, please email me at:+7"
        PRINT#f1," (hap2u@hotmail.com) fOR EASY TO GET permission!:-7"
        PRINT#f1,""
        PRINT#f1,"3.0 An INDEX ORGANIZER: which is tempermental!:-1"
        PRINT#f1,"    The Program will organize a text file by any surrounded period"
        PRINT#f1,"identifiers, (2.a), it finds at the start of a [-1 or -2] INDEX line."
        PRINT#f1,"The format is limited to a maximum of nine characters, including the period."
        PRINT#f1,"[Form: AB99.cd99]"
        PRINT#f1,"    The Organizer is temperamental because if you ID one Index, you must ID"
        PRINT#f1,"all the Indexes, or the Organizer will list Indexes in unexpected ways. The"
        PRINT#f1,"one exception is moving one section of Indexes to the top and the other to the"
        PRINT#f1,"bottom."
        PRINT#f1,""
        PRINT#f1,"4.0 CSS CONTROLS THE APPEARANCE OF YOUR WEBSITE!:-1"
        PRINT#f1,"    The technical features of Cascading Style Sheets, CSS, can be found at"
        PRINT#f1,"{http://www.w3schools.com/css/}CSS Tutorials}.:+4"
        PRINT#f1,"The CSS files (tail.css), (main.css) and (head.css) control the Layout of "
        PRINT#f1,"your Web Site. Delete CSS files in (!HTML_FOLDERS), and the CSS in (@WEBBER_TEXT)"
        PRINT#f1,"replaces them. Delete CSS files in (@WEBBER_TEXT), and the CSS in (@CONTROL)"
        PRINT#f1,"replaces them. Delete CSS files in (@CONTROL), and they are replaced by"
        PRINT#f1,"(Setup.txt) default values."
        PRINT#f1,"    By editing (@CONTROL/Setup.txt), one can change the default Settings for"
        PRINT#f1,"CSS! AS well you can edit THIS File TO change other DEFAULT descriptions AND"
        PRINT#f1,"settings. In (@WEBBER_TEXT), if you replace (Frame.html) with (Frame2.html)"
        PRINT#f1,"instead OF a three-frame site, you will have a two-frame one."
        PRINT#f1,""
        PRINT#f1,"5.0 Spider Bots: gather info on your HTML pages!:+2"
        PRINT#f1,"The 'HTML head tag' meta statements help 'Bots' gather information on your"
        PRINT#f1,"Site. In (@Control\setup.txt), you can change some of these spider"
        PRINT#f1,"discriptions. In the (@WEBBER_TEXT) text files, you will find three Spider"
        PRINT#f1,"lines at the top of the File. Removing them does not work because the Program"
        PRINT#f1,"will create them again. I thought about redoing this part of the Code, but it"
        PRINT#f1,"might mess up other code sections."
        PRINT#f1,""
        PRINT#f1,"6.0 Creating HTML Pages With TEXT CODE MARKS!:-1"
        PRINT#f1,"    Start editing (tail.txt) in the (@WEBBER_TEXT) Folder to create your"
        PRINT#f1,"introduction page. You can also use this procedure to create a TCM designed"
        PRINT#f1,"(head.html) Frame."
        PRINT#f1,"    Create descriptive (!HTML_FOLDERS) Sub Folders, without '@' in their name,"
        PRINT#f1,"place already created HTML Pages and any supporting files into the"
        PRINT#f1,"'Sub Folder'. Links to the Folders are now in the Left Frame Folder List."
        PRINT#f1,"Clicking a FOLDER will list the HTML files in that Folder."
        PRINT#f1,"    Some supporting HTML files may show up because they have the (.html)"
        PRINT#f1,"extension instead of the (.htm) extension. Rename the Supporting HTML "
        PRINT#f1,"extensions to (.htm) and edit the remaining (.html) pages to call (.htm)."
        PRINT#f1,"    In (@CONTROL/!setup.txt) remove the Headings listed after the keyword"
        PRINT#f1,"'Describe' by changing the keyword to -Describe-. Your Web Site will now"
        PRINT#f1,"function only with those Folders containing your designed or created HTML."
        PRINT#f1,"    The Program translates text files [.txt] in the folder @WEBBER_TEXT into"
        PRINT#f1,"HTML files (.html) in accordance with the 'Text Code Marks'(TCM) at the end of"
        PRINT#f1,"the text lines. The resultant HTML files are placed in the default folder"
        PRINT#f1,"(!HTML_FOLDERS\!HTML_FROM_TEXT) unless redirected to a different folder by a"
        PRINT#f1,"TCM [:-9]."
        PRINT#f1,"    The File Names in each active Sub Folder of (!HTML_FOLDERS) create the"
        PRINT#f1,"Links in the Left Frame. Supporting HTML file extensions should be designated"
        PRINT#f1,"(.htm) and placed in your (@HTML_HOLD_FILES) Folder or they will show up as a"
        PRINT#f1,"Link in the Left Frame. Avoid using nondescript names for the left frame link"
        PRINT#f1,"derived from the (.html) name."
        PRINT#f1,"    The default files (head.txt), (tail.txt), and (html_from_text.txt),"
        PRINT#f1,"are created by The Program in the folder (@WEBBER_TEXT). You can edit these"
        PRINT#f1,"files but, except for (html_from_text.txt), if you delete them The Program"
        PRINT#f1,"will recreate the default versions."
        PRINT#f1,""
        PRINT#f1,"7.0 THE [!setup.txt] FILE :-1:+1"
        PRINT#f1,"    This File contains all the amendable default data for your Web Site;"
        PRINT#f1,"items such as your name, frameset sizes, Link Headings, colour layouts and"
        PRINT#f1,"personal contact data. For CSS changes to this File to take effect, delete all"
        PRINT#f1,"the CSS files in Folders @CONTROL and @WEBBER_TEXT."
        PRINT#f1,""
        PRINT#f1,"8.0 TEXT CODE MARKS: (abbreviation = TCM):-1"
        PRINT#f1,"    These marks are your secret weapon for quickly creating HTML Pages without"
        PRINT#f1,"knowing the vast HTML Rule and Tag library! When placed at the end of a line,"
        PRINT#f1,"a TCM tells The Program how to create an HTML Page! They are composed of a"
        PRINT#f1,"colon (:) and a negative or positive sign and a number (:+-1 to :+-9)! This HTML"
        PRINT#f1,"document [html_from_text.txt] in Folder [@WEBBER_TEXT] is an Instruction:+1"
        PRINT#f1,"File and an Example File."
        PRINT#f1,""
        PRINT#f1,"         9.0 Text Code Mark (TCM) Rules:-2"
        PRINT#f1,"    1. Generally, you place TCM Marks at the end of a text line in any order."
        PRINT#f1,"Index TCM (-1,2 and -2) will cause errors when used with nested TCM 8,7,6 or the"
        PRINT#f1,"Line returns TCM (-5,+5). Use TCM 5 after an index (-1,2,-2) to properly"
        PRINT#f1,"display following indented lines."
        PRINT#f1,"    2. TCM Marks (+5,-5) are not compatible with TCM Marks (-1,6,2,-2)."
        PRINT#f1,"    3 TC Marks (+8,+7,+6) require closing TC Marks (-8,-7,-6) and must be"
        PRINT#f1,"nested when used together."
        PRINT#f1,"    4. The 'TAB' key, or a blank text line, containing 4 spaces will separate"
        PRINT#f1,"HTML lines with a 4 space spacer line; an empty line containing no spaces will"
        PRINT#f1,"start a new line without a spacer!"
        PRINT#f1,"    5. Index (-1) can not be indented, a sub index (-2,2) can be indented."
        PRINT#f1,"An unindented line following a Sub Index  will run into the Index Line."
        PRINT#f1,"A Sub-Index (-2,2) text line Indented with spaces will indent the Index Line"
        PRINT#f1,"with spaces grouped in fours, to a max of 24."
        PRINT#f1,"    6. Link TCM -3,-4 and +4 can be used after a (:) with Index TCM(-1,2,-2)."
        PRINT#f1,"That way you can jump from a Webber page's index to the picture or link"
        PRINT#f1,"quickly. The Program will leave an empty line after such links for HTML"
        PRINT#f1,"formatting reasons."
        PRINT#f1,"    eg. [{tail.html} ~secondimage.jpg~ HOME}:-4:-3] will move the Intro:+1"
        PRINT#f1,"back into the tail Frame."
        PRINT#f1,"    7. Generally TCM Marks can be placed at the end of a text line in any"
        PRINT#f1,"order.:-5"
        PRINT#f1,"    That's it for the general rules, only nine TCM numbers, seven rules and"
        PRINT#f1,"TCM 3,4 syntax to remember or look up!"
        PRINT#f1,""
        PRINT#f1,"10.0 TCM -9 MOVE HTML FILE TO FOLDER:-2"
        PRINT#f1,"This tells The Program in what Folder to place an HTML file created from a"
        PRINT#f1,"text file. (!HTML_FOLDERS\!HTML_FROM_TEXT' is the default folder without"
        PRINT#f1,"TCM -9. This line can not be used with any other marks and must consist of"
        PRINT#f1,"just the Folder Name! [It is crucial] to place this mark below your spider:+1"
        PRINT#f1,"references which are the first three lines of code created on every "
        PRINT#f1,"(@WEBBER_TEXT) text file."
        PRINT#f1,"    i.e. [TEST_Folder:-9]:+1"
        PRINT#f1,"    Once the HTML page has been created in this way, removing its text"
        PRINT#f1,"file from (@WEBBER_TEXT) will not remove the HTML page from the Folder."
        PRINT#f1,""
        PRINT#f1,"11.0 TCM +9 CREATE INTERNAL STYLE TAGS:-2"
        PRINT#f1,"Rather than rely upon an external Style Sheet, this TCM creates a heading"
        PRINT#f1,"Style Sheet Code. With a TCM +9, the HTML page is more portable than with its"
        PRINT#f1,"default format."
        PRINT#f1,""
        PRINT#f1,"12.0 USE TCM +9 AGAIN TO CREATE A RIGHT COLUMN INDEX:-2"
        PRINT#f1,"    Use TCM +9 a second time to place the page's Index in the right column."
        PRINT#f1,"The default position for the Index is at the top of the page. When many index"
        PRINT#f1,"lines obscure the main text, placing the Index to the right may help."
        PRINT#f1,""
        PRINT#f1,"13.0 TCM (+8-8) CREATE BOLD FONT:-2"
        PRINT#f1,"A missing close TCM -8 will create an error message!"
        PRINT#f1,"    Example:"
        PRINT#f1,"    The Bold Font requires TCM -8 to end this font!:+8"
        PRINT#f1,"    With this example the TCM +8 is on the line above!"
        PRINT#f1,"    TCM -8 is at the end of this line!:-8:+5"
        PRINT#f1,"    The"
        PRINT#f1,"Bold Font:+8:-8"
        PRINT#f1,"of TCM 8 can throw off width restrictions,"
        PRINT#f1,"mess with TCM 6,7."
        PRINT#f1,""
        PRINT#f1,"14.0 TCM (+7-7) OBLIQUE PARAGRAPH FORMAT:-2"
        PRINT#f1,"TCM 7 creates an indented oblique paragraph independent of line breaks.:+7"
        PRINT#f1,"A missing close TCM -7 will generate an error message! The Program will"
        PRINT#f1,"adequately control the TCM execution if there are no nesting errors!:-7"
        PRINT#f1,""
        PRINT#f1,"15.0 TCM (+6-6) KEEP TEXT FORMAT:-2"
        PRINT#f1,"Format lines the way they are recorded!  This is useful when your information"
        PRINT#f1,"requires lines larger than (40em) in length or a simple table. This will"
        PRINT#f1,"work only with [monospace Fonts]. Note HTML does not [monospace chr(32)]:+1"
        PRINT#f1,"even with entity code, so HTML tables which use spaces will not line up properly."
        PRINT#f1,"+________________________|________________________|________________________|________________________|:+6"
        PRINT#f1,"+------------------------|------------------------|------------------------|------------------------|"
        PRINT#f1,"+------------------------|------------------------|------------------------|------------------------|"
        PRINT#f1,"+========================|========================|========================|========================|"
        PRINT#f1,"01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
        PRINT#f1,"'''''''''''''''''''''''''25'''''''''''''''''''''''50'''''''''''''''''''''''75'''''''''''''''''''''''100"
        PRINT#f1,"                         25                       50                       75                       100:-6"
        PRINT#f1,"    This TCM does not always play well with other TCM marks like"
        PRINT#f1,"TCM 8,7,5,2,-2 and -1. Because HTML created with this program uses Fixed"
        PRINT#f1,"length Font, table like information can be outputted without using the HTML"
        PRINT#f1,"table tags and format rules."
        PRINT#f1,""
        PRINT#f1,"16.0 TCM (+5,-5) LINE RETURNS:-2"
        PRINT#f1,"    TCM 5 lines only work perfectly when not affected by following indented"
        PRINT#f1,"lines or blank lines. The [-5] starts a new line without creating a blank"
        PRINT#f1,"line. The [+5] adds a following blank line with four spaces.:+5"
        PRINT#f1,"When creating an indented list after an Index TCM(-1,2) use a +5 on the"
        PRINT#f1,"a following blanck line."
        PRINT#f1,":+5"
        PRINT#f1,"    I am a new line after a TCM +5.:-5"
        PRINT#f1,"    I am a new line after a TCM -5"
        PRINT#f1,""
        PRINT#f1,"17.0 TCM (+4-4) CREATE A LINK:-2"
        PRINT#f1,"    TCM 4 creates a link using three upper case brackets:"
        PRINT#f1,"{tail.html}Home_Page}. Place inside these brackets; the address of a File"
        PRINT#f1,"and label of a Link in the form:"
        PRINT#f1,"    {(http://)(URL address)(#place on page)}name_label}"
        PRINT#f1,"    A TCM +4 link calls up a new HTML page external to the HTML Frame subset,"
        PRINT#f1,"while a TCM -4 sends the page to the Right Frame. For an internal page link, "
        PRINT#f1,"place a [#] sign in front of the address."
        PRINT#f1,"   You can use a TCM -3 to create an icon image for your Link. Use software"
        PRINT#f1,"such as Paint to Reduce the Image resolution to approximately 300X220_PX."
        PRINT#f1,"    TCM (-3+4)  Example:"
        PRINT#f1,"    [{http://ca.yahoo.com/} ~secondimage.jpg~ Yahoo Canada}:+4:-3]"
        PRINT#f1,"    Image Links are temperamental and seek their own space."
        PRINT#f1,"{http://ca.yahoo.com/} ~secondimage.jpg~ Yahoo Canada}:+4:-3"
        PRINT#f1,"    If a web address indicator [/] is not in the address, The Program will"
        PRINT#f1,"attempt to copy the File in the address from the "
        PRINT#f1,"[!HTML_FOLDERS\@HTML_HOLD_FILES] to the Folder in use by the Link!"
        PRINT#f1,""
        PRINT#f1,"18.0 TCM (+-3) CREATE AN INLINE IMAGE OR TRANSFER A FILE :-2"
        PRINT#f1,"    [This Mark requires the up case wave mark] above the tab key of most:+1"
        PRINT#f1,"computers. Use this form with spaces [ ~image or file name~ ] followed by"
        PRINT#f1,"TCM -3 for an image or TCM +3 to transfer a file from the"
        PRINT#f1,"HOLD_HTML_FILES Folder.:+5"
        PRINT#f1,"    Examples:"
        PRINT#f1,"    The wave Code inside the brackets [~secondimage.jpg~] will not be displayed."
        PRINT#f1,"+3 will move the file to (HTNL_FROM_TEXT). -3 will display the Image!:-5"
        PRINT#f1,"    Image or file moved: [ ~secondimage.jpg~ ]:+3"
        PRINT#f1,"    The Image is displayed:-5"
        PRINT#f1,"    [ ~secondimage.jpg~ ]:-3:-5"
        PRINT#f1,"    Logo Image files (.jpg) used for links should be no larger in size"
        PRINT#f1,"than 320X200_PX and if smaller should maintain this aspect ratio!"
        PRINT#f1,"    When you can manage it the best size for regular photos"
        PRINT#f1,"is 800X450_PX this gives a 16/9 ratio frame."
        PRINT#f1,""
        PRINT#f1,"19.0 TCM (-2,+2) SUB INDEX ENTRY:-2"
        PRINT#f1,"Creates a Sub Index to a Main Index Entry and links to it!  Upon arrival at"
        PRINT#f1,"the sub-index HTML Label, a Left Click will navigate to the Previous Main"
        PRINT#f1,"Index Label before the used index link. As in a Main Index, placing a colon"
        PRINT#f1,"(:) in the line will reduce the length of the highlight.:-5"
        PRINT#f1,"        Example: "
        PRINT#f1,"        (This: is a Sub link:-2)"
        PRINT#f1,"        (This) - Will be the link name!:+5"
        PRINT#f1,"The TCM+2 works the same way as the TCM-2, except it will not show up in the"
        PRINT#f1,"Page's Index. The use of TCM (8,7,6 or 5) with TMC (-1,-2,+2) may create problems."
        PRINT#f1,""
        PRINT#f1,"20.0 TCM +1 [UNDERLINE TEXT]:+1:-2"
        PRINT#f1,"This Mark will remove rectangle brackets and underline the enclosed text."
        PRINT#f1,"    Form: ( [This text is underlined] :+1) uses the HTML tag 'EM' and CSS"
        PRINT#f1,"to achieve the below result.:-5"
        PRINT#f1,"    [This text is underlined], (this text is not!):+1:-5"
        PRINT#f1,""
        PRINT#f1,"21.0 TCM -1 MAIN INDEX ENTRY:-2"
        PRINT#f1,"Creates a Main Index Entry and links to it!  Upon arrival at the HTML Label,"
        PRINT#f1,"a Left Click will navigate back to the Main Index link before the used index"
        PRINT#f1,"link. Placing a colon (:) in the line will reduce the length of the Link.:+5"
        PRINT#f1,"        Example: "
        PRINT#f1,"        (This: is a main link:-1)"
        PRINT#f1,"        (This) - will be the link name!:+5"
        PRINT#f1,"The use of TCM (8,7,6 or 5) with TMC (-1,-2,+2) may create problems."
        PRINT#f1,""
        PRINT#f1,"22.0 THE FOLDERS: Create the Web Site's Structure!:-1"
        PRINT#f1,"    The [AT(@) Folders] add to or aid in the process of creating the documents"
        PRINT#f1,"found in your Exlamaintion (!) folders. The (@) folders do not change unless"
        PRINT#f1,"edited or deleted. The (!) Folders change almost with every run of"
        PRINT#f1,"The Program."
        PRINT#f1,"So copy creations you wish to keep using (!HTML_FOLDERS:-9) into a more:+7"
        PRINT#f1,"permanent subfolder in the folder (!HTML_FOLDERS).:-7 "
        PRINT#f1,""
        PRINT#f1,"23.0 THE [!GOODCOPY FOLDER]: last AS2 Text copy of your documents!:+2:+1"
        PRINT#f1,"    This folder contains processed text files created from files in"
        PRINT#f1,"@WEBBER_TEXT. Load one or more of these text files into a word processor to "
        PRINT#f1,"create other document types such as (html_from_text.doc)."
        PRINT#f1,""
        PRINT#f1,"24.0 THE [@CONTROL FOLDER]: contains your default Folders!:+2:+1"
        PRINT#f1,"This Folder contains default files and Information Files, which can be changed"
        PRINT#f1,"or adjusted after running 'TEXT WEBBER' for the first time. The information"
        PRINT#f1,"gathered during the programs execution can be helpful, check out:"
        PRINT#f1,"[file_found.txt] , [file_not_found.txt] , [file_required.txt] ,:+1"
        PRINT#f1,"[files_in_folders.txt] , [files_in_hold.txt]:+1"
        PRINT#f1,""
        PRINT#f1,"25.0 The [@WEBBER_TEXT] Folder: The guts of your Web Site!:+2:+1"
        PRINT#f1,"To create your HTML Pages, you can use any Word Processor that saves the"
        PRINT#f1,"text in ASCII [.txt] format. You do not have to use 'Hyper Text Markup"
        PRINT#f1,"Language'; all you need to know are the TCM codes!"
        PRINT#f1,"    The Program will list the created pages in this Folder in the left"
        PRINT#f1,"[main] frame by file name. Notepad, or the "
        PRINT#f1,"Text Editor {http://liquidninja.com/metapad/} MetaPad}:+4"
        PRINT#f1,"can be used to create or edit your text files."
        PRINT#f1,"    The default files in this Folder are Head.txt, Tail.txt and"
        PRINT#f1,"[Html_from_text.txt]. You can Edit the Files in this Folder to display"
        PRINT#f1,"something different from the default file's information. Deleting"
        PRINT#f1,"[@WEBBER_TEXT\HTML_FROM_TEXT.TXT] will remove it from your left Frame Links."
        PRINT#f1,"You can place the File in another folder for future reference by using TCM-9."
        PRINT#f1,"Make [@WORK:-9] the first line in the text body. You will now find this"
        PRINT#f1,"Document in Folder [!HTML_FOLDERS/@WORK]."
        PRINT#f1,""
        PRINT#f1,"26.0 The [!HTML_FOLDERS]: Contains your Web Site!:+2:+1"
        PRINT#f1,"    Located in this Folder are the sub-folders and HTML documents plus the CSS"
        PRINT#f1,"files that control your three-frame WEB SITE structure. The essential File is"
        PRINT#f1,"[MAIN.HTML] because it changes each time you run The Program, and it contains"
        PRINT#f1,"links to the HTML pages stored on the Web Site."
        PRINT#f1,""
        PRINT#f1,"27.0 The [HTML_HOLD_FILES] Folder: Contains the files used by your HTML documents!:+2:+1"
        PRINT#f1,"    Located in [HTML_HOLD_FILES] are the HTM, CSS and Graphic files required"
        PRINT#f1,"or called upon by your HTML pages. Except for the graphic files, you need to"
        PRINT#f1,"place files called by your [@WEBBER_TEXT] text(.TXT) files here. Graphic"
        PRINT#f1,"(.JPG) files placed in the root folder will be moved to this Folder"
        PRINT#f1,"by [Move_Graphics.EXE]."
        PRINT#f1,""
        PRINT#f1,"28.0 The [!HTML_FROM_TEXT] Folder: contains the Last HTML documents created!:+2:+1"
        PRINT#f1,"This is the Default Folder for HTML documents converted from the Text Files"
        PRINT#f1,"in Folder [@WEBBER_TEXT]. Running The Program cleans out all previous files"
        PRINT#f1,"in this Folder, therefore place image and HTML files that your text files"
        PRINT#f1,"require in Folder [@HTML_HOLD_FILES]. Editing files here is a mistake you will"
        PRINT#f1,"not likely repeat."
        PRINT#f1,"    When you run The Program, files referenced by TCM 3 or 4 will be copied to"
        PRINT#f1,"Folder (!HTML_FROM_TEXT) or to the Folder referenced by TCM -9. For missing"
        PRINT#f1,"Files, you can check the [@CONTROL\file_not_found.txt] File for more path and"
        PRINT#f1,"File information to aid you in determining what file is missing."
        PRINT#f1,""
        PRINT#f1,"29.0 The [DOWNLOADS] Folder: contains public ZIP files:+2:+1"
        PRINT#f1,"    Place zipped files (.zip) you wish visitors to download here!"
        PRINT#f1,"After running the program Clicking DOWNLOADS will display these files,"
        PRINT#f1,"clicking the file will start the download process. Place (Trial4Webber.zip) in"
        PRINT#f1,"your root folder, run the program, you will now find this file in your Web"
        PRINT#f1,"Site's DOWNLOAD folder."
        PRINT#f1,""
        PRINT#f1,"30.0 CREATE OTHER FOLDERS:-1"
        PRINT#f1,"You can use TCM -9 to create a Folder and place the generated HTML Page"
        PRINT#f1,"there. The Program will also move any Image files referenced with a "
        PRINT#f1,"TCM(4 or 3) stored in the [@HTML_HOLD_FILES] to the created Folder."
        PRINT#f1,"Clicking a FOLDER will list the HTML files links in that folder."
        PRINT#f1,"    Any HTML files not created directly by the program placed in a Folder will"
        PRINT#f1,"show up as a link. To have a supporting HTML document not show up as a link"
        PRINT#f1,"give it the (.htm) extension, then edit the calling HTML document to reflect"
        PRINT#f1,"the changed extension."
        PRINT#f1,""
        PRINT#f1,"31.0 THE [head Document] FILE :+1:-1"
        PRINT#f1,"Controls your Heading frame output; this is a good spot for Favorite Links!"
        PRINT#f1,"In (@WEBBER_TEXT), you can change its content or delete it to get back the"
        PRINT#f1,"Default File. The default file contains links to the official Markup"
        PRINT#f1,"Validation Services. Click on the HTML validator and use it to validate your"
        PRINT#f1,"HTML pages!"
        PRINT#f1,""
        PRINT#f1,"32.0 THE [TAIL FRAME] DOCUMENT:-1:+1"
        PRINT#f1,"The Program creates text File (Tail.txt) in Folder (@TEXT_WEBBER) when it is"
        PRINT#f1,"not there. The user can edit (Tail.txt) to suit his or her needs. Then,"
        PRINT#f1,"display this page by left-clicking the HOME heading link! Once you have"
        PRINT#f1,"created or obtained a DESIGNER page, rename it [tail.html] and place it in"
        PRINT#f1,"the (!HTML_FOLDERS) Folder. Make (@WORK:-9) the first entry in"
        PRINT#f1,"(@TEXT_WEBBER\tail.txt) file below your spider entries. Now (Tail.txt) in"
        PRINT#f1,"Folder (@TEXT_WEBBER) will not overwrite the designed file (tail.html)."
        PRINT#f1,""
        PRINT#f1,"33.0 THE [INDEX.HTML] DOCUMENT:-1:+1"
        PRINT#f1,"This Document is your Cover Page, a graphic page linked to your Site's"
        PRINT#f1,"interactive Web Pages. [Index.html] will display any graphic in your ROOT"
        PRINT#f1,"Folder named 'firstimage.jpeg'! If you name a designer page (index.html) and"
        PRINT#f1,"replace the original File, The Program will not overwrite it."
        PRINT#f1,""
        PRINT#f1,"34.0 [@WEBBER TEXT] BACKUP: saves month and year versions of your TCM documents!:-1:+1"
        PRINT#f1,"    With each running of The Program, new (@WEBBER TEXT) files are backed up"
        PRINT#f1,"one time to the month folder in (@WEBBER_TEXT_BACKUP). In addition, The"
        PRINT#f1,"Program backs up the current version of a text file to the Year Folder. The"
        PRINT#f1,"Program creates a new MONTH Folder after the first day of the month and"
        PRINT#f1,"one-time backups of existing files. When the Year date changes, files in the"
        PRINT#f1,"previous Year do not get placed in the new Year's Folder. If you happen to"
        PRINT#f1,"think of it; in January review and select the old text files you wish to keep"
        PRINT#f1,"from the previous year's folder and paste then to the new year folder, then"
        PRINT#f1,"delete the previous year folder."
        PRINT#f1,"Every month, the Program creates a New Month Backup Folder:+7"
        PRINT#f1,"which overwrites the same month Folder from the previous Year:-7"
        PRINT#f1,""
        PRINT#f1,"35.0 CLEAN UP UN-USED FILES: get rid of all the accumulated junk!:-1"
        PRINT#f1,"    Over time your Web Site will fill up with junk Files not being used."
        PRINT#f1,"[You can clean up unused files] by renaming the (@HTML_HOLD_FILES) Folder:+1"
        PRINT#f1,"as (@DELETE) then run the program. The Program will attempt to move required"
        PRINT#f1,"files from Folder [@DELETE] to the newly created [@HTML_HOLD_FILES].:+1"
        PRINT#f1,"This process may create some error messages. [Ignore these messages] and:+1"
        PRINT#f1,"rerun the Program. If the program detects no missing files you have cleaned"
        PRINT#f1,"up your Web Site. If after doing this procedure the program reports some not"
        PRINT#f1,"found files then try to find these files and place them in the "
        PRINT#f1,"[@HTML_HOLD_FILES] Folder.:+1"
        PRINT#f1,"    If Disk Space is not an issue; Cut and Paste the [@DELETE] Folder to:+1"
        PRINT#f1,"[@WEBBER TEXT\@WEBBER_TEXT_BACKUP\]. That way if you wish to bring an old:+1"
        PRINT#f1,"support file back into service, you may still have it in the backup Folder"
        PRINT#f1,"[@DELETE].:+1"

        CLOSE f1
        '--------
        FILECOPY file$,WebberText$ + "html_from_text.txt"
    FINALLY
        CLOSE f1
        '--------
    END TRY
2000:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION Create_Index_Page ALIAS "z2100" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2000
''''''''''''''''''''''
ff = FREEFILE
gg = FREEFILE
'''''''''''''''''''''''
TRY
    '--------------------------
    OPEN file$ FOR INPUT AS ff
    CLOSE ff
    '--------
CATCH
    CLOSE ff
    '--------
    Create_Index_Page = -1
' ________________________________________________
'/get data from setup.txt
    REDIM sxx$(20)
    x1 = getarray("index html",sxx$(),fileF$)
    messg$ = "ENTER WEB SITE HERE"
    y$ = TRIM$(sxx$(2))
    IF LEN(y$) > 0 THEN messg$ = UCASE$(y$)
    '''''''''''''''''''''''''''''''''''''''''
    REDIM sxx$(20)
    '---------------------------
    OPEN file$ FOR OUTPUT AS ff
        x$ = WebberText$ + "DOCTYPE_DECLARE.tmp"
        '-----------------------
        OPEN x$ FOR INPUT AS gg
            Image
            '----------------
            DO UNTIL EOF(gg)
                LINE INPUT#gg,y$
                '----------------
                PRINT#ff,y$
            LOOP
        CLOSE gg
        '--------
        x$ = WebberText$ + "DOCTYPE_heading.tmp"
        '-----------------------
        OPEN x$ FOR INPUT AS gg
            Image
            '----------------
            DO UNTIL EOF(gg)
                LINE INPUT#gg,y$
                '----------------
                IF INSTR(y$,"</head")>0 THEN
                    PRINT#ff,"    <link rel='stylesheet' TYPE='text/css' media='screen' href='tail.css'>"
                    PRINT#ff,y$
                    EXIT DO
                ELSEIF INSTR(y$,"<link rel")=0 THEN
                    PRINT#ff,y$
                END IF
            LOOP
        CLOSE gg
         '--------
        PRINT#ff,SPACE$(4)
        PRINT#ff,"  <body ID='body_index_page' >"
        PRINT#ff,"      <div class='Frame_inset'>"
        PRINT#ff,"          <img class='Frame_scaled' src='" + firstimage$ + "' alt='" + firstimage$ + " not found'>"
        PRINT#ff,"          <p class='p_in_image'><a href='" + HTMLfolders$ + "frame.html'>" + messg$ +"</a></p>"
        PRINT#ff,"       </div>"
        PRINT#ff,"  </body>"
        PRINT#ff,SPACE$(4)
        PRINT#ff,"</html>"
    CLOSE ff
    '--------
END TRY
'x=INSTR(firstimage$,".")
'IF x>0 THEN firstimage$=LEFT$(firstimage$,x-1)
'? ' ,2,"firstimage$=[";firstimage$;"]"
2000:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION DirCopy ALIAS "z2200" (were$,going$,outfile$,txType$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2200

f2 = FREEFILE
'----------------------------------------
OPEN outFile$ FOR OUTPUT AS f2: CLOSE f2
y = fetch_file(were$,outFile$,txType$)
IF y THEN
    y = 1
    '---------------------------------------
    OPEN outFile$ FOR APPEND AS f2:CLOSE f2
    OPEN outFile$ FOR INPUT AS f2
        opFile$ = ""
        Image
        '----------------
        DO UNTIL EOF(f2)
            INPUT#f2,opFile$
            opFile$ = LTRIM$(RTRIM$(opFile$))
            IF LEN(opFile$) > 1 THEN
                FILECOPY were$ + opFile$ + txType$,going$ + opFile$ + txType$
                y = -1
            END IF
        LOOP
    CLOSE f2
    '--------
END IF

2200:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
SUB get_dir ALIAS "z2300" (wereDIR$)
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 2300
'------------------
    DIM Listing$(1 TO 1000)
    DIM u&
    DIR$ CLOSE
    A$ = DIR$(wereDIR$,16)
    WHILE LEN(A$) AND u& < 1000    '| max = 1000
        INCR u&
        Listing$(u&) = A$
        A$ = DIR$
    WEND
    ARRAY SORT Listing$() FOR u&
    d1 = FREEFILE
    '-----------------------------------------------
    OPEN Trash$  + "directory.txt" FOR OUTPUT AS d1
        Image
        '---------------
        FOR x = 1 TO u&
            z = (UCASE$(Listing$(x)) = Listing$(x)) AND (MID$(Listing$(x),1,1) <> "@")
            IF  z THEN
                PRINT#d1,Listing$(x)
            END IF
        NEXT x
    CLOSE d1
    '--------
2300:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
FUNCTION getarray ALIAS "z2400" (varName$,sx$(),file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2400

    IF LEN(LTRIM$(file$)) = 0 THEN file$ = Control$ + "!setup.txt"
    file1$ = trash$ + "getarray.tmp"
    sa = FREEFILE
    REDIM sx$(20)
    TRY
        '--------------------------
        OPEN file$ FOR INPUT AS sa
    CATCH
        CLOSE sa
        '--------
        EXIT FUNCTION
    END TRY
        Image
        '----------------
        DO UNTIL EOF(sa)
            LINE INPUT#sa,x$
            '----------------
            x = INSTR(x$,",")    '| information line
            IF x > 0 THEN
                x$ = TRIM$(x$)
                sb = FREEFILE
                '---------------------------------------------------
                OPEN file1$ FOR OUTPUT AS sb    '| output for input
                    PRINT#sb,x$
                CLOSE sb
                '--------
                '---------------------------------------------------
                OPEN file1$ FOR INPUT AS sb    '| input from output
                    INPUT#sb,sx$(1)
                    sx$(1) = TRIM$(sx$(1))
                    'found peramiter no matter case
                    IF LCASE$(sx$(1)) = LCASE$(varName$) THEN
                        sx = 1
                        Image
                        '----------------
                        DO UNTIL EOF(sb)
                            INPUT#sb,x$
                            x$ = TRIM$(x$)
                            IF x$ = "/" OR x$ = "\" THEN EXIT DO
                            'rgb first color found
                            IF UCASE$(LEFT$(x$,3))="RGB" THEN
                                INPUT#sb,x1$    '| second color
                                INPUT#sb,x2$    '| third color
                                x$ = x$ + "," + x1$ + "," + x2$
                            END IF
                            sx = sx + 1
                            'if len(x$)>0 then replace ANY space$(1) WITH "_" IN x$
                            sx$(sx) = x$
                        LOOP
                        CLOSE sb
                        '--------
                        getarray = -1
                        EXIT DO
                    END IF
                CLOSE sb
                '--------
            END IF
        LOOP
    CLOSE sa,sb
    '-----------
2400:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION fetch_all ALIAS "z2500" (were$,File$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2500

    LOCAL ff AS LONG,gg AS LONG, fPath AS STRING,hPath AS STRING, fName AS STRING, sBuf AS STRING
    ' __________________________________________________________
    '/construct directory path
    fPath = CURDIR$
    IF RIGHT$(fPath, 1) <> "\" THEN fPath = fPath + "\"
    fPath = fPath + were$     '| adjust path if needed
    IF RIGHT$(fPath, 1) <> "\" THEN fPath = fPath + "\"
    ' _________________________________________________
    '/open text files storage file
    ff =FREEFILE              '| Get free file handle
    '-----------------------------------------------------------------------------
    OPEN File$ FOR OUTPUT AS ff: CLOSE ff    '| clear directory file name records
    ' ___________________________________________
    '/search for file type and add to File
    fName = DIR$(fPath + "*.*")
    y=0
    WHILE LEN(fName)
        ff =FREEFILE          '| Get free file handle
        '-----------------------------------------------------------------
        OPEN File$ FOR APPEND AS ff    '| open file with directories name
            fName = LTRIM$(RTRIM$(fName))
            IF LEN(fName) > 0 THEN
                tName$ = LCASE$(fName)
                Image
                '------------------------
                FOR x = 1 TO LEN(tName$)
                    IF MID$(tName$,x,1) = SPACE$(1) THEN MID$(tName$,x,1) = "_"
                NEXT x
                IF fName <> tName$ THEN
                    NAME fPath + fName AS fPath + tName$
                END IF
                PRINT#ff,tName$
                y = -1
            END IF
        CLOSE ff              '| close file handle
        '------------------------------------------
        fName = DIR$          '| any more *.txt files
    WEND
    fetch_all = y

2500:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION get_endline_number ALIAS "z2600" (wPar$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2600

    x = -LEN(wPar$): Acc$ = ""
    DO
        y$ = MID$(wPar$,ABS(x),1)
        y = (ASC(y$) > 47) * (ASC(y$) < 58)
        IF y THEN
            Acc$ = y$ + Acc$
        ELSE
            EXIT DO
        END IF
        x = x+1
    LOOP UNTIL x = 0
    IF LEN(Acc$) > 0  THEN
        wPar$ = LEFT$(wPar$,ABS(x))
        get_endline_number = VAL(Acc$)
    END IF

2600:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION Folder_Fix ALIAS "z2700" (x$) AS STRING
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2700

LOCAL xx$,FLD$
    xx$ = TRIM$(x$)
    IF RIGHT$(xx$,1) = "\" THEN xx$ = LEFT$(xx$,LEN(xx$)-1)
    xx$ = TRIM$(xx$)
    IF LEN(xx$) = 0 THEN EXIT FUNCTION
    xx$ = UCASE$(xx$)
    FLD$ = xx$
    yes = 0
    Image
    '---------------------
    FOR x = 1 TO LEN(xx$)
        y = ASC(MID$(xx$, x, 1))
        z = (y=33) OR (y > 47 AND y < 58) OR (y > 63 AND y < 91)
        IF z = 0 THEN MID$(xx$, x, 1) = SPACE$(1)
    NEXT x
    DO
        xx$ = TRIM$(xx$)
        x = INSTR(xx$,SPACE$(1))
        IF x>0 THEN
            a$ = LEFT$(xx$,x)
            b$ = MID$(xx$,x)
            a$ = TRIM$(a$)
            b$ = TRIM$(b$)
            xx$ = a$ + "_" + b$
        END IF
    LOOP UNTIL x = 0
    Folder_Fix  = xx$
    IF xx$ <> FLD$ THEN
        Topic$ = "8 IN -> " + opFile$ + "-> Folder Name Error"
        Problems(1) = "Bad Folder Name: " + FLD$
        Problems(2) = "Changed to: " + xx$
        Problems(3) = "This can cause problems with existing Folders!"
        problem_file(Topic$,3)
    END IF

2700:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION tags ALIAS "z2800" (xx$,z) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2800
    'are tags after line point z
    REDIM rt$(3,1)
    rt$(1,0)="<": rt$(1,1)=">"
    rt$(2,0)="{": rt$(2,1)="}"
    rt$(3,0)="~": rt$(3,1)="~"
    found = LEN(xx$)
    Image
    '--------------
    FOR r = 1 TO 3
        y = INSTR(z,xx$,rt$(r,0))
        t = INSTR(z,xx$,rt$(r,1))
        IF y = 0 THEN GOTO exit_r
        IF t > 0 AND y < t THEN
            IF y<found THEN found = y
        END IF
        exit_r:
    NEXT r
    FUNCTION = found
2800:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION backup_tab ALIAS "z2900" (file$) AS STRING
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 2900
'------------------
    REDIM FileOut$(200)
    FileOrg$ = WebberText$ + file$ + ".TXT"
    FileYear$ = YearFolder$ + file$ + ".BAK"
    FileBak$ = WebberTextBackup$ + file$ + ".BAK"
    File1$ = Trash$ + file$ + "1.tmp"
    File2$ = Trash$ + file$ + "2.tmp"
    ff=FREEFILE
    gg=FREEFILE
    hh=FREEFILE
    ' _________________________________________________
    '/Save to month backup if month change or new file \
     '''''''''''''''''''''''''''''''''''''''''''''''''''
    TRY
        OPEN FileBak$ FOR INPUT AS ff
            LINE INPUT#ff,x$
            '----------------
        CLOSE ff
        '--------
    CATCH
        CLOSE ff
        '--------
        FILECOPY FileOrg$,FileBak$
    END TRY
    ' __________________________________
    '/Create Backup in Year Folder      \
    OPEN FileYear$ FOR OUTPUT AS gg
        OPEN FileOrg$ FOR INPUT AS ff
            Image
            '-------------------------------------------------------------
            DO UNTIL EOF(ff)    '| eliminate blank lines at start of File
                LINE INPUT#ff,yy$
                '-----------------
            LOOP UNTIL LEN(TRIM$(yy$))>0
            IF EOF(ff) THEN
                CLOSE gg,ff
                '-----------
                backup_tab="-1"
                Topic$ ="9 IN " + FileOrg$+ " NO Info found!"
                Problems(1) = "File contains only empty lines!"
                Problems(2) = "Check " + FileBak$ + " for last good copy!"
                Problems(3) = "Repair or remove file!"
                problem_file(Topic$,3)
                EXIT FUNCTION
            END IF
            yy$ = TAB$(yy$,4)    '| turn tabs into spaces
            PRINT#gg,yy$
            ''''''''''''''''''''''''''''''''''''''''''''''
            DO UNTIL EOF(ff)
                LINE INPUT#ff,yy$
                '-----------------
                yy$ = TAB$(yy$,4)    '| turn tabs into spaces
                PRINT#gg,yy$
            LOOP
        CLOSE ff
        '--------
    CLOSE gg
    '--------

    ' _________________________________________________
    '/Sort peroid indexes using Array FileOut$(number)
    alfa_main$="!!!!.!!!!"
    alfa_subb$="!!!!.!!!!"
    number = 1
    FileOut$(number) = "!_"+alfa_main$+"_"+alfa_subb$+"_000.tmp"

    'sort will list numbers then upper case then lower case
    ' ___________________________________
    '/Create Sortable Array from Indexes \
    OPEN FileOrg$ FOR INPUT AS ff
        OPEN trash$+FileOut$(number) FOR OUTPUT AS gg
            TC=0: TCM=TC 'keep record of last index TC
            Image
            '----------------

            DO UNTIL EOF(ff): ct1=0: ct2=0
                ''''control blank lines '''''''''
                DO UNTIL EOF(ff)
                    LINE INPUT#ff,xx$
                    yy$=TRIM$(xx$)
                    IF LEN(yy$)=0 AND LEN(xx$)>3 THEN ct1=ct1+1
                    IF LEN(yy$)=0 THEN ct2=ct2+1
                LOOP UNTIL LEN(yy$)>0
                xx$=RTRIM$(xx$) 'cleanup spacing
                IF ct1>0 THEN PRINT#gg,SPACE$(4) 'spacing lines detected
                IF ct2>0 THEN PRINT#gg,SPACE$(0) 'spacing lines detected
               ''''''''''''''''''''''''''''''''''''''''''''''''''''''
                '_________________________________________
                DO'/check Single Line for all TCM on Line \
                    TC=dProcess(yy$)
                    IF TC=3 OR TC=-3 THEN GOSUB check_no_space_name
                LOOP UNTIL TC=0 OR TC=-1 OR ABS(TC)=2
                ''''''Break line before Index '''''''''''
                IF ABS(TC=2) OR TC=-1 THEN PRINT#gg,SPACE$(0)
                '''''''''''''''''''''''''''''''''''''''''''''
                IF TC=-1 THEN    '| create new alfa_main$
                    number=number+1
                    nb$="000"+TRIM$(STR$(number))
                    nb$=RIGHT$(nb$,3)
                    alfa_subb$="!!!!.!!!!"
                    'output if main ID not determined
                    FileOut$(number)="!_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                    x=INSTR(yy$,".")
                    y=INSTR(yy$,SPACE$(1))
                    IF y>x AND y>x+1 AND x<6 AND x>1 THEN '| new main ID found
                    'space after period, element after peroid, 1 to 5 elements before period
                        y1$=TRIM$(LEFT$(yy$,x-1)) '| before period
                        y2$=TRIM$(MID$(yy$,x+1,y-x))
                        ''''''''''''''''''''''''''''''
                        as1=check_as2 (y1$)
                        as2=check_as2 (y2$)
                        IF as1+as2<>-2 THEN
                            Topic$ = "ASc2 check Failed!"
                            Problems(1) = "An Error in an Index Identifier?"
                            Problems(2) = "This can cause a problems with Index ordering!"
                            problem_file(Topic$,2)
                        ELSE 'main ID determined
                            y1$=RIGHT$(("!!!!" + y1$),4)
                            y2$=RIGHT$(("!!!!" + y2$),4)
                            y0$=y1$+"."+y2$
                            alfa_main$ =y0$
                            FileOut$(number)="!_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                        END IF
                    END IF
                    '-----------------------------------------------

                    CLOSE gg
                    OPEN trash$ + FileOut$(number) FOR OUTPUT AS gg
                    '''''''''''''''''''''''''''''''''''''''''''''''''''
                ELSEIF ABS(TC)=2 THEN
                    number=number+1
                    nb$="000"+TRIM$(STR$(number))
                    nb$=RIGHT$(nb$,3)
                    'output if main ID not determined
                    FileOut$(number)="!_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                    x=INSTR(yy$,".")
                    y=INSTR(yy$,SPACE$(1))
                    IF y>x AND y>x+1 AND x<6 AND x>1 THEN '| new sub ID found
                    'space after period, element after peroid, 1 to 5 elements before period
                        y1$=TRIM$(LEFT$(yy$,x-1)) '| before period
                        y2$=TRIM$(MID$(yy$,x+1,y-x))
                        ''''''''''''''''''''''''''''''
                        as1=check_as2 (y1$)
                        as2=check_as2 (y2$)
                        IF as1+as2<>-2 THEN
                            Topic$ = "ASc2 check Failed!"
                            Problems(1) = "An Error in an Index Identifier?"
                            Problems(2) = "This can cause a problems with Index ordering!"
                            problem_file(Topic$,2)
                        ELSE 'sub ID determined
                            y1$=RIGHT$(("!!!!" + y1$),4)
                            y2$=RIGHT$(("!!!!" + y2$),4)
                            y0$=y1$+"."+y2$
                            alfa_subb$=y0$
                            FileOut$(number)="!_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                        END IF
                    END IF
                    '-----------------------------------------------
                    CLOSE gg
                    OPEN trash$ + FileOut$(number) FOR OUTPUT AS gg
                    '''''''''''''''''''''''''''''''''''''''''''''''''''
                END IF
                PRINT#gg,xx$
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
            LOOP
        CLOSE gg
        '--------
    CLOSE ff
    '--------
    'sorts from (1) to number
    ARRAY SORT FileOut$(1) FOR number    '| , COLLATE sortorder, deSCEND
    '----------------------------
    OPEN File1$ FOR OUTPUT AS ff
        Image
        '-------------------
        FOR x = 1 TO number
            '-----------------------------------------
            OPEN trash$ + FileOut$(x) FOR INPUT AS gg
                Image
                '----------------
                DO UNTIL EOF(gg)
                    LINE INPUT#gg,xx$
                    '-----------------
                    PRINT#ff,xx$
                LOOP
            CLOSE gg
            '--------
        NEXT number
    CLOSE ff
    '--------
    ' _________________________________________________________
    '/ADD SPIDERS
    OPEN File2$ FOR OUTPUT AS gg
    OPEN FileOrg$ FOR OUTPUT AS hh
        OPEN File1$ FOR INPUT AS ff    '| no leading lines
            spider=0: X=0: Y=0: Z=0: Key1$="": Key2$="": Key3$=""
            'leading line removal
            Image
            DO UNTIL EOF(ff)
                LINE INPUT#ff,yy$
                '-----------------
                xx$=UCASE$(TRIM$(yy$))
                IF EOF(ff) THEN IF LEN(xx$)=0 THEN EXIT DO
                spider=spider+1: Spy=0
                IF Spider < 4 THEN
                    'does line contain one of three spider lines
                    IF X=0 THEN X=INSTR(XX$,UCASE$("spider keywords,"))
                    IF Y=0 THEN Y=INSTR(XX$,UCASE$("spider description,"))
                    IF Z=0 THEN Z=INSTR(XX$,UCASE$("spider author,"))
                    IF spider=1 AND (X=0 AND Y=0 AND Z=0) THEN
                        X=1:Y=1:Z=1
                        Spider=3
                        PRINT#hh,"spider keywords,"+keyword$
                        PRINT#hh,"spider description,"+descript$
                        PRINT#hh,"spider author,"+author$
                        Key1$= keyword$
                        Key2$= descript$
                        Key3$=Author$
                        spider=4
                    ELSEIF spider=2 AND ( (X=0 AND Y=0) OR (x=0 AND z=0) OR (z=0 AND y=0) ) THEN
                        'is second spider line found
                        IF x=0 THEN
                            PRINT#hh,"spider keywords,"+keyword$
                            key1$=keyword$
                        ELSEIF y=0 THEN
                            PRINT#hh,"spider description,"+descript$
                            Key2$=descript$
                        ELSEIF z=0 THEN
                            PRINT#hh,"spider author,"+author$
                            Key3$=author$
                        ELSE
                            prb$=STR$(spider) + " problem in spider code!"
                            ? prb$: WAITKEY$
                        END IF
                    ELSEIF spider=3 AND (x=0 OR Y=0 OR z=0)  THEN
                        'is third spider line found
                        IF x=0 THEN
                            PRINT#hh,"spider keywords;"+keyword$
                            key1$=keyword$
                        ELSEIF y=0 THEN
                            PRINT#hh,"spider description;"+descript$
                            Key2$=descript$
                        ELSEIF z=0 THEN
                            PRINT#hh,"spider author,"+author$
                            Key3$=author$
                        ELSE
                            prb$=STR$(spider) + " problem in spider code!"
                            ? prb$: WAITKEY$
                        END IF
                    ELSE      '| check line for spider words
                        y$="":x$=""
                        '-----------------------------------------
                        OPEN Trash$ + "Temp.tmp" FOR OUTPUT AS ii
                            PRINT#ii,TRIM$(xx$)
                        CLOSE ii
                        '--------
                        TRY
                            '----------------------------------------
                            OPEN Trash$ + "Temp.tmp" FOR INPUT AS ii
                                INPUT#ii,x$
                                x$=LCASE$(TRIM$(x$))
                                INPUT#ii,y$    '| helps handle comma additions
                                y$=LCASE$(y$)
                                Image
                                '----------------
                                DO UNTIL EOF(ii)
                                    INPUT#ii,z$
                                    y$ =y$ + "," + LCASE$(TRIM$(z$))
                                LOOP
                            CLOSE ii
                            '--------
                        CATCH
                            CLOSE ii
                            '--------
                        END TRY
                        IF x$="spider keywords" THEN
                            key1$=y$
                            spy=-1
                            PRINT#hh,x$+","+y$
                        ELSEIF x$="spider description" THEN
                            key2$=y$
                            spy=-1
                            PRINT#hh,x$+","+y$
                        ELSEIF x$="spider author" THEN
                            key3$=y$
                            spy=-1
                            PRINT#hh,x$+","+y$
                        END IF
                    END IF
                END IF
                IF ISFALSE spy THEN PRINT#hh,yy$
                IF SPIDER>3 THEN PRINT#gg,yy$
            LOOP UNTIL EOF(ff)
        CLOSE ff
        '--------
    CLOSE hh
    '--------
    CLOSE gg
    '--------
    ' ___________________________________
    '/output spider enabled html heading
    OPEN trash$ + "last_head.HTML" FOR OUTPUT AS ff
        'output html first line type of document statement
        x$ = WebberText$ + "DOCTYPE_DECLARE.tmp"
        '-----------------------
        OPEN x$ FOR INPUT AS gg
            Image
            '----------------
            DO UNTIL EOF(gg)
                LINE INPUT#gg,y$
                '----------------
                PRINT#ff,y$
            LOOP
        CLOSE gg
        '--------
        PRINT#ff,"  <head>"
        PRINT#ff,"      <title>" + opFile$ + "</title>"
        PRINT#ff,"      <meta charset='utf-8' />"
        PRINT#ff,"      <meta name='keywords' content='" + key1$ + "' >"
        PRINT#ff,"      <meta name='description' content='" + Key2$ +"," Key3$ + "' >"
        'PRINT#ff,"      <meta name='author' content='" + Key3$ + "' >"
    CLOSE ff
    backup_tab=File2$
    'if new file save backup 'remove blank lines and tabs to spaces 'ADD SPIDERS 'output spider enabled html heading 'Sort period indexes
EXIT FUNCTION
check_no_space_name:
    a=1
    DO
        a$="": b$="":c$="":b=0
        a = INSTR(a,xx$,"~")
        IF a = 0 THEN EXIT DO
        b = INSTR(a+1,xx$,"~")
        IF b = 0 THEN EXIT DO
        a$=LEFT$(xx$,a)
        c$=MID$(xx$,b)
        b$=MID$(xx$,a+1,b-1-(a+1)+1)
        y=0
        b$=TRIM$(b$)
        Image
        '--------------------
        FOR x = 1 TO LEN(b$)
            IF MID$(b$,x,1) = SPACE$(1) THEN
                MID$(b$,x,1) = "_"
                y=-1
            END IF
        NEXT x
        xx$=a$+b$
        a=LEN(xx$)+2
        xx$=xx$+c$
        IF ISTRUE y THEN
            Topic$ ="10 Bad File Name in: " + FileOrg$ + "!"
            Problems(1) = "10% chance spaces in name: ["+b$+"] will be a problem!"
            Problems(2) = "If Program Convert caught Name error it is corrected!"
            Problems(3) = "Space replacer '_' may cause a missing file error!"
            problem_file(Topic$,3)
        END IF
    LOOP
RETURN
2900:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION single_quote_remove ALIAS "z3000" (phrase$) AS STRING
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3000

phrase$ = TRIM$(phrase$)
    Image
    '-------------------------
    FOR x = 1 TO LEN(phrase$)
        IF MID$(phrase$,x,1) = "'" OR MID$(phrase$,x,1) = CHR$(34) THEN MID$(phrase$,x,1) = SPACE$(1)
    NEXT x
    single_quote_remove = phrase$

3000:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
SUB attempt_restore_from_delete ALIAS "z3100"
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 3100
'------------------
    f1 = FREEFILE
    Pathit(2) = HTMLfolders$+"@DELETE\"

    TRY
        '----------------------------------------------------
        OPEN Pathit(2)+"temp.tmp" FOR APPEND AS f1: CLOSE f1
        gproblem=0
    CATCH
        CLOSE f1
        '--------
        Topic$ ="11 NO RECOVERY FOLDER FOUND!"
        Problems(1) = "Attempt to Find Missing Files Failed!"
        Problems(2) = "To Clean Up Web Site:
        Problems(3) =  "RENAME [" +HoldFiles$+"] As ["+ Pathit(2) +"]"
        Problems(4) = "Keep running program till No files are recovered!"
        problem_file(Topic$,4)
    END TRY
    IF gproblem = -1 THEN
        gproblem = 0
        EXIT SUB
    END IF
    REDIM Found$(100,2): found=0:Found2=0
    f2 = FREEFILE
    TRY
        '----------------------------------------------------
        OPEN Control$+"file_not_found.txt" FOR INPUT AS f1
            Image
            '----------------
            DO UNTIL EOF(f1)
                LINE INPUT#f1,x$
                found2=found2+1
                '----------------------------------------
                OPEN Trash$ + "tmp.tmp" FOR OUTPUT AS f2
                    PRINT#f2,x$
                CLOSE f2
                '---------------------------------------
                OPEN Trash$ + "tmp.tmp" FOR INPUT AS f2
                    Image
                    '----------------
                    FOR x=0 TO 2:Found$(found2,x)="":NEXT x
                    TRY
                        FOR x=0 TO 2
                            INPUT#f2,Found$(found2,x)
                        NEXT x

                        FILECOPY (Pathit(2)+Found$(found2,2)),(HoldFiles$+Found$(found2,2))
                        Y$=HTMLfolders$+Found$(found2,0)+"\"+ Found$(found2,2)
                        IF  Found$(found2,0)<>"!HTML_FROM_TEXT" THEN
                            x=INSTR(Found$(found2,0), HTMLfolders$)
                            IF x=-1 THEN
                                Y$=Found$(found2,0)+"\"+ Found$(found2,2)
                            ELSEIF Found$(found2,0)="root" THEN
                                y$=Found$(found2,2)
                            END IF
                        END IF
                        TRY
                            FILECOPY (Pathit(2)+Found$(found2,2)),Y$
                            KILL (Pathit(2)+Found$(found2,2))
                        CATCH
                        END TRY
                        FOR x=0 TO 2:Found$(found2,x)="":NEXT x
                        found=found+1

                    CATCH
                    END TRY
                CLOSE f2
                '--------
            LOOP
        CLOSE f1
        '--------
    CATCH
        CLOSE f1,f2
    FINALLY
        IF Found2>0 THEN
            Topic$ =" IN RECOVERY FOLDER ATTEMPTED TO FIND MISSING FILES! "
            y=0
            IF Found2>Found THEN
                'y=y+1
                Problems(y)="FOUND"+STR$(Found)+" Files! Lost"+STR$(Found2-Found)+" Files!"
            END IF

            FOR x=1 TO found2
                IF LEN(TRIM$(Found$(x,2)))>0 THEN
                    y=y+1
                    Problems(y) = "FOR PAGE ["+Found$(x,1)+"] File ("+Found$(x,2)+") NOT FOUND!"
                END IF
            NEXT x
        END IF
        problem_file(Topic$,y)
        gproblem = 0
    END TRY
'-----
3100:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
SUB Sort_File ALIAS "z3200" (File$,last)
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 3200
'------------------
last = 0
'number of records unknown so ramdom Record is used to determine number
OPEN Trash$  + "Record.rd" FOR OUTPUT AS 4: CLOSE 4
OPEN Trash$  + "Record.rd" FOR RANDOM AS 4 LEN = LEN(DUMMYRecord)
    OPEN File$ FOR INPUT AS 1
        x = 0
        Image
        '---------------
        DO UNTIL EOF(1)
            INPUT#1,Record(0).Record
            IF RTRIM$(Record(0).Record) <> "" THEN
                x = x + 1
                PUT#4,x, Record(0).Record
            END IF
        LOOP
    CLOSE 1
    '-------
    Last = x
    IF Last < 2 THEN
        CLOSE 1,4
        '---------
        EXIT SUB
    END IF
    DIM alfa(Last+2) AS LOCAL STRING
    Image
    '-----------------
    FOR x = 1 TO Last
        GET#4,x,Record(0).Record
        alfa(x)=Record(0).Record
    NEXT x
CLOSE 4
'-------
ARRAY SORT alfa() FOR Last, COLLATE sortorder
'--------------------------
OPEN File$ FOR OUTPUT AS 1
    Image
    '-----------------
    FOR x = 1 TO Last
        PRINT#1,alfa(x)
    NEXT x
CLOSE 1
'-------
3200:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
' ___________________________________________
'/security Functions added to end of program
FUNCTION ENTER ALIAS "z3300" (ttt) AS STRING
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3300

    'ttt can = 0, < 0, < 0
    CONSOLE SET FOCUS
    ON ERROR RESUME NEXT
    xx$ = ""
    y = CURSORY :x = CURSORX
    tm1 = TIMER + 10
    IF ttt = 0 THEN           '| EXECUTE TIMER ONCE ENTER LINE
        Image
        '-------------------------------------
        DO                    '| control loop
            Image
            '---------------------------------
            DO                '| empty buffer
                x$ = INKEY$
            LOOP UNTIL x$ = ""
            Image
            '-----------------------------------
            DO                '| look for input
                tm2 = TIMER
                IF tm2 > tm1 THEN
                    x$ = CHR$(13)
                    EXIT DO
                END IF
                x$ = INKEY$
            LOOP UNTIL x$ <> ""
            t = ASC(x$)
            IF t = 13 THEN EXIT DO
            LOCATE y,x: PRINT SPACE$(LEN(xx$))
            IF t = 8 THEN
                IF LEN(xx$) > 0 THEN
                    xx$ = LEFT$(xx$,LEN(xx$)-1)
                END IF
            ELSE
                IF (t > 31 AND t < 127) THEN
                    xx$ = xx$ + x$
                END IF
            END IF
            LOCATE y,x: PRINT xx$
            tm1 = TIMER + 10
        LOOP
    ELSEIF ttt < 0 THEN       '| PAUSE EXECUTION OF PROGRAM
        tm1 = TIMER + 20
        DO
            x$ = INKEY$
        LOOP UNTIL x$ = ""
        DO
            tm2 = TIMER
            IF tm2 > tm1 THEN
                x$ = CHR$(13)
                EXIT DO
            END IF
            x$ = INKEY$
        LOOP UNTIL x$ = CHR$(13)
    ELSE
        Image
        '----------------------------------------------------------
        FOR z = 1 TO ttt      '| EXECUTE ENTRY SET NUMBER OF TIMES
            DO
                x$ = INKEY$
            LOOP UNTIL x$ = ""
            DO
                tm2 = TIMER
                IF tm2 > tm1 THEN
                    x$ = CHR$(13)
                    EXIT DO
                END IF
                x$ = INKEY$
            LOOP UNTIL x$ <> ""
            t = ASC(x$)
            IF t = 13 THEN EXIT FOR
            LOCATE y,x: PRINT SPACE$(LEN(xx$))
            IF t = 8 THEN
                IF LEN(xx$) > 0 THEN
                    xx$ = LEFT$(xx$,LEN(xx$)-1)
                END IF
            ELSE
                IF (t > 31 AND t < 127) THEN
                    xx$ = xx$ + x$
                END IF
            END IF
            LOCATE y,x: PRINT xx$
            tm1 = TIMER + 10
        NEXT z
    END IF
    enter = xx$

3300:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION Create_Setup_File ALIAS "z3400" (file$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3400

    TRY
        '-------------------------
        OPEN File$ FOR INPUT AS 1
            LINE INPUT#1,x$
            '---------------
        CLOSE 1
        '-------
        EXIT FUNCTION
    CATCH
        Create_Setup_File = -1
    END TRY
    '--------------------------
    OPEN file$ FOR OUTPUT AS 1
        PRINT#1," ____________________________________________________________________________"
        PRINT#1,"|TO EFFECT CHANGES IN SETUP; DELETE program generated TXT AND CSS FILES FROM |
        PRINT#1,"|         Folders: @Control, @WEBBER_TEXT, !HTML_FOLDERS                     |"
        PRINT#1,"|_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=|"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/TEXT & Frame SIZE CONTROLS; Length of text line; EM= number of pixels     \"
        PRINT#1,"|''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"width PX,50,16,                                                           /"
        PRINT#1,"|------------------------------------------------------------------------/"
        PRINT#1,"frameset cols,25,75,                                                    /"
        PRINT#1,"frameset rows,15,85,                                                   /"
        PRINT#1,"\_____________________________________________________________________/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1," /WEB TITLE AND SEARCH DESCRIPTIONS: FOR FRAME,INDEX AND CREATED HTML      \"
        PRINT#1,"|''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"Author,Your Name in Setup.txt,                                            /"
        PRINT#1,"site title,Text Webber by Hugh Polley,                                   /"
        PRINT#1,"spider keywords,WEB SITE CREATOR,HTML INDEX CREATOR,                    /"
        PRINT#1,"spider description,CREATES HTML PAGES FROM TEXT FILES,                 /"
        PRINT#1,"\_____________________________________________________________________/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/WEB SITE LINK LABEL AND BACKGROUND IMAGE FOR COVER PAGE                   \"
        PRINT#1,"\''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"index html,ENTER WEB SITE HERE,firstimage.jpeg,                           /"
        PRINT#1,"\________________________________________________________________________/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/LEFT FRAME HEADINGS; COMA WITH BLANK SPACE FOR NO HEADING                 \"
        PRINT#1,"\''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"Describe,Left Frame Links,1..Off Site,2..Webber Files,3..Webber Folders,  /"
        PRINT#1,"\________________________________________________________________________/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/LEFT FRAME CONTACT LINKS; REMOVE WHOLE LINE TO ELIMINATE FROM PAGE        \"
        PRINT#1,"\''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"EMail,E_Mail Web Master,hap2u@hotmail.com,                                /"
        PRINT#1,"Form,Free Web Site Hosting,http://www.webs.com,                          /"
        PRINT#1,"\_______________________________________________________________________/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/HEAD FRAME CSS DEFAULT CONTROLS                                           \"
        PRINT#1,"\''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"| Item           Color               image           attributes            |"
        PRINT#1,"|'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"hBODY,         Lime          ,headground.jpg  ,repeat,fixed,left top,    /"
        PRINT#1,"hForeground,   black         ,secondimage.jpg,                            /"
        PRINT#1,"\______________________________________________________________________/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/MAIN FRAME CSS DEFAULT CONTROLS                                           \"
        PRINT#1,"\''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"| Item          Color                image          attributes             |"
        PRINT#1,"|'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"mBODY,      Lime            , mainground.jpg  ,no-repeat,fixed,left top, /"
        PRINT#1,"mForeground,black           , mainimage.jpg,                            /"
        PRINT#1,"\______________________________________________________________________/"
        PRINT#1,SPACE$(4)
        PRINT#1," ___________________________________________________________________________"
        PRINT#1,"/TAIL FRAME CSS DEFAULT CONTROLS                                            \"
        PRINT#1,"\'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"| Item          Color                image          attributes              |"
        PRINT#1,"|''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"tBODY,      Lime            , tailground.jpg   ,no-repeat,fixed,left top, /"
        PRINT#1,"tForeground,black           , tailimage.jpg,                             /"
        PRINT#1,"\_______________________________________________________________________/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/COVER PAGE CSS DEFAULT CONTROLS                                            \"
        PRINT#1,"\'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"| Item          Color                image          attributes              |"
        PRINT#1,"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"iBODY,      Lime            , indexground.jpeg  ,no-repeat,fixed,left top,/"
        PRINT#1,"iForeground,black           , firstimage.jpg,                            /"
        PRINT#1,"\_______________________________________________________________________/"
        PRINT#1,"_______________________________________________________________________"
        PRINT#1,"LINKS      Forefround       Background         active                  \"
        PRINT#1,"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''/"
        PRINT#1,"Link,   lime            , green           ,white,                     /"
        PRINT#1,"Hover,  black           , white           ,                          /"
        PRINT#1,"Active, white           , black           ,                         /"
        PRINT#1,"Offsite,black           , blue            ,                        /"
        PRINT#1,"\_________________________________________________________________/"
        PRINT#1,SPACE$(4)
        PRINT#1,"====================================================================="
        PRINT#1,"Color name  Hex        rgb Decimal"
        PRINT#1,"black      #000000     0,0,0      "
        PRINT#1,"silver     #C0C0C0     192,192,192"
        PRINT#1,"gray       #808080     128,128,128"
        PRINT#1,"white      #FFFFFF     255,255,255"
        PRINT#1,"maroon     #800000     128,0,0    "
        PRINT#1,"red        #FF0000     255,0,0    "
        PRINT#1,"purple     #800080     128,0,128  "
        PRINT#1,"fuchsia    #FF00FF     255,0,255  "
        PRINT#1,"green      #008000     0,128,0    "
        PRINT#1,"lime       #00FF00     0,255,0    "
        PRINT#1,"olive      #808000     128,128,0  "
        PRINT#1,"yellow     #FFFF00     255,255,0  "
        PRINT#1,"navy       #000080     0,0,128    "
        PRINT#1,"blue       #0000FF     0,0,255    "
        PRINT#1,"teal       #008080     0,128,128  "
        PRINT#1,"aqua       #00FFFF     0,255,255  "
        PRINT#1,"====================================================================="
    CLOSE 1
    '-------
3400:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION Engineer_output ALIAS "z3500"
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3500

    IF yes < 1 THEN
                            IF INSTR(UCASE$(ok$),">MOVEMENT RESULTS") > 0 THEN
                                yes = -1
                            ELSE
                                IF INSTR(UCASE$(ok$),">POSITIONS") > 0 THEN
                                    yes = 1
                                END IF
                                IF ISTRUE yes THEN
                                    ' _______________________________________
                                    '/remove brackets underline failed orders
                                    x = 1
                                    Image
                                    '--------------
                                    DO UNTIL x = 0
                                        x = INSTR(x,ok$,"[")
                                        IF x > 0 THEN
                                            a$ = LEFT$(ok$,x-1)
                                            y = INSTR(x,ok$,"]")
                                            IF y > 0 THEN
                                                b$ = MID$(ok$,x+1,y-x-1)
                                                b$ = "<em>" + b$ + "</em>"
                                                c$ = MID$(ok$,y+1)
                                                x$ = TRIM$(a$)
                                                z = ISTRUE (LEN(x$) = 0 OR MID$(x$,LEN(x$),1) = "," OR MID$(x$,LEN(x$),1) = ":")
                                                IF ISFALSE z THEN b$ = ", " + b$
                                                ok$ = a$ + b$ + c$
                                            END IF
                                        END IF
                                    LOOP
                                    ' __________________
                                    '/Fix round brackets
                                    y = 1
                                    Image
                                    '--------------
                                    DO UNTIL y = 0
                                        y = INSTR(ok$,")")
                                        DO
                                            x = INSTR(ok$,"(")
                                            t = INSTR(x + 1,ok$,"(")
                                            IF t > 0 AND t > x AND t < y THEN
                                                MID$(ok$,t,1) = "{"
                                                MID$(ok$,y,1) = "}"
                                                EXIT DO
                                            END IF
                                            IF x > 0 THEN
                                                'coast found
                                                IF y - x < 6 THEN
                                                    MID$(ok$,x,1) = "{"
                                                    MID$(ok$,y,1) = "}"
                                                    EXIT DO
                                                END IF
                                                a$ = LEFT$(ok$,x-1)
                                                y = INSTR(x,ok$,")")
                                                IF y > 0 THEN
                                                    b$ = MID$(ok$,x+1,y-x-1)
                                                    c$ = MID$(ok$,y+1)
                                                    x$ = TRIM$(a$)
                                                    z = ISTRUE (LEN(x$) = 0 OR MID$(x$,LEN(x$),1) = "," OR MID$(x$,LEN(x$),1) = ":")
                                                    IF ISFALSE z THEN b$ = ", " + b$
                                                    ok$ = a$ + b$ + c$
                                                    EXIT DO
                                                END IF
                                            END IF
                                        LOOP UNTIL x = 0
                                    LOOP
                                END IF
                            END IF
                        END IF

3500:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION tcm_first_set$ ALIAS "z3600" (hPar$,TCM)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3600

'Opening tags collected
'first check :+6:+7:+8:-4:+4:+3:-3:-9
'hPar$ is new line

    ' _________________________________________
    '/Capture entering Line or Input New Line  \
    IF LEN(hPar$) > 0 THEN
        nPar$ = hPar$
        hPar$ = ""

    ELSE
        LINE INPUT#3,nPar$
        '------------------
        lnumber=lnumber+1
    END IF
    '\_________________________________________/
    ' __________________________________________
    '/New information Line Indent Value         \
    i1 = LEN(nPar$): i2 = LEN(LTRIM$(nPar$))
    i3 = i1-i2: IF i3<0 THEN i3=0
    i1 = ((i1 - i2) > 1)*-(i1-i2)
    IF i1 > 2 THEN
        i1 = INT((i1 + 1)/4)*4
        IF i1 > 24 THEN i1 = 24
    ELSE
        i1=0
    END IF
    nPar$=TRIM$(nPar$)

    ' __________________________________________
    '/new information line; process for TMC
    hPar$ = Par$
    Par$ = nPar$
    TCM = dProcess(Par$)
    nPar$ = Par$
    Par$ = hPar$
    hPar$ = ""
    'Look after indent '''''''''''''''''''''''''
    '\__________________________________________/
    ' ______________________________________________________________________________________________
    '/Process Old information Line for print out elimination and line Return                        \
    par$ = RTRIM$(par$)
    good$ = RTRIM$(good$)
    ' ______________________________________________________________________________________________
    '/Determine LINE RETURN status after old line                                                   \
    IF TCM = -1 OR ABS(TCM) = 2 OR TCM=ABS(9) OR TCM = 6 OR TCM=7 THEN
        'new line requires line return on old Par$ line
        IF Format=0 THEN FixLength$ ELSE REPLACE SPACE$(1) WITH Spacing$ IN Par$
        'print out whats left
        IF i1>0 THEN Par$=RTRIM$(Par$)+"<br>"
        IF LEN(TRIM$(good$))>0 THEN PRINT#8,good$ ELSE PRINT#8,""
        IF LEN(TRIM$(Par$))>0 THEN PRINT#2,Par$ ELSE PRINT#2,""
    ' _____________________________________________________________________________________________
    '/Print out old line with space and no line return
    ELSE

        IF LEN(TRIM$(par$))>0 THEN
            IF i1>0 THEN par$=RTRIM$(par$)+"<br>"
            PRINT#2,RTRIM$(Par$)+SPACE$(1);
        ELSE
            PRINT#2,""
        END IF
        IF LEN(TRIM$(Good$))>0 THEN
            PRINT#8,RTRIM$(good$)+SPACE$(1);
            IF i1>0 THEN PRINT#8,""
        ELSE
            IF i1>0 THEN PRINT#8,""
        END IF

    END IF
    '\_____________________________________________________________________________________________/
    ' ______________________________________________
    '/Create new operational Lines Par$ and Good$   \
    gPar$ = check_enity_marks(nPar$,TCM)
    IF TMC=-1 THEN
        nPar$=TRIM$(nPar$)
        gPar$=TRIM$(gPar$)
    ELSE
        IF ABS(TCM)=2 THEN  nPar$=SPACE$(i1)+TRIM$(nPar$) ELSE nPar$=REPEAT$(i1,Spacing$)+TRIM$(nPar$)
        gPar$=SPACE$(i1)+TRIM$(gPar$)
    END IF
    Par$=nPar$
    Good$=gPar$
    '\______________________________________________/

    '_________________________________________________________________________________
    DO'/Process Start nest code or line HTML code for TCM                             \
        SELECT CASE TCM
        '---------------
        CASE -1
            Par$=TRIM$(par$)
            Good$=TRIM$(Good$)
            EXIT DO

        CASE 2,-2
            EXIT DO

        CASE -9 'move to new folder
            New_Dir$ = folder_process$(Par$)
            Par$ = ""
            Good$ = ""
            'EXIT DO
            ''''''''''''''''''''''''''''''''''''''
        CASE +9
            head_int=head_int-1
            '''''''''''''''''''''''''''''''''''''''
        CASE +1 'underline inside square brackets
            Topic$ = "12 IN -> " + opFile$ + "->"
            Problems(1) = "Text Code Mark +1 underline"
            Problems(2) = "Error occurred at Line Number" + STR$(Lnumber)
            Problems(3) = "Line=("+Par$+")"
            Problems(4) = "unexpected error!"
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            t=-1:a=1:b=1
            DO 'search for brackets add emphases
                a = INSTR(a,par$,"[")
                b = INSTR(b,par$,"]")
                IF a<1 AND b<1 THEN EXIT DO '| no brackets left
                t=0    '| error found setting
                IF a>0 AND b<a THEN
                    t=0: Problems(4) = "Second Bracket ']' position error?"
                    EXIT DO
                END IF
                IF b>0 AND (a>b OR a=0) THEN
                    t=0: Problems(4) = "First Bracket '[' position error?"
                    EXIT DO
                END IF
                IF a>0 AND b>a THEN
                    t = -1    '| no error found
                    a$ = LEFT$(par$,a-1)
                    b$ = MID$(par$,b+1)
                    c$ = MID$(par$,a+1,b-a-1)
                    c$=SPACE$(1)+"<em>"+TRIM$(c$)+"</em>"+SPACE$(1)
                    par$=a$+c$+b$
                END IF
            LOOP
            IF t = 0 THEN problem_file(Topic$,4)
            ''''''''''''''''''''''''''''''''''''''''''

        CASE -3
            create_Image$(support$)
            FileMoveSupport(Support$)
            ''''''''''''''''''''''''''''''''''''''''

        CASE +3
            Topic$ = "13 IN -> " + opFile$ + "->"
            Problems(1) = "Text Code Mark +3 move File"
            Problems(2) = "Error occurred at Line Number" + STR$(Lnumber)
            Problems(3) = "Line=[" + Par$ + "]"
            IF ISFALSE INSTR(par$,"~") THEN
                Problems(4) = "File Marks (~) not found!"
                problem_file(Topic$,4)
                EXIT DO
            END IF
            t = 0: a=1
            Image
            '--------------
            DO UNTIL a < 1
                a=INSTR(par$,"~")
                IF a = 0 THEN EXIT DO
                '''''''''''''''''''''
                a$ = LEFT$(par$,a-1)
                b$ = MID$(par$,a+1)
                b = INSTR(b$,"~")
                'process good copy
                a1 = INSTR(good$,"~")
                a1$ = LEFT$(good$,a1-1)
                b1$ = MID$(good$,a1+1)
                b1 = INSTR(b1$,"~")
                '''''''''''''''''''
                IF b = 0 THEN
                    Problems(4) = "Second File Mark (~) not found!"
                    problem_file(Topic$,4)
                    EXIT DO
                    '''''''''''
                ELSE
                    c$ = MID$(b$,b+1)
                    b$ = LEFT$(b$,b-1)
                    support$ = b$
                    IF LEN(TRIM$(a$)) > 0 OR LEN(TRIM$(c$)) > 0 THEN
                        par$ = a$ + c$
                    ELSE
                        par$ = ""
                    END IF
                    FileMoveSupport(Support$)

                    '''''''''''''''''''
                END IF
                ''''''''''''''''''

            LOOP

            '''''''''''''''''

        CASE +4
            create_link$ (Support$,TCM)
            FileMoveSupport(Support$)
            '''''''''''''''''''''''''''''''''

        CASE -4
            create_link$ (Support$,TCM)
            FileMoveSupport(Support$)
            '''''''''''''''''''''''''''''''''
        CASE 6
             nest_subtract(TCM)
             PRINT#2,"<" + TG(TCM)
             good$ = RTRIM$(good$)
             Format = -1


        CASE 7
             nest_subtract(TCM)
             Par$ = "<" + TG(TCM) + Par$
             good$="("+TRIM$(Good$)
             IF Format=-1 THEN
                Topic$ = "Possible 7TCM Nest problem: IN -> " + opFile$ + "->"
                Problems(1) = "Problems with other nested marks possible!"
                Problems(2) = "when nested with 8,6 results can be strange!
                Problems(3) = "Line=[" + Par$ + "] line: "+STR$(Lnumber)
                problem_file(Topic$,3)
             END IF

        CASE 8
             nest_subtract(TCM)
             Par$ = "<" + TG(TCM) + Par$
             Good$=SPACE$(i1)+"{"+TRIM$(Good$)
        CASE ELSE

             EXIT DO 'no first TCM left
        END SELECT
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        TCM = dProcess(Par$)

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    LOOP UNTIL TCM = 0
    '\_________________________________________________________________________________/

3600:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION FixLength$ ALIAS "z3700"
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3700

    IF ISTRUE Format THEN 'Format as written
        REPLACE SPACE$(1) WITH Spacing$ IN Par$
        PRINT#2,Par$
        PRINT#8,good$+" (4) "
        Par$ = "": good$=""
    END IF
    IF LEN(TRIM$(Par$))=0 THEN
        Par$=""
        EXIT FUNCTION
    END IF
    ' ___________________________________________________
    '/process par$
    DO
        ' ____________________________________________
        '/if second index line reduce line length by 4
        TMPlength = 0
        Image
        '---------------------------------------------------------------
        FOR x = 1 TO nNest: IF nest(x) = -2 THEN TMPlength = -4: NEXT x
        par$ = TRIM$(par$)
        ' __________________________________________
        '/add on to TMPlength any tag syntax
        'search for opening and closing HTML tags
        oTag$="<":cTag$=">"
        Spot = skip_tags (Par$,oTag$,cTag$,TMPlength)
        ' _______________________________________________
        '/max length of line after tags and indent added
        TMPlength = PRElength + TMPlength: IF TMPlength>200 THEN TMPlength=200
        IF LEN(par$) =< TMPlength THEN EXIT DO
        ' ______________________________________________
        '/Par$ is too Large find cut off point for line
        xx1 = 0: x = 1
        DO
            x1 = INSTR(x,par$,SPACE$(1))
            x2 = INSTR(x,par$,"&ensp;")
            IF x2>0 AND x2<x1 THEN x1=x2
            x2 = tags (par$,x)
            x = (x1>0 AND x2>0 AND x1<x2)*-x1 OR (x2>0 AND x1>0 AND x2<x1)*-x2 OR (x1 = 0 AND x2>0)*-x2 OR (x2=0 AND x1>0)*-x1
            IF x = 0 THEN EXIT DO
            IF x < TMPlength THEN xx1 = x
            x = x + 1
        LOOP UNTIL x > TMPlength
        'is cut off point found
        IF xx1 > 0 THEN
            ptPar$ = TRIM$(LEFT$(par$,xx1-1))
            par$ = TRIM$(MID$(par$,xx1))
            PRINT#2,ptPar$
        ELSE
            Topic$ = "15 IN -> " + opFile$ + "->"
            Problems(1) = "Long No GAP LINE!"
            Problems(2) ="[" + par$ + "]"
            Problems(3) ="May cause display problems!"
            problem_file(Topic$,3)
            PRINT#2,Par$
            PRINT#8,Good$+" (5) "
            Par$=""
            Good$=""
            EXIT DO
        END IF
    LOOP
3700:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION skip_tags ALIAS "z3800" (sPar$,oTag$,cTag$,TMPlength) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3800
'Spacing$="&ensp;"
    'changes TMPlength and returns last closing bracket
    spot = 1
    DO
        firstspot=0
        x = INSTR(spot,sPar$,oTag$)
        IF x> 0 THEN
            firstspot = x
            spot = x + 1
            y = INSTR(spot,sPar$,cTag$)
            IF y>0 THEN
                spot = y+1
                TMPlength = TMPlength + (spot-firstspot)
            END IF
        END IF
    LOOP UNTIL x = 0
    place=1
    DO
        x = INSTR(place,sPar$,"&")
        IF x>0 THEN
            x=x+1
            place = x
            y=INSTR(place,sPar$,";")
            IF y>0 THEN
                place=y+1
                y=y-1
                a$ = MID$(sPar$,x,y-x+1)
                Image
                '------------------
                FOR t=1 TO LEN(a$)
                    t1=ASC(MID$(a$,t,1))
                    t2=(t1>64 AND t1<91) OR (t1>96 AND t1<123)
                    IF t2<>-1 THEN EXIT FOR
                NEXT t
                IF ISTRUE t2 THEN TMPlength = TMPlength + LEN(a$)+2
            END IF
        END IF
    LOOP UNTIL x = 0
    IF Spot > 1 THEN FUNCTION = Spot
3800:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION const_index_linkage ALIAS "z3900" (Par$,TCM)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 3900

'!IMPORTANT! ALL LINKS HAVE FOLDER WERE FRAME IS AS THEIR ROOT FOLDER
    ' _____________________________________
    '/Individual Index Line is broken into\
    'its two possible sections
    'given a Name and link href target
    '\-------------------------------------/
    ' _________________________________________
    '/z CONTROLS LOWER LINK CONSTRUCTION
    z = (TCM = -1 OR TCM = -2 OR TCM=2) * TCM
    IF TCM = 2 THEN z = 2
    IF z = 0 THEN EXIT FUNCTION
    'PREP VARIABLES
    STATIC href$
    IF LEN(href$) = 0 THEN href$= "#top"
    ' _____________________________
    '/Create Highlighted part of link
    lx = INSTR(par$,":")
    IF lx > 0 THEN
        a$ = MID$(par$,1,lx-1)    '| lx-1
        b$ = RTRIM$(MID$(par$,lx+1))
        par$ = a$ + " " + b$
        lx = LEN(a$)
    END IF
    ' _________________________________
    '/CREATE UNHIGHLIGHTED PART OF LINK
    rear$ = ""
    IF lx = 0 THEN
        lx = LEN(par$)
    ELSE
        rear$ = TRIM$(MID$(par$,lx + 1))    '| information following Index Link
    END IF
    ' ___________________________________________
    '/ASSIGN DISPLAYED PART OF LINK - BLINK$
    'if short link constructed lx will be less than len(Par)
    bLink$ = MID$(par$,1,lx)    '| Displayed link Text
    ' ___________________________________
    '/CREATE UPPER AND LOWER INDEX NAMES
    INCR link                 '| INCREASE BY ONE LINK NUMBER
    aLink$ = "AAAAA" + TRIM$(STR$(link))    '| name of LOWER LINK
    aLink$ = RIGHT$(aLink$,4)
    cLink$ = "BBBBB" + TRIM$(STR$(link))    '| name of UPPER LINK OR destination of link
    cLink$ = RIGHT$(cLink$,4)
    ' ____________________
    '/Index fILE EXISTS
    TRY
        'YES EXISTS
        OPEN mIndex$ FOR INPUT AS 10
            INPUT#10,x$
        CLOSE 10
        '--------
    CATCH
        CLOSE 10
        '--------
        'DOES NOT EXIST
        iLink$ = "[SINGLE PAGE]"' (" + UCASE$(opfile$) + ")"
        IF LCASE$(opFile$) = "license" THEN
            xx$ = "tail.html"
        ELSE
            xx$ = opFile$ + ".html"
        END IF
        IF head_int = -2 THEN
            ilk$="<dl id='index' class='Frame_inset' >"    '| #index
        ELSE
            ilk$="<dl class='index_line'>"
        END IF
        ilk$ = ilk$ + "<dt><a id='top' href='" + xx$ + "' target='_blank'>" + iLink$ + "</a></dt>"
        '----------------------------
        OPEN mIndex$ FOR OUTPUT AS 9
            PRINT#9, ilk$
        CLOSE 9
        '-------
        href$ = "top"
    FINALLY
            CLOSE 9
            '-------
            CLOSE 10
            '--------
    END TRY
    ' ____________________________________
    '/CREATE HTML FOR LINKS
    lpar$ = "<a id='" + aLink$ + "' href='#" + href$ + "'>"
    IF LEN(TRIM$(bLink$))=0 THEN
        aLink$="top"
        bLink$="TO TOP OF INDEX"
    END IF
    aLink$ = "href='#" + alink$
    lk$=""
    IF TCM = -1 THEN
        lk$ = "<dt><a id='" + cLink$ + "' " + aLink$ + "'>" + bLink$ + "</a></dt>"
        href$ = cLink$
    ELSEIF TCM = -2 THEN      '| IF TCM=+2 THEN UPPER LINK WILL NOT BE CREATED
        lk$ = "<dd><a id='" + cLink$ + "' " + aLink$ + "'>" + bLink$ + "</a></dd>"
    END IF
    par$ = lpar$ + bLink$ + "</a> " + rear$
    IF z=1 THEN               '| main index found record in file
        GOSUB index_from_subindex
        '---------------------------------------
        OPEN sIndex$ FOR OUTPUT AS 10: CLOSE 10
        IF LEN(Par$) > 0 THEN
            '----------------------------
            OPEN mIndex$ FOR APPEND AS 9
                PRINT#9,lk$;
            CLOSE 9
            '-------
        END IF
    ELSEIF z=2 THEN
        '---------------------------------------------------------------
        OPEN sIndex$ FOR APPEND AS 10    '| sub index found record file
            PRINT#10,lk$
        CLOSE 10
        '--------
    END IF
    EXIT FUNCTION
index_from_subindex:
    '---------------------------------------
    OPEN sIndex$ FOR APPEND AS 10: CLOSE 10
    yes = -1                  '| no sub index found indicator
    '----------------------------
    OPEN sIndex$ FOR INPUT AS 10
        Image
        '----------------
        DO UNTIL EOF(10)
            '----------------------------
            OPEN mIndex$ FOR APPEND AS 9
                Image
                '----------------
                DO UNTIL EOF(10)
                    LINE INPUT#10,k$
                    '----------------
                    k$ = TRIM$(k$)
                    IF LEN(k$) > 0 THEN
                        PRINT#9,""
                        PRINT#9,k$;
                    END IF
                LOOP
                yes = 0
                PRINT#9,""
            CLOSE 9
            '-------
        LOOP
    CLOSE 10
    '--------
    IF ISTRUE yes THEN
        '----------------------------
        OPEN mIndex$ FOR APPEND AS 9
            PRINT#9,""
        CLOSE 9
        '-------
    END IF
RETURN

3900:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION tcm_third_set$ ALIAS "z4000" (TCM)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4000
   'clean up open index tags
    'third check :-1:-2

    SELECT CASE TCM
        CASE -1
            PRINT#2,"": PRINT#8,""
            y=nest_subtract(-2)
            IF y=2 THEN PRINT#2,"</div><br>"
            x=nest_subtract(ABS(TCM))
            IF x=ABS(TCM) THEN PRINT#2,"</div>"
            PRINT#2,"<pre></pre><div>";
            PRINT#8,STRING$(LEN(Good$),"=")
            good$ = TRIM$(good$)+SPACE$(1)
            par$ = TRIM$(par$)
            const_index_linkage (Par$,TCM)
            par$ = TRIM$(par$)
            x=INSTR(par$,"</a>")
             ' _____________________________________________
            IF x+3<LEN(par$) THEN 'short link inside line  \
                y=INSTR(Good$,":")
                IF Y>0 THEN
                    MID$(Good$,y,1)=";"
                END IF
                a$=LEFT$(par$,x+3)
                b$=MID$(par$,x+4)
                sPar$=Par$
                TMPlength=100
                oTag$="<":cTag$=">"
                y=skip_tags (sPar$,oTag$,cTag$,TMPlength)
                '_______________________________________
                IF y+4>TMPlength THEN' link is to long!\
                    par$=b$
                    PRINT#2,a$+"<br>"
                '_________________________________________________
                ELSEIF LEN(Par$)> TMPlength THEN' divide long line\
                    Image
                    '------------------------------
                    FOR x=TMPlength-7 TO LEN(par$)
                        IF MID$(par$,x,1)=":" OR MID$(par$,x,1)=";" OR MID$(par$,x,1)="," OR MID$(par$,x,1)=SPACE$(1) THEN EXIT FOR
                    NEXT x
                    IF x>TMPlength THEN
                        par$=b$
                        PRINT#2,a$
                    ELSE
                        a$=LEFT$(par$,x)
                        par$=MID$(par$,x+1)
                        PRINT#2,a$
                    END IF
                '__________________________
                ELSE' Line is right length \
                    PRINT#2,par$+"<br>"
                    PRINT#8,good$+SPACE$(1);
                    good$ = ""
                    par$ = ""
                END IF
            '________________________
            ELSE 'whole line is link \
                PRINT#2,par$+"<br>"
                PRINT#8,good$;
                par$ = "":good$=""
            END IF
            'existindex = -1
            action$ = ""
            TCM = 0
        CASE -2, 2
            PRINT#2,"": PRINT#8,"" 'line break before Index
            ' ______________________
            '/New Line Indent Value \
            i1 = LEN(Par$): i2 = LEN(LTRIM$(Par$))
            i1 = ((i1 - i2) > 1)*-(i1-i2)
            IF i1 > 2 THEN
                i1 = INT((i1 + 1)/4)*4
                IF i1 > 24 THEN i1 = 24
            ELSE
                i1=4
            END IF
            '''''''''''''''''''''''''''''''''''''''
            Par$=TRIM$(par$)
            x=nest_subtract(ABS(TCM))
            IF x=ABS(TCM) THEN PRINT#2,"</div>"
            PRINT#2,"<div>" + REPEAT$(i1,Spacing$);
            PRINT#8,SPACE$(i1)+STRING$(LEN(TRIM$(Good$)),"-")
            good$ = SPACE$(i1) + TRIM$(good$)
            const_index_linkage (Par$,TCM)
            par$ = TRIM$(par$)
            x=INSTR(par$,"</a>")
            '___________________________________________
            IF x+3<LEN(par$) THEN'shorten link found    \
                y=INSTR(Good$,":")
                IF Y>0 THEN
                    MID$(Good$,y,1)=";"
                END IF
                ' _____________________________
                '/short link inside line
                TMPlength=100-4
                sPar$=Par$
                oTag$ = "<a"
                cTag$ = "a>"
                y=skip_tags (sPar$,oTag$,cTag$,TMPlength)
                IF NOT(TMPlength - y - LEN(MID$(par$,x+3+1))-10 > 0) THEN
                    PRINT#2,par$
                    PRINT#8,good$
                    par$ = "":good$=""
                END IF
                good$= RTRIM$(good$)
                FixLength$
            ELSE
                ' _____________________________
                '/whole line is link
                PRINT#2,par$+"<br>"
                PRINT#8,good$+SPACE$(1);
                par$ = "":good$=""
            END IF
            action$ = ""
            TCM = 0

        CASE 0 'no tag

        CASE ELSE 'more tags on line
            'reset line to hPar if Par$>0

            IF LEN(TRIM$(Par$))>0 THEN
               hPar$=Par$: Par$=""
            END IF


        END SELECT
4000:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
'opFile$
FUNCTION tcm_second_set$ ALIAS "z4100" (TCM)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4100
    'second check :+5:-5:-6:-7:-8

    DO
        SELECT CASE TCM
        '---------------
            CASE -8
                nest_subtract(TCM)
                Par$ = Par$ + "</" + TG(ABS(TCM))
                Good$=Good$+"}"
            CASE -7
                nest_subtract(TCM)
                Par$ = RTRIM$(Par$) + "</" + TG(ABS(TCM))
                PRINT#2,Par$
                Par$ = ""
                good$=RTRIM$(good$)
                PRINT#8,good$+")"
                good$ = ""
            CASE -6
                nest_subtract(TCM)
                Par$ = Par$ + "</" + TG(ABS(TCM))
                REPLACE SPACE$(1) WITH Spacing$ IN Par$
                PRINT#2,Par$
                Par$ = ""
                IF LEN(TRIM$(Good$))>0 THEN PRINT#8,Good$ ELSE PRINT#8,""
                good$ = ""
                format = 0
            CASE -5
                par$ = RTRIM$(par$) + "<br>"
                good$ = RTRIM$(good$)
                IF Format=0 THEN FixLength$ ELSE REPLACE SPACE$(1) WITH Spacing$ IN Par$
                PRINT#2,RTRIM$(par$)
                PRINT#8,RTRIM$(good$)
                par$ = "":good$=""
            CASE 5
                par$ = RTRIM$(par$) + "<br>"
                good$ = RTRIM$(good$)
                IF Format=0 THEN FixLength$ ELSE REPLACE SPACE$(1) WITH Spacing$ IN Par$
                PRINT#2,RTRIM$(par$)
                PRINT#8,RTRIM$(good$)
                PRINT#2,REPEAT$(4,Spacing$) + "<br>"
                PRINT#8,SPACE$(4)
                par$ ="":good$=""
            CASE ELSE 'goto third as not nested or TCM 5
                EXIT DO
        END SELECT
        TCM = dProcess (Par$)
    LOOP UNTIL TCM = 0
    IF Format = -1 THEN
        REPLACE SPACE$(1) WITH Spacing$ IN Par$
        PRINT#2,Par$
        PRINT#8,Good$
        Par$ = "": Good$ = ""
    END IF
4100:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
'opFile$
FUNCTION folder_process$ ALIAS "z4200" (Pr$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4200
'------------------
    pr$ = TRIM$(pr$)

    IF LEN(pr$) = 0 THEN EXIT FUNCTION
    'clange internet address to file address
    DO
        x = INSTR(pr$,"/")
        IF x>0 THEN MID$(pr$,x,1)="\"
    LOOP UNTIL x=0

    'remove at end of input root forward slash
    DO
        x=RIGHT$(pr$,1)="\"
        IF ISTRUE x THEN MID$(pr$,LEN(pr$),1)=SPACE$(1)
    LOOP UNTIL x=0

    'remove root address from name
    x=INSTR(pr$,HTMLfolders$)
    IF x>0 THEN
        pr$= MID$(pr$,x+LEN(HTMLfolders$))
    END IF

    pr$=TRIM$(pr$)
    'remove any listed root directory
    x=INSTR(-1,pr$,"\")
    pr$ = MID$(pr$,x+1)

    'fix up syntex
    pr$ = Folder_Fix(pr$)

    New_Dir$ = TRIM$(Pr$)
    IF LEN(New_Dir$)=0 THEN
        New_Dir$ = "\"
        Topic$ = "16 IN -> " + opFile$ + "->"
        Problems(1) = "Only slash marks found!"
        Problems(2) = "File created in Root Directory!"
        problem_file(Topic$,2)
    ELSEIF New_Dir$ + "\" = HTMLfolders$ THEN
        New_Dir$ = HTMLfolders$
    ELSE
        New_Dir$ = UCASE$(HTMLfolders$ + New_Dir$)
        ON ERROR RESUME NEXT
        MKDIR New_Dir$
        ERRCLEAR
        New_Dir$ = New_Dir$ + "\"
    END IF

    FUNCTION = New_Dir$
4200:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION create_Image$ ALIAS "z4300" (support$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4300

    Topic$ = "17 IN -> " + opFile$ + "->"
    Problems(1) = "Text Code Mark -3 create Image"
    support$ = ""
    a=1:t=0
    Image
    '? ' ,1,"|"+Good$+"|"
    '------------
    DO UNTIL a<1
        a = INSTR(par$,"~")
        IF a = 0 THEN EXIT DO
        t=-1
        a$ = LEFT$(par$,a-1)
        b$ = MID$(par$,a+1)
        b = INSTR(b$,"~")
        IF b = 0 THEN
            Problems(2) = "2. line=[" + Par$ + "]"
            Problems(3) = "Second Image Mark (~) not found!"
            problem_file(Topic$,3)
            EXIT DO
        ELSE
            c$ = MID$(b$,b+1)
            b$ = LEFT$(b$,b-1)
            bb$ = UCASE$(b$)
            y = INSTR(bb$,".GIF") + INSTR(bb$,".JPG") + INSTR(bb$,".PNG") + INSTR(bb$,".BMP") + INSTR(bb$,".JPEG")
            IF y=0 THEN
                Problems(2) = "3. line=[" + Par$ + "]"
                Problems(3) = "Image=[" + b$ + "]"
                Problems(4) = "Unsupported Video or Image file"
                problem_file(Topic$,4)
                EXIT DO
            ELSE
                support$ = b$
                b$ = LCASE$(b$)
                Image
                '--------------------
                FOR x = 1 TO LEN(b$)
                    IF MID$(b$,x,1) = SPACE$(1) THEN MID$(b$,x,1) = "_"
                NEXT x
                f1$ ="": f2$=""
                t$=opFile$
                ' _______________________________
                '/Link Logo?
                x1=INSTR(Par$,"{")
                IF x1>0 THEN
                    x2=INSTR(x1+1,Par$,"}")
                    IF x2>x1 THEN
                        x3=INSTR(x2+1,Par$,"}")
                        IF x3>x2 THEN
                            t$="head"
                        END IF
                    END IF
                END IF
                IF t$="head" THEN    '| (LEFT$(TRIM$(a$),1)+RIGHT$(TRIM$(a$),1))="{}" THEN
                    i$ = "<img class='buttonscaled'"
                    i2$ = ""
                ELSE
                    i$ = "<div class='Frame_inset'><img class='Frame_scaled' "
                    i2$ = "</div>"
                END IF
                par$ = a$ + i$ + " src='" + b$ + "' alt='" + b$ + "_not found'>" + i2$ + c$
            END IF
        END IF
                    '? ' ,"    ==================="
                    '? ' , "Parts:",a$,i$,b$,i2$,c$
                    '? ' ,"         -----------"
                    '? ' ,2,"|"+Good$+"|"
                    '? ' ,"         -----------"

    LOOP
    IF t=0 THEN
        Problems(2) = "1. line=[" + Par$ + "]"
        Problems(3) = "First Image Mark (~) not found!"
        problem_file(Topic$,3)
    END IF
4300:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION FileMoveSupport ALIAS "z4400" (Support$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4400

    Support$ = TRIM$(Support$)
    IF LEN(Support$) = 0 THEN EXIT FUNCTION
    support$ = (opFile$ + "," + support$)
    fileF$ = Control$  + "file_required.txt"
    gg = FREEFILE
    '---------------------------
    OPEN fileF$ FOR INPUT AS gg
        Image
        '----------------
        DO UNTIL EOF(gg)
            LINE INPUT#gg,x$
            '----------------
            IF x$ = support$ THEN
                CLOSE gg
                '--------
                EXIT FUNCTION
            END IF
        LOOP
    CLOSE gg
    '--------
    '----------------------------
    OPEN fileF$ FOR APPEND AS gg
        PRINT#gg,support$
    CLOSE gg
    '--------
4400:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION create_link$ ALIAS "z4500" (Support$,TCM)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4500

    Topic$ = "18 IN -> " + opFile$ + "->"
    Problems(1) = "Create Link Text Code Mark 4 "
    support$ = ""
    a = INSTR(Par$,"{")
    b = INSTR(a+1,Par$,"}")
    c = INSTR(b+1,Par$,"}")
    IF a>0 AND c=0 AND b>a THEN
        c = b
        b = INSTR(a+1,Par$,"{")
        IF b>a AND b<c THEN
            Problems(2) = "0.1 line=[" + Par$ + "]"
            Problems(3) = " {link}display} bracket order problem repaired!"
            MID$(Par$,b,1) = "}"
            problem_file(Topic$,3)
        ELSE
            Problems(2) = "0.2 line=[" + Par$ + "]"
            Problems(3) = " {link}display} brackets corrupt!"
            problem_file(Topic$,3)
            EXIT FUNCTION
        END IF
    ELSEIF a=0 OR b=0 OR c=0 OR c<b OR b<a THEN
            Problems(2) = "0.3 line=[" + Par$ + "]"
            Problems(3) = " {link}display} brackets corrupt!"
            problem_file(Topic$,3)
            EXIT FUNCTION
    END IF
    a = INSTR(Par$,"{")
    a$ = LEFT$(Par$,a-1)
    b = INSTR(Par$,"}")
    b$ = MID$(Par$,a+1,b-a-1)
    c = INSTR(b+1,Par$,"}")
    c$ = MID$(Par$,b+1,c-b-1)
    d$ = MID$(Par$,c+1)
    e$=""
    a1 = INSTR(c$,"<")
    b1 = INSTR(a1+1,c$,">")
    c1 = LEN(c$)
    IF a1>0 AND b1>a1 THEN
        e$=MID$(c$,b1+1)
        c$=MID$(c$,a1,b1-a1+1)
    ELSE
        c1=0
    END IF
    IF c1>0 THEN
        a$=a$ + "<div class='img'>"
        d$="<div class='desc'>" + e$ + "</div></div>" + d$
    END IF
    x = INSTR(b$,"http:") OR INSTR(b$,"\") OR opFile$ = "head"
    IF x = 0 THEN
        b$ = LCASE$(b$)
        support$ = b$
        Image
        '--------------------
        FOR x = 1 TO LEN(b$)
            IF MID$(b$,x,1) = SPACE$(1) THEN MID$(b$,x,1) = "_"
        NEXT x
        IF  support$ <> b$ THEN
            Problems(2) = "4. File=[" + support$ + "] changed to [" + b$ + "]"
            Problems(3) = "Spaces in file names not supported for internal Files!"
            Problems(4) = "[" + opFile$ + "] may need repair!"
            problem_file(Topic$,4)
        END IF
        Support$ = b$
    END IF
    IF TCM = 4 THEN
        b$ = b$ + "' target='_blank"
    ELSE
        b$ = b$ + "' target='tail"
    END IF
    Par$ = a$ + "<a href='" + b$ + "'>" + c$ + "</a>" + d$
4500:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION nest_subtract ALIAS "z4600" (TM) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4600

    yes=0
    nest(0) = 0
    nest(0) = nest(nNest)     '| set nest(0) to last tag used
    ' ______________________________________________________
    '/for nesting; last in - First Out
    IF TM < 0 THEN            '| remove nested item
        Image
        '------------------
        FOR x = 1 TO nNest
            IF ABS(TM) = nest(x) THEN yes = x
        NEXT x
        IF yes > 0 THEN       '| nested item found
            FUNCTION = nest(yes)
            nNest = nNest-1
            'eliminate oldest nested item
            Image
            FOR y = yes TO nNest
                    nest(y) = nest(y+1)
            NEXT y
            IF nest(yes) <> nest(0) THEN
                Topic$ = "19 IN -> " + opFile$ + "-> CLOSE NESTING ERROR"
                Problems(1) = "TCM" + TRIM$(STR$(TM)) + " LAST IN - NOT FIRST OUT"
                Problems(2) = "Last Tag Removed=(" + STR$(nest(yes)) + " )"
                Problems(3) = "Last Tag Recorded=[" + STR$(nest(0)) + "]"
                problem_file(Topic$,3)
            END IF
        ELSE
            IF TM<-2 THEN
                FUNCTION=TM    '| open tag not found'| close tag nest not found
                Topic$ = "20 IN -> " + opFile$ + "-> CLOSE NESTING ERROR"
                Problems(1) = "TCM" + TRIM$(STR$(TM)) + " CLOSE TAG NOT FOUND"
                Problems(2) = "Last tag Recorded=[" + STR$(nest(0)) + "]"
                Problems(3) = "This can result in unstable HTML!"
                CALL problem_file(Topic$,3)
                IF ERN > 0 THEN ERROR ERN
            END IF
        END IF
    ELSEIF TM > 0 THEN        '| create nested item
        'check for open tag
        Image
        FOR x = 1 TO nNest
            IF TM = nest(x) THEN yes = x
        NEXT x
        IF yes > 0 THEN
            FUNCTION = nest(yes)
            'close open tag
            nNest = nNest-1
            Image
            '--------------------
            FOR y = yes TO nNest
                nest(y) = nest(y+1)
            NEXT y
            IF TM>2 THEN
                Topic$ = "21 IN -> " + opFile$ + "-> OPEN NESTING ERROR"
                Problems(1) = "TCM" + TRIM$(STR$(TM)) + " OPEN TAG FOUND"
                Problems(2) = "Last Tag Removed=(" + STR$(yes) + " )"
                Problems(3) = "Last Tag Recorded=[" + STR$(nest(x-1)) + "]"
                problem_file(Topic$,3)
            END IF
        END IF
        nNest = nNest + 1
        nest(nNest) = TM
    END IF
4600:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
SUB internal_head ALIAS "z4800" (Title$)
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 4800
'------------------
    t1 = FREEFILE
    t2 = FREEFILE
    '-----------------------------------------------
    OPEN trash$ + "last_head.HTML" FOR APPEND AS t1
        PRINT#t1,"      <style type='text/css' media='screen'>"
        IF opFile$ = "head" THEN
            '-----------------------------------------------
            OPEN (WebberText$ + "head.css") FOR INPUT AS t2
        ELSE
            '-----------------------------------------------
            OPEN (WebberText$ + "tail.css") FOR INPUT AS t2
        END IF
            Image
            '----------------
            DO UNTIL EOF(t2)
                LINE INPUT#t2,x$
                '----------------
                x$ = TRIM$(x$)
                IF LEN(x$)> 0 AND LEFT$(x$,2) <> "/*" THEN
                    PRINT#t1,SPACE$(8) + x$
                END IF
            LOOP
        CLOSE t2
        '--------
        PRINT#t1,"      </style>"
        PRINT#t1,"  </head>"
    CLOSE t1
    '--------
4800:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
FUNCTION DOCTYPE_DECLARE ALIAS "z4900" (file$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 4900
'------------------
ff = FREEFILE
test=0
TRY
    '--------------------------
    OPEN file$ FOR INPUT AS ff
        LINE INPUT#ff,x$
        '----------------
        IF INSTR(x$,"<!DOCTYPE")>0 THEN test=1
        Image
        '----------------
        DO UNTIL EOF(ff)
            LINE INPUT#ff,x$
            '----------------
            IF INSTR(x$,"<html")>0 THEN test=test+1
        LOOP
    CLOSE ff
    '--------
CATCH
    CLOSE ff
    '--------
FINALLY
    IF test<>2 THEN
        '---------------------------
        OPEN file$ FOR OUTPUT AS ff
            PRINT#ff,"<!DOCTYPE HTML PUBLIC "+$DQ+"-//W3C//DTD HTML 4.01 Frameset//EN"+$DQ
            PRINT#ff,$DQ+"http://www.w3.org/TR/html4/frameset.dtd"+$DQ+">"
            ''''''''''''''''''''''''''''''''''''
            PRINT#ff,"<html lang=en>"
        CLOSE ff
        '--------
        DOCTYPE_DECLARE=-1
    END IF
END TRY
CLOSE ff
'--------
4900:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION DOCTYPE_heading ALIAS "z5000" (File$)
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 5000
'------------------
ff = FREEFILE

test=0
TRY
    '--------------------------
    OPEN file$ FOR INPUT AS ff
        Image
        '----------------
        DO UNTIL EOF(ff)
            LINE INPUT#ff,x$
            IF INSTR(x$,"<head ") THEN test=test+1
            IF INSTR(x$,"</head ") THEN test=test+1
            IF INSTR(x$,"<link rel") THEN test=test+1
            '----------------
        LOOP

        IF test=3 THEN EXIT TRY
    CLOSE ff
    '--------
CATCH
    CLOSE ff
    '--------
FINALLY
    ' ______________________________________________
    '/Web Site Descriptions!
    test=-1: fileF$ = Control$  + "!setup.txt"
    'get web site title
    Title$ = "TEXT WEBBER"
    REDIM sxx$(20)
    x1 = getarray("site title",sxx$(),fileF$)
    IF ISTRUE x1 THEN
        Image
        '---------------
        FOR x = 2 TO 20
            y$ = TRIM$(sxx$(x))
            IF LEN(y$)> 0 THEN Title$ = y$
        NEXT x
    END IF
    Title$ = single_quote_remove(Title$)
    '---------------------------
    OPEN file$ FOR OUTPUT AS ff
        PRINT#ff,"  <head>"
        PRINT#ff,"      <title>" + Title$ + "</title>"
        PRINT#ff,"      <meta charset='utf-8' />"
        IF LEN(TRIM$(keyword$))>0 THEN PRINT#ff,"      <meta name='keywords' content='" + keyword$ + "' >"
        IF LEN(TRIM$(descript$))>0 THEN PRINT#ff,"      <meta name='description' content='" + Day$ +"," + descript$ + "' >"
        PRINT#ff,"      <link rel='stylesheet' TYPE='text/css' media='screen' href='tail.css'>"
        PRINT#ff,"  </head>"
    CLOSE ff
    '--------

END TRY
DOCTYPE_heading = test
'--------
5000:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION File_Fix ALIAS "z5200" (x$) AS STRING
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 5200

LOCAL xx$,FLD$
    xx$ = TRIM$(x$)
    IF RIGHT$(xx$,1) = "\" THEN xx$ = LEFT$(xx$,LEN(xx$)-1)
    xx$ = TRIM$(xx$)
    IF LEN(xx$) = 0 THEN
        xx$="-1"
        EXIT FUNCTION
    END IF
    FLD$ = xx$
    yes = 0
    Image
    '---------------------
    FOR x = 1 TO LEN(xx$)
        y = ASC(MID$(xx$, x, 1))
        z = (y=32) OR (y=33) OR (y=46) OR (y=64) OR (y > 47 AND y < 58) OR (y > 94 AND y < 123) OR (y > 64 AND y < 90)
        IF z = 0 THEN MID$(xx$, x, 1) = "0"
    NEXT x
    DO
        xx$ = TRIM$(xx$)
        x = INSTR(xx$,SPACE$(2))
        IF x>0 THEN
            a$ = LEFT$(xx$,x)
            b$ = MID$(xx$,x)
            a$ = TRIM$(a$)
            b$ = TRIM$(b$)
            xx$ = a$ + SPACE$(1) + b$
        END IF
    LOOP UNTIL x = 0
    File_Fix = xx$
    IF xx$ <> FLD$ THEN
        Topic$ = "22 IN -> " + opFile$ + "-> Index Error"
        Problems(1) = "Field Characters not acceptable: ["+FLD$+"]"
        Problems(2) = "Changed to: ("+ xx$+")"
        Problems(3) = "This can cause problems with Index ordering!"
        problem_file(Topic$,3)
    END IF
5200:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
SUB Colate_AS2 ALIAS "z5300"
    'DIM MyArray$(1 TO 20) AS STRING
    'ARRAY SORT MyArray$(), COLLATE sortorder
    'cls
    '?
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 5300
'------------------
    LOCAL a$,b$,c$,d$,e1$,e2$,e3$,f$,g$
    sortorder= CHR$(33)
    '? "(";sortorder;")"
    'y=len(sortorder)
    '? 1,y
    Image
    FOR x = 65 TO 90: a$=a$ + CHR$(x): NEXT x    '| up case letters
    '? a$
    'y=y+LEN(a$)
    '? 2, LEN(a$)
    Image
    FOR x = 48 TO 57: b$=b$ + CHR$(x): NEXT x    '| numbers
    '? b$
    'y=y+LEN(b$)
    '? 3,LEN(b$)
    Image
    FOR x = 97 TO 122: c$=c$ + CHR$(x): NEXT x    '| low case letters
    '? c$
    'y=y+LEN(c$)
    '? 4,LEN(c$)
    Image
    FOR x = 34 TO 47: d$=d$ + CHR$(x): NEXT x
    '? d$
    'y=y+LEN(d$)
    '? 5, LEN(d$)
    Image
    FOR x = 58 TO 64: e1$=e1$ + CHR$(x): NEXT x
    '? e1$
    'y=y+LEN(e1$)
    '? 6,LEN(e1$)
    Image
    FOR x = 91 TO 96: e2$=e2$ + CHR$(x): NEXT x
    '? e2$
    'y=y+LEN(e2$)
    '? 7,LEN(e2$)
    Image
    FOR x = 123 TO 126: e3$=e3$ + CHR$(x): NEXT x
    '? e3$
    'y=y+LEN(e3$)
    '? 8,LEN(e3$)
    Image
    FOR x = 1 TO 32: f$=f$ + CHR$(x): NEXT x
    'y=y+LEN(f$)
    '? 9, LEN(f$)
    Image
    FOR x = 127 TO 256: g$=g$ + CHR$(x): NEXT x
    'y=y+LEN(g$)
    '? 10, LEN(g$)
    sortorder = sortorder + a$ + b$ + c$ +  d$ + e1$ + e2$ + e3$ + f$ + g$
    '? "here"
    '? len(sortorder)
    'waitkey$
    'CLSHERE
    '? sortorder
    '?
    'DIM MyArray$(20)
    'MyArray$(1)="!!!!.AAAA 1"
    'MyArray$(2)="AAAA.!!!! 2"
    'MyArray$(3)="bbbb.AAAA 3"
    'MyArray$(4)="!!!!.AAAA 4"
    'MyArray$(5)="!!!!.AAAA 5"
    'MyArray$(6)="!!!!.AAAA 6"
    'MyArray$(7)="!!!!.AAAA 7"
    'ARRAY SORT MyArray$(1) FOR 7, COLLATE sortorder,descend
    'CLS
    '? "HERE"
    'FOR X=1 TO 7
    '? MyArray$(X)
    'NEXT X
    'WAITKEY$
5300:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
FUNCTION Delay_for_action ALIAS "z5400" (delay) AS STRING
'call with 'IF Delay_for_action (10) = "Q" THEN END
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 5400

    LOCATE ,5: PRINT
    COLOR 12,14
    LOCATE ,5: PRINT STR$(delay) + " Sec To: Hit ENTER to Continue, Q to Quit, Any Key to Pause!";
    COLOR ForeRG,BackRG
    i$ = ""
    oldtimer = TIMER
    Image
    '-----------------------------------
    DO UNTIL (TIMER - oldtimer) > delay
        i$ = INKEY$
        IF UCASE$(i$) = CHR$(13) THEN EXIT DO
        IF UCASE$(i$) = "Q" THEN
            Delay_for_action = "Q"
            EXIT DO
        END IF
        IF UCASE$(i$) <> "" THEN
            ii$ = WAITKEY$
            oldtimer = TIMER
        END IF
    LOOP
    LOCATE 3,3
5400:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
SUB Frameit ALIAS "z5500" (Sw,F,B)
'call Frameit (-1,ForeRG,BackRG)
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 5500
'------------------
IF sw = -1 THEN
    nSize = 80
    lenth = 25
ELSEIF sw = 1 THEN
    nSize = 40
    lenth = 25
ELSE
    nSize = 80
    lenth = 50
END IF
CONSOLE SCREEN lenth,nSize

COLOR F,B
CLS
LOCATE 5,5
COLOR BlackBR,BorderRG
LOCATE 1, 1
PRINT SPACE$(nSize);
LOCATE lenth, 1
PRINT SPACE$(nSize);
LOCATE 1, 1
'----------------------
FOR i = 2 TO lenth - 1
    LOCATE i, nSize - 1
    PRINT SPACE$(2);
    LOCATE i, 1
    PRINT SPACE$(2);
NEXT i
COLOR F,B
LOCATE i+1, nSize+1: PRINT "\";
LOCATE 2,3: PRINT "\"
LOCATE 2,5

5500:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
SUB Problem_file ALIAS "z5600" (Topic$, Number)
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 5600

    gproblem = -1             '| Add after call: IF ISTRUE gProblem THEN ERROR 154
    COLOR 0,YellowRG
    CLS
    CONSOLE SET FOCUS
    Y$="see: "+ New_Prg_Name$ + ".LOG"
    LONGIS=LEN(Y$)+20: IF LONGIS>70 THEN LONGIS=70
    LOCATE 3,5: PRINT STRING$(LONGIS, "=")
    LOCATE ,10: PRINT "A PROBLEM NEEDS YOUR ATTENTION!"
    LOCATE ,10: PRINT Y$
    LOCATE ,5: PRINT STRING$(LONGIS, "=")
    pp = 20
    '-----------------------------------------------------------
    'OPEN Control$ + Prm_name$ + "_PROBLEM.txt" FOR APPEND AS pp
        x$ = STRING$(LONGIS, "-")
        PRINT#pp,x$
        LOCATE ,5: PRINT x$
        PRINT#pp,TAB(1);Topic$
        LOCATE ,9: PRINT Topic$
        PRINT#pp,x$
        LOCATE ,5: PRINT x$
        IF LEN(TRIM$(problems(0)))>0 THEN
            PRINT#pp,TAB(1);problems(x)
            LOCATE ,9: PRINT problems(x)
        END IF

        IF Number > 0 THEN
            X$= STRING$(LONGIS, "-")
            LOCATE ,5: PRINT X$
            PRINT#PP,X$
            Image
            '-------------------
            FOR x = 1 TO Number
                PRINT#pp,TAB(1);problems(x)
                LOCATE ,9: PRINT problems(x)
            NEXT x
            LOCATE ,5: PRINT X$
            PRINT#PP,X$
        END IF
        PRINT#pp, SPACE$(4)
    'CLOSE pp
    '--------
    REDIM problems(100)
    TIME(15): COLOR ForeRG,BackRG: CLS

5600:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END SUB
'''''''
SUB image ALIAS "z5700"
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 5700

    col& = CURSORX   'column location
    row& = CURSORY   'row location
    CURSOR OFF
    STATIC OT AS INTEGER 'output image blink
    OT = NOT (OT)
    LOCATE 1,1
    IF OT THEN PRINT "{"; CHR$(60); "}" ELSE PRINT "{"; CHR$(62); "}"
    LOCATE row&, col&
    CURSOR ON
5700:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END SUB
'''''''
FUNCTION Time ALIAS "z5800" (x) AS STRING
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 5800

' ______________________
'/Timer Delay with Choice
'outputs Q, [gProblem=0 (error or no error)]
    CURSOR OFF
    row& = CURSORY + 1
    IF x<5 THEN x=5
    COLOR RedRG
    DO
        ii$ = INKEY$
    LOOP UNTIL ii$=""
    oldtimer = TIMER + x
    DO
        ii$ = INKEY$
        IF ii$ = CHR$(27) THEN
            gproblem = 0
            EXIT DO
        ELSEIF ii$ = SPACE$(1) THEN
            ii$ = WAITKEY$
            oldtimer = TIMER + x
        ELSEIF ii$ = CHR$(13) THEN
            EXIT DO
        ELSEIF UCASE$(ii$) = "Q" THEN
            TIME="Q"
            EXIT DO
        END IF
        LOCATE Row&,5
        y=oldtimer-TIMER
        PRINT "("+TRIM$(STR$(ABS(y))) + ") [Q]=Quit [ESC]=Ignore [RETURN]=Continue [SPACE]=Pause"
    LOOP UNTIL (y<1)
    CURSOR ON
5800:
IF ERR>0 THEN ERN=ERR: ERNerror$=FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION check_enity_marks (nPar$,TCM) AS STRING

    'Unicode entity references should be reserved for use in
    'documents certain to be written in
    'the UTF-8 or UTF-16 character sets.
    ' _________________________________________________
    '/Set any double quote marks to single quote mark
    x=1:x5=0 '---------------------------------------------
    DO
        x=INSTR(x,nPar$,CHR$(34))
        IF x>0 THEN MID$(nPar$,x,1)="'"
    LOOP UNTIL x=0

    IF ABS(TCM)=9 THEN
        EXIT FUNCTION
    END IF
    gPar$=nPar$
    IF ABS(TCM)=1 OR ABS(TCM)=2 OR ABS(TCM)=3 OR ABS(TCM)=4 OR ABS(TCM)=6 THEN
        check_enity_marks=gPar$
        EXIT FUNCTION
    END IF
    ' ________________________________________
    '/Add entity for unbalanced HTML tag marks
    a1$="":a$=nPar$:b$="":c$=""
     DO
        x1=INSTR(1,a$,"<")
        IF x1>0 THEN
            x2=INSTR(x1,a$,">")
            IF x2>0 THEN
                x3=INSTR(x2,a$,"</")
                IF x3>0 THEN x4=INSTR(x3,a$,">")
            END IF
        END IF
        IF x4>0 THEN
            IF LEN(a$)>x4 THEN
                c$=MID$(a$,x4+1)
            END IF
            b$=MID$(a$,x1,x4-x1+1)
            IF x1>1 THEN a$=LEFT$(a$,x1-1)ELSE a$=""
        END IF
        IF LEN(a$)>0 THEN
            IF INSTR(a$,"<")>0 OR INSTR(a$,">")>0 THEN
                REPLACE "<" WITH "&lt;" IN a$ ' &gt;
                REPLACE ">" WITH "&gt;" IN a$ ' &gt;
            END IF
            IF INSTR(a$,"'")>0 THEN
                REPLACE "'" WITH "&lsquo;" IN a$
            END IF
        END IF
        IF x4>0 THEN
            a1$=a1$+a$+b$
            b$=""
            a$=c$
            c$=""
            x5=x5+1
            x4=0

        ELSE
            nPar$=a1$+a$+b$+c$
            check_enity_marks=gPar$
            EXIT DO
        END IF
    LOOP


END FUNCTION
''''''''''''''''''
FUNCTION Help_File(file$)
    '!Quick Help and Move_Graphics.txt
    f1 = FREEFILE
    TRY
        '-------------------------------------------------------
        OPEN File$ FOR INPUT AS f1
        CLOSE f1
        '-------
    CATCH
         CLOSE f1
        '----------------------------------------------------------------------
        OPEN File$ FOR OUTPUT AS f1
            PRINT#f1,"          If you wish to use the code, in whole or in part,"
            PRINT#f1,"        email me [hap2u@hotmail.com] for easy to get permission!"
            PRINT#f1,"===================================================================================="
            PRINT#f1,"Text Code Mark","Action it will Perform","Form Structure"
            PRINT#f1,"==============","======================","============="
            PRINT#f1,"  ( :-9 )","Move a HTML Document to a Sub-Folder","Folder_Name:-9"
            PRINT#f1,"  ( :+9 )","The CSS Style Sheet is placed inside","At top of page"
            PRINT#f1,"         ","the PAGES HTML Header Code.",":+9"
            PRINT#f1,"  ( :+9 )","A second TCM +9 places a pages index",":+9:+9"
            PRINT#f1,"         ","on a documents right column",""
            PRINT#f1,"  ( :+8 )","Bold front requires ending :-8",":+8 ...:-8"
            PRINT#f1,"  ( :+7 )","Indented Oblique Bold paragraph text",":+7...:-7"
            PRINT#f1,"         ","requires ending :-7",""
            PRINT#f1,"  ( :+6 )","Keep Text Format requires ending :-6",":+6...:-6"
            PRINT#f1,"  ( :+5 )","Start a new line with a line spacer","...:+5"
            PRINT#f1,"  ( :-5 )","Start a New Line without a spacer","...:-5"
            PRINT#f1,"  ( :+4 )","Open the linked page in a new","{tail.html}new_window}:+4"
            PRINT#f1,"         ","Browser Window",""
            PRINT#f1,"  ( :-4 )","Open an Interior/Exterior Link in the","{tail.html}Home_Page}:-4"
            PRINT#f1,"         ","tail Frame",""
            PRINT#f1,"  ( :+3 )","Transfer a File from the HOLD_HTML_FILES","~thirdimage.jpg~ :+3"
            PRINT#f1,"         ","Folder to the page's folder",""
            PRINT#f1,"  ( :-3 )","Display an inline image on your page","~secondimage.jpg~ :-3"
            PRINT#f1,"  ( :+1 )","Underline bracketed text",".[...]..:+1"
            PRINT#f1,"  ( :+2 )","Sub Index Entry without registering","....:+2"
            PRINT#f1,"         ","index link",""
            PRINT#f1,"  ( :-2 )","Sub Index Entry with index link","....:-2"
            PRINT#f1,"  ( :-1 )","Main Index Entry               ","....:-1"
            PRINT#f1,"------------------------------------------------------------------------------------"
            PRINT#f1,"   The Sequence of Actions [Move_Graphics] Will Execute"
            PRINT#f1,"   ''''''''''''''''''''''''''''''''''''''''''''''''''''"
            PRINT#f1,"1. Move_Graphics.exe will rename All [??ground.JPG] files as [JPEG],"
            PRINT#f1,"   then copy them TO ["+HTMLfolders$+"] and [" +HoldFiles$+"]"
            PRINT#f1,"   To turn on a background; in these Folders rename the JPEG"
            PRINT#f1,"   extension to JPG!"
            PRINT#f1,"    "
            PRINT#f1,"2. Move_Graphics.exe will RENAME all [.JPEG] files to [.JPG].
            PRINT#f1,"   and then COPY all [.JPG] files to ["+HoldFiles$+"]"
            PRINT#f1,"    "
            PRINT#f1,"3. Move_Graphics.exe will RENAME all [.JPG] files to [.JPEG]."
            PRINT#f1,"------------------------------------------------------------------------------------"
            PRINT#f1,"IMPORTANT! Each time you run The Program, it will erase the files"
            PRINT#f1,"in the Folder ["+HTMLfromTEXT$+"]. Therefore place the image and HTML"
            PRINT#f1,"files that your text files require in Folder ["+HoldFiles$+"]."
            PRINT#f1,"------------------------------------------------------------------------------------"
            PRINT#f1,"You can use the 'Libre Office' program to turn the HTML documents into PDF documents."
            PRINT#f1,"------------------------------------------------------------------------------------"
            PRINT#f1,"The Program will organize a text file by any surrounded period identifiers,(2.a)"
            PRINT#f1,"it finds at the start of a [-1 or -2,+2] INDEX line."
            PRINT#f1,"You can use this to reorganize your text file and its HTML output."
            PRINT#f1,"------------------------------------------------------------------------------------"
            PRINT#f1,"THE '!GOODCOPY FOLDER' contains processed text files created from files in"
            PRINT#f1,"@WEBBER_TEXT. Load these text files into a word processor to create other"
            PRINT#f1,"document types such as 'html_from_text.doc'."
            PRINT#f1,"------------------------------------------------------------------------------------"
            PRINT#f1,"You can clean up unused files by renaming the @HTML_HOLD_FILES Folder as"
            PRINT#f1,"'@DELETE' then run the Program."
        CLOSE f1
        '-------
        Help_File=-1
    END TRY


END FUNCTION
''''''''''''''''''''
FUNCTION check_as2 (letter$) AS LONG
IF ERN > 0 THEN EXIT FUNCTION
ON ERROR GOTO 5550
    IF LEN(TRIM$(letter$))=0 THEN
        Topic$ = "ASc check ordered on nul characters"
        Problems(1) = "An Error in an Index Identifier?"
        Problems(2) = "This can cause a problems with Index ordering!"
        problem_file(Topic$,2)
        EXIT FUNCTION
    END IF

    ':: DOS ASC2 Token Codes (1-126) :or: Drunk Programer creates AS2 Codes
    '  1[ CTRL A ]   2[ CTRL B ]   3[ CTRL C ]   4[ CTRL D ]   5[ CTRL E ]   6[ CTRL F ]
    '  7[ CTRL G ]   8[ CTRL H ]   9[ CTRL I ]  10[ CTRL J ]  11[ CTRL K ]  12[ CTRL L ]
    ' 13[ CTRL M ]  14[ CTRL N ]  15[ CTRL O ]  16[ CTRL P ]  17[ CTRL Q ]  18[ CTRL R ]
    ' 19[ CTRL S ]  20[ CTRL T ]  21[ CTRL U ]  22[ CTRL V ]  23[ CTRL W ]  24[ CTRL X ]
    ' 25[ CTRL Y ]  26[ CTRL Z ]
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' 27[ ESCAPE ]  28[ none ]  29[ none ]  30[ none ]  31[ none ]
    ' 32[ SPACE ]  33[ ! ]  34[ " ]  35[ # ] '|
    ' 36[ $ ]  37[ % ]  38[ & ]  39[ ' ]  40[ ( ]  41[ ) ] '|
    ' 42[ * ]  43[ + ]  44[ , ]  45[ - ]  46[ . ]  47[ / ]
    ' 48[ 0 ]  49[ 1 ]  50[ 2 ]  51[ 3 ]  52[ 4 ]  53[ 5 ] '|
    ' 54[ 6 ]  55[ 7 ]  56[ 8 ]  57[ 9 ]
    ' 58[ : ]  59[ ; ] '|
    ' 60[ < ]  61[ = ]  62[ > ]  63[ ? ]  64[ @ ]
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' 91[ [ ]  92[ \ ]  93[ ] ]  94[ ^ ]  95[ _ ] 96[ ` ] '|
    ' 123[ { ]  124[ | ]  125[ } ] 126[ ~ ]
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'48 to 57 : 0 to 9
'65 to 90 : A-Z
'97 to 122: a-z


FOR x=1 TO LEN(letter$)
    y = ASC(letter$,x)
    z=((y>47) AND (y<58)) OR ((y>64) AND (y<91)) OR ((y>96) AND (y<123))
    IF z=0 THEN EXIT FOR
NEXT x
check_as2=z

5550:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION

''''''''''''''''''''''''''''''
