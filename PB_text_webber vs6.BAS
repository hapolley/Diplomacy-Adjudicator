'GLOBALIZED: 2014.03.14
#CONSOLE ON 'if set to 'OFF' program must produce no console output
#COMPILE EXE
#REGISTER NONE
#DEBUG ERROR ON 'checks for array boundary and null-  errors
'#INCLUDE"Win32Api.Inc"
DEFLNG A-Z  'defines numeric variables starting with letter of Alfabet as long
'------------------------------------------------------------------------------
'sub to function
'TO DO ---- Enable logoes and discriptions in Main Frame
'MainDummy$ =  WebberText$ + "MainDummy.tmp" is created from MainFrame Links
'it accumulates information in form [{file_name.html}(~file_name.jpg~)(Discription)}:-4(-3)]
'This file can be used to EDIT or Create WebberText$ + "MainDummy.txt"
'if in the created "MainDummy.html" the main frame finds one of its links in form 'file name.html'
'it replaces the Text Link with the Discriptive and graphic link
'have ending program call convert with rename information, convert then renames program back to TEXT4WEBBER!
' ________________________________________________________________________
'/Programs Objectives                                                     \
'Purpose: To have a Power Basic program convert all [(file).txt] files in  |
'Folder [HTML Text Files] to html files with a Linked Index added |
'to each File!                                                    |
'Method:  HTML tags <pre></pre> will be added to the start and end of each |
'TEXT file record and outputted to the [same name .html] file!     |
'\________________________________________________________________________/
MACRO CONST = MACRO
CONST ForeRG = 0              '| | | | | | Black
CONST BackRG = 2              '| | | | | | Green
CONST BorderRG = 8            '| | | | | | Gray
CONST BackBR = 10             '| | | | | | Light green
CONST RedRG = 4               '| | | | | | message output
CONST RedBR = 12              '| | | | | | warning background
'unused color
CONST white = 7
'OTHER MACROS
CONST PTH = 20
' _____________________________________
'/a5.Type Required by Random File Arrays
TYPE recorded
    record AS STRING * 254
END TYPE
'------------------------------------------------------------------------------
DECLARE SUB ATTEMPT_RESTORE_FROM_DELETE
DECLARE SUB COLATE_AS2
DECLARE SUB Frameit (SW,F,B)
DECLARE SUB GET_DIR (WEREDIR$)
DECLARE SUB HEADING (TITLE$)
DECLARE SUB HFRAME_MAIN (FILE$)
DECLARE SUB IMAGE
DECLARE SUB INTERNAL_HEAD (TITLE$)
DECLARE SUB PROBLEM_FILE (TOPIC$, NUMBER)        '| 2005/01/10
DECLARE SUB SORT_FILE (FILE$,LAST)
'----------------------------------
DECLARE FUNCTION BACKUP_TAB (FILE$) AS STRING
DECLARE FUNCTION CONST_INDEX_LINKAGE (PAR$,TCM)
DECLARE FUNCTION CREATE_IMAGE$ (SUPPORT$)
DECLARE FUNCTION Create_Index_Page (FILE$) AS LONG
DECLARE FUNCTION CREATE_LINK$ (SUPPORT$,TCM)
DECLARE FUNCTION CREATE_STYLESHEETS (FILE$) AS LONG
DECLARE FUNCTION DELAY_FOR_ACTION(DELAY) AS STRING
DECLARE FUNCTION DIRCOPY (WERE$,GOING$,OUTFILE$,TXTYPE$) AS LONG
DECLARE FUNCTION DOCTYPE_DECLARE (FILE$)
DECLARE FUNCTION DOCTYPE_HEADING (FILE$)
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
DECLARE FUNCTION SETUP_FILE (FILE$) AS LONG
DECLARE FUNCTION SINGLE_QUOTE_REMOVE (PHRASE$) AS STRING
DECLARE FUNCTION SKIP_TAGS (SPAR$,OTAG$,CTAG$,TMPLENGTH) AS LONG
DECLARE FUNCTION TAGS (XX$,Z) AS LONG
DECLARE FUNCTION TCM_FIRST_SET$ (HPAR$,TCM)
DECLARE FUNCTION TCM_SECOND_SET$(TCM)
DECLARE FUNCTION TCM_THIRD_SET$(TCM)
DECLARE FUNCTION Time (x) AS STRING
DECLARE FUNCTION check_enity_marks (nPar$,TCM) AS STRING
'-----------------------------------
GLOBAL ERNerror$,ERN AS LONG,NRL AS LONG,gProblem AS LONG
GLOBAL Prm_name$,New_Prg_Name$,Expire$,eMail$
GLOBAL sortorder AS STRING
GLOBAL Fore AS LONG,Back AS LONG            '| COLOR control variables
GLOBAL Trash$,Control$,werePROB$,spacing$   '| Program Control Data Folders
GLOBAL HTMLfromTEXT$,DownLoad$,HTMLfolders$,WebberText$,HoldFiles$,WebberTextBackup$,MainDummy$
GLOBAL prob AS LONG,link AS LONG,PRElength AS LONG,place AS LONG,Cast AS LONG,indexing AS LONG
GLOBAL nNest AS LONG,Format AS LONG,Index AS LONG,existindex AS LONG,head_int AS LONG
GLOBAL Lnumber AS LONG,Day$,Dest_Folder$,YearFolder$,Good$,GoodCopy$,descript$,keyword$
GLOBAL action$,opFile$,owner$,par$,New_Dir$,tpFile$,mIndex$,sIndex$,firstimage$,tailground$
GLOBAL generator$,author$
'-------------------------
FUNCTION PBMAIN
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
    Expire$="2019.01"
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
'/b2. setup Main Folders
    ON ERROR RESUME NEXT
    Trash$ = "@TRASH"
    MKDIR Trash$
    Trash$ = Trash$ + "\"
    KILL Trash$ + "*.*"
    Control$ = "@CONTROL"
    MKDIR Control$            '| common files used to construct other files
    Control$ = Control$ + "\"
    WebberText$ = "@WEBBER_TEXT"
    MKDIR WebberText$         '| stores text files to be processed to HTML_FROM_TEXT
    WebberText$ = WebberText$ + "\"
    GoodCopy$ = "@GOODCOPY"
    MKDIR GoodCopy$
    GoodCopy$ = GoodCopy$ + "\"
    KILL GoodCopy$ + "*.*"
    HTMLfolders$ = "HTML_FOLDERS"
    MKDIR HTMLfolders$        '| root directory for website three frames
    HTMLfolders$ = HTMLfolders$ + "\"
' ___________________________________________________________________
'/b3.setup second level Folders
    DownLoad$ = HTMLfolders$ + "DOWNLOADS"    '| zipped files for downloading - NOT USED
    MKDIR DownLoad$
    DownLoad$ = DownLoad$ + "\"
    HTMLfromTEXT$ = HTMLfolders$ + "HTML_FROM_TEXT"    '| converted from text HTML files
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
' _____________________________________________________________________________
'/b4.Create Backup Folder From Month Date
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
' _____________________________________________________________________________
'/b5.Clear Temporary Files
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
' _______________________________________
'/b6.Dim Arrays and Random Records
    DIM DUMMYrecord AS GLOBAL recorded
    DIM record(0 TO 2) AS GLOBAL recorded
    REDIM Pathit(PTH) AS GLOBAL STRING
    REDIM Problems(100) AS GLOBAL STRING
    ERRCLEAR
    ON ERROR GOTO error_trap
' ______________________________________
'/used by security program do not remove
    test_checksum:
    expire$ = TRIM$(expire$)
    IF LEN(expire$) = 0 THEN expire$ = day$
' __________________________________________
'/Serial Number Generator
    n1$ = "5"                 '| version number
    x$ = "abcdefghijklmnopqrstuvwxyz"
    Image
    '------------------------
    020: NRL = 020
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
    author$=Owner$

    ' ___________________________________________
'/security setup depending upon owner name
'from setup2.txt file
' _________________________________________________________
'/                                                         \
'<              Main Operating Routines                      >
'\_________________________________________________________/
' _________________________________________________________
    Frameit(-1,ForeRG,BackRG)
    LOCATE 5,5
    Colate_AS2
    ' _________________________________________
    '/c1.SetUP Global Variables
    link = 0
    prob = 0
    Cast = 78
    Spacing$="&ensp;"
    OPEN Control$ + Prm_name$ + "_PROBLEM.txt" FOR OUTPUT AS 1:CLOSE 1
    KILL Control$ + Prm_name$ + "_PROBLEM.txt"
    ' __________________________________________________
    '/Program Log for Error Checks and Program structure
    OPEN Prm_name$+".LOG" FOR OUTPUT AS 20

    LOCATE ,5: PRINT "CREATING HTML STRUCTURES"
    ' _________________________________________
    '/Create Default Record File
    file$ = Control$  + "!Setup.TXT"
    Exists = setup_File(file$)
    IF ISTRUE Exists THEN
        LOCATE ,5: PRINT "- 01. ";file$;" Created!"
    ELSE
        LOCATE ,5: PRINT "01. ";file$;" Exists!"
    END IF
    REDIM sxx$(20)
    x1 = getarray("index html",sxx$(),fileF$)
    firstimage$ = "firstimage"
    y$ = TRIM$(sxx$(3))
    IF LEN(y$) > 0 THEN firstimage$ = LCASE$(y$)
    x=INSTR(firstimage$,".")
    IF x>0 THEN firstimage$=LEFT$(firstimage$,x-1)
    ' ________________________________________
    '/SEARCH ENGINE KEYS keyword$ descript$
    'get descriptions
    REDIM sxx$(20)
    descript$ = "GENERATED BY TEXT WEBBER A WEB SITE CREATOR"
    REDIM sxx$(20)
    x1 = getarray("spider description",sxx$(),fileF$)
    IF ISTRUE x1 THEN
        Image
        '---------------
        025: NRL = 025
        FOR x = 2 TO 20
            IF LEN(LTRIM$(sxx$(x))) > 0 THEN
                descript$ = descript$ + "," + TRIM$(sxx$(x))
            END IF
        NEXT x
    END IF
    'get keywords
    REDIM sxx$(20)
    keyword$ = n1$            '| serial number from version, registration: name & Date
    x1 = getarray("spider keywords",sxx$(),fileF$)
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
    descript$ = single_quote_remove(descript$)
    file$ = WebberText$ + "DOCTYPE_DECLARE.tmp"
    Exists = DOCTYPE_DECLARE (file$)
    IF ISTRUE Exists THEN
        LOCATE ,5: PRINT "- 02. DOCTYPE_DECLARE Created!"
    ELSE
        LOCATE ,5: PRINT "02. DOCTYPE_DECLARE Exists!"
    END IF
    file$ = WebberText$ + "DOCTYPE_heading.tmp"
    Exists = DOCTYPE_heading (file$)
    IF ISTRUE Exists THEN
        LOCATE ,5: PRINT "- 03. DOCTYPE_heading Created!"
    ELSE
        LOCATE ,5: PRINT "03. DOCTYPE_heading Exists!"
    END IF

    ' ____________________________________
    '/First run Create and Display LICENSE
    file$ = Control$ + "LICENSE.TXT"
    disclaim = license(file$)
    IF ISFALSE disclaim THEN
        LOCATE ,5: PRINT "04. License exists! Not first run of program!"
    ELSE
        FILECOPY File$, WebberText$ + LCASE$("LICENSE.TXT")
        LOCATE ,5: PRINT "- 04. First run of Program License created!"
    END IF

    ' _________________________________________
    '/SET width of tail display
    REDIM sxx$(20)
    x1 = getarray("WIDTH PX",sxx$(),(Control$  + "!setup.txt"))
    IF LEN(TRIM$(sxx$(2)))>0 THEN Cast = VAL(sxx$(2))
    IF Cast<41 THEN Cast=40
    IF Cast>120 THEN Cast=120
    PRElength = Cast          '| frame line length
    LOCATE ,5: PRINT "04. Line Length set to: ";Cast
    ' _______________________________________________
    '/Load and Adjust Style Sheets
    file$ = "tail.css"
    Exists = CREATE_STYLESHEETS (file$)
    IF ISTRUE Exists THEN
        LOCATE ,5: PRINT "05. Style Sheet " + file$ + " Exists!"
    ELSE
        LOCATE ,5: PRINT "- 05. Style Sheet ";file$;" Created!"
    END IF

    file$ = "head.css"
    Exists = CREATE_STYLESHEETS (file$)
    IF ISTRUE Exists THEN
        LOCATE ,5: PRINT "06. Style Sheet " + file$ + " Exists!"
    ELSE
        LOCATE ,5: PRINT "- 06. Style Sheet ";file$;" Created!"
    END IF
    ' _________________________________________________
    '/Create and/or distribute CSS
    035: NRL = 035
    TRY
        '-------------------------------------------------------
        OPEN WebberText$ + "tail.css" FOR INPUT AS 10: CLOSE 10
    CATCH
        FILECOPY Control$ + "tail.css",WebberText$ + "tail.css"
    END TRY
    040: NRL = 040
    TRY
        '-------------------------------------------------------
        OPEN WebberText$ + "head.css" FOR INPUT AS 10: CLOSE 10
    CATCH
        FILECOPY Control$ + "head.css",WebberText$ + "head.css"
    END TRY
    LOCATE ,5: PRINT "07a. Moving CSS to Folders!"
    ON ERROR RESUME NEXT
    FILECOPY (WebberText$ + "tail.css"),(HTMLfolders$ + "tail.css")
    FILECOPY (WebberText$ + "head.css"),(HTMLfolders$ + "head.css")
    FILECOPY (WebberText$ + "tail.css"),(HTMLfromTEXT$ + "tail.css")
    FILECOPY (WebberText$ + "tail.css"),"tail.css"

    ' _____________________________________________
    '/c4.Creating Instruction File
    file$ = Control$ + "html_from_text.txt"
    exist = instructions(file$)
    ' __________________________________________________
    '/c5.Converting and Moving IMAGE FILES with CONVERT.EXE
    LOCATE ,5: PRINT "07b. Convert is moving Graphic Files From Root!"
    cmd$ = Prm_name$ +","+  Owner$ + "," + expire$
    x$="convert.exe" + cmd$
    x???=SHELL (x$,6)         '| converts renames and moves image files
    ERRCLEAR
    ON ERROR GOTO error_trap
    045: NRL = 045
    TRY
        '-------------------------------------------------------
        OPEN "!Moving Image Files From Root.txt" FOR INPUT AS 3
        CLOSE 3
        '-------
    CATCH
        CLOSE 3
        '-------
        '---------------------------------------------------
        OPEN "!Controlling Image Files.txt" FOR OUTPUT AS 3
            PRINT#3,"1. The Webber Program will convert all BMP, GIF, PNG files"
            PRINT#3,"   placed in the Root FOLDER to JPG Files."
            PRINT#3,"   "
            PRINT#3,"2. After the JPG files are Copied the originals are renamed as [.JPEG]."
            PRINT#3,"   "
            PRINT#3,"3. The '??ground.jpg' Files are copied to " + HTMLfolders$
            PRINT#3,"   ALL other jpg Files are Copied TO Folder ";HoldFiles$
            PRINT#3,"    "
            PRINT#3,"4. To prevent the HTML from looking for 'headground.jpg' head file"
            PRINT#3,"   remove 'url(headground.jpg)' from 'head.css' in Folder " + WebberText$
            PRINT#3, "    "
            PRINT#3,"5. To prevent the HTML from looking for '??ground.jpg' in other files"
            PRINT#3,"   remove url(headground.jpg), or url(mainground.jpg), or url(tailground.jpg),"
            PRINT#3,"   or url(indexground.jpg) from 'tail.css' in Folder " + WebberText$
            PRINT#3, "    "
            PRINT#3,"6. In Root Folder Run 'Convert.exe' to rename any .JPEG  Files to .JPG!"
        CLOSE 3
        '-------
    END TRY
    ' ___________________________________________
    '/d1.Creating Three Frame File
    ON ERROR GOTO error_trap
    file$ = WebberText$ + LCASE$("Frame.HTML")
    exist = hFrame_THREE(file$)
    IF ISTRUE exist THEN
        LOCATE ,5: PRINT "- 08. Creating Frame Structures: ";file$
    ELSE
        LOCATE ,5: PRINT "08. Frame Structure: ";file$;" already exists!"
    END IF
    ' __________________________________________________
    '/d2.Default Text of Head Frame File in Text Webber
    file$ = "head"
    exist = hFrame_Head(file$)
    IF ISTRUE exist THEN
        LOCATE ,5: PRINT "- 09. Created text for Heading Frame: ";file$
    ELSE
        LOCATE ,5: PRINT "09. Heading Frame Text: ";file$;" exists!"
    END IF
    ' ___________________________________________
    '/d3.Default Tail Frame File
    file$ = "tail"
    exist = hFrame_Tail(file$)
    IF ISTRUE exist THEN
        LOCATE ,5: PRINT "- 10. Created Tail Frame Output: ";file$
    ELSE
        LOCATE ,5: PRINT "10. Tail Frame Output: ";file$;" Exists!"
    END IF
    ' ___________________________________________
    '/d4.Default Index HTML Start up PAGE
    file$ = LCASE$("Index.HTML")
    exist = Create_Index_Page(file$)
    IF ISTRUE exist THEN
        LOCATE ,5: PRINT "- 11. Creating Index File: ";file$
    ELSE
        LOCATE ,5: PRINT "11. Index File: ";file$;" Exists!"
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
' _______________________
'/E2.Set Basic HTML Tags
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
                LOCATE ,5: PRINT "1. Processing Text File: ";opFile$
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
                    Topic$ = "1 IN -> " + Pathit(1) + "-> Empty File"
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
                LOCATE ,5: PRINT  "2. Writing results to Temporary File!"
                tpFile$ = trash$ + "process.tmp"
                '----------------------------
                075: NRL = 075
                OPEN tpFile$ FOR OUTPUT AS 2
                OPEN goodCopy$ + opFile$ + ".txt" FOR OUTPUT AS 8
                    ' __________________________________________________________
                    '/01e.Start Processing Text in WebberText$ + opFile$ + ".txt"
                    080: NRL = 080
                    OPEN Pathit(1) FOR INPUT AS 3    '| @TRASH$ + opFile$ + ".tmp"
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
                        prelength = Cast
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
                        Image
                        DO UNTIL EOF(3)
                            tcm_first_set$ (hPar$,TCM)
                            tcm_second_set$(TCM)
                            tcm_third_set$(TCM)
                        LOOP UNTIL EOF(3)
                    CLOSE 3
                    '-------
                    FixLength$
                    par$=TRIM$(par$)
                    Good$=RTRIM$(Good$)
                    IF LEN(Par$) >0 THEN PRINT#2,Par$
                    IF LEN(Good$)>0 THEN PRINT#8,Good$
                    Par$ = "": Good$ = ""
                CLOSE 8
                '-------
                IF nNest > 0 THEN    '| here
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
                LOCATE ,5: PRINT  "3. Completed processing Text Code Marks!"
                EXIT DO
            LOOP
            ' ______________________________________________
            '/create default or keep html !head
            LOCATE ,5: PRINT  "4. Sub Index added to Main Index!"
            ' __________________________________________________
            '/clear html file and add heading
            LOCATE ,5: PRINT  "5. Markup Heading created!"
            xx$ = LCASE$(opFile$) + ".html"
            IF New_Dir$ = HTMLfromTEXT$ THEN
                htmlFile$ = HTMLfromTEXT$ + xx$
            ELSE
                htmlFile$ = New_Dir$ + xx$
            END IF
            ' _______________________________________
            '/Change file name spaces to underscore
            DO
                y = INSTR(htmlFile$,SPACE$(1))
                IF y > 0 THEN MID$(htmlFile$,y,1) = "_"
            LOOP UNTIL y = 0
            ' ___________________________________________
            '/Clear HTML File and add sections
            LOCATE ,5: PRINT  "6. Creating: "; htmlFile$
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
            LOCATE ,5: PRINT  "7. "; htmlFile$;" Created!"
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
' _________________________________________________________
'/                                                         \
'<           MOVING FILES RECORDING PROBLEMS                 >
'\_________________________________________________________/
Frameit(-1,ForeRG,BackRG)   'x5
LOCATE 5,5
LOCATE ,5: PRINT  "1. Moving Required HTML_HOLD files to Folders!"
' ____________________________________________________________
'/copy style sheets from Webber Text Folder to directories
115: NRL = 115
TRY
    FILECOPY HTMLfolders$ + tailground$,HTMLfromTEXT$ + tailground$
CATCH
END TRY
'---------------------------------------------------------------
120: NRL = 120
OPEN Control$ + "file_not_found.txt" FOR OUTPUT AS 10: CLOSE 10
x5=0
TRY
    FILECOPY firstimage$ + ".jpeg",HoldFiles$ + firstimage$ + ".jpg"
CATCH
    TRY
        FILECOPY  HoldFiles$ + firstimage$ + ".jpg",firstimage$ + ".jpeg"
    CATCH
        '-----------------------------------------------------
        OPEN Control$ + "file_not_found.txt" FOR APPEND AS 10
            x$ = "root,INDEX.HTML,"+ firstimage$ + ".jpeg"
            PRINT#10,x$
            x5=-1
        CLOSE 10
        '--------
    END TRY
END TRY
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
        INPUT#fd,FolderName$
        'were$ equals folder name location
        were$ = HTMLfolders$ + FolderName$ + "\"
        IF HTMLfolders$ = FolderName$ + "\" THEN were$ = HTMLfolders$
        '1. copy css file to directory
        FILECOPY file2$,were$ + "tail.css"
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
                INPUT#fd2,folderHTML$    '| html file name
                IF LEN(LTRIM$(folderHTML$)) <> 0 THEN
                    ' _________________________________________________________
                    '/check list of image GIF files required for folder's name
                    fileF$ = Control$ + "file_required.txt"
                    '----------------------------------------
                    150: NRL = 150
                    OPEN fileF$ FOR APPEND AS fd1: CLOSE fd1
                    OPEN fileF$ FOR INPUT AS fd1
                        gif$ = "": Html$ = ""
                        Image
                        '-----------------
                        DO UNTIL EOF(fd1)
                            'get HTML file and gif names
                            INPUT#fd1,Html$,gif$
                            ' ________________________________________
                            '/is GIF required by Html file Name
                            'required gif$ found
                            IF LCASE$(folderHTML$) = LCASE$(Html$) THEN
                                ' ______________________________________________
                                '/check HTML_HOLD for existence of file
                                'If exist copy to were$
                                155: NRL = 155
                                OPEN hdFile$ FOR APPEND AS fd3: CLOSE fd3
                                OPEN hdFile$ FOR INPUT AS fd3
                                    x3 = 0
                                    Image
                                    '-----------------
                                    DO UNTIL EOF(fd3)
                                        INPUT#fd3,gif2$
                                        IF LCASE$(gif$) = LCASE$(gif2$) THEN
                                            x3 = -1
                                            EXIT DO
                                        END IF
                                    LOOP
                                CLOSE fd3
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
                                IF RIGHT$(LCASE$(folderHTML$),5) = "dummy" THEN
                                    xx$ = were$ + folderHTML$ + ".html"
                                    165: NRL = 165
                                    TRY
                                        KILL xx$
                                    CATCH
                                    END TRY
                                END IF
                                IF ISTRUE x3 THEN
                                    FILECOPY HoldFiles$ + gif$ ,were$ + gif$
                                    '-------------------------------------------------
                                    170: NRL = 170
                                    OPEN Control$ + "file_found.txt" FOR APPEND AS 10
                                        x$ = FolderName$ + "," + folderHTML$ + "," + gif$
                                        PRINT#10,x$
                                    CLOSE 10
                                    '--------
                                ELSEIF ISTRUE x4 THEN
                                    x$ = LCASE$(HTMLfolders$ + gif$)
                                    y$ = LCASE$(were$ + gif$)
                                    IF x$ <> y$ THEN
                                        FILECOPY x$,y$
                                    END IF
                                    '-------------------------------------------------
                                    175: NRL = 175
                                    OPEN Control$ + "file_found.txt" FOR APPEND AS 10
                                        x$ = FolderName$ + "," + folderHTML$ + "," + gif$
                                        PRINT#10,x$
                                    CLOSE 10
                                    '--------
                                ELSE
                                    '-----------------------------------------------------
                                    180: NRL = 180
                                    OPEN Control$ + "file_not_found.txt" FOR INPUT AS 10
                                        F=0
                                        DO UNTIL EOF(10)
                                            TRY
                                                INPUT#10,A$,B$,C$
                                                IF A$="HTML_FROM_TEXT" AND B$=folderHTML$ AND C$=gif$ THEN
                                                    F=-1
                                                    EXIT DO
                                                END IF
                                            CATCH
                                            END TRY

                                        LOOP
                                    CLOSE 10
                                    IF F=0 THEN
                                        OPEN Control$ + "file_not_found.txt" FOR APPEND AS 10
                                            x$ = FolderName$ + "," + folderHTML$ + "," + gif$
                                            PRINT#10,x$
                                            x5=-1
                                        CLOSE 10
                                        '--------
                                    END IF
                                END IF
                            END IF
                        LOOP
                    CLOSE fd1
                    '---------
                END IF
            LOOP
        CLOSE fd2
        '---------
    LOOP
CLOSE fd
'--------
185: NRL = 185

IF ISTRUE x5 THEN
    'Control$ + "file_not_found.txt"
    Topic$ ="3 IN CONVERTED TEXT FILES REFERENCED FILES NOT FOUND"
    Problems(1) = "Check [" + Control$ + "file_not_found.txt" + "]
    Problems(2) = "for missing File's Expected Location,HTML Page,File Name!"
    Problem_file(Topic$,2)
    attempt_restore_from_delete
END IF
IF ISTRUE x5 THEN
    Frameit(-1,ForeRG,BackRG)
    LOCATE 5,5
    LOCATE ,5: PRINT  "1. ERRORS! Moving Required HTML_HOLD files to Folders!"
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
    IF ERN =0 THEN ERN = ERR
    ON ERROR RESUME NEXT
    ' _____________________________________________
    '/Error Information to passon to desplay error
    DIM Einfo$(10)
    Einfo$(1)=Prm_name$
    Einfo$(2)=New_Prg_Name$
    Einfo$(3)=STR$(gProblem)
    Einfo$(4)=STR$(ERN)
    Einfo$(5)=ERNerror$
    Einfo$(6)=STR$(NRL)
    ' _______________________________________
    '/Record Open Files - 20 is log file

    CLOSE 21
    '--------
    '------------------------------------------
    OPEN Trash$ + "Paths.TXT" FOR OUTPUT AS 21
        Image
        '----------------
        FOR x = 1 TO PTH
            a$=""
            a$ = FILENAME$(x)
            IF LEN(a$) > 0 THEN PRINT#21,a$
        NEXT x
    CLOSE
    '-----
    CMD$=Einfo$(1)
    Image
    Image
    '------------
    FOR x=2 TO 6
        CMD$=CMD$+","+Einfo$(x)
    NEXT x
    x$ = "Display_error.exe " + cmd$
    pid??? = SHELL(x$)
END FUNCTION

FUNCTION dProcess ALIAS "z1100" (Par$)
ON ERROR RESUME NEXT
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
        order$ = ":-1:-2:+2:+5:-5:-6:-7:-8:+6:+7:+8:-4:+4:-3:+3:+1:+9:-9"
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
END FUNCTION
''''''''''''
FUNCTION fetch_file ALIAS "z1200" (were$,File$,fType$) AS LONG
ON ERROR RESUME NEXT
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
TRY
    '-------------------------------------
    OPEN Control$ + file$ FOR INPUT AS ff
    CLOSE ff
    '--------
    CREATE_STYLESHEETS = -1
    EXIT FUNCTION
CATCH
    CLOSE ff
    '--------
    CREATE_STYLESHEETS = 0
END TRY
ON ERROR RESUME NEXT
    ' ____________________________________________________
    '/CREATE DEFAULT STYLESHEET File$=tail.css or head.css
    '/Get General Values: PX, CAST
    fileF$ = Control$  + "!setup.txt"
    REDIM sxx$(20): PX = 16: Cast = 78
    x1 = getarray("width PX",sxx$(),fileF$)
    IF ISTRUE x1 THEN
        IF LEN(TRIM$(sxx$(2)))>0 THEN Cast = VAL(sxx$(2))
        IF Cast<40 THEN Cast=40 ELSE IF Cast>120 THEN Cast=120
        IF LEN(TRIM$(sxx$(3)))>0 THEN
            PX = VAL(sxx$(3))
            IF px < 6 THEN px = 6 ELSE IF px> 20 THEN px=20
        END IF
    END IF
    ' ____________________________________________________________________________________
    '/width relative formulas to Courier letters px = (cast*8), em window width = INT((6.4/100)*cast*8)
        COL_Width = INT((6.4/100)*cast*px)    '| COL_Width em
    '\___________________________________________________________________________________/
    OPEN Control$ + file$ FOR OUTPUT AS ff
        PRINT#ff,"/* 'TEXT WEBBER' " + File$ + " Style Sheet */"
        PRINT#ff,"/* <link rel='stylesheet' TYPE='text/css' media='screen' href='"+File$+"'> */"
        PRINT#ff,"/* */"
        IF file$="head.css" THEN
            PRINT#ff,"#headbody  {"
            PRINT#ff,"    font-family:  'Courier New', Courier, monospace;"
            PRINT#ff,"    font-variant: small-caps;"    '| small-caps, normal
            PRINT#ff,"    font-style:  normal;"    '| normal or italic or oblique
            PRINT#ff,"    font-size:  small;"    '| xx-small,x-small,small,medium,large,x-large,xx-large
            PRINT#ff,"    font-weight: bold;"    '| font-weight bold or normal
            PRINT#ff,"    text-align: left;"    '| right, left, center
            REDIM sxx$(20): Foreground$ = "BLACK;"
            x1 = getarray("hForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN Foreground$ = sxx$(2) + ";"
            PRINT#ff,"    color: " + Foreground$
            REDIM sxx$(20): Background$ = "GRAY;"
            x1 = getarray("hBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            PRINT#ff,"    background: " + Background$
            PRINT#ff,"      }"
            PRINT#ff,"#mainbody  {"
            PRINT#ff,"    font-family:  'Courier New', Courier, monospace;"
            PRINT#ff,"    font-variant: small-caps;"    '| small-caps, normal
            PRINT#ff,"    font-style:  normal;"    '| normal or italic or oblique
            PRINT#ff,"    font-size:  small;"    '| xx-small,x-small,small,medium,large,x-large,xx-large
            PRINT#ff,"    font-weight: bold;"    '| font-weight bold or normal
            PRINT#ff,"    text-align: left;"    '| right, left, center
            REDIM sxx$(20): Foreground$ = "BLACK;"
            x1 = getarray("mForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN Foreground$ = sxx$(2) + ";"
            PRINT#ff,"    color: " + Foreground$
            REDIM sxx$(20): Background$ = "GRAY;"
            x1 = getarray("mBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            PRINT#ff,"    background: " + Background$
            PRINT#ff,"      }"
            PRINT#ff,"#maxarea {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 800px;"
            PRINT#ff,"      width: auto;"
            PRINT#ff,"      height: 300%;"
            PRINT#ff,"      background-color: yellow;"
            PRINT#ff,"         }"
            PRINT#ff,".head1 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 26%; "
            PRINT#ff,"      width: 70%; "
            PRINT#ff,"      height: 20%;"
            PRINT#ff,"      }"
            PRINT#ff,".head3 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 88%; "
            PRINT#ff,"      width: 10%; "
            PRINT#ff,"      height: 95%;"
            PRINT#ff,"      }"
            PRINT#ff,".main1 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 2%;"
            PRINT#ff,"      width: 22%;"
            PRINT#ff,"      height: 95%;"
            PRINT#ff,"      }"
            PRINT#ff,".main2 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 2%;"
            PRINT#ff,"      width: 22%;"
            PRINT#ff,"      height: 95%;"
            PRINT#ff,"      }"
            PRINT#ff,".main3 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 2%;"
            PRINT#ff,"      width: 22%;"
            PRINT#ff,"      height: 95%;"
            PRINT#ff,"      }"
            PRINT#ff,".tail1 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 26%;"
            PRINT#ff,"      left: 26%;"
            PRINT#ff,"      width: 70%;"
            PRINT#ff,"      height: 71%;"
            PRINT#ff,"      }"
            PRINT#ff,".tail2 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 26%;"
            PRINT#ff,"      width: 70%;"
            PRINT#ff,"      height: 95%;"
            PRINT#ff,"      }"
            PRINT#ff,".tail3 {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 2%;"
            PRINT#ff,"      left: 26%;"
            PRINT#ff,"      width: 60%;"
            PRINT#ff,"      height: 95%;"
            PRINT#ff,"      }"
        END IF
        IF file$="tail.css" THEN
            PRINT#ff,"#tailbody  {"
            PRINT#ff,"      font-family:  'Courier New', Courier, monospace;"
            PRINT#ff,"      font-style:  normal;"    '| normal or italic or oblique
            PRINT#ff,"      font-weight: bold;"    '| bold or normal
            PRINT#ff,"      text-align: left;"    '| right, left, center
            PRINT#ff,"      font-variant: normal;"    '| small-caps, normal
            PRINT#ff,"      top: 1px;"
            PRINT#ff,"      left: 1px;"
            PRINT#ff,"      font-size:  .8em;"
            PRINT#ff,"      padding-right: 1px;"
            PRINT#ff,"      background-color: transparent;"
            'from cast/courier formula
            PRINT#ff,"      width: " + STR$((col_width+1)/2) + "em;"
            REDIM sxx$(20): Foreground$ = "BLACK;"
            x1 = getarray("tForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN Foreground$ = sxx$(2) + ";"
            PRINT#ff,"      color: " + Foreground$
            REDIM sxx$(20): Background$ = "GRAY;"
            x1 = getarray("tBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            PRINT#ff,"      background: " + Background$
            PRINT#ff,"      }"
            PRINT#ff,"/* Used in Index.html page */"
            PRINT#ff,"#indexbody  {"
            PRINT#ff,"      font-family:  'Courier New', Courier, monospace;"
            PRINT#ff,"      font-style:  normal;"    '| normal or italic or oblique
            PRINT#ff,"      font-size:  2em;"
            PRINT#ff,"      font-weight: bold;"    '| bold or normal
            PRINT#ff,"      text-align: center;"    '| right, left, center
            PRINT#ff,"      font-variant: small-caps;"    '| small-caps, normal
            REDIM sxx$(20)
            REDIM sxx$(20): Foreground$ = "BLACK;"
            x1 = getarray("iForeground",sxx$(),fileF$)
            IF ISTRUE x1 THEN Foreground$ = sxx$(2) + ";"
            PRINT#ff,"      color: " + Foreground$
            REDIM sxx$(20): Background$ = "GRAY;"
            x1 = getarray("iBODY",sxx$(),fileF$)
            IF ISTRUE x1 THEN Background$ = UCASE$(sxx$(2)) + " url(" + LCASE$(sxx$(3)) + ") "  + LCASE$(sxx$(4)) + " " + LCASE$(sxx$(5)) + " " + LCASE$(sxx$(6)) + ";"
            PRINT#ff,"      background: " + Background$
            PRINT#ff,"      }"
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
            PRINT#ff,".ptail {"
            PRINT#ff,"      position: absolute;"
            PRINT#ff,"      top: 20%;"
            PRINT#ff,"      left: 30%;"
            PRINT#ff,"      float: left;"
            PRINT#ff,"      }"
            PRINT#ff,".indexline {"
            PRINT#ff,"      padding-left: 5%;"
            PRINT#ff,"      border-left:  solid;"
            PRINT#ff,"      border-right: none;"
            PRINT#ff,"      border-top:   none;"
            PRINT#ff,"      border-bottom: none;"
            PRINT#ff,"      border-left-width: thick;"
            PRINT#ff,"      border-color: red;"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      font-size:    .8em;"
            PRINT#ff,"          }"
            PRINT#ff,".figureleft {"
            PRINT#ff,"      border: thick silver solid;"
            PRINT#ff,"      border-style: solid;"
            PRINT#ff,"      border-width: 1em;"
            PRINT#ff,"                  }"
            PRINT#ff,".tailscaled {"
            PRINT#ff,"      width:  90%;"
            PRINT#ff,"      height: 70%;"
            PRINT#ff,"      margin: 5px;"
            PRINT#ff,"      float: left;"
            PRINT#ff,"      border-width: 2em;"
            PRINT#ff,"      border-style: inset;"
            PRINT#ff,"              }"
        END IF
            PRINT#ff,".buttonscaled {"
            PRINT#ff,"      width: auto;"
            PRINT#ff,"      height: 2em;"
            PRINT#ff,"            }"
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
            PRINT#ff,"h1,h2,h3,h4 {"
            PRINT#ff,"      text-align: left;"
            PRINT#ff,"      font-size:    1em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"            }"
            PRINT#ff,"h5 {"
            PRINT#ff,"      text-align: left;"
            PRINT#ff,"      margin-left:  0;"
            PRINT#ff,"      margin-bottom:  0;"
            PRINT#ff,"      font-size:    1em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"      color: black;"
            PRINT#ff,"      background: white;"
            PRINT#ff,"      }"
            PRINT#ff,"h6 {"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"      margin-left:  0;"
            PRINT#ff,"      margin-bottom:  0;"
            PRINT#ff,"      font-size:    1em;"
            PRINT#ff,"      font-weight:  bold;"
            PRINT#ff,"      color: black;"
            PRINT#ff,"      background: white;"
            PRINT#ff,"      }"
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
            PRINT#ff,"pre   {"
            PRINT#ff,"      text-align:  left;"
            PRINT#ff,"      text-indent:  0;"
            PRINT#ff,"      margin-left:  0;"
            PRINT#ff,"      margin-bottom: 0;"
            PRINT#ff,"      }"
            PRINT#ff,"strong    {"
            PRINT#ff,"          font-size:    1.2em;"
            PRINT#ff,"          font-weight:  bold;"
            PRINT#ff,"          }"
            PRINT#ff,"em    {"
            PRINT#ff,"      font-style:     oblique;"    '| normal or italic or oblique
            PRINT#ff,"      font-size:      1em;"
            PRINT#ff,"      font-weight:    bold;"    '| bold or normal
            PRINT#ff,"      text-decoration:underline;"    '| sets text to normal size and weight
            PRINT#ff,"      }"
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
            PRINT#ff,"          font-variant: small-caps;"    '| small-caps, normal
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
            PRINT#ff,"div.img       {"
            PRINT#ff,"      margin: 2px;"
            PRINT#ff,"      border: 1px solid #0000ff;"
            PRINT#ff,"      height: auto;"
            PRINT#ff,"      width: auto;"
            PRINT#ff,"      float: left;"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"              }"
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
            PRINT#ff,"div.desc      {"
            PRINT#ff,"      font-size:   .8em;"
            PRINT#ff,"      text-align: center;"
            PRINT#ff,"      font-weight: normal;"
            PRINT#ff,"      background: " + Background$ + ";"
            PRINT#ff,"      width: auto;"
            PRINT#ff,"      height: auto;"
            PRINT#ff,"      margin: 1px;"
            PRINT#ff,"              }"
            PRINT#ff,"div.text      {"
            PRINT#ff,"      font-family:  'Courier New', Courier, monospace;"
            PRINT#ff,"      font-style:  normal;"    '| normal or italic or oblique
            PRINT#ff,"      font-weight: bold;"    '| bold or normal
            PRINT#ff,"      text-align: left;"    '| right, left, center
            PRINT#ff,"      font-variant: normal;"    '| small-caps, normal
            PRINT#ff,"      top: 1px;"
            PRINT#ff,"      left: 1px;"
            PRINT#ff,"      font-size:  .8em;"
            'from cast/courier formula
            PRINT#ff,"      width: " + STR$((col_width+1)/2) + "em;"
            PRINT#ff,"      padding-right: 1px;"
            PRINT#ff,"              }"
    CLOSE ff
    '--------
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
        FILECOPY "Trial4Webber.zip",DownLoad$ + "Trial4Webber.zip"
    CATCH
    END TRY
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
        PRINT#ff,SPACE$(4)
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
    '/Web Site 3 possible Spider Descriptions and Frames!
    fileF$ = Control$  + "!setup.txt"
    REDIM sxx$(20): Title$ = "TEXT WEBBER"
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
                PRINT#ff,"<!DOCTYPE HTML>"
                PRINT#ff,"<html lang=en-CA>"
                x$ = WebberText$ + "DOCTYPE_heading.tmp"
                '-----------------------
                OPEN x$ FOR INPUT AS gg
                    Image
                    '----------------
                    DO UNTIL EOF(gg)
                        LINE INPUT#gg,y$
                        '----------------
                        IF INSTR(y$,"</head")>0 THEN
                            PRINT#ff,"    <link rel='stylesheet' TYPE='text/css' media='screen' href='head.css'>"
                            PRINT#ff,y$
                            EXIT DO
                        ELSEIF INSTR(y$,"<link rel")=0 THEN
                            PRINT#ff,y$
                        END IF
                    LOOP
                CLOSE gg
                '--------
                PRINT#ff,"    <body>"
                IF  framing<>2 THEN
                    PRINT#ff,"        <iframe seamless id='head' class='head"+TRIM$(STR$(framing))+"'  src='head.html'></iframe>"
                END IF
                PRINT#ff,"        <iframe seamless id='tail' class='tail"+TRIM$(STR$(framing))+"'  src='tail.html'></iframe>"
                PRINT#ff,"        <iframe seamless id='main' class='main"+TRIM$(STR$(framing))+"'  src='main.html'></iframe>"
                PRINT#ff,"    </body>"
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
    '---------------------------------------------------
    OPEN (WebberText$ + file$ + ".txt") FOR INPUT AS f1
        Image
        '----------------
        DO UNTIL EOF(f1)
                LINE INPUT#f1,x$
                '----------------
                x$ = TRIM$(x$)
                IF LEN(x$)>0 THEN
                    IF LCASE$(LEFT$(x$,6))<>"spider" THEN
                        IF UCASE$(LEFT$(x$,15))<>"HTML_FOLDERS:-9" THEN
                            Topic$ ="6 IN "+ File$+" Serious Error!"
                            Problems(1) = "'HTML_FOLDERS:-9' required after Spider lines!"
                            Problems(2) = "OPEN '"+WebberText$ + file$ + ".txt'"
                            Problems(3) = "ADD 'HTML_FOLDERS:-9' after Spider lines!"
                            problem_file(Topic$,3)
                        ELSE
                            EXIT DO
                        END IF
                    END IF
                END IF
        LOOP UNTIL LEN(x$)>0
    CLOSE f1
    '--------
CATCH
    CLOSE f1
    '--------
    TRY
        '-----------------------------------------------------
        OPEN (HTMLfolders$ + file$ + ".html") FOR INPUT AS f1
        CLOSE f1
        '--------
    CATCH
        CLOSE f1
        '--------
        hFrame_Head = -1
        '----------------------------------------------------
        OPEN (WebberText$ + file$ + ".txt") FOR OUTPUT AS f1
            PRINT#f1,"HTML_FOLDERS:-9"
            PRINT#f1,"{tail.html} ~secondimage.jpg~ HOME}:-4:-3"
            PRINT#f1,"{http://validator.w3.org}Validator HTML}:-4"
            PRINT#f1,"{http://jigsaw.w3.org/css-validator/}Validator CSS}:-4"
        CLOSE f1
        '--------
    END TRY
END TRY
CLOSE f1
'--------
1700:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
SUB hFrame_Main ALIAS "z1800" (file$)       '| | <br>
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
    OPEN FileGoody$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileStyle1$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileStyle2$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileWork$ FOR OUTPUT AS 4: CLOSE 4
    OPEN FileStore$ FOR OUTPUT AS 4: CLOSE 4
    OPEN Trash$ + "maxarea.tmp" FOR OUTPUT AS 4
        PRINT#4,"position: absolute;"
        PRINT#4,"top: 800px;"
        PRINT#4,"left: 1px;"
        PRINT#4,"width: 100%;"
        PRINT#4,"height: 100%;"
        PRINT#4,"padding-right: 5px;"
        PRINT#4,"background-color: green;"
        PRINT#4,"}"
    CLOSE 4
    '-------
    File$ = FileStyle1$
    f1 = FREEFILE
    f2 = FREEFILE
    '---------------------------
    OPEN file$ FOR OUTPUT AS f1
        PRINT#f1,"<!DOCTYPE HTML>"
        PRINT#f1,"<html lang=en-ca>"
        PRINT#f1,"  <head>"
        PRINT#f1,"       <meta charset='utf-8' />"
        PRINT#f1,"      <title>" + Day$ + " " + expire$ + "</title>"
        PRINT#f1,"      <style type='text/css' media='screen' >"
        '-----------------------------------------------
        OPEN (WebberText$ + "head.css") FOR INPUT AS f2
            Image
            '----------------
            DO UNTIL EOF(f2)
                LINE INPUT#f2,x$
                '----------------
                x$ = TRIM$(x$)
                IF LEN(x$)> 0 AND LEFT$(x$,2) <> "/*" THEN
                    PRINT#f1,SPACE$(8) + x$
                END IF
                IF LEFT$(x$,8) = "#maxarea" THEN
                    '-------------------------------------------
                    OPEN Trash$ + "maxarea.tmp" FOR OUTPUT AS 4
                        Image
                        '----------------
                        DO UNTIL EOF(f2)
                            LINE INPUT#f2,x$
                            '----------------
                            x$ = TRIM$(x$)
                            PRINT#f1,SPACE$(8) + x$
                            PRINT#4,x$
                            IF RIGHT$(x$,1)="}" THEN
                                PRINT#f1,"/* here */"
                                CLOSE f1
                                '--------
                                '---------------------------------
                                OPEN FileStyle2$ FOR OUTPUT AS f1
                                EXIT DO
                            END IF
                        LOOP
                    CLOSE 4
                    '-------
                END IF
            LOOP
        CLOSE f2
        '--------
        CLOSE f1
        '--------
        File$ = FileWork$
        '---------------------------
        OPEN File$ FOR OUTPUT AS f1
        PRINT#f1,"  </head>"
        PRINT#f1,SPACE$(4)
        PRINT#f1,"  <body ID='mainbody'>"
        PRINT#f1,"  <div>"
        ' _________________________________
        '/1. Site Title and Date updated
        s$ = DATE$
        mm$ = MID$(s$,1,2)
        Dy$ = MID$(s$,4,2)
        yy$ = MID$(s$,7)
        yy$ = yy$ + "/" + mm$ + "/" + Dy$
        xx$ ="Text Webber"
        REDIM sx$(20)
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
        mlk$ = "<div id='" + mainLink$ + "'><h6>" + bLink$ + "</h6></div>"
        PRINT#f1,mlk$
        ' _____________________________________________________________________
        '/create right frame headings and basic links from directions in fileF$
        REDIM sx$(20)
        x1 = getarray("Describe",sx$(),fileF$)    '| LEFT LINKS HEADING
        x$ = SPACE$(20)
        IF ISTRUE x1 THEN
            IF LEN(TRIM$(sx$(2))) > 0 THEN
                x$ = "<div><h1>" + TRIM$(sx$(2)) + "</h1></div>"
                PRINT#f1,x$
            END IF
            IF LEN(TRIM$(sx$(3))) > 0 THEN
                x$ = "<div><h2>" + sx$(3) + "</h2></div>"
                PRINT#f1,x$
            END IF
        END IF
        REDIM sx$(20)
        x1 = getarray("EMail",sx$(),fileF$)    '| CONTACT EMAIL
        IF ISTRUE x1 THEN
            sx$(3) = single_quote_remove(sx$(3))
            y$ = TRIM$(sx$(3))
            IF INSTR(y$,"@")>0 THEN
                x$ = "<div class='mainbutton'><a href='mailto:" + LCASE$(y$) + "' target='tail'>" + TRIM$(sx$(2)) + "</a></div><br>"
                PRINT#f1,x$
            END IF
        END IF
        REDIM sx$(20)
        x1 = getarray("Form",sx$(),fileF$)    '| FREE WEBSITE INFORMATION
        IF ISTRUE x1 THEN
            sx$(3) = single_quote_remove(sx$(3))
            y$ = TRIM$(sx$(3))
            IF LEN(y$)> 0 THEN
                x$ = "<div class='mainbutton'><a href='" + LCASE$(y$) + "' target='tail'>" + TRIM$(sx$(2)) + "</a></div><br>"
                PRINT#f1,x$ 'target='_blank'
            END IF
        END IF
        '\__________________________________________________________________________________/
        ' ___________________________________________________
        '/Output links to files in HTML from TEXT to Page
        a$ = "<div class='subbutton'><a href='"
        b$ = "' target='tail'>"
        c$ = "</a></div>"
        were$ = HTMLfromTEXT$: nType$ = ".html"
        ' ____________________________________________________
        '/Extract .html Files listed in Folder HTML_FROM_TEXT
        f2 = FREEFILE
        File$ = trash$ + "FileList.tmp"
        '-------------------------------------
        OPEN File$ FOR OUTPUT AS f2: CLOSE f2
        y = fetch_file(were$,File$,nType$)
        CALL Sort_File(File$,last)
        IF ERN > 0 THEN ERROR ERN
        IF Last > 0 THEN
            ' _____________________________________________________
            '/Title of Files created From TCM outputted to Page
            REDIM sx$(20)
            xx$ = ""
            x1 = getarray("Describe",sx$(),fileF$)
            IF ISTRUE x1 THEN
                y$ = TRIM$(sx$(4))
                IF LEN(y$)> 0 THEN
                    xx$ = "<h3>" + sx$(4) + "</h3>"
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
                LOOP
            CLOSE f2
            '--------
        END IF
        ' ______________________________________
        '/Output to Main Page Folder List Title
        REDIM sx$(20)
        x1 = getarray("Describe",sx$(),fileF$)
        IF ISTRUE x1 THEN
            y$ = TRIM$(sx$(5))
            IF LEN(y$) > 0 THEN
                x$ = "<div><h4>" + sx$(5)+ "</h4></div>"
                PRINT#f1,x$
            END IF
        END IF
        ' _____________________________________________________________________________
        '/Output to main page links to Folders in !HTML FOLDERS
        ' __________________________________________________________
        '/Send to "directory.txt" the Folders names in 'HTML_FOLDERS
        CALL get_dir(HTMLfolders$)    '| Trash$  + "directory.txt"
        IF ERN > 0 THEN ERROR ERN
        '----------------------------------------------
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
                IF LEFT$(x$,1) <> "@" AND LEFT$(x$,14) <> "HTML_FROM_TEXT" THEN
                    INCR Link
                    LastLink$ = "AAAAA" + LTRIM$(STR$(Link))
                    LastLink$ = RIGHT$(LastLink$,4) + x$
                    nextLink = Link + 1
                    nextLink$ = "AAAAA" + LTRIM$(STR$(nextLink))
                    nextLink$ = RIGHT$(nextLink$,4) + x$
                    ' _________________________________
                    '/Folder Button Created
                    bLink$ = "<cite>" + x$ + "</cite>"
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
        '--------
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
        PRINT#f1,"      </div>"
        PRINT#f1,"  </body>"
        PRINT#f1,SPACE$(4)
        PRINT#f1,"</html>"
    CLOSE f1
    '--------
    '-------------------------------
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
        '--------
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
        '--------
        PRINT#f1,SPACE$(6) + "</style>"
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
        '--------
    CLOSE f1
    '--------
    SLEEP 250
EXIT SUB
main_folder_pages:
    a$ = "<div class='subbutton'><a href='"
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
        PRINT#f2,SPACE$(8) + "position: absolute;"
        x$ = "top: " + x$ + "px;"
        PRINT#f2,SPACE$(8) + x$
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
        'mlk$ = "<div class='mainbutton'><a href='main.html#" + mainLink$ + "' target='main'>" + UCASE$("return home") + "</a></div><br><br>"
        mlk$ = "<div class='mainbutton'><a id='" + nextLink$+ "' href='main.html#" + mainLink$ + "' target='main'>" + UCASE$("return_home") + "</a></div><br>"
        PRINT#f4,mlk$
        'mlk$ = "<div class='mainbutton'><a id='" + nextLink$ + "'  href='main.html#" + LastLink$ + "' target='main'>" + bLink$ + "</a></div><br><br>"
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
RETURN                        '| | <br>
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
    '---------------------------------------------------
    OPEN (WebberText$ + file$ + ".txt") FOR INPUT AS f1
        Image
        '----------------
        DO UNTIL EOF(f1)
                LINE INPUT#f1,x$
                '----------------
                x$ = TRIM$(x$)
                IF LEN(x$)>0 THEN
                    IF LCASE$(LEFT$(x$,6))<>"spider" THEN
                        IF UCASE$(LEFT$(x$,15))<>"HTML_FOLDERS:-9" THEN
                            Topic$ ="7 IN "+ File$+" Serious Error!"
                            Problems(1) = "'HTML_FOLDERS:-9' required after Spider lines!"
                            Problems(2) = "OPEN '"+WebberText$ + file$ + ".txt'"
                            Problems(3) = "ADD 'HTML_FOLDERS:-9' after Spider lines!"
                            problem_file(Topic$,3)
                        ELSE
                            EXIT DO
                        END IF
                    END IF
                END IF
        LOOP UNTIL LEN(x$)>0
    CLOSE f1
    '--------
CATCH
    CLOSE f1
    '--------
    TRY
        '-----------------------------------------------------
        OPEN (HTMLfolders$ + file$ + ".html") FOR INPUT AS f1
        CLOSE f1
        '--------
    CATCH
        CLOSE f1
        '--------
        hFrame_Tail = -1
        '----------------------------------------------------
        OPEN (WebberText$ + file$ + ".txt") FOR OUTPUT AS f1
            PRINT#f1,"HTML_FOLDERS:-9"
            PRINT#f1,"Navigational Design:-1"
            PRINT#f1,"In the Left Main Frame Clicking a Link will Display the Linked File in the Right Tail Frame."
            PRINT#f1,"If the Top Heading Frame remains unchanged Clicking the Home Link will display this Page."
            PRINT#f1,"If you click the last link in this document you can travel up the link chain by Clicking each new link you travel to."
            PRINT#f1,"Introduction:-1"
            PRINT#f1,"This Operating Program for a Web Site was created by a program called 'TEXT WEBBER' written in 2005 by Hugh Polley"
            PRINT#f1,"Revised: (&copy;2005/01/14,&copy;2007,&copy;2009,&copy;2010,&copy;2014):-1"
            PRINT#f1,"If you wish to purchase this program it will cost to the next dollar 20 times the cost of a first class Canadian Stamp!"
            PRINT#f1,"The Author's E_mail is [hap2u@hotmail.com], TEXTWEBBER must be in subject line!:+1"
            PRINT#f1,"How Index Links Work:-1"
            PRINT#f1,""
            PRINT#f1,"The [Index Links] will Navigate to their [Subject Link]!:+1"
            PRINT#f1,"If you click the last link in this document you can travel up the"
            PRINT#f1,"[link chain] by Clicking each new link you travel to.:+1"
        CLOSE f1
        '--------
    END TRY
FINALLY
    CLOSE f1
    '--------
END TRY
1900:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION instructions ALIAS "z2000" (file$) AS LONG
ON ERROR RESUME NEXT
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
        PRINT#f1,":+9:+9"
        PRINT#f1,"INTRODUCTION - Creating HTML from Text Files!:-1"
        PRINT#f1,"    The program 'TEXT WEBBER', hereafter referred to as 'The Program', creates a HTML Framed Web Site from its root folder and sub folders."
        PRINT#f1,"The best place for the root folder is your WINDOW'S DESKTOP, a folder name such as MY WEB SITE will work well."
        PRINT#f1,"Text files (.TXT) and Data files (.TMP) in the sub-folder @WEBBER_TEXT are used by the program to generate your Web Site's HTML documents."
        PRINT#f1,"By changing these text and data files you can have the program change an existing HTML page's location and content, or create a new page."
        PRINT#f1,"Before using The Program open a 'WINDOWS EXPLORER' window, under 'TOOLS' click 'FOLDER OPTIONS', click 'VIEW' tab,"
        PRINT#f1,"uncheck 'hide extensions for known file types'."
        PRINT#f1,"    The Web Site created in the sub-folder HTML_FOLDERS can be copied to an Internet Server to create or update your internet Web Site."
        PRINT#f1,"    [You can now within 5 minutes update the wording of an HTML document and up-load it to your Web Site!]:+1"
        PRINT#f1,"    This program is the operating system for your Web Site;"
        PRINT#f1,"it both produces HTML Pages, and organizes HTML pages to fit within a two or three Frameset Format."
        PRINT#f1,"    The Text Files in the Folder @CONTROL affect the default appearance of your Web Site."
        PRINT#f1,"    The Text Files in Folder @WEBBER_TEXT edit the control files, and/or create your Web Site's content."
        PRINT#f1,"    You can place in the Folder @HTML_HOLD_FILES material referenced by your Text Code Marks."
        PRINT#f1,"    When you run the program image files placed in your Root Folder will be converted to JPG format and moved to @HTML_HOLD_FILES."
        PRINT#f1,"    Run Convert.exe and JPEG image files in your Root Folder will be changed to JPG files."
        PRINT#f1,SPACE$(0)
        PRINT#f1,"[The Text Code Marks, hereafter referred to as TCM, allow you to program your text file's key elements to produce your HTML documents.]:+1"
        PRINT#f1, SPACE$(0)
        PRINT#f1,"    You can place HTML code in your text files as long as the sections enclosed by less than and greater than marks '<"
        PRINT#f1,">' are on the same line.  Otherwise they will be replaced by ENTITY HTML code and not work properly."
        PRINT#f1,"Separate onto different lines these marks when you are using them in a different way so they are not treated as HTML code."
        PRINT#f1,"Double quote symbols are turned into single quote symbols and Entity marks are created for;"
        PRINT#f1,"leading spaces, less and more than symbols, and single quote symbols."
        PRINT#f1,"These Entity changes do not occur for symbols inside less than and more than symbols '<:+8"
        PRINT#f1,">' on the same line as such lines are treated as HTML code.:-8"
        PRINT#f1,"    2. STEP TWO!: Run the program, it will create your default Web Site.:-2"
        PRINT#f1,"Decide which of the three framing possibilities recorded in the @WEBBER_TEXT folder you desire."
        PRINT#f1,"After step 3 click that HTML file and save it as Frame.html."
        PRINT#f1,"The three frame default is Frame3.html, the Frame2.html has a two frame structure,"
        PRINT#f1,"Frame1.html has a three column Frame structure."
        PRINT#f1,"    3. STEP THREE!: After the Web Site pops up, find in the left main frame:-2"
        PRINT#f1,"the Link 'Free Web Site Hosting'. Visit this link and acquire a free internet web site host."
        PRINT#f1,"    4. STEP FOUR!: Mirror on the internet site the folder HTML_FOLDERS and its sub-folders:-2"
        PRINT#f1,"which DO NOT start with the '@' symbol. The internet default structure is HTML_FOLDERS, with the Sub Folder 'HTML_FROM_TEXT."
        PRINT#f1,"    5. STEP FIVE!: Upload to the Internet Root directory 'tail.css',:-2"
        PRINT#f1,"'index.html', and '" + firstimage$ + "' and any required background images."
        PRINT#f1,"    If in one of your frames you are not going to use a background image, you need to remove the URL call"
        PRINT#f1,"from your @WEBBER_TEXT\tail.css file."
        PRINT#f1,"For example to remove the head frame image call delete 'url(headground.jpg)' from the #headbody element."
        PRINT#f1,"    6. STEP SIX!: Upload to the Internet Operating Folder HTML_FOLDERS;:-2"
        PRINT#f1,"style sheet 'tail.css'; the HTML structure file 'frame.html',"
        PRINT#f1,"'head.html', 'main.html', 'tail.html'; and the logo graphic 'secondimage.jpg'."
        PRINT#f1,"You can also upload Frame background files 'headground.jpg', 'mainground.jpg' and 'tailground.jpg'."
        PRINT#f1,"    7. STEP SEVEN!: Upload to the Internet sub-directory:-2"
        PRINT#f1,"HTML_FOLDERS/HTML_FROM_TEXT; the style sheet 'tail.css'; and for testing purposes this"
        PRINT#f1,"HTML File 'html_from_text.html'; and the Background file 'tailground.jpg', and logo file 'secondimage.jpg'."
        PRINT#f1,"    1. STEP ONE!: Turn on the EXE Program by renaming the application with your name framing the version number.:-2"
        PRINT#f1,"for example rename 'TEXT WEBBER vs4.exe' as 'TRIAL4WEBBER.exe'."
        PRINT#f1,"    8. STEP EIGHT: Type into your Browser address Bar the Internet website:-2"
        PRINT#f1,"address with index.html after the slash AND execute. If the Browser reports missing files,"
        PRINT#f1,"or web pages, locate them on your computer and upload."
        PRINT#f1,"A PERIOD INDEX ORGANIZER: see TEXT CODE MARKS (6):-1"
        PRINT#f1,"If you check '@CONTROL/html_from_text' you will find in this unchanged file STEP ONE is placed after STEP SEVEN,"
        PRINT#f1,"yet in '@WEBBER_TEXT/html_from_text' it is listed first.  The program will organize the text files"
        PRINT#f1,"by any period (.) designators it finds.  The format is limited to nine characters including the period."
        PRINT#f1,"[Form: AB34.A234]:+1"
        PRINT#f1,"CSS CONTROLS THE APPEARANCE OF YOUR WEB SITE!:-1"
        PRINT#f1,"The technical features of Cascading Style Sheets, CSS, can be found at {http://www.w3schools.com/css/}CSS Tutorials}.:+4"
        PRINT#f1,"    The Layout of your Web Site is controlled by the CSS files 'tail.css' and 'head.css'."
        PRINT#f1,"Delete tail.css in @WEBBER_TEXT then run the program and it is replaced by the one in @CONTROL."
        PRINT#f1,"Delete the one in @CONTROL then run the program and it is replaced by the Default CSS File from the settings in @CONTROL\SetUp.TXT."
        PRINT#f1,"    You can change the Setup.txt default Settings for CSS!"
        PRINT#f1,"After changing these settings, you must delete the CSS files in @CONTROL, and @WEBBER_TEXT for CSS changes to occur!"
        PRINT#f1,"Test Out Changing Your CSS!:-2"
        PRINT#f1,"Edit @WEBBER_TEXT\tail.css with Notepad and in the 'body' attribute remove 'font-variant: small-caps;'."
        PRINT#f1,"Also change 'color: RGB(0,0,0)' to 'color: BLUE;' save the file and run the program!"
        PRINT#f1,"    Blue is not my favorite for text so lets get back the original CSS by deleting the 'tail.css' file in @WEBBER_TEXT."
        PRINT#f1,"Run the program and then delete the 'font-variant: small-caps;' again and change 'background: RGB(200,200,200)'"
        PRINT#f1,"to 'background: RGB(255,255,255)' and delete 'url(tailground.jpg)'."
        PRINT#f1,"Now save @WEBBER_TEXT\tail.css, and copy it to @Control and you have a new default file with regular type and a white background."
        PRINT#f1,"    This feature of the program allows you to play around with the CSS files in @WEBBER_TEXT deleting the ones you"
        PRINT#f1,"are unhappy with, or saving the changes you like to the default @CONTROL folder."
        PRINT#f1,"To get back the original settings delete tail.css from both the @CONTROL and @WEBBER_TEXT folders."
        PRINT#f1,"THE @CONTROL FOLDER:-1"
        PRINT#f1,"This Folder contains the Information Files listed below which can be changed or"
        PRINT#f1,"adjusted after running 'TEXT WEBBER' for the first time."
        PRINT#f1,"SPIDER BOTS GATHER INFO ON THE WEB SITE:-1"
        PRINT#f1,"The HTML head tag meta statements contain information used by services such as Google to gather information on your Site."
        PRINT#f1,"In 'DOCTYPE_heading.tmp' you will find some of this information and can change it there."
        PRINT#f1,"    In the @WEBBER_TEXT (.txt) files you will find three Spider lines at the top of the file."
        PRINT#f1,"Do not bother to remove them as they will just be created again."
        PRINT#f1,"They are useful as you can change the spiders key words here to better match your documents subject matter."
        PRINT#f1,"A Spider may then find this and Googling the key word will bring up a reference to your document."
        PRINT#f1,"The License Agreement:-2"
        PRINT#f1,"The first time you run the program the Tail Frame is taken over by the License Agreement and the 'tail.txt' file will not be displayed!"
        PRINT#f1,"    You will not find the file license.txt in @WEBBER_TEXT because it is created, used, then deleted; however delete"
        PRINT#f1,"it from @CONTROL and it will be created and removed again."
        PRINT#f1,"THE '!setup.txt' FILE:-2"
        PRINT#f1,"This File contains the Link Headings and the personal contact data required by"
        PRINT#f1,"your Web Site.  Edit this file to create your own Default Headings, E-Mail address and"
        PRINT#f1,"color layouts. Here you can set the website layout and other important features."
        PRINT#f1,"For CSS changes to this file to take affect, delete all the CSS files in Folders @CONTROL and @WEBBER_TEXT."
        PRINT#f1,"THE 'head.txt' FILE:-2"
        PRINT#f1,"Controls your Heading frame output, this is a good spot for Favorite Links!"
        PRINT#f1,"In @WEBBER_TEXT you can change it's content or delete it to get back the Default File."
        PRINT#f1,"The default file contains links to the offical Markup Validation Services."
        PRINT#f1,"Click on the HTML validator and use it to validate your 'index.html' page!"
        PRINT#f1,"The CSS Cascading Style Sheets:-2"
        PRINT#f1,"CSS tail.css and head.css control the appearance of your HTML and HTM pages."
        PRINT#f1,"Head controls the 'head' Frame, Tail controls all other Frames."
        PRINT#f1,"    The CSS Style Sheets have a three level redundancy built in."
        PRINT#f1,"Edit 'tail.css' in the @WEBBER_TEXT Folder and run the Program, the appearance of your Web Site will change."
        PRINT#f1,"    If these changes result in the display of pages not looking right,"
        PRINT#f1,"activate the second redundancy by deleting the 'tail.css' file in the @WEBBER_TEXT Folder,"
        PRINT#f1,"then run the Program!"
        PRINT#f1,"The CSS files in @CONTROL will move to @WEBBER_TEXT."
        PRINT#f1,"    Remove the 'tail.css' file from @CONTROL"
        PRINT#f1,"and the 'tail.css' file will be created in the @CONTROL folder from your setup.txt file."
        PRINT#f1,"CREATING A WEB SITE WITHOUT TEXT CODE MARKS:-1"
        PRINT#f1,"    Start by deleting 'tail.txt' from the @WEBBER_TEXT folder."
        PRINT#f1,"    Place your Designer 'tail.html' document in the HTML_FOLDERS folder over writing the old tail file."
        PRINT#f1,"The same procedure can be used to create a designer 'head.html' Frame."
        PRINT#f1,"    Create descriptive 'Active' Sub Folders without '@' in their Name, place them in the HTML_FOLDERS folder."
        PRINT#f1,"Place your HTML Web Pages and any supporting files in the appropriate Sub Folder, run the program."
        PRINT#f1,"    Links to the Folders are now in the Left Frame Folder List. Clicking a FOLDER will list the HTML files in that folder."
        PRINT#f1,"    Some supporting HTML files may show up because they have the (.html) extension instead of the (.htm) extension."
        PRINT#f1,"Rename the Supporting HTML extensions to (.htm) and edit the remaining (.html) pages to call (.htm) instead of (.html)."
        PRINT#f1,"    In @CONTROL/!setup.txt remove the Headings listed after the keyword 'Describe' by changing the key word to -Describe-."
        PRINT#f1,"Your Web Site will now function only with those Folders containing your designer HTML."
        PRINT#f1,"PROGRAMING YOUR WEB SITE WITH TEXT CODE MARKS:-1"
        PRINT#f1,"    The Program translates text files (.txt) in the folder @WEBBER_TEXT into HTML files (.html)"
        PRINT#F1,"in accordance with the 'Text Code Marks'(TCM) found at the end of text lines in the text files."
        PRINT#f1,"The resultant HTML files are placed in the default folder HTML_FOLDERS\HTML_FROM_TEXT"
        PRINT#f1,"unless redirected to a different folder by a TCM -9."
        PRINT#f1,"    Links in the Left Frame are created from all [.html] File Names in each active Sub Folder of HTML_FOLDERS."
        PRINT#f1,"Supporting HTML file extensions should be designated (.htm) and placed in your @HTML_HOLD_FILES Folder"
        PRINT#f1,"or they will show up as a Link in the Left Frame."
        PRINT#f1,"    Because the left frame link is derived from the (.html) name, avoid using nondescript names."
        PRINT#f1,"    The default files 'license.txt', 'head.txt', 'tail.txt', and 'html_from_text.txt', are created in the folder @WEBBER_TEXT."
        PRINT#f1,"    Once you are done with the 'html_from_text.txt' file you can delete it, or redirect it to @work."
        PRINT#f1,"    Deleting 'tail.txt' or 'head.txt' from @WEBBER_TEXT will not work as they will be recreated from default files."
        PRINT#f1,"If you wish to use Designer Tail and Head HTML files then first redirect these files with TCM -9 to the @WORK folder."
        PRINT#f1,"TEXT CODE MARKS: (abbreviation = TCM):-1"
        PRINT#f1,"    These marks are your secret weapon for easily creating HTML Pages without knowing the huge HTML Tag library!"
        PRINT#f1,"When placed at the end of a line; a TCM tells The Program how to create a HTML Page!"
        PRINT#f1,"They are composed of a colon (:) and a negative or positive sign and a number (1 to 9)!"
        PRINT#f1,"This HTML Document was created by the File 'html_from_text.txt' in Folder @WEBBER_TEXT."
        PRINT#f1,"It is both an Instruction File and an Example File."
        PRINT#f1, SPACE$(4)
        PRINT#f1,"    1. Generally TCM Marks can be placed at the end of a text line in any order."
        PRINT#f1,"Index TCM -1 and -2 will cause errors when used with nested TCM 8,7,6 or the Line Returns TCM -5 and +5."
        PRINT#f1,"    2. TCM Marks (+5,-5) are not compatible with TCM Marks (+6,-2,-1)."
        PRINT#f1,"    3. TC Marks (+8,+7,+6) require closing TC Marks (-8,-7,-6) and must be nested"
        PRINT#f1,"when used together."
        PRINT#f1,"    4. The 'TAB' key, or a blank text line, containing 4 spaces will separate HTML lines with a 4 space spacer line;"
        PRINT#f1,"an empty line containing no spaces will start a new line without a spacer!"
        PRINT#f1,"It is better to control line spacing this way as TCM +-5 may be used for HTML Forms in a future version."
        PRINT#f1,"    5. A line Indented with spaces will indent a new paragraph with spaces grouped in fours, to a max of 24."
        PRINT#f1,"    6. A PERIOD INDEX ORGANIZER works with TCM -1+2-2 to put indexes out of order into order."
        PRINT#f1,"You will find the STEP list in the Introduction index listed in order even though the document is created with STEP 1 out of order."
        PRINT#f1,"If you have an Index line started with up to four numbers and/or letters followed by a period and a space"
        PRINT#f1,"the Program will attempt to organize that HTNL page by its index. [0001. Will be placed above a line starting with 0002. or AAAA.]"
        PRINT#f1,"    7. Link TCM -3,-4 and +4 can be used after a (:) with Index TCM -1,-2."
        PRINT#f1,"That way you can jump from a Webber page's index to the picture or link quickly."
        PRINT#f1,"[The Program will leave an empty line after such links for HTML formatting reasons.]:+1"
        PRINT#f1,"    eg. (Photo of Pam on Beach:~pam123.jpg~:-3:-1) [will take you to this photo from the Main Index.]:+1"
        PRINT#f1, SPACE$(4)
        PRINT#f1,"That's it for the general rules, only nine TCM numbers, seven rules and TCM 3,4 syntax"
        PRINT#f1,"to remember or look up!"
        PRINT#f1,"    TCM :-9 The Name of the HTML Folder to be created for the Text file:+6"
        PRINT#f1,"    TCM :+9 The CSS Style Sheet is placed inside the pages HTML Header Code."
        PRINT#f1,"    TCM :+9 A second TCM +9 places a pages index in a right column"
        PRINT#f1,"         rather than at the top of the page."
        PRINT#f1,"    TCM :+8 Bold front requires ending -8"
        PRINT#f1,"    TCM :+7 Indented Oblique Bold paragraph text requires ending -7"
        PRINT#f1,"    TCM :+6 Keep Text Format requires ending -6"
        PRINT#f1,"    TCM :+5 Start a new line with a spacer"
        PRINT#f1,"    TCM :-5 Start a New Line without a spacer"
        PRINT#f1,"    TCM :+4 Open the linked page in a new Browser Window"
        PRINT#f1,"    TCM :-4 Open an Interior/Exterior Link in the tail Frame"
        PRINT#f1,"    TCM :+3 Transfer a File from the HOLD_HTML_FILES Folder to the page's folder"
        PRINT#f1,"    TCM :-3 Display an inline image on your page"
        PRINT#f1,"    TCM :+1 Underline bracketed text"
        PRINT#f1,"    TCM :+2 Sub Index Entry without registering index link"
        PRINT#f1,"    TCM :-2 Sub Index Entry with index link"
        PRINT#f1,"    TCM :-1 Main Index Entry"
        PRINT#f1,"    :-6"
        PRINT#f1,"TCM -9 MOVE HTML FILE TO FOLDER:-2"
        PRINT#f1,"This tells The Program in what Folder to place an HTML file created from a text file."
        PRINT#f1,"    HTML_FOLDERS\HTML_FROM_TEXT is the default folder without TCM -9."
        PRINT#f1,"This line can not be used with any other marks and must consist of just the Folder Name!"
        PRINT#f1,"    [It is very important to place this mark below your spider references]:+1"
        PRINT#f1,"which are the first three lines of code created on every @WEBBER_TEXT text file."
        PRINT#f1,"    ie. [TEST_Folder:-9]"
        PRINT#f1,"    Once the HTML page has been created in this way,"
        PRINT#f1,"removing its text file from @WEBBER_TEXT will not remove the HTML page from the folder it was created in."
        PRINT#f1,"TCM +9 CREATE INTERNAL STYLE TAGS:-2"
        PRINT#f1,"    Rather than rely upon an external Style Sheet the Style Tags are created in the HTML heading Code."
        PRINT#f1,"This makes the HTML page more portable with a fixed format."
        PRINT#f1,"USE TCM +9 AGAIN TO CREATE A RIGHT COLUMN INDEX:-2"
        PRINT#f1,"    Use TCM +9 a second time, and a right Column is created for the page's index."
        PRINT#f1,"The default position for the index is at the top of the page."
        PRINT#f1,"When a large number of index lines obscures the main text, placing the index to the right makes for less scrolling."
        PRINT#f1,"TCM +8 -8 CREATE BOLD FONT:-2"
        PRINT#f1,"A missing close TCM -8 will create an error message!"
        PRINT#f1,"Example::+6"
        PRINT#f1,"    The Bold Font requires TCM -8 to end this font!:+8"
        PRINT#f1,"    With this example the TCM +8 is on the line above!"
        PRINT#f1,"    TCM -8 is at the end of this line!:-8:-6"
        PRINT#f1,"The Bold Font of TCM 8 will throw off width restrictions, this outcome can be controlled with blank lines."
        PRINT#f1,"TCM +7 -7 OBLIQUE PARAGRAPH FORMAT:-2"
        PRINT#f1,"This creates an indented oblique paragraph independent of line breaks.:+7"
        PRINT#f1,"A missing close TCM -7 will create an error message!"
        PRINT#f1,"The program will properly control the TCM execution so long as there are no nesting errors!:-7"
        PRINT#f1,"TCM +6 KEEP TEXT FORMAT:-2"
        PRINT#f1,"    Format lines the way they are recorded!  This is useful when your information"
        PRINT#f1,"requires lines larger than " + STR$(Cast) + " in length or a simple table.:-5"
        PRINT#f1,"+___________________________|______________________________|_!:+6"
        PRINT#f1,"+---------------------------|------------------------------|-!"
        PRINT#f1,"+---------------------------|------------------------------|-!"
        PRINT#f1,"+===========================|==============================|=!:-6:-5"
        PRINT#f1,"    This TCM does not always play well with other TCM marks like TCM 8,7,5,2,-2 and -1."
        PRINT#f1,"    Because HTML created with this program uses the monospace Courier Font,"
        PRINT#f1,"table like information can be outputted without using the HTML table tags and format rules."
        PRINT#f1,"TCM +5 -5 LINE RETURNS:-2"
        PRINT#f1,"    These TCM are depreciated because a future version will make use of them to create forms."
        PRINT#f1,"    +5 Starts a new line and adds a blank line with four spaces.:+5"
        PRINT#f1,"    I am a new line after a TCM +5."
        PRINT#f1,"    -5 Starts a new line and ends any indent.:-5"
        PRINT#f1,"I am a new line after a TCM -5."
        PRINT#f1,"    The 'TAB' key or a blank text line containing 4 spaces will separate HTML lines;"
        PRINT#f1,"an empty line created with the 'RETURN' key will start a new line without a spacer!"
        PRINT#f1,"TCM +-4 CREATE A LINK:-2"
        PRINT#f1,"    TCM 4 creates a link using 3 upper case brackets: {tail.html}Home_Page}"
        PRINT#f1,"Place inside these brackets; address of file and label of link in form"
        PRINT#f1,"        {(http://)(URL address)(#place on page)}name_label}"
        PRINT#F1,"    A TCM +4 calls up a new HTML page while a TCM -4 sends the page to the Right Frame."
        PRINT#f1,"For an internal page link place a [#] sign in front of the address."
        PRINT#f1,"    You can use a TCM -3 to create a icon image for your link."
        PRINT#f1,"The image should be in the ratio of 300X220_PX and about that size."
        PRINT#f1,"    Format Example [{http://ca.yahoo.com/}~secondimage.jpg~ Yahoo Canada}:+4:-3]"
        PRINT#f1, SPACE$(0)
        PRINT#f1,"{http://ca.yahoo.com/}~secondimage.jpg~ Yahoo Canada}:+4:-3"
        PRINT#f1, SPACE$(0)
        PRINT#f1,"    As in the case above, if a web address indicator [/] is not in the address"
        PRINT#f1,"the program will attempt to copy the file in the address from the"
        PRINT#f1,"HTML_FOLDERS\@HTML_HOLD_FILES to the Folder being used by the link!"
        PRINT#f1, SPACE$(0)
        PRINT#f1,"TCM +-3 CREATE AN INLINE IMAGE OR TRANSFER A FILE:-2"
        PRINT#f1,"    [This Mark requires the up case wave mark above the tab key of most computers.]:+1"
        PRINT#f1,"Use form [ ~image or file name~ ] followed by TCM -3 for an image"
        PRINT#f1,"or TCM +3 to transfer a file from the HOLD_HTML_FILES Folder."
        PRINT#f1,"    Examples:"
        PRINT#f1,"    What's inside the brackets will be moved to 'HTNL_FROM_TEXT'"
        PRINT#f1,"but will not be displayed in this Document!"
        PRINT#f1,"      [~secondimage.jpg~]:+3"
        PRINT#f1,"    The Image inside the following bracket will be shown!"
        PRINT#f1,"    [~secondimage.jpg~]:-3"
        PRINT#f1,"    Logo Image files (.jpg) used for links should be no larger in size than 320X200_PX"
        PRINT#f1,"and if smaller should maintain this aspect ratio!"
        PRINT#f1,"    When you can manage it the best size for regular photos is 800X450_PX this gives a 16/9 ratio frame."
        PRINT#f1,"TCM +1 UNDERLINE TEXT: This Mark will remove rectangle brackets and underline:-2"
        PRINT#f1,"the enclosed text.  [This text is underlined] uses the HTML tag 'EM' and CSS to"
        PRINT#f1,"achieve the below result:"
        PRINT#f1,"    [This text is underlined], (this text is not!):+1"
        PRINT#f1,"SPACE$(0)"
        PRINT#f1,"TCM -1 MAIN INDEX ENTRY:-2"
        PRINT#f1,"Creates a Main Index Entry and links to it!  Upon arrival at the"
        PRINT#f1,"HTML Label, a Left Click will navigate back to the Main Index link before the used index link."
        PRINT#f1,"Placing a colon (:) in the Line will reduce the length of the Link."
        PRINT#f1,"        Example: "
        PRINT#f1,"        (This: is a main link:-1)"
        PRINT#f1,"        'This' - will be the link name!"
        PRINT#f1,"    The use of TCM 7,6 or 5 with this or the Sub Index 2 is not advised!:+5"
        PRINT#f1,"TCM -2 or +2 SUB INDEX ENTRY:-2"
        PRINT#f1,"Creates a Sub Index to a Main Index Entry and links to it!  Upon arrival at the"
        PRINT#f1,"sub index HTML Label, a Left Click will navigate back to Previous Main Index Label before the used index link."
        PRINT#f1,"As in a Main Index placing a colon (:) in the Line will reduce the length of the Link."
        PRINT#f1,"        Example: "
        PRINT#f1,"        (This: is a Sub link:-2)"
        PRINT#f1,"        'This' - Will be the link name!:+5"
        PRINT#f1,"    The use of TCM 7,6 or 5 with this or the Main Index -1 is not advised!:+5"
        PRINT#f1, SPACE$(0)
        PRINT#f1,"TCM +2 UNLISTED SUB INDEX ENTRY:-2"
        PRINT#f1,"This index has all the properties of TCM -2 except it will not show up in the Page's Index."
        PRINT#f1,"The TCM standard entry is the TCM -2 which creates a displayed index link, by using TCM +2 instead"
        PRINT#f1,"only the Main TCM -1 index is displayed, but the sub index will still link to the Main index."
        PRINT#f1,"    The use of TCM 7,6 or 5 with this or the Main Index -1 is not advised!"
        PRINT#f1, SPACE$(0)
        PRINT#f1,"THE @WEBBER_TEXT FOLDER:-1"
        PRINT#f1,"The guts of your Web Site.  You can use any Word Processor that will save text in"
        PRINT#f1,"ASCII [.txt] format to create your own HTML Pages without knowing Hyper Text Markup"
        PRINT#f1,"Language; all you need to know are the TCM codes!"
        PRINT#f1,SPACE$(4)
        PRINT#f1,"The created pages in this folder will be listed in the left frame by file name."
        PRINT#f1,"Notepad, or {http://liquidninja.com/metapad/} MetaPad} can be used to create or:+4"
        PRINT#f1,"edit your text files."
        PRINT#f1,SPACE$(4)
        PRINT#f1,"The startup files in this folder are Head, Tail and Html_from_text.txt."
        PRINT#f1,"You can Edit the Files in this Folder to display something different from the default file's information."
        PRINT#f1,"    Deleting @WEBBER_TEXT\HTML_FROM_TEXT.TXT will remove it from your LEFT FRAME LINKS."
        PRINT#f1,"Instead of removing the File, you can remove it from the Web Site by adding '@WORK:-9' as the first line of it's text."
        PRINT#f1,"You will now find it's generated HTML Document in your @WORK Folder."
        PRINT#f1, SPACE$(0)
        PRINT#f1,"THE TAIL FRAME DOCUMENT:-2"
        PRINT#f1,"The @TEXT_WEBBER\tail.txt file is the introduction page displayed after entering the Web Site from the Index.html page."
        PRINT#f1,"It can be Edited by the user to suit his/her needs."
        PRINT#f1,"It can be displayed at any time by left clicking the HOME heading link!"
        PRINT#f1,"    If you have a DESIGNER page rename it tail.html and place it in the HTML_FOLDERS Folder."
        PRINT#f1,"Make '@WORK:-9' the first entry in @TEXT_WEBBER\tail.txt file below your spider entries."
        PRINT#f1,"Now run the program and your designer page will not be replaced by your tail.txt page."
        PRINT#f1,"MOVE FILES WITHOUT CREATING A HTML DOCUMENT:-2"
        PRINT#F1,"You can move files from the @HTML_HOLD_FILES folder to other Web Site folders"
        PRINT#F1,"using any file with 'dummy.txt' forming the last part of the name (move-this-dummy.txt)!"
        PRINT#f1,"    When used with TCM -9 this file will move files referenced with TCM -4,-3 or +3"
        PRINT#f1,"from the @HTML_HOLD_FILES to the TCM -9 folder."
        PRINT#f1,"The program will then delete this 'dummy' HTML file after moving the referenced files."
        PRINT#f1,"This file can be used to move designer HTML files about."
        PRINT#f1,"THE INDEX.HTML DOCUMENT:-2"
        PRINT#f1,"This is your default Web Page, a general call to your Web Site will be answered by this Document!"
        PRINT#f1,"For most Web Sites this is a graphic page with a link to your Site's INTERACTIVE WEB PAGES."
        PRINT#f1,"    Any graphic in your ROOT Folder named '" + firstimage$ + "' will be displayed by index.html!"
        PRINT#f1,"A default 'index.html' is only created if it does not exist, this means you can replace it with a designer page."
        PRINT#f1,"CONVERTING AND MOVING GRAPHIC FILES:-1"
        PRINT#f1,"    [Any GIF, PNG, JPG, BMP Graphic file placed in the root folder will be converted to JPG]:+1"
        PRINT#F1,"and moved to the HTML_HOLD_FILES Folder except for 'secondimage.jpg' and '" + firstimage$ + "' and the ground files."
        PRINT#f1,"The file exceptions will be moved to program folders like HTML_FOLDERS, HTML_FROM_TEXT."
        PRINT#f1,"    The GIF format in many ways is the best one for internet graphics but it requires any computer program"
        PRINT#f1,"generating or manipulating GIF to pay a License fee, the next best is PNG but it is not supported by SAFARI,"
        PRINT#f1,"so that leaves us with JPG.  You can however manually place a GIF or PNG file in the HTML_HOLD_FILES"
        PRINT#f1,"folder and use TCM tags to insert it into a HTML Page."
        PRINT#f1,"The HTML_FOLDERS Folder: Contains your Web Site!:-1"
        PRINT#f1,"    The HTML and CSS files which control the appearance of your three frame WEB SITE are located here."
        PRINT#f1,"The most important file is MAIN.HTML because it changes each time you run the program."
        PRINT#f1,"Unless you are confident that it has not changed you must upload it to your Web Site after running the program."
        PRINT#f1,"The DOWNLOADS Folder:-2"
        PRINT#f1,"Place zipped files (.zip) you wish visitors to download here!"
        PRINT#f1,"After running the program Clicking DOWNLOADS will display these files,"
        PRINT#f1,"clicking the file will start the download process."
        PRINT#f1,"    Place 'Trial4Webber.zip' in your root folder, run the program, you will now find this file in your Web Site's DOWNLOAD folder."
        PRINT#f1,"The @HTML_HOLD_FILES Folder:-2"
        PRINT#f1,"The files in the Folder 'HTML_FROM_TEXT' are erased each time you run The Program,"
        PRINT#f1,"therefore place image and HTML files that your text files require in Folder '@HTML_HOLD_FILES'."
        PRINT#f1,"    When you run The Program, files referenced by TCM 3 or 4 will be copied to Folder HTML_FROM_TEXT"
        PRINT#f1,"or to the Folder referenced by TCM -9."
        PRINT#f1,"    When a file is not found you can check the @CONTROL\file_not_found.txt"
        PRINT#f1,"for more path and file information to aid you in determining what file is missing."
        PRINT#f1,"The HTML_FROM_TEXT Folder:-2"
        PRINT#f1,"    The Default Folder for Converted Text Files from the Folder @WEBBER_TEXT."
        PRINT#f1,"Running The Program cleans out all previous files in this folder so directly placing"
        PRINT#f1,"or editing files here is a mistake you will not likely repeat."
        PRINT#f1,"    After running the program you do not have to upload to the internet every file in folder HTML_FROM_TEXT."
        PRINT#f1,"Upload to your internet site from this folder only the missing and/or changed files."
        PRINT#f1, SPACE$(0)
        PRINT#f1,"CREATE OTHER FOLDERS:-2"
        PRINT#f1,"    You can use TCM -9 to create a Folder and place the generated HTML Page there."
        PRINT#f1,"Any Image files referenced with a TCM 4 or 3 stored in the '@HTML_HOLD_FILES' will also be moved to"
        PRINT#f1,"the created Folder."
        PRINT#f1,"Clicking a FOLDER will list the HTML files links in that folder."
        PRINT#f1,"    Any HTML files not created directly by the program placed in a Folder will show up as a link."
        PRINT#f1,"[To have a supporting HTML document not show up as a link give it the (.htm) extension.]:+1"
        PRINT#f1,"Edit the calling HTML document to reflect the changed extension."
        PRINT#f1,"THE @GOODCOPY FOLDER:-1"
        PRINT#f1,"    This folder contains processed text files created from files in @WEBBER_TEXT."
        PRINT#f1,"    Load one or more of these text files into a word processor to create other document types"
        PRINT#f1,"such as 'html_from_text.doc'."
        PRINT#f1,"BACKUP FOR @WEBBER TEXT:-1"
        PRINT#f1,"    New @WEBBER TEXT files are backed up one time to the month folder in @WEBBER_TEXT_BACKUP when the program is run."
        PRINT#f1,"    Each time The Program is run the current version of a text file is backed up to the Year Folder."
        PRINT#F1,"After the first of the month, a new MONTH Folder is created and fresh, one time backups, are made of existing files."
        PRINT#f1,"    Whenever the year changes files in the previous year are not placed in the new year's folder."
        PRINT#f1,"If you happen to think of it; in January review and select the old text files you wish to keep from the previous year's"
        PRINT#f1,"folder and copy then to the new year folder, then delete the previous year folder."
        PRINT#f1,"    [Every Month a New Month Backup Folder is created and any files in that folder from last year are DELETED!]:+1"
        PRINT#f1,"UNUSED FILE CLEAN UP:-1"
        PRINT#f1,"    Over time your Web Site will fill up with junk Files not being used."
        PRINT#f1,"[You can clean up unused files by renaming the @HTML_HOLD_FILES Folder as '@DELETE' then run the program.]:+1"
        PRINT#f1,"    The Program will attempt to move required files from Folder '@DELETE' to the newly created '@HTML_HOLD_FILES'."
        PRINT#f1,"This process may create some error messages even when the missing files are found in '@DELETE', so run the program again."
        PRINT#f1,"    If the program detects no missing files you have cleaned up your Web Site."
        PRINT#f1,"    If after doing this procedure the program reports some not found files then try to find these files"
        PRINT#f1,"and place them in the @HTML_HOLD_FILES Folder."
        PRINT#f1,"    Now update your INTERNET SITE by removing from the site's HTML_FROM_TEXT Folder,"
        PRINT#f1,"all files not found in the Computer's HTML_FROM_TEXT Folder."
        PRINT#f1,"    If Disk Space is not an issue; Cut and Paste the @DELETE Folder to [@WEBBER TEXT\@WEBBER TEXT BACKUP\]."
        PRINT#f1,"That way if you wish to bring an old support file back into service, you may still have it in the accumulated backup @DELETE folder."
        CLOSE f1
        '--------
        FILECOPY file$,WebberText$ + "html_from_text.txt"
    FINALLY
        CLOSE f1
        '--------
    END TRY
END FUNCTION
''''''''''''
FUNCTION Create_Index_Page ALIAS "z2100" (file$) AS LONG
ON ERROR RESUME NEXT
ff = FREEFILE
gg = FREEFILE
' ________________________________________________
'/get firstfile name and other data from setup.txt
    REDIM sxx$(20)
    x1 = getarray("index html",sxx$(),fileF$)
    messg$ = "ENTER WEB SITE HERE"
    y$ = TRIM$(sxx$(2))
    IF LEN(y$) > 0 THEN messg$ = UCASE$(y$)
    firstimage$ = "firstimage.jpeg"
    y$ = TRIM$(sxx$(3))
    IF LEN(y$) > 0 THEN firstimage$ = LCASE$(y$)
    x=INSTR(firstimage$,".")
    IF x>0 THEN firstimage$=LEFT$(firstimage$,x-1)+".jpeg"
TRY
    '--------------------------
    OPEN file$ FOR INPUT AS ff
    CLOSE ff
    '--------
CATCH
    CLOSE ff
    '--------
    REDIM sxx$(20)
    Create_Index_Page = -1
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
                PRINT#ff,y$
            LOOP
        CLOSE gg
        '--------
        PRINT#ff,SPACE$(4)
        PRINT#ff,"  <body ID='indexbody' >"
        PRINT#ff,"      <div class='figureleft'>"
        PRINT#ff,"          <img class='tailscaled' src='" + firstimage$ + "' alt='" + firstimage$ + " not found'>"
        PRINT#ff,"          <p class='ptail'><a href='" + HTMLfolders$ + "frame.html'>" + messg$ +"</a></p>"
        PRINT#ff,"       </div>"
        PRINT#ff,"  </body>"
        PRINT#ff,SPACE$(4)
        PRINT#ff,"</html>"
    CLOSE ff
    '--------
FINALLY
    CLOSE ff
    '--------
END TRY
x=INSTR(firstimage$,".")
IF x>0 THEN firstimage$=LEFT$(firstimage$,x-1)
END FUNCTION
''''''''''''
FUNCTION DirCopy ALIAS "z2200" (were$,going$,outfile$,txType$) AS LONG
ON ERROR RESUME NEXT
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
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION fetch_all ALIAS "z2500" (were$,File$) AS LONG
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION get_endline_number ALIAS "z2600" (wPar$)
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION Folder_Fix ALIAS "z2700" (x$) AS STRING
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION tags ALIAS "z2800" (xx$,z) AS LONG
ON ERROR RESUME NEXT
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
    ' _______________________________
    '/Save backups FileBak$,FileYear$
    TRY                       '| if no backup creates one
        '-----------------------------
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
    ' _________________________________________
    '/Create New Backup and convert tabs to spaces
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
            sw=-1
            DO
                IF sw=-1 THEN
                    yy$ = TAB$(yy$,4)    '| turn tabs into spaces
                    PRINT#gg,yy$
                    sw=0
                ELSE
                    LINE INPUT#ff,yy$
                    '-----------------
                    yy$ = TAB$(yy$,4)    '| turn tabs into spaces
                    IF EOF(ff) THEN IF LEN(TRIM$(yy$))=0 THEN EXIT DO
                    PRINT#gg,yy$
                END IF
            LOOP UNTIL EOF(ff)
        CLOSE ff
        '--------
    CLOSE gg
    '--------
    ' _________________________________________________
    '/Sort peroid indexes using Array FileOut$(number)
    alfa_main$="!!!!.!!!!"
    alfa_subb$="!!!!.!!!!"
    number = 1
    FileOut$(number) = "A_"+alfa_main$+"_"+alfa_subb$+"_000.tmp"

    'sort should list letters before numbers
    ' ___________________________________
    '/Create Sortable Array from Indexes
    OPEN trash$ + FileOut$(number) FOR OUTPUT AS gg
        OPEN FileYear$ FOR INPUT AS ff
            TC=0
            Image
            '----------------
            DO UNTIL EOF(ff)
                LINE INPUT#ff,xx$
                '-----------------
                yy$=TRIM$(xx$)
                ' ___________________________________
                '/check Single Line for all TCM on Line
                TCM=TC
                DO
                    TC=dProcess(yy$)
                    IF TC=3 OR TC=-3 THEN GOSUB check_no_space_name
                LOOP UNTIL TC=0 OR TC=-1 OR TC=-2 OR TC=2
                'without this space return one index after another can cause display problem
                IF (TC=-1 OR TC=-2 OR TC=2) AND (TCM=-1 OR TCM=-2 OR TCM=2) THEN PRINT#gg,SPACE$(0)
                ' __________________________________________________
                '/Record Data by File Indexes, then Sort and Output
                'file name in two parts alfa_000000_000000.tmp
                'check for TC1 if new advance alfa_main$, zero alfa_subb$
                'check for TC2 if new advance alfa_subb$ keep alfa_main$
                IF TC=-1 THEN    '| create new alfa_main$
                    number=number+1
                    nb$="000"+TRIM$(STR$(number))
                    nb$=RIGHT$(nb$,3)
                    alfa_subb$="!!!!.!!!!"
                    'alfa_subb$="0000.0000"
                    FileOut$(number)="A_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                    '? number
                    '? FileOut$(number)
                    'waitkey$
                    x=INSTR(yy$,".")
                    IF x<6 AND x>1 THEN
                        'ordered index
                        yy$=yy$+SPACE$(1)    '| check enabler
                        y1$=LEFT$(yy$,x-1)    '| ???.??? to ???
                        y1$=TRIM$(y1$)
                        IF LEN(y1$)>0 THEN
                            y=INSTR(x,yy$,SPACE$(1))
                            y2$=""
                            IF MID$(yy$,x+1,1)<>SPACE$(1) THEN y2$=TRIM$(MID$(yy$,x+1,y-x))
                            IF LEN(y2$)>4 THEN y2$=LEFT$(y2$,4)
                            y1$="!!!!" + y1$
                            'y1$="0000" + y1$
                            'y1$=y1$+"!!!!"
                            'y1$=y1$+"0000"
                            y1$=RIGHT$(y1$,4)
                            y2$="!!!!" + y2$
                            'y2$="0000" + y2$
                            'y2$=y2$+"!!!!"
                            'y2$=y2$+"0000"
                            y2$=RIGHT$(y2$,4)
                            x$ =y1$+"."+y2$
                            y0$ = File_Fix(x$)
                            IF y0$<>"-1" THEN    '| no file name possible
                                alfa_main$ =y0$
                                FileOut$(number)="A_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                            END IF
                        END IF
                    END IF
                    CLOSE gg
                    '--------
                    '-----------------------------------------------
                    OPEN trash$ + FileOut$(number) FOR OUTPUT AS gg
                ELSEIF TC=-2 OR TC=2 THEN
                    number=number+1
                    nb$="000"+TRIM$(STR$(number))
                    nb$=RIGHT$(nb$,3)
                    FileOut$(number)="A_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                    x=INSTR(yy$,".")
                    IF x<6 AND x>1 THEN
                        'ordered index
                        yy$=yy$+SPACE$(1)    '| check enabler
                        y1$=LEFT$(yy$,x-1)    '| ???.??? to ???
                        y1$=TRIM$(y1$)
                        IF LEN(y1$)>0 THEN
                            y=INSTR(x,yy$,SPACE$(1))
                            y2$=""
                            IF MID$(yy$,x+1,1)<>SPACE$(1) THEN y2$=TRIM$(MID$(yy$,x+1,y-x))
                            IF LEN(y2$)>4 THEN y2$=LEFT$(y2$,4)
                            y1$="!!!!" + y1$
                            'y1$="0000" + y1$
                            'y1$=y1$+"!!!!"
                            'y1$=y1$+"0000"
                            y1$=RIGHT$(y1$,4)
                            y2$="!!!!" + y2$
                            'y2$="0000" + y2$
                            'y2$=y2$+"!!!!"
                            'y2$=y2$+"0000"
                            y2$=RIGHT$(y2$,4)
                            x$ =y1$+"."+y2$
                            y0$ = File_Fix(x$)
                            IF y0$<>"-1" THEN
                                alfa_subb$ =y0$
                                FileOut$(number)="A_"+alfa_main$+"_"+alfa_subb$+"_"+nb$+".tmp"
                            END IF
                        END IF
                    END IF
                    CLOSE gg
                    '--------
                    '-----------------------------------------------
                    OPEN trash$ + FileOut$(number) FOR OUTPUT AS gg
                END IF
                IF EOF(ff) THEN IF LEN(TRIM$(xx$))= 0 THEN EXIT DO
                PRINT#gg,xx$
            LOOP
        CLOSE gg
        '--------
    CLOSE ff
    '--------
    ? "add to file "aa OR 111"'here
    ? FileOut$(number)
    WAITKEY$

    'sorts from (1) to number
    ARRAY SORT FileOut$(1) FOR number    '| , COLLATE sortorder, deSCEND

    ? "check for list"'here
    ? FileOut$(number)
    WAITKEY$
    END
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
                    IF EOF(gg) THEN IF LEN(TRIM$(xx$))= 0 AND LEN(xx$)<4 THEN EXIT DO
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
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION single_quote_remove ALIAS "z3000" (phrase$) AS STRING
ON ERROR RESUME NEXT
phrase$ = TRIM$(phrase$)
    Image
    '-------------------------
    FOR x = 1 TO LEN(phrase$)
        IF MID$(phrase$,x,1) = "'" OR MID$(phrase$,x,1) = CHR$(34) THEN MID$(phrase$,x,1) = SPACE$(1)
    NEXT x
    single_quote_remove = phrase$
END FUNCTION
''''''''''''
SUB attempt_restore_from_delete ALIAS "z3100"
IF ERN > 0 THEN EXIT SUB
ON ERROR GOTO 3100
'------------------
    f1 = FREEFILE
    Pathit(2) = HTMLfolders$ + "@DELETE\"

    TRY
        '----------------------------------------------------
        OPEN Pathit(2) + "temp.tmp" FOR APPEND AS f1: CLOSE f1
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
        OPEN Control$ + "file_not_found.txt" FOR INPUT AS f1
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
                        IF  Found$(found2,0)<>"HTML_FROM_TEXT" THEN
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
            Topic$ ="11 IN RECOVERY FOLDER ATTEMPTED TO FIND MISSING FILES!"
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
ON ERROR RESUME NEXT
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
    ERRCLEAR
END FUNCTION
''''''''''''
FUNCTION setup_File ALIAS "z3400" (file$) AS LONG
ON ERROR RESUME NEXT
    TRY
        '-------------------------
        OPEN File$ FOR INPUT AS 1
            LINE INPUT#1,x$
            '---------------
        CLOSE 1
        '-------
        EXIT FUNCTION
    CATCH
        setup_File = -1
    END TRY
    '--------------------------
    OPEN file$ FOR OUTPUT AS 1
        PRINT#1," __________________________________________________________________________________"
        PRINT#1,"/TO ACTIVATE TAIL.CSS CHANGES; DELETE FILE FROM @CONTROL AND @WEBBER_TEXT          \"
        PRINT#1,"\==================================================================================/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/TEXT SIZE CONTROL; Length of text line; Size of Font in pixels            \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"width PX,78,16,                                                           /"
        PRINT#1,"\------------------------------------------------------------------------/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/WEB TITLE AND SEARCH DESCRIPTIONS:                                        \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"  FOR FRAME,INDEX AND CREATED HTML                                         |"
        PRINT#1,"---------------------------------------------------------------------------|"
        PRINT#1,"site title,FIX IN @CONTROL\SETUP.TXT,                                     /"
        PRINT#1,"spider keywords,WEB SITE CREATOR,HTML INDEX CREATOR,                     /"
        PRINT#1,"spider description,CREATES HTML PAGES FROM TEXT FILES,                  /"
        PRINT#1,"\----------------------------------------------------------------------/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/LINK LABEL FOR ENTERING WEB SITE                                          \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"index html,ENTER WEB SITE HERE,firstimage.jpeg,                            /"
        PRINT#1,"\------------------------------------------------------------------------/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/LEFT FRAME HEADINGS; COMA WITH BLANK SPACE FOR NO HEADING                 \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"Describe,Left Frame Links,1..Off Site,2..Webber Files,3..Webber Folders,  /"
        PRINT#1,"\------------------------------------------------------------------------/"
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/LEFT FRAME CONTACT LINKS; REMOVE WHOLE LINE TO ELIMINATE FROM PAGE        \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"EMail,E_Mail Web Master,you@yahoo.ca,                                /"
        PRINT#1,"Form,Free Web Site Hosting,http://www.webs.com,                          /"
        PRINT#1,"\-----------------------------------------------------------------------/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/HEAD FRAME CSS DEFAULT CONTROLS                                           \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"Item           Color               image           attributes              |"
        PRINT#1,"---------------------------------------------------------------------------|"
        PRINT#1,"hBODY,         RGB(200,200,200),headground.jpg,no-repeat,fixed,left,     /"
        PRINT#1,"hForeground,   RGB(000,000,000),                                         /"
        PRINT#1,"\-----------------------------------------------------------------------/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/MAIN FRAME CSS DEFAULT CONTROLS                                            \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"Item          Color                image          attributes               |"
        PRINT#1,"----------------------------------------------------------------------------"
        PRINT#1,"mBODY,      RGB(200,200,200),  mainground.jpg,no-repeat,fixed,left,    /"
        PRINT#1,"mForeground,RGB(000,000,000),                                             /"
        PRINT#1,"\------------------------------------------------------------------------/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/TAIL FRAME CSS DEFAULT CONTROLS                                            \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"Item          Color                image          attributes               |"
        PRINT#1,"----------------------------------------------------------------------------"
        PRINT#1,"tBODY,      RGB(200,200,200),  tailground.jpg,no-repeat,fixed,left,    /"
        PRINT#1,"tForeground,RGB(000,000,000),                                             /"
        PRINT#1,"\------------------------------------------------------------------------/"
        PRINT#1,SPACE$(4)
        PRINT#1," __________________________________________________________________________"
        PRINT#1,"/INDEX PAGE CSS DEFAULT CONTROLS                                            \"
        PRINT#1,"\--------------------------------------------------------------------------/"
        PRINT#1,"Item          Color                image          attributes               |"
        PRINT#1,"----------------------------------------------------------------------------"
        PRINT#1,"iBODY,      RGB(200,200,200),  indexground.jpeg,no-repeat,fixed,left,    /"
        PRINT#1,"iForeground,RGB(000,000,000),                                             /"
        PRINT#1,"\------------------------------------------------------------------------/"
        PRINT#1,"___________________________________________________________________________"
        PRINT#1,"LINKS      Forefround       Background         active                      \"
        PRINT#1,"---------------------------------------------------------------------------/"
        PRINT#1,"Link,   RGB(255,255,255), RGB(000,000,255),   RGB(000,000,000),           /"
        PRINT#1,"Hover,  RGB(000,000,255), RGB(255,255,255),                              /"
        PRINT#1,"Active, RGB(255,255,255), RGB(000,000,000),                             /"
        PRINT#1,"Offsite,RGB(000,000,000), RGB(000,000,255),                            /"
        PRINT#1,"\---------------------------------------------------------------------/"
        PRINT#1,SPACE$(4)
        PRINT#1,"==================================================================================="
        PRINT#1,"   RED GREEN BLUE COLOR TABLE          DARK                                        |"
        PRINT#1,"BLACK      RGB(000,000,000)                                                        |"
        PRINT#1,"GRAY       RGB(200,200,200)        136,136,136                                     |"
        PRINT#1,"RED        RGB(255,000,000)            160,000,000                                     |"
        PRINT#1,"GREEN      RGB(000,255,000)                                                            |"
        PRINT#1,"BLUE       RGB(000,000,255)                                                            |"
        PRINT#1,"YELLOW     RGB(255,255,000)                                                        |"
        PRINT#1,"CYAN       RGB(000,255,255)                                                          |"
        PRINT#1,"MAGENTA    RGB(255,000,255)                                                          |"
        PRINT#1,"WHITE      RGB(255,255,255)                                                        |"
        PRINT#1,"==================================================================================="
    CLOSE 1
    '-------
END FUNCTION
''''''''''''
FUNCTION Engineer_output ALIAS "z3500"
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION tcm_first_set$ ALIAS "z3600" (hPar$,TCM)
'opFile$
ON ERROR RESUME NEXT
    ' _________________________________________
    '/Capture opening Line or Input New Line   \
    IF LEN(hPar$) > 0 THEN
        nPar$ = hPar$
        hPar$ = ""
    ELSE                      '| nPar$ outputted for Opening tests
        LINE INPUT#3,nPar$
        '------------------
        lnumber=lnumber+1
    END IF
    '\_________________________________________/
    ' ______________________________________________
    '/New Line Indent Value                         \
    i1 = LEN(nPar$): i2 = LEN(LTRIM$(nPar$))
    i3 = i1-i2: IF i3<0 THEN i3=0    '| no indent change
    i1 = ((i1 - i2) > 1)*-(i1-i2)
    IF i1 > 2 THEN
        i1 = INT((i1 + 1)/4)*4
        IF i1 > 24 THEN i1 = 24
    ELSE
        i1=0
    END IF
    nPar$=TRIM$(nPar$)
    '\______________________________________________/
    IF LEN(nPar$) = 0 AND Format = 0 THEN
        ' _______________________________________
        '/nPar empty line; process and empty par$
        IF LEN(Par$)>0 THEN
            par$ = TRIM$(par$)+"<br>"
            FixLength$
            'trim and keep indent
            par$ = RTRIM$(par$)
            good$ = RTRIM$(good$)
        END IF
        'print out whats left
        IF LEN(Par$)>0 THEN PRINT#2,Par$
        IF LEN(good$)>0 THEN PRINT#8,good$
        'if Space Line; make space
        IF i1>0 THEN
            PRINT#2,SPACE$(4) + "<br>"
            PRINT#8,SPACE$(4)
        END IF
        Par$ = "":nPar$="":good$ = ""
    ELSE                      '| ______________________________________________________________
        '/nPar information line; process TMC
        ' __________________________
        '/Get TCM from new line
        hPar$ = Par$
        Par$ = nPar$
        TCM = dProcess(Par$)
        nPar$ = Par$
        Par$ = hPar$
        hPar$ = ""
        ' ______________________________________
        '/enities created except <> on same line
        gPar$ = check_enity_marks(nPar$,TCM)
        'gPar$ = check_power_mark(nPar$)
        nPar$=TRIM$(nPar$)
        gPar$=TRIM$(gPar$)
        xx = TCM = -1 OR ABS(TCM) = 2 OR TCM=-9 OR TCM = +6 OR TCM=+7 OR (i1>0 AND Format=0)
        IF ISTRUE xx THEN
            ' __________________________________________
            '/TCM or spacing(i1) requires new line start
            IF i1>0 THEN
                Par$ = TRIM$(Par$) + "<br>"
                good$ = RTRIM$(good$)+SPACE$(i1)
            ELSE
                good$ = RTRIM$(good$)
            END IF
            FixLength$
            Par$ = TRIM$(Par$)
            IF LEN(par$) >0 THEN PRINT#2,Par$
            IF LEN(good$)>0 THEN PRINT#8,good$
            Par$ = "": good$ = ""
            'fixup new line spacing
            IF TCM = -1 THEN
                i1=0: i3=0
            ELSEIF ABS(TCM) = 2 THEN
                IF i1<5 THEN i1=4
                IF i1>16 THEN i1=16
                nPar$=SPACE$(i1)+nPar$
                gPar$=SPACE$(i1)+gPar$
            ELSEIF TCM = +6 THEN
                i1=0
            ELSEIF TCM=+7 THEN
                i1=0          '| prevents repeat spacing
            ELSEIF i1>0 THEN
                nPar$=REPEAT$(i1,Spacing$)+nPar$
                gPar$=SPACE$(i1) + gPar$
            END IF
            Par$=nPar$
            Good$=gPar$
            nPar$="":gPar$=""
            'IF TCM=-1 THEN Good$="== "+TRIM$(Good$)+" =="
            'IF ABS(TCM)=2 THEN Good$="=== "+TRIM$(Good$)+" ==="

            IF TCM=-1 THEN
                 PRINT#8,SPACE$(4)
                 Good$=TRIM$(Good$)
                 PRINT#8,good$;
                 Good$=STRING$(LEN(good$),"=")

            END IF
            IF ABS(TCM)=2 THEN
                 Good$=TRIM$(Good$)
                 PRINT#8,SPACE$(4)+Good$;
                 Good$=SPACE$(4)+STRING$(LEN(good$),"-")
            END IF
            i1=0

        ELSE' __________________________________________
            '/not a new line start
            IF Format = -1 THEN
                Par$ = SPACE$(i3)+ nPar$
                Good$ = SPACE$(i3)+ gPar$
                nPar$="": gPar$=""
            ELSE
                FixLength$
                IF TCM=+8 THEN
                    PRINT#2,TRIM$(par$)+SPACE$(1);
                    PRINT#8,TRIM$(good$)+SPACE$(1);
                    Par$="": good$=""
                END IF
                Par$ = LTRIM$(Par$ + SPACE$(1)) + TRIM$(nPar$)
                good$ = LTRIM$(good$ + SPACE$(1)) + TRIM$(gPar$)
                gPar$="":nPar$=""
            END IF
        END IF
        ' _________________________________________________________________________________
        '/Process all First Stage Checks                                                   \
        DO
            SELECT CASE TCM
            '---------------
                CASE -9
                    New_Dir$ = folder_process$(Par$)
                    Par$ = ""
                    Good$ = ""
                    EXIT DO
                CASE +9
                    head_int=head_int-1
                CASE +1
                    Topic$ = "12 IN -> " + opFile$ + "->"
                    Problems(1) = "Text Code Mark +1 underline"
                    Problems(2) = "Error occurred at Line Number" + STR$(Lnumber)
                    Problems(3) = "Line=[" + Par$ + "]"
                    Problems(4) = "unexpected error!"
                    t=-1:a=1:b=1
                    DO
                        a = INSTR(a,par$,"[")
                        b = INSTR(b,par$,"]")
                        IF a<1 AND b<1 THEN EXIT DO

                        t=0    '| error found setting
                        IF a>0 AND b<a THEN
                            Problems(4) = "Second Bracket ']' position error?"
                            EXIT DO
                        END IF
                        IF b>0 AND (a>b OR a=0) THEN
                            Problems(4) = "First Bracket '[' position error?"
                            EXIT DO
                        END IF
                        IF a>0 AND b>a THEN
                            t = -1    '| no error found
                            a$ = LEFT$(par$,a-1)
                            b$ = MID$(par$,b+1)
                            c$ = MID$(par$,a+1,b-a-1)
                            a$=a$+"<em>"+c$+"</em>"
                            a=LEN(a$)+1
                            b=a
                            par$ = a$ + b$
                            IF a>=LEN(par$) THEN a=0
                        END IF
                    LOOP
                    IF t = 0 THEN problem_file(Topic$,4)
                CASE -3
                    create_Image$(support$)
                    FileMoveSupport(Support$)
                    REPLACE ANY "~" WITH SPACE$(1) IN Good$

                CASE +3
                    Topic$ = "13 IN -> " + opFile$ + "->"
                    Problems(1) = "Text Code Mark +3 move File"
                    Problems(2) = "Error occurred at Line Number" + STR$(Lnumber)
                    Problems(3) = "Line=[" + Par$ + "]"
                    t = 0: a=1
                    Image
                    '--------------
                    DO UNTIL a < 1
                        a = INSTR(par$,"~")
                        IF a = 0 THEN EXIT DO
                        t = -1
                        a$ = LEFT$(par$,a-1)
                        b$ = MID$(par$,a+1)
                        b = INSTR(b$,"~")
                        'process good copy
                        a1 = INSTR(good$,"~")
                        a1$ = LEFT$(good$,a1-1)
                        b1$ = MID$(good$,a1+1)
                        b1 = INSTR(b1$,"~")
                        IF b = 0 THEN
                            Problems(4) = "Second File Mark (~) not found!"
                            problem_file(Topic$,4)
                            EXIT DO
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
                            'process goodcopy
                            c1$ = MID$(b1$,b1+1)
                            b1$ = LEFT$(b1$,b1-1)
                            IF LEN(TRIM$(a1$)) > 0 OR LEN(TRIM$(c1$)) > 0 THEN
                                good$ = a1$ + c1$
                            ELSE
                                good$ = ""
                            END IF
                            '?#20,"  "'here
                            '?#20,"Format=(";Format;")"
                            '?#20,"TCM=(";TCM;")",Par$
                            '?#20,3,"|"+Good$+"|"
                            '?#20,"         ---------      "

                        END IF

                    LOOP
                    IF t = 0 THEN
                        Problems(4) = "First File Mark (~) not found!"
                        problem_file(Topic$,4)
                    END IF
                    REPLACE ANY "~" WITH SPACE$(1) IN Good$
                CASE +4
                    create_link$ (Support$,TCM)
                    FileMoveSupport(Support$)
                    REPLACE ANY "~" WITH SPACE$(1) IN Good$
                CASE -4
                    create_link$ (Support$,TCM)
                    FileMoveSupport(Support$)
                    REPLACE ANY "~" WITH SPACE$(1) IN Good$
                CASE 6
                    nest_subtract(TCM)
                    Par$ = "<" + TG(TCM) + REPEAT$(i3,Spacing$) + RTRIM$(Par$)
                    good$ = RTRIM$(SPACE$(i3) + TRIM$(good$)) + SPACE$(2)
                    PRINT#8,SPACE$(4)
                    Format = -1
                    '?#20,"  "'here
                    '?#20,"Format=(";Format;")"
                    '?#20,"TCM=(";TCM;")",Par$
                    '?#20,4,"|"+Good$+"|"
                    '?#20,"         ---------      "

                CASE 7
                    IF Format=0 THEN
                        nest_subtract(TCM)
                        Par$ = "<" + TG(TCM) + Par$
                        PRINT#8,SPACE$(4)
                    ELSE
                        Topic$ = "14 IN -> " + opFile$ + "->"
                        Problems(1) = "Text Code Mark +7 Paragraph"
                        Problems(2) = "Not compatible with +6 Format" + STR$(Lnumber)
                        Problems(3) = "Line=[" + Par$ + "]"
                        problem_file(Topic$,3)
                    END IF
                CASE 8
                    nest_subtract(TCM)
                    Par$ = "<" + TG(TCM) + Par$
                CASE ELSE
                    EXIT DO
            END SELECT
            TCM = dProcess(Par$)
        LOOP UNTIL TCM = 0
        ?#20,"  'here"
        ?#20,3,"|"+Good$+"|"
        ?#20,"         ---------      "

        '\_________________________________________________________________________________/
    END IF
END FUNCTION
''''''''''''
'opFile$
FUNCTION FixLength$ ALIAS "z3700"
ON ERROR RESUME NEXT
    IF ISTRUE Format THEN
        PRINT#2,Par$
        PRINT#8,good$
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
        TMPlength = PRElength + TMPlength
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
            PRINT#2,Good$
            Par$=""
            Good$=""
            EXIT DO
        END IF
    LOOP

END FUNCTION
''''''''''''
FUNCTION skip_tags ALIAS "z3800" (sPar$,oTag$,cTag$,TMPlength) AS LONG
'Spacing$="&ensp;"
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION const_index_linkage ALIAS "z3900" (Par$,TCM)
'!IMPORTANT! ALL LINKS HAVE FOLDER WERE FRAME IS AS THEIR ROOT FOLDER
ON ERROR RESUME NEXT
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
            ilk$="<dl id='index' class='figureleft' >"    '| #index
        ELSE
            ilk$="<dl class='indexline'>"
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
END FUNCTION
''''''''''''
FUNCTION tcm_third_set$ ALIAS "z4000" (TCM)
'opFile$
ON ERROR RESUME NEXT
    'third check :-1:-2
    'second check :+5:-5:-6:-7:-8
    'first check :+6:+7:+8:-4:+4:+3:-3:-9
    'hPar$ is new line
    SELECT CASE TCM
        CASE -1
            y=nest_subtract(-2)
            IF y=2 THEN PRINT#2,"</div><br>"
            x=nest_subtract(ABS(TCM))
            IF x=ABS(TCM) THEN PRINT#2,"</div>"
            PRINT#2,"<pre></pre><div>";
            PRINT#8,SPACE$(4)'STRING$(Cast-8,"=")
            good$ = TRIM$(good$)
            par$ = TRIM$(par$)
            const_index_linkage (Par$,TCM)
            par$ = TRIM$(par$)
            x=INSTR(par$,"</a>")
            IF x+3<LEN(par$) THEN
                ' _____________________________
                '/short link inside line
                a$=LEFT$(par$,x+3)
                b$=MID$(par$,x+4)
                sPar$=Par$
                TMPlength=Cast
                oTag$="<":cTag$=">"
                y=skip_tags (sPar$,oTag$,cTag$,TMPlength)
                IF y+4>TMPlength THEN    '| link is to long!
                    par$=b$
                    PRINT#2,a$+"<br>"
                ELSEIF LEN(Par$)> TMPlength THEN    '| line too long
                    Image
                    '------------------------------
                    FOR x=TMPlength-7 TO LEN(par$)
                        IF MID$(par$,x,1)=":" OR MID$(par$,x,1)=";" OR MID$(par$,x,1)="," OR MID$(par$,x,1)=SPACE$(1) THEN EXIT FOR
                    NEXT x
                    IF x>TMPlength THEN
                        par$=b$
                        PRINT#2,a$+"<br>"
                    ELSE
                        a$=LEFT$(par$,x)
                        par$=MID$(par$,x+1)
                        PRINT#2,a$+"<br>"
                    END IF
                ELSE          '| Line is right length
                    PRINT#2,par$+"<br>"
                    PRINT#8,good$
                    good$ = ""
                    par$ = ""
                END IF
            ELSE
                ' _____________________________
                '/whole line is link
                PRINT#2,par$+"<br>"
                PRINT#8,good$
                par$ = "":good$=""
            END IF
            'existindex = -1
            action$ = ""
            TCM = 0
        CASE -2, 2
            'New Line Indent Value
            i1 = LEN(Par$): i2 = LEN(LTRIM$(Par$))
            i1 = ((i1 - i2) > 1)*-(i1-i2)
            IF i1 > 3 THEN
                i1 = INT((i1 + 1)/4)*4
                IF i1 > 16 THEN i1 = 16
            ELSE
                i1=4
            END IF
            Par$=TRIM$(par$)
            x=nest_subtract(ABS(TCM))
            IF x=ABS(TCM) THEN PRINT#2,"</div>"
            PRINT#2,"<div>" + REPEAT$(i1,Spacing$);
            PRINT#8,SPACE$(4)'STRING$(cast-4,"-")
            good$ = SPACE$(i1) + TRIM$(good$)
            '?#20,"  "'here
            '?#20,"Format=(";Format;")"
            '?#20,"TCM=(";TCM;")",Par$
            '?#20,5,"|"+Good$+"|"
            '?#20,"         ---------      "

            const_index_linkage (Par$,TCM)
            par$ = TRIM$(par$)
            x=INSTR(par$,"</a>")
            IF x+3<LEN(par$) THEN
                ' _____________________________
                '/short link inside line
                TMPlength=Cast-4
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
                PRINT#2,par$
                PRINT#8,good$
                par$ = "":good$=""
            END IF
            action$ = ""
            TCM = 0
        CASE ELSE
            IF format=0 THEN
                Par$ = TRIM$(par$)
                good$ = RTRIM$(good$)
                IF LEN(par$) > 0 THEN
                    FixLength$
                    par$ = TRIM$(par$)
                    good$ = RTRIM$(good$)
                END IF
            END IF
        END SELECT
END FUNCTION
''''''''''''
'opFile$
FUNCTION tcm_second_set$ ALIAS "z4100" (TCM)
'opFile$
ON ERROR RESUME NEXT
    'second check :+5:-5:-6:-7:-8
    'third check :-1:-2
    'first check :+6:+7:+8:-4:+4:+3:-3:-9
    DO
        SELECT CASE TCM
        '---------------
            CASE -8
                nest_subtract(TCM)
                Par$ = Par$ + "</" + TG(ABS(TCM))
            CASE -7
                IF Format=0 THEN
                    nest_subtract(TCM)
                    Par$ = RTRIM$(Par$) + "</" + TG(ABS(TCM))
                    PRINT#2,Par$
                    Par$ = ""
                    good$=RTRIM$(good$)
                    PRINT#8,good$
                    'PRINT#8,SPACE$(4)
                    good$ = ""
                END IF
            CASE -6
                nest_subtract(TCM)
                Par$ = Par$ + "</" + TG(ABS(TCM))
                PRINT#2,Par$
                Par$ = ""
                IF LEN(TRIM$(Good$))>0 THEN PRINT#8,Good$ ELSE PRINT#8,SPACE$(4)
                good$ = ""
                format = 0
            CASE -5
                par$ = TRIM$(par$) + "<br>"
                good$ = RTRIM$(good$)
                FixLength$
                IF LEN(TRIM$(Par$)) > 0 THEN PRINT#2,TRIM$(par$)
                IF LEN(TRIM$(Good$)) > 0 THEN PRINT#8,good$
                par$ = "":good$=""
            CASE 5
                par$ = TRIM$(par$) + "<br>"
                good$ = RTRIM$(good$)
                FixLength$
                par$=RTRIM$(par$)
                IF LEN(Par$) > 0 THEN PRINT#2,par$
                good$=RTRIM$(good$)
                IF LEN(good$) > 0 THEN PRINT#8,good$
                PRINT#2,REPEAT$(4,Spacing$) + "<br>"
                PRINT#8,SPACE$(4)
                par$ ="":good$=""
            CASE ELSE
                EXIT DO
        END SELECT
        TCM = dProcess (Par$)
    LOOP UNTIL TCM = 0
    IF Format = -1 THEN
        PRINT#2,Par$
        PRINT#8,Good$
        Par$ = "": Good$ = ""
    END IF
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
ON ERROR RESUME NEXT
    Topic$ = "17 IN -> " + opFile$ + "->"
    Problems(1) = "Text Code Mark -3 create Image"
    support$ = ""
    a=1:t=0
    Image
    ?#20,1,"|"+Good$+"|"
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
                    i$ = "<div class='figureleft'><img class='tailscaled' "
                    i2$ = "</div>"
                END IF
                par$ = a$ + i$ + " src='" + b$ + "' alt='" + b$ + "_not found'>" + i2$ + c$
            END IF
        END IF
                    ?#20,"    ==================="'here
                    ?#20, "Parts:",a$,i$,b$,i2$,c$
                    ?#20,"         -----------"
                    ?#20,2,"|"+Good$+"|"
                    ?#20,"         -----------"

    LOOP
    IF t=0 THEN
        Problems(2) = "1. line=[" + Par$ + "]"
        Problems(3) = "First Image Mark (~) not found!"
        problem_file(Topic$,3)
    END IF
END FUNCTION
''''''''''''
FUNCTION FileMoveSupport ALIAS "z4400" (Support$)
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION create_link$ ALIAS "z4500" (Support$,TCM)
ON ERROR RESUME NEXT
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
END FUNCTION
''''''''''''
FUNCTION nest_subtract ALIAS "z4600" (TM) AS LONG
ON ERROR RESUME NEXT
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
                FUNCTION=TM    '| open tag not found                        '| close tag nest not found
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
EXIT FUNCTION
END FUNCTION
''''''''''''
' ___________________________________________
'/security Functions added to end of program
'FUNCTION ExeName ALIAS "z4700" (Op AS LONG) AS STRING
'ON ERROR RESUME NEXT
'LOCAL TmpAsciiz AS ASCIIZ * %MAX_PATH
'GetModuleFileName GetModuleHandle(""), TmpAsciiz, SIZEOF(TmpAsciiz)
'IF Op = 1 THEN
'    prm_name$ = TmpAsciiz
'ELSE
'    LOCAL i AS LONG
'    i = INSTR(-1, TmpAsciiz, "\")
'    IF Op = 2 THEN
'        prm_name$ = LEFT$(TmpAsciiz$, i)
'    ELSE
'        prm_name$ = MID$(TmpAsciiz$, i + 1)
'    END IF
'END IF
'FUNCTION = prm_name$
'END FUNCTION
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
            PRINT#ff,"<!DOCTYPE HTML>"
            PRINT#ff,"    <html lang=en-ca>"
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
            '----------------
            IF LEN(TRIM$(x$))>0 THEN EXIT TRY
        LOOP
        test = -1
    CLOSE ff
    '--------
CATCH
    CLOSE ff
    '--------
    test = -1
FINALLY
    CLOSE ff
    '--------
    ' ______________________________________________
    '/Web Site Descriptions!
    fileF$ = Control$  + "!setup.txt"
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
    IF ISTRUE test THEN
        '---------------------------
        OPEN file$ FOR OUTPUT AS ff
            PRINT#ff,"  <head>"
            PRINT#ff,"      <title>" + Title$ + "</title>"
            PRINT#ff,"      <meta charset='utf-8' />"
            PRINT#ff,"      <meta name='keywords' content='" + keyword$ + "' >"
            PRINT#ff,"      <meta name='description' content='" + Day$ +"," + descript$ + "' >"
            PRINT#ff,"      <link rel='stylesheet' TYPE='text/css' media='screen' href='tail.css'>"
            PRINT#ff,"  </head>"
        CLOSE ff
        '--------
    END IF
    DOCTYPE_heading = test
END TRY
CLOSE ff
'--------
5000:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END FUNCTION
''''''''''''
FUNCTION File_Fix ALIAS "z5200" (x$) AS STRING
ON ERROR RESUME NEXT
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
ON ERROR RESUME NEXT
    Clr? = SCREENATTR(row&, col&)
    ForeGround? = Clr? MOD 16
    BackGround? = Clr? \ 16
    LOCATE ,5: PRINT
    COLOR 12,14
    LOCATE ,5: PRINT STR$(delay) + " Sec To: Hit ENTER to Continue, Q to Quit, Any Key to Pause!"
    COLOR ForeGround?,BackGround?
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
END FUNCTION
''''''''''''
SUB Frameit ALIAS "z5500" (Sw,F,B)
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
'SLEEP 250'gives console time to form
COLOR F,B
CLS
COLOR BlackBR,BorderRG
LOCATE 1, 1
PRINT SPACE$(nSize);
LOCATE lenth, 1
PRINT SPACE$(nSize);
LOCATE 1, 1
Image
'----------------------
FOR i = 2 TO lenth - 1
    LOCATE i, nSize - 1
    PRINT "  ";
    LOCATE i, 1
    PRINT "  ";
NEXT i
COLOR F,B
LOCATE 3,3
5500:
IF ERR > 0 THEN ERN = ERR: ERNerror$ = FUNCNAME$
END SUB
'''''''
SUB Problem_file ALIAS "z5600" (Topic$, Number)
ON ERROR RESUME NEXT
    gproblem = -1             '| Add after call: IF ISTRUE gProblem THEN ERROR 154
    COLOR 0,12
    CLS
    CONSOLE SET FOCUS
    Y$="see: " + Control$ + Prm_name$ + "_PROBLEM.txt"
    LONGIS=LEN(Y$)+20: IF LONGIS>70 THEN LONGIS=70
    LOCATE 3,5: PRINT STRING$(LONGIS, "=")
    LOCATE ,10: PRINT "A PROBLEM NEEDS YOUR ATTENTION!"
    LOCATE ,10: PRINT Y$
    LOCATE ,5: PRINT STRING$(LONGIS, "=")
    pp = FREEFILE
    '-----------------------------------------------------------
    OPEN Control$ + Prm_name$ + "_PROBLEM.txt" FOR APPEND AS pp
        x$ = STRING$(LONGIS, "-")
        'PRINT#pp,x$
        'LOCATE ,5: PRINT x$
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
    CLOSE pp
    '--------
    REDIM problems(100)
    TIME(15)
END SUB
'''''''
SUB image ALIAS "z5700"
ON ERROR RESUME NEXT
    col& = CURSORX   'column location
    row& = CURSORY   'row location
    CURSOR OFF
    LOCATE 1, 1:PRINT "-" 'create screen attribute at location
    Clr? = SCREENATTR(1,1)    'use to calculate fore and back ground
    ForeGround? = Clr? MOD 16
    BackGround? = Clr? \ 16
    STATIC OT AS INTEGER 'output image blink
    OT = NOT (OT)
    LOCATE 1,1
    COLOR 0,15,3   '| colors only four spots
    IF OT THEN PRINT "{"; CHR$(60); "}" ELSE PRINT "{"; CHR$(62); "}"
    COLOR ForeGround?,BackGround?
    LOCATE row&, col&
    CURSOR ON
END SUB
'''''''
FUNCTION Time ALIAS "z5800" (x) AS STRING
ON ERROR RESUME NEXT
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
