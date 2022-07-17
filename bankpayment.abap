*&---------------------------------------------------------------------*
*&                  
*&---------------------------------------------------------------------*
*& Report Name  : 
*& Report Title : 
*& SAP Release  : 
*& Programmer   : 
*& Functional   : 
*& Reference    :
*& Date Created : 
*&---------------------------------------------------------------------*
*&                DEVELOPMENT / MODIFICATION HISTORY
*&---------------------------------------------------------------------*
*& No.  |Ver. #   |Date             |Author        |Transport#
*&---------------------------------------------------------------------*
*&    |      |      |      |  
*&---------------------------------------------------------------------*
*&                       Description Transport
*----------------------------------------------------------------------*
REPORT ZBANKPAYMENT
       LINE-SIZE  132
       LINE-COUNT 65
       MESSAGE-ID ZF001
       NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES:REGUH.

*----------------------------------------------------------------------*
* Internal tables
*----------------------------------------------------------------------*
TYPES:BEGIN OF TY_GIRO_OCBC,
        DETAIL(10000),
      END OF TY_GIRO_OCBC.

TYPES:BEGIN OF TY_RENTASTT_OCBC,
        DETAIL(10000),
      END OF TY_RENTASTT_OCBC.

TYPES:BEGIN OF TY_JOMPAY_OCBC,
        DETAIL(10000),
      END OF TY_JOMPAY_OCBC.

TYPES:BEGIN OF TY_DUITNOW_OCBC,
        DETAIL(10000),
      END OF TY_DUITNOW_OCBC.

TYPES:BEGIN OF TY_ZBANKREQ,
        ZBUKRS TYPE BUKRS, "Paying company code
      END OF TY_ZBANKREQ.

DATA:LT_REGUH          TYPE TABLE OF REGUH WITH HEADER LINE,
     LT_REGUP          TYPE TABLE OF REGUP WITH HEADER LINE,
     LT_GIRO           TYPE TABLE OF REGUH WITH HEADER LINE,
     LT_RENTASTT_REGUH TYPE TABLE OF REGUH WITH HEADER LINE,
     LT_RENTASTT_REGUP TYPE TABLE OF REGUP WITH HEADER LINE,
     LT_GIRO_FILE      TYPE TABLE OF TY_GIRO_ WITH HEADER LINE,
     LT_LEAF           TYPE TABLE OF SETLEAF WITH HEADER LINE,
     LS_REGUH          TYPE REGUH,
     LS_LEAF           TYPE SETLEAF,
     LS_GIRO           TYPE REGUH,
     LS_GIRO2          TYPE TY_GIRO_OCBC.

DATA: LS_RENTASTT_REGUH TYPE REGUH,
      LS_RENTASTT       TYPE TY_RENTASTT_OCBC,
      LT_RENTASTT       TYPE STANDARD TABLE OF TY_JOMPAY_OCBC WITH HEADER LINE.

DATA: LT_JOMPAY_REGUH TYPE TABLE OF REGUH WITH HEADER LINE,
      LT_JOMPAY_REGUP TYPE TABLE OF REGUP WITH HEADER LINE,
      LS_JOMPAY_REGUH TYPE REGUH,
      LS_JOMPAY       TYPE TY_JOMPAY_OCBC,
      LT_JOMPAY       TYPE STANDARD TABLE OF TY_JOMPAY_OCBC WITH HEADER LINE.

DATA: LT_DUITNOW_REGUH TYPE TABLE OF REGUH WITH HEADER LINE,
      LT_DUITNOW_REGUP TYPE TABLE OF REGUP WITH HEADER LINE,
      LS_DUITNOW_REGUH TYPE REGUH,
      LS_DUITNOW       TYPE TY_DUITNOW_OCBC,
      LT_DUITNOW       TYPE STANDARD TABLE OF TY_DUITNOW_OCBC WITH HEADER LINE.


DATA: LT_ZBANKREQ TYPE STANDARD TABLE OF TY_ZBANKREQ.
*----------------------------------------------------------------------*
* Constant
*----------------------------------------------------------------------*
CONSTANTS:LC_RT(02) VALUE '01'.   "Record Type


*----------------------------------------------------------------------*
* Selection-options
*----------------------------------------------------------------------*
SELECT-OPTIONS:S_LAUFD FOR REGUH-LAUFD NO-EXTENSION NO INTERVALS,
               S_LAUFI FOR REGUH-LAUFI NO-EXTENSION NO INTERVALS.

*----------------------------------------------------------------------*
* Execution
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM EXTRACT_REGUH.
  PERFORM PROCESSING.

END-OF-SELECTION.

FORM EXTRACT_REGUH.

  SELECT * FROM REGUH
    WHERE LAUFD IN @S_LAUFD
      AND LAUFI IN @S_LAUFI
      AND XVORL IS INITIAL
      AND DTAWS IS INITIAL
  APPENDING CORRESPONDING FIELDS OF TABLE @LT_REGUH.

ENDFORM.

FORM PROCESSING.

  SORT LT_REGUH BY ZBVTY.
  LOOP AT LT_REGUH INTO LS_REGUH.
    CASE LS_REGUH-ZBVTY.
      WHEN 'GIRO'.
        MOVE-CORRESPONDING LS_REGUH TO LS_GIRO.
        APPEND LS_GIRO TO LT_GIRO.
      WHEN 'TT' OR 'RT'. " Rentas or TT
*        MOVE-CORRESPONDING LS_REGUH TO LS_RENTASTT_REGUH .
        APPEND LS_REGUH  TO  LT_RENTASTT_REGUH.
      WHEN 'JP'. "JOM PAY
        APPEND LS_REGUH TO LT_JOMPAY_REGUH.
      WHEN 'DN'. " Duit Now
        APPEND LS_REGUH TO LT_DUITNOW_REGUH.
    ENDCASE.

  ENDLOOP.
  REFRESH LT_REGUH.
  FREE LT_REGUH.

  IF LT_GIRO[] IS NOT INITIAL.
    PERFORM PROCESS_GIRO.
    PERFORM UPDATE_REGUH.
    PERFORM TRANSFER_DATASET_GIRO.
  ENDIF.
  IF LT_RENTASTT_REGUH[] IS NOT INITIAL.
    PERFORM PROCESS_RENTASTT.
    PERFORM UPDATE_REGUH.
    PERFORM TRANSFER_DATASET_RENTASTT.
  ENDIF.
  IF LT_JOMPAY_REGUH[] IS NOT INITIAL.
    PERFORM PROCESS_JOMPAY.
    PERFORM UPDATE_REGUH.
    PERFORM TRANSFER_DATASET_JOMPAY.
  ENDIF.
  IF LT_DUITNOW_REGUH[] IS NOT INITIAL.
    PERFORM PROCESS_DUITNOW.
    PERFORM UPDATE_REGUH.
    PERFORM TRANSFER_DATASET_DUITNOW.
  ENDIF.


ENDFORM.

FORM PROCESS_GIRO.
  DATA:LV_DEBIT_DATE(8),
       LV_DUMMY1(40),
       LV_BANKN2(20)    TYPE N,
       LV_AMOUNT(17)    TYPE N,
       LV_TOT_REC(6)    TYPE N,
       LV_TOTAL         LIKE REGUH-RBETR,
       LV_TOTAL2        LIKE REGUH-RBETR,
       LV_TOTAL_AMT     LIKE REGUH-RBETR,
       LV_DESCRIPT(9).
  DATA:LV_STRING_OUT TYPE STRING,
       LV_HTYPE      LIKE DD01V-DATATYPE.

  DATA:LS_REGUP TYPE REGUP.

  SELECT * FROM SETLEAF
    WHERE SETNAME = 'ZBANKROUTING'
  APPENDING CORRESPONDING FIELDS OF TABLE @LT_LEAF.

  READ TABLE LT_GIRO INDEX 1 INTO LS_GIRO.
  SELECT SINGLE BUTXT INTO @DATA(LV_BUTXT)
    FROM T001
  WHERE BUKRS = @LS_GIRO-ZBUKR.

  SELECT SINGLE HBKID INTO @DATA(LV_HBKID)
    FROM T012
  WHERE BUKRS = @LS_GIRO-ZBUKR.

  SELECT SINGLE BANKN INTO @DATA(LV_BANKN)
    FROM T012K
    WHERE BUKRS = @LS_GIRO-ZBUKR
  AND HBKID = @LV_HBKID.

  SELECT SINGLE TEXT1 INTO @DATA(LV_TEXT1)
    FROM T012T
    WHERE SPRAS = @SY-LANGU
      AND BUKRS = @LS_GIRO-ZBUKR
  AND HBKID = @LV_HBKID.

*   select single adrnr into @data(lv_adrnr)
*      from lfa1
*      where lifnr = @ls_giro-lifnr.
*
*
*  select single smtp_addr into @data(lv_email)
*    from adr6
*    where addrnumber = @lv_adrnr.

  WRITE LV_BUTXT LEFT-JUSTIFIED TO LV_BUTXT.
* write lv_text1 left-justified to lv_text1.
  WRITE LV_TEXT1 RIGHT-JUSTIFIED TO LV_TEXT1.
  WRITE LV_BANKN RIGHT-JUSTIFIED TO LV_BANKN.

  MOVE LV_BANKN TO LV_BANKN2.

  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4)
    INTO LV_DEBIT_DATE.
  CONCATENATE LV_DEBIT_DATE LS_GIRO-LAUFI INTO LV_TEXT1.

  REFRESH LT_GIRO_FILE.
*Header
  PERFORM HEADER_GIRO USING LV_BUTXT LV_BANKN2 LV_DEBIT_DATE LV_TEXT1.

*Detail
  LV_TOTAL = LV_TOT_REC = LV_TOTAL_AMT = 0.
  LOOP AT LT_GIRO INTO LS_GIRO.
    REFRESH LT_REGUP.
    CLEAR LS_GIRO2.
    ADD 1 TO LV_TOT_REC.
    PERFORM DETAIL_GIRO USING LS_GIRO LV_TOTAL CHANGING LV_TOTAL2.
    MOVE LV_TOTAL2 TO LV_TOTAL.
  ENDLOOP.

  PERFORM TRAILER_GIRO USING LV_TOT_REC LV_TOTAL.


ENDFORM.

FORM HEADER_GIRO USING BUTXT BANKN DEBIT_DATE TEXT1.
  DATA:LV_DUMMY1(40),
       LV_COUNT TYPE I.

  MOVE LC_RT         TO LS_GIRO2+0(2).      "Record Type
  MOVE '001'         TO LS_GIRO2+2(3).      "Record Running Sequence
  MOVE '00101'       TO LS_GIRO2+5(5).      "Branch of Account
  MOVE 'A999999'     TO LS_GIRO2+10(20).    "Company CIF#
  MOVE BUTXT         TO LS_GIRO2+30(30).    "Company Name
  MOVE BANKN         TO LS_GIRO2+60(20).    "Company Account Number
  MOVE 'D'           TO LS_GIRO2+80(1).     "Instruction
  MOVE 'N'           TO LS_GIRO2+81(1).     "Reversal Indicator
  MOVE DEBIT_DATE    TO LS_GIRO2+82(8).     "Debiting Date
  MOVE LV_DUMMY1     TO LS_GIRO2+90(40).    "Unused
  MOVE TEXT1         TO LS_GIRO2+130(16).   "Customer Ref No
  MOVE ''            TO LS_GIRO2+146(334).  "Unused
  APPEND LS_GIRO2 TO LT_GIRO_FILE.
  CLEAR  LS_GIRO2.

ENDFORM.

FORM DETAIL_GIRO USING LS_GIRO LIKE REGUH LV_TOTAL CHANGING LV_TOTAL2.
  DATA:LV_DEBIT_DATE(8),
       LV_DUMMY1(40),
       LV_BANKN2(20)    TYPE N,
       LV_AMOUNT(17)    TYPE N,
       LV_TOT_REC(7)    TYPE N,
*      lv_total like reguh-rbetr,
       LV_TOTAL_AMT     LIKE REGUH-RBETR,
       LV_DESCRIPT(9).
  DATA:LV_STRING_OUT TYPE STRING,
       LV_HTYPE      LIKE DD01V-DATATYPE,
       LV_COUNT      TYPE I.

  DATA:LS_REGUP TYPE REGUP.

  SELECT * FROM REGUP
     WHERE LAUFD = @LS_GIRO-LAUFD
       AND LAUFI = @LS_GIRO-LAUFI
       AND LIFNR = @LS_GIRO-LIFNR
       AND VBLNR = @LS_GIRO-VBLNR
       AND BUKRS = @LS_GIRO-ZBUKR
  APPENDING CORRESPONDING FIELDS OF TABLE @LT_REGUP.

  READ TABLE LT_LEAF INTO LS_LEAF
    WITH KEY SETNAME = 'ZBANKROUTING'
             VALFROM = LS_GIRO-ZBNKL.

  IF SY-SUBRC = 0.
    SELECT SINGLE DESCRIPT INTO @LV_DESCRIPT
      FROM SETLINET
       WHERE SETNAME = @LS_LEAF-SETNAME
    AND  LINEID  = @LS_LEAF-LINEID.
  ELSE.
    WRITE '' TO LV_DESCRIPT.
  ENDIF.

  SELECT SINGLE ADRNR INTO @DATA(LV_ADRNR)
     FROM LFA1
  WHERE LIFNR = @LS_GIRO-LIFNR.


  SELECT SINGLE SMTP_ADDR INTO @DATA(LV_EMAIL)
    FROM ADR6
  WHERE ADDRNUMBER = @LV_ADRNR.

  LOOP AT LT_REGUP ASSIGNING  FIELD-SYMBOL(<LS_REGUP>).

    WRITE LS_GIRO-ZBNKN LEFT-JUSTIFIED TO LS_GIRO-ZBNKN.
    WRITE LS_GIRO-NAME3 LEFT-JUSTIFIED TO LS_GIRO-NAME3.
*    IF LS_GIRO-RBETR LT 0.
*      LS_GIRO-RBETR = LS_GIRO-RBETR * -1.
*    ENDIF.
*    ADD LS_GIRO-RBETR  TO LV_TOTAL.
*    MULTIPLY LS_GIRO-RBETR  BY 100.
*    MOVE LS_GIRO-RBETR TO LV_AMOUNT.
    LS_REGUP-WRBTR = <LS_REGUP>-WRBTR.
    IF <LS_REGUP>-WRBTR LT 0.
      LS_REGUP-WRBTR = <LS_REGUP>-WRBTR * -1.
    ENDIF.
    ADD LS_REGUP-WRBTR  TO LV_TOTAL.
    MULTIPLY LS_REGUP-WRBTR  BY 100.
    MOVE LS_REGUP-WRBTR TO LV_AMOUNT.

    MOVE '02'          TO LS_GIRO2+0(2).      "Record Type
    MOVE LS_GIRO-ZBNKN TO LS_GIRO2+2(20).     "Account Number
    MOVE LV_AMOUNT     TO LS_GIRO2+22(17).    "Amount
    MOVE 'C'           TO LS_GIRO2+39(1).     "Instruction
    REPLACE '-' IN LS_GIRO-NAME3 WITH ''.

    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING
        STRING_IN  = LS_GIRO-NAME3
      IMPORTING
        STRING_OUT = LV_STRING_OUT
        HTYPE      = LV_HTYPE.

    IF LS_GIRO-NAME3 IS NOT INITIAL.
      IF LV_HTYPE = 'NUMC'.
        MOVE LS_GIRO-NAME3 TO LS_GIRO2+40(12).    "New IC Number
        MOVE ''            TO LS_GIRO2+52(8).     "Old IC Number
      ELSEIF LV_HTYPE = 'CHAR'.
        MOVE ''            TO LS_GIRO2+40(12).    "New IC Number
        MOVE LS_GIRO-NAME3 TO LS_GIRO2+52(8).     "Old IC Number
      ENDIF.
    ELSE.
      MOVE ''               TO LS_GIRO2+40(12).    "New IC Number
      MOVE ''               TO LS_GIRO2+52(8).     "Old IC Number
    ENDIF.

*    READ TABLE LT_REGUP INDEX 1 INTO LS_REGUP.
    REPLACE '-' IN LS_GIRO-NAME4 WITH ''.
    WRITE <LS_REGUP>-KIDNO LEFT-JUSTIFIED TO LS_REGUP-KIDNO.
    MOVE  LS_REGUP-KIDNO TO LS_GIRO2+60(20).                     "Transaction Description
    WRITE LS_GIRO-NAME4 LEFT-JUSTIFIED TO LS_GIRO2+80(20).       "Business Registration No
*  WRITE LS_REGUP-VBLNR LEFT-JUSTIFIED TO LS_GIRO2+100(20).     "Reference Number
    WRITE <LS_REGUP>-BELNR LEFT-JUSTIFIED TO LS_GIRO2+100(20).
    WRITE LV_DESCRIPT                   TO LS_GIRO2+120(9).      "Beneficiary ID
    WRITE LS_GIRO-NAME1  LEFT-JUSTIFIED TO LS_GIRO2+129(22).     "Beneficiary Name

    IF LS_GIRO-NAME3 IS NOT INITIAL AND LV_HTYPE = 'CHAR'.       "Police/Army ID/Passport No
      WRITE LS_GIRO-NAME3 LEFT-JUSTIFIED TO LS_GIRO2+151(20).
    ELSE.
      WRITE ''                           TO LS_GIRO2+151(20).
    ENDIF.
    IF LV_EMAIL IS NOT INITIAL.
      MOVE 'E'                              TO LS_GIRO2+171(1).   "Send Advice Via
      WRITE LV_EMAIL LEFT-JUSTIFIED         TO LS_GIRO2+172(50).  "E-mail.
    ELSE.
      MOVE ''                               TO LS_GIRO2+171(1).   "Send Advice Via
      WRITE LV_EMAIL LEFT-JUSTIFIED         TO LS_GIRO2+172(50).  "E-mail.
    ENDIF.
    WRITE ''                              TO LS_GIRO2+222(24).  "Fax No
    WRITE 'N'                             TO LS_GIRO2+246(2).   "ID Checking
    WRITE ''                              TO LS_GIRO2+247(2).   "Payment Type
    LV_COUNT = 248.
    DO 231 TIMES.
      LV_COUNT = LV_COUNT + 1.
*  WRITE ''                              TO LS_GIRO2+249(231). "Filler
      WRITE ''                              TO LS_GIRO2+LV_COUNT(1). "Filler
    ENDDO.
    APPEND LS_GIRO2 TO LT_GIRO_FILE.
    CLEAR LS_GIRO2.
*    REFRESH LT_REGUP.
    MOVE LV_TOTAL TO LV_TOTAL2.
  ENDLOOP.
ENDFORM.

FORM TRAILER_GIRO USING TOT_REC TOTAL.
  DATA:LV_AMOUNT(19) TYPE N,
       LV_HASH(20)   TYPE N.

  DATA: LV_COUNT TYPE I.
  CLEAR LS_GIRO2.
  LV_AMOUNT = TOTAL * 100.
  WRITE '03'      TO LS_GIRO2+0(2).               "Record Type
  WRITE TOT_REC   TO LS_GIRO2+2(6).               "Total count
  WRITE LV_AMOUNT TO LS_GIRO2+8(19).              "Total Amount
  WRITE LV_HASH   TO LS_GIRO2+27(20).             "Hash Total
*  DO 433 TIMES.
*    LV_COUNT = LV_COUNT + 1.
*    concatenate LS_GIRO2+47(1) '' INTO LS_GIRO2+47(LV_COUNT) RESPECTING BLANKS.
**    WRITE ''      TO LS_GIRO2+47(LV_COUNT).            "Filler
*  ENDDO.
  APPEND LS_GIRO2 TO LT_GIRO_FILE.
  CLEAR LS_GIRO2.

ENDFORM.
*
FORM TRANSFER_DATASET_GIRO .
  DATA:LV_FILENAME LIKE RLGRAP-FILENAME,
       LV_FILE     LIKE RLGRAP-FILENAME,
       LS_ZBANK_NO TYPE ZBANK_SEQ.
  DATA:LV_ZNO2(3) TYPE N VALUE 0.

  SELECT SINGLE ZNO INTO @DATA(LV_ZNO)
    FROM ZBANK_SEQ
    WHERE ZBUKRS = @LS_GIRO-ZBUKR
  AND ZDATE  = @SY-DATUM.

  IF SY-SUBRC = 0.
    ADD 1 TO LV_ZNO.
    UPDATE ZBANK_SEQ
      SET:ZNO = @LV_ZNO
      WHERE ZBUKRS = @LS_GIRO-ZBUKR
        AND ZDATE  = @SY-DATUM.
    MOVE LV_ZNO TO LV_ZNO2.
  ELSE.
    ADD 1 TO LV_ZNO2.
    MOVE LS_GIRO-ZBUKR TO LS_ZBANK_NO-ZBUKRS.
    MOVE SY-DATUM      TO LS_ZBANK_NO-ZDATE.
    MOVE LV_ZNO2       TO LS_ZBANK_NO-ZNO.
    INSERT ZBANK_SEQ FROM LS_ZBANK_NO.
  ENDIF.

  LV_FILENAME = |{ LS_GIRO-ZBUKR }_GIRO_{ LV_ZNO2 }_{ SY-DATUM }_{ LV_ZNO2 }.TXT|.
  LV_FILENAME = |/usr/sap/{ SY-SYSID }/banking/out/{ LV_FILENAME }|.

  OPEN DATASET LV_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  LOOP AT LT_GIRO_FILE.
    TRANSFER LT_GIRO_FILE TO LV_FILENAME LENGTH 480.
  ENDLOOP.
  CLOSE DATASET LV_FILENAME.

  DATA: LV_MSG TYPE STRING.

  LV_MSG = | { LV_FILENAME } Successfully generated|.
  .
  MESSAGE LV_MSG TYPE 'I' DISPLAY LIKE 'S'.

ENDFORM.

FORM UPDATE_REGUH .
  LOOP AT LT_GIRO INTO LS_GIRO.
  ENDLOOP.
ENDFORM.
FORM PROCESS_RENTASTT.

  DATA: LS_RENTASTT TYPE TY_RENTASTT_OCBC,
        LS_ZBANKREQ TYPE TY_ZBANKREQ.


  DATA: LV_AMOUNT(15) TYPE C.


  SELECT * FROM REGUP FOR ALL ENTRIES IN @LT_RENTASTT_REGUH
      WHERE LAUFD = @LT_RENTASTT_REGUH-LAUFD
        AND LAUFI = @LT_RENTASTT_REGUH-LAUFI
        AND LIFNR = @LT_RENTASTT_REGUH-LIFNR
        AND VBLNR = @LT_RENTASTT_REGUH-VBLNR
        AND BUKRS = @LT_RENTASTT_REGUH-ZBUKR
   APPENDING CORRESPONDING FIELDS OF TABLE @LT_RENTASTT_REGUP.


  LOOP AT LT_RENTASTT_REGUP INTO DATA(LS_RENTASTT_REGUP).
    TRY.
        DATA(LS_RENTASTT_REGUH) = LT_RENTASTT_REGUH[ VBLNR = LS_RENTASTT_REGUP-VBLNR  ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.

    ENDTRY.
    LS_ZBANKREQ-ZBUKRS = LS_RENTASTT_REGUH-ZBUKR.
    APPEND LS_ZBANKREQ TO LT_ZBANKREQ.
    "Product ID (2)
    MOVE LS_RENTASTT_REGUH-ZBVTY(2) TO LS_RENTASTT+0(2).

    "Your Reference No(16)
    DATA(DOCNO) = LS_RENTASTT_REGUP-BELNR.

    MOVE DOCNO TO LS_RENTASTT+2(16).

    "Debit Account No(14)
    MOVE LS_RENTASTT_REGUH-UBKNT TO LS_RENTASTT+18(14)  .

    "Debit Account Currency(3)
    MOVE LS_RENTASTT_REGUP-WAERS(3) TO LS_RENTASTT+32(3).

    "Amount Payable(15)
    LV_AMOUNT = LS_RENTASTT_REGUP-WRBTR.
    WRITE LV_AMOUNT TO LV_AMOUNT RIGHT-JUSTIFIED.
    OVERLAY LV_AMOUNT WITH '000000000000000'.
    MOVE  LV_AMOUNT TO LS_RENTASTT+35(15).

    "Amount Payable CUrrency(3)
    DATA(APCURRENCY) = LS_RENTASTT_REGUP-WAERS.
    MOVE APCURRENCY TO LS_RENTASTT+50(3).

    "Agent Charges(3)
    WRITE 'OUR' TO LS_RENTASTT+53(3).

    "Payee Account No.(35)
    MOVE LS_RENTASTT_REGUH-UBKNT TO LS_RENTASTT+56(35).

    "Payee Name(35).
    MOVE LS_RENTASTT_REGUH-NAME1 TO LS_RENTASTT+91(35).

    "Payee Add1 (35).
    MOVE LS_RENTASTT_REGUH-STRAS TO LS_RENTASTT+126(35).

    "Payee Add2 (35).
    MOVE LS_RENTASTT_REGUH-ORT01 TO LS_RENTASTT+161(35).

    "Payee Add3 (35)
    MOVE LS_RENTASTT_REGUH-PSTLZ TO LS_RENTASTT+196(35).

    "Filler(20)
    WRITE '' TO LS_RENTASTT+231(20).

    "Payee Country(2)
    MOVE LS_RENTASTT_REGUH-ZLAND TO LS_RENTASTT+251(2).

    "Payee Bank Name(35)
    MOVE LS_RENTASTT_REGUH-ZBNKL TO LS_RENTASTT+253(35).

    "Payee Bank Add1 (35).
    MOVE LS_RENTASTT_REGUH-STRAS TO LS_RENTASTT+288(35).

    "Payee Bank Add2(35) SPACE
    WRITE '' TO LS_RENTASTT+323(35).

    "Payee Bank Add3 (35)

    IF LS_REGUH-ZBVTY = 'RT'.

      WRITE '' TO LS_RENTASTT+358(35).

    ELSEIF LS_REGUH-ZBVTY = 'TT'.

      DATA(ADD3) = |{  LS_RENTASTT_REGUH-ZORT1 },{ LS_RENTASTT_REGUH-ZBNKS }|.
      MOVE ADD3 TO  LS_RENTASTT+358(35).

    ENDIF.

    "Payee Bank Number(4)
    MOVE LS_RENTASTT_REGUH-ZBNKN TO LS_RENTASTT+393(4).

    "Payee Branch Code(3)
    MOVE LS_RENTASTT_REGUH-ZBNKN(3) TO LS_RENTASTT+397(3).

    "Intermediary Bank Name(35)
    WRITE '' TO LS_RENTASTT+400(35).

    "Intermediary Bank Add1(35)
    WRITE '' TO LS_RENTASTT+435(35).

    "Intermediary Bank Add2(35)
    WRITE '' TO LS_RENTASTT+470(35).

    "Intermediary Bank Add3(35)
    WRITE '' TO LS_RENTASTT+505(35).

    "Bank to Bank Info (210)
    WRITE '' TO LS_RENTASTT+540(210).

    "Payment Details (140)
    WRITE '' TO LS_RENTASTT+750(140).

    "Send Invoice Via(1)
    WRITE 'E' TO LS_RENTASTT+890(1).

    "Email(50)

    SELECT SINGLE ADRNR INTO @DATA(LV_ADRNR)
    FROM LFA1
    WHERE LIFNR = @LS_RENTASTT_REGUH-LIFNR.


    SELECT SINGLE SMTP_ADDR INTO @DATA(LV_EMAIL)
      FROM ADR6
    WHERE ADDRNUMBER = @LV_ADRNR.

    MOVE LV_EMAIL TO  LS_RENTASTT+891(50).

    "Filler(24)
    WRITE '' TO LS_RENTASTT+941(24).

    "Mode Of Contract(1)
    WRITE 'D' TO LS_RENTASTT+965(1).

    "Filler(37)
    WRITE '' TO LS_RENTASTT+966(37).

    "Relationship with Beneficiary (1)
    WRITE 'N' TO LS_RENTASTT+1003(1).

    "FEA Approval ID(20)
    WRITE '' TO LS_RENTASTT+1004(20).

    "Contract No 1 (12)
    WRITE '' TO LS_RENTASTT+1024(12).


    "Contract No 2 (12)
    WRITE '' TO LS_RENTASTT+1036(12).

    "Contract No 3 (12)
    WRITE '' TO LS_RENTASTT+1048(12).

    "Contract No 4 (12)
    WRITE '' TO LS_RENTASTT+1060(12).

    "Contract No 5 (12)
    WRITE '' TO LS_RENTASTT+1072(12).


    "Buy Amount 1 (15)
    WRITE '' TO LS_RENTASTT+1084(15).

    "Buy Amount 2 (15)
    WRITE '' TO LS_RENTASTT+1099(15).

    "Buy Amount 3 (15)
    WRITE '' TO LS_RENTASTT+1114(15).

    "Buy Amount 4 (15)
    WRITE '' TO LS_RENTASTT+1129(15).

    "Buy Amount 5 (15)
    WRITE '' TO LS_RENTASTT+1144(15).

    "Filler(1)
    WRITE '' TO LS_RENTASTT+1159(1).

    "Charges Debit Account No.(12)
    WRITE '' TO LS_RENTASTT+1160(12).

    "Charges Debit Account Ccy(3).
    WRITE '' TO LS_RENTASTT+1172(3).

    "Purpose Code(5)

    WRITE '' TO LS_RENTASTT+1175(5).

    "Filler(15)
    WRITE '' TO LS_RENTASTT+1180(15).

    "Payee Resident Status(3)
    WRITE '' TO LS_RENTASTT+1195(3).

    "Value Date(8)
    MOVE LS_RENTASTT_REGUH-VALUT TO LS_RENTASTT+1198(8).

    "Intermediary Bank SWIFT ID (11)
    WRITE '' TO LS_RENTASTT+1206(11).

    "Beneficiary Bank SWIFT ID(11)
    MOVE LS_RENTASTT_REGUH-ZBNKN TO LS_RENTASTT+1217(11).
*    REGUP-ZBNKN

    "Intermediary Bank Clearing Code(16)
    WRITE '' TO LS_RENTASTT+1228(16).

    "Beneficiary Bank Clearing Code(16)
    WRITE '' TO LS_RENTASTT+1244(16).

    "Instructions Code 1 (4)
    WRITE '' TO LS_RENTASTT+1260(4).

    "Instructions Info 1 (30)
    WRITE '' TO LS_RENTASTT+1264(30).

    "Filler(68)
    WRITE '' TO LS_RENTASTT+1294(68).
    APPEND LS_RENTASTT TO LT_RENTASTT.
    """ New Line
    CLEAR LS_RENTASTT.
    "Record Type(3)
    WRITE 'INV' TO LS_RENTASTT+0(3).
    "Invoice Details (75)
    WRITE '' TO LS_RENTASTT+3(75).
    APPEND LS_RENTASTT TO LT_RENTASTT.
    CLEAR LS_RENTASTT.
  ENDLOOP.

ENDFORM.
FORM TRANSFER_DATASET_RENTASTT.
  DATA:LV_FILENAME LIKE RLGRAP-FILENAME,
       LV_FILE     LIKE RLGRAP-FILENAME,
       LS_ZBANK_NO TYPE ZBANK_SEQ.
  DATA:LV_ZNO2(3) TYPE N VALUE 0,
       LV_ZNO(3)  TYPE N VALUE 0.

  DELETE ADJACENT DUPLICATES FROM LT_ZBANKREQ.
  LOOP AT LT_ZBANKREQ INTO DATA(LS_ZBANK).
    SELECT SINGLE ZNO INTO @LV_ZNO
      FROM ZBANK_SEQ
      WHERE ZBUKRS = @LS_ZBANK-ZBUKRS
    AND ZDATE  = @SY-DATUM.

    IF SY-SUBRC = 0.
      ADD 1 TO LV_ZNO.
      UPDATE ZBANK_SEQ
        SET:ZNO = @LV_ZNO
        WHERE ZBUKRS = @LS_ZBANK-ZBUKRS
          AND ZDATE  = @SY-DATUM.
      MOVE LV_ZNO TO LV_ZNO2.
    ELSE.
      ADD 1 TO LV_ZNO2.
      MOVE LS_ZBANK-ZBUKRS TO LS_ZBANK_NO-ZBUKRS.
      MOVE SY-DATUM      TO LS_ZBANK_NO-ZDATE.
      MOVE LV_ZNO2       TO LS_ZBANK_NO-ZNO.
      INSERT ZBANK_SEQ FROM LS_ZBANK_NO.
    ENDIF.
  ENDLOOP.
  LV_FILENAME = |{ LS_ZBANK-ZBUKRS }_RENTASTT_{ LV_ZNO2 }_{ SY-DATUM }_{ LV_ZNO2 }.TXT|.
  LV_FILENAME = |/usr/sap/{ SY-SYSID }/banking/out/{ LV_FILENAME }|.

  OPEN DATASET LV_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  LOOP AT LT_RENTASTT.
    IF LT_RENTASTT+0(3) = 'INV'.
      TRANSFER  LT_RENTASTT TO LV_FILENAME LENGTH 75.
    ELSE.
      TRANSFER  LT_RENTASTT TO LV_FILENAME LENGTH 1362.
    ENDIF.
  ENDLOOP.
  CLOSE DATASET LV_FILENAME.

  DATA: LV_MSG TYPE STRING.

  LV_MSG = | { LV_FILENAME } Successfully generated|.
  .
  MESSAGE LV_MSG TYPE 'I' DISPLAY LIKE 'S'.

ENDFORM.
FORM PROCESS_JOMPAY.


  DATA: LS_JOMPAY   TYPE TY_JOMPAY_OCBC,
        LS_ZBANKREQ TYPE TY_ZBANKREQ.


  DATA: LV_AMOUNT_14(14) TYPE C,
        LV_ACCOUNT(20)   TYPE C.

  REFRESH: LT_JOMPAY_REGUP, LT_ZBANKREQ.

  SELECT * FROM REGUP FOR ALL ENTRIES IN @LT_JOMPAY_REGUH
      WHERE LAUFD = @LT_JOMPAY_REGUH-LAUFD
        AND LAUFI = @LT_JOMPAY_REGUH-LAUFI
        AND LIFNR = @LT_JOMPAY_REGUH-LIFNR
        AND VBLNR = @LT_JOMPAY_REGUH-VBLNR
        AND BUKRS = @LT_JOMPAY_REGUH-ZBUKR
   APPENDING CORRESPONDING FIELDS OF TABLE @LT_JOMPAY_REGUP.

  LOOP AT LT_JOMPAY_REGUP INTO DATA(LS_JOMPAY_REGUP).
    TRY.
        DATA(LS_JOMPAY_REGUH) = LT_JOMPAY_REGUH[ VBLNR = LS_JOMPAY_REGUP-VBLNR  ].
      CATCH  CX_SY_ITAB_LINE_NOT_FOUND.

    ENDTRY.
    " To update Ztable
    LS_ZBANKREQ-ZBUKRS = LS_JOMPAY_REGUH-ZBUKR.
    APPEND LS_ZBANKREQ TO LT_ZBANKREQ.


    "Debiting Account
    LV_ACCOUNT = |{ LS_JOMPAY_REGUH-UBKNT ALPHA = IN }|.
    MOVE LV_ACCOUNT TO LS_JOMPAY+0(20).


    "Currency CODE
    WRITE 'MYR' TO LS_JOMPAY+20(3).

    "Value Date
    MOVE LS_JOMPAY_REGUH-LAUFD TO LS_JOMPAY+23(8).

    "Biller COde
    WRITE LS_JOMPAY_REGUH-ZBNKN  TO LS_JOMPAY+31(10) LEFT-JUSTIFIED.

    "Filler
    WRITE '' TO LS_JOMPAY+41(16).

    "Billing Organization
    WRITE LS_JOMPAY_REGUH-NAME1 TO LS_JOMPAY+57(50) LEFT-JUSTIFIED.

    "AMOUNT
    LV_AMOUNT_14 = LS_JOMPAY_REGUP-WRBTR.
    WRITE LV_AMOUNT_14 TO LV_AMOUNT_14  RIGHT-JUSTIFIED.
    OVERLAY LV_AMOUNT_14 WITH '00000000000000'. "fourteen zero
    MOVE  LV_AMOUNT_14 TO LS_JOMPAY+107(14) .

    "JomPAY Ref-1(20 )
    "– From 6th onward of the Payee Account No
    MOVE LS_JOMPAY_REGUH-ZBNKN+5(13) TO LS_JOMPAY+121(20).

    "Filler
    WRITE '' TO LS_JOMPAY+141(30).

    "JomPAY Ref-2
    WRITE '' TO LS_JOMPAY+171(30).

    "Filler
    WRITE '' TO LS_JOMPAY+201(20).

    "Your Reference.
    MOVE LS_JOMPAY_REGUP-BELNR TO LS_JOMPAY+221(16).


    "Payment Details
    MOVE LS_JOMPAY_REGUP-KIDNO TO LS_JOMPAY+237(140).

    "Filler
    WRITE '' TO LS_JOMPAY+377(103).

    APPEND LS_JOMPAY TO LT_JOMPAY.
    CLEAR LS_JOMPAY.
  ENDLOOP.

ENDFORM.
FORM TRANSFER_DATASET_JOMPAY.
  DATA:LV_FILENAME LIKE RLGRAP-FILENAME,
       LV_FILE     LIKE RLGRAP-FILENAME,
       LS_ZBANK_NO TYPE ZBANK_SEQ.
  DATA:LV_ZNO2(3) TYPE N VALUE 0,
       LV_ZNO(3)  TYPE N VALUE 0.

  DELETE ADJACENT DUPLICATES FROM LT_ZBANKREQ.
  LOOP AT LT_ZBANKREQ INTO DATA(LS_ZBANK).
    SELECT SINGLE ZNO INTO @LV_ZNO
      FROM ZBANK_SEQ
      WHERE ZBUKRS = @LS_ZBANK-ZBUKRS
    AND ZDATE  = @SY-DATUM.

    IF SY-SUBRC = 0.
      ADD 1 TO LV_ZNO.
      UPDATE ZBANK_SEQ
        SET:ZNO = @LV_ZNO
        WHERE ZBUKRS = @LS_ZBANK-ZBUKRS
          AND ZDATE  = @SY-DATUM.
      MOVE LV_ZNO TO LV_ZNO2.
    ELSE.
      ADD 1 TO LV_ZNO2.
      MOVE LS_ZBANK-ZBUKRS TO LS_ZBANK_NO-ZBUKRS.
      MOVE SY-DATUM      TO LS_ZBANK_NO-ZDATE.
      MOVE LV_ZNO2       TO LS_ZBANK_NO-ZNO.
      INSERT ZBANK_SEQ FROM LS_ZBANK_NO.
    ENDIF.
  ENDLOOP.
  LV_FILENAME = |{ LS_ZBANK-ZBUKRS }_JOMPAY_{ LV_ZNO2 }_{ SY-DATUM }_{ LV_ZNO2 }.TXT|.
  LV_FILENAME = |/usr/sap/{ SY-SYSID }/banking/out/{ LV_FILENAME }|.

  OPEN DATASET LV_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  LOOP AT LT_JOMPAY.
    TRANSFER  LT_JOMPAY TO LV_FILENAME LENGTH 480.
  ENDLOOP.
  CLOSE DATASET LV_FILENAME.

  DATA: LV_MSG TYPE STRING.

  LV_MSG = | { LV_FILENAME } Successfully generated|.
  .
  MESSAGE LV_MSG TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM.
FORM PROCESS_DUITNOW.
  DATA: LS_DUITMOW  TYPE TY_DUITNOW_OCBC,
        LS_ZBANKREQ TYPE TY_ZBANKREQ.


  DATA: LV_AMOUNT_14(14) TYPE C,
        LV_ACCOUNT(20)   TYPE C.

  DATA: LV_DUITNOW_STR TYPE STRING,
        LV_PIPE        TYPE C VALUE '|'.

  REFRESH: LT_DUITNOW_REGUP, LT_ZBANKREQ.

  SELECT * FROM REGUP FOR ALL ENTRIES IN @LT_DUITNOW_REGUH
      WHERE LAUFD = @LT_DUITNOW_REGUH-LAUFD
        AND LAUFI = @LT_DUITNOW_REGUH-LAUFI
        AND LIFNR = @LT_DUITNOW_REGUH-LIFNR
        AND VBLNR = @LT_DUITNOW_REGUH-VBLNR
        AND BUKRS = @LT_DUITNOW_REGUH-ZBUKR
   APPENDING CORRESPONDING FIELDS OF TABLE @LT_DUITNOW_REGUP.

  LOOP AT LT_DUITNOW_REGUP INTO DATA(LS_DUITNOW_REGUP).

    TRY.
        DATA(LS_DUITNOW_REGUH) = LT_DUITNOW_REGUH[ VBLNR = LS_DUITNOW_REGUP-VBLNR  ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.

    ENDTRY.

    LS_ZBANKREQ-ZBUKRS = LS_DUITNOW_REGUH-ZBUKR.
    APPEND LS_ZBANKREQ TO LT_ZBANKREQ.

    "Product Code
    LV_DUITNOW_STR = |P2A|.

    "ValueDate
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LS_DUITNOW_REGUH-ZALDT }|.

    "Your Reference
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LS_DUITNOW_REGUP-BELNR }|.

    "Your Account No
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LS_DUITNOW_REGUH-UBKNT }|.

    "Amount Payable (15)
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LS_DUITNOW_REGUP-WRBTR }|.

    "Amount Currency(3)
    DATA : LV_CURR(3) TYPE C.

    LV_CURR =  LS_DUITNOW_REGUH-WAERS.
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LV_CURR }|.

    "Proxy Type(4).
    DATA: LV_PROXY(4).
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LV_PROXY }|.

    "Transfer Type/Payment Type.
    DATA: LV_PAYMENTYPE(4).
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LV_PAYMENTYPE }|.

    "Beneficiary Account No(35)
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LS_DUITNOW_REGUH-ZBNKN }|.

    "Beneficiary Bank SWIFT Code
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LS_DUITNOW_REGUH-ZBNKN }|.

    "Beneficiary Country Code
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }DFLT|.

    "DuitNow ID
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }|.

    "Recipient’s Reference
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }|. "Blank

    "Other Payment Details
    LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }|. "Blank


    SELECT SINGLE ADRNR INTO @DATA(LV_ADRNR)
    FROM LFA1
    WHERE LIFNR = @LS_DUITNOW_REGUH-LIFNR.


    SELECT SINGLE SMTP_ADDR INTO @DATA(LV_EMAIL)
      FROM ADR6
    WHERE ADDRNUMBER = @LV_ADRNR.

    IF LV_EMAIL IS NOT INITIAL.
      "Notify Beneficiary Via
      LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }E|.
      "Email Address
      LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }{ LV_EMAIL }|.
    ELSE.
      "Notify Beneficiary Via
      LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }|. "Blank
      "Email Address
      LV_DUITNOW_STR = |{ LV_DUITNOW_STR  }{ LV_PIPE }|. "Blank

    ENDIF.

    APPEND LV_DUITNOW_STR TO LT_DUITNOW.
    CLEAR LV_DUITNOW_STR.


  ENDLOOP.

ENDFORM.
FORM TRANSFER_DATASET_DUITNOW.
  DATA:LV_FILENAME LIKE RLGRAP-FILENAME,
       LV_FILE     LIKE RLGRAP-FILENAME,
       LS_ZBANK_NO TYPE ZBANK_SEQ.
  DATA:LV_ZNO2(3) TYPE N VALUE 0,
       LV_ZNO(3)  TYPE N VALUE 0.

  DELETE ADJACENT DUPLICATES FROM LT_ZBANKREQ.
  LOOP AT LT_ZBANKREQ INTO DATA(LS_ZBANK).
    SELECT SINGLE ZNO INTO @LV_ZNO
      FROM ZBANK_SEQ
      WHERE ZBUKRS = @LS_ZBANK-ZBUKRS
    AND ZDATE  = @SY-DATUM.

    IF SY-SUBRC = 0.
      ADD 1 TO LV_ZNO.
      UPDATE ZBANK_SEQ
        SET:ZNO = @LV_ZNO
        WHERE ZBUKRS = @LS_ZBANK-ZBUKRS
          AND ZDATE  = @SY-DATUM.
      MOVE LV_ZNO TO LV_ZNO2.
    ELSE.
      ADD 1 TO LV_ZNO2.
      MOVE LS_ZBANK-ZBUKRS TO LS_ZBANK_NO-ZBUKRS.
      MOVE SY-DATUM      TO LS_ZBANK_NO-ZDATE.
      MOVE LV_ZNO2       TO LS_ZBANK_NO-ZNO.
      INSERT ZBANK_SEQ FROM LS_ZBANK_NO.
    ENDIF.
  ENDLOOP.
  LV_FILENAME = |{ LS_ZBANK-ZBUKRS }_DUITNOW_{ LV_ZNO2 }_{ SY-DATUM }_{ LV_ZNO2 }.TXT|.
  LV_FILENAME = |/usr/sap/{ SY-SYSID }/banking/out/{ LV_FILENAME }|.

  OPEN DATASET LV_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  LOOP AT LT_DUITNOW.
    TRANSFER  LT_DUITNOW TO LV_FILENAME.
  ENDLOOP.
  CLOSE DATASET LV_FILENAME.

  DATA: LV_MSG TYPE STRING.

  LV_MSG = | { LV_FILENAME } Successfully generated|.
  .
  MESSAGE LV_MSG TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM..
