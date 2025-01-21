
*#Backup Code only * Refer to the original post https://gist.github.com/sandraros/8b54e98166809fdb7ae39b1c11a846fe
* Improvement of code at https://blogs.sap.com/2014/02/25/alternative-to-readtext-function-module/comment-page-1/#comment-4121
* including Kenneth proposal at https://blogs.sap.com/2014/02/25/alternative-to-readtext-function-module/comment-page-1/#comment-452639
*
* Create your own class implementing LIF_PROCESSOR to process each text extracted.
* Two demo classes are provided: LCL_WRITER (active) and LCL_VERIFIER (if you want to test).
*
* Note: you won't need this code if you have the newest standard function modules READ_MULTIPLE_TEXTS and READ_TEXT_TABLE installed in
* your system (cf note 2261311 – Function module for reading multiple SAPscript texts -> https://launchpad.support.sap.com/#/notes/2261311/E )

REPORT zdemo.

INTERFACE lif_processor.
  TYPES ty_tlines TYPE STANDARD TABLE OF tline WITH EMPTY KEY.
  METHODS one_text
    IMPORTING
      object   TYPE stxl-tdobject
      name     TYPE stxl-tdname
      id       TYPE stxl-tdid
      language TYPE stxl-tdspras
      lines    TYPE ty_tlines.
ENDINTERFACE.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_objects   TYPE RANGE OF stxh-tdobject,
           ty_names     TYPE RANGE OF stxh-tdname,
           ty_ids       TYPE RANGE OF stxh-tdid,
           ty_languages TYPE RANGE OF stxh-tdspras.
    METHODS select
      IMPORTING
        objects      TYPE ty_objects
        names        TYPE ty_names
        ids          TYPE ty_ids
        languages    TYPE ty_languages
        package_size TYPE i DEFAULT 1000
        processor    TYPE REF TO lif_processor.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD select.
    TYPES:
      BEGIN OF ty_stxl,
        relid    TYPE stxl-relid,
        tdobject TYPE stxl-tdobject,
        tdname   TYPE stxl-tdname,
        tdid     TYPE stxl-tdid,
        tdspras  TYPE stxl-tdspras,
        srtf2    TYPE stxl-srtf2,
        clustr   TYPE stxl-clustr,
        clustd   TYPE stxl-clustd,
      END OF ty_stxl,
      "! compressed text data without text name
      BEGIN OF ty_stxl_raw,
        clustr TYPE stxl-clustr,
        clustd TYPE stxl-clustd,
      END OF ty_stxl_raw.
    DATA:
      t_stxl        TYPE STANDARD TABLE OF ty_stxl,
      t_stxl_buffer TYPE STANDARD TABLE OF ty_stxl,
      t_stxl_raw    TYPE STANDARD TABLE OF ty_stxl_raw,
      w_stxl_raw    TYPE ty_stxl_raw,
      "! uncompressed text
      t_tline       TYPE lif_processor=>ty_tlines,
      t_stxh        TYPE STANDARD TABLE OF stxh,
      w_stxh        TYPE stxh,
      s_stxl        TYPE ty_stxl,
      l_first_tabix TYPE sy-tabix,
      l_last_tabix  TYPE sy-tabix,
      subrc         TYPE sy-subrc,
      process       TYPE abap_bool,
      cursor        TYPE cursor.
    FIELD-SYMBOLS:
      <stxl>  TYPE ty_stxl,
      <tline> TYPE tline.


* OLD SELECT logic (may short dump cf https://blogs.sap.com/2014/02/25/alternative-to-readtext-function-module/comment-page-1/#comment-452639)
*    SELECT tdname tdobject tdid tdspras
*        FROM stxh
*          INTO CORRESPONDING FIELDS OF TABLE t_stxh
*        WHERE tdobject IN objects
*          AND tdname   IN names
*          AND tdid     IN ids
*          AND tdspras  IN languages.
*    CHECK t_stxh IS NOT INITIAL.
*    SORT t_stxh BY tdobject tdname tdid tdspras.
*
*    " select compressed text lines in blocks of 3000 (adjustable)
*    OPEN CURSOR cursor FOR
*    SELECT relid tdobject tdname tdid tdspras srtf2 clustr clustd
*            FROM stxl
*            FOR ALL ENTRIES IN t_stxh "WITH APPLICATION DATA AND TDNAME
*            WHERE relid    = 'TX'          "standard text
*              AND tdobject = t_stxh-tdobject
*              AND tdname   = t_stxh-tdname
*              AND tdid     = t_stxh-tdid
*              AND tdspras  = t_stxh-tdspras
*            ORDER BY PRIMARY KEY ##NO_TEXT.
* NEW SELECT logic:
    OPEN CURSOR cursor FOR
        SELECT l~relid l~tdobject l~tdname l~tdid l~tdspras l~srtf2 l~clustr l~clustd
        FROM
          stxl  AS l JOIN
          stxh  AS h
                ON l~tdobject = h~tdobject AND
                   l~tdname = h~tdname AND
                   l~tdid = h~tdid AND
                   l~tdspras = h~tdspras
        WHERE l~relid = 'TX' ##no_text
          AND h~tdobject IN objects
          AND h~tdname   IN names
          AND h~tdid     IN ids
          AND h~tdspras  IN languages
        ORDER BY
          l~relid
          l~tdobject
          l~tdname
          l~tdid
          l~tdspras
          l~srtf2.

    DO.
      FETCH NEXT CURSOR cursor
              APPENDING TABLE t_stxl
              PACKAGE SIZE package_size.
      subrc = sy-subrc.

      IF subrc = 4.
        IF lines( t_stxl ) > 0.
          process = abap_true.
        ELSE.
          process = abap_false.
        ENDIF.

      ELSEIF subrc = 0.
        IF lines( t_stxl ) < package_size.
          process = abap_true.
        ELSE.

          " put lines of last key aside, as there may be other lines for the same key
          DESCRIBE TABLE t_stxl LINES l_last_tabix.
          READ TABLE t_stxl INDEX l_last_tabix INTO s_stxl.
          READ TABLE t_stxl INDEX 1 ASSIGNING <stxl>.

          IF <stxl>-relid           = s_stxl-relid
                AND <stxl>-tdobject = s_stxl-tdobject
                AND <stxl>-tdname   = s_stxl-tdname
                AND <stxl>-tdid     = s_stxl-tdid
                AND <stxl>-tdspras  = s_stxl-tdspras.

            " The whole package has same key -> load next lines
            process = abap_false.

          ELSE.

            process = abap_true.

            l_first_tabix = l_last_tabix.
            l_first_tabix = l_last_tabix.
            DO.
              SUBTRACT 1 FROM l_first_tabix.
              READ TABLE t_stxl INDEX l_first_tabix ASSIGNING <stxl>.
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              IF NOT ( <stxl>-relid    = s_stxl-relid
                   AND <stxl>-tdobject = s_stxl-tdobject
                   AND <stxl>-tdname   = s_stxl-tdname
                   AND <stxl>-tdid     = s_stxl-tdid
                   AND <stxl>-tdspras  = s_stxl-tdspras ).
                EXIT.
              ENDIF.
            ENDDO.

            ADD 1 TO l_first_tabix.
            APPEND LINES OF t_stxl FROM l_first_tabix TO l_last_tabix TO t_stxl_buffer.
            DELETE t_stxl FROM l_first_tabix TO l_last_tabix.

          ENDIF.
        ENDIF.
      ELSE.
        " can't happen
        ASSERT 0 = 1.
      ENDIF.

      IF process = abap_true.
        LOOP AT t_stxl ASSIGNING <stxl>.

          AT NEW tdspras.
            CLEAR t_stxl_raw.
          ENDAT.

          " uncompress text
          CLEAR w_stxl_raw.
          w_stxl_raw-clustr = <stxl>-clustr.
          w_stxl_raw-clustd = <stxl>-clustd.
          APPEND w_stxl_raw TO t_stxl_raw.

          AT END OF tdspras.
            IMPORT tline = t_tline FROM INTERNAL TABLE t_stxl_raw.
            processor->one_text( object = <stxl>-tdobject id = <stxl>-tdid name = <stxl>-tdname language = <stxl>-tdspras lines = t_tline ).
            CLEAR t_stxl_raw.
          ENDAT.

        ENDLOOP.
      ENDIF.

      t_stxl = t_stxl_buffer.
      CLEAR t_stxl_buffer.

      IF subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    CLOSE CURSOR cursor.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_writer DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_processor.
ENDCLASS.

CLASS lcl_writer IMPLEMENTATION.
  METHOD lif_processor~one_text.

    FORMAT COLOR 5.
    WRITE: / object, name, id, language.
    FORMAT RESET.
    LOOP AT lines ASSIGNING FIELD-SYMBOL(<tline>).
      WRITE: / <tline>-tdformat, <tline>-tdline.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_verifier DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_processor.
ENDCLASS.

CLASS lcl_verifier IMPLEMENTATION.
  METHOD lif_processor~one_text.
    DATA tlines2 TYPE lif_processor~ty_tlines.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = id
        language                = language
        name                    = name
        object                  = object
      TABLES
        lines                   = tlines2
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7.
    ASSERT sy-subrc = 0.
    ASSERT lines = tlines2.
  ENDMETHOD.
ENDCLASS.

TABLES stxh.
SELECT-OPTIONS s_object FOR stxh-tdobject.
SELECT-OPTIONS s_name   FOR stxh-tdname.
SELECT-OPTIONS s_id     FOR stxh-tdid.
SELECT-OPTIONS s_langu  FOR stxh-tdspras.

START-OF-SELECTION.
  NEW lcl_app( )->select(
    EXPORTING
      objects      = s_object[]
      names        = s_name[]
      ids          = s_id[]
      languages    = s_langu[]
      package_size = 1000
      processor    = NEW lcl_writer( ) ).
