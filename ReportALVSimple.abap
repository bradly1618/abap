*&---------------------------------------------------------------------*
*& Include          ZRTR_F110_APRROVER_STATUS_CLS
*&---------------------------------------------------------------------*


CLASS lcl_f110_approver DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_display,
             laufd                TYPE laufd , " Run Date
             laufi                TYPE laufi, " Indentification
             currentapprover      TYPE char200,
             currentapproverlevel TYPE num03,
             nextapprover         TYPE char200,
             nextapproverlevel    TYPE num03,
*             workflowstatus       TYPE char50, " To know if its already approved or rejected
           END OF ty_display.

    CLASS-DATA:
      gt_display TYPE STANDARD TABLE OF ty_display.

    CLASS-METHODS: main.

  PRIVATE SECTION.
    CLASS-METHODS:
      get_data,
      display_alv.
ENDCLASS.
CLASS lcl_f110_approver IMPLEMENTATION.
  METHOD main.
*    CALL SELECTION-SCREEN 1000 .
*    WRITE: 'Hellpo'.
    get_data( ).
    display_alv( ).
  ENDMETHOD.
  METHOD get_data.

    DATA: ls_disp TYPE ty_display,
          member  TYPE zrtr_f110_stat,
          members TYPE TABLE OF zrtr_f110_stat WITH EMPTY KEY.

    SELECT * FROM zrtr_f110_stat INTO TABLE @DATA(lt_stat)
      WHERE laufd = @p_laufd
      AND laufi = @p_laufi
      AND zworfklowstatus = 'In Process'.
    .


    LOOP AT lt_stat ASSIGNING FIELD-SYMBOL(<lt_stat>) GROUP BY ( zparentworkitemid = <lt_stat>-zparentworkitemid  ) DESCENDING
                                                        .
      CLEAR members.
      LOOP AT GROUP <lt_stat> INTO member.
        members = VALUE #( BASE members ( member ) ).
        ls_disp-laufd = <lt_stat>-laufd.
        ls_disp-laufi = <lt_stat>-laufi.
        ls_disp-currentapprover =  <lt_stat>-zcurrentapprover.
        ls_disp-currentapprover =  REDUCE #( INIT text TYPE char50  FOR <line>
                                          IN lt_stat  WHERE ( zcurrentapproverlevel = <lt_stat>-zcurrentapproverlevel  AND zparentworkitemid =  <lt_stat>-zparentworkitemid )
                                          NEXT text = |{ text } { <line>-zcurrentapprover },|  ).

        SHIFT ls_disp-currentapprover RIGHT DELETING TRAILING ''.
        SHIFT ls_disp-currentapprover RIGHT DELETING TRAILING ','.
        SHIFT ls_disp-currentapprover LEFT DELETING LEADING ''.
        ls_disp-currentapproverlevel  = <lt_stat>-zcurrentapproverlevel.
        ls_disp-nextapprover    = <lt_stat>-znextapprover.
        SHIFT ls_disp-nextapprover RIGHT DELETING TRAILING ''.
        SHIFT ls_disp-nextapprover RIGHT DELETING TRAILING ','.
        SHIFT ls_disp-nextapprover LEFT DELETING LEADING ''.
        ls_disp-nextapproverlevel = <lt_stat>-znextapproverlevel.
        SHIFT ls_disp-nextapproverlevel  RIGHT DELETING TRAILING ','.
        APPEND ls_disp TO gt_display.
      ENDLOOP.


    ENDLOOP.

    SORT gt_display.
    DELETE ADJACENT DUPLICATES FROM gt_display.
  ENDMETHOD.


  METHOD display_alv.

    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

*    DATA: lo_event_handler TYPE REF TO lcl_handler, " Variables for events
    DATA    lo_events        TYPE REF TO cl_salv_events_table.

    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
          lo_content     TYPE REF TO cl_salv_form_element,
          lv_title       TYPE string,
          lv_rows        TYPE string.

    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
          lv_key    TYPE salv_s_layout_key.

    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table.

* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_display.
      CATCH cx_salv_msg.
    ENDTRY.

* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( abap_false ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).

* Create header


    lv_title = 'F110 Approval Status'.

    CREATE OBJECT lo_grid.
    CREATE OBJECT lo_layout_logo.
    lo_grid->create_label( row = 1 column = 1 text = lv_title tooltip = lv_title ).
    lo_layout_logo->set_left_content( lo_grid ).
    lo_content = lo_layout_logo.
    lo_gr_alv->set_top_of_list( lo_content ).

* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable the save layout buttons
    lv_key-report = sy-repid.
    lo_layout = lo_gr_alv->get_layout( ).
    lo_layout->set_key( lv_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lo_layout->set_default( abap_true ).

** Register events
*    lo_events = lo_gr_alv->get_event( ).
*    CREATE OBJECT lo_event_handler.
*    SET HANDLER lo_event_handler->on_double_click FOR lo_events.
*
** Enable cell selection mode
*    lo_selections = lo_gr_alv->get_selections( ).
*    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    TRY.
        lo_column ?= lo_columns->get_column( 'CURRENTAPPROVER' ). " Find the 'MAKTX' column ans change attributes
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Current Approver' ).
        lo_column->set_medium_text( 'Current Approver' ).
        lo_column->set_short_text( 'Curr Apprv' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'CURRENTAPPROVERLEVEL' ). " Find the 'MAKTX' column ans change attributes
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Current Appv Level' ).
        lo_column->set_medium_text( 'Current Appv Level' ).
        lo_column->set_short_text( 'C. App Lvl' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'NEXTAPPROVER' ). " Find the 'MAKTX' column ans change attributes
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Next Approver' ).
        lo_column->set_medium_text( 'Next Approver' ).
        lo_column->set_short_text( 'Next Apprv' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'NEXTAPPROVERLEVEL' ). " Find the 'MAKTX' column ans change attributes
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Next Approver Level' ).
        lo_column->set_medium_text( 'Next Approver Level' ).
        lo_column->set_short_text( 'N. App Lvl' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'WORKFLOWSTATUS' ). " Find the 'MAKTX' column ans change attributes
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Worflow Status' ).
        lo_column->set_medium_text( 'Worflow Status' ).
        lo_column->set_short_text( 'WF Status.' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_gr_alv->display( ).

  ENDMETHOD.
ENDCLASS.
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.                    "cl_handler DEFINITION

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_double_click.
*    IF column EQ 'MATNR'.
*      READ TABLE gt_material INTO DATA(wa_st_data) INDEX row.
*
** Check that material exists
*      SELECT COUNT( * ) FROM mara UP TO 1 ROWS WHERE matnr EQ wa_st_data-matnr.
*
*      IF sy-subrc = 0. " Exists?
** Load parameters
*        SET PARAMETER ID 'MXX' FIELD 'K'. " Default view
*        SET PARAMETER ID 'MAT' FIELD wa_st_data-matnr. " Material number
*
*        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*      ELSE. " No ?
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = wa_st_data-matnr
*          IMPORTING
*            output = wa_st_data-matnr.
*
*        DATA(lv_err) = `Material ` && wa_st_data-matnr && ` does not exist.`.
*        MESSAGE lv_err TYPE 'I' DISPLAY LIKE 'E'.
*      ENDIF.
*    ELSE.
*      MESSAGE TEXT-002 TYPE 'I'. " Invalid cell
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
