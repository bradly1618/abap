*&---------------------------------------------------------------------*
*& Report  zsolman_email_noti_ps
*&
*&---------------------------------------------------------------------*
*& Background Job Program SM36
*&
*&---------------------------------------------------------------------*
REPORT zsolman_email_noti_ps.

*&---------------------------------------------------------------------*
DATA: gv_run TYPE string.
TABLES: crmd_orderadm_h.

SELECTION-SCREEN BEGIN OF BLOCK a1 .
SELECT-OPTIONS : s_date FOR crmd_orderadm_h-posting_date.
PARAMETERS: p_test TYPE char01 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK a1.


AT SELECTION-SCREEN.
  gv_run = sy-ucomm.

INITIALIZATION.

CLASS lcl_rpt DEFINITION.
  PUBLIC SECTION.

    "Get Partner Data
    TYPES: BEGIN OF ty_partner,
             partner_no TYPE crmt_partner_guid,
             addr_nr    TYPE  ad_persnum,
             guid       TYPE crmt_partner_guid,
           END OF ty_partner,
           tt_partner TYPE TABLE OF ty_partner.

    " Types for Notification Email

    TYPES: BEGIN OF ty_notif_email,
             ticketid TYPE crm_fs_object_id,
             email    TYPE ad_smtpadr,
             status   TYPE stat5,
           END OF ty_notif_email,
           tt_notif_email TYPE TABLE OF ty_notif_email.
    CLASS-DATA:
      lt_partner     TYPE tt_partner,
      ls_partner     TYPE ty_partner,
      gt_notif_email TYPE tt_notif_email.

    CLASS-METHODS: main,
      display.

  PROTECTED SECTION.
    CLASS-METHODS: send_email.

ENDCLASS.
CLASS lcl_rpt IMPLEMENTATION.
  METHOD main.

    DATA: ls_notif_email TYPE ty_notif_email.
    "GET Ticket with Purpose Solution Status.
    SELECT
      crmd_orderadm_h~guid,
      crmd_orderadm_h~object_id AS object_id,
      crmd_orderadm_h~description AS description,
      crmd_orderadm_h~created_at AS created_at,
      crmd_orderadm_h~posting_date AS posting_date,
      tj30t~txt30 AS status
      INTO TABLE @DATA(lt_order)
      FROM crmd_orderadm_h
      INNER JOIN crm_jest ON crm_jest~objnr = crmd_orderadm_h~guid AND crm_jest~inact = ' '
      INNER JOIN tj30t ON tj30t~estat = crm_jest~stat AND tj30t~spras = 'E' AND (  tj30t~stsma EQ 'ZMIN0001'  OR  stsma EQ 'ZMPR0001' OR stsma EQ 'ZMRQ0001' OR stsma EQ 'ZMRT0001' )
      WHERE tj30t~txt30 LIKE '%Proposed Solution%' AND ( crmd_orderadm_h~process_type   EQ 'ZMIN' OR process_type EQ 'ZMRQ' )
      AND crmd_orderadm_h~posting_date IN @s_date
      .

    SORT lt_order BY object_id.
    DELETE ADJACENT DUPLICATES FROM lt_order COMPARING ALL FIELDS.

    " Get The link
    SELECT *
        INTO TABLE @DATA(lt_link)
        FROM crmd_link
        FOR ALL ENTRIES IN @lt_order
        WHERE guid_hi = @lt_order-guid
        AND objtype_set EQ '07'
        AND objtype_hi EQ '05'
    .

    SELECT partner_no,
       addr_nr,
       guid
    FROM crmd_partner
    INTO  TABLE  @DATA(lt_partner_tmp)
    FOR ALL ENTRIES IN @lt_link
    WHERE guid EQ @lt_link-guid_set
    AND partner_fct EQ 'SLFN0002'. " Reported By

    "Change to 16 char format

    LOOP AT lt_partner_tmp ASSIGNING FIELD-SYMBOL(<fs_partner>).
      MOVE-CORRESPONDING <fs_partner> TO ls_partner.
      ls_partner-partner_no = <fs_partner>-partner_no.
      APPEND ls_partner TO lt_partner.
    ENDLOOP.

    " Get user name
    SELECT
       mc_name1,
       mc_name2,
       persnumber,
       partner_guid
      FROM but000
       INTO TABLE @DATA(lt_adrc_partner)
       FOR ALL ENTRIES IN @lt_partner
       WHERE partner_guid EQ   @lt_partner-partner_no .

    " Partner Address Detail including Email

    SELECT adrc~addrnumber, adrc~location, adrc~str_suppl3, adrc~str_suppl2,
           adr6~smtp_addr
    FROM adrc
    INNER JOIN adr6
    ON adrc~addrnumber EQ adr6~addrnumber
    INTO TABLE @DATA(lt_partner_address)
    FOR ALL ENTRIES IN @lt_partner
    WHERE adrc~addrnumber = @lt_partner-addr_nr
    .

    LOOP AT lt_order ASSIGNING FIELD-SYMBOL(<fs_order>).
* VALUE #( lt_multicat_final[ guid =  <fs_order>-guid ]-cat_guid OPTIONAL ).
      DATA(lv_link) = VALUE #( lt_link[ guid_hi = <fs_order>-guid ]-guid_set OPTIONAL  ).

      DATA(lv_addr_nr) = VALUE #( lt_partner[ guid = lv_link ]-addr_nr OPTIONAL ).
      DATA(lv_email) = VALUE #( lt_partner_address[ addrnumber = lv_addr_nr ]-smtp_addr OPTIONAL ).
      IF lv_email IS INITIAL AND p_test NE 'X' . CONTINUE. ENDIF. " Skip...
      ls_notif_email-email = COND #( WHEN lv_email IS NOT INITIAL THEN lv_email ).
      ls_notif_email-ticketid = |{ <fs_order>-object_id }|.
      APPEND ls_notif_email TO gt_notif_email.
    ENDLOOP.

    IF gt_notif_email IS NOT INITIAL.
      IF  p_test NE 'X'.
        send_email( ).
      ELSE.

        display( ).
      ENDIF.


    ENDIF.

  ENDMETHOD.

  METHOD send_email.

    CONSTANTS:

      gc_raw     TYPE char03 VALUE 'HTM'.

    DATA:
      gv_mlrec         TYPE so_obj_nam,
      gv_sent_to_all   TYPE os_boolean,
      gv_email         TYPE adr6-smtp_addr,
      gv_subject       TYPE so_obj_des,
      gv_text          TYPE bcsy_text,
      zls_text         TYPE soli,
      xhtml_string     TYPE xstring,
      gr_send_request  TYPE REF TO cl_bcs,
      gr_bcs_exception TYPE REF TO cx_bcs,
      gr_recipient     TYPE REF TO if_recipient_bcs,
      sender           TYPE REF TO cl_cam_address_bcs,
      gr_sender        TYPE REF TO cl_sapuser_bcs,
      t_hex            TYPE solix_tab,
      gr_document      TYPE REF TO cl_document_bcs.

    DATA: lv_subject TYPE so_obj_des.

    DATA: zlv_longstring_message TYPE string.
    DATA: zlt_et_soli TYPE soli_tab.
    DATA: zls_et_soli TYPE soli.

    DATA: lv_ticketnumber TYPE string.

    LOOP AT gt_notif_email ASSIGNING FIELD-SYMBOL(<fs_email>).
      TRY.
          lv_ticketnumber = <fs_email>-ticketid.
          lv_subject = |{ lv_ticketnumber } Proposed Solution Status, Need your Attention|.
          "Create send request
          gr_send_request = cl_bcs=>create_persistent( ).

          "Email FROM...
*          gv_email = 'badli.husaini@gmail.com'.
          sender =  cl_cam_address_bcs=>create_internet_address(
                                            's4hana@penergy.com.my' ).
*    gr_sender = cl_sapuser_bcs=>create( sy-uname ).
*    "Add sender to send request
          CALL METHOD gr_send_request->set_sender
            EXPORTING
              i_sender = sender. "gr_sender.

          "Email TO...
*      gv_email = 's4hana@penergy.com.my'.

          gv_email = <fs_email>-email.

          IF sy-mandt EQ '100'.
            gv_email = |{ <fs_email>-email }|.
          ENDIF.
          gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
          "Add recipient to send request
          CALL METHOD gr_send_request->add_recipient
            EXPORTING
              i_recipient = gr_recipient
              i_express   = 'X'.

          CONCATENATE '</body></html>'
          '<p>Dear User,</p>'
          '<p>Your ticket: ' lv_ticketnumber ' logged with the ABC Support Team is now at <strong>&ldquo;Proposed Solution&rdquo;</strong> status.</p>'
          '<p>Please confirm if the solution provided by the ABCSupport Team has solved your reported issue.</p>'
          '<p>If so, please close/confirm the ticket from the Fiori App &ldquo;My Incidents&rdquo; after you&rsquo;ve logged in to the ITSM on SAP Solution Manager Fiori.</p>'
          '<p>To access the SAP Solution Manager Fiori App &ldquo;My Incidents&rdquo;, please log in using this URL link below:</p>'
          '<p><a href="https://solman.penergy.my">https://solman.penergy.my</a></p>'
          '<p>If we do not receive your response within 5 working days, and the ticket is still in &ldquo;Proposed Solution&rdquo; status we will proceed to close the ticket at our end.</p>'
          '<p><br></p>'
          '<p>Thank you for your understanding.</p>'
          '<p><br></p>'
          '<p>Best regards,</p>'
          '<p><br></p>'
          '<p>ABC Support Team</p>'

         INTO zlv_longstring_message.

          CONCATENATE zlv_longstring_message '</body></html>' INTO zlv_longstring_message.

          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text   = zlv_longstring_message
            IMPORTING
              buffer = xhtml_string
            EXCEPTIONS
              failed = 1
              OTHERS = 2.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = xhtml_string
            TABLES
              binary_tab = t_hex.

          gr_document = cl_document_bcs=>create_document(
                          i_type    = gc_raw
                          i_hex    = t_hex
                          i_length  = '1200'
                          i_subject = lv_subject ).
          "Add document to send request
          CALL METHOD gr_send_request->set_document( gr_document ).

* set send immediately flag
          gr_send_request->set_send_immediately( 'X' ).
          "Send email
          CALL METHOD gr_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = gv_sent_to_all ).
          IF gv_sent_to_all = 'X'.
            <fs_email>-status = |X|.
*            WRITE 'Email sent!'.
          ENDIF.

          "Commit to send email
          COMMIT WORK.

          "Exception handling
        CATCH cx_bcs INTO gr_bcs_exception.
          WRITE:
            'Error!',
            'Error type:',
            gr_bcs_exception->error_type.
      ENDTRY.
    ENDLOOP.

    display( ).

  ENDMETHOD.

  METHOD display.

    DATA: lt_collector_alv TYPE        tt_notif_email,
          lo_alv           TYPE REF TO cl_salv_table,
          lx_salv_msg      TYPE REF TO cx_salv_msg.

    DATA: lr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lr_columns TYPE REF TO cl_salv_columns,
          lr_column  TYPE REF TO cl_salv_column.

    lt_collector_alv = gt_notif_email .

    TRY.
        cl_salv_table=>factory(
          IMPORTING
           r_salv_table   = lo_alv
          CHANGING
            t_table       = lt_collector_alv ).


        lr_functions = lo_alv->get_functions( ).
        lr_functions->set_default( abap_true ).
        lr_functions->set_export_spreadsheet( abap_true ).
        lr_functions->set_export_localfile( abap_true )  .

        lr_columns = lo_alv->get_columns( ).
        lr_columns->set_optimize( abap_true ).
*        set_columns( lo_alv ).

        TRY.
            lr_column = lr_columns->get_column( 'STATUS' ).
            lr_column->set_medium_text( 'Email Sent Status').
            lr_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.

        ENDTRY.
        lo_alv->display( ).

      CATCH cx_salv_msg INTO lx_salv_msg.    " ALV: General Error Class with Message
        MESSAGE lx_salv_msg TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  lcl_rpt=>main( ).
