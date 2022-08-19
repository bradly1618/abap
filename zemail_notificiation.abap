*&---------------------------------------------------------------------*
*& Report  zsolman_email_noti_ps
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsolman_email_noti_ps.

*&---------------------------------------------------------------------*

CONSTANTS:
  gc_subject TYPE so_obj_des VALUE 'ABAP Email with CL_BCS',
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

DATA: zlv_longstring_message TYPE string.
DATA: zlt_et_soli TYPE soli_tab.
DATA: zls_et_soli TYPE soli.

DATA: lv_ticketnumber TYPE string.

TRY.
    "Create send request
    gr_send_request = cl_bcs=>create_persistent( ).

    "Email FROM...

    gv_email = 'badli.husaini@gmail.com'.
    sender =  cl_cam_address_bcs=>create_internet_address(
                                      's4hana@penergy.com.my' ).
*    gr_sender = cl_sapuser_bcs=>create( sy-uname ).
*    "Add sender to send request
    CALL METHOD gr_send_request->set_sender
      EXPORTING
        i_sender = sender  . "gr_sender.

    "Email TO...
*      gv_email = 's4hana@penergy.com.my'.
    gv_email = 'badli.husaini@gmail.com'.
    gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
    "Add recipient to send request
    CALL METHOD gr_send_request->add_recipient
      EXPORTING
        i_recipient = gr_recipient
        i_express   = 'X'.

    CONCATENATE '</body></html>'
    '<p>Dear User,</p>'
    '<p>Your ticket: ' lv_ticketnumber ' logged with the GIT Support Team is now at <strong>&ldquo;Proposed Solution&rdquo;</strong> status.</p>'
    '<p>Please confirm if the solution provided by the GIT Support Team has solved your reported issue.</p>'
    '<p>If so, please close/confirm the ticket from the Fiori App &ldquo;My Incidents&rdquo; after you&rsquo;ve logged in to the ITSM on SAP Solution Manager Fiori.</p>'
    '<p>To access the SAP Solution Manager Fiori App &ldquo;My Incidents&rdquo;, please log in using this URL link below:</p>'
    '<p><a href="https://solman.penergy.my">https://solman.penergy.my</a></p>'
    '<p>If we do not receive your response within 5 working days, and the ticket is still in &ldquo;Proposed Solution&rdquo; status we will proceed to close the ticket at our end.</p>'
    '<p><br></p>'
    '<p>Thank you for your understanding.</p>'
    '<p><br></p>'
    '<p>Best regards,</p>'
    '<p><br></p>'
    '<p>GIT Support Team</p>'

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
                    i_subject = gc_subject ).
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
      WRITE 'Email sent!'.
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
