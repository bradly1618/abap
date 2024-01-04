*Using TVARC and used it as selection parameter.


  DATA lt_param_sf TYPE STANDARD TABLE OF tvarvc.
      get_tvarc(
         EXPORTING
           iv_name  = 'Z_OTCIN_VAT_CALCULATE'
         IMPORTING
           et_param = lt_param_sf
       ).

      gr_pstyv  = VALUE ty_pstyv( FOR ls IN lt_param_sf ( sign = ls-sign
                                                          option = ls-opti
                                                          low    = ls-low
                                                          high   = ls-high ) ).

" This is code snippet, need to copy paste to you own class to make it work
    CLASS-METHODS:

      get_tvarc IMPORTING iv_name  TYPE rvari_vnam
                EXPORTING et_param TYPE ty_tvarc

METHOD get_tvarvc

  DATA lt_param_sf TYPE STANDARD TABLE OF tvarvc.

  SELECT  * FROM tvarvc
     INTO TABLE lt_param_sf
    WHERE name = iv_name
    AND type = 'S'
    .
  IF sy-subrc EQ 0.
    et_param = lt_param_sf.
  ENDIF.

ENDMETHOD 
