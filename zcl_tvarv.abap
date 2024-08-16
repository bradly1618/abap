CLASS zcl_tvarv DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_value
      IMPORTING
        !iv_name         TYPE simple
        !iv_value        TYPE data
      RETURNING
        VALUE(rv_result) TYPE flag .
    CLASS-METHODS get_range
      IMPORTING
        !iv_name  TYPE simple
      EXPORTING
        !er_range TYPE STANDARD TABLE .
    CLASS-METHODS get_value
      IMPORTING
        !iv_name    TYPE simple
        !iv_default TYPE data OPTIONAL
      EXPORTING
        !ev_value   TYPE data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_data,
        sign   TYPE tvarvc-sign,
        option TYPE tvarvc-opti,
        low    TYPE tvarvc-low,
        high   TYPE tvarvc-high,
      END OF ts_data .
    TYPES:
      tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_cache,
        name   TYPE tvarvc-name,
        t_data TYPE tt_data,
      END OF ts_cache .
    TYPES:
      tt_cache TYPE HASHED TABLE OF ts_cache WITH UNIQUE KEY name .

    CLASS-DATA st_cache TYPE tt_cache .

    CLASS-METHODS _get_ref
      IMPORTING
        !iv_name         TYPE simple
      RETURNING
        VALUE(rps_cache) TYPE REF TO ts_cache .
ENDCLASS.



CLASS zcl_tvarv IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARVC=>CHECK_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [--->] IV_VALUE                       TYPE        DATA
* | [<-()] RV_RESULT                      TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_value.
    CLEAR rv_result.

    DATA(lps_cache) = _get_ref( iv_name ).
    IF lps_cache->t_data[] IS NOT INITIAL AND iv_value IN lps_cache->t_data.
      rv_result = 'X'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARVC=>GET_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [<---] ER_RANGE                       TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_range.

    FREE er_range.

    DATA(lps_cache) = _get_ref( iv_name ).

    MOVE-CORRESPONDING lps_cache->t_data  TO er_range.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TVARVC=>GET_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [--->] IV_DEFAULT                     TYPE        DATA(optional)
* | [<---] EV_VALUE                       TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_value.

    ev_value = iv_default.

    DATA(lps_cache) = _get_ref( iv_name ).

    READ TABLE lps_cache->t_data ASSIGNING FIELD-SYMBOL(<s_data>) INDEX 1.
    IF sy-subrc = 0.
      ev_value = <s_data>-low.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TVARVC=>_GET_REF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SIMPLE
* | [<-()] RPS_CACHE                      TYPE REF TO TS_CACHE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_ref.
    READ TABLE st_cache REFERENCE INTO rps_cache WITH KEY
      name = iv_name.
    CHECK sy-subrc NE 0.

    INSERT VALUE #(
      name = iv_name
    ) INTO TABLE st_cache REFERENCE INTO rps_cache.

    SELECT
      sign
      opti AS option
      low
      high
      FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE rps_cache->t_data
      WHERE name = iv_name.

    LOOP AT rps_cache->t_data ASSIGNING FIELD-SYMBOL(<s_data>).
      IF <s_data>-sign IS INITIAL.
        <s_data>-sign = 'I'.
      ENDIF.
      IF <s_data>-option IS INITIAL.
        <s_data>-option = 'EQ'.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
