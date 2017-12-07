FUNCTION zauth.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_UNAME) TYPE  SY-UNAME OPTIONAL
*"     VALUE(P_MATNR) TYPE  CHECKBOX OPTIONAL
*"     VALUE(P_PRODH) TYPE  CHECKBOX OPTIONAL
*"     VALUE(P_VKBUR) TYPE  CHECKBOX OPTIONAL
*"     VALUE(P_VKGRP) TYPE  CHECKBOX OPTIONAL
*"  EXPORTING
*"     VALUE(E_PRODH) TYPE  CHAR1
*"     VALUE(E_VKBUR) TYPE  CHAR1
*"     VALUE(E_VKGRP) TYPE  CHAR1
*"     VALUE(IR_PRODH) TYPE  ZUI_T_RANGE_PRDHA
*"  TABLES
*"      ET_T179T STRUCTURE  T179T OPTIONAL
*"      ET_MARA STRUCTURE  MARA OPTIONAL
*"      ET_KNVV STRUCTURE  KNVV OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
**"----------------------------------------------------------------------
  DATA : BEGIN OF lt_t179t OCCURS 0,
          prodh LIKE t179t-prodh,
          vtext LIKE t179t-vtext,
       END OF lt_t179t.

  DATA : BEGIN OF lt_mara OCCURS 0,
            matnr LIKE mara-matnr,
            prdha LIKE mara-prdha,
         END OF lt_mara.

  DATA : BEGIN OF lt_knvv OCCURS 0,
            kunnr LIKE knvv-kunnr,
            vkbur LIKE knvv-vkbur,
            vkgrp LIKE knvv-vkgrp,
         END OF lt_knvv.

  DATA lt_prodh TYPE TABLE OF zui_auth_prodh WITH HEADER LINE.
  DATA lt_vk TYPE TABLE OF zui_auth_vk WITH HEADER LINE.
  DATA ls_vk LIKE LINE OF lt_vk.
  DATA:  ls_prodh LIKE LINE OF lt_prodh,
         is_prodh LIKE LINE OF ir_prodh,
         es_t179t LIKE LINE OF et_t179t,
         ls_t179t LIKE LINE OF lt_t179t,
         ls_mara  LIKE LINE OF lt_mara,
         es_mara  LIKE LINE OF et_mara,
         ls_knvv  LIKE LINE OF lt_knvv,
         es_knvv  LIKE LINE OF et_knvv.

*// aktifken statuler degisiyor.

  SELECT
  uname
  prodh
  FROM zui_auth_prodh
  INTO CORRESPONDING FIELDS OF TABLE lt_prodh
  WHERE uname = i_uname.


  IF sy-subrc NE 0.
    e_prodh = 'Z'.
  ELSE.
    READ TABLE lt_prodh INTO ls_prodh WITH KEY prodh = 'ALL'.
    IF sy-subrc EQ 0.
      e_prodh = 'A'.
    ELSE.
      e_prodh = 'P'.
    ENDIF.
  ENDIF.

  SELECT
   uname
   vkbur
   vkgrp
  FROM zui_auth_vk
  INTO CORRESPONDING FIELDS OF TABLE lt_vk
  WHERE uname = i_uname.

  IF sy-subrc NE 0.
    e_vkbur = 'Z'.
    e_vkgrp = 'Z'.
  ELSE.

    READ TABLE lt_vk INTO ls_vk WITH KEY vkbur = 'ALL'.
    IF sy-subrc EQ 0.
      e_vkbur = 'A'.
    ELSE.
      e_vkbur = 'P'.
    ENDIF.
    READ TABLE lt_vk INTO ls_vk WITH KEY vkgrp = 'ALL'.
    IF sy-subrc EQ 0.
      e_vkgrp = 'A'.
    ELSE.
      e_vkgrp = 'P'.
    ENDIF.
  ENDIF.
  "
  IF lt_vk[] IS NOT INITIAL.

    IF p_vkgrp = 'X' AND e_vkgrp = 'A' AND e_vkbur = 'P'.
      SELECT
       kunnr
       vkbur
       vkgrp
      FROM knvv
      INTO CORRESPONDING FIELDS OF TABLE lt_knvv
      FOR ALL ENTRIES IN lt_vk
      WHERE vkbur = lt_vk-vkbur.
    ELSEIF p_vkgrp = 'X' AND e_vkgrp EQ 'P'. "ne A
      SELECT
       kunnr
       vkbur
       vkgrp
      FROM knvv
      INTO CORRESPONDING FIELDS OF TABLE lt_knvv
      FOR ALL ENTRIES IN lt_vk
      WHERE vkgrp = lt_vk-vkgrp
      AND   vkbur = lt_vk-vkbur.
    ENDIF.

    IF p_vkbur = 'X' AND e_vkgrp = 'A' AND e_vkbur = 'P'.
      SELECT
      kunnr
      vkbur
      vkgrp
      FROM knvv
      APPENDING CORRESPONDING FIELDS OF TABLE lt_knvv
      FOR ALL ENTRIES IN lt_vk
      WHERE vkbur = lt_vk-vkbur.
"XMD
    ELSEIF p_vkbur = 'X' AND e_vkgrp EQ 'P' AND e_vkbur = 'P'.

      SELECT
      kunnr
      vkbur
      vkgrp
      FROM knvv
      APPENDING CORRESPONDING FIELDS OF TABLE lt_knvv
      FOR ALL ENTRIES IN lt_vk
      WHERE vkbur = lt_vk-vkbur
      AND   vkgrp = lt_vk-vkgrp.

    ENDIF.
  ENDIF.
  SORT lt_knvv BY kunnr vkbur vkgrp.
  DELETE ADJACENT DUPLICATES FROM lt_knvv COMPARING ALL FIELDS.


  LOOP AT lt_prodh INTO ls_prodh.
    IF ls_prodh-prodh CA '*'.
      is_prodh-sign   = 'I'.
      is_prodh-option = 'CP'.
      is_prodh-low    = ls_prodh-prodh.
      APPEND is_prodh TO ir_prodh.
    ELSE.
      is_prodh-sign   = 'I'.
      is_prodh-option = 'EQ'.
      is_prodh-low    = ls_prodh-prodh.
      APPEND is_prodh TO ir_prodh.
    ENDIF.
  ENDLOOP.

  IF ir_prodh[] IS NOT INITIAL.

    IF p_prodh = 'X' OR p_matnr = 'X'.
      SELECT
      prodh
      vtext
     FROM t179t
     INTO CORRESPONDING FIELDS OF TABLE lt_t179t
     WHERE prodh IN ir_prodh
     AND   spras = sy-langu.
    ENDIF.


    IF lt_t179t[] IS NOT INITIAL.
      IF p_matnr = 'X' AND e_prodh = 'P'.
        SELECT
         matnr
         prdha
        FROM mara
        INTO CORRESPONDING FIELDS OF TABLE lt_mara
        FOR ALL ENTRIES IN lt_t179t
        WHERE prdha = lt_t179t-prodh.
      ENDIF.
    ENDIF.
  ENDIF.

*  IF p_prodh = 'X'.
  LOOP AT lt_t179t INTO ls_t179t.
    es_t179t-prodh = ls_t179t-prodh.
    es_t179t-vtext = ls_t179t-vtext.
    APPEND es_t179t TO et_t179t.
    CLEAR: ls_t179t, es_t179t.
  ENDLOOP.
*  ENDIF.
  LOOP AT lt_mara INTO ls_mara.
    es_mara-matnr = ls_mara-matnr.
    es_mara-prdha = ls_mara-prdha.
    APPEND es_mara TO et_mara.
    CLEAR: ls_mara, es_mara.
  ENDLOOP.
  LOOP AT lt_knvv INTO ls_knvv.
    es_knvv-kunnr = ls_knvv-kunnr.
    es_knvv-vkbur = ls_knvv-vkbur.
    es_knvv-vkgrp = ls_knvv-vkgrp.
    APPEND es_knvv TO et_knvv.
    CLEAR: ls_knvv, es_knvv.
  ENDLOOP.


ENDFUNCTION.
