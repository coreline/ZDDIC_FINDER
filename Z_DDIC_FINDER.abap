*&---------------------------------------------------------------------*
*& Report Z_DDIC_FINDER
*& Поиск по ABAP-словарю
*&---------------------------------------------------------------------*
*& Сайт программы: http://abap4.ru/?p=707
*& Разработчик: admin@abap4.ru
*&---------------------------------------------------------------------*
REPORT Z_DDIC_FINDER.

TABLES: tadir, dd01l, dd01t, dd02l, dd03l, dd02t, dd04t, dd40l, dd40t.

SELECTION-SCREEN BEGIN OF BLOCK select_object WITH FRAME TITLE text-sel.
PARAMETERS f_doma TYPE flag RADIOBUTTON GROUP gr0 USER-COMMAND sel_obj.
PARAMETERS f_dtel TYPE flag RADIOBUTTON GROUP gr0.
PARAMETERS f_tabl TYPE flag RADIOBUTTON GROUP gr0 DEFAULT 'X'.
PARAMETERS f_ttyp TYPE flag RADIOBUTTON GROUP gr0.
PARAMETERS f_srng TYPE flag RADIOBUTTON GROUP gr0.
PARAMETERS f_trng TYPE flag RADIOBUTTON GROUP gr0.
SELECTION-SCREEN END OF BLOCK select_object.

SELECTION-SCREEN BEGIN OF BLOCK table WITH FRAME TITLE text-tab.
SELECT-OPTIONS s_tab_nm FOR dd40l-typename NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_tab_tx FOR dd40t-ddtext NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_tab_rt FOR dd40l-rowtype NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK table.

SELECTION-SCREEN BEGIN OF BLOCK structure WITH FRAME TITLE text-str.
SELECT-OPTIONS s_str_nm FOR dd02l-tabname NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_str_tx FOR dd02t-ddtext NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK structure.

SELECTION-SCREEN BEGIN OF BLOCK range_table WITH FRAME TITLE text-rgt.
SELECT-OPTIONS s_rgt_nm FOR dd40l-typename NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_rgt_tx FOR dd40t-ddtext NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK range_table.

SELECTION-SCREEN BEGIN OF BLOCK range_structure WITH FRAME TITLE text-rgs.
SELECT-OPTIONS s_rgs_nm FOR dd02l-tabname NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_rgs_tx FOR dd02t-ddtext NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK range_structure.

SELECTION-SCREEN BEGIN OF BLOCK range_components WITH FRAME TITLE text-rng.
SELECT-OPTIONS s_rng_en FOR dd04t-rollname NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_rng_dn FOR dd01l-domname NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_rng_dt FOR dd01l-datatype NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_rng_dl FOR dd01l-leng NO-EXTENSION.
SELECT-OPTIONS s_rng_dc FOR dd01l-decimals NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK range_components.

SELECTION-SCREEN BEGIN OF BLOCK structure_components WITH FRAME TITLE text-scm.
DEFINE scm_line.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_scm_c&1 TYPE dd03l-fieldname VISIBLE LENGTH 30.
  PARAMETERS p_scm_e&1 TYPE dd03l-rollname VISIBLE LENGTH 30.
  PARAMETERS p_scm_t&1 TYPE dd03l-datatype VISIBLE LENGTH 10.
  PARAMETERS p_scm_l&1 TYPE dd03l-leng VISIBLE LENGTH 4.
  PARAMETERS p_scm_d&1 TYPE dd03l-decimals VISIBLE LENGTH 4.
  SELECTION-SCREEN END OF LINE.
END-OF-DEFINITION.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) p_scm_c0.
SELECTION-SCREEN COMMENT 32(30) p_scm_e0.
SELECTION-SCREEN COMMENT 63(10) p_scm_t0.
SELECTION-SCREEN COMMENT 74(4) p_scm_l0.
SELECTION-SCREEN COMMENT 79(4) p_scm_d0.
SELECTION-SCREEN END OF LINE.
scm_line: 1, 2, 3, 4, 5, 6, 7, 8, 9.
SELECT-OPTIONS s_scm_cn FOR dd03l-position NO-EXTENSION.
PARAMETERS p_scm_so TYPE flag AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK structure_components.

SELECTION-SCREEN BEGIN OF BLOCK data_item_parameters WITH FRAME TITLE text-itm.
SELECT-OPTIONS s_elm_nm FOR dd04t-rollname NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_elm_tx FOR dd04t-ddtext NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_elm_tr FOR dd04t-reptext NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_elm_ts FOR dd04t-scrtext_s NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_elm_tm FOR dd04t-scrtext_m NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_elm_tl FOR dd04t-scrtext_l NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK data_item_parameters.

SELECTION-SCREEN BEGIN OF BLOCK domain_parameters WITH FRAME TITLE text-dom.
SELECT-OPTIONS s_dom_nm FOR dd01l-domname NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_dom_tx FOR dd01t-ddtext NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_dom_dt FOR dd01l-datatype NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_dom_dl FOR dd01l-leng NO-EXTENSION.
SELECT-OPTIONS s_dom_ol FOR dd01l-outputlen NO-EXTENSION.
SELECT-OPTIONS s_dom_dc FOR dd01l-decimals NO-EXTENSION.
SELECT-OPTIONS s_dom_lc FOR dd01l-lowercase NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS s_dom_sf FOR dd01l-signflag NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK domain_parameters.

SELECTION-SCREEN BEGIN OF BLOCK display_options WITH FRAME TITLE text-dis.
PARAMETERS p_langu TYPE sy-langu DEFAULT sy-langu.
PARAMETERS p_limit TYPE i DEFAULT 200.
SELECT-OPTIONS s_devcls FOR tadir-devclass NO-EXTENSION NO INTERVALS.
PARAMETERS p_as4loc TYPE as4local DEFAULT 'A' NO-DISPLAY.  " Запись активирована или создана в этой форме
PARAMETERS p_as4ver TYPE as4vers DEFAULT '0000' NO-DISPLAY. " Версия записи (не используется)
SELECTION-SCREEN END OF BLOCK display_options.

INITIALIZATION.
  PERFORM init.

AT SELECTION-SCREEN.
  PERFORM pai.

AT SELECTION-SCREEN OUTPUT.
  PERFORM pbo.

FORM init.
  PERFORM init_text.
ENDFORM.

FORM init_text.
  PERFORM set_frame_text USING 'SEL' 'ABAP-словарь'.
  PERFORM set_param_text USING f_doma 'Домен'.
  PERFORM set_param_text USING f_dtel 'Элемент данных'.
  PERFORM set_param_text USING f_tabl 'Таблица / структура'.
  PERFORM set_param_text USING f_ttyp 'Тип таблицы'.
  PERFORM set_param_text USING f_srng 'Структура диапазона'.
  PERFORM set_param_text USING f_trng 'Тип таблицы диапазона'.

  PERFORM set_frame_text USING 'DOM' 'Параметры домена'.
  PERFORM set_param_text USING s_dom_nm 'Домен'.
  PERFORM set_param_text USING s_dom_tx 'Краткое описание'.
  PERFORM set_param_text USING s_dom_dt 'Тип данных в ABAP-словаре'.
  PERFORM set_param_text USING s_dom_dl 'Длина (число знаков)'.
  PERFORM set_param_text USING s_dom_ol 'Длина вывода'.
  PERFORM set_param_text USING s_dom_dc 'Число десятичных разрядов'.
  PERFORM set_param_text USING s_dom_lc 'Разрешить строчные буквы'.
  PERFORM set_param_text USING s_dom_sf 'Просмотр знака +/-'.

  PERFORM set_frame_text USING 'ITM' 'Параметры элемента данных'.
  PERFORM set_param_text USING s_elm_nm 'Элемент данных'.
  PERFORM set_param_text USING s_elm_tx 'Краткое описание'.
  PERFORM set_param_text USING s_elm_tr 'Заголовок'.
  PERFORM set_param_text USING s_elm_ts 'Краткая метка поля'.
  PERFORM set_param_text USING s_elm_tm 'Средняя метка поля'.
  PERFORM set_param_text USING s_elm_tl 'Длинная метка поля'.

  PERFORM set_frame_text USING 'STR' 'Параметры таблицы / структуры'.
  PERFORM set_param_text USING s_str_nm 'Имя таблицы / структуры'.
  PERFORM set_param_text USING s_str_tx 'Краткое описание'.

  PERFORM set_frame_text USING 'TAB' 'Параметры типа таблицы'.
  PERFORM set_param_text USING s_tab_nm 'Имя типа таблицы'.
  PERFORM set_param_text USING s_tab_tx 'Краткое описание'.
  PERFORM set_param_text USING s_tab_rt 'Тип строки'.

  PERFORM set_frame_text USING 'RGT' 'Параметры типа таблицы диапазона'.
  PERFORM set_param_text USING s_rgt_nm 'Имя типа таблицы'.
  PERFORM set_param_text USING s_rgt_tx 'Краткое описание'.

  PERFORM set_frame_text USING 'RGS' 'Параметры структуры диапазона'.
  PERFORM set_param_text USING s_rgs_nm 'Имя структуры'.
  PERFORM set_param_text USING s_rgs_tx 'Краткое описание'.

  PERFORM set_frame_text USING 'RNG' 'Параметры LIGH/LOW-компонентов'.
  PERFORM set_param_text USING s_rng_en 'Имя элемента данных'.
  PERFORM set_param_text USING s_rng_dn 'Имя домена'.
  PERFORM set_param_text USING s_rng_dt 'Тип данных в ABAP-словаре'.
  PERFORM set_param_text USING s_rng_dl 'Длина (число знаков)'.
  PERFORM set_param_text USING s_rng_dc 'Число десятичных разрядов'.

  PERFORM set_frame_text USING 'SCM' 'Поля структуры'.
  PERFORM set_param_text USING p_scm_c0 'Имя поля'.
  PERFORM set_param_text USING p_scm_e0 'Элемент данных'.
  PERFORM set_param_text USING p_scm_t0 'Тип данных'.
  PERFORM set_param_text USING p_scm_l0 'Длина'.
  PERFORM set_param_text USING p_scm_d0 'ДесРазряды'.
  PERFORM set_param_text USING s_scm_cn 'Количество полей'.
  PERFORM set_param_text USING p_scm_so 'Строгий порядок полей'.

  PERFORM set_frame_text USING 'DIS' 'Параметры поиска'.
  PERFORM set_param_text USING p_langu 'Язык'.
  PERFORM set_param_text USING p_limit 'Макс. число совпадений'.
  PERFORM set_param_text USING s_devcls 'Пакет'.
  PERFORM set_param_text USING p_as4loc 'Статус активации объекта'.
  PERFORM set_param_text USING p_as4ver 'Версия записи'.
ENDFORM.

FORM set_frame_text USING iv_name TYPE clike
                          iv_text TYPE clike.
  LOOP AT SCREEN.
    CHECK screen-group3 EQ 'BLK'.
    CHECK iv_name EQ screen-name+2(3).
    ASSIGN (screen-name) TO FIELD-SYMBOL(<lv_frame>).
    IF sy-subrc EQ 0.
      <lv_frame> = iv_text.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM set_param_text USING i_param TYPE any
                          iv_text TYPE clike.
  DESCRIBE FIELD i_param TYPE DATA(lv_type).
  DATA(lv_range) = xsdbool( lv_type EQ 'u' ).
  IF lv_range IS INITIAL.
    ASSIGN i_param TO FIELD-SYMBOL(<l_input>).
  ELSE.
    ASSIGN COMPONENT 'LOW' OF STRUCTURE i_param TO <l_input>.
  ENDIF.
  CHECK <l_input> IS ASSIGNED.

  LOOP AT SCREEN.
    CHECK screen-name IS NOT INITIAL.
    ASSIGN (screen-name) TO FIELD-SYMBOL(<l_param>).
    CHECK sy-subrc EQ 0.
    CHECK REF #( <l_input> ) EQ REF #( <l_param> ).
    IF screen-group3 EQ 'COM'.
      <l_input> = iv_text.
      EXIT.
    ENDIF.
    DATA(lv_text) = '%_'
      && COND #( WHEN lv_range EQ abap_true THEN substring_before( val = screen-name sub = '-' ) ELSE screen-name )
      && '_%_APP_%-TEXT'.
    ASSIGN (lv_text) TO FIELD-SYMBOL(<lv_text>).
    IF sy-subrc EQ 0.
      <lv_text> = iv_text.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM pbo.
  PERFORM display_ddic_param.
ENDFORM.

FORM display_ddic_param.
  CASE abap_true.
    WHEN f_doma.
      PERFORM display_block USING 'SEL DOM DIS'.
    WHEN f_dtel.
      PERFORM display_block USING 'SEL ITM DOM DIS'.
      PERFORM enabled_field USING s_dom_tx abap_false.
    WHEN f_tabl.
      PERFORM display_block USING 'SEL STR SCM DIS'.
    WHEN f_ttyp.
      PERFORM display_block USING 'SEL TAB SCM DIS'.
    WHEN f_srng.
      PERFORM display_block USING 'SEL RGS RNG DIS'.
    WHEN f_trng.
      PERFORM display_block USING 'SEL RGT RNG DIS'.
    WHEN OTHERS.
      PERFORM display_block USING 'SEL DIS'.
  ENDCASE.
ENDFORM.

FORM display_block USING iv_blocks TYPE clike.
  DATA lv_block TYPE string.

  LOOP AT SCREEN.
    IF screen-group3 EQ 'BLK'.
      lv_block = screen-name+2(3).
    ENDIF.
    screen-active = strlen( xsdbool( iv_blocks CS lv_block ) ).
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

FORM enabled_field USING i_param TYPE any iv_value TYPE abap_bool.
  DESCRIBE FIELD i_param TYPE DATA(lv_type).
  DATA(lv_range) = xsdbool( lv_type EQ 'u' ).
  IF lv_range IS INITIAL.
    ASSIGN i_param TO FIELD-SYMBOL(<l_input>).
  ELSE.
    ASSIGN COMPONENT 'LOW' OF STRUCTURE i_param TO <l_input>.
  ENDIF.
  CHECK <l_input> IS ASSIGNED.

  LOOP AT SCREEN.
    CHECK screen-name IS NOT INITIAL.
    ASSIGN (screen-name) TO FIELD-SYMBOL(<l_param>).
    CHECK sy-subrc EQ 0.
    IF REF #( <l_input> ) EQ REF #( <l_param> ).
      screen-input = strlen( iv_value ).
      MODIFY SCREEN.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM pai.
  IF sy-ucomm EQ 'ONLI'.
    PERFORM main.
  ENDIF.
ENDFORM.

FORM main.
  CASE abap_true.
    WHEN f_ttyp. " Тип таблицы
      PERFORM search_table.
    WHEN f_tabl. " Таблица / структура
      PERFORM search_structure.
    WHEN f_trng. " Тип таблицы диапазона
      PERFORM search_range_table.
    WHEN f_srng. " Структура диапазона
      PERFORM search_range_structure.
    WHEN f_dtel. " Элемент данных
      PERFORM search_data_item.
    WHEN f_doma. " Домен
      PERFORM search_domains.
  ENDCASE.
ENDFORM.

FORM search_domains.
  CONSTANTS lc_object TYPE tadir-object VALUE 'DOMA'.
  SELECT dd01l~domname,
         dd01t~ddtext,
         tadir~devclass,
         dd01l~datatype,
         dd01l~leng,
         dd01l~outputlen,
         dd01l~decimals,
         dd01l~lowercase,
         dd01l~signflag
    UP TO @p_limit ROWS
    INTO TABLE @DATA(lt_table)
    FROM dd01l
    JOIN tadir ON tadir~obj_name EQ dd01l~domname
    LEFT JOIN dd01t ON dd01t~domname EQ dd01l~domname
                   AND dd01t~as4local EQ dd01l~as4local
                   AND dd01t~as4vers EQ dd01l~as4vers
                   AND dd01t~ddlanguage EQ @p_langu
    WHERE dd01l~as4local EQ @p_as4loc
      AND dd01l~as4vers EQ @p_as4ver
      AND dd01l~domname IN @s_dom_nm
      AND dd01t~ddtext IN @s_dom_tx
      AND dd01l~datatype IN @s_dom_dt
      AND dd01l~leng IN @s_dom_dl
      AND dd01l~outputlen IN @s_dom_ol
      AND dd01l~decimals IN @s_dom_dc
      AND dd01l~lowercase IN @s_dom_lc
      AND dd01l~signflag IN @s_dom_sf
      AND tadir~object EQ @lc_object
      AND tadir~devclass IN @s_devcls.

  SORT lt_table BY domname devclass.
  PERFORM display USING lt_table.
ENDFORM.

FORM search_data_item.
  CONSTANTS lc_object TYPE tadir-object VALUE 'DTEL'.
  SELECT dd04l~rollname,
         dd04t~ddtext,
         tadir~devclass,
         dd04t~reptext,
         dd04t~scrtext_s,
         dd04t~scrtext_m,
         dd04t~scrtext_l,
         dd01l~domname,
         dd01l~datatype,
         dd01l~leng,
         dd01l~outputlen,
         dd01l~decimals,
         dd01l~lowercase,
         dd01l~signflag
    UP TO @p_limit ROWS
    INTO TABLE @DATA(lt_table)
    FROM dd04l
    JOIN tadir ON tadir~obj_name EQ dd04l~rollname
    LEFT JOIN dd04t ON dd04t~rollname EQ dd04l~rollname
                   AND dd04t~as4local EQ dd04l~as4local
                   AND dd04t~as4vers EQ dd04l~as4vers
                   AND dd04t~ddlanguage EQ @p_langu
    JOIN dd01l ON dd01l~domname EQ dd04l~domname
              AND dd01l~as4local EQ dd04l~as4local
              AND dd01l~as4vers EQ dd04l~as4vers
    WHERE dd04l~as4local EQ @p_as4loc
      AND dd04l~as4vers EQ @p_as4ver
      AND dd04l~rollname IN @s_elm_nm
      AND dd04t~ddtext IN @s_elm_tx
      AND dd04t~reptext IN @s_elm_tr
      AND dd04t~scrtext_s IN @s_elm_ts
      AND dd04t~scrtext_m IN @s_elm_tm
      AND dd04t~scrtext_l IN @s_elm_tl
      AND dd01l~domname IN @s_dom_nm
      AND dd01l~datatype IN @s_dom_dt
      AND dd01l~leng IN @s_dom_dl
      AND dd01l~outputlen IN @s_dom_ol
      AND dd01l~decimals IN @s_dom_dc
      AND dd01l~lowercase IN @s_dom_lc
      AND dd01l~signflag IN @s_dom_sf
      AND tadir~object EQ @lc_object
      AND tadir~devclass IN @s_devcls.

  DATA(lv_limit) = nmax( val1 = 0 val2 = p_limit - lines( lt_table ) ).
  IF p_limit EQ 0 OR lv_limit GT 0.
    SELECT dd04t~rollname,
           dd04t~ddtext,
           tadir~devclass,
           dd04t~reptext,
           dd04t~scrtext_s,
           dd04t~scrtext_m,
           dd04t~scrtext_l,
           dd04l~domname,
           dd04l~datatype,
           dd04l~leng,
           dd04l~outputlen,
           dd04l~decimals,
           dd04l~lowercase,
           dd04l~signflag
      UP TO @lv_limit ROWS
      APPENDING TABLE @lt_table
      FROM dd04l
      JOIN tadir ON tadir~obj_name EQ dd04l~rollname
      LEFT JOIN dd04t ON dd04t~rollname EQ dd04l~rollname
                     AND dd04t~as4local EQ dd04l~as4local
                     AND dd04t~as4vers EQ dd04l~as4vers
                     AND dd04t~ddlanguage EQ @p_langu
      WHERE dd04l~as4local EQ @p_as4loc
        AND dd04l~as4vers EQ @p_as4ver
        AND dd04t~rollname IN @s_elm_nm
        AND dd04t~ddtext IN @s_elm_tx
        AND dd04t~reptext IN @s_elm_tr
        AND dd04t~scrtext_s IN @s_elm_ts
        AND dd04t~scrtext_m IN @s_elm_tm
        AND dd04t~scrtext_l IN @s_elm_tl
        AND dd04l~domname EQ @space
        AND dd04l~datatype IN @s_dom_dt
        AND dd04l~leng IN @s_dom_dl
        AND dd04l~outputlen IN @s_dom_ol
        AND dd04l~decimals IN @s_dom_dc
        AND dd04l~lowercase IN @s_dom_lc
        AND dd04l~signflag IN @s_dom_sf
        AND tadir~object EQ @lc_object
        AND tadir~devclass IN @s_devcls
        AND dd04l~rollname NE @space.
  ENDIF.

  SORT lt_table BY rollname devclass.
  PERFORM display USING lt_table.
ENDFORM.

FORM search_structure.
  CONSTANTS lc_object TYPE tadir-object VALUE 'TABL'.
  DATA lv_join TYPE string.
  DATA lv_where TYPE string.

  SELECT dd03l~tabname,
         dd02t~ddtext,
         tadir~devclass,
         dd03l~position,
         dd03l~fieldname,
         dd03t~ddtext AS fieldname_ddtext,
         dd03l~rollname,
         dd03l~domname,
         dd03l~datatype,
         dd03l~leng,
         dd03l~decimals
    UP TO 1 ROWS
    INTO TABLE @DATA(lt_table)
    FROM dd03l
    LEFT JOIN dd02t ON dd02t~tabname EQ dd03l~tabname
                   AND dd02t~as4local EQ dd03l~as4local
                   AND dd02t~as4vers EQ dd03l~as4vers
                   AND dd02t~ddlanguage EQ @p_langu
    LEFT JOIN dd03t ON dd03t~tabname EQ dd03l~tabname
                   AND dd03t~as4local EQ dd03l~as4local
                   AND dd03t~fieldname EQ dd03l~fieldname
                   AND dd03t~ddlanguage EQ @p_langu
    JOIN tadir ON tadir~obj_name EQ dd03l~tabname.
  CLEAR lt_table.

  PERFORM get_struct_clause CHANGING lv_join lv_where.
  DATA(lv_from) = | dd03l                                                 |
               && | LEFT JOIN dd02t ON dd02t~tabname EQ dd03l~tabname     |
               && |                AND dd02t~as4local EQ dd03l~as4local   |
               && |                AND dd02t~as4vers EQ dd03l~as4vers     |
               && |                AND dd02t~ddlanguage EQ @p_langu       |
               && | LEFT JOIN dd03t ON dd03t~tabname EQ dd03l~tabname     |
               && |                AND dd03t~as4local EQ dd03l~as4local   |
               && |                AND dd03t~fieldname EQ dd03l~fieldname |
               && |                AND dd03t~ddlanguage EQ @p_langu       |
               && | LEFT JOIN dd04t ON dd04t~rollname EQ dd03l~rollname   |
               && |                AND dd04t~as4local EQ dd03l~as4local   |
               && |                AND dd04t~as4vers EQ dd03l~as4vers     |
               && |                AND dd04t~ddlanguage EQ @p_langu       |
               && | JOIN tadir ON tadir~obj_name EQ dd03l~tabname         |
               && lv_join.
  lv_from = to_upper( lv_from ).
  lv_where = to_upper( lv_where ).

  SELECT dd03l~tabname,
         dd02t~ddtext,
         tadir~devclass,
         dd03l~position,
         dd03l~fieldname,
         COALESCE( dd03t~ddtext, dd04t~ddtext ) AS fieldname_ddtext,
         dd03l~rollname,
         dd03l~domname,
         dd03l~datatype,
         dd03l~leng,
         dd03l~decimals
    UP TO @p_limit ROWS
    APPENDING TABLE @lt_table
    FROM (lv_from)
    WHERE dd03l~as4local EQ @p_as4loc
      AND dd03l~as4vers EQ @p_as4ver
      AND dd03l~tabname IN @s_str_nm
      AND dd02t~ddtext IN @s_str_tx
      AND tadir~object EQ @lc_object
      AND tadir~devclass IN @s_devcls
      AND (lv_where).

  SORT lt_table BY tabname position.
  PERFORM display USING lt_table.
ENDFORM.

FORM search_table.
  CONSTANTS lc_object TYPE tadir-object VALUE 'TTYP'.
  DATA lv_join TYPE string.
  DATA lv_where TYPE string.

  SELECT dd40l~typename,
         dd40t~ddtext,
         dd40l~rowtype,
         tadir~devclass,
         dd03l~position,
         dd03l~fieldname,
         dd03t~ddtext AS fieldname_ddtext,
         dd03l~rollname,
         dd03l~domname,
         dd03l~datatype,
         dd03l~leng,
         dd03l~decimals
    UP TO 1 ROWS
    INTO TABLE @DATA(lt_table)
    FROM dd40l
    LEFT JOIN dd40t ON dd40t~typename EQ dd40l~typename
                   AND dd40t~as4local EQ dd40l~as4local
                   AND dd40t~ddlanguage EQ @p_langu
    JOIN dd03l  ON dd03l~tabname EQ dd40l~rowtype
               AND dd03l~as4local EQ dd40l~as4local
               AND dd03l~as4vers EQ @p_as4ver
    LEFT JOIN dd02t ON dd02t~tabname EQ dd03l~tabname
                   AND dd02t~as4local EQ dd03l~as4local
                   AND dd02t~as4vers EQ dd03l~as4vers
                   AND dd02t~ddlanguage EQ @p_langu
    LEFT JOIN dd03t ON dd03t~tabname EQ dd03l~tabname
                   AND dd03t~as4local EQ dd03l~as4local
                   AND dd03t~fieldname EQ dd03l~fieldname
                   AND dd03t~ddlanguage EQ @p_langu
    JOIN tadir ON tadir~obj_name EQ dd40l~typename.
  CLEAR lt_table.

  PERFORM get_struct_clause CHANGING lv_join lv_where.
  DATA(lv_from) = | dd40l                                                    |
               && | LEFT JOIN dd40t ON dd40t~typename EQ dd40l~typename      |
               && |                AND dd40t~as4local EQ dd40l~as4local      |
               && |                AND dd40t~ddlanguage EQ @p_langu          |
               && | JOIN dd03l  ON dd03l~tabname EQ dd40l~rowtype            |
               && |            AND dd03l~as4local EQ dd40l~as4local          |
               && |            AND dd03l~as4vers EQ @p_as4ver                |
               && | LEFT JOIN dd02t ON dd02t~tabname EQ dd03l~tabname        |
               && |                AND dd02t~as4local EQ dd03l~as4local      |
               && |                AND dd02t~as4vers EQ dd03l~as4vers        |
               && |                AND dd02t~ddlanguage EQ @p_langu          |
               && | LEFT JOIN dd03t ON dd03t~tabname EQ dd03l~tabname        |
               && |                AND dd03t~as4local EQ dd03l~as4local      |
               && |                AND dd03t~fieldname EQ dd03l~fieldname    |
               && |                AND dd03t~ddlanguage EQ @p_langu          |
               && | LEFT JOIN dd04t ON dd04t~rollname EQ dd03l~rollname   |
               && |                AND dd04t~as4local EQ dd03l~as4local   |
               && |                AND dd04t~as4vers EQ dd03l~as4vers     |
               && |                AND dd04t~ddlanguage EQ @p_langu       |
               && | JOIN tadir ON tadir~obj_name EQ dd40l~typename            |
               && lv_join.
  lv_from = to_upper( lv_from ).
  lv_where = to_upper( lv_where ).

  SELECT dd40l~typename,
         dd40t~ddtext,
         dd40l~rowtype,
         tadir~devclass,
         dd03l~position,
         dd03l~fieldname,
         COALESCE( dd03t~ddtext, dd04t~ddtext ) AS fieldname_ddtext,
         dd03l~rollname,
         dd03l~domname,
         dd03l~datatype,
         dd03l~leng,
         dd03l~decimals
    UP TO @p_limit ROWS
    APPENDING TABLE @lt_table
    FROM (lv_from)
    WHERE dd40l~as4local EQ @p_as4loc
      AND dd40l~typename IN @s_tab_nm
      AND dd40l~rowtype IN @s_tab_rt
      AND dd40t~ddtext IN @s_tab_tx
      AND tadir~object EQ @lc_object
      AND tadir~devclass IN @s_devcls
      AND (lv_where).

  SORT lt_table BY typename position.
  PERFORM display USING lt_table.
ENDFORM.

FORM get_struct_clause CHANGING ev_from TYPE clike
                                ev_where TYPE clike.
  DATA lr_fieldname TYPE RANGE OF dd03l-fieldname.
  DATA lr_rollname TYPE RANGE OF dd03l-rollname.
  DATA lr_datatype TYPE RANGE OF dd03l-datatype.
  DATA lr_leng TYPE RANGE OF dd03l-leng.
  DATA lr_decimals TYPE RANGE OF dd03l-decimals.
  DATA lv_position TYPE dd03l-position.

  CLEAR: ev_from, ev_where.

  DO 9 TIMES.
    lv_position = sy-index.
    PERFORM get_struct_param USING 'P_SCM_C' lv_position CHANGING lr_fieldname[].
    PERFORM get_struct_param USING 'P_SCM_E' lv_position CHANGING lr_rollname[].
    PERFORM get_struct_param USING 'P_SCM_T' lv_position CHANGING lr_datatype[].
    PERFORM get_struct_param USING 'P_SCM_L' lv_position CHANGING lr_leng[].
    PERFORM get_struct_param USING 'P_SCM_D' lv_position CHANGING lr_decimals[].
    CHECK lr_fieldname IS NOT INITIAL
       OR lr_rollname IS NOT INITIAL
       OR lr_datatype IS NOT INITIAL
       OR lr_leng IS NOT INITIAL
       OR lr_decimals IS NOT INITIAL.
    DATA(lv_tab) = |dd03l_{ lv_position }|.
    DATA(lv_join) = | LEFT JOIN dd03l AS { lv_tab }                             |
                 && |                 ON { lv_tab }~tabname EQ dd03l~tabname    |
                 && |                AND { lv_tab }~as4local EQ dd03l~as4local  |
                 && |                AND { lv_tab }~as4vers EQ dd03l~as4vers    |.
    IF p_scm_so EQ abap_true.
      lv_join = lv_join && | AND { lv_tab }~POSITION EQ '{ lv_position }'       |.
    ENDIF.
    ev_from = ev_from && lv_join.

    DATA(lt_range) = VALUE rsds_trange( (
        tablename = lv_tab
        frange_t = VALUE rsds_frange_t(
         ( fieldname = lv_tab && '~FIELDNAME' selopt_t = CORRESPONDING #( lr_fieldname ) )
         ( fieldname = lv_tab && '~ROLLNAME'  selopt_t = CORRESPONDING #( lr_rollname ) )
         ( fieldname = lv_tab && '~DATATYPE'  selopt_t = CORRESPONDING #( lr_datatype ) )
         ( fieldname = lv_tab && '~LENG'      selopt_t = CORRESPONDING #( lr_leng ) )
         ( fieldname = lv_tab && '~DECIMALS'  selopt_t = CORRESPONDING #( lr_decimals ) )
        )
    ) ).
    DATA(lt_where) = VALUE rsds_twhere( ).

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_range
      IMPORTING
        where_clauses = lt_where.

    IF line_exists( lt_where[ tablename = lv_tab ] ).
      ev_where = ev_where && concat_lines_of( table = lt_where[ tablename = lv_tab ]-where_tab sep = space ).
      ev_where = ev_where && | AND |.
    ENDIF.
  ENDDO.

  IF s_scm_cn[] IS NOT INITIAL.
    DATA(lv_min) = VALUE dd03l-position( ).
    DATA(lv_max) = VALUE dd03l-position( ).
    DO 999 TIMES.
      CHECK sy-index IN s_scm_cn.
      IF lv_min IS INITIAL.
        lv_min = sy-index.
      ENDIF.
      lv_max = sy-index + 1.
    ENDDO.
    ev_where = ev_where && | EXISTS (                                |
                        && |   SELECT tabname                        |
                        && |     FROM dd03l AS dd03l_min             |
                        && |     WHERE tabname EQ dd03l~tabname      |
                        && |       AND as4local EQ dd03l~as4local    |
                        && |       AND as4vers EQ dd03l~as4vers      |
                        && |       AND position EQ '{ lv_min }'      |
                        && |  ) AND                                  |.
    ev_where = ev_where && | NOT EXISTS (                            |
                        && |   SELECT tabname                        |
                        && |     FROM dd03l AS dd03l_max             |
                        && |     WHERE tabname EQ dd03l~tabname      |
                        && |       AND as4local EQ dd03l~as4local    |
                        && |       AND as4vers EQ dd03l~as4vers      |
                        && |       AND position EQ '{ lv_max }'      |
                        && |  ) AND                                  |.
  ENDIF.

  ev_where = ev_where && | dd03l~tabname IS NOT NULL|.
ENDFORM.

FORM get_struct_param USING iv_name TYPE clike
                            iv_num TYPE clike
                      CHANGING et_range TYPE STANDARD TABLE.
  DATA(lv_num) = CONV i( iv_num ).
  CLEAR et_range[].
  DATA(lv_name) = |{ iv_name }{ lv_num }|.
  ASSIGN (lv_name) TO FIELD-SYMBOL(<lv_value>).
  ASSERT sy-subrc EQ 0.
  CHECK <lv_value> IS NOT INITIAL.
  DATA(lv_option) = COND #( WHEN <lv_value> CA '+*' THEN rs_c_range_opt-pattern ELSE rs_c_range_opt-equal ).
  APPEND |{ rs_c_range_sign-including }{ lv_option }{ <lv_value> }| TO et_range.
ENDFORM.

FORM search_range_structure.
  CONSTANTS lc_object TYPE tadir-object VALUE 'TABL'.
  SELECT dd02l~tabname,
         dd02t~ddtext,
         tadir~devclass,
         dd03l_low~rollname,
         dd03l_low~domname,
         dd03l_low~datatype,
         dd03l_low~leng,
         dd03l_low~decimals
    UP TO @p_limit ROWS
    INTO TABLE @DATA(lt_table)
    FROM dd02l
    JOIN tadir ON tadir~obj_name EQ dd02l~tabname
    JOIN dd03l AS dd03l_sign ON dd03l_sign~tabname EQ dd02l~tabname
                            AND dd03l_sign~as4local EQ dd02l~as4local
                            AND dd03l_sign~as4vers EQ dd02l~as4vers
                            AND dd03l_sign~position EQ '0001'
    JOIN dd03l AS dd03l_opt ON dd03l_opt~tabname EQ dd02l~tabname
                           AND dd03l_opt~as4local EQ dd02l~as4local
                           AND dd03l_opt~as4vers EQ dd02l~as4vers
                           AND dd03l_opt~position EQ '0002'
    JOIN dd03l AS dd03l_low ON dd03l_low~tabname EQ dd02l~tabname
                           AND dd03l_low~as4local EQ dd02l~as4local
                           AND dd03l_low~as4vers EQ dd02l~as4vers
                           AND dd03l_low~position EQ '0003'
    JOIN dd03l AS dd03l_high ON dd03l_high~tabname EQ dd02l~tabname
                            AND dd03l_high~as4local EQ dd02l~as4local
                            AND dd03l_high~as4vers EQ dd02l~as4vers
                            AND dd03l_high~position EQ '0004'
                            AND dd03l_high~rollname EQ dd03l_low~rollname
                            AND dd03l_high~inttype EQ dd03l_low~inttype
                            AND dd03l_high~intlen EQ dd03l_low~intlen
                            AND dd03l_high~domname EQ dd03l_low~domname
                            AND dd03l_high~datatype EQ dd03l_low~datatype
                            AND dd03l_high~leng EQ dd03l_low~leng
    LEFT JOIN dd02t ON dd02t~tabname EQ dd02l~tabname
                   AND dd02t~as4local EQ dd02l~as4local
                   AND dd02t~as4vers EQ dd02l~as4vers
                   AND dd02t~ddlanguage EQ @p_langu
    WHERE dd02l~as4local EQ @p_as4loc
      AND dd02l~as4vers EQ @p_as4ver
      AND dd02l~tabname IN @s_rgs_nm
      AND dd02t~ddtext IN @s_rgs_tx
      AND dd03l_sign~fieldname EQ 'SIGN'
      AND dd03l_sign~datatype EQ 'CHAR'
      AND dd03l_sign~leng EQ '000001'
      AND dd03l_opt~fieldname EQ 'OPTION'
      AND dd03l_opt~datatype EQ 'CHAR'
      AND dd03l_opt~leng EQ '000002'
      AND dd03l_low~fieldname EQ 'LOW'
      AND dd03l_high~fieldname EQ 'HIGH'
      AND dd03l_low~rollname IN @s_rng_en
      AND dd03l_low~domname IN @s_rng_dn
      AND dd03l_low~datatype IN @s_rng_dt
      AND dd03l_low~leng IN @s_rng_dl
      AND dd03l_low~decimals IN @s_rng_dc
      AND tadir~object EQ @lc_object
      AND tadir~devclass IN @s_devcls
      AND NOT EXISTS (
        SELECT tabname
          FROM dd03l
          WHERE tabname EQ dd02l~tabname
            AND as4local EQ dd02l~as4local
            AND as4vers EQ dd02l~as4vers
            AND position NOT IN ('0001', '0002', '0003', '0004')
      ).

  SORT lt_table BY tabname devclass.
  PERFORM display USING lt_table.
ENDFORM.

FORM search_range_table.
  CONSTANTS lc_object TYPE tadir-object VALUE 'TTYP'.
  SELECT dd40l~typename,
         dd40t~ddtext,
         tadir~devclass,
         dd40l~rowtype,
         dd03l_low~rollname,
         dd03l_low~domname,
         dd03l_low~datatype,
         dd03l_low~leng,
         dd03l_low~decimals
    UP TO @p_limit ROWS
    INTO TABLE @DATA(lt_table)
    FROM dd40l
    JOIN tadir ON tadir~obj_name EQ dd40l~typename
    JOIN dd02l ON dd02l~tabname EQ dd40l~rowtype
              AND dd02l~as4local EQ dd40l~as4local
    JOIN dd03l AS dd03l_sign ON dd03l_sign~tabname EQ dd02l~tabname
                            AND dd03l_sign~as4local EQ dd02l~as4local
                            AND dd03l_sign~as4vers EQ dd02l~as4vers
                            AND dd03l_sign~position EQ '0001'
    JOIN dd03l AS dd03l_opt ON dd03l_opt~tabname EQ dd02l~tabname
                           AND dd03l_opt~as4local EQ dd02l~as4local
                           AND dd03l_opt~as4vers EQ dd02l~as4vers
                           AND dd03l_opt~position EQ '0002'
    JOIN dd03l AS dd03l_low ON dd03l_low~tabname EQ dd02l~tabname
                           AND dd03l_low~as4local EQ dd02l~as4local
                           AND dd03l_low~as4vers EQ dd02l~as4vers
                           AND dd03l_low~position EQ '0003'
    JOIN dd03l AS dd03l_high ON dd03l_high~tabname EQ dd02l~tabname
                            AND dd03l_high~as4local EQ dd02l~as4local
                            AND dd03l_high~as4vers EQ dd02l~as4vers
                            AND dd03l_high~position EQ '0004'
                            AND dd03l_high~rollname EQ dd03l_low~rollname
                            AND dd03l_high~inttype EQ dd03l_low~inttype
                            AND dd03l_high~intlen EQ dd03l_low~intlen
                            AND dd03l_high~domname EQ dd03l_low~domname
                            AND dd03l_high~datatype EQ dd03l_low~datatype
                            AND dd03l_high~leng EQ dd03l_low~leng
    LEFT JOIN dd40t ON dd40t~typename EQ dd40l~typename
                   AND dd40t~as4local EQ dd40l~as4local
                   AND dd40t~ddlanguage EQ @p_langu
    WHERE dd40l~as4local EQ @p_as4loc
      AND dd40l~typename IN @s_rgt_nm
      AND dd02l~as4vers EQ @p_as4ver
      AND dd40t~ddtext IN @s_rgt_tx
      AND dd03l_sign~fieldname EQ 'SIGN'
      AND dd03l_sign~datatype EQ 'CHAR'
      AND dd03l_sign~leng EQ '000001'
      AND dd03l_opt~fieldname EQ 'OPTION'
      AND dd03l_opt~datatype EQ 'CHAR'
      AND dd03l_opt~leng EQ '000002'
      AND dd03l_low~fieldname EQ 'LOW'
      AND dd03l_high~fieldname EQ 'HIGH'
      AND dd03l_low~rollname IN @s_rng_en
      AND dd03l_low~domname IN @s_rng_dn
      AND dd03l_low~datatype IN @s_rng_dt
      AND dd03l_low~leng IN @s_rng_dl
      AND dd03l_low~decimals IN @s_rng_dc
      AND tadir~object EQ @lc_object
      AND tadir~devclass IN @s_devcls
      AND NOT EXISTS (
        SELECT tabname
          FROM dd03l
          WHERE tabname EQ dd02l~tabname
            AND as4local EQ dd02l~as4local
            AND as4vers EQ dd02l~as4vers
            AND position NOT IN ('0001', '0002', '0003', '0004')
      ).

  SORT lt_table BY typename devclass rowtype.
  PERFORM display USING lt_table.
ENDFORM.

FORM display USING it_table TYPE STANDARD TABLE.
  STATICS lo_salv TYPE REF TO cl_salv_table.

  MESSAGE |Выбрано записей: { lines( it_table ) }| TYPE rs_c_success.
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_salv
        CHANGING
          t_table      = it_table.

      READ TABLE lo_salv->get_columns( )->get( ) INTO DATA(ls_key) INDEX 1.
      IF sy-subrc EQ 0.
        CAST cl_salv_column_list( ls_key-r_column )->set_key( abap_true ).
      ENDIF.
      LOOP AT lo_salv->get_columns( )->get( ) INTO DATA(ls_sort).
        lo_salv->get_sorts( )->add_sort( columnname = ls_sort-r_column->get_columnname( ) ).
        IF ls_sort-r_column->get_columnname( ) EQ 'DEVCLASS'.
          EXIT.
        ENDIF.
      ENDLOOP.

      lo_salv->get_columns( )->set_optimize( abap_true ).
      lo_salv->get_functions( )->set_all( abap_true ).
      lo_salv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_salv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv TYPE rs_c_info DISPLAY LIKE rs_c_error.
  ENDTRY.
ENDFORM.