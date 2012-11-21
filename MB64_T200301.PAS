{+----------------------------------------------------------------+}
{|   'Testprogramm fÅr MB64 (FG 450.390) und MB32 (FG 450. 36x)   |}
{+----------------------------------------------------------------+}

PROGRAM mb32_tst;

{$S-,G+,B-,N+,E+,I-}


{$DEFINE noTEST}

uses Crt, Mil_Unit, display, convert;

TYPE

  t_Ctrl_Word_Byte = RECORD CASE BYTE OF
             1: ( W: WORD; );
             2: ( B: PACKED RECORD
                       ADR:  BYTE;
                       SKAL: BYTE;
                     END; );
             END;

  t_Word_Byte = RECORD CASE BYTE OF
             1: ( W: WORD; );
             2: ( B: PACKED RECORD
                       LB: BYTE;
                       HB: BYTE;
                     END; );
             END;

  t_Long_Word = PACKED RECORD
                  LW   : WORD;
                  HW   : WORD;
                END;


CONST head_txt =
 'Testprogramm fÅr MB64 (FG 450.390) und MB32 (FG 450. 36x)           [20.03.2001]';

VAR
  mil:  t_Mil_Unit;  { OBJECT: In UNIT Mil_UNIT.PAS deklariert.   }
  disp: t_disp;      { OBJECT: In UNIT Display.PAS deklariert.    }
  conv: t_convert;   { OBJECT: In UNIT Convert.PAS deklariert.    }

  answer: CHAR;
  z:      BYTE; {Zeilennummer}

  ifc_test_nr: BYTE;      { Achtung hier steht global die IFK-Nr. mit der    }
                          { getestet werden soll.                            }
  mil_err:     t_mil_err; { t_mil_err ist in UNIT Mil_Unit.Pas deklariert.   }


  mbc_param   : BYTE;    {Modulbus-Contr. fuer Skalierung, Adressen und Data I/O}
  mbc_test	  : BYTE;    {Modulbus-Contr. fuer den Test der 32/64 Bit I/O}
  mbc_virt	  : BYTE;    {Virt.-MB-Contr. fuer den Test der 32/64 Bit I/O}

  mb32_skal_adr_k0_k1 : BYTE;  {MB32 I/O fuer die Skalierung, zum testen von Kanal 0 und 1}
  mb32_skal_adr_k2_k3 : BYTE;  {MB32 I/O fuer die Skalierung, zum testen von Kanal 2 und 3}
  mb32_data_k0_k1     : BYTE;  {MB32 I/O fuer das Data I/O, zum testen von Kanal 0 und 1}
  mb32_data_k2_k3     : BYTE;  {MB32 I/O fuer das Data I/O, zum testen von Kanal 2 und 3}
  mb32_64_test		  : BYTE;  {Test-Leiterplatte MB32 oder MB64}


CONST

{ Interface-Karten-ADR. der MB-Contr.}

  c_mbc_param_adr= $02;		{Adr. des MB-Contr. fuer Skalierung, Adressen und Data I/O   }
  c_mbc_test_adr = $10; 	{ADR. des MB-Contr. zum testen der 32/64 Bit I/O }
  c_mbc_virt_adr = $11;		{virt.- ADR des MB-Contr. zum testen der 32/64 Bit I/O }


{ Modulbus-Adressen der 32bit-IOs }

  c_mb32_skal_adr_k0_k1	= 1;
  c_mb32_data_k0_k1		= 2;
  c_mb32_skal_adr_k2_k3	= 3;
  c_mb32_data_k2_k3		= 4;
  c_mb32_64_test		= 1;


{ Verschiedenes }
  c_max_word_cnt  = $FFFF;
  c_wait          = $1FFF;
  c_wait_loop     = $3FFF;
  c_wait_end      = $3FFFF;

  s = black;
  b = blue;
  r = red;
  g = yellow;
  gn = green;
  m = magenta;

  c_fc_irm       = $C9; {Funktionscode: read INR-Reg. der IFA}

  c_Be            = 2;	{Feldbegin der Textausgabe: Bezeichnung des Testes	}
  c_Bm            = 25;	{Feldbegin der Textausgabe: Bitmuster				}
  c_CR            = 63;	{Feldbegin der Textausgabe: Best‰tigung				}
  c_En            = 63;	{Feldbegin der Textausgabe: Test-Ergebnis			}

  {---------------------------------------------------------------}
  { Konstanten fuers 32-Bit-I/O Status-Register FöR Kanal 0 und 1 }
  {---------------------------------------------------------------}
  C_sts32_Mask_DRDY  = $8000; { Interupt-Maske fÅr INR DRDY (1 => Enable }
  C_sts32_Mask_DRQ   = $4000; { Interupt-Maske fÅr INR DRQ  (1 => Enable }
  C_sts32_DRDY       = $2000; { Interupt DRDY (1 => Aktiv }
  C_sts32_DRQ        = $1000; { Interupt-DRQ  (1 => Aktiv }
  C_sts32_INF        = $0800; { Int. Inp- Outputbuffer full (1 => full (MB-Seite) }
  C_sts32_EXF        = $0400; { Ext. Inp- Outputbuffer full (1 => full (APK-Seite) }
  C_sts32_OBF        = $0200; { Outputbuffer full (1 => full }
  C_sts32_IBF        = $0100; { Inputbuffe r full (1 => full }
  C_sts32_RD_SEQ_ERR = $0080; { Read Sequence-Error im 32 Bit-Mode  }
  C_sts32_WR_SEQ_ERR = $0040; { Write Sequence-Error im 32 Bit-Mode }
  C_sts32_FREI_BIT_5 = $0020; { Frei }
  C_sts32_FREI_BIT_4 = $0010; { Frei }
  C_sts32_FREI_BIT_3 = $0008; { Frei }
  C_sts32_RD_ID_ERR  = $0004; { Error beim lesen der APK-ID }
  C_sts32_RD_ERR     = $0002; { Read-Error  }
  C_sts32_WR_ERR     = $0001; { Write-Error }



  VAR
    ifc_online_str : t_ifc_online_str;
    mb32_ctrl_data : t_Word_Byte;


PROCEDURE set_mb32t_skal ( mbc_adr, mb32_adr : Byte;
                           mb32_io_skal, mb32_test_skal : WORD;
                           VAR mil_error: t_mil_err);
  BEGIN

  {Sete Skalierung der 'mb32_data_k0_k1' ueber Kanal 0 der 'mb32_skal_adr_k0_k1'}

  mil.modulbus_adr_wr(NOT mb32_io_skal, c_io32_kanal_0,
                       mb32_adr, mbc_adr, mil_error);


  {Sete Skalierung der 'mb32_64_test' ueber Kanal 1 der 'mb32_skal_adr_k0_k1'}

  mil.modulbus_adr_wr(NOT mb32_test_skal, c_io32_kanal_1,
                       mb32_adr, mbc_adr, mil_error);
  END;



PROCEDURE clr_mb32(mbc_adr, mod_adr : Byte; VAR mil_error: t_mil_err);

  BEGIN   {Reset alle Flag's (Error) der MB32_tst}
  mil.modulbus_adr_wr($0000, c_io32_reset, mod_adr, mbc_adr, mil_error);
  END;



PROCEDURE mb32t_err_wait(VAR err_count:BYTE);

  VAR
    answer:      CHAR;

  BEGIN
    err_count := err_count+1;
    disp.w(c_CR,z,r,'weiter mit <CR>');
    keypressed;
    readkey;
    mil.timer2_wait(c_wait);
  END;



PROCEDURE set_bitm_str ( VAR comp_str: t_mb_str );
  BEGIN
  Mil.clear_mb_str(comp_str);
  WITH comp_str.arr[mb32_skal_adr_k0_k1] DO BEGIN
    Card_Conf_Stat := [Card_Conf_Ok];
    Adr.r.ADR      := c_mb32_skal_adr_k0_k1;
    Adr.r.SCAL     := C_io32_K0_Out_K1_Out
                    + {C_io32_16bit_032bit}
                    + C_io32_K1_M_Standard
                    + C_io32_K0_M_Standard;
    VG_ID          := C_io32_ID;
    Mod_ID         := C_io32_ID;
    EPLD.w         := 0;
    END;

  WITH comp_str.arr[mb32_data_k0_k1] DO BEGIN
    Card_Conf_Stat := [Card_Conf_Ok];
    Adr.r.ADR      := mb32_data_k0_k1;
    Adr.r.SCAL     := C_io32_K0_In_K1_In
                    + C_io32_16bit_032bit
                    + C_io32_K1_M_Standard
                    + C_io32_K0_M_Standard;
    VG_ID          := C_io32_ID;
    Mod_ID         := C_io32_ID;
    EPLD.w         := 0;
    END;

  Comp_Str.Card_Cnt:= 2;
  END;


PROCEDURE set_mb32t_str ( VAR comp_str: t_mb_str );
  BEGIN
  Mil.clear_mb_str(comp_str);
  WITH comp_str.arr[mb32_skal_adr_k0_k1] DO BEGIN
    Card_Conf_Stat := [Card_Conf_Ok];
    Adr.r.ADR      := mb32_skal_adr_k0_k1;
    Adr.r.SCAL     := C_io32_K0_Out_K1_Out
                    + C_io32_16bit_032bit
                    + C_io32_K1_M_Standard
                    + C_io32_K0_M_Standard;
    VG_ID          := C_io32_ID;
    Mod_ID         := C_io32_ID;
    EPLD.w         := 0;
    END;

  Comp_Str.Card_Cnt:= 1;

  END;

PROCEDURE set_mb32t_apk_str ( VAR comp_str: t_mb_str );
  BEGIN
  Mil.clear_mb_str(comp_str);
  WITH comp_str.arr[mb32_data_k0_k1] DO BEGIN
    Card_Conf_Stat := [Card_Conf_Ok];
    Adr.r.ADR      := mb32_data_k0_k1;
    Adr.r.SCAL     := C_io32_K0_Out_K1_Out
                    + C_io32_16bit_032bit
                    + C_io32_K1_M_Standard
                    + C_io32_K0_M_Standard;
    VG_ID          := C_io32_ID;
    Mod_ID         := C_io32_ID;
    EPLD.w         := 0;
{   APK1.APK_ID    := C_io32_APK_BDU3;
    APK0.APK_ID    := C_io32_APK_BDU1;}
    END;

  Comp_Str.Card_Cnt:= 1;
  END;





PROCEDURE autom_konfig_tst;
  TYPE
    t_hb_lw = RECORD CASE BYTE OF
                1: ( r : PACKED RECORD
                           lw   : WORD;
                           hw   : WORD;
                         END;
                    );
                2: ( con: LONGINT; );
              END;
    t_korrektur =  t_hb_lw;
    t_delta =      t_hb_lw;

    t_test_status = ( delta_ok, delta_not_ok, mil_err_occur,
                      no_ifc_211_fg, modul_bus_err );


  VAR
    x, y: BYTE;
    MB_Str: t_MB_Str;
    Comp_Str: t_mb_str;
    Comp_Result: t_Comp_Result;
    set_ifc_adr_stat: t_set_ifc_adr_stat;
    i: INTEGER;
    ifc_type: t_ifc_type;
    mb_cntrl_adr: BYTE;
    ifc_adr: BYTE;
    IFK_211_Istmode: t_ifc_type;
    input_ok: BOOLEAN;
    text_farbe: BYTE;
    mil_error: t_mil_err;

  BEGIN

  mil_error := [];


  {Set Skal. 'mb32_data_k0_k1' öber Kanal 0 von 'mb32_skal_adr_k0_k1'}

  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, c_mbc_param_adr, mil_error);


  {Set Skal. 'mb32_64_test'(Kanal 0+1) Åber Kanal 1 von 'mb32_skal_adr_k0_k1'}

  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, c_mbc_param_adr, mil_error);











  disp.ini_bot_w;
  disp.wr_top_w(10,2,' Automatischer Konfigurations-Test');
  clrscr;
  mil.pc_mil_res;


  disp.w(1,1,s,'Dieser Test braucht');
  disp.w(0,0,r,' zwei');
  disp.w(0,0,s,' Modulbuscontroller (ADR = ');
  disp.w(0,0,r, conv.hex_byte(mbc_test) );
  disp.w(0,0,s,' und ADR = ');
  disp.w(0,0,r, conv.hex_byte(mbc_param) );
  disp.w(0,0,s,' ).');

  disp.w(1,wherey+1,s,'An MB-Ctrl.-ADR = ');
         disp.w(0,0,r, conv.hex_byte(mbc_param) );
         disp.w(0,0,s, ' sind zwei 32Bit-I/O angeschlossen.');

  disp.w(3,wherey+1,s,'Ein 32Bit-I/O muss mit beiden KanÑlen auf '); disp.w(0,0,r,'Output'); disp.w(0,0,s,' stehen.');

  disp.w(5,wherey+1,s,'Kanal 0 (D7..0) = Skal. fÅr die zweite 32Bit-I/O an  MBC-ADR ');
         disp.w(0,0,r, conv.hex_byte(mbc_param) );
         disp.w(0,0,s, '.');

   disp.w(5,wherey+1,s,'Kanal 1 (D7..0) = Skal., (D12..8) = Mod-ADR der 32Bit-I/O an MBC-ADR ');
         disp.w(0,0,r, conv.hex_byte(mbc_test) );
         disp.w(0,0,s, '.');

  disp.w(3,wherey+1,s,'Ein 32Bit-I/O muss mit beiden KanÑlen auf '); disp.w(0,0,r,'Input'); disp.w(0,0,s,' stehen.');
  disp.w(1,wherey+1,s,'An MB-Ctrl.-ADR = ');
         disp.w(0,0,r, conv.hex_byte(mbc_test) );
         disp.w(0,0,s, ' ist eine 32Bit-I/O angeschlossen.');

  disp.w(3,wherey+1,s,'Ein 32Bit-I/O muss mit beiden KanÑlen auf '); disp.w(0,0,r,'Output'); disp.w(0,0,s,' stehen.');
  disp.w(1,wherey+1,s,'Sind min. zwei Modulbuscontroller vorhanden? ');
  writeln('');

  mil.set_ifk_array(ifc_online_str, IFC_211_MBC);
  mil.disp_ifk_array( 3, wherey+1, ifc_online_str);


  set_ifc_adr_stat := Okay;

  Gotoxy(1, wherey+1);
  Writeln('Es wird auf dem Modulbus nach den passend konfigurierten 32Bit-I/Os gesucht.');
  disp.weiter(answer, x, y, weiter_txt);
  disp.ini_mid_w;


  {Teste die Konfiguration an Modul-Bus-Contr. ADR = c_MBC_Bitmuster_adr }

  ifc_test_nr := mbc_param;
  Mil.clear_mb_str(mb_str);
  set_bitm_str(comp_str);

  mil.mb_search(ifc_test_nr, mb_str, C_io32_ID);
  mil.comp_mb_card_arr(mb_str, comp_str, Comp_Result, C_io32_ID);

  mil.disp_mb_card_arr(wherey+1, 20, mb_str);
  mil.disp_comp_result(Comp_Result, TextAttr);


  {Teste die Konfiguration an Modul-Bus-Contr. ADR = mb32_64_test }

  ifc_test_nr := mbc_test;
  Mil.clear_mb_str(mb_str);
  set_mb32t_str(comp_str);

  mil.mb_search(ifc_test_nr, mb_str, C_io32_ID);
  mil.comp_mb_card_arr(mb_str, comp_str, Comp_Result, C_io32_ID);

  mil.disp_mb_card_arr(wherey+1, 20, mb_str);
  mil.disp_comp_result(Comp_Result, TextAttr);
  disp.weiter(answer, x, y, weiter_txt);


 END;



PROCEDURE display_online_ifc;
  VAR
    answer: CHAR;
    loop_cnt: LONGINT;
  BEGIN
  mil.pc_mil_res;
  disp.wr_top_w(10,2,'Es werden die Online-IFCs angzeigt');
  clrscr;
  loop_cnt := 1;
  answer := ' ';
  disp.w(10, 2, b,'Anzahl der Durchlaeufe: ');
  REPEAT
    REPEAT
    gotoxy(40,2); writeln(loop_cnt:0);
    mil.set_ifk_array(ifc_online_str, all_types);
    mil.disp_ifk_array(1,wherey+1, ifc_online_str);
    loop_cnt := loop_cnt + 1;
    UNTIL keypressed OR (answer = ' ');
  disp.single_loop(answer, loop_txt);
  UNTIL answer IN ['x','X'];
  END;



PROCEDURE mb32t_pu_reset(zeilennummer:BYTE);


  VAR
    input_ok:     BOOLEAN;
    text_farbe:   BYTE;
    mil_error:    t_mil_err;
    answer:       CHAR;
    loop_cnt:     LONGINT;

    i: INTEGER;
    mod_adr:        BYTE;
    err_cnt:        BYTE;
    adr_skal:       t_word_byte;
    read_data_vor:  t_word_byte;
    read_data_nach: t_word_byte;

  BEGIN

  mil_error := [];
  mil.pc_mil_res;
  answer  := ' ';
  mod_adr := c_mb_first_adr;
  err_cnt := 0;
  z       := Zeilennummer;


  mil_error := [];
  disp.w(c_Be,z,s,'Powerup-Reset-------->');


  disp.w(c_Bm,z,s,'SEQ-Err. LEDs / PURES LED, J / N ? ');

  REPEAT UNTIL keypressed;
  answer := readkey;
  IF answer IN ['n','N'] THEN  err_cnt := err_cnt+1;



  {setze die Skalierung der mb32_data_k0_k1 Åber den Kanal 0 der mb32_skal_adr_k0_k1}
  adr_skal.b.hb := mb32_skal_adr_k0_k1; {setze die Modul-ADR}
  adr_skal.b.lb := $FF;

  mil.modulbus_adr_wr(NOT adr_skal.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

  {setze Modul_ADR und Skalierung der MB32_tst Åber den Kanal 0 der mb32_skal_adr_k0_k1}
  adr_skal.b.hb := mod_adr; {setze die Modul-ADR}
  adr_skal.b.lb := 0;

  mil.modulbus_adr_wr(NOT adr_skal.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  {lese Daten, EPLD_ID und 'frei' MB32_tst vor dem Reset}
  mil.modulbus_adr_rd(read_data_vor.w, C_ModAdr_EPLD,
                       mod_adr, mbc_test, mil_error);


  disp.w(c_Bm,z,s,'EPLD = ');
  disp.w(0,0,s, conv.hex_byte(read_data_vor.b.hb));
  disp.w(0,0,s,', PuRes = ');
  disp.w(0,0,s, conv.hex_byte(read_data_vor.b.lb));



  {Reset alle Flag's (Error) der MB32_tst}
  mil.modulbus_adr_wr($0000, c_io32_reset, mod_adr, mbc_test, mil_error);


  {lese Daten, EPLD_ID und 'frei' MB32_tst nach dem Reset}
  mil.modulbus_adr_rd(read_data_nach.w, C_ModAdr_EPLD,
                       mod_adr, mbc_test, mil_error);



  IF ((read_data_vor.b.lb  AND $01) = 1) OR   {Err., PupRes nicht gesetzt}
     ((read_data_nach.b.lb AND $01) = 0) THEN {Err., PupRes nicht geresetet}


    BEGIN
    err_cnt := err_cnt+1;
    disp.w(0,0,r,' / Err. = ');
    disp.w(0,0,r, conv.hex_byte(read_data_nach.b.lb));
    disp.w(0,0,r,'        ');
    disp.w(c_CR,z,s,'weiter mit <CR>');
    keypressed;
    readkey;
    mil.timer2_wait(c_wait);
    END
    ELSE disp.w(0,0,s,'              ');


  IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');

  END;


PROCEDURE mb32t_adr(zeilennummer:BYTE);


  VAR
    input_ok:     BOOLEAN;
    text_farbe:   BYTE;
    mil_error:    t_mil_err;
    answer:       CHAR;
    loop_cnt:     LONGINT;

    i: INTEGER;
    mod_adr:      BYTE;
    err_cnt:      BYTE;
    adr_skal:     t_word_byte;
    read_data:    t_word_byte;

  BEGIN

  mil_error := [];
  mil.pc_mil_res;
  answer  := ' ';
  mod_adr := c_mb_first_adr;
  err_cnt := 0;
  z       := Zeilennummer;

  FOR i:= 0 TO 31 DO

    BEGIN
    mil_error := [];
    disp.w(c_Be,z,s,'Modul-ADR ----------->');

    disp.w(c_Bm,z,s,'Set = ');
    disp.w(0,0,s, conv.hex_byte(mod_adr));
    disp.w(0,0,s,', read-ADR = ');


    adr_skal.b.hb := mb32_skal_adr_k0_k1; {setze die Modul-ADR}
    adr_skal.b.lb := $FF;

    mil.modulbus_adr_wr(NOT adr_skal.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

    adr_skal.b.hb := mod_adr; {setze die Modul-ADR}
    adr_skal.b.lb := 0;

    mil.modulbus_adr_wr(NOT adr_skal.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

    mil.modulbus_adr_rd(read_data.w, C_ModAdr_Scal,
                       mod_adr, mbc_test, mil_error);


    IF (read_data.b.lb AND $1F) <> mod_adr THEN
      BEGIN
      err_cnt := err_cnt+1;
      disp.w(0,0,r, conv.hex_byte(read_data.b.lb));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_byte(read_data.b.lb));
      END;

    mod_adr:= mod_adr+1;
    mil.timer2_wait(c_wait_loop);

    END;

    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

  END;





PROCEDURE mb32t_skal(zeilennummer:BYTE);

  VAR
    input_ok:     BOOLEAN;
    text_farbe:   BYTE;
    mil_error:    t_mil_err;
    answer:       CHAR;
    loop_cnt:     LONGINT;

    i: INTEGER;
    mod_adr:      BYTE;
    err_cnt:      BYTE;
    led_err_cnt:  BYTE;
    skal:         BYTE;
    adr_skal:     t_word_byte;
    read_data:    t_word_byte;



  BEGIN

  mil_error := [];
  mil.pc_mil_res;
  answer      := ' ';
  mod_adr     := c_mb_first_adr+1;
  skal        := 0;
  err_cnt     := 0;
  led_err_cnt := 0;
  i           := 0;
  z           := Zeilennummer;


  FOR i:= 0 TO 16 DO
    BEGIN

    disp.w(c_Be,z,s,'Skalierung----------->');

    IF i IN [1..8] THEN disp.w(c_Bm,z,s,'LED "');

    IF i =  1 THEN BEGIN skal := $01; disp.w(0,0,s,'K0-MODE0"'); END;
    IF i =  2 THEN BEGIN skal := $02; disp.w(0,0,s,'K0-MODE1"'); END;
    IF i =  3 THEN BEGIN skal := $04; disp.w(0,0,s,'K1-MODE0"'); END;
    IF i =  4 THEN BEGIN skal := $08; disp.w(0,0,s,'K1-MODE1"'); END;
    IF i =  5 THEN BEGIN skal := $10; disp.w(0,0,s,'16BIT   "'); END;
    IF i =  6 THEN BEGIN skal := $20; disp.w(0,0,s,'APK-ID  "'); END;
    IF i =  7 THEN BEGIN skal := $40; disp.w(0,0,s,'K0-INPUT"'); END;
    IF i =  8 THEN BEGIN skal := $80; disp.w(0,0,s,'K1-INPUT"'); END;
    IF i =  9 THEN       skal := $FE;
    IF i = 10 THEN       skal := $FD;
    IF i = 11 THEN       skal := $FB;
    IF i = 12 THEN       skal := $F7;
    IF i = 13 THEN       skal := $EF;
    IF i = 14 THEN       skal := $DF;
    IF i = 15 THEN       skal := $BF;
    IF i = 16 THEN       skal := $7F;

    IF i IN [1..8] THEN disp.w(0,0,s,' = EIN, J / N ? ');


    adr_skal.b.hb := mb32_skal_adr_k0_k1; {setze die Modul-ADR}
    adr_skal.b.lb := $FF;

    mil.modulbus_adr_wr(NOT adr_skal.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

    adr_skal.b.hb := mb32_64_test; {setze die Modul-ADR}
    adr_skal.b.lb := skal;

    mil.modulbus_adr_wr(NOT adr_skal.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


    mil.modulbus_adr_rd(read_data.w,  C_ModAdr_Scal,
                       mb32_64_test, mbc_test, mil_error);


    IF read_data.b.hb <> skal THEN
     BEGIN
     err_cnt := err_cnt+1;
     mil_error := [];
     disp.w(c_Bm,z,s,'Set = ');
     disp.w(0,0,s, conv.hex_byte(skal));
     disp.w(0,0,s,', Read-Skal. = ');

     disp.w(0,0,r, conv.hex_byte(read_data.b.hb));
     disp.w(0,0,s, '    ');
     disp.w(c_CR,z,s,'weiter mit <CR>');

     REPEAT
     UNTIL keypressed;

     END;


    IF i IN [1..8] THEN
      BEGIN
        REPEAT UNTIL keypressed;
        answer := readkey;
        IF answer IN ['n','N'] THEN  led_err_cnt := led_err_cnt+1;
      END;

    IF i IN [9..16] THEN mil.timer2_wait(c_wait_loop);

    END;


    IF ( err_cnt = 0 ) AND ( led_err_cnt = 0 )

              THEN disp.w(c_En,z,b,'Test ok        ')
              ELSE disp.w(c_En,z,r,'Fehler         ');

 END;



PROCEDURE mb32t_standard_16bit(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer;

  disp.w(c_Be,z,s,'K0=Out, Standard ---->');


  REPEAT
    REPEAT

    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);

    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);


    IF ist_data16 <> soll_data16 THEN
     BEGIN
     err_cnt := err_cnt+1;
     IF mil_error <> [] THEN mil_error := []; {clear mil_error nach Lesefehler}
     END;

    disp.w(c_Bm,z,s,'Write = '); disp.w(0,0,s, conv.hex_word(soll_data16));
    disp.w(0,0,s,', Read = ');

    IF ist_data16 <> soll_data16 THEN
      BEGIN
      disp.w(0,0,r, conv.hex_word(ist_data16));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_word(ist_data16));
      END;


    soll_data16 := soll_data16 +1;
    i:= i+1;

    UNTIL keypressed OR (answer  = ' ')
                     OR (i       = c_max_word_cnt+1);

  IF i = (c_max_word_cnt+1) THEN answer := 'x'
                            ELSE disp.single_loop(answer, loop_txt);
  UNTIL answer IN ['x','X'];


  IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');

  IF i <= c_max_word_cnt THEN disp.w(c_En,z,r,'Abbruch');





  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer+1;


  disp.w(c_Be,z,s,'K1=Inp,  Standard --->');


  REPEAT
    REPEAT

    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);

    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    IF ist_data16 <> soll_data16 THEN
     BEGIN
     err_cnt := err_cnt+1;
     IF mil_error <> [] THEN mil_error := []; {clear mil_error nach Lesefehler}
     END;

    disp.w(c_Bm,z,s,'Write = ');   disp.w(0,0,s, conv.hex_word(soll_data16));
    disp.w(0,0,s,', Read = ');


    IF ist_data16 <> soll_data16 THEN
      BEGIN
      disp.w(0,0,r, conv.hex_word(ist_data16));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_word(ist_data16));
      END;

    soll_data16 := soll_data16 +1;
    i:= i+1;


    UNTIL keypressed OR (answer  = ' ')
                     OR (i       = c_max_word_cnt+1);

  IF i = (c_max_word_cnt+1) THEN answer := 'x'
                 ELSE disp.single_loop(answer, loop_txt);
  UNTIL answer IN ['x','X'];


  IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');

  IF i <= c_max_word_cnt THEN disp.w(c_En,z,r,'Abbruch');

 END;



PROCEDURE mb32t_standard_32bit_write(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data32:  t_long_word;
    soll_data32: t_long_word;





  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0+K1=Out, Standard ->');


  FOR i:= 0 TO 3 DO

  BEGIN
    IF i = 0 THEN BEGIN soll_data32.hw:=$0000; soll_data32.lw:=$0000; END;
    IF i = 1 THEN BEGIN soll_data32.hw:=$55AA; soll_data32.lw:=$AA55; END;
    IF i = 2 THEN BEGIN soll_data32.hw:=$AA55; soll_data32.lw:=$55AA; END;
    IF i = 3 THEN BEGIN soll_data32.hw:=$FFFF; soll_data32.lw:=$FFFF; END;


    mil.modulbus_adr_wr(soll_data32.hw, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_wr(soll_data32.lw, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    mil.modulbus_adr_rd(ist_data32.hw, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
    mil.modulbus_adr_rd(ist_data32.lw, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);


    disp.w(c_Bm,z,s,'Write = ');
    disp.w(0,0,s, conv.hex_word(soll_data32.hw));
    disp.w(0,0,s, conv.hex_word(soll_data32.lw));
    disp.w(0,0,s,', Read = ');


    IF (ist_data32.hw <> soll_data32.hw) OR
       (ist_data32.lw <> soll_data32.lw) THEN

     BEGIN
     err_cnt := err_cnt+1;
     mil_error := []; {clear mil_error nach Lesefehler}
     disp.w(0,0,r, conv.hex_word(ist_data32.hw));
     disp.w(0,0,r, conv.hex_word(ist_data32.lw));
     disp.w(c_CR,z,s,'weiter mit <CR>');
     keypressed;
     readkey;
     mil.timer2_wait(c_wait);
     END
     ELSE
     disp.w(0,0,s, conv.hex_word(ist_data32.hw));
     disp.w(0,0,s, conv.hex_word(ist_data32.lw));
     END;

    mil.timer2_wait(c_wait_loop);


    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

 END;




PROCEDURE mb32t_standard_32bit_read(Zeilennummer: BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data32:  t_long_word;
    soll_data32: t_long_word;





  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0+K1=Inp, Standard ->');


  FOR i:= 0 TO 3 DO

  BEGIN

    IF i = 0 THEN BEGIN soll_data32.hw:=$0000; soll_data32.lw:=$0000; END;
    IF i = 1 THEN BEGIN soll_data32.hw:=$55AA; soll_data32.lw:=$AA55; END;
    IF i = 2 THEN BEGIN soll_data32.hw:=$AA55; soll_data32.lw:=$55AA; END;
    IF i = 3 THEN BEGIN soll_data32.hw:=$FFFF; soll_data32.lw:=$FFFF; END;


    mil.modulbus_adr_wr(soll_data32.hw, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
    mil.modulbus_adr_wr(soll_data32.lw, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);


    mil.modulbus_adr_rd(ist_data32.hw, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(ist_data32.lw, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'Write = ');
    disp.w(0,0,s, conv.hex_word(soll_data32.hw));
    disp.w(0,0,s, conv.hex_word(soll_data32.lw));
    disp.w(0,0,s,', Read = ');


    IF (ist_data32.hw <> soll_data32.hw) OR
       (ist_data32.lw <> soll_data32.lw) THEN


     BEGIN
     err_cnt := err_cnt+1;
     mil_error := []; {clear mil_error nach Lesefehler}
     disp.w(0,0,r, conv.hex_word(ist_data32.hw));
     disp.w(0,0,r, conv.hex_word(ist_data32.lw));
     disp.w(c_CR,z,s,'weiter mit <CR>');
     keypressed;
     readkey;
     mil.timer2_wait(c_wait);
     END
     ELSE
     disp.w(0,0,s, conv.hex_word(ist_data32.hw));
     disp.w(0,0,s, conv.hex_word(ist_data32.lw));
     END;

    mil.timer2_wait(c_wait_loop);


    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

 END;




PROCEDURE mb32t_handshake_16bit(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0=Out, Handshake --->');




  FOR i:= 0 TO 3 DO

  BEGIN

    IF i = 1 THEN soll_data16 :=$FFFF;
    IF i = 2 THEN soll_data16 :=$55AA;
    IF i = 3 THEN soll_data16 :=$AA55;
    IF i = 4 THEN soll_data16 :=$0000;


    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);

    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);


    IF ist_data16 <> soll_data16 THEN
     BEGIN
     err_cnt := err_cnt+1;
     IF mil_error <> [] THEN mil_error := []; {clear mil_error nach Lesefehler}
     END;

    disp.w(c_Bm,z,s,'Write = '); disp.w(0,0,s, conv.hex_word(soll_data16));
    disp.w(0,0,s,', Read = ');

    IF ist_data16 <> soll_data16 THEN
      BEGIN
      disp.w(0,0,r, conv.hex_word(ist_data16));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_word(ist_data16));
      END;


    mil.timer2_wait(c_wait_loop);

  END;


  IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');




  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer+1;


  disp.w(c_Be,z,s,'K1=Inp, Handshake --->');


  FOR i:= 0 TO 3 DO

  BEGIN

    IF i = 1 THEN soll_data16 :=$FFFF;
    IF i = 2 THEN soll_data16 :=$55AA;
    IF i = 3 THEN soll_data16 :=$AA55;
    IF i = 4 THEN soll_data16 :=$0000;


    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);

    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    IF ist_data16 <> soll_data16 THEN
     BEGIN
     err_cnt := err_cnt+1;
     IF mil_error <> [] THEN mil_error := []; {clear mil_error nach Lesefehler}
     END;

    disp.w(c_Bm,z,s,'Write = ');   disp.w(0,0,s, conv.hex_word(soll_data16));
    disp.w(0,0,s,', Read = ');


    IF ist_data16 <> soll_data16 THEN
      BEGIN
      disp.w(0,0,r, conv.hex_word(ist_data16));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_word(ist_data16));
      END;

    mil.timer2_wait(c_wait_loop);

  END;

  IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');

 END;



PROCEDURE mb32t_handshake_32bit_write(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data32:  t_long_word;
    soll_data32: t_long_word;





  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0+K1=Out, Handsh. -->');


  FOR i:= 0 TO 3 DO

  BEGIN
    IF i = 0 THEN BEGIN soll_data32.hw:=$0000; soll_data32.lw:=$0000; END;
    IF i = 1 THEN BEGIN soll_data32.hw:=$55AA; soll_data32.lw:=$AA55; END;
    IF i = 2 THEN BEGIN soll_data32.hw:=$AA55; soll_data32.lw:=$55AA; END;
    IF i = 3 THEN BEGIN soll_data32.hw:=$FFFF; soll_data32.lw:=$FFFF; END;


    mil.modulbus_adr_wr(soll_data32.hw, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_wr(soll_data32.lw, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    mil.modulbus_adr_rd(ist_data32.hw, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
    mil.modulbus_adr_rd(ist_data32.lw, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);


    disp.w(c_Bm,z,s,'Write = ');
    disp.w(0,0,s, conv.hex_word(soll_data32.hw));
    disp.w(0,0,s, conv.hex_word(soll_data32.lw));
    disp.w(0,0,s,', Read = ');


    IF (ist_data32.hw <> soll_data32.hw) OR
       (ist_data32.lw <> soll_data32.lw) THEN

     BEGIN
     err_cnt := err_cnt+1;
     mil_error := []; {clear mil_error nach Lesefehler}
     disp.w(0,0,r, conv.hex_word(ist_data32.hw));
     disp.w(0,0,r, conv.hex_word(ist_data32.lw));
     disp.w(c_CR,z,s,'weiter mit <CR>');
     keypressed;
     readkey;
     mil.timer2_wait(c_wait);
     END
     ELSE
     disp.w(0,0,s, conv.hex_word(ist_data32.hw));
     disp.w(0,0,s, conv.hex_word(ist_data32.lw));
     END;

    mil.timer2_wait(c_wait_loop);


    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

 END;






PROCEDURE mb32t_handshake_32bit_read(Zeilennummer: BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data32:  t_long_word;
    soll_data32: t_long_word;


  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0+K1=Inp, Handsh. -->');


  FOR i:= 0 TO 3 DO

  BEGIN

    IF i = 0 THEN BEGIN soll_data32.hw:=$0000; soll_data32.lw:=$0000; END;
    IF i = 1 THEN BEGIN soll_data32.hw:=$55AA; soll_data32.lw:=$AA55; END;
    IF i = 2 THEN BEGIN soll_data32.hw:=$AA55; soll_data32.lw:=$55AA; END;
    IF i = 3 THEN BEGIN soll_data32.hw:=$FFFF; soll_data32.lw:=$FFFF; END;


    mil.modulbus_adr_wr(soll_data32.hw, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
    mil.modulbus_adr_wr(soll_data32.lw, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);


    mil.modulbus_adr_rd(ist_data32.hw, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(ist_data32.lw, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'Write = ');
    disp.w(0,0,s, conv.hex_word(soll_data32.hw));
    disp.w(0,0,s, conv.hex_word(soll_data32.lw));
    disp.w(0,0,s,', Read = ');


    IF (ist_data32.hw <> soll_data32.hw) OR
       (ist_data32.lw <> soll_data32.lw) THEN


     BEGIN
     err_cnt := err_cnt+1;
     mil_error := []; {clear mil_error nach Lesefehler}
     disp.w(0,0,r, conv.hex_word(ist_data32.hw));
     disp.w(0,0,r, conv.hex_word(ist_data32.lw));
     disp.w(c_CR,z,s,'weiter mit <CR>');
     keypressed;
     readkey;
     mil.timer2_wait(c_wait);
     END
     ELSE
     disp.w(0,0,s, conv.hex_word(ist_data32.hw));
     disp.w(0,0,s, conv.hex_word(ist_data32.lw));
     END;

    mil.timer2_wait(c_wait_loop);


    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

 END;




PROCEDURE mb32t_Ext_Rd_T_16bit(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    out1_data16: WORD;
    out2_data16: WORD;
    soll_data16: WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  {mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
    mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Ext_Rd_T
                         + C_io32_K0_M_Ext_Rd_T;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  ist_data16  := 0;
  out1_data16 := 0;
  out2_data16 := 0;
  soll_data16 := 0;
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0=Out, HS ext.Takt-->');


  FOR i:= 0 TO 4 DO

  BEGIN

    IF i = 0 THEN soll_data16 :=$1234;{Dummy Wert}
    IF i = 1 THEN soll_data16 :=$FFFF;
    IF i = 2 THEN soll_data16 :=$55AA;
    IF i = 3 THEN soll_data16 :=$AA55;
    IF i = 4 THEN soll_data16 :=$0000;


    {lese das Output-Register fÅr Kanal 0 von MB32-Test}
    {die Daten dÅrfen noch nicht im Output-Register stehen}
    mil.modulbus_adr_rd(out1_data16, c_io32_rd_out0,
                       mb32_64_test, mbc_test, mil_error);

    {schreibe Soll-Daten auf Kanal 0 der MB32-Test}
    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);


    {lese das Output-Register fÅr Kanal 0 von MB32-Test}
    {die Daten mÅssen wegen dem autom. Handshake des Kanal0 der MB32-Test}
    {jetzt im Output-Register stehen}
    mil.modulbus_adr_rd(out2_data16, c_io32_rd_out0,
                       mb32_64_test, mbc_test, mil_error);


    {lese das Input-Register fÅr Kanal 0 von MB32-Io}
    {die Daten dÅrfen noch nicht im Input-Register stehen (Dummy read)}
    {es werden immer die Daten der letzten öbertagung gelesen. }
    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);



    disp.w(c_Bm,z,s,'Write = '); disp.w(0,0,s, conv.hex_word(soll_data16));
    disp.w(0,0,s,', Read = ');



    IF ( soll_data16 =  out1_data16) OR
       ( soll_data16 <> out2_data16) THEN

      BEGIN
      disp.w(0,0,r, conv.hex_word(out2_data16));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_word(out2_data16));
      END;


    mil.timer2_wait(c_wait_loop);

  END;

    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');





  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  ist_data16  := 0;
  out1_data16 := 0;
  out2_data16 := 0;
  soll_data16 := 0;
  z           := Zeilennummer+1;


  disp.w(c_Be,z,s,'K1=Out, HS ext.Takt-->');


  FOR i:= 0 TO 4 DO

  BEGIN

    IF i = 0 THEN soll_data16 :=$1234;{Dummy Wert}
    IF i = 1 THEN soll_data16 :=$FFFF;
    IF i = 2 THEN soll_data16 :=$55AA;
    IF i = 3 THEN soll_data16 :=$AA55;
    IF i = 4 THEN soll_data16 :=$0000;


    {lese das Output-Register fÅr Kanal 1 von MB32-Test}
    {die Daten dÅrfen noch nicht im Output-Register stehen}
    mil.modulbus_adr_rd(out1_data16, c_io32_rd_out1,
                       mb32_64_test, mbc_test, mil_error);

    {schreibe Soll-Daten auf Kanal 1 der MB32-Test}
    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    {lese das Output-Register fÅr Kanal 1 von MB32-Test}
    {die Daten mÅssen wegen dem autom. Handshake des Kanal0 der MB32-Test}
    {jetzt im Output-Register stehen}
    mil.modulbus_adr_rd(out2_data16, c_io32_rd_out1,
                       mb32_64_test, mbc_test, mil_error);


    {lese das Input-Register fÅr Kanal 1 von MB32-Io}
    {die Daten dÅrfen noch nicht im Input-Register stehen (Dummy read)}
    {es werden immer die Daten der letzten öbertagung gelesen. }
    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);



    disp.w(c_Bm,z,s,'Write = '); disp.w(0,0,s, conv.hex_word(soll_data16));
    disp.w(0,0,s,', Read = ');



    IF ( soll_data16 =  out1_data16) OR
       ( soll_data16 <> out2_data16) THEN

      BEGIN
      disp.w(0,0,r, conv.hex_word(out2_data16));
      disp.w(c_CR,z,s,'weiter mit <CR>');
      keypressed;
      readkey;
      mil.timer2_wait(c_wait);
      END
      ELSE
      BEGIN
      disp.w(0,0,s, conv.hex_word(out2_data16));
      END;

     mil.timer2_wait(c_wait_loop);

  END;

    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

 END;




PROCEDURE mb32t_Ext_Rd_T_32bit_write(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    soll_data32: t_long_word;
    ist_data32:  t_long_word;
    out1_data32: t_long_word;
    out2_data32: t_long_word;



  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  {mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
    mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Ext_Rd_T
                         + C_io32_K0_M_Ext_Rd_T;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  i           := 0;
  err_cnt     := 0;
  answer      := ' ';
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'K0+K1=Out, HS ext.T-->');


  FOR i:= 0 TO 4 DO

  BEGIN
    IF i = 0 THEN BEGIN soll_data32.hw:=$1234; soll_data32.lw:=$5678; END;
    IF i = 1 THEN BEGIN soll_data32.hw:=$0000; soll_data32.lw:=$0000; END;
    IF i = 2 THEN BEGIN soll_data32.hw:=$55AA; soll_data32.lw:=$AA55; END;
    IF i = 3 THEN BEGIN soll_data32.hw:=$AA55; soll_data32.lw:=$55AA; END;
    IF i = 4 THEN BEGIN soll_data32.hw:=$FFFF; soll_data32.lw:=$FFFF; END;

    {lese das Output-Register fÅr Kanal 0+1 von MB32-Test}
    {die Daten dÅrfen noch nicht im Output-Register stehen}
    mil.modulbus_adr_rd(out1_data32.hw, c_io32_rd_out0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(out1_data32.lw, c_io32_rd_out1,
                       mb32_64_test, mbc_test, mil_error);

    {schreibe Soll-Daten auf Kanal 0+1 der MB32-Test}
    mil.modulbus_adr_wr(soll_data32.hw, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_wr(soll_data32.lw, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    {lese das Output-Register fÅr Kanal 0+1 von MB32-Test}
    {die Daten mÅssen wegen dem autom. Handshake des Kanal0 der MB32-Test}
    {jetzt im Output-Register stehen}
    mil.modulbus_adr_rd(out2_data32.hw, c_io32_rd_out0,
                       mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(out2_data32.lw, c_io32_rd_out1,
                       mb32_64_test, mbc_test, mil_error);


    {lese das Input-Register fÅr Kanal 0 von MB32-Io}
    {die Daten dÅrfen noch nicht im Input-Register stehen (Dummy read)}
    {es werden immer die Daten der letzten öbertagung gelesen. }
    mil.modulbus_adr_rd(ist_data32.hw, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
    mil.modulbus_adr_rd(ist_data32.lw, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);



    disp.w(c_Bm,z,s,'Write = ');
    disp.w(0,0,s, conv.hex_word(soll_data32.hw));
    disp.w(0,0,s, conv.hex_word(soll_data32.lw));
    disp.w(0,0,s,', Read = ');


    IF ( (soll_data32.lw = out1_data32.lw) AND
         (soll_data32.hw = out1_data32.hw)     ) OR

       ( (soll_data32.lw <> out2_data32.lw) OR
         (soll_data32.hw <> out2_data32.hw)    ) THEN

     BEGIN
     err_cnt := err_cnt+1;
     mil_error := []; {clear mil_error nach Lesefehler}
     disp.w(0,0,r, conv.hex_word(ist_data32.hw));
     disp.w(0,0,r, conv.hex_word(ist_data32.lw));
     disp.w(c_CR,z,s,'weiter mit <CR>');
     keypressed;
     readkey;
     mil.timer2_wait(c_wait);
     END
     ELSE
     disp.w(0,0,s, conv.hex_word(ist_data32.hw));
     disp.w(0,0,s, conv.hex_word(ist_data32.lw));
     END;

    mil.timer2_wait(c_wait_loop);


    IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                       ELSE disp.w(c_En,z,r,'Fehler         ');

 END;



PROCEDURE mb32t_led_test(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;



  BEGIN

  mil.pc_mil_res;
  mil_error := [];
  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer;


  disp.w(c_Be,z,s,'LED-Test ------------>');



  {-------------------set Skalierung----------------------}
  { mb32_data_k0_k1:   00 = ADR=Dummy, 3F = K0+K1 Standard write }
  { mb32_64_test: 01 = MB-ADR,    FF = K0+K1 Standard read  }
  set_mb32t_skal(mbc_param, mb32_skal_adr_k0_k1, $003F, $01FF, mil_error);

  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}

  {-------------------Teste LED----------------------}
  answer := ' '; {}

  REPEAT
    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'LED K0-RD       = EIN,  J / N ? ');

    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}
  REPEAT

    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);


    disp.w(c_Bm,z,s,'LED K1-RD       = EIN,  J / N ? ');

    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);


  {--------------------set Skalierung-----------------------}
  { mb32_data_k0_k1:   00 = ADR=Dummy, FF = K0+K1 read,  Standard   }
  { mb32_64_test: 01 = MB-ADR,    3A = K0+K1 write, Handshake  }
  set_mb32t_skal(mbc_param, mb32_skal_adr_k0_k1, $00FF, $013A, mil_error);


  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  REPEAT
    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'K0-LED: WR,OBF,Err.= EIN,  J / N ? ');

    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  REPEAT
    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'K1-LED: WR,OBF,Err.= EIN,  J / N ? ');

    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {--------------------set Skalierung----------------------}
  { mb32_data_k0_k1:   00 = ADR=Dummy, 3A = K0+K1 write, Handshake }
  { mb32_64_test: 01 = MB-ADR,    FF = K0+K1 read,  Handshake }
  set_mb32t_skal(mbc_param, mb32_skal_adr_k0_k1, $003A, $01FA, mil_error);


  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                           mb32_data_k0_k1, mbc_param, mil_error);

  disp.w(c_Bm,z,s,'LED K0-IBF        = EIN,  J / N ? ');

  REPEAT
    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);

  disp.w(c_Bm,z,s,'LED K1-IBF        = EIN,  J / N ? ');

  REPEAT
    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);




  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  REPEAT
    mil.modulbus_adr_rd(ist_data16, c_io32_stsAPK0,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'LED  RD-ID         = EIN,  J / N ? ');

    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  REPEAT
    mil.modulbus_adr_rd(ist_data16, c_io32_status1,
                       mb32_64_test, mbc_test, mil_error);

    disp.w(c_Bm,z,s,'LED  RW-INRM       = EIN,  J / N ? ');

    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {--------------------set Skalierung----------------------}
  { mb32_data_k0_k1:   00 = ADR=Dummy, 2F = K0+K1 write, Standard, 32 Bit }
  { mb32_64_test: 01 = MB-ADR,    EF = K0+K1 read,  Standard, 32 Bit }
  set_mb32t_skal(mbc_param, mb32_skal_adr_k0_k1, $002F, $01EF, mil_error);


  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,
                          mb32_64_test, mbc_test, mil_error);{erster Read (ok)}

  mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,        {zweiter Read (Err)}
                          mb32_64_test, mbc_test, mil_error);

  disp.w(c_Bm,z,s,'LED RD-SEQ-Err    = EIN,  J / N ? ');

  REPEAT
    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  {-------------------------set Skalierung------------------------}
  { mb32_data_k0_k1:   00 = ADR=Dummy, EF = K0+K1 read,  Standard, 32 Bit }
  { mb32_64_test: 01 = MB-ADR,    2F = K0+K1 write, Standard, 32 Bit }
  set_mb32t_skal(mbc_param, mb32_skal_adr_k0_k1, $00EF, $012F, mil_error);


  {-------------------Teste LED----------------------}
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  answer := ' '; {}

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                          mb32_64_test, mbc_test, mil_error);{erster Write (ok)}

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,       {zweiter Write (Err)}
                          mb32_64_test, mbc_test, mil_error);

  disp.w(c_Bm,z,s,'WR-SQ-Err,W-NEXT  = EIN,  J / N ? ');

  REPEAT
    IF keypressed THEN answer := readkey;

  UNTIL answer IN ['n','N','j','J'];

  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);



  clr_mb32(mbc_test, mb32_64_test, mil_err);

  IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');

 END;



PROCEDURE mb32t_dtack(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ifa_int_sts: word;
    ist_data16:  word;
    soll_data16: word;





  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer+1;


  disp.w(c_Be,z,s,'K0+1=Out, Dtack-Test->');


  REPEAT
    REPEAT

{ mil.wr_fc (                     Adr, Fct: Byte; VAR mil_err: t_mil_err);
  mil.rd    ( VAR mil_data: WORD; Adr, Fct: Byte; VAR mil_err: t_mil_err);
  mil.wr    ( data: WORD;         Adr, Fct: Byte; VAR mil_err: t_mil_err); }


    mil.pc_mil_res;
    mil_error := [];

    mil.wr_fc ( mbc_test, $75, mil_error); {Reset Dtack-Error}
    mil.rd    ( ifa_int_sts, mbc_test, $CA, mil_error);
    disp.w(c_Bm,z,s,'Dtack-Test = ');
{   disp.w(0,0,s, conv.hex_word(ifa_int_sts));}{Display Status-Register}

    IF (ifa_int_sts AND $4000) = 0 THEN disp.w(0,0,r,'Error')
                                   ELSE disp.w(0,0,s,'ok   ');


    mil.modulbus_adr_rd(soll_data16, $FE, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $FC, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $FA, mb32_64_test, mbc_test, mil_error);

    mil.modulbus_adr_rd(soll_data16, $18, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $16, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $14, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $12, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $10, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $06, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_rd(soll_data16, $04, mb32_64_test, mbc_test, mil_error);

    mil.modulbus_adr_wr($0000, $12, mb32_64_test, mbc_test, mil_error);
    mil.modulbus_adr_wr($0000, $10, mb32_64_test, mbc_test, mil_error);

    mil.modulbus_adr_wr($0000, $20, mb32_64_test, mbc_test, mil_error);{Res. MB32}

    IF mil_error <> [] THEN mil.pc_mil_res;


    mil.rd    ( ifa_int_sts, mbc_test, $CA, mil_error);
    disp.w(0,0,s,'  Test1 = ');
{   disp.w(0,0,s, conv.hex_word(ifa_int_sts));}{Display Status-Register}

    IF (ifa_int_sts AND $4000) = 0 THEN disp.w(0,0,r,'Error')
                                   ELSE disp.w(0,0,s,'ok   ');



    mil.modulbus_adr_rd(soll_data16, $02, mb32_64_test, mbc_test, mil_error);



    IF mil_error <> [] THEN mil.pc_mil_res;

    mil.rd    ( ifa_int_sts, mbc_test, $CA, mil_error);
    disp.w(0,0,s,'  Test2 = ');
 {  disp.w(0,0,s, conv.hex_word(ifa_int_sts));}{Display Status-Register}

    IF (ifa_int_sts AND $4000) = 0 THEN disp.w(0,0,r,'Error')
                                   ELSE disp.w(0,0,s,'ok   ');


    UNTIL keypressed OR (answer  = ' ');


  disp.single_loop(answer, loop_txt);
  UNTIL answer IN ['x','X'];


 END;



PROCEDURE mb32t_ram_dtack(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;
    fc_rw_test1: BYTE;
    fc_rw_test2: BYTE;

    ram_adr1:    t_word_byte;
    adr_mb_sub1: t_word_byte;
    ram_adr2:    t_word_byte;
    adr_mb_sub2: t_word_byte;

    ifa_int_sts: word;
    ist_data16:  word;
    soll_data16: word;



  BEGIN

  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  z           := Zeilennummer+1;


  disp.w(c_Be,z,s,'32OUT,DTACK-RAM-Test->');


  mil.wr($0004, mbc_test, $64, mil_error);  {set Group-Count = 2}

  fc_rw_test1:= $31;
  fc_rw_test2:= $B4;

  ram_adr1.b.hb    := $00;{frei}    ram_adr1.b.lb    := fc_rw_test1;{FC-Code}
  adr_mb_sub1.b.hb := $01;{Mod-Adr} adr_mb_sub1.b.lb := $00;        {Sub-Adr}

  ram_adr2.b.hb    := $00;{frei}    ram_adr2.b.lb    := fc_rw_test2;{FC-Code}
  adr_mb_sub2.b.hb := $01;{Mod-Adr} adr_mb_sub2.b.lb := $06;        {Sub-Adr}


  {Init RAM}

  mil.wr( ram_adr1.w,    MBC_virt, $62, mil_error); {set RAM-ADR}
  mil.wr( adr_mb_sub1.w, MBC_virt, $63, mil_error); {set MB-Adr und Sub-Adr}
  mil.wr( ram_adr2.w,    MBC_virt, $62, mil_error); {set RAM-ADR}
  mil.wr( adr_mb_sub2.w, MBC_virt, $63, mil_error); {set MB-Adr und Sub-Adr}

  disp.w(c_Be,10,s,' RAdr1 ');
  disp.w(0,0,s, conv.hex_word(ram_adr1.w));
  disp.w(0,0,s,' MbSubAdr1 ');
  disp.w(0,0,s, conv.hex_word(adr_mb_sub1.w));
  disp.w(0,0,s,' RAdr2 ');
  disp.w(0,0,s, conv.hex_word(ram_adr2.w));
  disp.w(0,0,s,' MbSubAdr2 ');
  disp.w(0,0,s, conv.hex_word(adr_mb_sub2.w));


  REPEAT
    REPEAT

    mil.pc_mil_res;
    mil_error := [];

    mil.wr_fc ( mbc_test, $75, mil_error);


    mil.rd    ( ifa_int_sts, mbc_test, $CA, mil_error);
    disp.w(c_Bm,z,s,'Dtack = ');
    IF (ifa_int_sts AND $4000) = 0 THEN disp.w(0,0,r,'Error')
                                   ELSE disp.w(0,0,s,'ok   ');


    mil.wr(soll_data16, MBC_virt, fc_rw_test1, mil_error);
    IF mil_error <> [] THEN mil.pc_mil_res;


    mil.rd    ( ifa_int_sts, mbc_test, $CA, mil_error);
    disp.w(0,0,s,'  Test1 = ');
    IF (ifa_int_sts AND $4000) = 0 THEN disp.w(0,0,r,'Error')
                                   ELSE disp.w(0,0,s,'ok   ');


    mil.rd(soll_data16, MBC_virt, fc_rw_test2, mil_error);
    IF mil_error <> []THEN mil.pc_mil_res;


    mil.rd    ( ifa_int_sts, mbc_test, $CA, mil_error);
    disp.w(0,0,s,'  Test2 = ');
    IF (ifa_int_sts AND $4000) = 0 THEN disp.w(0,0,r,'Error')
                                   ELSE disp.w(0,0,s,'ok   ');




    UNTIL keypressed OR (answer  = ' ');


  disp.single_loop(answer, loop_txt);
  UNTIL answer IN ['x','X'];


{ IF ( err_cnt = 0 ) THEN disp.w(c_En,z,b,'Test ok        ')
                     ELSE disp.w(c_En,z,r,'Fehler         ');}


 END;



PROCEDURE disp_sts(zeilennummer:BYTE);


  VAR
    mil_error:     t_mil_err;
    k0_sts:        WORD;
    k1_sts:        WORD;
    sum_sts:       WORD;
    adr_k0_sts:    t_word_byte;
    adr_k1_sts:    t_word_byte;
    adr_sum_sts:   t_word_byte;


  BEGIN

{  mil.pc_mil_res;  }
  mil_error := [];

  z           := Zeilennummer;
  k0_sts      := $A55A;
  k1_sts      := $5AA5;
  sum_sts     := $A5A5;

{ mil.wr_fc (                     Adr, Fct: Byte; VAR mil_err: t_mil_err);
  mil.rd    ( VAR mil_data: WORD; Adr, Fct: Byte; VAR mil_err: t_mil_err);
  mil.wr    ( data: WORD;         Adr, Fct: Byte; VAR mil_err: t_mil_err); }

{
  adr_k0_sts.b.hb  := mb32_64_test; adr_k0_sts.b.lb  := c_io32_status1;
  adr_k1_sts.b.hb  := mb32_64_test; adr_k1_sts.b.lb  := c_io32_status2;
  adr_sum_sts.b.hb := mb32_64_test; adr_sum_sts.b.lb := c_io32_sumstat;

  mil.wr    ( adr_k0_sts.w,  mbc_test, c_fc_mod_adr_set, mil_error);
  mil.rd    ( k0_sts,        mbc_test, c_fc_mod_rd,      mil_error);
  mil.wr    ( adr_k1_sts.w,  mbc_test, c_fc_mod_adr_set, mil_error);
  mil.rd    ( k1_sts,        mbc_test, c_fc_mod_rd,      mil_error);
  mil.wr    ( adr_sum_sts.w, mbc_test, c_fc_mod_adr_set, mil_error);
  mil.rd    ( sum_sts,       mbc_test, c_fc_mod_rd,      mil_error);
}

  mil.modulbus_adr_rd(k0_sts,  c_io32_status1, mb32_64_test, mbc_test, mil_error);
  mil.modulbus_adr_rd(k1_sts,  c_io32_status2, mb32_64_test, mbc_test, mil_error);
  mil.modulbus_adr_rd(sum_sts, c_io32_sumstat, mb32_64_test, mbc_test, mil_error);


  disp.w(c_Be,z,s,'STS(K0, K1, Su) ');

  disp.w(0,0,s, conv.word_bin(k0_sts));
  disp.w(0,0,s,' ');
  disp.w(0,0,s, conv.word_bin(k1_sts));
  disp.w(0,0,s,' ');
  disp.w(0,0,s, conv.word_bin(sum_sts));

 END;




PROCEDURE mb32t_flags_16bit(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;

    k0_sts:      WORD;
    k1_sts:      WORD;
    sum_sts:     WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_standard
                         + C_io32_K0_M_standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

  clr_mb32(mbc_test, mb32_64_test, mil_err);

  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  soll_data16 :=$5A5A;
  z           := Zeilennummer;

  disp.w(c_Be,z,s,'K0-Wr-Flags, Disp. STS-->');

  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  disp_sts(z+1);

  {Enable INR-DRQ}
  mil.modulbus_adr_wr($4000, c_io32_status1, mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);


  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);


  z           := Zeilennummer+6;

  disp.w(c_Be,z,s,'K1-Rd-Flags, Disp. STS-->');
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  disp_sts(z+1);

  {Enable INR-DRDY}
  mil.modulbus_adr_wr($8000, c_io32_status1, mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);


  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);
  disp_sts(z+1);


  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);
  disp_sts(z+1);


  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);
  disp_sts(z+1);

 END;



PROCEDURE mb32t_flags_wr32bit(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;

    k0_sts:      WORD;
    k1_sts:      WORD;
    sum_sts:     WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_standard
                         + C_io32_K0_M_standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  soll_data16 :=$5A5A;
  z           := Zeilennummer;

  disp.w(c_Be,z,s,'Flags-Wr32,  Disp. STS-->');

  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  clr_mb32(mbc_param,  mb32_data_k0_k1,   mil_err);{Clear Flags}

  disp_sts(z+1);

  {Enable INR-DRQ}
  mil.modulbus_adr_wr($4000, c_io32_status1, mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);


  mil_error := [];
  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  disp_sts(z+1);

  {Enable INR-DRQ}
  mil.modulbus_adr_wr($4000, c_io32_status1, mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);


  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);
  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);

 END;


PROCEDURE mb32t_flags_rd32bit(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;

    k0_sts:      WORD;
    k1_sts:      WORD;
    sum_sts:     WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_standard
                         + C_io32_K0_M_standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                       { + C_io32_16bit_032bit }
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  i           := 0;
  err_cnt     := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  soll_data16 :=$5A5A;
  z           := Zeilennummer;

  disp.w(c_Be,z,s,'Flags-Rd32,  Disp. STS-->');


  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}
  clr_mb32(mbc_param,  mb32_data_k0_k1,   mil_err);{Clear Flags}


  disp_sts(z+1);

  {Enable INR-DRDY}
  mil.modulbus_adr_wr($8000, c_io32_status1, mb32_64_test, mbc_test, mil_error);
  disp_sts(z+1);



  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);
  disp_sts(z+1);

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);
  disp_sts(z+1);

  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);
  mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);
  disp_sts(z+1);

 END;




PROCEDURE mb32t_drq(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;
    mask_err:    BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;
    ifc_sts:     WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  mask_err    := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  ifc_sts     := 0;
  z           := Zeilennummer;


  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}

  disp.w(c_Be,z,s,'K0=Out, Test: DRQ --->');


  {Enable INR-DRQ fÅr Kanal 0}
  mil.modulbus_adr_wr($C000, c_io32_status1, mb32_64_test, mbc_test, mil_error);


  mil.rd (ifc_sts, mbc_test, c_fc_irm, mil_error);{read INR-Reg der IFA}
  IF ifc_sts AND ifc_stat_dreq = 0 THEN
                 mask_err := mask_err +1; {DRQ bereits aktiv}

  REPEAT
    REPEAT

    soll_data16 :=$5A5A;

    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_0,
                       mb32_64_test, mbc_test, mil_error);

    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_0,
                       mb32_data_k0_k1, mbc_param, mil_error);


    mil.rd (ifc_sts, mbc_test, c_fc_irm, mil_error);{read INR-Reg der IFA}
    IF ifc_sts AND ifc_stat_dreq = ifc_stat_dreq THEN
                   mask_err := mask_err +1; {DRQ nicht aktiv}


    disp.w(c_Bm,z,s,'LED DRQ         = EIN,  J / N ? ');


    UNTIL keypressed OR (answer  = ' ');


  disp.single_loop(answer, loop_txt);
  UNTIL answer IN ['n','N','j','J'];


  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);


  IF ( err_cnt  <> 0 ) THEN disp.w(c_En,z,r,'LED-Fehler     ');
  IF ( mask_err <> 0 ) THEN disp.w(c_En,z,r,'Fehler: INR-STS');

  IF ( err_cnt = 0 ) AND (mask_err = 0 )
                       THEN disp.w(c_En,z,b,'Test ok        ')

 END;



PROCEDURE mb32t_drdy(zeilennummer:BYTE);


  VAR
    input_ok:    BOOLEAN;
    text_farbe:  BYTE;
    mil_error:   t_mil_err;

    i:           longint;
    answer:      CHAR;
    err_cnt:     BYTE;
    mask_err:    BYTE;

    ist_data16:  WORD;
    soll_data16: WORD;
    ifc_sts:     WORD;



  BEGIN


  mil.pc_mil_res;
  mil_error := [];


  {Sete die Skalierung fur die MB32 'mb32_data_k0_k1' ueber Kanal 0 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb := 0; {Dummy, die ADR ist fest Verdrahtet}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Sete die Skalierung fur die MB32 'mb32_64_test' ueber Kanal 1 der MB32 'mb32_skal_adr_k0_k1'}
  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_handshake
                         + C_io32_K0_M_handshake;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);



  i           := 0;
  err_cnt     := 0;
  mask_err    := 0;
  answer      := 't';{Dummy: ' ' = Stop, 'X' = Exit}
  ist_data16  := 0;
  soll_data16 := 0;
  ifc_sts     := 0;
  z           := Zeilennummer;

  clr_mb32(mbc_test, mb32_64_test, mil_err);{Clear Flags}

  disp.w(c_Be,z,s,'K0=IN,  Test: DRDY -->');


  {Enable INR-DRDY fÅr Kanal 1}
  mil.modulbus_adr_wr($C000, c_io32_status2, mb32_64_test, mbc_test, mil_error);


  mil.rd (ifc_sts, mbc_test, c_fc_irm, mil_error);{read INR-Reg der IFA}
  IF ifc_sts AND ifc_stat_drdy = 0 THEN
                 mask_err := mask_err +1; {DRQ bereits aktiv}

  REPEAT
    REPEAT

    soll_data16 :=$5A5A;


    mil.modulbus_adr_wr(soll_data16, c_io32_kanal_1,
                       mb32_data_k0_k1, mbc_param, mil_error);


    mil.rd (ifc_sts, mbc_test, c_fc_irm, mil_error);{read INR-Reg der IFA}
    IF ifc_sts AND ifc_stat_drdy = ifc_stat_drdy THEN
                   mask_err := mask_err +1; {DRDY nicht aktiv}



    mil.modulbus_adr_rd(ist_data16, c_io32_kanal_1,
                       mb32_64_test, mbc_test, mil_error);



    disp.w(c_Bm,z,s,'LED DRDY        = EIN,  J / N ? ');


    UNTIL keypressed OR (answer  = ' ');
  disp.single_loop(answer, loop_txt);

  UNTIL answer IN ['n','N','j','J'];


  IF answer IN ['n','N'] THEN mb32t_err_wait(err_cnt);


  IF ( err_cnt  <> 0 ) THEN disp.w(c_En,z,r,'LED-Fehler     ');
  IF ( mask_err <> 0 ) THEN disp.w(c_En,z,r,'Fehler: INR-STS');

  IF ( err_cnt = 0 ) AND (mask_err = 0 )
                       THEN disp.w(c_En,z,b,'Test ok        ')

 END;



PROCEDURE disp_test;



  BEGIN

    z:=5;
    disp.w(c_Be,z,s,'32Bit_Hand');
    disp.w(c_Bm,z,s,'Bitmuster Ausgabe');
    disp.w(c_CR,z,s,', weiter mit <CR>');
    disp.w(c_En,z,b,'Test ok');


    REPEAT UNTIL keypressed OR (answer  IN ['x','X']);


  END;




FUNCTION menue_win: CHAR;
VAR
  answer:   CHAR;
  zeile:    BYTE;

begin
  disp.ini_top_w;
  disp.wr_top_w(1,1,head_txt);
  disp.ini_mid_w;


  disp.w(2, 2,s,'*********************  TEST-MENUE  *********************** ');
  disp.w(2, 4,s,'                                                           ');
  disp.w(2, 5,s,'[0]<-  Welche IFC am MIL-Bus?                              ');
  disp.w(2, 6,s,'[1]<-  Automatischer Konfigurations-Test                   ');
  disp.w(2, 7,s,'[2]<-  MB32-Test                                           ');
  disp.w(2, 8,s,'[3]<-  Test_DTACK                                          ');
  disp.w(2, 9,s,'[4]<-  Test_Flags_16Bit                                    ');
  disp.w(2,10,s,'[5]<-  Test_Flags_wr32Bit                                  ');
  disp.w(2,11,s,'[6]<-  Test_Flags_rd32Bit                                  ');
  disp.w(2,12,s,'[7]<-  Test                                                ');

  answer := ' ';
  zeile  := 2; {Start des Testprogramms in Zeile}

  mil.pc_mil_da;
  {$IFDEF TEST}
    glob_var_pc_mil_da := c_pc_mil_da;
  {$ENDIF}
  IF glob_var_pc_mil_da = c_pc_mil_da THEN
    disp.single_loop(answer, auswahl_txt)
  ELSE BEGIN
    disp.single_loop(answer,' Es ist keine PC_MIL-Karte vorhanden !!!!!');
    answer := 'X';
    END;

  menue_win := UPCASE(answer);
  End; { menue_win }


BEGIN
{ Init VAR fuer die Interface-Karten-ADR. der MB-Contr.}
  mbc_param   	:=  c_mbc_param_adr; {Modulbus-Contr. fuer Skalierung, Adressen und Data I/O}
  mbc_test    	:=  c_MBC_test_adr;  {Modulbus-Contr. fuer den Test der 32/64 Bit I/O}
  mbc_virt    	:=  c_MBC_virt_adr;  {Virt.-MB-Contr. fuer den Test der 32/64 Bit I/O}

{ Init VAR fuer die Modulbus-Adressen der 32bit-IOs }
  mb32_skal_adr_k0_k1	:= c_mb32_skal_adr_k0_k1;   {MB32 I/O fuer die Skal. und ADR.}
  mb32_skal_adr_k0_k1	:= c_mb32_skal_adr_k0_k1;   {MB32 I/O fuer die Skal. und ADR.}
  mb32_data_k0_k1	:= c_mb32_data_k0_k1;       {MB32 I/O fuer das Data I/O}
  mb32_data_k2_k3	:= c_mb32_data_k2_k3;	    {MB32 I/O fuer das Data I/O}
  mb32_64_test		:= c_mb32_64_test;	    {MB32 I/O die getestet erden soll}


  disp.ini_top_w;
  disp.ini_mid_w;
  disp.ini_bot_w;
  disp.ini_right_w;
  disp.ini_mid_w;
  IF TEST_Mil THEN BEGIN
    Writeln ('Es wird eine mit Test Åbersetzte Mil_Unit verwendet!');
    Repeat until keypressed;
    readkey;
    END;

  REPEAT
    answer := menue_win;

    CASE answer OF

     '0': display_online_ifc;

     '1': autom_konfig_tst;

     '2': BEGIN
          disp.ini_bot_w;
          disp.wr_top_w(10,2,' Test Modulbus 32 Bit I/O' );
          clrscr;

          mb32t_pu_reset(2);
          mb32t_adr(3);
          mb32t_skal(4);
          mb32t_standard_16bit(5);
          mb32t_standard_32bit_write(7);
          mb32t_standard_32bit_read(8);
          mb32t_handshake_16bit(9);
          mb32t_handshake_32bit_write(11);
          mb32t_handshake_32bit_read(12);
          mb32t_Ext_Rd_T_16bit(13);
          mb32t_Ext_Rd_T_32bit_write(15);
          mb32t_led_test(16);
          mb32t_drq(17);
          mb32t_drdy(18);

          disp.w(10,19,s,'MB32-Test Ende, weiter mit <CR>');
          REPEAT UNTIL keypressed;
          readkey;
          mil.timer2_wait(c_wait);
          END;

     '3': BEGIN
          disp.ini_bot_w;
          disp.wr_top_w(10,2,' Test Modulbus 32 Bit I/O' );
          clrscr;

          mb32t_dtack(18);

          disp.w(10,20,s,'Test Ende, weiter mit <CR>');
          REPEAT UNTIL keypressed;
          readkey;
          mil.timer2_wait(c_wait);
          END;


       '4': BEGIN
          disp.ini_bot_w;
          disp.wr_top_w(10,2,' Test Modulbus 32 Bit I/O' );
          clrscr;

          mb32t_flags_16bit(5);

          disp.w(10,20,s,'Test Ende, weiter mit <CR>');
          REPEAT UNTIL keypressed;

          readkey;
          mil.timer2_wait(c_wait);
          END;


       '5': BEGIN
          disp.ini_bot_w;
          disp.wr_top_w(10,2,' Test Modulbus 32 Bit I/O' );
          clrscr;

          mb32t_flags_wr32bit(5);

          disp.w(10,20,s,'Test Ende, weiter mit <CR>');
          REPEAT UNTIL keypressed;

          readkey;
          mil.timer2_wait(c_wait);
          END;


       '6': BEGIN
          disp.ini_bot_w;
          disp.wr_top_w(10,2,' Test Modulbus 32 Bit I/O' );
          clrscr;

          mb32t_flags_rd32bit(5);

          disp.w(10,20,s,'Test Ende, weiter mit <CR>');
          REPEAT UNTIL keypressed;

          readkey;
          mil.timer2_wait(c_wait);
          END;


       '7': BEGIN
          disp.ini_bot_w;
          disp.wr_top_w(10,2,' Test Modulbus 32 Bit I/O' );
          clrscr;

          mb32t_led_test(16);

          disp.w(10,21,s,'Test Ende, weiter mit <CR>');
          REPEAT UNTIL keypressed;

          readkey;
          mil.timer2_wait(c_wait);
          END;


    End; { CASE }
  UNTIL (answer IN ['x', 'X']);

END. { MB32_TST }
