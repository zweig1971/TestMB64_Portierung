UNIT Mil_Unit;             { TURBO PASCAL ab V 5.5 }

{$S-,G+,B-,N+,E+,I-}

{$DEFINE noTEST_MIL}

INTERFACE

uses
  Crt32,
  UnitMil,
  convert,
  display;

{$IFDEF TEST_MIL}
  CONST Mil_Unit_Version: String = 'T1.00';
{$ELSE}
  CONST Mil_Unit_Version: String = 'V1.00';
{$ENDIF}

CONST
  Mil_Unit_Create_Date = '16.08.2000';

VAR
  conv: t_convert;
  disp: t_disp;


CONST

 c_res = $0001;

 stat_intlock  = $0001;
 stat_dta_rdy  = $0002;
 stat_dta_req  = $0004;
 stat_wrt_rdy  = $0008;
 stat_val_wrd  = $0010;
 stat_ffo_ety  = $0020;
 stat_ffo_ful  = $0040;
 stat_cmd_mode = $0080;
 stat_cmd_rcv  = $0100;
 stat_timeout1 = $0200; { Bit 09 Hardware-Timer1: 0=laeuft, 1=abgelaufen  }
 stat_timeout2 = $0400; { Bit 10 Hardware-Timer2: 0=laeuft, 1=abgelaufen  }
 stat_timer2   = $0800; { Bit 11 Hardware-Timer2 Clock-Ticks: 0=10us 1=1ms}

 ifc_irmask_all  = $E000; {Interrupt Enable Maske auf der IFC-Karte}
 ifc_irmask_intl = $8000;
 ifc_irmask_drdy = $4000;
 ifc_irmask_dreq = $2000;

 ifc_stat_dreq   = $0400;
 ifc_stat_drdy   = $0800;
 ifc_stat_intl   = $1000;


 c_fc_sw1    = $06;
 c_fc_sw2    = $07;
 c_fc_sw3    = $08;
 c_fc_sw4    = $09;
 c_fc_sw5    = $0A;
 c_fc_reset  = $01;

  c_fc_wr_ifk_mode = $60; { Mode-Reg der IFK_211:       0 => IFK-Mode
                                                        1 => Fg-Mode
                                                        2 => Mb-Mode
                                                        4 => Sweeper-Mode }
  m_ifc_211         = 0;
  m_ifc_211_fg      = 1;
  m_ifc_211_mbc     = 2;
  m_ifc_211_sweeper = 4;

  c_fc_rd_ifk_mode = $97;

 CONST
 {+-----------------------------------------------------------------+}
 {|   Funktionscodes zur Bedienung eines Modulbus-Controllers.      |}
 {+-----------------------------------------------------------------+}
 c_fc_mod_wr      = $10;
 c_fc_mod_adr_set = $11;
 c_fc_mod_rd      = $90;

 c_fc_wr_irm = $12;
 c_fc_ifa_mode = $60;
 c_fc_iw1    = $81;
 c_fc_iw2    = $82;
 c_fc_stat   = $C0;
 c_fc_ifk_id = $CC;
 c_fc_bc_set = $7F;
 c_fc_bc_reset = $7E;


 c_fc_ifk_global_stat = $CA;


 p_stat      = $220;
 p_data      = $222;
 p_mode      = $224;
 p_rset      = $226;
 p_intr      = $228;   {wichtig fr Ger„te-Simulation: Interrupts setzen}
 p_tmr1      = $22A;   {set and start timer}
 p_tmr2      = $22C;   {set and start timer}
 p_tmr2_1ms  = $22E;   {Ansprechen der Adr. schaltet Timer2 auf 1 ms}

 dta_mode   = 0;
 cmd_mode   = 1;


 c_wr_rdy_time  = 5;
 c_val_wrd_time = 10;

CONST
  max_ifc = 254;

TYPE
 t_adr_fc = RECORD CASE BYTE OF
              1: (r : PACKED RECORD
                      adr : BYTE;
                      fc  : BYTE
                    END;
                  );
              2: (w : WORD);
            END;

 t_mil_err = SET OF (fc_to, rd_to, wr_to, fifo_not_empty, fifo_not_cleared, no_pc_mil);

 t_result = ( ok, not_ok );
 {+------------------------------------------------------------------------+}
 {| In der Procedur 'set_ifk_array' kann nach einem bestimmten Kartentyp   |}
 {| gesucht werden. Falls es in Zukunft neue Funktionen fr die IFC_211    |}
 {| gibt, oder ganz neue Karten, der Type wrde in dem nachfolgenen        |}
 {| Aufz„hlungstyp eingetragen. Deshalb eignet sich 't_IFC_Type' auch gut  |}
 {| als Suchauswahl fr die Procedur 'set_ifk_array' damit auch nach allen |}
 {| IFC-Karten gesucht werden kann ist das Element 'All_Types' definiert.  |}
 {| Es stellt natrlich keine IFK-Karte dar und es sollte nie gesetzt      |}
 {| werden, Die gilt ebenso fuer die Fehler-Namen 'IFC_211_unknown' und    |}
 {| 'Answer_Wrong_Nr' sie werden gegebenenfalls in den Funktionen 'IFC_Da' |}
 {| oder 'set_IFK_211_Mode' gesetzt.                                       |}
 {+------------------------------------------------------------------------+}

 t_IFC_Type = ( All_Types, No_IFC, MBC_012, IFC_old, IFC_201, IFC_203,
                IFC_211, IFC_211_FG, IFC_211_MBC, IFC_211_Sweeper,
                IFC_211_unknown, Answer_Wrong_Nr );

 t_ifc_online_str = RECORD
                      cnt:          Byte;
                      search_type:  t_IFC_Type;
                      first_adr:    Byte;
                      IFC_Type_Arr: Array [0..max_ifc] OF t_IFC_Type;
                     END;

 t_set_ifc_adr_stat = (Okay, Abbruch, No_Ifc_To_Sel);

{+--------------------------------------------------------------------------+}
{| Nachfolgend die Vereinbarungen rund um den Modul-Bus:                    |}
{+--------------------------------------------------------------------------+}
CONST
  c_mb_first_adr  = 0;
  c_mb_last_adr   = 31;
TYPE
  t_modul_adr = c_mb_first_adr..c_mb_last_adr; { Die Adresse 0 und 31 sollte nicht verwendet werden. }
                                               { Um Fehlkonfigurationen untersuchen zu k”nnen, sind  }
                                               { sie trotzdem erlaubt.                               }
CONST
  C_All_MB_Cards   = 30; { Achtung diese Konstante ist nur fr das Suchen ber alle MB-Karten definiert. }
  C_Empty_ID       = C_All_MB_Cards;
  C_Schalt_8Bit_ID = 31;
  C_io24_ID        = 32;
  C_Stat_24Bit_ID  = 33;
  C_DAC_2_ADC_ID   = 34;
  C_io32_ID        = 36;
  C_Sequencer_ID   = 37;
  C_Gpgen_ID       = 38;
  C_io64_ID        = 39;

TYPE
  t_MB_Card_Type = C_All_MB_Cards..C_Gpgen_ID;
TYPE
  t_ADR_Reg= RECORD CASE BYTE OF
             1: ( W: WORD; );
             2: ( R: PACKED RECORD
                       ADR:  t_modul_adr;
                       SCAL: BYTE;
                     END; );
             END;

   t_ID_Reg=  RECORD CASE BYTE OF
              1: ( W: WORD; );
              2: ( R: PACKED RECORD
                        VG_ID:  t_MB_Card_Type;
                        MOD_ID: t_MB_Card_Type;
                      END; );
               END;

   t_Epld_Reg=  RECORD CASE BYTE OF
                1: ( W: WORD; );
                2: ( R: PACKED RECORD
                          EPLD_ID: BYTE;
                          Frei_ID: BYTE;
                        END; );
                END;

  t_Card_Conf_Stat = SET OF ( Card_Conf_OK, Card_Adr_Not_OK, Card_ID_Not_Match, EPLD_Vers_0, Card_Scal_Not_OK, Empty );

  t_io32_APK = RECORD CASE BYTE OF
               1: ( APK_ID:     BYTE;
                    APK_Stat:   BYTE; );
               2: ( W: WORD );
          END;

  t_card_str = RECORD
                 Card_Conf_Stat: t_Card_Conf_Stat;
                 ADR:        t_ADR_Reg;
                 Epld:       t_Epld_Reg;
                 Mod_ID:     t_MB_Card_Type;
                 CASE VG_ID: t_MB_Card_Type OF
                   C_io32_ID: (
                              APK0: t_io32_APK;
                              APK1: t_io32_APK;
                              );
                END;

TYPE
  t_MB_Search_Stat = ( Search_OK, No_IFC_Online, Wrong_IFC_Type );

  t_mb_str = RECORD
               MBC_Adr:        BYTE;
               MB_Search_Stat: t_MB_Search_Stat;
               Glob_Conf_Stat: t_Card_Conf_Stat; { hier ist sind die Einzel Card_Conf_Stat als }
                                                 { 'Summe' zusammen gefasst.                   }
               MB_Search_Type: t_MB_Card_Type;
               Card_Cnt: 0..c_mb_last_adr+1;     { Modul-Adr 0..31 => 1..32 Karten }
               Arr: array [t_modul_adr] OF t_card_str;
             END;

t_Comp_Result = SET OF ( Card_cnt_Not_Eq, Types_Not_Match, Search_Not_OK, ADR_Not_Eq, VG_ID_Not_Eq,
                       MOD_ID_Not_Eq, Scal_Not_Eq, Epld_Vers_Not_Eq, Modul_Spec_Diff );

CONST
  {---------------------------------}
  { global subadresses for MB-Cards }
  {---------------------------------}
  C_ModAdr_ID  = $FE;   { sub address to read module ID }
  C_ModAdr_Scal= $FC;   { sub address to read module scaling }
  C_ModAdr_EPLD= $FA;   { sub address to read module EPLD version }

CONST
  {------------------------------}
  { sub-addresses for 32-Bit-I/O }
  {------------------------------}
  c_io32_kanal_0 = $00; { Address of Kanal 0 read/write }
  c_io32_kanal_1 = $02; { Address of Kanal 1 read/write }
  c_io32_rd_out0 = $04; { Address of Kanal 0 read Output register }
  c_io32_rd_out1 = $06; { Address of Kanal 1 read Output register }
  c_io32_status1 = $10; { Address of Status-Register and INR-Mask Kanal 0 }
  c_io32_status2 = $12; { Address of Status-Register and INR-Mask Kanal 1 }
  c_io32_sumstat = $14; { Address of Summen-Status-Register Kanal 0 u. 1 }
  c_io32_stsAPK0 = $16; { Address of Status-Register Anpasskarte Kanal 0 }
  c_io32_stsAPK1 = $18; { Address of Status-Register Anpasskarte Kanal 1 }
  c_io32_reset   = $20; { Address of Reset MB32 }
  {-----------------------------------------}
  { Skalierungs-Konstanten fuers 32-Bit-I/O }
  {-----------------------------------------}
  C_io32_Mask_Port_Dir = $C0; { Maskiert die 2 Bit der Richtungsvorgabe im Scalierungsbyte. }
  C_io32_K0_Out_K1_Out = $00; { Scalbit[7,6] = 00 => beide Kanaele auf Ausgang.             }
  C_io32_K0_In_K1_Out  = $40; { Scalbit[7,6] = 01 => Kanal0 = Eingang, Kanal1 = Ausgang.    }
  C_io32_K0_Out_K1_In  = $80; { Scalbit[7,6] = 10 => Kanal0 = Ausgang, Kanal1 = Eingang.    }
  C_io32_K0_In_K1_In   = $C0; { Scalbit[7,6] = 11 => beide Kanaele auf Eingang.             }
  C_io32_K1_In_0out    = $80; { Scalbit[7] = 1 => Kanal1 = Eingang;  = 0 => Ausgang.        }
  C_io32_K0_In_0out    = $40; { Scalbit[6] = 1 => Kanal0 = Eingang;  = 0 => Ausgang.        }

  C_io32_APK_0stecker  = $20; { Scalbit[5] = 1 => APK_ID;  = 0 => Stecker_IO.               }

  C_io32_16bit_032bit  = $10; { Scalbit[4] = 1 => 16 Bit-Mode;  = 0 => 32 Bit-Mode.         }

  C_io32_Mask_K1_Mode  = $0C; { Maskiert die 2 Bit der Mode-Vorgabe von Kanal1 im Skal-Reg. }
  C_io32_Mask_K0_Mode  = $03; { Maskiert die 2 Bit der Mode-Vorgabe von Kanal0 im Skal-Reg. }
  C_io32_K1_M_Standard = $0C; { K1-Mode = Standard-Lesen => rd_ackn; -Schreiben wr_str.     }
  C_io32_K1_M_Handshake= $08; { K1-Mode = Handshake entweder fuer Lesen oder Schreiben.     }
  C_io32_K1_M_Ext_Rd_T = $04; { K1-Mode = Schreiben mit externer Best„tigung.               }
  C_io32_K1_Mode_0     = $00; { K1-Mode ... ist noch frei ...                               }
  C_io32_K0_M_Standard = $03; { K0-Mode = Standard-Lesen => rd_ackn; -Schreiben wr_str.     }
  C_io32_K0_M_Handshake= $02; { K0-Mode = Handshake entweder fuer Lesen oder Schreiben.     }
  C_io32_K0_M_Ext_Rd_T = $01; { K0-Mode = Schreiben mit externer Best„tigung.               }
  C_io32_K0_Mode_0     = $00; { K0-Mode ... ist noch frei ...                               }

  {-----------------------------------------}
  { APK-Konstanten des 32-Bit-I/O           }
  {-----------------------------------------}
  C_io32_APK_not_set           = $00;
  C_io32_APK_BDU1              = $01;
  C_io32_APK_BDU2              = $02;
  C_io32_APK_BDU3              = $03;
  C_io32_APK_ADI               = $04;

  C_io32_APK_OUT50_Alt         = $07;
  C_io32_APK_OUT50_Pos_Led_on  = $30;
  C_io32_APK_OUT50_Pos_Led_off = $31;
  C_io32_APK_OUT50_Low_Led_on  = $32;
  C_io32_APK_OUT50_Low_Led_off = $33;


CONST
  c_pc_mil_nicht_da       = $a55a;
  c_pc_mil_da             = $5aa5;
  c_pc_mil_nicht_geprueft = $55aa;
  PCIMilCardNr            = 1;

  glob_var_pc_mil_da: LONGINT = c_pc_mil_nicht_geprueft;


TYPE
 t_Mil_Unit =
   OBJECT
     PROCEDURE pc_mil_res;
     PROCEDURE pc_mil_da;

     //PROCEDURE timer1_set( time: WORD );  { Startet Timer1 time*10us    }
     //FUNCTION  timeout1  : BOOLEAN;       { Testet Timer1 auf Timeout   }
     //PROCEDURE timer1_wait(time: WORD);   { Wartet mit Timer1 time*10us }

     PROCEDURE timer2_set( time: LONGINT );  { Startet Timer2:                }
                                             { time <= $FFFF dann time*10us   }
                                             { time >  $FFFF dann time/100*ms }
     FUNCTION  timeout2  : BOOLEAN;          { Testet Timer2 auf Timeout      }
     PROCEDURE timer2_wait ( time: LONGINT ); { Wartet mit Timer2:             }
                                              { time <= $FFFF dann time*10us   }
                                              { time >  $FFFF dann time/100*ms }

     PROCEDURE wr_fc ( Adr, Fct: Byte;
                       VAR mil_err: t_mil_err );

     PROCEDURE rd ( VAR mil_data: WORD;
                    Adr, Fct: Byte;
                    VAR mil_err: t_mil_err );

     PROCEDURE wr ( data: WORD;
                    Adr, Fct : Byte;
                    VAR mil_err: t_mil_err );

     FUNCTION ifc_da ( ifk_nr: byte ) : t_ifc_type;

//     PROCEDURE set_IFK_211_Mode ( ifk_nr: BYTE; IFK_211_Sollmode: t_IFC_Type;
                                 // VAR IFK_211_Istmode: t_IFC_type; VAR result: t_result );

     PROCEDURE set_ifk_array ( VAR ifc_online_str: t_ifc_online_str;
                               search_type: t_IFC_Type );

     PROCEDURE disp_ifc_type ( ifc_type: t_ifc_type; color: BYTE );

     PROCEDURE disp_ifk_array ( X_POS, Y_POS: BYTE;
                                ifc_online_str: t_ifc_online_str );

     PROCEDURE set_ifc_adr ( X_POS, Y_POS: BYTE;
                             VAR ifc_adr: BYTE;
                             ifc_online_str: t_ifc_online_str;
                             VAR set_ifc_adr_stat: t_set_ifc_adr_stat );

     //FUNCTION fifo_empty : BOOLEAN;

     PROCEDURE clear_fifo ( VAR mil_err: t_mil_err );

     PROCEDURE modulbus_adr_set( sub_adr, mod_adr: BYTE;
                                 modulbus_cntrl_adr: BYTE;
                                 VAR mil_err: t_mil_err );

     PROCEDURE modulbus_wr( data: WORD;
                            modulbus_cntrl_adr: BYTE;
                            VAR mil_err: t_mil_err );

     PROCEDURE modulbus_rd( VAR data: WORD;
                            modulbus_cntrl_adr: BYTE;
                            VAR mil_err: t_mil_err );

     PROCEDURE modulbus_adr_rd( VAR data: WORD;
                                sub_adr, mod_adr: BYTE;
                                modulbus_cntrl_adr: BYTE;
                                VAR mil_err: t_mil_err );

     PROCEDURE modulbus_adr_wr( data: WORD;
                                sub_adr, mod_adr: BYTE;
                                modulbus_cntrl_adr: BYTE;
                                VAR mil_err: t_mil_err );

   PROCEDURE mb_search ( Mil_Adr: BYTE;
                         VAR MB_Str: t_MB_Str;
                         MB_Card_Type: t_MB_Card_Type );

   PROCEDURE disp_mb_card_type ( MB_Card_Type: t_MB_Card_Type;
                                 Color: Byte );

   PROCEDURE disp_mb_card_arr( Y_POS, COL: BYTE;
                               MB_Str: t_MB_Str );

   PROCEDURE comp_mb_card_arr( VAR MB_Str:  t_MB_Str;
                               Comp_MB_Str: t_MB_Str;
                               VAR Comp_Result: t_Comp_Result;
                               MB_Card_Type: t_MB_Card_Type );

   PROCEDURE disp_comp_result ( Comp_result: t_Comp_result; color: Byte );

   PROCEDURE clear_mb_str ( VAR mb_str: t_mb_str );

   PROCEDURE err_io ( mil_err: t_mil_err );

   FUNCTION err_string ( mil_err: t_mil_err ) : STRING;

   END; { t_Mil_Unit }

IMPLEMENTATION   { Teil der UNIT-Deklaration }




PROCEDURE t_Mil_Unit.pc_mil_res;
  var
   ErrStatus: _DWORD;
  BEGIN
   PCI_PCIcardReset(Cardauswahl,ErrStatus);
 { PORTW[p_rset] := c_res;
  timer1_wait(500);   }
  END;

PROCEDURE t_Mil_Unit.pc_mil_da;
  CONST
    c_test_cnt_max = 100000;
  VAR
    test_cnt: LongInt;
  BEGIN
    if PCI_MilCardOpen then glob_var_pc_mil_da := c_pc_mil_da
     else glob_var_pc_mil_da := c_pc_mil_nicht_da;

{  timer1_set(10);
  test_cnt := 0;
  IF NOT timeout1 THEN BEGIN
    REPEAT
    test_cnt := test_cnt + 1;
    UNTIL timeout1 OR (test_cnt = c_test_cnt_max);
    IF test_cnt = c_test_cnt_max THEN
      glob_var_pc_mil_da := c_pc_mil_nicht_da
    ELSE
      glob_var_pc_mil_da := c_pc_mil_da;
    END
  ELSE
    glob_var_pc_mil_da := c_pc_mil_nicht_da;
  //$IFDEF Test_Mil
    write('test_cnt: ',test_cnt);
  {$ENDIF}
  END;

PROCEDURE t_Mil_Unit.clear_fifo ( VAR mil_err: t_mil_err );
  VAR
  dummy: LONGINT;
  dummy_rd: WORD;
  BEGIN
{  IF glob_var_pc_mil_da = c_pc_mil_da THEN BEGIN
    DUMMY := 0;
    REPEAT
      dummy := SUCC(dummy);
      dummy_rd := PORTW[p_data];
    UNTIL ((PORTW[p_stat] AND stat_ffo_ety) = stat_ffo_ety) OR (dummy = 100000);
    IF dummy = 100000 THEN
       mil_err := mil_err + [fifo_not_cleared]
    ELSE
       mil_err := mil_err - [fifo_not_empty];
    END
  ELSE
    mil_err := mil_err + [no_pc_mil];  }
  END;                                 


{FUNCTION t_Mil_Unit.fifo_empty : BOOLEAN;
  Begin
  //fifo_empty := ((PORTW[p_stat] AND stat_ffo_ety) = stat_ffo_ety);
  End;


FUNCTION t_Mil_Unit.timeout1 : BOOLEAN;
  Begin
  //timeout1 := PORTW[p_stat] AND stat_timeout1 = stat_timeout1;
  End;

 PROCEDURE t_Mil_Unit.timer1_set ( time: WORD );  //setzt u. startet den Timer: 10 us Ticks
  Begin
  //PORTW[p_tmr1] := time;
  End;   }

{
PROCEDURE t_Mil_Unit.timer1_wait ( time: WORD );
  BEGIN
  {PORTW[p_tmr1] := time;
  REPEAT UNTIL (PORTW[p_stat] AND stat_timeout1) = stat_timeout1;
  END;                                                            }

FUNCTION t_Mil_Unit.timeout2 : BOOLEAN;
  var ErrStatus:_DWORD;
  Begin
    If (PCI_StatusTest(Cardauswahl, Timeout_2, ErrStatus)) then
     Timeout2 := True
    else Timeout2 := False;
  End;

PROCEDURE t_Mil_Unit.timer2_set ( time: LONGINT ); { setzt u. startet den Timer:    }
  var Errstatus:_DWORD;                            { time <= $FFFF dann time*10us   }
                                                   { time >  $FFFF dann time/100*ms }
  Begin
    if Time <= $FFFF then begin
      PCI_TimerSet(Cardauswahl, Word(Time), 0, ErrStatus);
    end else begin
      PCI_TimerSet(Cardauswahl, Word(Time div 100), 1, ErrStatus);
    end;
{  IF time <= $FFFF THEN
    BEGIN
    PORTW[p_tmr2_1ms] := 0;
    PORTW[p_tmr2] := WORD(time);
    END
  ELSE
    BEGIN
    PORTW[p_tmr2_1ms] := 1;
    PORTW[p_tmr2] := WORD(time DIV 100);
    END;  }
  End;

PROCEDURE t_Mil_Unit.timer2_wait ( time: LONGINT );
 var ErrStatus:_DWORD;

  begin
    if Time <= $FFFF then begin
      PCI_TimerWait(Cardauswahl, Word(Time), 0, ErrStatus);
    end else begin
      PCI_TimerWait(Cardauswahl, Word(Time div 100), 1, ErrStatus);
    end;
{  IF time <= $FFFF THEN
    BEGIN
    PORTW[p_tmr2_1ms] := 0;
    PORTW[p_tmr2] := WORD(time);
    END
  ELSE
    BEGIN
    PORTW[p_tmr2_1ms] := 1;
    PORTW[p_tmr2] := WORD(time DIV 100);
    END;
  REPEAT UNTIL (PORTW[p_stat] AND stat_timeout2) = stat_timeout2;  }
  END;

PROCEDURE t_Mil_Unit.wr_fc ( Adr, Fct : Byte;
                             VAR mil_err: t_mil_err );
  VAR
    time_out:   BOOLEAN;
    Adr_FC:     t_Adr_FC;
    ErrStatus: _DWORD;
    status   : _DWORD;

  BEGIN
   Adr_FC.r.Adr := adr;
   Adr_FC.r.FC  := fct;

   status:= PCI_MilBusCMDWrite(Cardauswahl, Adr_FC.w, ErrStatus);
   if(Status <> StatusOK) then mil_err:= mil_err + [fc_to];

{  IF glob_var_pc_mil_da = c_pc_mil_da THEN BEGIN
    PORTW[p_tmr1] := c_wr_rdy_time;
    REPEAT
      Status := PORTW[p_stat];
    UNTIL (Status AND (stat_wrt_rdy OR stat_timeout1)) <> 0;
    IF (Status AND stat_wrt_rdy) = stat_wrt_rdy THEN
      BEGIN
      PORTW[p_mode] := cmd_mode;
      Adr_FC.r.Adr := adr;
      Adr_FC.r.FC  := fct;
      PORTW[p_data] := Adr_FC.w; // Funktioncode schreiben
      END
    ELSE
      mil_err := mil_err + [fc_to];
    END
  ELSE
    mil_err := mil_err + [no_pc_mil];  }
  END; { wr_fc }

PROCEDURE t_Mil_Unit.rd ( VAR mil_data: WORD;
                          Adr, Fct: Byte;
                          VAR mil_err: t_mil_err );
  VAR
    time_out:   BOOLEAN;
    Adr_FC:     t_Adr_FC;
    ErrStatus: _DWORD;
    Status   : _DWORD;

    BEGIN
    Status:= PCI_IfkRead(Cardauswahl, Adr, Fct, mil_data, ErrStatus);
    if(Status <> StatusOK) then mil_err:= mil_err +  [rd_to];


{  IF glob_var_pc_mil_da = c_pc_mil_da THEN BEGIN
    //---------------------------------
    // Zuerst testen ob FIFO leer ist.
    //---------------------------------
    IF fifo_empty THEN
      BEGIN
      //--------------------------------------------------
      // Zuerst muss der Funktionscode geschrieben werden
      //--------------------------------------------------
      PORTW[p_tmr1] := c_wr_rdy_time;
      REPEAT
        Status := PORTW[p_stat];
      UNTIL (Status AND (stat_wrt_rdy OR stat_timeout1)) <> 0;
      IF (Status AND stat_wrt_rdy) = stat_wrt_rdy THEN
        BEGIN
        PORTW[p_mode] := cmd_mode;
        Adr_FC.r.Adr := adr;
        Adr_FC.r.FC  := fct;
        PORTW[p_data] := Adr_FC.w; // Funktionscode schreiben
        //-------------------------------
        // Dann werden die Daten gelesen
        //-------------------------------
        PORTW[p_tmr1] := c_val_wrd_time;
        REPEAT
          Status := PORTW[p_stat];
        UNTIL (Status AND (stat_val_wrd OR stat_timeout1)) <> 0;
        IF (Status AND stat_val_wrd) = stat_val_wrd THEN
          BEGIN
          mil_data := PORTW[p_data];  //Mil-Daten lesen
          END
        ELSE
          mil_err := mil_err + [rd_to];
        END
      ELSE
        mil_err := mil_err + [fc_to];
      END
    ELSE
      mil_err := mil_err + [fifo_not_empty];
    END
  ELSE
    mil_err := mil_err + [no_pc_mil];   }
  END; { rd }


PROCEDURE t_Mil_Unit.wr( data: WORD;
                         Adr, Fct : Byte;
                         VAR mil_err: t_mil_err );

  VAR
    Adr_FC:     t_Adr_fc;
    ErrStatus: _DWORD;
    Status   : _DWORD;


  BEGIN
   Status:= PCI_IfkWrite(Cardauswahl, Adr, Fct, Data, ErrStatus);
   if(Status <> StatusOK) then mil_err := mil_err + [wr_to];

{  IF glob_var_pc_mil_da = c_pc_mil_da THEN BEGIN
    PORTW[p_tmr1] := c_wr_rdy_time;
    REPEAT
      Status := PORTW[p_stat];
    UNTIL ((Status AND (stat_wrt_rdy OR stat_timeout1)) <> 0);
    IF (Status AND stat_wrt_rdy) = stat_wrt_rdy THEN
      BEGIN
      PORTW[p_mode] := dta_mode;
      PORTW[p_data] := data; // Mil-Daten schreiben
      //--------------------------------------
      // Dann wird der Funktionscode gesendet
      //--------------------------------------
      PORTW[p_tmr1] := c_wr_rdy_time;
      REPEAT
        Status := PORTW[p_stat];
      UNTIL (Status AND (stat_wrt_rdy OR stat_timeout1)) <> 0;
      IF (Status AND stat_wrt_rdy) = stat_wrt_rdy THEN
        BEGIN
        PORTW[p_mode] := cmd_mode;
        Adr_FC.r.Adr := adr;
        Adr_FC.r.FC  := fct;
        PORTW[p_data] := Adr_FC.w; // Mil-Funktioncode schreiben
        END
      ELSE
        mil_err := mil_err + [fc_to];
      END
    ELSE
      mil_err := mil_err + [wr_to];
    END
  ELSE
    mil_err := mil_err + [no_pc_mil]; }
  END; { wr }

FUNCTION t_Mil_Unit.ifc_da ( ifk_nr: byte ) : t_ifc_type;
  VAR
    status:       WORD;
    ifk_mode:     WORD;
    mil_err:      t_mil_err;
  BEGIN
  mil_err := [];
  rd(status, ifk_nr, c_fc_stat, mil_err);
  IF fifo_not_empty IN mil_err THEN
    clear_fifo(mil_err);
  IF mil_err = [] THEN BEGIN
    IF (status AND $00FF) = ifk_nr THEN BEGIN
      rd(status, ifk_nr, c_fc_ifk_id, mil_err);
      IF mil_err = [] THEN BEGIN
        IF (status AND $00FF) = ifk_nr THEN
          IFC_DA := IFC_old
        ELSE
          CASE status OF
            $FB00:
                BEGIN
                rd( Ifk_mode, ifk_nr, c_fc_rd_ifk_mode, mil_err );
                IF mil_err = [] THEN BEGIN
                  CASE IFK_MODE OF
                      m_IFC_211:         IFC_DA := IFC_211;
                      m_IFC_211_FG:      IFC_DA := IFC_211_FG;
                      m_IFC_211_MBC:     IFC_DA := IFC_211_MBC;
                      m_IFC_211_Sweeper: IFC_DA := IFC_211_Sweeper;
                    ELSE
                      IFC_DA := IFC_211_unknown;
                    END; // CASE
                  END // IF mil_err = [] THEN
                ELSE
                  IFC_DA := NO_IFC;
                END; // CASE $FB00
            $FC00: IFC_DA := IFC_203;
            $FD00: IFC_DA := MBC_012;
            $FE00: IFC_DA := IFC_201;
            ELSE
              IFC_DA := IFC_old;
            END; // CASE status
         END
      ELSE
        IFC_DA := No_IFC;
      END // IF (status AND $00FF) = ifk_nr THEN...
    ELSE
      IFC_DA := Answer_Wrong_Nr;
    END  //IF mil_err = [] THEN...
  ELSE
    IFC_DA := No_IFC;
  END;

{PROCEDURE t_Mil_Unit.set_IFK_211_Mode ( ifk_nr: BYTE; IFK_211_Sollmode: t_IFC_Type;
                                        VAR IFK_211_Istmode: t_IFC_type; VAR result: t_result );
  VAR
    mil_err: t_mil_err;
  BEGIN
  mil_err := [];
  IFK_211_Istmode := ifc_da(ifk_nr);
  IF IFK_211_Sollmode <> IFK_211_Istmode THEN BEGIN
    IF ( IFK_211_Sollmode IN [IFC_211, IFC_211_FG, IFC_211_MBC, IFC_211_Sweeper] )
        AND ( IFK_211_Istmode IN [IFC_211, IFC_211_FG, IFC_211_MBC, IFC_211_Sweeper] ) THEN BEGIN
      CASE IFK_211_Sollmode OF
        IFC_211:         wr(WORD(m_IFC_211), ifk_nr, c_fc_wr_ifk_mode, mil_err);
        IFC_211_FG:      wr(WORD(m_Ifc_211_fg), ifk_nr, c_fc_wr_ifk_mode, mil_err);
        IFC_211_MBC:     wr(WORD(m_Ifc_211_mbc), ifk_nr, c_fc_wr_ifk_mode, mil_err);
        IFC_211_Sweeper: wr(WORD(m_Ifc_211_sweeper), ifk_nr, c_fc_wr_ifk_mode, mil_err);
        END; // CASE
      IFK_211_Istmode := ifc_da(ifk_nr);
      IF IFK_211_Sollmode = IFK_211_Istmode THEN
        result := ok
      ELSE
        result := not_ok;
      END
    ELSE BEGIN
      result := not_ok;
      END;
    END
  ELSE BEGIN
    result := ok;
    END;
  END;              }

PROCEDURE t_Mil_Unit.set_ifk_array ( VAR ifc_online_str: t_ifc_online_str;
                                     search_type: t_IFC_Type );
  VAR
    ifc_adr    : WORD;
    IFC_Type   : t_IFC_Type;

  Begin
  ifc_online_str.cnt         := 0;            // Z„hler der Online-IFCs.
  ifc_online_str.search_type := search_type;  // Bei sp„teren Auswertungen ist klar
                                              // welche Eintragungen zu erwarten sind.
  ifc_online_str.first_adr   := max_ifc+1;    // Wenn <> max_ifc+1 => zuerst gefundene Ifc_Adr.
  FOR ifc_adr := 0 TO max_ifc DO              // Fill Array with actual data
    BEGIN
    ifc_online_str.ifc_type_arr[ifc_adr] := NO_IFC;
    IFC_Type := ifc_da(ifc_adr);
    IF ((search_type = All_Types) OR (IFC_Type = search_type))
       AND NOT (IFC_Type = NO_IFC) THEN BEGIN
      ifc_online_str.ifc_type_arr[ifc_adr] := IFC_Type;
      ifc_online_str.cnt := succ(ifc_online_str.cnt);
      IF ifc_online_str.first_adr = max_ifc+1 THEN // Wurde first_adr schon mal gesetzt ?
          ifc_online_str.first_adr := ifc_adr;     // Nein, aber jetzt.
      END;
    END;
  END;

PROCEDURE t_Mil_Unit.disp_ifc_type ( ifc_type: t_ifc_type; color: BYTE );
  VAR
    old_text_color: BYTE;
  BEGIN
  old_text_color := TextAttr;
  Textcolor(color);
  CASE ifc_type OF
    All_Types:       Write('All Types');
    IFC_OLD:         Write('IFC_old');
    MBC_012:         Write('MBC_012');
    IFC_201:         Write('IFC_201');
    IFC_203:         Write('IFC_203');
    IFC_211:         Write('IFC_211');
    IFC_211_FG:      Write('FG__211');
    IFC_211_MBC:     Write('MBC_211');
    IFC_211_Sweeper: Write('SWE_211');
    ELSE
      Write('Unknown');
    END;
  TextAttr := old_text_color;
  END;

PROCEDURE t_Mil_Unit.set_ifc_adr ( X_POS, Y_POS: BYTE;
                                   VAR ifc_adr: BYTE;
                                   ifc_online_str: t_ifc_online_str;
                                   VAR set_ifc_adr_stat: t_set_ifc_adr_stat );
  VAR
    input_ok: BOOLEAN;
    answer: CHAR;

  BEGIN
  CASE ifc_online_str.cnt OF
    0 : BEGIN
          ifc_adr := 0;
          set_ifc_adr_stat := No_Ifc_TO_Sel;
        END;
    1 : BEGIN
          REPEAT
          Gotoxy(x_pos, y_pos);
          ifc_adr := ifc_online_str.first_adr;
          write('Soll mit IFK Nr: '+conv.hex_byte(ifc_adr)+' getestet werden (y/n) ? ');
          Readln(answer);
          answer := upcase(answer);
          UNTIL answer IN ['Y','J','N'];
          IF answer = 'N' THEN
            set_ifc_adr_stat := abbruch
          ELSE
            set_ifc_adr_stat := Okay;
        END;
    2..254 : BEGIN
             REPEAT
               REPEAT
                 REPEAT
                 Gotoxy(x_pos, y_pos);
                 Write('Bitte Adresse auswaehlen (ff = Abbruch) --> '); CLREOL;
                 conv.read_hexbyte(ifc_adr, input_ok);
                 UNTIL input_ok;
               GOTOXY(x_pos+47, y_pos);
               Write(' Okay ? (Y, N, X = Abbruch) '); CLREOL;
               READLN(answer);
               answer := upcase(answer);
               UNTIL answer IN ['Y','J','X'];
               IF answer IN ['Y','J'] THEN BEGIN
                 IF ifc_adr = $FF THEN
                   answer := 'X'
                 ELSE BEGIN
                   IF ifc_online_str.IFC_Type_arr[ifc_adr] <> NO_IFC THEN
                     answer := 'Y'
                   ELSE
                     answer := ' ';
                   END;
                 END;
             UNTIL answer IN ['Y','J','X'];
             IF answer = 'X' THEN
               set_ifc_adr_stat := abbruch
             ELSE
               set_ifc_adr_stat := Okay;
             END;
    END; // CASE
  END;

PROCEDURE t_Mil_Unit.disp_ifk_array ( X_POS, Y_POS: BYTE;
                                      ifc_online_str: t_ifc_online_str );
  VAR
    save_text_attr: BYTE;
    ifc_adr, i:     WORD;
  BEGIN
  save_text_attr := TextAttr; // Vordefinierte globale Variable von Unit CRT
  gotoxy(x_pos, y_pos);
  CASE ifc_online_str.cnt OF
    0: BEGIN
       Write('Es ist '); Textcolor(red);
       Write('keine');
       TextAttr := save_text_attr;
       Write(' Karte ');
       IF ifc_online_str.search_type <> All_Types THEN BEGIN
         Write('vom Type ');
         disp_ifc_type(ifc_online_str.search_type, blue);
         write(' ');
         END;
       writeln('online.');
       END;
    1: BEGIN
       Write('Es ist eine Karte ');
       IF ifc_online_str.search_type <> All_Types THEN BEGIN
         Write('vom Type ');
         disp_ifc_type(ifc_online_str.search_type, blue);
         END;
       writeln(' online. Adresse = ', conv.hex_byte(ifc_online_str.first_adr));
       clreol;
       GOTOXY(1,wherey+1);
       clreol;
       END;
    2..max_ifc:
       BEGIN
       Write('Es sind ', ifc_online_str.cnt:0,' Karten ');
       IF ifc_online_str.search_type <> All_Types THEN BEGIN
         Write('vom Type ');
         disp_ifc_type(ifc_online_str.search_type, blue);
         END
       ELSE
         write('(alle Typen)');
       Writeln(' online.');
       Writeln('Ab der n„chsten Zeile stehen die Adressen:');
       i := 0;
       ifc_adr := 0;
       WHILE i < ifc_online_str.cnt DO BEGIN
         ifc_adr := succ(ifc_adr);
         IF (ifc_online_str.ifc_type_arr[ifc_adr] <> NO_IFC)
            AND (ifc_online_str.ifc_type_arr[ifc_adr] <> ALL_TYPES) THEN BEGIN
           GotoXY( ((i) MOD 6)*13 + X_POS,
                   (i) DIV 6 + Y_POS+2);
           Write(conv.hex_byte(ifc_adr),'=');
           disp_ifc_type(ifc_online_str.ifc_type_arr[ifc_adr], blue);
           i := i + 1;
           END;
         END;
       END;
    END;
  END;

PROCEDURE t_Mil_Unit.modulbus_adr_set( sub_adr, mod_adr: BYTE;
                                       modulbus_cntrl_adr: BYTE;
                                       VAR mil_err: t_mil_err );
  VAR
    address: RECORD CASE BYTE OF
               1: ( r: PACKED RECORD
                         lb: BYTE;
                         hb: BYTE;
                       END; );
               2: ( w: WORD );
             END;

  BEGIN
  address.r.lb := sub_adr;
  address.r.hb := mod_adr;
  wr( address.w, modulbus_cntrl_adr, c_fc_mod_adr_set, mil_err );
  END;

PROCEDURE t_Mil_Unit.modulbus_wr( data: WORD;
                                  modulbus_cntrl_adr: BYTE;
                                  VAR mil_err: t_mil_err );

  BEGIN
   wr( data, modulbus_cntrl_adr, c_fc_mod_wr, mil_err );
  END;


PROCEDURE t_Mil_Unit.modulbus_rd( VAR data: WORD;
                                  modulbus_cntrl_adr: BYTE;
                                  VAR mil_err: t_mil_err );

  BEGIN
   rd( data, modulbus_cntrl_adr, c_fc_mod_rd, mil_err );
  END;

PROCEDURE t_Mil_Unit.modulbus_adr_rd( VAR data: WORD;
                                      sub_adr, mod_adr: BYTE;
                                      modulbus_cntrl_adr: BYTE;
                                      VAR mil_err: t_mil_err );
  BEGIN
  modulbus_adr_set( sub_adr, mod_adr, modulbus_cntrl_adr, mil_err );
  IF mil_err = [] THEN
    modulbus_rd( data, modulbus_cntrl_adr, mil_err );
  END;

PROCEDURE t_Mil_Unit.modulbus_adr_wr( data: WORD;
                                      sub_adr, mod_adr: BYTE;
                                      modulbus_cntrl_adr: BYTE;
                                      VAR mil_err: t_mil_err );
  BEGIN
  modulbus_adr_set( sub_adr, mod_adr, modulbus_cntrl_adr, mil_err );
  IF mil_err = [] THEN
    modulbus_wr( data, modulbus_cntrl_adr, mil_err );
  END;

PROCEDURE t_Mil_Unit.mb_search ( Mil_Adr: BYTE;
                                 VAR MB_Str: t_MB_Str;
                                 MB_Card_Type: t_MB_Card_Type );
  VAR
    IFC_Type: t_IFC_Type;

    ID_Reg:   t_ID_Reg;
    ADR_Reg:  t_ADR_Reg;
    EPLD_Reg: t_Epld_Reg;

    mil_err:  t_mil_err;

  PROCEDURE init_mb_str_head;
    BEGIN
    MB_Str.MBC_Adr        := Mil_Adr;
    MB_Str.MB_Search_Type := MB_Card_Type;
    MB_Str.Card_Cnt       := 0;
    MB_Str.Glob_Conf_Stat := [];   
    END;

  PROCEDURE set_mb_card_arr;
    VAR
      Modul_Adr: t_Modul_Adr;

    PROCEDURE Test_MB_Card_Spec( VAR MB_Str: t_MB_Str; Modul_Adr: t_Modul_Adr );
      VAR
        io32_APK1, io32_APK0: t_io32_APK;
      BEGIN
      CASE MB_Str.arr[Modul_Adr].VG_ID OF
        C_io32_ID:
          BEGIN
          modulbus_adr_rd(io32_APK1.w, C_io32_stsAPK1, Modul_Adr, Mil_Adr, mil_err);
          modulbus_adr_rd(io32_APK0.w, C_io32_stsAPK0, Modul_Adr, Mil_Adr, mil_err);
          IF mil_err = [] THEN BEGIN
            MB_Str.arr[Modul_Adr].APK1 := io32_APK1;
            MB_Str.arr[Modul_Adr].APK0 := io32_APK0;
            END
          ELSE BEGIN
            MB_Str.arr[Modul_Adr].APK1.APK_ID := C_io32_APK_not_set;
            MB_Str.arr[Modul_Adr].APK0.APK_ID := C_io32_APK_not_set;
            END;
          END; // CASE C_io_32_ID
        END; // CASE
      END;

    BEGIN
    init_mb_str_head;
    FOR Modul_Adr := c_mb_first_adr to c_mb_last_adr DO BEGIN
      mil_err := [];
      modulbus_adr_rd(ID_Reg.w, C_ModAdr_ID, Modul_Adr, Mil_Adr, mil_err);
      IF mil_err = [] THEN
        modulbus_adr_rd(ADR_Reg.w, C_ModAdr_Scal, Modul_Adr, Mil_Adr, mil_err);
      IF mil_err = [] THEN
         modulbus_adr_rd(Epld_Reg.w, C_ModAdr_EPLD, Modul_Adr, Mil_Adr, mil_err);
      IF mil_err = [] THEN BEGIN
        IF (MB_Card_Type = C_All_MB_Cards) OR (MB_Card_Type = ID_Reg.r.Mod_ID) THEN
          BEGIN
          MB_Str.Card_Cnt := MB_Str.Card_Cnt + 1;
          MB_Str.Arr[Modul_Adr].Card_Conf_Stat := []; // Den [empty]-Eintrag loeschen
          MB_Str.Arr[Modul_Adr].Mod_ID   := ID_Reg.r.Mod_ID;
          MB_Str.Arr[Modul_Adr].VG_ID    := ID_Reg.r.VG_ID;
          MB_Str.Arr[Modul_Adr].Adr  := ADR_Reg;
          MB_Str.Arr[Modul_Adr].EPLD := Epld_Reg;
          WITH MB_Str.Arr[Modul_Adr] DO BEGIN
            IF EPLD_Reg.r.EPLD_Id = 0 THEN
              Card_Conf_Stat := Card_Conf_Stat + [EPLD_Vers_0];
            IF ID_Reg.r.VG_ID <> ID_Reg.r.MOD_ID THEN
              Card_Conf_Stat := Card_Conf_Stat + [Card_ID_Not_Match];
            //--------------------------------------------------
            // Kontrolle ob die Modul_ADR gleich 0 oder 31 ist.
            // Die Adressen sind zwar nicht verboten, sollten
            // aber nicht verwendet werden.
            //--------------------------------------------------
            IF (Modul_Adr = c_mb_first_adr) OR (Modul_Adr = c_mb_last_adr) THEN
              Card_Conf_Stat := Card_Conf_Stat + [Card_Adr_Not_OK];
            IF Card_Conf_Stat = [] THEN
              Card_Conf_Stat := [Card_Conf_OK];
            MB_Str.Glob_Conf_Stat := MB_Str.Glob_Conf_Stat + Card_Conf_Stat;
          END;
          IF MB_Str.Arr[Modul_Adr].Card_Conf_Stat * [Card_ID_Not_Match] = [] THEN
            Test_MB_Card_Spec( MB_Str, Modul_Adr );
          END; // WITH MB_Str.Arr[Modul_Adr] DO BEGIN
        END // IF mil_err = [] THEN BEGIN
      ELSE BEGIN
        WITH MB_Str.Arr[Modul_Adr] DO BEGIN
          Card_Conf_Stat := [Empty];
          Adr.w          := 0;
          Mod_ID         := C_Empty_ID;
          VG_ID          := C_Empty_ID;
          EPLD.w         := 0;
          END;
        END;
      END;
      IF MB_Str.Card_Cnt = 0 THEN
        MB_Str.Glob_Conf_Stat := [Empty];
      MB_Str.MB_Search_Stat := Search_OK;
    END;

  BEGIN // PROCEDURE set_mb_card_arr
  IFC_Type := ifc_da(Mil_Adr);
  CASE IFC_Type OF
    IFC_211_MBC, MBC_012:
       BEGIN
       set_mb_card_arr;
       END;
    NO_IFC:
      BEGIN
      Init_MB_Str_Head;
      MB_Str.MB_Search_Stat := NO_IFC_Online;
      END;
    ELSE
      Init_MB_Str_Head;
      MB_Str.MB_Search_Stat := Wrong_IFC_Type;
    END; // CASE
  END; // PROCEDURE set_mb_card_arr

PROCEDURE t_Mil_Unit.disp_mb_card_type ( MB_Card_Type: t_MB_Card_Type;
                                         Color: Byte );
  VAR
    old_text_color: BYTE;
  BEGIN // disp_mb_card_type
  old_text_color := TextAttr;
  Textcolor(color);
  CASE MB_Card_Type OF
    C_All_MB_Cards:   Write('All_MB_Cards');
    C_Schalt_8Bit_ID: Write('Schalt_8Bit');
    C_io24_ID:        Write('24Bit_IO');
    C_Stat_24Bit_ID:  Write('Stat_24Bit');
    C_DAC_2_ADC_ID:   Write('DAC_2_ADC');
    C_io32_ID:        Write('32Bit_IO');
    C_Sequencer_ID:   Write('Sequenzer');
    C_Gpgen_ID:       Write('Gpgen');
    ELSE
      Write('Unknown');
    END;
  TextAttr := old_text_color;
  END; // disp_mb_card_type

PROCEDURE t_Mil_Unit.disp_mb_card_arr( Y_POS, COL: Byte;
                                       MB_Str: t_MB_Str );
  VAR
    save_text_attr: BYTE;
    i: BYTE;

  PROCEDURE More_MB_Card_Info ( MB_Card_Entry: t_Card_Str );

    PROCEDURE disp_io32_APK_Type( APK: t_io32_APK );
      BEGIN
        CASE APK.APK_ID OF
          C_io32_APK_BDU1:              Write('  BDU1');
          C_io32_APK_BDU2:              Write('  BDU2');
          C_io32_APK_BDU3:              Write('  BDU3');
          C_io32_APK_ADI:               Write('  ADI ');

          C_io32_APK_OUT50_Alt:         Write('O50alt');
          C_io32_APK_OUT50_Pos_Led_on:  Write('O50PLP');
          C_io32_APK_OUT50_Pos_Led_off: Write('O50PLN');
          C_io32_APK_OUT50_Low_Led_on:  Write('O50LLP');
          C_io32_APK_OUT50_Low_Led_off: Write('O50LLN');
        ELSE
          disp.W(0,0,red,' ' + conv.hex_word(APK.w) + ' ');
          END;
      END;

    BEGIN
    CASE MB_Card_Entry.VG_ID OF
      C_io32_ID:
        BEGIN
        WITH MB_Card_Entry.Adr.r DO BEGIN
          CLREOL;
          IF Scal AND C_io32_K1_IN_0out = 0 THEN Write('K1out')
                                            ELSE Write(' K1in');
          Write(':',((Scal AND C_io32_Mask_K1_Mode) shr 2):0,'/');
          IF Scal AND C_io32_K0_IN_0out = 0 THEN Write('K0out')
                                            ELSE Write(' K0in');
          Write(':',(Scal AND C_io32_Mask_K0_Mode):0);
          IF Scal AND C_io32_APK_0stecker <> 0 THEN Write(' APK')
                                               ELSE Write(' STK');
          IF Scal AND C_io32_16bit_032bit <> 0 THEN Write(' M16B')
                                               ELSE Write(' M32B');
          IF Scal AND C_io32_APK_0stecker <> 0 THEN BEGIN
            disp_io32_APK_Type( MB_Card_Entry.APK1 );
            disp_io32_APK_Type( MB_Card_Entry.APK0 );
            END;
        END; // WITH MB_Card_Entry.Adr.r
        END; // CASE C_io32_ID

      END; // CASE

    END;

  BEGIN
  save_text_attr := TextAttr; // Vordefinierte globale Variable von Unit CRT
  gotoxy(1, y_pos);
  CASE MB_str.MB_Search_Stat OF
    Search_OK:
      BEGIN
      Write('Am Modulbus mit Adr: ', conv.hex_byte(MB_Str.MBC_Adr),'h wurde nach ');
      disp_mb_card_type(MB_str.MB_Search_Type, blue);
      Writeln(' gesucht.');
      IF mb_str.card_cnt <> 0 THEN BEGIN
        Write('Es wurden ', mb_str.card_Cnt:0,' Karten gefunden.');
        IF mb_str.glob_conf_Stat = [Card_Conf_OK] THEN BEGIN
          write(' Alle Karten richtig konfiguriert.');
          END
        ELSE BEGIN
          Write(' Achtung:');
          IF Card_Adr_Not_OK IN mb_str.glob_conf_Stat THEN
            disp.w(0,0,red,' Adr 0 od 31!');
          IF Card_ID_Not_Match IN mb_str.glob_conf_Stat THEN
            disp.w(0,0,red,' M_Id <> V_Id!');
          IF EPLD_Vers_0 IN mb_str.glob_conf_Stat THEN
            disp.w(0,0,red,' EPLD_V_0!');
          END; // mb_str.card_cnt <> 0
        Writeln;

        Writeln('  Type  ',' Adr Scal',' Conf_Stat',' VGid MODid');
        FOR i := c_mb_first_adr TO c_mb_last_adr DO BEGIN
          IF MB_Str.arr[i].card_conf_stat <> [empty] THEN BEGIN
            disp_mb_card_type(MB_Str.Arr[i].VG_ID, blue);
            Gotoxy( 11, wherey);
            Write(conv.hex_byte(MB_Str.Arr[i].Adr.r.Adr),'  ', conv.hex_byte(MB_Str.Arr[i].Adr.r.Scal));
            GotoXY( 19, Wherey);
            IF MB_Str.arr[i].Card_Conf_Stat = [Card_Conf_OK] THEN
              Write('Config_OK')
            ELSE BEGIN
              IF Card_Adr_Not_OK IN MB_Str.arr[i].Card_Conf_Stat THEN
                disp.w(0,0,red,'AD_');
              IF Card_ID_Not_Match IN MB_Str.arr[i].Card_Conf_Stat THEN
                disp.w(0,0,red,'ID_');
              IF EPLD_Vers_0 IN MB_Str.arr[i].Card_Conf_Stat THEN
                disp.w(0,0,red,'EPV');
              IF Card_Scal_Not_OK IN MB_Str.arr[i].Card_Conf_Stat THEN
                disp.w(0,0,red,'SCAL');
              END;
            GotoXY( 30, Wherey);
            Write(conv.hex_byte(MB_Str.Arr[i].VG_ID),'   ', conv.hex_byte(MB_Str.Arr[i].MOD_ID));
            GotoXY( 40, Wherey);
            IF MB_Str.Arr[i].Card_Conf_Stat * [Card_ID_Not_Match] = [] THEN
              More_MB_Card_Info(MB_Str.Arr[i]);
            Writeln;
            END; // MB_Str.arr[i].card_conf_stat <> [empty]
          END; //FOR i := c_mb_first_adr TO c_mb_last_adr
        END // mb_str.card_cnt <> 0
      ELSE BEGIN
        Writeln('Es wurde keine Karte gefunden!');
        END;
      END;
    No_IFC_Online:
      BEGIN
      Write('Die Mil-Adr: ', conv.hex_byte(MB_Str.MBC_Adr),' war nicht online!');
      END;
    Wrong_IFC_Type:
      BEGIN
      Write('Die Mil-Adr: ', conv.hex_byte(MB_Str.MBC_Adr),' war kein MB-Controller!');
      END;
    END; // CASE
  END;

PROCEDURE t_Mil_Unit.comp_mb_card_arr( VAR MB_Str:      t_MB_Str;
                                       Comp_MB_Str: t_MB_Str;
                                       VAR Comp_Result: t_Comp_Result;
                                       MB_Card_Type: t_MB_Card_Type );
  TYPE
    t_result = (okay, not_okay);
  VAR
    result:    t_result;
    Modul_Adr: Byte;

  PROCEDURE Comp_MB_Card_Spec ( MB_Card_Entry:   t_Card_Str;
                                Comp_Card_Entry: t_Card_Str;
                                VAR result: t_result );

    BEGIN
    CASE MB_Card_Entry.VG_ID OF
      C_io32_ID:
        BEGIN
        WITH MB_Card_Entry.Adr.r DO BEGIN
          IF Scal AND C_io32_APK_0stecker <> 0 THEN BEGIN
            IF (MB_Card_Entry.APK1.APK_ID <> Comp_Card_Entry.APK1.APK_ID)
               OR (MB_Card_Entry.APK0.APK_ID <> Comp_Card_Entry.APK0.APK_ID) THEN
              result := not_okay
            ELSE
              result := okay;
            END
          ELSE
            result := okay;
          END; // WITH MB_Card_Entry.Adr.r
        END; // CASE C_io32_ID
      ELSE
        result := okay;
      END; // CASE

    END;

  BEGIN
  {Beim Vergleich mit der comp_str wird zur Zeit nur bei ungleicher Skalierung}
  {im Individuellen Status 'Card_Conf_Stat' ein Fehler gesetzt.               }

  Comp_Result := [];
  IF MB_Str.MB_search_Type = MB_Card_Type THEN BEGIN
     IF [MB_Str.MB_Search_Stat] = [Search_OK] THEN BEGIN
       IF MB_Str.Card_Cnt = Comp_MB_Str.Card_Cnt THEN BEGIN
         Modul_Adr := c_mb_first_adr;
         WHILE (Modul_Adr <= c_mb_last_adr) DO BEGIN
           IF (MB_Str.arr[Modul_ADR].card_conf_stat <> [empty])
              AND (Comp_MB_Str.arr[Modul_ADR].card_conf_stat <> [empty]) THEN BEGIN
             WITH MB_Str.Arr[Modul_Adr] DO BEGIN
               IF ADR.r.ADR <> Comp_MB_Str.Arr[Modul_Adr].ADR.r.ADR THEN
                 Comp_Result := Comp_Result + [Adr_Not_Eq]
               ELSE
                 IF ADR.r.Scal <> Comp_MB_Str.Arr[Modul_Adr].ADR.r.Scal THEN BEGIN
                   Comp_Result := Comp_Result + [Scal_Not_Eq];
                   Card_Conf_Stat := [Card_Scal_Not_OK];
                   END
                 ELSE
                   IF VG_ID <> Comp_MB_Str.Arr[Modul_Adr].VG_ID THEN
                     Comp_Result := Comp_Result + [VG_ID_Not_Eq]
                   ELSE
                     IF MOD_ID <> Comp_MB_Str.Arr[Modul_Adr].MOD_ID THEN
                       Comp_Result := Comp_Result + [MOD_ID_Not_Eq];
               IF Card_Conf_Stat * [Card_ID_Not_Match] = [] THEN BEGIN
                 Comp_MB_Card_Spec ( MB_Str.Arr[Modul_Adr], Comp_MB_Str.arr[Modul_Adr], result );
                 IF result <> okay THEN
                   Comp_Result := Comp_Result + [Modul_Spec_Diff];
                 END;
               END; // WITH MB_Str.Arr[Modul_Adr]
             END; // IF (MB_Str.arr[Modul_ADR].card_conf_stat <> [empty]) AND (.....
             Modul_Adr := Modul_Adr + 1;
           END; // WHILE (Modul_Adr <= c_mb_last_adr)
         END
       ELSE BEGIN
         Comp_Result := Comp_Result + [Card_cnt_Not_Eq];
         END;
       END
     ELSE BEGIN
       Comp_Result := Comp_Result + [Search_Not_OK];
       END;
     END
  ELSE BEGIN
    Comp_Result := Comp_Result + [Types_Not_Match];
    END;
  END;

PROCEDURE t_mil_Unit.disp_comp_result ( Comp_result: t_Comp_result; color: BYTE );
  BEGIN
  TextAttr := color;
  Write('Die Modulbus-Konfiguration entspricht ');
  IF Comp_result <> [] THEN BEGIN
    TextColor(red); Write('nicht'); TextAttr := color;
    Writeln(' den Vorgaben!');
    Write('  Unterschiede:');
    TextColor(red);
    IF Comp_result * [Card_cnt_Not_Eq] <> []  THEN Write(' Card_Cnt_NEQ');
    IF Comp_result * [Types_Not_Match] <> []  THEN Write(' Types_NEQ');
    IF Comp_result * [Search_Not_OK] <> []    THEN Write(' Search_N_OK');
    IF Comp_result * [ADR_Not_Eq] <> []       THEN Write(' Adr_NEQ');
    IF Comp_result * [VG_ID_Not_Eq] <> []     THEN Write(' VG_ID_NEQ');
    IF Comp_result * [MOD_ID_Not_Eq] <> []    THEN Write(' Mod_ID_NEQ');
    IF Comp_result * [Scal_Not_Eq] <> []      THEN Write(' Scal_NEQ');
    IF Comp_result * [Epld_Vers_Not_Eq] <> [] THEN Write(' Epld_Vers_NEQ');
    IF Comp_result * [Modul_Spec_Diff] <> []  THEN Write(' Modul_Spec_Diff');
    TextAttr := color;
    Writeln;
    END
  ELSE BEGIN
    Writeln(' den Vorgaben!');
    END;
{t_Comp_Result = SET OF ( , , , , ,
                       , , ,  );
}  END;

PROCEDURE t_Mil_Unit.clear_mb_str ( VAR mb_str: t_mb_str );
  VAR
    i: BYTE;
  BEGIN
  MB_Str.MBC_Adr         := 0;
  MB_Str.MB_Search_Stat  := Search_OK;
  MB_Str.Glob_Conf_Stat  := [Card_Conf_OK];
  MB_Str.MB_Search_Type  := C_All_MB_Cards;
  MB_Str.Card_Cnt        := 0;
  FOR i := c_mb_first_adr TO c_mb_last_adr DO BEGIN
    WITH MB_Str.Arr[i] DO BEGIN
      Card_Conf_Stat := [Empty];
      Adr.w          := 0;
      Mod_ID         := C_Empty_ID;
      VG_ID          := C_Empty_ID;
      Epld.w         := 0;
      END;
    END;
  END;

PROCEDURE t_mil_unit.err_io(mil_err: t_mil_err);
  BEGIN
  IF mil_err = [] THEN           disp.W(0,0,black,'no_err')
  ELSE BEGIN
    IF      fc_to            IN mil_err THEN  disp.W(0,0,red,'fc_to ')
    ELSE IF rd_to            IN mil_err THEN  disp.W(0,0,red,'rd_to ')
    ELSE IF wr_to            IN mil_err THEN  disp.W(0,0,red,'wr_to ')
    ELSE IF fifo_not_empty   IN mil_err THEN  disp.W(0,0,red,'ff_ne ')
    ELSE IF fifo_not_cleared IN mil_err THEN  disp.W(0,0,red,'ff_nc ')
    ELSE IF no_pc_mil        IN mil_err THEN  disp.W(0,0,red,'no_pcm')
    END;
  END;

FUNCTION t_mil_unit.err_string(mil_err: t_mil_err) : STRING;
  BEGIN
  IF mil_err = [] THEN   err_string := 'no_err'
  ELSE BEGIN
    IF      fc_to            IN mil_err THEN  err_string :='fc_to '
    ELSE IF rd_to            IN mil_err THEN  err_string :='rd_to '
    ELSE IF wr_to            IN mil_err THEN  err_string :='wr_to '
    ELSE IF fifo_not_empty   IN mil_err THEN  err_string :='ff_ne '
    ELSE IF fifo_not_cleared IN mil_err THEN  err_string :='ff_nc '
    ELSE IF no_pc_mil        IN mil_err THEN  err_string :='no_pcm'
    END;  
  END;

END. { MIL_Unit }

