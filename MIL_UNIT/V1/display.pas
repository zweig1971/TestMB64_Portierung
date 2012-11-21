{+---------------------------------------------------------------------+}
{ énderung 24.09.99:                                                    }
{          1) Die Standardtexte, weiter_txt, auswahl_txt und loop_txt   }
{             aufgenommen.                                              }
{          2) Die Procedure 'single_loop' eingefÅgt.                    }
{+---------------------------------------------------------------------+}
{ énderung 27.09.99:                                                    }
{             Die Procedure 'weiter' aufgenommen.                       }
{ énderung 03.10.99:                                                    }
{             Die Proceduren 'w'+ 'wl' aufgenommen.                     }
{+---------------------------------------------------------------------+}
{ énderung 08.10.99:                                                    }
{             Die Procedure 'wr_top_w' aufgenommen.                     }
{+---------------------------------------------------------------------+}

UNIT display;

{$S-,G+,B-,N+,E+,I-}

INTERFACE
  USES Crt32;

CONST
  c_tw_sl = 1;  { TopWindowStartLine      }
  c_tw_sc = 1;  { TopWindowStartColumn    }
  c_tw_el = 2;  { TopWindowEndLine        }
  c_tw_ec = 80; { TopWindowEndColumn      }
  c_mw_sl = 3;  { MiddleWindowStartLine   }
  c_mw_sc = 1;  { MiddleWindowStartColumn }
  c_mw_el = 24; { MiddleWindowEndLine     }
  c_mw_ec = 80; { MiddleWindowEndColumn   }
  c_bw_sl = 25; { BottomWindowStartLine   }
  c_bw_sc = 1;  { BottomWindowStartColumn }
  c_bw_el = 25; { BottomWindowEndLine     }
  c_bw_ec = 80; { BottomWindowEndColumn   }
  c_rw_sl = 3;  { RightWindowStartLine    }
  c_rw_sc = 70; { RightWindowStartColumn  }
  c_rw_el = 20; { RightWindowEndLine      }
  c_rw_ec = 80; { RightWindowEndColumn    }

  weiter_txt =
    'weiter, beliebige Taste drÅcken (ausser X-Taste).';
  auswahl_txt =
    'Bitte Auswahl eingeben (Exit = X) : ';
  loop_txt =
    'Single Step mit <SPACE>, Loop mit <CR> , Ende mit [X]   ';


TYPE
  t_disp =
    OBJECT
      PROCEDURE ini_top_w;
      PROCEDURE set_top_w;
      PROCEDURE ini_mid_w;
      PROCEDURE set_mid_w;
      PROCEDURE ini_bot_w;
      PROCEDURE set_bot_w;
      PROCEDURE ini_right_w;
      PROCEDURE set_right_w;
      PROCEDURE single_loop ( VAR answere: CHAR; str: STRING );
      PROCEDURE weiter ( VAR key: CHAR; VAR x,y: byte; str: string );
      PROCEDURE w ( x, y, color : byte; s : STRING );
      PROCEDURE wl ( x, y, color : byte; s : STRING );
      PROCEDURE wr_top_w ( x,y: BYTE; str: string );
    END; { t_disp }

               {*********** Routinen fÅr div. Fenster ***************}
{
   Farbnamen fÅr Fenster und Schrift:
   Black, Blue, Green, Cyan, Red, Magenta, Brown, Yellow, White, Blink
}


IMPLEMENTATION

procedure t_disp.ini_top_w;
  begin                            {Definitionen gelten bis neu definiert}
  Window(c_tw_sc, c_tw_sl, c_tw_ec, c_tw_el); {Definiert ein Textfenster: Spalte/Zeile}
  TextBackground(Magenta);         {Setze Hintergrund fÅr Textfenster}
  TextColor(Yellow);               {Setze Schriftfarbe}

  ClrScr;                          {Clear Window}
  GotoXY(c_tw_sc, c_tw_sl);        {Cursor auf Anfang Fenster}
  end;

procedure t_disp.set_top_w;
  begin
  Window(c_tw_sc, c_tw_sl, c_tw_ec, c_tw_el);
  TextBackground(Magenta);
  TextColor(Yellow);               {Setze Schriftfarbe}
  end;

procedure t_disp.ini_mid_w;
  begin
  Window(c_mw_sc, c_mw_sl, c_mw_ec, c_mw_el);
  TextBackground(LightGray);
  TextColor(Black);               {Setze Schriftfarbe}
  ClrScr;
  end;

procedure t_disp.set_mid_w;
  begin
  Window(c_mw_sc, c_mw_sl, c_mw_ec, c_mw_el);
  TextBackground(White);
  TextColor(Black);               {Setze Schriftfarbe}
  end;

procedure t_disp.ini_bot_w;
  begin
  Window(c_bw_sc, c_bw_sl, c_bw_ec, c_bw_el);
  TextBackground(Green);
  TextColor(Yellow);               {Setze Schriftfarbe}
  ClrScr;
  end;

procedure t_disp.set_bot_w;
  begin
  Window(c_bw_sc, c_bw_sl, c_bw_ec, c_bw_el);
  TextBackground(Green);
  TextColor(Yellow);               {Setze Schriftfarbe}
  end;

procedure t_disp.ini_right_w;
  begin
  Window(c_rw_sc, c_rw_sl, c_rw_ec, c_rw_el);
  TextBackground(cyan);
  TextColor(Black);               {Setze Schriftfarbe}
  ClrScr;
  end;

procedure t_disp.set_right_w;
  begin
  Window(c_rw_sc, c_rw_sl, c_rw_ec, c_rw_el);
  TextBackground(cyan);
  TextColor(Black);               {Setze Schriftfarbe}
  end;

PROCEDURE t_disp.single_loop ( VAR answere: CHAR; str: STRING);

  VAR
    x, y: BYTE;

  BEGIN
  x := whereX;          { Die aktuelle X-Position merken }
  y := whereY;          { Die aktuelle Y-Position merken }
  IF answere = ' ' THEN
    BEGIN
      ini_bot_w;
      write(str);
    REPEAT UNTIL KEYPRESSED;
    set_mid_w;
    answere := readkey;
    END
  ELSE
    answere := readkey;
  set_mid_w;
  GOTOXY(x,y);
  END;

PROCEDURE t_disp.weiter(VAR key: CHAR; VAR x,y: byte; str: string);
  VAR
    dummy: CHAR;
  BEGIN
  x := whereX;
  y := whereY;
  ini_bot_w;
  Gotoxy(1,1); 
  Write(str);
  REPEAT UNTIL Keypressed;
     key := readkey;
  WHILE keypressed DO
    dummy := readkey; { Vorauseilende Eingaben wegschmei·en. }
  GOTOXY(1,1); CLREOL;
  set_mid_w;
  Gotoxy(x,y);
  END;

PROCEDURE t_disp.w (x, y, color : byte; s : STRING);
  VAR
    save_text_attr: BYTE;
  BEGIN
  save_text_attr := TextAttr;
  if (x <> 0) and (y <> 0) then begin
   gotoXY(x,y);
  end;
  Textcolor(color);
  write(s);
  TextAttr := save_text_attr;
  END;

PROCEDURE t_disp.wl ( x, y, color : byte; s : STRING );
  VAR
    save_text_attr: BYTE;
  BEGIN
  save_text_attr := TextAttr;
  gotoXY(x,y);
  Textcolor(color);
  writeln(s);
  TextAttr := save_text_attr;
  END;

PROCEDURE t_disp.wr_top_w ( x, y: BYTE; str: string );
  VAR
    x_save, y_save: BYTE;
  BEGIN
  x_save := wherex;
  y_save := wherey;
  set_top_w;
  gotoxy(x,y); clreol;
  w(x, y, yellow, str);
  set_mid_w;
  gotoxy(x_save, y_save);
  END;

END. { UNIT }