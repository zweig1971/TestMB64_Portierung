UNIT convert;

{$S-,G+,B-,N+,E+,I-}

{ W.Panschow Stand 24.11.99 }

INTERFACE

uses UnitMil;

  TYPE
    t_convert =
      OBJECT
        FUNCTION hex_byte(b: BYTE): STRING;
        FUNCTION hex_word(w: WORD): STRING;
        FUNCTION word_bin(data:WORD) : STRING;
        PROCEDURE conv_to_hex (in_string:       string;
                               VAR hw:          WORD;
                               VAR input_ok:    BOOLEAN);
        PROCEDURE conv_to_hex_n ( in_string:       string;
                                  VAR hl:          LONGINT;
                                  VAR input_ok:    BOOLEAN);
        PROCEDURE read_hexword (VAR hw : word; VAR input_ok : BOOLEAN);
        PROCEDURE read_hex_n(VAR hl : LONGINT;
                                  n: BYTE;
                                  VAR input_ok : BOOLEAN);
        PROCEDURE read_hexbyte(VAR hb : byte; VAR input_ok : BOOLEAN);
    END; { t_convert }

IMPLEMENTATION

FUNCTION t_convert.hex_byte ( b: BYTE ): STRING;
  CONST hex_num : STRING[16] = '0123456789ABCDEF';
  Begin
  hex_byte := hex_num[(b shr 4) + 1] + hex_num[(b AND $0F) + 1];
//  SetLength(hex_byte[0], 2);
//  hex_byte[0] := CHR(2);     { Stringl„nge = 2 }
  End;

FUNCTION t_convert.hex_word ( w: WORD ): STRING;
  Begin
  hex_word := hex_byte(HI(w)) + hex_byte(LO(w));
  //hex_word[0] := CHR(4);      { Stringl„nge = 4 }
  End; { hex_word }

FUNCTION t_convert.word_bin(data:WORD) : STRING;
  VAR i : INTEGER;
  Begin

  result:= intToBinary(data,16);

{  FOR i := 1 TO 16 DO
    Begin
    IF (data AND $8000) <> 0 THEN
      word_bin[i] := '1'
    ELSE
      word_bin[i] := '0';
    data := data shl 1;
    End;
  //word_bin[0] := CHR(16);    }
  End; { hex_bin }

PROCEDURE t_convert.conv_to_hex (in_string:       string;
                                 VAR hw:          WORD;
                                 VAR input_ok:    BOOLEAN);
  VAR
   i : INTEGER;
  Begin
  hw := 0;
  input_ok := TRUE;
  FOR i :=  1 TO length(in_string) DO
    Begin
    hw := hw * 16;
    CASE in_string[i] OF
      'a'..'f' : hw := hw + ORD(in_string[i])-ORD('a')+10;
      'A'..'F' : hw := hw + ORD(in_string[i])-ORD('A')+10;
      '0'..'9' : hw := hw + ORD(in_string[i])-ORD('0');
      ELSE
        input_ok := FALSE;
      END;
    End; {FOR i}
   IF input_ok = FALSE THEN hw := 0;
  End;

PROCEDURE t_convert.conv_to_hex_n ( in_string:       string;
                                    VAR hl:          LONGINT;
                                    VAR input_ok:    BOOLEAN);
  VAR
   i : INTEGER;
  Begin
  hl := 0;
  input_ok := TRUE;
  IF length(in_string) <= 8 THEN BEGIN
    FOR i :=  1 TO length(in_string) DO
      Begin
      hl := hl * 16;
      CASE in_string[i] OF
        'a'..'f' : hl := hl + ORD(in_string[i])-ORD('a')+10;
        'A'..'F' : hl := hl + ORD(in_string[i])-ORD('A')+10;
        '0'..'9' : hl := hl + ORD(in_string[i])-ORD('0');
        ELSE
          input_ok := FALSE;
        END;
      End; {FOR i}
    End;
   IF input_ok = FALSE THEN hl := 0;
  End;

PROCEDURE t_convert.read_hexword(VAR hw : word; VAR input_ok : BOOLEAN);
  VAR
    read_str: string[20];
  BEGIN
  readln(read_str);
  IF length(read_str) IN [1..4] THEN
    conv_to_hex(read_str, hw, input_ok)
  ELSE
    BEGIN
    hw := 0;
    input_ok := FALSE;
    END;
  END;

PROCEDURE t_convert.read_hexbyte(VAR hb : byte; VAR input_ok : BOOLEAN);
  VAR
    read_str: string[20];
    hw : word;
  BEGIN
  readln(read_str);
  IF length(read_str) IN [1..2] THEN
    BEGIN
    conv_to_hex(read_str, hw, input_ok);
    hb := hw;
    END
  ELSE
    BEGIN
    hb := 0;
    input_ok := FALSE;
    END;
  END;

PROCEDURE t_convert.read_hex_n(VAR hl : LONGINT; n: BYTE; VAR input_ok : BOOLEAN);
  VAR
    read_str: string[20];
    hw : word;
  BEGIN
  readln(read_str);
  IF length(read_str) IN [1..n] THEN
    BEGIN
    conv_to_hex_n(read_str, hl, input_ok);
    END
  ELSE
    BEGIN
    hl := 0;
    input_ok := FALSE;
    END;
  END;

END. { UNIT }
