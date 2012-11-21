
{**************************************************************************}
{*                    Set Skalierung K0_K1, Mod-ADR[4..0]                 *}
{**************************************************************************}

  {Kanal 0 'mb32_skal_adr_k0_k1': D[7..0] = Skal. von 'mb32_data_K0_K1'}

  mb32_ctrl_data.b.hb := 0; {frei}
  mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);


  {Kanal 1 'mb32_skal_adr_k0_k1': D[12..8]= Mod-ADR. von 'mb32_64_test'}
  {                             : D[7..0] = Skal. (K0+K1) von 'mb32_64_test'}

  mb32_ctrl_data.b.hb   := mb32_64_test;
  mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

  mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                       mb32_skal_adr_k0_k1, mbc_param, mil_error);

{--------------------------------------------------------------------------}


  {Kanal 0 'mb32_skal_adr_k2_k3': D[7..0] = Skal. von 'mb32_data_k2_k3'}

    mb32_ctrl_data.b.hb := 0; {frei}
    mb32_ctrl_data.b.lb := C_io32_K0_In_K1_In
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

    mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_0,
                       mb32_skal_adr_k2_k3, mbc_param, mil_error);


  {Kanal 1 'mb32_skal_adr_k2_k3': D[15..14]= Mod-ID       +   }
  {                             : D[13..8] = Logik        +=> 'mb32_64_test'}
  {                             : D[7..0]  = Skal. (K2+K3)+   }

    mb32_ctrl_data.b.hb := mb32_64_id_logik; {VG-Mod-ID und Logik-ID}
    mb32_ctrl_data.b.lb   := C_io32_K0_Out_K1_Out
                       { + C_io32_APK_0stecker }
                         + C_io32_16bit_032bit
                         + C_io32_K1_M_Standard
                         + C_io32_K0_M_Standard;

    mil.modulbus_adr_wr(NOT mb32_ctrl_data.w, c_io32_kanal_1,
                         mb32_skal_adr_k2_k3, mbc_param, mil_error);


{**************************************************************************}
