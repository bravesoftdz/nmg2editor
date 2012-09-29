unit g2_types;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

//  ////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2011 Bruno Verhue
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  ////////////////////////////////////////////////////////////////////////////

//  ////////////////////////////////////////////////////////////////////////////
//
//  This unit contains the G2 constants and types and some utility functions.
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses
  SysUtils,
  Classes,
  //StdCtrls,
  math;

const
  NMG2_VERSION = '0.24';
  PATCH_VERSION = 23;

  NMORPHS = 8;
  NVARIATIONS = 9;
  NSLOTS = 4;

  //LOG_PATH = 'C:\Users\Bruno\Documents\Ableton\VST\';

  VENDOR_ID  = $ffc; // clavia, g2
  PRODUCT_ID = 2;

  PATCH_DATA = $00;
  PERF_DATA  = $01;

  LOCATION_FX = 0;
  LOCATION_VA = 1;
  LOCATION_PATCH_SETTINGS = 2;

  XCL_CONTROL_HIGHLIGHT  = $00FFFF00;
  XCL_LED                = $0000FF00;
  CL_SELECTED_PATCH      = $00B6B6B6;
  CL_SELECTED_PARAM      = $00FFFFFF;
  CL_KNOB_MORPH          = $00F4948C;
  CL_KNOB_MORPH_SELECTED = $008582F4;
  CL_DISPLAY_BACKGRND    = $00555555;
  CL_BTN_FACE            = $00D0D0D0;
  CL_BTN_BORDER          = $00404040;
  XCL_CLAVIA_RED         = $005A5692;//$006550DC;//$001620D1;
  XCL_CLAVIA_BLUE        = $00531F00;

  // Module dimensions
  UNITS_COL = 255;
  UNITS_ROW = 15;

  COLOR_RED    = 0;
  COLOR_BLUE   = 1;
  COLOR_YELLOW = 2;
  COLOR_ORANGE = 3;
  COLOR_GREEN  = 4;
  COLOR_PURPLE = 5;
  COLOR_WHITE  = 6;

  // Cable simulation parameters
  CABLE_CONTROL_MARGIN = 6;
  NCABLE_ELEMENTS      = 16;
  CABLE_THICKNESS      = 2;
  TENSION              = 10;
	DAMP                 = 0.8;
	GRAVITY              = 0.04;

  MAX_BULK_DATA_SIZE      = 4096;
  MAX_INTERRUPT_DATA_SIZE = 4096;

  LOGCMD_NUL = $00;
  LOGCMD_HDR = $01;
  LOGCMD_ERR = $02;
  LOGCMD_TAB = $03;

  CMD_SLOT = $08;
  CMD_SYS  = $0c;
  CMD_INIT = $80;
  CMD_CUST = $0B; // Some custom message i use between server and client

  CMD_REQ  = $20;
  CMD_RESP = $00;
  CMD_NO_RESP = $30;

const
  Q_SYNTH_SETTINGS        = $02;
  S_SYNTH_SETTINGS        = $03;
  Q_ASSIGNED_VOICES       = $04;
  R_ASSIGNED_VOICES       = $05;
  // $06
  // $07
  // $08
  S_SEL_SLOT              = $09;
  S_RETREIVE              = $0a;
  S_STORE                 = $0b;
  S_CLEAR                 = $0c;
  R_STORE                 = $0d; // ?
  S_CLEAR_BANK            = $0e;
  // $0f
  Q_PERF_SETTINGS         = $10;
  C_PERF_SETTINGS         = $11;
  R_CLEAR_BANK            = $12;
  R_LIST_NAMES            = $13;
  Q_LIST_NAMES            = $14;
  R_CLEAR                 = $15;
  R_ADD_NAMES             = $16;
  S_PATCH_BANK_UPLOAD     = $17;
  R_PATCH_BANK_UPDLOAD    = $18;
  S_PATCH_BANK_DATA       = $19;
  // $1a
  // $1b
  S_ASS_GLOBAL_KNOB       = $1c;
  S_DEASS_GLOB_KNOB       = $1d;
  S_SEL_GLOBAL_PAGE       = $1e;
  // $1f
  // $20
  C_PATCH_DESCR           = $21;
  S_ASSIGN_MIDICC         = $22;
  S_DEASSIGN_MIDICC       = $23;
  // $24
  S_ASSIGN_KNOB           = $25;
  S_DEASSIGN_KNOB         = $26;
  S_PATCH_NAME            = $27;
  Q_PATCH_NAME            = $28;
  C_PERF_NAME             = $29;
  S_SET_UPRATE            = $2a;
  S_SET_MODE              = $2b;
  // $2c
  S_SEL_PARAM_PAGE        = $2d;
  Q_SELECTED_PARAM        = $2e;
  S_SEL_PARAM             = $2f;
  S_ADD_MODULE            = $30;
  S_SET_MODULE_COLOR      = $31;
  S_DEL_MODULE            = $32;
  S_SET_MODULE_LABEL      = $33;
  S_MOV_MODULE            = $34;
  Q_VERSION_CNT           = $35;
  // $36
  S_SET_PATCH             = $37;
  R_PATCH_VERSION_CHANGE  = $38; // After upload patch through midi
  R_LED_DATA              = $39;
  R_VOLUME_DATA           = $3a;
  Q_MASTER_CLOCK          = $3b;
  Q_PATCH                 = $3c;
  S_MIDI_DUMP             = $3d;
  S_SET_PARAM_MODE        = $3e;
  // $3f
  S_SET_PARAM             = $40;
  // $41
  S_SET_PARAM_LABEL       = $42;
  S_SET_MORPH_RANGE       = $43;
  S_COPY_VARIATION        = $44;
  // $45
  // $46
  // $47
  // $48
  // $49
  C_MODULE_LIST           = $4a;
  // $4b
  Q_PARAMS                = $4c;
  C_PARAM_LIST            = $4d;
  // $4e
  Q_PARAM_NAMES           = $4f;
  S_ADD_CABLE             = $50;
  S_DEL_CABLE             = $51;
  C_CABLE_LIST            = $52;
  // $53
  S_CABLE_COLOR           = $54;
  S_CTRL_SNAPSHOT         = $55;
  S_PLAY_NOTE             = $56;
  // $57
  // $58
  M_UNKNOWN_2             = $59;
  C_MODULE_NAMES          = $5a;
  C_PARAM_NAMES           = $5b;
  // $5c
  R_MASTER_CLOCK          = $5d;
  Q_GLOBAL_KNOBS          = $5e;
  C_KNOBS_GLOBAL          = $5f;
  C_CONTROLLERS           = $60;
  // $61
  C_KNOBS                 = $62;
  // $63
  // $64
  C_MORPH_PARAM           = $65;
  // $66
  // $67
  Q_CURRENT_NOTE          = $68;
  C_CURRENT_NOTE_2        = $69;
  S_SEL_VARIATION         = $6a;
  // $6b
  // $6c
  // $6d
  Q_PATCH_TEXT            = $6e;
  C_PATCH_NOTES           = $6f;
  M_UNKNOWN_6             = $70;
  Q_RESOURCES_USED        = $71;
  R_RESOURCES_USED        = $72;
  // 73
  // 74
  // 75
  // 76
  // 77
  // 78
  // 79
  // 7a
  // 7b
  // 7c
  S_START_STOP_COM        = $7d;
  R_ERROR                 = $7e;
  R_OK                    = $7f;
  R_MIDI_CC               = $80;
  M_UNKNOWN_1             = $81;


  START_COMM              = $00;
  STOP_COMM               = $01;

  PATCH_ENV               = $04;
    ENV_ATTACK            = $01;
    ENV_DECAY             = $02;
    ENV_SUSTAIN           = $03;
    ENV_RELEASE           = $04;

  PATCH_MORPH             = $01;
  PATCH_VOLUME            = $02;
    VOLUME_LEVEL          = $00;
    VOLUME_MUTE           = $01;
  PATCH_GLIDE             = $03;
    GLIDE_TYPE            = $00;
    GLIDE_SPEED           = $01;
  PATCH_BEND              = $04;
    BEND_ON_OFF           = $00;
    BEND_RANGE            = $01;
  PATCH_VIBRATO           = $05;
    VIBRATO_MOD           = $00;
    VIBRATO_DEPTH         = $01;
    VIBRATO_RATE          = $02;
  PATCH_ARPEGGIATOR       = $06;
    ARP_ON_OFF            = $00;
    ARP_SPEED             = $01;
    ARP_DIRECTION         = $02;
    ARP_OCTAVES           = $03;
  PATCH_SUSTAIN           = $07;
    SUSTAIN_PEDAL         = $01;
    OCTAVE_SHIFT          = $00;

  PATCH_MASTERCLOCK       = $08;
  PATCH_VOICES            = $09;

  STD_MORPH_NAMES : array[0..7] of AnsiString = ('Wheel',
                                                 'Vel',
                                                 'Keyb',
                                                 'Aft.Tch',
                                                 'Sust.Pd',
                                                 'Ctrl.Pd',
                                                 'P.Stick',
                                                 'G.Wh 2');

  PARAM_PAGE_NAMES : array[0..4] of string = ('Osc',
                                              'LFO',
                                              'Env',
                                              'Filter',
                                              'Effect');

  CATEGORIES : array[0..15] of string = ('None',
                                         'Acoustic',
                                         'Sequencer',
                                         'Bass',
                                         'Classic',
                                         'Drum',
                                         'Fantasy',
                                         'FX',
                                         'Lead',
                                         'Organ',
                                         'Pad',
                                         'Piano',
                                         'Synth',
                                         'Audio in',
                                         'User1',
                                         'User2');

  MODULECATEGORIES : array[0..16] of string =('In/Out',
                                              'Note',
                                              'Osc',
                                              'LFO',
                                              'Rnd',
                                              'Env',
                                              'Filter',
                                              'FX',
                                              'Delay',
                                              'Shaper',
                                              'Level',
                                              'Mixer',
                                              'Switch',
                                              'Logic',
                                              'Seq',
                                              'MIDI',
                                              'Test');

  ENV_TIMES : array[0..127] of single =
                (0.0005, 0.0006, 0.0007, 0.0009, 0.0011, 0.0013, 0.0015, 0.0018,
                 0.0021, 0.0025, 0.0030, 0.0035, 0.0040, 0.0047, 0.0055, 0.0063,
                 0.0073, 0.0084, 0.0097, 0.0111, 0.0127, 0.0145, 0.0165, 0.0187,
                 0.0212, 0.0240, 0.0271, 0.0306, 0.0344, 0.0387, 0.0434, 0.0486,
                 0.0543, 0.0606, 0.0676, 0.0752, 0.0836, 0.0928, 0.1030, 0.1140,
                 0.1260, 0.1390, 0.1530, 0.1690, 0.1860, 0.2040, 0.2240, 0.2460,
                 0.2690, 0.2950, 0.3220, 0.3520, 0.3840, 0.4190, 0.4560, 0.4960,
                 0.5400, 0.5860, 0.6360, 0.6900, 0.7480, 0.8100, 0.8760, 0.9470,
                 1.0200, 1.1000, 1.1900, 1.2800, 1.3800, 1.4900, 1.6000, 1.7200,
                 1.8500, 1.9900, 2.1300, 2.2800, 2.4600, 2.6200, 2.8100, 3.0000,
                 3.2100, 3.4300, 3.6600, 3.9100, 4.1700, 4.4500, 4.7400, 5.0500,
                 5.3700, 5.7200, 6.0800, 6.4700, 6.8700, 7.3000, 7.7500, 8.2200,
                 8.7200, 9.2500, 9.8000, 10.400, 11.000, 11.600, 12.300, 13.000,
                 13.800, 14.600, 15.400, 16.200, 17.100, 18.100, 19.100, 20.100,
                 21.200, 22.400, 23.500, 24.800, 26.100, 27.500, 28.900, 30.400,
                 32.000, 33.600, 35.300, 37.100, 38.900, 40.900, 42.900, 45.000);

  UNKNOWN_TIMES : array[0..127] of string =
               ( '0.2m', '0.3m', '0.4m', '0.5m', '0.6m', '0.8m', '0.9m', '1.0m',
                 '1.2m', '1.4m', '1.6m', '1.7m', '2.0m', '2.2m', '2.4m', '2.6m',
                 '2.9m', '3.1m', '3.4m', '3.7m', '4.0m', '4.3m', '4.6m', '4.9m',
                 '5.3m', '5.6m', '6.0m', '6.3m', '6.7m', '7.1m', '7.5m', '7.9m',
                 '8.4m', '8.8m', '9.3m', '9.7m','10.2m','10.7m','11.2m','11.7m',
                '12.2m','12.7m','13.3m','13.8m','14.4m','14.9m','15.5m','16.1m',
                '16.7m','17.4m','18.0m','18.6m','19.3m','19.9m','20.6m','21.3m',
                '22.0m','22.7m','23.4m','24.1m','24.9m','25.6m','26.4m','27.2m',
                '28.0m','28.8m','29.6m','30.4m','31.2m','32.1m','32.9m','33.8m',
                '34.6m','35.5m','36.4m','37.3m','38.3m','39.2m','40.1m','41.1m',
                '42.0m','43.0m','44.0m','45.0m','46.0m','47.0m','48.1m','49.1m',
                '50.2m','51.2m','52.3m','53.4m','54.5m','55.6m','56.7m','57.9m',
                '59.0m','60.2m','61.3m','62.5m','63.7m','64.9m','66.1m','67.3m',
                '68.6m','69.8m','71.1m','72.3m','73.6m','74.9m','76.2m','77.5m',
                '78.8m','80.2m','81.5m','82.9m','84.2m','85.6m','87.0m','88.4m',
                '89.8m','91.2m','92.7m','94.1m','95.6m','97.0m','98.5m','100m');

  COMPR_ATTACK_TIMES : array[0..127] of string =
               ( 'Fast','0.53m','0.56m','0.59m','0.63m','0.67m','0.71m','0.75m',
                '0.79m','0.84m','0.89m','0.94m','1.00m','1.06m','1.12m','1.19m',
                '1.26m','1.33m','1.41m','1.50m','1.59m','1.68m','1.78m','1.89m',
                '2.00m','2.12m','2.24m','2.38m','2.52m','2.67m','2.83m','3.00m',
                '3.17m','3.36m','3.56m','3.78m','4.00m','4.24m','4.49m','4.76m',
                '5.04m','5.34m','5.66m','5.99m','6.35m','6.73m','7.13m','7.55m',
                '8.00m','8.48m','8.98m','9.51m','10.1m','10.7m','11.3m','12.0m',
                '12.7m','13.5m','14.3m','15.1m','16.0m','17.0m','18.0m','19.0m',
                '20.2m','21.4m','22.6m','24.0m','25.4m','26.9m','28.5m','30.2m',
                '32.0m','33.9m','35.9m','38.1m','40.3m','42.7m','45.3m','47.9m',
                '50.8m','53.8m','57.0m','60.4m','64.0m','67.8m','71.8m','76.1m',
                '80.6m','85.4m','90.5m','95.9m',' 102m',' 108m',' 114m',' 121m',
                ' 128m',' 136m',' 144m',' 152m',' 161m',' 171m',' 181m',' 192m',
                ' 203m',' 215m',' 228m',' 242m',' 256m',' 271m',' 287m',' 304m',
                ' 323m',' 342m',' 362m',' 384m',' 406m',' 431m',' 456m',' 483m',
                ' 512m',' 542m',' 575m',' 609m',' 645m',' 683m',' 724m',' 767m');

  COMPR_RELEASE_TIMES : array[0..127] of string =
               (' 125m',' 129m',' 134m',' 139m',' 144m',' 149m',' 154m',' 159m',
                ' 165m',' 171m',' 177m',' 183m',' 189m',' 196m',' 203m',' 210m',
                ' 218m',' 225m',' 233m',' 241m',' 250m',' 259m',' 268m',' 277m',
                ' 287m',' 297m',' 308m',' 319m',' 330m',' 342m',' 354m',' 366m',
                ' 379m',' 392m',' 406m',' 420m',' 435m',' 451m',' 467m',' 483m',
                ' 500m',' 518m',' 536m',' 555m',' 574m',' 595m',' 616m',' 637m',
                ' 660m',' 683m',' 707m',' 732m',' 758m',' 785m',' 812m',' 841m',
                ' 871m',' 901m',' 933m',' 966m','1.00s','1.04s','1.07s','1.11s',
                '1.15s','1.19s','1.23s','1.27s','1.32s','1.37s','1.41s','1.46s',
                '1.52s','1.57s','1.62s','1.68s','1.74s','1.80s','1.87s','1.93s',
                '2.00s','2.07s','2.14s','2.22s','2.30s','2.38s','2.46s','2.55s',
                '2.64s','2.73s','2.83s','2.93s','3.03s','3.14s','3.25s','3.36s',
                '3.48s','3.61s','3.73s','3.86s','4.00s','4.14s','4.29s','4.44s',
                '4.59s','4.76s','4.92s','5.10s','5.28s','5.46s','5.66s','5.86s',
                '6.06s','6.28s','6.50s','6.73s','6.96s','7.21s','7.46s','7.73s',
                '8.00s','8.28s','8.57s','8.88s','9.19s','9.51s','9.85s','10.2s');

  // Unknown: '12.0s','12.3s','12.7s','13.1s','13.7s','14.9s','16.4s','18.3s',
  // '19.3s','20.5s','21.5s','22.5s','23.5s'

  // Made lookup tables for the following parameters, because I couldn't figure out
  // the formula's

  PHASER_FREQ : array[0..127] of single = (0.05, 0.05, 0.05, 0.05, 0.06, 0.06, 0.07, 0.08,
                                           0.09, 0.10, 0.12, 0.13, 0.15, 0.17, 0.19, 0.21,
                                           0.23, 0.25, 0.28, 0.30, 0.33, 0.36, 0.39, 0.42,
                                           0.46, 0.49, 0.53, 0.57, 0.61, 0.65, 0.69, 0.73,
                                           0.78, 0.82, 0.87, 0.92, 0.97, 1.02, 1.08, 1.13,
                                           1.19, 1.25, 1.31, 1.37, 1.43, 1.49, 1.56, 1.63,
                                           1.69, 1.76, 1.83, 1.91, 1.98, 2.05, 2.13, 2.21,
                                           2.29, 2.37, 2.45, 2.53, 2.62, 2.71, 2.80, 2.88,
                                           2.98, 3.07, 3.16, 3.26, 3.35, 3.45, 3.55, 3.65,
                                           3.75, 3.86, 3.96, 4.07, 4.18, 4.29, 4.40, 4.51,
                                           4.62, 4.74, 4.86, 4.97, 5.09, 5.21, 5.34, 5.46,
                                           5.58, 5.71, 5.84, 5.97, 6.10, 6.23, 6.37, 6.50,
                                           6.64, 6.77, 6.92, 7.06, 7.20, 7.34, 7.49, 7.63,
                                           7.78, 7.93, 8.08, 8.23, 8.39, 8.54, 8.70, 8.86,
                                           9.02, 9.18, 9.34, 9.50, 9.67, 9.84, 10.0, 10.2,
                                           10.3, 10.5, 10.7, 10.9, 11.0, 11.2, 11.4, 11.6);

  FLANGER_RATE : array[0..127] of single = (0.01, 0.02, 0.05, 0.07, 0.09, 0.11, 0.14, 0.16,
                                            0.18, 0.21, 0.23, 0.25, 0.27, 0.30, 0.32, 0.34,
                                            0.37, 0.39, 0.41, 0.43, 0.46, 0.48, 0.50, 0.53,
                                            0.55, 0.57, 0.60, 0.62, 0.64, 0.66, 0.69, 0.71,
                                            0.73, 0.76, 0.78, 0.80, 0.82, 0.85, 0.87, 0.89,
                                            0.92, 0.94, 0.96, 0.98, 1.01, 1.03, 1.05, 1.08,
                                            1.10, 1.12, 1.14, 1.17, 1.19, 1.21, 1.24, 1.26,
                                            1.28, 1.30, 1.33, 1.35, 1.37, 1.40, 1.42, 1.44,
                                            1.46, 1.49, 1.51, 1.53, 1.56, 1.58, 1.60, 1.63,
                                            1.65, 1.67, 1.69, 1.72, 1.74, 1.76, 1.79, 1.81,
                                            1.83, 1.85, 1.88, 1.90, 1.92, 1.95, 1.97, 1.99,
                                            2.01, 2.04, 2.06, 2.08, 2.11, 2.13, 2.15, 2.17,
                                            2.20, 2.22, 2.24, 2.27, 2.29, 2.31, 2.33, 2.36,
                                            2.38, 2.40, 2.43, 2.45, 2.47, 2.49, 2.52, 2.54,
                                            2.56, 2.59, 2.61, 2.63, 2.66, 2.68, 2.70, 2.72,
                                            2.75, 2.77, 2.79, 2.82, 2.84, 2.86, 2.88, 2.91);


  PULSE_RANGE : array[0..127] of single = ( 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17,
                                           0.18, 0.19, 0.21, 0.22, 0.24, 0.26, 0.27, 0.29,
                                           0.32, 0.34, 0.36, 0.39, 0.42, 0.45, 0.48, 0.51,
                                           0.55, 0.59, 0.64, 0.68, 0.73, 0.79, 0.84, 0.90,
                                           0.97, 1.04, 1.12, 1.20, 1.29, 1.38, 1.48, 1.59,
                                           1.71, 1.83, 1.97, 2.11, 2.27, 2.44, 2.62, 2.81,
                                           3.02, 3.24, 3.48, 3.74, 4.02, 4.32, 4.64, 4.98,
                                           5.35, 5.75, 6.18, 6.64, 7.13, 7.67, 8.24, 8.86,
                                           9.52, 10.2, 11.0, 11.8, 12.7, 13.7, 14.7, 15.8,
                                           17.0, 18.3, 19.6, 21.1, 22.7, 24.4, 26.3, 28.3,
                                           30.4, 32.7, 35.2, 37.9, 40.8, 43.9, 47.2, 50.8,
                                           54.7, 58.8, 63.3, 68.1, 73.4, 79.0, 85.0, 91.5,
                                           98.5, 106, 114, 123, 133, 143, 154, 166,
                                           178, 192, 207, 223, 240, 259, 279, 300,
                                           324, 349, 376, 405, 437, 471, 508, 547,
                                           590, 636, 686, 739, 797, 860, 927, 1.00);

  ENV_FOLLOW_ATTACK : array[0..127] of single = (0.00, 0.53, 0.56, 0.60, 0.64, 0.67, 0.72, 0.76,
                                                 0.81, 0.86, 0.91, 0.97, 1.03, 1.09, 1.16, 1.23,
                                                 1.30, 1.38, 1.47, 1.56, 1.66, 1.76, 1.87, 1.98,
                                                 2.10, 2.23, 2.37, 2.52, 2.67, 2.84, 3.01, 3.20,
                                                 3.39, 3.60, 3.83, 4.06, 4.31, 4.58, 4.86, 5.16,
                                                 5.48, 5.82, 6.18, 6.56, 6.96, 7.39, 7.85, 8.33,
                                                 8.84, 9.39, 9.97, 10.6, 11.2, 11.9, 12.7, 13.4,
                                                 14.3, 15.2, 16.1, 17.1, 18.1, 19.3, 20.4, 21.7,
                                                 23.0, 24.5, 26.0, 27.6, 29.3, 31.1, 33.0, 35.0,
                                                 37.2, 39.5, 41.9, 44.5, 47.3, 50.2, 53.3, 56.5,
                                                 60.0, 63.7, 67.7, 71.8, 76.3, 81.0, 86.0, 91.3,
                                                 96.9, 103, 109, 116, 123, 131, 139, 147,
                                                 156, 166, 176, 187, 199, 211, 224, 238,
                                                 253, 268, 285, 302, 321, 341, 362, 384,
                                                 408, 433, 459, 488, 518, 550, 584, 620,
                                                 659, 699, 742, 787, 837, 888, 942, 1000);

  ENV_FOLLOW_RELEASE : array[0..127] of single = (10.0, 10.5, 10.9, 11.4, 12.0, 12.5, 13.1, 13.7,
                                                  14.3, 15.0, 15.7, 16.4, 17.1, 17.9, 18.8, 19.6,
                                                  20.5, 21.5, 22.4, 23.5, 24.6, 25.7, 26.9, 28.1,
                                                  29.4, 30.7, 32.1, 33.6, 35.2, 36.8, 38.5, 40.2,
                                                  42.1, 44.0, 46.0, 48.2, 50.4, 52.7, 55.1, 57.6,
                                                  60.3, 63.1, 66.0, 69.0, 72.2, 75.5, 78.9, 82.6,
                                                  86.3, 90.3, 94.5, 98.8, 103, 108, 113, 118,
                                                  124, 129, 135, 142, 148, 155, 162, 169,
                                                  177, 185, 194, 203, 212, 222, 232, 243,
                                                  254, 265, 278, 291, 304, 318, 332, 347,
                                                  363, 380, 398, 416, 435, 455, 476, 498,
                                                  521, 545, 570, 596, 624, 652, 682, 713,
                                                  747, 780, 816, 854, 894, 934, 977, 1020,
                                                  1070, 1120, 1170, 1220, 1280, 1340, 1400, 1460,
                                                  1530, 1600, 1680, 1760, 1840, 1920, 2010, 2100,
                                                  2200, 2300, 2410, 2520, 2630, 2760, 2870, 3000);

  NOISE_GATE_ATTACK : array[0..127] of single =( 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 0.9, 1.0,
                                                 1.2, 1.4, 1.6, 1.7, 2.0, 2.2, 2.4, 2.6,
                                                 2.9, 3.1, 3.4, 3.7, 4.0, 4.3, 4.6, 4.9,
                                                 5.3, 5.6, 6.0, 6.3, 6.7, 7.1, 7.5, 7.9,
                                                 8.4, 8.8, 9.3, 9.7, 10.2, 10.7, 11.2, 11.7,
                                                 12.2, 12.7, 13.3, 13.8, 14.4, 14.9, 15.5, 16.1,
                                                 16.7, 17.4, 18.0, 18.6, 19.3, 19.9, 20.6, 21.3,
                                                 22.0, 22.7, 23.4, 24.1, 24.9, 25.6, 26.4, 27.2,
                                                 28.0, 28.8, 29.6, 30.4, 31.2, 32.1, 32.9, 33.8,
                                                 34.6, 35.5, 36.4, 37.3, 38.3, 39.2, 40.1, 41.1,
                                                 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.1, 49.1,
                                                 50.2, 51.2, 52.3, 53.4, 54.5, 55.6, 56.7, 57.9,
                                                 59.0, 60.2, 61.3, 62.5, 63.7, 64.9, 66.1, 67.3,
                                                 68.6, 69.8, 71.1, 72.3, 73.6, 74.9, 76.2, 77.5,
                                                 78.8, 80.2, 81.5, 82.9, 84.2, 85.6, 87.0, 88.4,
                                                 89.8, 91.2, 92.7, 94.1, 95.6, 97.0, 98.5, 100);

  NOISE_GATE_RELEASE : array[0..127] of single = ( 0.5, 0.59, 0.67, 0.76, 0.87, 0.98, 1.11, 1.25,
                                                  1.40, 1.57, 1.75, 1.95, 2.17, 2.41, 2.66, 2.94,
                                                  3.25, 3.57, 3.92, 4.30, 4.71, 5.15, 5.62, 6.12,
                                                  6.66, 7.24, 7.85, 8.51, 9.21, 9.96, 10.7, 11.6,
                                                  12.5, 13.4, 14.4, 15.5, 16.6, 17.8, 19.1, 20.4,
                                                  21.8, 23.3, 24.9, 26.5, 28.2, 30.0, 32.0, 34.0,
                                                  36.1, 38.3, 40.6, 43.0, 45.5, 48.2, 51.0, 53.9,
                                                  56.9, 60.1, 63.4, 66.8, 70.4, 74.2, 78.1, 82.2,
                                                  86.4, 90.9, 95.5, 100, 105, 110, 116, 121,
                                                  127, 133, 139, 146, 153, 160, 167, 174,
                                                  182, 190, 198, 207, 216, 225, 234, 244,
                                                  254, 265, 275, 286, 298, 310, 322, 335,
                                                  348, 361, 375, 389, 404, 419, 434, 450,
                                                  467, 484, 501, 519, 537, 556, 578, 596,
                                                  616, 638, 659, 682, 705, 728, 752, 777,
                                                  802, 828, 855, 883, 911, 940, 970, 1000);


  EXTENDED_MODULE_IDS = [  2,   6,  10,  11,  14,  35,  39,  56,  65,  70,  77,
                          80,  95,  99, 107, 110, 111, 122, 133, 135, 136, 137,
                         138, 151, 153, 155, 168, 191, 201, 203, 207, 209, 210,
                         211, 212, 213, 214, 215, 216, 217, 218, 219];

  KeyNames : array[0..11] of string = ('C','C#','D','D#','E','F','F#','G','G#','A','A#','B');

  MaxPixelCount = 32768;

type
  TPatchFileType = (pftPatch, pftPerf, pftEnd);
  TMessageDataType = (mdtResponseMessage = 0, mdtSendMessage = 1);
  TParamType = ( ptParam, ptMode, ptMasterClock, ptVoiceMode);
  TKnobType = ( ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium, ktSlider, ktSeqSlider);
  TButtonTextType = (bttNormal, bttPush, bttCheck, bttCheckBox);
  TOrientationType = ( otHorizontal, otVertical);
  TIconType = (itNone, itUp, itDown, itLeft, itRight, itCheck, itSine, itSaw, itPulse, itTri, itPulse25, itPulse10);
  TBandwidthType = (btStatic, btDynamic);
  TConnectorType = (ctAudio, ctLogic, ctControl);
  TConnectorKind = (ckInput, ckOutput);
  TLedType = (ltSequencer, ltGreen, ltMiniVU);
  TLocationType = (ltFX = 0, ltVA = 1, ltPatch = 2);
  TClientType = (ctEditor, ctVST);
  TMidiDeviceAssignmentType = (mdatNone, mdatCtrl, mdatSysEx);

  TByteBuffer = packed array of byte;

  TStaticByteBuffer = packed array[0..MAX_BULK_DATA_SIZE-1] of byte;
  PStaticByteBuffer = ^TStaticByteBuffer;


  TBits16 = word;
  TBits14 = word;
  TBits12 = word;
  TBits10 = word;
  TBits8 = byte;
  TBits7 = byte;
  TBits6 = byte;
  TBits5 = byte;
  TBits4 = byte;
  TBits3 = byte;
  TBits2 = byte;
  TBits1 = byte;

  TEndToken = set of AnsiChar;

  TBitReader = class
    FReadBitPointer : integer;
    FBitBuffer      : byte;
    constructor Create;
    function    ReadBits(aStream : TStream; NoOffBits : byte): Cardinal;
  end;

  TBitWriter = class
    FWriteBitPointer : integer;
    FBitBuffer       : byte;
    constructor Create;
    procedure   WriteBits(aStream : TStream; Value : Cardinal; NoOffBits : byte);
    function    GetWriteBufferBitsLeft : integer;
  end;

  TPatchChunk = class
    FStream      : TStream;
    FId          : byte;
    FSize        : word;
    FLogLines    : TStrings;
    FBitReader   : TBitReader;
    FBitWriter   : TBitWriter;
    FWriteBuffer : TMemoryStream;
    FReadBuffer  : TMemoryStream;
    FWriteCrc    : Word;
    FReadCrc     : Word;
    constructor Create( aStream : TStream);
    destructor  Destroy; override;
    function    PeekID: integer;
    procedure   ReadChunk;
    procedure   ReadBuffer(aSize : integer);
    function    ReadName: AnsiString;
    function    ReadBits(NoOffBits : byte): Cardinal;
    function    GetReadBufferBitsLeft: integer;
    procedure   WriteChunk(aId : byte);
    procedure   WriteBits(Value : Cardinal; NoOffBits : byte);
    procedure   WriteName(Value : AnsiString);
    procedure   WriteCrc(aStream : TStream);
    procedure   Flush;
    procedure   DumpChunkData(aStream : TStream);
  end;

  TModuleDefStream = class( TMemoryStream)
    constructor Create( Filename : string);
    destructor  Destroy; override;
    function    GetNextString( var source : AnsiString; EndTokens : TEndToken): AnsiString;
    function    GetNextByte( var source : AnsiString; EndTokens : TEndToken): Byte;
    function    GetNextInteger( var source : AnsiString; EndTokens : TEndToken): Integer;
    procedure   ReadSpaces;
    function    ReadUntil( EndTokens : TEndToken): AnsiString;
    function    ReadConst( value : AnsiString): boolean;
    function    ReadOptions( sl : TStrings; ListTokens, EndTokens : TEndToken): integer;
    function    PeekValue( Name : AnsiString; ValueEndTokens, EndTokens : TEndToken): AnsiString;
    function    UnQuote( Value : AnsiString): AnsiString;
  end;

function  CrcClavia( Seed: Integer; aVal: Integer): Word;
function  G2BPM( aValue : integer): string;
function  BoolToByte( Value : boolean): byte;
function  BoolToInt( value : boolean): integer;
function  BoolToStr( value : boolean): string;
function  HexToByte( c : char): byte;
function  IntToByte( i : integer): byte;
function  ByteToInt( b : byte): integer;
function  max( v1, v2 : integer): integer;
function  min( v1, v2 : integer): integer;
function  CompletePath( path : string): string;
function  ConvertFwSlahsToBwSlash( filename : string): string;
function  ConvertBwSlahsToFwSlash( filename : string): string;
function  ConvertToObjectName( aValue : string): string;
function  GetKeyName( aKeyNumber : integer): string;
function  G2FloatToStr( aValue : single; aLen : integer): string;
function  G2FloatToStrFixed( aValue : single; aLen : integer): string;
function  PatchNameFromFileName( aFileName : string): string;


{$IFNDEF G2_VST}
function  InRange(value: string; min, max: integer): integer;
{$ENDIF}

var
  ModuleColors : array[0..24] of integer = ($00C0C0C0,
                                            $00BABACC, // 1
                                            $00BACCBA, // 2
                                            $00CCBAB0, // 3
                                            $00AACBD0, // 4
                                            $00D4A074, // 5
                                            $007A77E5, // 6 R
                                            $00BDC17B, // 7
                                            $0080B982, // 8
                                            $0048D1E7, // 9
                                            $0062D193, // 10
                                            $007DC7DE, // 11
                                            $00C29A8F, // 12
                                            $00817DBA, // 13
                                            $008D8DCA, // 14
                                            $00A5D1DE, // 15
                                            $009CCF94, // 16
                                            $00C7D669, // 17
                                            $00C8D2A0, // 18
                                            $00D2D2BE, // 19
                                            $00C08C80, // 20
                                            $00C773D6, // 21
                                            $00BE82BE, // 22
                                            $00D2A0CD, // 23
                                            $00D2BED2);// 24

  CableColors : array[0..6] of integer = ($005A5AFF,
                                          $00FF6464,
                                          $0050E6E6,
                                          $0050C0FF,
                                          $0050D250,
                                          $00E600C8,
                                          $00FFFFFF);

  G_HighlightColor : integer;
  G_LedColor : integer;
  G_SlotStripColor : integer;
  G_SlotStripInverseColor : integer;
  G_SlotStripDisabledColor : integer;
  G_CableThickness : integer;

implementation

function PatchNameFromFileName( aFileName : string): string;
var i : integer;
begin
  Result := '';
  aFileName := ExtractFilename( aFileName);
  i := 1;
  while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
    Result := Result + AnsiChar(aFileName[i]);
    inc(i);
  end;
end;

function G2BPM( aValue : integer): string;
begin
   if aValue <= 32 then
     Result := IntToStr(24 + 2*aValue)
   else
     if aValue <= 96 then
       Result := IntToStr(88 + aValue - 32)
     else
       Result := IntToStr(152 + (aValue - 96)*2);
end;

function CompletePath( path : string): string;
begin
  if length(path) > 0 then begin
    if path[ Length(path)] = '\' then
      Result := path
    else
      Result := path + '\';
  end else
    Result := '';
end;

function G2FloatToStr( aValue : single; aLen : integer): string;
var intpart : single;
    fl, t, p : integer;
begin
  intpart := abs(aValue);
  fl := 1; // calc numbers before decimal point
  while intpart > 10 do begin
    inc(fl);
    intpart := intpart / 10;
  end;
  fl := aLen - (fl+1);
  if fl > 0 then begin
    p := 1; // calc number of visible decimals
    while fl > 0 do begin
      p := p * 10;
      dec(fl);
    end;
    t := round(aValue*p);
    Result := FloatToStr(t/p);
  end else
    Result := IntToStr(round(aValue));
end;

function G2FloatToStrFixed( aValue : single; aLen : integer): string;
var temp : single;
    fl, t, p : integer;
begin
  temp := abs(aValue);
  fl := 1; // calc numbers before decimal point
  while temp >= 10 do begin
    inc(fl);
    temp := temp / 10;
  end;
  fl := aLen - (fl+1);
  if fl > 0 then begin
    p := 1; // calc number of visible decimals
    while fl > 0 do begin
      p := p * 10;
      dec(fl);
    end;
    t := round(aValue*p);
    if p > 1 then begin
      Result := FloatToStr(t/p);
      if frac(t/p)=0 then
        Result := Result + '.';
      if t < 0 then
        inc(aLen); // - sign
      while Length(Result) < aLen do
        Result := Result + '0'
    end else
      Result := FloatToStr(t/p);
  end else
    Result := IntToStr(round(aValue));
end;

function GetKeyName( aKeyNumber : integer): string;
var Key, Octave : integer;
begin
  Key := aKeyNumber mod 12;
  Octave := aKeyNumber div 12 - 1;
  Result := KeyNames[Key] + IntToStr(Octave);
end;

function ConvertToObjectName( aValue : string): string;
var i : integer;
begin
  Result := '';
  for i := 1 to Length(aValue) do begin
    if aValue[i] in [' ','/','#'] then
      Result := Result + '_'
    else
      Result := Result + aValue[i];
  end;
end;

function ConvertFwSlahsToBwSlash( filename : string): string;
var i : integer;
begin
  Result := filename;
  i := pos('/', Result);
  while i > 0 do begin
    Result[i] := '\';
    i := Pos('/', Result);
  end;
end;

function ConvertBwSlahsToFwSlash( filename : string): string;
var i : integer;
begin
  Result := filename;
  i := pos('\', Result);
  while i > 0 do begin
    Result[i] := '/';
    i := Pos('\', Result);
  end;
end;

function CrcClavia( Seed: Integer; aVal: Integer): Word;
var
   i    : Integer;
   aCrc : Integer;
   k    : Integer;
begin
   k    := ((( Seed shr 8) xor aVal) and 255) shl 8;
   aCrc := 0;
   for i := 1 to 8
   do begin
     if ( aCrc xor k) and $8000 <> 0
     then aCrc := ( aCrc shl 1) xor $1021
     else aCrc := aCrc shl 1;
     k := k shl 1;
   end;
   Result := (( Seed shl 8) xor aCrc) and $ffff;
end;

function HexToByte( c : char): byte;
begin
  if c >= 'a' then
    Result := 10 + ord(c) - ord('a')
  else
    Result := ord(c) - ord('0');
end;

function BoolToByte( Value : boolean): byte;
begin
  If Value then
    Result := 1
  else
    Result := 0;
end;

function BoolToInt(value : boolean): integer;
begin
  if value then
    Result := 1
  else
    Result := 0;
end;

function BoolToStr( value : boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function IntToByte( i : integer): byte;
begin
  if i < 0 then
    Result := 256 + i
  else
    Result := i;
end;

function ByteToInt( b : byte): integer;
begin
  if b > 127 then
    Result := b - 256
  else
    Result := b;
end;

function InRange(value: string; min, max: integer): integer;
var Code : integer;
begin
  val(Value, Result, Code);
  if Code <> 0 then
    raise Exception.Create('Number required.');

  if (Result < min) or (Result > max)  then
    raise Exception.Create('Value out of range (' + IntToStr(min) + '..' + IntTostr(max) + ').');
end;

function max( v1, v2 : integer): integer;
begin
  if v1 > v2 then
    Result := v1
  else
    Result := v2;
end;

function min( v1, v2 : integer): integer;
begin
  if v1 < v2 then
    Result := v1
  else
    Result := v2;
end;

// ==== TModuleDefStream =======================================================


constructor TModuleDefStream.Create( FileName : string);
begin
  inherited Create;
  LoadFromFile( FileName);
end;

destructor TModuleDefStream.Destroy;
begin
  inherited;
end;

function TModuleDefStream.GetNextString( var source : AnsiString; EndTokens : TEndToken): AnsiString;
begin
  Result := '';
  while (length(source) > 0) and not(source[1] in EndTokens) do begin
    Result := Result + Source[1];
    source := copy(source, 2, Length(source)-1);
  end;
  if (length(source) > 0) then
    source := copy(source, 2, Length(source)-1);
end;

function TModuleDefStream.GetNextByte( var source : AnsiString; EndTokens : TEndToken): Byte;
var value : AnsiString;
begin
  value := '';
  while (length(source) > 0) and not(source[1] in EndTokens) do begin
    value := value + source[1];
    source := copy(source, 2, Length(source)-1);
  end;
  if (length(source) > 0) then
    source := copy(source, 2, Length(source)-1);
  Result := StrToInt(string(value));
end;

function TModuleDefStream.GetNextInteger( var source : AnsiString; EndTokens : TEndToken): Integer;
var value : AnsiString;
begin
  value := '';
  while (length(source) > 0) and not(source[1] in EndTokens) do begin
    value := value + source[1];
    source := copy(source, 2, Length(source)-1);
  end;
  if (length(source) > 0) then
    source := copy(source, 2, Length(source)-1);
  Result := StrToInt(string(value));
end;

procedure TModuleDefStream.ReadSpaces;
var c : AnsiChar;
begin
  while (Position < Size) and (Read( c, 1) = 1) and (c in [' ', #10, #13]) do begin
  end;
  if not(c in [' ', #10, #13]) then
    Position := Position - 1;
end;

function TModuleDefStream.ReadUntil( EndTokens : TEndToken): AnsiString;
var c : AnsiChar;
begin
  Result := '';
  while ( Position < Size) and ( Read( c, 1) = 1) and not(c in EndTokens) do begin
    Result := Result + c;
  end;
end;

function TModuleDefStream.ReadConst( value : AnsiString): boolean;
var c : AnsiChar;
    i : integer;
begin
  Result := false;
  i := 1;
  while ( Position < Size) and (i<=Length(Value)) and ( Read( c, 1) = 1) and (value[i] = c) do begin
    inc(i);
  end;
  Result := True;
end;

function TModuleDefStream.ReadOptions( sl : TStrings; ListTokens, EndTokens : TEndToken): integer;
var c : AnsiChar;
    s : AnsiString;
begin
  while ( Position < Size) and ( Read( c, 1) = 1) and not(c in EndTokens) do begin
    if (c in ListTokens) then begin
      sl.Add(string(s));
      s := '';
    end else
      s := s + c;
  end;
  if s <> '' then
    sl.Add(string(s));
  Result := sl.Count;
end;

function TModuleDefStream.PeekValue( Name : AnsiString; ValueEndTokens, EndTokens : TEndToken): AnsiString;
var oldpos, start : integer;
    c : AnsiChar;
    found : boolean;
begin
  Result := '';
  oldpos := Position;
  try
    Found := False;
    start := Position;
    Read(c, 1);
    while (Position < Size) and not(found) and not( c in EndTokens) do begin
      if c <> Name[Position - start] then begin
        Start := Position;
      end else
        if Position - start = Length(Name) then
          found := True;

      if not(found) then
        Read(c, 1);
    end;

    if Found then begin
      Read(c, 1);
      while ( Position < Size) and not( c in ValueEndTokens) do begin
        Result := Result + c;
        Read(c, 1);
      end;
    end;

  finally
    Position := oldpos;
  end;
end;

function TModuleDefStream.UnQuote( Value : AnsiString): AnsiString;
var i : integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
    if Value[i] <> '"' then
      Result := Result + Value[i];
end;

// ==== TBitReader =============================================================

constructor TBitReader.create;
begin
  FReadBitPointer := 8; // Forces a new read of a new byte in the BitBuffer
  FBitBuffer := 0;
end;

function TBitReader.ReadBits(aStream: TStream; NoOffBits: byte): Cardinal;
var
  Mask: Byte;
  BitBufferSize, BitsLeft : integer;
begin
  // Clavia uses a kind of compression with the patch data

  BitBufferSize := SizeOf(FBitBuffer) * 8;
  BitsLeft := BitBufferSize - FReadBitPointer; // Bits not read in BitBuffer;

  Mask := $FF shr FReadBitPointer;             // Readpointer : msb = 0, lsb = 7 !

  if (NoOffBits - BitsLeft) <= 0 then begin    // The bits left in the buffer can be used

    Result := (FBitBuffer and Mask)            // Clear the bits that are not needed
                  shr (BitsLeft - NoOffBits);  // Move the remaining bits to the right position

    FReadBitPointer := FReadBitPointer + NoOffBits; // New readpointer position

  end else begin                               // Read new data

    NoOffBits := NoOffBits - BitsLeft;
    Result := (FBitBuffer and Mask)            // Clear the bits that are not needed from the old buffer
                               shl NoOffBits;  // Move the remaining bits to the right position

    while (NoOffBits > 0) do begin
      // Read a new byte in the bitbuffer
      if aStream.Read(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
        raise Exception.Create('Read error.');

      if NoOffBits < BitBufferSize then begin
        FReadBitPointer := NoOffBits;
        Result  := Result + FBitBuffer
          shr (BitBufferSize - FReadBitPointer);  // Add the bits from the new bitbuffer}
        NoOffBits := 0;
      end else begin
        FReadBitPointer := 8;
        NoOffBits := NoOffBits - BitBufferSize;
        Result  := Result + FBitBuffer
                                shl (NoOffBits);  // Add the bits from the new bitbuffer}
      end;
    end;
  end;
end;

// ==== TBitWriter =============================================================

constructor TBitWriter.Create;
begin
  FWriteBitPointer := 0;
  FBitBuffer := 0;
end;

procedure TBitWriter.WriteBits(aStream : TStream; Value : Cardinal; NoOffBits : byte);
var
  Mask : Cardinal;
  BitBufferSize, BitsLeft : integer;
begin
  BitBufferSize := SizeOf(FBitBuffer) * 8;

  BitsLeft := BitBufferSize - FWriteBitPointer;

  while NoOffBits > BitsLeft do begin

    FBitBuffer := FBitBuffer + Value shr (NoOffBits - BitsLeft);

    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');

    Mask := $FFFFFFFF shr (32 - (NoOffBits - BitsLeft));
    Value := Value and Mask;

    FBitBuffer := 0;
    FWriteBitPointer := 0;

    NoOffBits := NoOffBits - BitsLeft;

    BitsLeft := BitBufferSize - FWriteBitPointer;
  end;

  if NoOffBits = BitsLeft then begin

    FBitBuffer := FBitBuffer + Value;

    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');

    FBitBuffer := 0;
    FWriteBitPointer := 0;

  end else begin

    FBitBuffer := FBitBuffer + Value shl (BitsLeft - NoOffBits);
    FWriteBitPointer := FWriteBitPointer + NoOffBits;

  end;
end;

function TBitWriter.GetWriteBufferBitsLeft : integer;
begin
  Result := SizeOf(FBitBuffer) * 8 - FWriteBitPointer;
end;

// ==== TPatchChunk ============================================================

constructor TPatchChunk.Create( aStream : TStream);
begin
  FStream := aStream;
  //aStream.Position := 0;
  FBitReader := TBitReader.Create;
  FBitWriter := TBitWriter.Create;
  FWriteBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  {if aStream.Size > 0 then begin
    ReadBuffer(aStream.Size);
  end;}
  FWriteCrc := 0;
  FReadCrc := 0;
end;

destructor TPatchChunk.Destroy;
begin
  FReadBuffer.Free;
  FWriteBuffer.Free;
  FBitWriter.Free;
  FBitReader.Free;

  inherited;
end;

procedure TPatchChunk.ReadBuffer( aSize : integer);
var i : integer;
    Crc : Word;
begin
  FReadBuffer.Size := aSize;
  if FStream.Read(FReadBuffer.Memory^, aSize) <> aSize then
    raise Exception.Create('Read error');

  for i := 0 to aSize - 1 do
    FReadCrc := CrcClavia( FReadCrc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);

  if assigned(FLogLines) then begin
    // Calculate another CRC for the buffer, to be able to compare chunkdata in logfile
    Crc := 0;
    for i := 0 to aSize - 1 do
      Crc := CrcClavia( Crc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);
    FLogLines.Add('Caluclated Crc : ' + IntToHex( Crc, 4));
    FLogLines.Add('');
  end;

  FReadBuffer.Position := 0;

  FBitReader.FReadBitPointer := 8; // Forces a new read of a new byte in the BitBuffer
end;

function TPatchChunk.PeekID: integer;
var aId : byte;
begin
  Result := -1;
  if FStream.Read(aId, SizeOf(aId)) = SizeOf(aid) then begin
    Result := aId;
    FStream.Position := FStream.Position - 1;
  end;
end;

procedure TPatchChunk.ReadChunk;
var bm, bl : byte;
begin
  if FStream.Read(Fid, SizeOf(Fid)) <> SizeOf(Fid) then
    raise Exception.Create('Error reading chunk.');

  if FStream.Read(bm, SizeOf(bm)) <> SizeOf(bm) then
    raise Exception.Create('Error reading chunk.');

  if FStream.Read(bl, SizeOf(bl)) <> SizeOf(bl) then
    raise Exception.Create('Error reading chunk.');

  FSize := bm * 256 + bl;

  if assigned(FLogLines) then begin
    FLogLines.Add('Chunk id: ' + IntToHex(FId,2) + ', size: ' + IntToStr(FSize));
    DumpChunkData(FStream);
  end;

  // Calc crc
  FReadCrc := CrcClavia( FReadCrc, Fid);
  FReadCrc := CrcClavia( FReadCrc, bm);
  FReadCrc := CrcClavia( FReadCrc, bl);

  ReadBuffer( FSize);
end;

function TPatchChunk.ReadBits(NoOffBits : byte): Cardinal;
begin
  Result := FBitReader.ReadBits(FReadBuffer, NoOffBits);
end;

procedure TPatchChunk.WriteBits(Value : Cardinal; NoOffBits : byte);
begin
  FBitWriter.WriteBits(FWriteBuffer, Value, NoOffBits);
end;

procedure TPatchChunk.WriteChunk(aId : byte);
var BufSize : Word;
    bm, bl : byte;
    i : integer;
begin
  // Write the remaining byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');


  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;

  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;
  bm := Hi(BufSize);
  bl := Lo(BufSize);

  // Calc crc
  FWriteCrc := CrcClavia( FWriteCrc, aId);
  FWriteCrc := CrcClavia( FWriteCrc, bm);
  FWriteCrc := CrcClavia( FWriteCrc, bl);
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia( FWriteCrc, PStaticByteBuffer(FWriteBuffer.Memory)^[i]);

  if (FStream.Write(aId, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(bm, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(bl, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');

  FWriteBuffer.Clear;
end;

procedure TPatchChunk.Flush;
var BufSize : Word;
    i : integer;
begin
  // Write the remaining byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');

  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;

  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;

  // Calc crc
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia( FWriteCrc, PStaticByteBuffer(FWriteBuffer.Memory)^[i]);

  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');

  FWriteBuffer.Clear;
end;

function TPatchChunk.GetReadBufferBitsLeft: integer;
begin
  Result := SizeOf(FBitReader.FBitBuffer) * 8 - FBitReader.FReadBitPointer + (FReadBuffer.Size - FReadBuffer.Position) * 8;
end;

procedure TPatchChunk.WriteCrc(aStream : TStream);
var bm, bl : byte;
begin
  bm := (FWriteCrc shr 8) and $ff;
  aStream.Write( bm, SizeOf( bm));
  bl := FWriteCrc and $ff;
  aStream.Write( bl, SizeOf( bl));
end;

function TPatchChunk.ReadName: AnsiString;
var b : byte;
begin
  Result := '';
  b := ReadBits(8);
  while (b <> 0) do begin
    Result := Result + AnsiChar(b);
    if Length(Result) = 16 then
      break;
    b := ReadBits(8);
  end;
end;

procedure TPatchChunk.WriteName(Value : AnsiString);
var i : integer;
    b : Byte;
begin
  i := 1;
  while (i <= Length(Value)) and (i<=16) do begin
    if FWriteBuffer.Write(Value[i], 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i<=16) then begin
    b := 0;
    if FWriteBuffer.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

procedure TPatchChunk.DumpChunkData(aStream : TStream);
var p, i, c, position : integer;
    char_line, line : string;
    b : byte;
begin
  position := aStream.Position;
  try
    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i<FSize) do begin
      if c < 16 then begin
        aStream.Read(b, 1);
        line := line + IntToHex(b, 2) + ' ';
        if b >= 32 then
          char_line := char_line + chr(b)
        else
          char_line := char_line + '.';
        inc(c);
        inc(i);
      end else begin
        FLogLines.Add(IntToHex(p, 6) + ' - ' + line + ' ' + char_line);
        p := i;
        c := 0;
        line := '';
        char_line := '';
      end;
    end;
    if c <> 0 then
      FLogLines.Add(IntToHex(p, 6) + ' - ' + line + stringofchar(' ', 16*3 - Length(line) + 1) + char_line);
  finally
    aStream.Position := position;
  end;
end;


initialization
  G_HighlightColor := XCL_CONTROL_HIGHLIGHT;
  G_SlotStripColor := XCL_CLAVIA_RED;
  G_SlotStripInverseColor := XCL_CLAVIA_BLUE;
  G_SlotStripDisabledColor := CL_BTN_FACE;
  G_CableThickness := 2;

end.
