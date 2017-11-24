{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
List of common Windows Messages

2005 Shengalts Aleksander aka Instructor <Shengalts@mail.ru>

For usage example see @Examples\/WinMessages.hs@.

> Prefix  Message category
> -------------------------
> SW      ShowWindow Commands
> BM      Button control
> CB      Combo box control
> EM      Edit control
> LB      List box control
> WM      General window
> ABM     Application desktop toolbar
> DBT     Device
> DM      Default push button control
> HDM     Header control
> LVM     List view control
> SB      Status bar window
> SBM     Scroll bar control
> STM     Static control
> TCM     Tab control
> PBM     Progress bar

> NOT included messages (WM_USER + X)
> -----------------------------------
> CBEM    Extended combo box control
> CDM     Common dialog box
> DL      Drag list box
> DTM     Date and time picker control
> HKM     Hot key control
> IPM     IP address control
> MCM     Month calendar control
> PGM     Pager control
> PSM     Property sheet
> RB      Rebar control
> TB      Toolbar
> TBM     Trackbar
> TTM     Tooltip control
> TVM     Tree-view control
> UDM     Up-down control
-}
module Development.NSIS.Plugins.WinMessages where

hwnd_BROADCAST = 0xFFFF

-- ShowWindow Commands
sw_HIDE             = 0
sw_SHOWNORMAL       = 1
sw_NORMAL           = 1
sw_SHOWMINIMIZED    = 2
sw_SHOWMAXIMIZED    = 3
sw_MAXIMIZE         = 3
sw_SHOWNOACTIVATE   = 4
sw_SHOW             = 5
sw_MINIMIZE         = 6
sw_SHOWMINNOACTIVE  = 7
sw_SHOWNA           = 8
sw_RESTORE          = 9
sw_SHOWDEFAULT      = 10
sw_FORCEMINIMIZE    = 11
sw_MAX              = 11

-- Button Control Messages --
bm_CLICK           = 0x00F5
bm_GETCHECK        = 0x00F0
bm_GETIMAGE        = 0x00F6
bm_GETSTATE        = 0x00F2
bm_SETCHECK        = 0x00F1
bm_SETIMAGE        = 0x00F7
bm_SETSTATE        = 0x00F3
bm_SETSTYLE        = 0x00F4

bst_UNCHECKED      = 0
bst_CHECKED        = 1
bst_INDETERMINATE  = 2
bst_PUSHED         = 4
bst_FOCUS          = 8

-- Combo Box Messages --
cb_ADDSTRING                = 0x0143
cb_DELETESTRING             = 0x0144
cb_DIR                      = 0x0145
cb_FINDSTRING               = 0x014C
cb_FINDSTRINGEXACT          = 0x0158
cb_GETCOUNT                 = 0x0146
cb_GETCURSEL                = 0x0147
cb_GETDROPPEDCONTROLRECT    = 0x0152
cb_GETDROPPEDSTATE          = 0x0157
cb_GETDROPPEDWIDTH          = 0x015f
cb_GETEDITSEL               = 0x0140
cb_GETEXTENDEDUI            = 0x0156
cb_GETHORIZONTALEXTENT      = 0x015d
cb_GETITEMDATA              = 0x0150
cb_GETITEMHEIGHT            = 0x0154
cb_GETLBTEXT                = 0x0148
cb_GETLBTEXTLEN             = 0x0149
cb_GETLOCALE                = 0x015A
cb_GETTOPINDEX              = 0x015b
cb_INITSTORAGE              = 0x0161
cb_INSERTSTRING             = 0x014A
cb_LIMITTEXT                = 0x0141
cb_MSGMAX                   = 0x015B  -- 0x0162 0x0163
cb_MULTIPLEADDSTRING        = 0x0163
cb_RESETCONTENT             = 0x014B
cb_SELECTSTRING             = 0x014D
cb_SETCURSEL                = 0x014E
cb_SETDROPPEDWIDTH          = 0x0160
cb_SETEDITSEL               = 0x0142
cb_SETEXTENDEDUI            = 0x0155
cb_SETHORIZONTALEXTENT      = 0x015e
cb_SETITEMDATA              = 0x0151
cb_SETITEMHEIGHT            = 0x0153
cb_SETLOCALE                = 0x0159
cb_SETTOPINDEX              = 0x015c
cb_SHOWDROPDOWN             = 0x014F

cb_ERR                      = -1

-- Edit Control Messages --
em_CANUNDO              = 0x00C6
em_CHARFROMPOS          = 0x00D7
em_EMPTYUNDOBUFFER      = 0x00CD
em_EXLIMITTEXT          = 0x0435
em_FMTLINES             = 0x00C8
em_GETFIRSTVISIBLELINE  = 0x00CE
em_GETHANDLE            = 0x00BD
em_GETIMESTATUS         = 0x00D9
em_GETLIMITTEXT         = 0x00D5
em_GETLINE              = 0x00C4
em_GETLINECOUNT         = 0x00BA
em_GETMARGINS           = 0x00D4
em_GETMODIFY            = 0x00B8
em_GETPASSWORDCHAR      = 0x00D2
em_GETRECT              = 0x00B2
em_GETSEL               = 0x00B0
em_GETTHUMB             = 0x00BE
em_GETWORDBREAKPROC     = 0x00D1
em_LIMITTEXT            = 0x00C5
em_LINEFROMCHAR         = 0x00C9
em_LINEINDEX            = 0x00BB
em_LINELENGTH           = 0x00C1
em_LINESCROLL           = 0x00B6
em_POSFROMCHAR          = 0x00D6
em_REPLACESEL           = 0x00C2
em_SCROLL               = 0x00B5
em_SCROLLCARET          = 0x00B7
em_SETHANDLE            = 0x00BC
em_SETIMESTATUS         = 0x00D8
em_SETLIMITTEXT         = 0x00C5  -- Same as EM_LIMITTEXT
em_SETMARGINS           = 0x00D3
em_SETMODIFY            = 0x00B9
em_SETPASSWORDCHAR      = 0x00CC
em_SETREADONLY          = 0x00CF
em_SETRECT              = 0x00B3
em_SETRECTNP            = 0x00B4
em_SETSEL               = 0x00B1
em_SETTABSTOPS          = 0x00CB
em_SETWORDBREAKPROC     = 0x00D0
em_UNDO                 = 0x00C7

-- Listbox Messages --
lb_ADDFILE              = 0x0196
lb_ADDSTRING            = 0x0180
lb_DELETESTRING         = 0x0182
lb_DIR                  = 0x018D
lb_FINDSTRING           = 0x018F
lb_FINDSTRINGEXACT      = 0x01A2
lb_GETANCHORINDEX       = 0x019D
lb_GETCARETINDEX        = 0x019F
lb_GETCOUNT             = 0x018B
lb_GETCURSEL            = 0x0188
lb_GETHORIZONTALEXTENT  = 0x0193
lb_GETITEMDATA          = 0x0199
lb_GETITEMHEIGHT        = 0x01A1
lb_GETITEMRECT          = 0x0198
lb_GETLOCALE            = 0x01A6
lb_GETSEL               = 0x0187
lb_GETSELCOUNT          = 0x0190
lb_GETSELITEMS          = 0x0191
lb_GETTEXT              = 0x0189
lb_GETTEXTLEN           = 0x018A
lb_GETTOPINDEX          = 0x018E
lb_INITSTORAGE          = 0x01A8
lb_INSERTSTRING         = 0x0181
lb_ITEMFROMPOINT        = 0x01A9
lb_MSGMAX               = 0x01A8  -- 0x01B0 0x01B1
lb_MULTIPLEADDSTRING    = 0x01B1
lb_RESETCONTENT         = 0x0184
lb_SELECTSTRING         = 0x018C
lb_SELITEMRANGE         = 0x019B
lb_SELITEMRANGEEX       = 0x0183
lb_SETANCHORINDEX       = 0x019C
lb_SETCARETINDEX        = 0x019E
lb_SETCOLUMNWIDTH       = 0x0195
lb_SETCOUNT             = 0x01A7
lb_SETCURSEL            = 0x0186
lb_SETHORIZONTALEXTENT  = 0x0194
lb_SETITEMDATA          = 0x019A
lb_SETITEMHEIGHT        = 0x01A0
lb_SETLOCALE            = 0x01A5
lb_SETSEL               = 0x0185
lb_SETTABSTOPS          = 0x0192
lb_SETTOPINDEX          = 0x0197

lb_ERR                  = -1

-- Window Messages --
wm_ACTIVATE                     = 0x0006
wm_ACTIVATEAPP                  = 0x001C
wm_AFXFIRST                     = 0x0360
wm_AFXLAST                      = 0x037F
wm_APP                          = 0x8000
wm_APPCOMMAND                   = 0x0319
wm_ASKCBFORMATNAME              = 0x030C
wm_CANCELJOURNAL                = 0x004B
wm_CANCELMODE                   = 0x001F
wm_CAPTURECHANGED               = 0x0215
wm_CHANGECBCHAIN                = 0x030D
wm_CHANGEUISTATE                = 0x0127
wm_CHAR                         = 0x0102
wm_CHARTOITEM                   = 0x002F
wm_CHILDACTIVATE                = 0x0022
wm_CLEAR                        = 0x0303
wm_CLOSE                        = 0x0010
wm_COMMAND                      = 0x0111
wm_COMMNOTIFY                   = 0x0044  -- no longer suported
wm_COMPACTING                   = 0x0041
wm_COMPAREITEM                  = 0x0039
wm_CONTEXTMENU                  = 0x007B
wm_CONVERTREQUESTEX             = 0x108
wm_COPY                         = 0x0301
wm_COPYDATA                     = 0x004A
wm_CREATE                       = 0x0001
wm_CTLCOLOR                     = 0x0019
wm_CTLCOLORBTN                  = 0x0135
wm_CTLCOLORDLG                  = 0x0136
wm_CTLCOLOREDIT                 = 0x0133
wm_CTLCOLORLISTBOX              = 0x0134
wm_CTLCOLORMSGBOX               = 0x0132
wm_CTLCOLORSCROLLBAR            = 0x0137
wm_CTLCOLORSTATIC               = 0x0138
wm_CUT                          = 0x0300
wm_DDE_FIRST                    = 0x3E0
wm_DEADCHAR                     = 0x0103
wm_DELETEITEM                   = 0x002D
wm_DESTROY                      = 0x0002
wm_DESTROYCLIPBOARD             = 0x0307
wm_DEVICECHANGE                 = 0x0219
wm_DEVMODECHANGE                = 0x001B
wm_DISPLAYCHANGE                = 0x007E
wm_DRAWCLIPBOARD                = 0x0308
wm_DRAWITEM                     = 0x002B
wm_DROPFILES                    = 0x0233
wm_ENABLE                       = 0x000A
wm_ENDSESSION                   = 0x0016
wm_ENTERIDLE                    = 0x0121
wm_ENTERMENULOOP                = 0x0211
wm_ENTERSIZEMOVE                = 0x0231
wm_ERASEBKGND                   = 0x0014
wm_EXITMENULOOP                 = 0x0212
wm_EXITSIZEMOVE                 = 0x0232
wm_FONTCHANGE                   = 0x001D
wm_GETDLGCODE                   = 0x0087
wm_GETFONT                      = 0x0031
wm_GETHOTKEY                    = 0x0033
wm_GETICON                      = 0x007F
wm_GETMINMAXINFO                = 0x0024
wm_GETOBJECT                    = 0x003D
wm_GETTEXT                      = 0x000D
wm_GETTEXTLENGTH                = 0x000E
wm_HANDHELDFIRST                = 0x0358
wm_HANDHELDLAST                 = 0x035F
wm_HELP                         = 0x0053
wm_HOTKEY                       = 0x0312
wm_HSCROLL                      = 0x0114
wm_HSCROLLCLIPBOARD             = 0x030E
wm_ICONERASEBKGND               = 0x0027
wm_IME_CHAR                     = 0x0286
wm_IME_COMPOSITION              = 0x010F
wm_IME_COMPOSITIONFULL          = 0x0284
wm_IME_CONTROL                  = 0x0283
wm_IME_ENDCOMPOSITION           = 0x010E
wm_IME_KEYDOWN                  = 0x0290
wm_IME_KEYLAST                  = 0x010F
wm_IME_KEYUP                    = 0x0291
wm_IME_NOTIFY                   = 0x0282
wm_IME_REQUEST                  = 0x0288
wm_IME_SELECT                   = 0x0285
wm_IME_SETCONTEXT               = 0x0281
wm_IME_STARTCOMPOSITION         = 0x010D
wm_INITDIALOG                   = 0x0110
wm_INITMENU                     = 0x0116
wm_INITMENUPOPUP                = 0x0117
wm_INPUT                        = 0x00FF
wm_INPUTLANGCHANGE              = 0x0051
wm_INPUTLANGCHANGEREQUEST       = 0x0050
wm_KEYDOWN                      = 0x0100
wm_KEYFIRST                     = 0x0100
wm_KEYLAST                      = 0x0108
wm_KEYUP                        = 0x0101
wm_KILLFOCUS                    = 0x0008
wm_LBUTTONDBLCLK                = 0x0203
wm_LBUTTONDOWN                  = 0x0201
wm_LBUTTONUP                    = 0x0202
wm_MBUTTONDBLCLK                = 0x0209
wm_MBUTTONDOWN                  = 0x0207
wm_MBUTTONUP                    = 0x0208
wm_MDIACTIVATE                  = 0x0222
wm_MDICASCADE                   = 0x0227
wm_MDICREATE                    = 0x0220
wm_MDIDESTROY                   = 0x0221
wm_MDIGETACTIVE                 = 0x0229
wm_MDIICONARRANGE               = 0x0228
wm_MDIMAXIMIZE                  = 0x0225
wm_MDINEXT                      = 0x0224
wm_MDIREFRESHMENU               = 0x0234
wm_MDIRESTORE                   = 0x0223
wm_MDISETMENU                   = 0x0230
wm_MDITILE                      = 0x0226
wm_MEASUREITEM                  = 0x002C
wm_MENUCHAR                     = 0x0120
wm_MENUCOMMAND                  = 0x0126
wm_MENUDRAG                     = 0x0123
wm_MENUGETOBJECT                = 0x0124
wm_MENURBUTTONUP                = 0x0122
wm_MENUSELECT                   = 0x011F
wm_MOUSEACTIVATE                = 0x0021
wm_MOUSEFIRST                   = 0x0200
wm_MOUSEHOVER                   = 0x02A1
wm_MOUSELAST                    = 0x0209  -- 0x020A 0x020D
wm_MOUSELEAVE                   = 0x02A3
wm_MOUSEMOVE                    = 0x0200
wm_MOUSEWHEEL                   = 0x020A
wm_MOVE                         = 0x0003
wm_MOVING                       = 0x0216
wm_NCACTIVATE                   = 0x0086
wm_NCCALCSIZE                   = 0x0083
wm_NCCREATE                     = 0x0081
wm_NCDESTROY                    = 0x0082
wm_NCHITTEST                    = 0x0084
wm_NCLBUTTONDBLCLK              = 0x00A3
wm_NCLBUTTONDOWN                = 0x00A1
wm_NCLBUTTONUP                  = 0x00A2
wm_NCMBUTTONDBLCLK              = 0x00A9
wm_NCMBUTTONDOWN                = 0x00A7
wm_NCMBUTTONUP                  = 0x00A8
wm_NCMOUSEHOVER                 = 0x02A0
wm_NCMOUSELEAVE                 = 0x02A2
wm_NCMOUSEMOVE                  = 0x00A0
wm_NCPAINT                      = 0x0085
wm_NCRBUTTONDBLCLK              = 0x00A6
wm_NCRBUTTONDOWN                = 0x00A4
wm_NCRBUTTONUP                  = 0x00A5
wm_NCXBUTTONDBLCLK              = 0x00AD
wm_NCXBUTTONDOWN                = 0x00AB
wm_NCXBUTTONUP                  = 0x00AC
wm_NEXTDLGCTL                   = 0x0028
wm_NEXTMENU                     = 0x0213
wm_NOTIFY                       = 0x004E
wm_NOTIFYFORMAT                 = 0x0055
wm_NULL                         = 0x0000
wm_PAINT                        = 0x000F
wm_PAINTCLIPBOARD               = 0x0309
wm_PAINTICON                    = 0x0026
wm_PALETTECHANGED               = 0x0311
wm_PALETTEISCHANGING            = 0x0310
wm_PARENTNOTIFY                 = 0x0210
wm_PASTE                        = 0x0302
wm_PENWINFIRST                  = 0x0380
wm_PENWINLAST                   = 0x038F
wm_POWER                        = 0x0048
wm_POWERBROADCAST               = 0x0218
wm_PRINT                        = 0x0317
wm_PRINTCLIENT                  = 0x0318
wm_QUERYDRAGICON                = 0x0037
wm_QUERYENDSESSION              = 0x0011
wm_QUERYNEWPALETTE              = 0x030F
wm_QUERYOPEN                    = 0x0013
wm_QUERYUISTATE                 = 0x0129
wm_QUEUESYNC                    = 0x0023
wm_QUIT                         = 0x0012
wm_RBUTTONDBLCLK                = 0x0206
wm_RBUTTONDOWN                  = 0x0204
wm_RBUTTONUP                    = 0x0205
wm_RASDIALEVENT                 = 0xCCCD
wm_RENDERALLFORMATS             = 0x0306
wm_RENDERFORMAT                 = 0x0305
wm_SETCURSOR                    = 0x0020
wm_SETFOCUS                     = 0x0007
wm_SETFONT                      = 0x0030
wm_SETHOTKEY                    = 0x0032
wm_SETICON                      = 0x0080
wm_SETREDRAW                    = 0x000B
wm_SETTEXT                      = 0x000C
wm_SETTINGCHANGE                = 0x001A  -- Same as WM_WININICHANGE
wm_SHOWWINDOW                   = 0x0018
wm_SIZE                         = 0x0005
wm_SIZECLIPBOARD                = 0x030B
wm_SIZING                       = 0x0214
wm_SPOOLERSTATUS                = 0x002A
wm_STYLECHANGED                 = 0x007D
wm_STYLECHANGING                = 0x007C
wm_SYNCPAINT                    = 0x0088
wm_SYSCHAR                      = 0x0106
wm_SYSCOLORCHANGE               = 0x0015
wm_SYSCOMMAND                   = 0x0112
wm_SYSDEADCHAR                  = 0x0107
wm_SYSKEYDOWN                   = 0x0104
wm_SYSKEYUP                     = 0x0105
wm_TABLET_FIRST                 = 0x02C0
wm_TABLET_LAST                  = 0x02DF
wm_THEMECHANGED                 = 0x031A
wm_TCARD                        = 0x0052
wm_TIMECHANGE                   = 0x001E
wm_TIMER                        = 0x0113
wm_UNDO                         = 0x0304
wm_UNICHAR                      = 0x0109
wm_UNINITMENUPOPUP              = 0x0125
wm_UPDATEUISTATE                = 0x0128
wm_USER                         = 0x400
wm_USERCHANGED                  = 0x0054
wm_VKEYTOITEM                   = 0x002E
wm_VSCROLL                      = 0x0115
wm_VSCROLLCLIPBOARD             = 0x030A
wm_WINDOWPOSCHANGED             = 0x0047
wm_WINDOWPOSCHANGING            = 0x0046
wm_WININICHANGE                 = 0x001A
wm_WTSSESSION_CHANGE            = 0x02B1
wm_XBUTTONDBLCLK                = 0x020D
wm_XBUTTONDOWN                  = 0x020B
wm_XBUTTONUP                    = 0x020C


-- Application desktop toolbar --
abm_ACTIVATE         = 0x00000006  -- lParam == TRUE/FALSE means activate/deactivate
abm_GETAUTOHIDEBAR   = 0x00000007
abm_GETSTATE         = 0x00000004
abm_GETTASKBARPOS    = 0x00000005
abm_NEW              = 0x00000000
abm_QUERYPOS         = 0x00000002
abm_REMOVE           = 0x00000001
abm_SETAUTOHIDEBAR   = 0x00000008  -- This can fail, you MUST check the result
abm_SETPOS           = 0x00000003
abm_WINDOWPOSCHANGED = 0x0000009

-- Device --
dbt_APPYBEGIN                   = 0x0000
dbt_APPYEND                     = 0x0001
dbt_CONFIGCHANGECANCELED        = 0x0019
dbt_CONFIGCHANGED               = 0x0018
dbt_CONFIGMGAPI32               = 0x0022
dbt_CONFIGMGPRIVATE             = 0x7FFF
dbt_CUSTOMEVENT                 = 0x8006  -- User-defined event
dbt_DEVICEARRIVAL               = 0x8000  -- System detected a new device
dbt_DEVICEQUERYREMOVE           = 0x8001  -- Wants to remove, may fail
dbt_DEVICEQUERYREMOVEFAILED     = 0x8002  -- Removal aborted
dbt_DEVICEREMOVECOMPLETE        = 0x8004  -- Device is gone
dbt_DEVICEREMOVEPENDING         = 0x8003  -- About to remove, still avail.
dbt_DEVICETYPESPECIFIC          = 0x8005  -- Type specific event
dbt_DEVNODES_CHANGED            = 0x0007
dbt_DEVTYP_DEVICEINTERFACE      = 0x00000005  -- Device interface class
dbt_DEVTYP_DEVNODE              = 0x00000001  -- Devnode number
dbt_DEVTYP_HANDLE               = 0x00000006  -- File system handle
dbt_DEVTYP_NET                  = 0x00000004  -- Network resource
dbt_DEVTYP_OEM                  = 0x00000000  -- Oem-defined device type
dbt_DEVTYP_PORT                 = 0x00000003  -- Serial, parallel
dbt_DEVTYP_VOLUME               = 0x00000002  -- Logical volume
dbt_LOW_DISK_SPACE              = 0x0048
dbt_MONITORCHANGE               = 0x001B
dbt_NO_DISK_SPACE               = 0x0047
dbt_QUERYCHANGECONFIG           = 0x0017
dbt_SHELLLOGGEDON               = 0x0020
dbt_USERDEFINED                 = 0xFFFF
dbt_VOLLOCKLOCKFAILED           = 0x8043
dbt_VOLLOCKLOCKRELEASED         = 0x8045
dbt_VOLLOCKLOCKTAKEN            = 0x8042
dbt_VOLLOCKQUERYLOCK            = 0x8041
dbt_VOLLOCKQUERYUNLOCK          = 0x8044
dbt_VOLLOCKUNLOCKFAILED         = 0x8046
dbt_VPOWERDAPI                  = 0x8100  -- VPOWERD API for Win95
dbt_VXDINITCOMPLETE             = 0x0023

-- Default push button control --
dm_BITSPERPEL       = 0x00040000
dm_COLLATE          = 0x00008000
dm_COLOR            = 0x00000800
dm_COPIES           = 0x00000100
dm_DEFAULTSOURCE    = 0x00000200
dm_DISPLAYFLAGS     = 0x00200000
dm_DISPLAYFREQUENCY = 0x00400000
dm_DITHERTYPE       = 0x04000000
dm_DUPLEX           = 0x00001000
dm_FORMNAME         = 0x00010000
dm_GRAYSCALE        = 0x00000001  -- This flag is no longer valid
dm_ICMINTENT        = 0x01000000
dm_ICMMETHOD        = 0x00800000
dm_INTERLACED       = 0x00000002  -- This flag is no longer valid
dm_LOGPIXELS        = 0x00020000
dm_MEDIATYPE        = 0x02000000
dm_NUP              = 0x00000040
dm_ORIENTATION      = 0x00000001
dm_PANNINGHEIGHT    = 0x10000000
dm_PANNINGWIDTH     = 0x08000000
dm_PAPERLENGTH      = 0x00000004
dm_PAPERSIZE        = 0x00000002
dm_PAPERWIDTH       = 0x00000008
dm_PELSHEIGHT       = 0x00100000
dm_PELSWIDTH        = 0x00080000
dm_POSITION         = 0x00000020
dm_PRINTQUALITY     = 0x00000400
dm_SCALE            = 0x00000010
dm_SPECVERSION      = 0x0320       -- 0x0400 0x0401
dm_TTOPTION         = 0x00004000
dm_YRESOLUTION      = 0x00002000

-- Header control --
hdm_FIRST           = 0x1200

-- List view control --
lvm_FIRST           = 0x1000

-- Status bar window --
sb_CONST_ALPHA      = 0x00000001
sb_GRAD_RECT        = 0x00000010
sb_GRAD_TRI         = 0x00000020
sb_NONE             = 0x00000000
sb_PIXEL_ALPHA      = 0x00000002
sb_PREMULT_ALPHA    = 0x00000004
sb_SIMPLEID         = 0x00ff

-- Scroll bar control --
sbm_ENABLE_ARROWS           = 0x00E4  -- Not in win3.1
sbm_GETPOS                  = 0x00E1  -- Not in win3.1
sbm_GETRANGE                = 0x00E3  -- Not in win3.1
sbm_GETSCROLLINFO           = 0x00EA
sbm_SETPOS                  = 0x00E0  -- Not in win3.1
sbm_SETRANGE                = 0x00E2  -- Not in win3.1
sbm_SETRANGEREDRAW          = 0x00E6  -- Not in win3.1
sbm_SETSCROLLINFO           = 0x00E9

-- Static control --
stm_GETICON                 = 0x0171
stm_GETIMAGE                = 0x0173
stm_MSGMAX                  = 0x0174
stm_ONLY_THIS_INTERFACE     = 0x00000001
stm_ONLY_THIS_NAME          = 0x00000008
stm_ONLY_THIS_PROTOCOL      = 0x00000002
stm_ONLY_THIS_TYPE          = 0x00000004
stm_SETICON                 = 0x0170
stm_SETIMAGE                = 0x0172

-- Tab control --
tcm_FIRST                   = 0x1300

-- Progress bar control --
pbm_SETRANGE   = 0x0401
pbm_SETPOS     = 0x0402
pbm_DELTAPOS   = 0x0403
pbm_SETSTEP    = 0x0404
pbm_STEPIT     = 0x0405
pbm_GETPOS     = 0x0408
pbm_SETMARQUEE = 0x040a
