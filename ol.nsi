; Otus Lisp windows installer
;--------------------------------
; Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
; Defines

  !define REGISTRY_KEY "Software\Otus Lisp\2.0"

;--------------------------------
; General

  ;Name and file
  Name "Otus Lisp (Version 2.0)"
  OutFile "setup.ol-2.0.exe"

  ;Default installation folder
  InstallDir "$LOCALAPPDATA\ol\2.0"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "${REGISTRY_KEY}" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

;--------------------------------
; Variables

  Var StartMenuFolder

;--------------------------------
; Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "LICENCE"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  
  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU"
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "${REGISTRY_KEY}"
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Otus Lisp\2.0"
  
  !insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder
  
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Otus Lisp" SecOL

  SetOutPath "$INSTDIR"
  File "vm.exe"
  File "repl"
  File "ol.exe"

  SetOutPath "$INSTDIR\r5rs"
  File /r r5rs\*.scm
  SetOutPath "$INSTDIR\otus"
  File /r otus\*.scm
  SetOutPath "$INSTDIR\owl"
  File /r owl\*.scm
  SetOutPath "$INSTDIR\lib"
  File /r lib\*.scm
  SetOutPath "$INSTDIR\etc"
  File /r etc\*.scm
  SetOutPath "$INSTDIR\scheme"
  File /r scheme\*.scm

  SetOutPath "$INSTDIR\OpenGL"
  File /r OpenGL\*.scm
  
  ;Store installation folder
  WriteRegStr HKCU "${REGISTRY_KEY}" "" $INSTDIR
  WriteRegStr HKCU "SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\ol.exe" "" "$INSTDIR\ol.exe"

  ;; Set HOME environment variable
  ; include for some of the windows messages defines
  !include "winmessages.nsh"
  ; HKLM (all users) vs HKCU (current user) defines
  !define env_hklm 'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
  !define env_hkcu 'HKCU "Environment"'
  ; set variable for local machine
  WriteRegExpandStr ${env_hklm} OL_HOME $INSTDIR
  ; and current user
  WriteRegExpandStr ${env_hkcu} OL_HOME $INSTDIR
  ; make sure windows knows about the change
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
    CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
; Descriptions

  ; Language strings
  LangString DESC_SecOL ${LANG_ENGLISH} "Binary files with libraries"

  ; Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecOL} $(DESC_SecOL)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete "$INSTDIR\Uninstall.exe"

  RMDir "$INSTDIR"

  ; delete variable
  DeleteRegValue ${env_hklm} OL_HOME
  DeleteRegValue ${env_hkcu} OL_HOME
  ; make sure windows knows about the change
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000
  
  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder
    
  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"
  
  DeleteRegKey /ifempty HKCU "${REGISTRY_KEY}"

SectionEnd
