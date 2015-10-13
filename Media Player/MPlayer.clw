   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

   MAP
     MODULE('MPLAYER_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('MPLAYER001.CLW')
Frame                  PROCEDURE   !
     END
   END

GLO:uiMode           CSTRING(30)
GLO:Test             BYTE
GLO:MediaFile        CSTRING(255)
GLO:FullScreen       BYTE
GLO:MplayerHandle    LONG
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
!endregion

EVENT:PlayMedia     Equate(404h)
EVENT:FullScreen     Equate(404h)

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               CLASS(INIClass)                       ! Global non-volatile storage manager
Fetch                  PROCEDURE(),DERIVED
Update                 PROCEDURE(),DERIVED
                     END

GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\MPlayer.INI', NVD_INI)                    ! Configure INIManager to use INI file
  DctInit
  SYSTEM{PROP:Icon} = 'sv.ico'
  Frame
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()


INIMgr.Fetch PROCEDURE

  CODE
  GLO:uiMode = SELF.TryFetch('Preserved','GLO:uiMode')     ! Resore ' preserved variable' from non-volatile store
  GLO:Test = SELF.TryFetch('Preserved','GLO:Test')         ! Resore ' preserved variable' from non-volatile store
  PARENT.Fetch


INIMgr.Update PROCEDURE

  CODE
  PARENT.Update
  SELF.Update('Preserved','GLO:uiMode',GLO:uiMode)         ! Save 'preserved variable' in non-volatile store
  SELF.Update('Preserved','GLO:Test',GLO:Test)             ! Save 'preserved variable' in non-volatile store

