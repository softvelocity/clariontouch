                     PROGRAM

                     INCLUDE ('TOUCH.INC'),ONCE

EXTENT               GROUP,TYPE
cx                     LONG
cy                     LONG
                     END

                     MAP
                       TouchTest()
                       MODULE('')
                         OutputDebugString (CONST *CSTRING),PASCAL,NAME('OutputDebugStringA')
                       END
                       DBG (*TouchPoint),PRIVATE
                     END

    CODE
  SYSTEM{PROP:Icon} = 'Touch.ico'      
  TouchTest()
  RETURN

! =============================================================================

TouchTest  PROCEDURE()

TouchTest   BYTE(0)
ZoomTest    BYTE(0)
TechData    BYTE(0) !Should we display Coordinates and Stats?

Starts      QUEUE       !A queue, we will use it to store the start points
PointID       ULong(6)
StartPoint  CSTRING(57)
END                         !End queue structure
Drops      QUEUE       !A queue, we will use it to store the points when the finger leaves the screen
PointID       ULong(6)
DropPoint  CSTRING(57)
END                         !End queue structure
Paths           QUEUE       !A queue, we will use it to store the coordinates, real time
PointID       ULong(6)
Coords  CSTRING(57)
END                         !End queue structure


W               WINDOW('Touch Input Test'),AT(,,524,258),CENTER,IMM,AUTO,SYSTEM,MAX, |
                    FONT('MS Shell Dlg',12),COLOR(COLOR:White),WALLPAPER('wp.jpg'),Tiled
                    TOOLBAR,AT(0,0,524,47),USE(?TB),FONT(,,COLOR:White),COLOR(COLOR:Black), |
                        WALLPAPER('toolbar.jpg'),Tiled
                        BUTTON,AT(11,24,23,20),USE(?Close),SKIP,STD(STD:Close),FONT('Microso' & |
                            'ft Sans Serif',8,COLOR:Gray),ICON('close.png'),FLAT,TRN
                        CHECK('Touch'),AT(1,1,9,6),USE(TouchTest),SKIP,TRN,HIDE,FONT('Micros' & |
                            'oft Sans Serif',10),ICON('touch.png')
                        CHECK('Zoom'),AT(1,9,9,6),USE(ZoomTest),SKIP,TRN,HIDE,ICON('Fit-To-S' & |
                            'ize.png')
                        BUTTON,AT(222,0,56,48),USE(?Drag_BTN),SKIP,FONT('Microsoft Sans Serif',8), |
                            ICON('drag.png'),FLAT,TRN
                        BUTTON,AT(164,0,56,48),USE(?Touch_BTN),SKIP,FONT('Microsoft Sans Serif',8), |
                            COLOR(COLOR:Black),ICON('wtouch.png'),FLAT,TRN
                        TEXT,AT(341,0,63,48),USE(?Starts),SKIP,TRN,FLAT,FONT('Microsoft Sans' & |
                            ' Serif',7,0CEFFFFH),READONLY
                        TEXT,AT(406,0,52,48),USE(?Paths),SKIP,TRN,FLAT,FONT('Microsoft Sans ' & |
                            'Serif',7,0FFFFE2H),READONLY
                        TEXT,AT(460,0,63,48),USE(?Drops),SKIP,TRN,FLAT,FONT('Microsoft Sans ' & |
                            'Serif',7,0FFFEFEH),COLOR(COLOR:Black),READONLY
                        BUTTON('Tech Data'),AT(280,0,56,48),USE(?Tech_Data),SKIP, |
                            FONT('Microsoft Sans Serif',8),COLOR(COLOR:Black),ICON('off.png'), |
                            FLAT,TRN
                    END
                    IMAGE('morfo.png'),AT(199,49,87,72),USE(?Img),HIDE,HVSCROLL
                    IMAGE('touch_mode.png'),AT(7,2,157,48),USE(?Selected_Mode),HIDE,TILED
                    STRING(''),AT(24,3,479),USE(?instruct),TRN,FONT('Microsoft Sans Serif', |
                        14,COLOR:White)
                END

Notifier        &ITouchNotifier,AUTO

TouchResponder  CLASS,IMPLEMENTS(IPointerResponder)
FEQs              SIGNED,DIM(10)
IDs               UNSIGNED,DIM(10)
Ends              LONG,DIM(10)
Mids              LONG,DIM(10)
StartTest         PROCEDURE()
EndTest           PROCEDURE()
Tick              PROCEDURE(LONG)
FindPt            PROCEDURE(UNSIGNED),UNSIGNED
                END

ZoomResponder  CLASS,IMPLEMENTS(IGestureResponder)

X                UNSIGNED
Y                UNSIGNED
W                UNSIGNED
H                UNSIGNED
Org              LIKE(POINT)
NaturalSz        LIKE(EXTENT)
Sz               LIKE(EXTENT)

StartTest        PROCEDURE()
EndTest          PROCEDURE()
               END


  CODE
  OPEN (W)
        W {PROP:Pixels} = TRUE
        W{PROP:Buffer} = 1
        W{PROP:LazyDisplay} = 0     
        Notifier &= 0 + W {PROP:TouchInterface}

    ACCEPT !Accept loop
    
        
        CASE ACCEPTED()

        OF ?Tech_Data

            IF TechData = TRUE
                TechData = FALSE
                ?Tech_Data{PROP:Icon} = 'off.png'
                HIDE(?Starts)
                HIDE(?Paths)
                HIDE(?Drops)
            ELSE
                TechData = TRUE
                ?Tech_Data{PROP:Icon} = 'on.png'
                UNHIDE(?Starts)
                UNHIDE(?Paths)
                UNHIDE(?Drops)
      
            END
            
            
        OF ?Drag_BTN
            IF ZoomTest = TRUE
                ZoomTest = FALSE
                DO StopZoomTest
            ELSE
                ZoomTest = TRUE
                DO StopTouchTest
                DO StartZoomTest
      
            END

        OF ?Touch_BTN
            IF TouchTest = TRUE
                TouchTest = FALSE
                DO StopTouchTest
            ELSE
                TouchTest = TRUE
                DO StopZoomTest
                DO StartTouchTest
            END            

        SETCURSOR()
            
    ELSE
      IF EVENT() = EVENT:Timer
        IF TouchTest
          TouchResponder.Tick (CLOCK())
        END
      END
    END
  END

  CLOSE (W)
  RETURN

! ----------------------------------------------------------------------------

StartTouchTest      ROUTINE
    ?Selected_Mode{PROP:Text} = 'touch_mode.png'
    ?instruct{PROP:Text} = 'touch the screen with 1 or more fingers and drag across display, code also detects when finger is lifted'
    UNHIDE(?Selected_Mode)
    W{PROP:Text} = 'Touch Mode Selected'
        ?Drops{PROP:Text} =  '' 
        ?Starts{PROP:Text} = ''
        ?Paths{PROP:Text} = ''
    TouchResponder.StartTest()
  Notifier.TouchResponder (TouchResponder.IPointerResponder)
  EXIT

! ----------------------------------------------------------------------------

StartZoomTest       ROUTINE
    ?instruct{PROP:Text} = 'drag the image or pinch to zoom in/out'
    ?Selected_Mode{PROP:Text} = 'drag_mode.png'
    UNHIDE(?Selected_Mode)
    W{PROP:Text} = 'Zoom Mode Selected'
    SETCURSOR('morfo.cur')
        ?Drops{PROP:Text} =  '' 
        ?Starts{PROP:Text} = ''
        ?Paths{PROP:Text} = ''
   
    ZoomResponder.StartTest()
  Notifier.InputResponder (ZoomResponder.IGestureResponder)
  EXIT

! ----------------------------------------------------------------------------

StopTouchTest       ROUTINE
  
  HIDE(?Selected_Mode)
  W{PROP:Text} = 'SoftVelocity Touch Demo'
  TouchTest = FALSE
  Notifier.TouchResponder()
  TouchResponder.EndTest()
  DISPLAY (?TouchTest)
  EXIT

! ----------------------------------------------------------------------------

StopZoomTest        ROUTINE
    HIDE(?Selected_Mode)
    W{PROP:Text} = 'SoftVelocity Touch Demo'
    SETCURSOR()
  ZoomTest = FALSE
  Notifier.InputResponder()
  ZoomResponder.EndTest()
  DISPLAY (?ZoomTest)
  EXIT

! =============================================================================
!
! Multi-touch test
!    
! =============================================================================

TouchResponder.StartTest  PROCEDURE()

i        UNSIGNED,AUTO

  CODE
  LOOP i = 1 TO MAXIMUM (SELF.IDs, 1)
    SELF.FEQs[i] = 0
    SELF.IDs [i] = 0
    SELF.Ends[1] = 0
    SELF.Mids[1] = 0
  END

  W {PROP:Timer} = 20
  RETURN

! ----------------------------------------------------------------------------

TouchResponder.EndTest  PROCEDURE()

i        UNSIGNED,AUTO

  CODE
  W {PROP:Timer} = 0

  LOOP i = 1 TO MAXIMUM (SELF.FEQs, 1)
    IF SELF.FEQs[i] <> 0
      DESTROY (SELF.FEQs[i])
      SELF.FEQs[i] = 0
    END
    SELF.IDs [i] = 0
    SELF.Ends[i] = 0
    SELF.Mids[i] = 0
  END
  DISPLAY
  RETURN

! ----------------------------------------------------------------------------

TouchResponder.Tick  PROCEDURE (LONG tm)

i        UNSIGNED,AUTO

  CODE
    LOOP i = 1 TO MAXIMUM (SELF.FEQs, 1)
            
    IF SELF.Mids[i] <> 0 AND SELF.Mids[i] < tm
        SELF.FEQs[i] {PROP:Text} = 'dying_dot.png'
    END
        
    IF SELF.Ends[i] <> 0 AND SELF.Ends[i] < tm
      DESTROY (SELF.FEQs[i])
      SELF.FEQs[i] = 0
      SELF.IDs [i] = 0
      SELF.Ends[i] = 0
    END
  END
  RETURN

! ----------------------------------------------------------------------------

TouchResponder.FindPt  PROCEDURE (UNSIGNED id)

i       UNSIGNED,AUTO

  CODE
  i = MAXIMUM (SELF.FEQs, 1)

  LOOP
    IF SELF.FEQs[i] <> 0
      IF SELF.IDs[i] = id
        RETURN i
      END
    END
    i -= 1
  UNTIL i = 0

  RETURN 0

! ----------------------------------------------------------------------------

TouchResponder.IPointerResponder.Event  PROCEDURE (*TouchData tdata)
  
td      &TouchData,AUTO
pt      &TouchPoint,AUTO
xy      LIKE(POINT),AUTO
clr     LONG,AUTO
pic     CSTRING(254),AUTO
i       UNSIGNED,AUTO
ret     BYTE(FALSE)


  CODE
  IF tdata.ctl <> 0
    RETURN FALSE             ! Touch outside WINDOW's client area
  END

  td &= tdata

  LOOP UNTIL td &= NULL
    pt &= td.ptaction
?   DBG (pt)
    IF BAND (pt.PTFlags, PTFLAG_NEW)
      IF SELF.FindPt (pt.ID) = 0
        i = MAXIMUM (SELF.FEQs, 1)

        LOOP UNTIL SELF.FEQs[i] = 0
          i -= 1
        UNTIL i = 0

        IF i <> 0
          xy.x = pt.ptlocal.x
          xy.y = pt.ptlocal.y

            SELF.FEQs[i] = CREATE (0, CREATE:Image)
            SETPOSITION (SELF.FEQs[i], xy.x - 40, xy.y - 40, 80, 80)
            SELF.FEQs[i] {PROP:Text} = 'default_dot.png'

            SELF.FEQs[i] {PROP:NoWidth}  = TRUE
            SELF.FEQs[i] {PROP:NoHeight} = TRUE

            IF TechData = TRUE
                
                Starts.PointID = SELF.FEQs[i]
                GET(Starts,+Starts.PointID)
                IF ERRORCODE() = 30
                    Starts.PointID = SELF.FEQs[i]
                    Starts.StartPoint = 'Started point ' & SELF.FEQs[i] & ' at ' & xy.x - 40 & ',' & xy.y - 40  
                    ADD(Starts,+Starts.PointID)
                END

                          
                ?Starts{PROP:Text} = ''
                Rs# = RECORDS(Starts)
                LOOP Ls# = 10 TO 1 BY -1
                    GET(Starts,Rs#)
                    ?Starts{PROP:Text} = ?Starts{PROP:Text} & Starts.StartPoint & '<13,10>'  !Let's load up the TEXT control with only 10 elements, Full Touch Support involves 10 points
                    Rs# -= 1
                END !LOOP            
            END !IF
            
            UNHIDE (SELF.FEQs[i])

            SELF.IDs [i] = pt.ID
            SELF.ENDs[i] = 0
            SELF.Mids[i] = 0
            
          ret = TRUE
        END
      END
    ELSE
      i = SELF.FindPt (pt.ID)
        
        IF i <> 0
            IF BAND (pt.PTFlags, PTFLAG_UP)    
            SELF.FEQs[i] {PROP:NoWidth}  = TRUE
            SELF.FEQs[i] {PROP:NoHeight} = TRUE
            END
            
        IF BAND (pt.PTFlags, PTFLAG_INCONTACT)

            PIC = 'p_dot.png'
            
        ELSIF BAND (pt.PTFlags, PTFLAG_CANCELED + PTFLAG_UP)
            
            SELF.Ends[i] = CLOCK() + 1.7 * 100         ! + 1.7 sec, cooler effect
            SELF.Mids[i] = CLOCK() + 1.3 * 100         ! + 1.3 sec, cooler effect

            PIC = 'alive_dot.png'
             
            IF TechData = TRUE
                
                Drops.PointID = SELF.FEQs[i]
                GET(Drops,+Drops.PointID)
                IF ERRORCODE() = 30
                    Drops.PointID = SELF.FEQs[i] 
                    Drops.DropPoint = 'Point ' & SELF.FEQs[i] & ' Dropped at ' & SELF.FEQs[i]{PROP:Xpos} & ',' & SELF.FEQs[i]{PROP:Ypos}  
                    ADD(Drops,+Drops.PointID)
                END

                          
                ?Drops{PROP:Text} = ''
                Ds# = RECORDS(Drops)
                LOOP Ys# = 10 TO 1 BY -1
                    GET(Drops,Ds#)
                    ?Drops{PROP:Text} = ?Drops{PROP:Text} & Drops.DropPoint & '<13,10>'  !Let's load up the TEXT control with only 10 elements
                    Ds# -= 1
                END !LOOP            
            END !IF
            
        
        ELSIF NOT BAND (pt.PTFlags, PTFLAG_DOWN)    
            PIC = 'dying_dot.png'
            
        ELSE           
            PIC = 'default_dot.png'
        END

        IF BAND (pt.PTFlags, PTFLAG_UPDATE)
            xy.x = pt.ptlocal.x
            xy.y = pt.ptlocal.y

            IF TechData = TRUE
                 
                Paths.PointID = SELF.FEQs[i]
                GET(Paths,+Paths.PointID)
                IF NOT ERRORCODE()
                    Paths.Coords = 'Point ' & SELF.FEQs[i] & ' at ' & xy.x - 40 & ',' & xy.y - 40 
                    PUT(Paths)
                END
                
                IF ERRORCODE() = 30
                    Paths.PointID = SELF.FEQs[i]
                    Paths.Coords = 'Point ' & SELF.FEQs[i] & ' at ' & xy.x - 40 & ',' & xy.y - 40  
                    ADD(Paths,+Paths.PointID)                
                END
                
                            
                ?Paths{PROP:Text} = ''
                Rp# = RECORDS(Paths)
     
                LOOP Xs# = 10 TO 1 BY -1
                    GET(Paths,Rp#)
                    ?Paths{PROP:Text} = ?Paths{PROP:Text} & Paths.Coords & '<13,10>'  !Real time coordinates, Let's load up the TEXT control with only 10 elements
                    Rp# -= 1
                END !LOOP            
            END   

            SETPOSITION (SELF.FEQs[i], xy.x - 40, xy.y - 40, 80, 80)
        END            

            
            SELF.FEQs[i] {PROP:Text} = PIC

        ret = TRUE
      END
    END

    td &= td.Next()
  END

  IF ret
    DISPLAY
    RETURN TRUE
  END
  RETURN FALSE

! =============================================================================
!
! Zoom test
!    
! =============================================================================

ZoomResponder.StartTest  PROCEDURE()

WW      UNSIGNED,AUTO
WH      UNSIGNED,AUTO

    CODE
  !--- Chose an image for dragging
        
        RR# = RANDOM(1,4)
        CASE RR#
        OF 1
            ?Img{PROP:Text} = 'morfo.png'
        OF 2
            ?Img{PROP:Text} = 'sv_logo.png'
        OF 3
            ?Img{PROP:Text} = 'clarion.png'
        OF 4
            ?Img{PROP:Text} = 'two-dolphins.jpg'
        END !CASE
        
        
  ! --- Get natural image size

  ?Img {PROP:NoWidth}  = TRUE
  ?Img {PROP:NoHeight} = TRUE

        
  ?Img {PROP:HScroll} = FALSE
  ?Img {PROP:VScroll} = FALSE      
        
  GETPOSITION (?Img,,, SELF.W, SELF.H)

  SELF.NaturalSz.cx = SELF.W
  SELF.NaturalSz.cy = SELF.H

  ! --- Place it in the center of WINDOW 

  GETPOSITION (0,,, WW, WH)
  WH -= ?TB {PROP:Height}

  SELF.X = (WW - SELF.W) / 2
  SELF.Y = (WH - SELF.H) / 2

  SETPOSITION (?Img, SELF.X, SELF.Y)
       
  ! --- Unhide and activate

        UNHIDE (?Img)
        
        ?Img {PROP:Active} = TRUE

        Notifier.AllowZoomGesture (?Img, TRUE)
        
  RETURN

! ----------------------------------------------------------------------------

ZoomResponder.EndTest  PROCEDURE()
  CODE
        Notifier.AllowZoomGesture (?Img, FALSE)
  HIDE (?Img)
  RETURN

! ----------------------------------------------------------------------------

ZoomResponder.IGestureResponder.Event  PROCEDURE (*InputData idata)

gesture &InputData,AUTO
WW      UNSIGNED,AUTO
WH      UNSIGNED,AUTO
dx      SIGNED,AUTO
dy      SIGNED,AUTO
sz      LIKE(EXTENT),AUTO

  CODE
  gesture &= idata
    
  LOOP UNTIL gesture &= NULL

    CASE gesture.IA
    OF GESTURE_PAN

      IF BAND (gesture.Param, INFLAG_BEGIN)
        GETPOSITION (?Img, SELF.X, SELF.Y, SELF.W, SELF.H)

        SELF.Org.x = SELF.X
        SELF.Org.y = SELF.Y
      ELSIF NOT BAND (gesture.Param, INFLAG_END)
        dx = gesture.ptaction.pt.x - gesture.pt2.pt.x
        dy = gesture.ptaction.pt.y - gesture.pt2.pt.y

        IF dx <> 0 OR dy <> 0
          GETPOSITION (?Img,,, SELF.W, SELF.H)
          GETPOSITION (0,,, WW, WH)
          WH -= ?TB {PROP:Height}
            
          SELF.X = SELF.Org.X + dx
          
          IF SELF.X + SELF.W > WW
            SELF.X = WW - SELF.W
          END
          IF SELF.X < 0
            SELF.X = 0
          END

          SELF.Y = SELF.Org.Y + dy
          
          IF SELF.Y + SELF.H > WH
            SELF.Y = WH - SELF.Y
          END
          IF SELF.Y < 0
            SELF.Y = 0
          END
            
            SETPOSITION (?Img, SELF.X, SELF.Y)
            ?Paths{PROP:Text} =  'Current Coords ' & SELF.X & ',' & SELF.Y
            DISPLAY
        END
      END
    OF GESTURE_ZOOMIN
    OROF GESTURE_ZOOMOUT
      IF BAND (gesture.Param, INFLAG_BEGIN)
        GETPOSITION (?Img, SELF.X, SELF.Y, SELF.W, SELF.H)
        
        SELF.Sz.cx = SELF.W
        SELF.Sz.cy = SELF.H
        SELF.Org.x = SELF.X + SELF.W / 2
        SELF.Org.y = SELF.Y + SELF.H / 2


      ELSIF NOT BAND (gesture.Param, INFLAG_END)
        IF gesture.IA = GESTURE_ZOOMIN
          sz.cx = INT (SELF.Sz.cx * gesture.Ratio)
          sz.cy = INT (SELF.Sz.cy * gesture.Ratio)
        ELSE
          sz.cx = INT (SELF.Sz.cx / gesture.Ratio)
          sz.cy = INT (SELF.Sz.cy / gesture.Ratio)
        END

        IF sz.cx <> SELF.W OR sz.cy <> SELF.H
          GETPOSITION (0,,, WW, WH)
          WH -= ?TB {PROP:Height}

          IF sz.cx < SELF.NaturalSz.cx OR sz.cy < SELF.NaturalSz.cy
            sz.cx = SELF.NaturalSz.cx
            sz.cy = SELF.NaturalSz.cy
          END

          IF sz.cx > WW
            sz.cx = WW
            sz.cy = WW * SELF.sz.cy / SELF.sz.cx
          END
          IF sz.cy > WH
            sz.cy = WH
            sz.cx = WH * SELF.sz.cx / SELF.sz.cy
          END

          SELF.W = sz.cx
          SELF.H = sz.cy
          SELF.X = SELF.Org.x - sz.cx / 2
          SELF.Y = SELF.Org.y - sz.cy / 2

          IF SELF.X < 0
            SELF.X = 0
          END
          IF SELF.Y < 0
            SELF.Y = 0
          END

            SETPOSITION (?Img, SELF.X, SELF.Y, SELF.W, SELF.H)

        END
      END
    ELSE
      RETURN FALSE
    END

    gesture &= gesture.Next()
  END

  DISPLAY
  
  RETURN TRUE

! =============================================================================
!
! Tracing
!    
! =============================================================================

DBG  PROCEDURE (*TouchPoint pt)

M      CSTRING (1000)

  CODE
   M = 'ID=' & pt.ID & '; Flags:'

   IF BAND (pt.PTFlags, PTFLAG_NEW)
     M = M & ' NEW'
   END

   IF BAND (pt.PTFlags, PTFLAG_PRIMARY)
     M = M & ' PRIMARY'
   END

   IF BAND (pt.PTFlags, PTFLAG_INCONTACT)
     M = M & ' INCONTACT'
   END

   IF BAND (pt.PTFlags, PTFLAG_INRANGE)
     M = M & ' INRANGE'
   END

   IF BAND (pt.PTFlags, PTFLAG_DOWN)
     M = M & ' DOWN'
   END

   IF BAND (pt.PTFlags, PTFLAG_UP)
     M = M & ' UP'
   END

   IF BAND (pt.PTFlags, PTFLAG_UPDATE)
     M = M & ' UPDATE'
   END

   IF BAND (pt.PTFlags, PTFLAG_CANCELED)
     M = M & ' CANCELED'
   END

    M = M & '<13,10>'
    
    OutputDebugString (M)
   RETURN
