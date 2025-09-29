'$Debug
'UFO v1.01
'1.0 initial cut - ship random movements above grass
'1.01 controller input to steer ship; allow it to fly over entire screen
'1.02 added "phaser" bolts - basic, flat horizontal, no time decay.  also disabled _delay for max speed

'QB64PE 4.2.0 (4.0+ should work)

'Simulate vehicle flying over rolling terrain

Option _Explicit
Dim Shared As Long scr, wid, hgt, shipImage, shipWid, shipHgt, shipBkgImg, groundHgt, yIndex, grassRect, deviceCount
Dim Shared As Single shipX, shipY, shipDX, shipDY, grassY, grassDY
Dim Shared As Long nPhasers, phaserX(10), phaserY(10) ', phaserLength(10), phaserTurnsLeft(10), phaserBkgImage(10)
'Dim Shared As Single phaserAngle(10)

Randomize Timer
deviceCount = _Devices
scr = _NewImage(1280, 800, 32)
Screen scr
wid = _Width
hgt = _Height
ReDim Shared As Single grassTops(0 To wid - 1)
groundHgt = hgt / 2 'between 0 and hgt/2
Window (0, 0)-(wid - 1, hgt - 1)
$Color:32
Cls , DarkBlue

shipX = 100
shipY = hgt * 3 \ 4
shipDX = 0
shipDY = 0
shipImage = _LoadImage("ufo.png")
shipWid = _Width(shipImage)
shipHgt = _Height(shipImage)
shipBkgImg = _NewImage(shipWid, shipHgt, 32)

grassY = 0: grassDY = 0.5 * Rnd
grassRect = _NewImage(wid - 1, hgt \ 2, 32)

Do
    _Limit 60
    For yIndex = 0 To wid - 1
        hideShip
        hidePhasers
        moveShip
        firePhasers
        moveGround
        showPhasers
        showShip
        _Display
        Select Case InKey$
            Case _CHR_ESC:
                Exit Do
                'If _MessageBox("Confirm", "Quit now?", "YesNo", "Question") = 1 Then Exit Do
        End Select
        '_Delay .005
    Next yIndex
Loop
Screen 0
_FreeImage grassRect: _FreeImage scr: _FreeImage shipImage: _FreeImage shipBkgImg
System

Sub hidePhasers
    'Dim Shared As Long nPhasers, phaserX(10), phaserY(10), phaserLength(10), phaserTurnsLeft(10), phaserBkgImage(10)
    'Dim Shared As Single phaserAngle(10)
    'crude for now - phasers are always angle 0 (straight right) with infinite turns left (until it leaves window)
    'hide by drawing over in background color
    Dim As Integer i, dY
    For i = 1 To nPhasers
        For dY = -1 To 1
            Line (phaserX(i), phaserY(i) + dY)-(phaserX(i) + wid \ 4, phaserY(i) + dY), DarkBlue
        Next dY
    Next i
End Sub

Sub firePhasers
    'only call this when ship is hidden!

    'If devices = 0 Then devices = _Devices 'MUST be read in order for other 2 device functions to work!
    Dim As Integer i, j ', nButtons, dev
    'Locate 1, 1
    'For i% = 1 To _Devices
    '    'Print Str$(i%) + ") " + _Device$(i%)
    '    'Print "Button:"; _LastButton(i%); ",Axis:"; _LastAxis(i%); ",Wheel:"; _LastWheel(i%)
    'Next i%

    'dev = _DeviceInput
    'If dev = 3 Then
    '    nButtons = _LastButton(dev)
    '    Locate 2, 1
    '    Print Spc(40);
    '    Locate 2, 1
    '    For j = 1 To nButtons
    '        'ABXY=1234, 56=LTRT,
    '        If (_Button(j) <> 0) Then
    '            Print j; "="; _Button(j); "  "; '-1=button pressed?
    '        End If
    '    Next j
    'End If
    i = 1
    Do While i <= nPhasers
        phaserX(i) = phaserX(i) + 5
        If phaserX(i) >= wid Then
            'remove phaser(i) from list
            For j = i To nPhasers - 1
                phaserX(j) = phaserX(j + 1)
                phaserY(j) = phaserY(j + 1)
            Next j
            nPhasers = nPhasers - 1
            'Locate 5, 1: Print "Phaser bolts = "; nPhasers
        Else
            i = i + 1
        End If
    Loop
End Sub

Sub showPhasers
    Dim As Integer i, dY
    For i = 1 To nPhasers
        For dY = -1 To 1
            Line (phaserX(i), phaserY(i) + dY)-(phaserX(i) + wid \ 4, phaserY(i) + dY), _IIf(dY, LightYellow, Red)
        Next dY
    Next i
End Sub

Sub moveGround
    Dim As Long grassX, j, grassPlotY
    grassTops(yIndex) = grassY
    grassY = _Clamp(grassY + grassDY, 0, 0.5 * hgt)
    grassDY = _Clamp(grassDY - 0.1 + 0.2 * Rnd, -.5, .5)
    If grassY < 10 Then grassDY = grassDY + 0.005
    If grassY > groundHgt - 10 Then grassDY = grassDY - 0.005
    grassX = wid - 1
    'scroll bottom half of screen left 1 pixel
    _PutImage (0, 0)-(wid - 2, hgt \ 2 - 1), scr, grassRect, (1, 0)-(wid - 1, hgt \ 2 - 1)
    _PutImage (0, 0)-(wid - 2, hgt \ 2 - 1), grassRect, scr, (0, 0)-(wid - 2, hgt \ 2 - 1)
    grassPlotY = grassTops((grassX + yIndex) Mod wid)
    Dim pColor As Long
    If grassPlotY > 0 Then
        pColor = DarkGreen '_RGB(10, 128, 25)
    Else 'bottom of screen - draw water (blue)
        pColor = Blue
    End If
    'PSet (grassx, grassplotY), pColor 'this draws a horizon line but does not fill in
    Line (grassX, hgt \ 2 - 1)-(grassX, grassPlotY - 5), DarkBlue 'erase remnants of prior rightmost pixel column
    Line -(grassX, 0), DarkGreen 'erase remnants of prior rightmost pixel column
    'Line -(grassx, 0), pColor
    For j = 1 To grassPlotY \ 3
        Dim bladey As Single
        'somewhat grass-like texture
        bladey = (grassPlotY - 8) * Rnd
        Line (grassX - 5, bladey)-(grassX - 10 + 10 * Rnd, bladey + 8 - 12 * Rnd), _RGB(10, 64 + 128 * Rnd, 25)
    Next j
End Sub

Sub showShip
    _PutImage , scr, shipBkgImg, (shipX - shipWid \ 2, shipY + shipHgt \ 2)-(shipX - shipWid \ 2 + shipWid - 1, shipY + shipHgt \ 2 - (shipHgt - 1))
    _PutImage (shipX - shipWid \ 2, shipY + shipHgt \ 2)-(shipX - shipWid \ 2 + shipWid - 1, shipY + shipHgt \ 2 - (shipHgt - 1)), shipImage, scr, (1, 1)-(shipWid, shipHgt)
End Sub

Sub hideShip
    _PutImage (shipX - shipWid \ 2, shipY + shipHgt \ 2)-(shipX - shipWid \ 2 + shipWid - 1, shipY + shipHgt \ 2 - (shipHgt - 1)), shipBkgImg, scr
End Sub

Sub moveShip
    'only call this when ship is hidden!
    'devices = _Devices 'MUST be read in order for other 2 device functions to work!
    'Dim i As Integer
    'Locate 1, 1
    'For i% = 1 To _Devices
    'Print Str$(i%) + ") " + _Device$(i%)
    'Print "Button:"; _LastButton(i%); ",Axis:"; _LastAxis(i%); ",Wheel:"; _LastWheel(i%)
    'Next i%
    Static As Single h, v 'last controller input
    Dim dev As Integer
    dev = _DeviceInput
    If dev = 3 Then
        h = (_Axis(1) + _Axis(6)) / 20
        v = (_Axis(2) + _Axis(7)) / 20
        If Abs(h) < 0.001 Then h = 0
        If Abs(v) < 0.001 Then v = 0

        'code to show button and axis states, to id which inputs do what
        'Dim As Integer nButtons, nAxes, i
        'nButtons = _LastButton(dev)
        'nAxes = _LastAxis(dev)
        'Locate 1, 1
        'Print _Device$(dev)
        'For i = 1 To nButtons
        '    'xboxmode: 1=x,2=a,3=b,4=y,5=lb,6=rb,7=lt,8=rt,9=back,10=start,11=lstickdown,12=rstickdown
        '    'psxmode: 1=a/x, 2=b/o, 3=x/sq, 4y/tr=, 5lb=, 6=rb, 7=back/select, 8=start, 9=lstickdown, 10=rstickdown

        '    Print "Button "; i; "="; _Button(i); " ";
        'Next i
        'Print
        'For i = 1 To nAxes
        '    'xboxmode: 1=lstickh, 2=lstickv, 3=rstickh, 4=rstickv, 5=dpadh, 6=dpadv
        '    'psx mode: 1=lstickh, 2=lstickv, 3=lt(+)rt(-), 4=rstickv, 5=rstickh, 6=dpadh, 7=dpadv
        '    Print "Axis "; i; "="; _Axis(i); " ";
        'Next i
        'Print

        '    Print Spc(40);
        '    Locate 2, 1
        '    For j = 1 To nButtons
        '        If (_Button(j) <> 0) Then
        '            Print j; "="; _Button(j); "  "; '-1=button pressed?
        '        End If
        '    Next j

        If nPhasers < 10 Then
            If ((_ButtonChange(6) = -1) And (_Button(6) = -1)) Or ((_ButtonChange(1) = -1) And (_Button(1) = -1)) Then ' Or _ButtonChange(2) = -1 Then
                nPhasers = nPhasers + 1
                'Locate 5, 1: Print "Phaser bolts = "; nPhasers
                phaserX(nPhasers) = shipX + shipWid \ 2 - 5
                phaserY(nPhasers) = shipY - 12 'looks better coming from edge of suacer than vertical center
            End If
        End If

    End If
    shipDX = _Clamp(shipDX + h, -5, 5) 'max speed 5 pixels/frame
    shipDY = _Clamp(shipDY - v, -5, 5)
    shipX = _Clamp(shipX + shipDX, shipWid \ 2 + 1, wid - shipWid \ 2 - 1)
    shipY = _Clamp(shipY + shipDY, 0 * hgt \ 2 + shipHgt \ 2 + 1, hgt - shipHgt \ 2 - 1)
    shipDX = shipDX * 0.97 'friction from the ether slows the ship down
    shipDY = shipDY * 0.97
End Sub
