'$Debug
'UFO v1.01
'1.0 initial cut - ship random movements above grass
'1.01 controller input to steer ship; allow it to fly over entire screen
'1.02 added "phaser" bolts - basic, flat horizontal, no time decay.  also disabled _delay for max speed
'1.021 fatter phaser bolts, with red core
'1.022 Added table of Logitech F310 controller inputs, for PSX and XBox modes
'1.023 Restructured using Types to encapsulate related variables (but not arrays). Moved controller input map to separate text file - IDE was getting sluggish

'QB64PE 4.2.0 (4.0+ should work)

'Simulate vehicle flying over rolling terrain

Option _Explicit
$Color:32

Type ScreenInfo
    As Long image, width, height
End Type

Type ShipInfo
    As Long sourceImage, backimage, width, height, backgroundImage
    As Single x, y, dx, dy
End Type

Type GroundInfo
    As Long maxY, panBufferImage
    As Single lastY, dY
End Type

Type PhaserInfo
    count As Integer
End Type

Dim Shared scrn As ScreenInfo
Dim Shared ship As ShipInfo
Dim Shared ground As GroundInfo
Dim Shared phasers As PhaserInfo
Dim Shared As Long deviceCount
Dim Shared As Long phaserX(10), phaserY(10) ', phaserLength(10), phaserTurnsLeft(10), phaserBkgImage(10)
'Dim Shared As Single phaserAngle(10)

'initialization - runtime
Randomize Timer

'initialization - controller
deviceCount = _Devices

'initialization - screen
scrn.width = 1280
scrn.height = 800
scrn.image = _NewImage(scrn.width, scrn.height, 32)
Screen scrn.image
Window (0, 0)-(scrn.width - 1, scrn.height - 1)
Cls , DarkBlue

'initialization - ground
ReDim Shared As Single groundTops(0 To scrn.width - 1)
ground.maxY = scrn.height / 2 'between 0 and hgt/2
ground.lastY = 0: ground.dY = 0.5 * Rnd
ground.panBufferImage = _NewImage(scrn.width - 1, scrn.height \ 2, 32)

'initialization - ship
ship.x = 100
ship.y = scrn.height * 3 \ 4
ship.dx = 0
ship.dy = 0
ship.sourceImage = _LoadImage("ufo.png")
ship.width = _Width(ship.sourceImage)
ship.height = _Height(ship.sourceImage)
ship.backimage = _NewImage(ship.width, ship.height, 32)

'Main Loop
Do
    _Limit 60
    Dim yIndex As Integer
    For yIndex = 0 To scrn.width - 1
        hideShip
        hidePhasers
        moveShip
        firePhasers
        moveGround yIndex
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
_FreeImage ground.panBufferImage: _FreeImage scrn.image: _FreeImage ship.sourceImage: _FreeImage ship.backimage
System

Sub hidePhasers
    'Dim Shared As Long phasers.count, phaserX(10), phaserY(10), phaserLength(10), phaserTurnsLeft(10), phaserBkgImage(10)
    'Dim Shared As Single phaserAngle(10)
    'crude for now - phasers are always angle 0 (straight right) with infinite turns left (until it leaves window)
    'hide by drawing over in background color
    Dim As Integer i, dY
    For i = 1 To phasers.count
        For dY = -1 To 1
            Line (phaserX(i), phaserY(i) + dY)-(phaserX(i) + scrn.width \ 4, phaserY(i) + dY), DarkBlue
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
    Do While i <= phasers.count
        phaserX(i) = phaserX(i) + 5
        If phaserX(i) >= scrn.width Then
            'remove phaser(i) from list
            For j = i To phasers.count - 1
                phaserX(j) = phaserX(j + 1)
                phaserY(j) = phaserY(j + 1)
            Next j
            phasers.count = phasers.count - 1
            'Locate 5, 1: Print "Phaser bolts = "; phasers.count
        Else
            i = i + 1
        End If
    Loop
End Sub

Sub showPhasers
    Dim As Integer i, dY
    For i = 1 To phasers.count
        For dY = -1 To 1
            Line (phaserX(i), phaserY(i) + dY)-(phaserX(i) + scrn.width \ 4, phaserY(i) + dY), _IIf(dY, LightYellow, Red)
        Next dY
    Next i
End Sub

Sub moveGround (yIndex As Long)
    Dim As Long grassX, j, grassPlotY
    groundTops(yIndex) = ground.lastY
    ground.lastY = _Clamp(ground.lastY + ground.dY, 0, ground.maxY)
    ground.dY = _Clamp(ground.dY - 0.1 + 0.2 * Rnd, -.5, .5)
    If ground.lastY < 10 Then ground.dY = ground.dY + 0.005
    If ground.lastY > ground.maxY - 10 Then ground.dY = ground.dY - 0.005
    grassX = scrn.width - 1
    'scroll bottom half of screen left 1 pixel
    _PutImage (0, 0)-(scrn.width - 2, scrn.height \ 2 - 1), scrn.image, ground.panBufferImage, (1, 0)-(scrn.width - 1, scrn.height \ 2 - 1)
    _PutImage (0, 0)-(scrn.width - 2, scrn.height \ 2 - 1), ground.panBufferImage, scrn.image, (0, 0)-(scrn.width - 2, scrn.height \ 2 - 1)
    grassPlotY = groundTops((grassX + yIndex) Mod scrn.width)
    Dim pColor As Long
    If grassPlotY > 0 Then
        pColor = DarkGreen '_RGB(10, 128, 25)
    Else 'bottom of screen - draw water (blue)
        pColor = Blue
    End If
    'PSet (grassx, grassplotY), pColor 'this draws a horizon line but does not fill in
    Line (grassX, scrn.height \ 2 - 1)-(grassX, grassPlotY - 5), DarkBlue 'erase remnants of prior rightmost pixel column
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
    _PutImage , scrn.image, ship.backimage, (ship.x - ship.width \ 2, ship.y + ship.height \ 2)-(ship.x - ship.width \ 2 + ship.width - 1, ship.y + ship.height \ 2 - (ship.height - 1))
    _PutImage (ship.x - ship.width \ 2, ship.y + ship.height \ 2)-(ship.x - ship.width \ 2 + ship.width - 1, ship.y + ship.height \ 2 - (ship.height - 1)), ship.sourceImage, scrn.image, (1, 1)-(ship.width, ship.height)
End Sub

Sub hideShip
    _PutImage (ship.x - ship.width \ 2, ship.y + ship.height \ 2)-(ship.x - ship.width \ 2 + ship.width - 1, ship.y + ship.height \ 2 - (ship.height - 1)), ship.backimage, scrn.image
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
        h = (_Axis(1) + _Axis(6)) / 20 'update ship movement rate
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
        '    Print "Button "; i; "="; _Button(i); " ";
        'Next i
        'Print spc(20)
        'For i = 1 To nAxes
        '    Print "Axis "; i; "="; _Axis(i); " ";
        'Next i
        'Print spc(20)

        '    Print Spc(40);
        '    Locate 2, 1
        '    For j = 1 To nButtons
        '        If (_Button(j) <> 0) Then
        '            Print j; "="; _Button(j); "  "; '-1=button pressed?
        '        End If
        '    Next j

        If phasers.count < 10 Then
            If ((_ButtonChange(6) = -1) And (_Button(6) = -1)) Or ((_ButtonChange(1) = -1) And (_Button(1) = -1)) Then ' Or _ButtonChange(2) = -1 Then
                phasers.count = phasers.count + 1
                'Locate 5, 1: Print "Phaser bolts = "; phasers.count
                phaserX(phasers.count) = ship.x + ship.width \ 2 - 5
                phaserY(phasers.count) = ship.y - 12 'looks better coming from edge of saucer than vertical center
            End If
        End If

    End If
    ship.dx = _Clamp(ship.dx + h, -5, 5) 'max speed 5 pixels/frame
    ship.dy = _Clamp(ship.dy - v, -5, 5)
    ship.x = _Clamp(ship.x + ship.dx, ship.width \ 2 + 1, scrn.width - ship.width \ 2 - 1)
    ship.y = _Clamp(ship.y + ship.dy, 0 * scrn.height \ 2 + ship.height \ 2 + 1, scrn.height - ship.height \ 2 - 1)
    ship.dx = ship.dx * 0.97 'friction from the atmosphere/ether slows the ship down
    ship.dy = ship.dy * 0.97
End Sub
