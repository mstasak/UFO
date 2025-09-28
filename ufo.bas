'$Debug
'UFO v1.01
'1.0 initial cut - ship random movements above grass
'1.01 controller input to steer ship; allow it to fly over entire screen

'QB64PE 4.2.0 (4.0+ should work)
'simulate vehicle flying over rolling terrain

Option _Explicit
Dim Shared As Long scr, wid, hgt, shipImage, shipWid, shipHgt, shipBkgImg, groundHgt
Dim Shared As Single shipX, shipY, shipDX, shipDY
Dim Shared As Single y, plotY, dY
Dim Shared As Long yIndex, grassRect

Randomize Timer
scr = _NewImage(1280, 800, 32)
Screen scr
wid = _Width
hgt = _Height
ReDim Shared As Single ys(0 To wid - 1)
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

y = 0: dY = 0.5 * Rnd
grassRect = _NewImage(wid - 1, hgt \ 2, 32)

Do
    _Limit 60
    For yIndex = 0 To wid - 1
        hideShip
        moveShip
        moveGround
        showShip
        _Display
        Select Case InKey$
            Case _CHR_ESC:
                If _MessageBox("Confirm", "Quit now?", "YesNo", "Question") = 1 Then Exit Do
        End Select
        _Delay .005
    Next yIndex
Loop
Screen 0
_FreeImage grassRect: _FreeImage scr: _FreeImage shipImage: _FreeImage shipBkgImg
System

Sub moveGround
    Dim As Long x, j
    ys(yIndex) = y
    y = _Clamp(y + dY, 0, 0.5 * hgt)
    dY = _Clamp(dY - 0.1 * Rnd + 0.1 * Rnd, -.5, .5)
    If y < 10 Then dY = dY + 0.005
    If y > groundHgt - 10 Then dY = dY - 0.005
    x = wid - 1
    'scroll bottom half of screen left 1 pixel
    _PutImage (0, 0)-(wid - 2, hgt \ 2 - 1), scr, grassRect, (1, 0)-(wid - 1, hgt \ 2 - 1)
    _PutImage (0, 0)-(wid - 2, hgt \ 2 - 1), grassRect, scr, (0, 0)-(wid - 2, hgt \ 2 - 1)
    plotY = ys((x + yIndex) Mod wid)
    Dim pColor As Long
    If plotY > 0 Then
        pColor = DarkGreen '_RGB(10, 128, 25)
    Else 'bottom of screen - draw water (blue)
        pColor = Blue
    End If
    'PSet (x, plotY), pColor 'this draws a horizon line but does not fill in
    Line (x, hgt \ 2 - 1)-(x, plotY - 5), DarkBlue 'erase remnants of prior rightmost pixel column
    Line -(x, 0), DarkGreen 'erase remnants of prior rightmost pixel column
    'Line -(x, 0), pColor
    For j = 1 To plotY \ 3
        Dim bladey As Single
        'somewhat grass-like texture
        bladey = (plotY - 8) * Rnd
        Line (x - 5, bladey)-(x - 10 + 10 * Rnd, bladey + 8 - 12 * Rnd), _RGB(10, 64 + 128 * Rnd, 25)
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
    Dim devices As Long
    devices = _Devices 'MUST be read in order for other 2 device functions to work!
    'Dim i As Integer
    'Locate 1, 1
    'For i% = 1 To _Devices
    'Print Str$(i%) + ") " + _Device$(i%)
    'Print "Button:"; _LastButton(i%); ",Axis:"; _LastAxis(i%); ",Wheel:"; _LastWheel(i%)
    'Next i%

    Static As Single h, v 'last controller input
    If _DeviceInput = 3 Then
        h = _Axis(1) / 10
        v = _Axis(2) / 10
        If Abs(h) < 0.001 Then h = 0
        If Abs(v) < 0.001 Then v = 0
    End If
    shipDX = _Clamp(shipDX + h, -5, 5) 'max speed 5 pixels/frame
    shipDY = _Clamp(shipDY - v, -5, 5)
    shipX = _Clamp(shipX + shipDX, shipWid \ 2 + 1, wid - shipWid \ 2 - 1)
    shipY = _Clamp(shipY + shipDY, 0 * hgt \ 2 + shipHgt \ 2 + 1, hgt - shipHgt \ 2 - 1)
    shipDX = shipDX * 0.97 'friction from the ether slows the ship down
    shipDY = shipDY * 0.97
End Sub
