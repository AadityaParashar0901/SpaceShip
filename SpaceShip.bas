'$Dynamic
'$Include:'QIMG.bi'
'$Include:'libVector.bi'
Type Ship
    As Vec2 Pos, Vel, Accel
    As Integer Health
    As Integer Bullets
    As Long Fuel
    As Single Angle
End Type
Type Planet
    As Vec2 Pos, Vel
    As Long Color
    As Double Mass
End Type

Randomize Timer

Dim Shared QIMG_SPRITES(0) As QIMG_Sprite

Dim Shared StarPos As Vec2, StarIMG&, StarColor As Long, StarMass As Double
VectorNewZero StarPos
StarColor = _RGB32(255, 191, 0)
StarIMG& = _NewImage(4000, 4000, 32)
_Dest StarIMG&
_Source StarIMG&
Circle (2000, 2000), 2000, StarColor
Paint (2000, 2000), StarColor
_Dest 0
_Source 0
StarMass = 100000000 '10 ^ 8

Const GridSize = 500
Const ShipMass = 1

Dim Shared Planets(1 To 10) As Planet
InitializePlanets

Dim Shared As Vec2 Camera
Dim Shared As Ship Ship

Const GravitationalConstant = 0.01

Const ShipMaxVelocity = 5000, ShipMaxAccel = 2500
Const ShipMaxVelocitySquared = ShipMaxVelocity * ShipMaxVelocity

Screen _NewImage(1920, 1080, 32)
Color -1, _RGB32(0, 0, 0, 127)
_FullScreen _SquarePixels

Dim Shared As Integer W, H, hW, hH
W = _Width
H = _Height
hW = W / 2
hH = H / 2
_MouseMove _Width / 2, _Height / 2
_MouseHide

InitializeShip
InitializeCamera

Const FPS = 240
Dim Shared GAMETICK As _Unsigned Long

Dim As Vec2 MouseThruster

Do
    Cls
    _Limit FPS
    If GAMETICK = 240 Then GAMETICK = 0
    LMX = _MouseX
    LMY = _MouseY
    LMW = _WindowHasFocus
    If _WindowHasFocus = 0 Then
        _Continue
    Else
        If LMW = 0 Then _MouseMove LMX, LMY
    End If
    If _MouseButton(1) Then
        If DMX = 0 Then DMX = _MouseX
        If DMY = 0 Then DMY = _MouseY
        VectorNew MouseThruster, _MouseX - DMX, _MouseY - DMY
        VectorMul MouseThruster, 10
        If VectorLength(MouseThruster) <= 640 Then
            DashedCircle DMX, DMY, 64, _RGB32(0, 191, 0)
            Line (DMX, DMY)-(_MouseX, _MouseY), _RGB32(0, 191, 0)
        ElseIf inRange(640, VectorLength(MouseThruster), 640 + ShipMaxAccel) Then
            Circle (DMX, DMY), VectorLength(MouseThruster) / 10, _RGB32(0, 191, 0)
            Line (DMX, DMY)-(_MouseX, _MouseY), _RGB32(0, 191, 0)
        Else
            Circle (DMX, DMY), 64 + ShipMaxAccel / 10, _RGB32(0, 191, 0)
            T = _Atan2(_MouseY - DMY, _MouseX - DMX)
            Line (DMX, DMY)-(DMX + (64 + ShipMaxAccel / 10) * Cos(T), DMY + (64 + ShipMaxAccel / 10) * Sin(T)), _RGB32(0, 191, 0)
        End If
        Ship.Angle = _Atan2(MouseThruster.Y, MouseThruster.X)
    Else
        DMX = 0
        DMY = 0
        VectorNewZero MouseThruster
    End If
    GAMETICK = GAMETICK + 1
    While _MouseInput: Wend
    Line (_MouseX - 9, _MouseY)-(_MouseX + 9, _MouseY), -1
    Line (_MouseX, _MouseY - 9)-(_MouseX, _MouseY + 9), -1
    OperatePhysics
    OperateShip Max(0, VectorLength(MouseThruster) - 640)
    OperateCamera
    If _KeyDown(32) Then VectorNewZero Ship.Vel
    DrawPlanets
    DrawStar
    DrawShip
    Print "Ship Position: "; Int(Ship.Pos.X); Int(Ship.Pos.Y)
    Print "Ship Velocity: "; VectorLength(Ship.Vel); "m/s"
    Print "Ship Fuel: "; Int(Ship.Fuel)
    Print "Camera Position: "; Int(Camera.X); Int(Camera.Y)
    For I = LBound(Planets) To UBound(Planets)
        Print "Planet "; I; ":"; Planets(I).Pos.X; Planets(I).Pos.Y
    Next I
    X = GridSize * (Camera.X \ GridSize)
    Y = GridSize * (Camera.Y \ GridSize)
    For I = 0 To Max(_Width / GridSize, _Height / GridSize) + 1
        T = I * GridSize
        Line (X - Camera.X - T, 0)-(X - Camera.X - T, _Height), _RGB32(127)
        Line (0, Y - Camera.Y - T)-(_Width, Y - Camera.Y - T), _RGB32(127)
        Line (X - Camera.X + T, 0)-(X - Camera.X + T, _Height), _RGB32(127)
        Line (0, Y - Camera.Y + T)-(_Width, Y - Camera.Y + T), _RGB32(127)
    Next I
    If _KeyDown(116) Then PI = (PI Mod UBound(Planets)) + 1: Ship.Pos = Planets(PI).Pos: Ship.Pos.X = Ship.Pos.X + 50
    _Display
Loop Until Inp(&H60) = 1
System

Sub InitializeCamera
    Camera.X = Ship.Pos.X
    Camera.Y = Ship.Pos.Y
End Sub
Sub InitializeShip
    Ship.Pos.X = 0: Ship.Pos.Y = -5000
    Ship.Vel.X = 0
    Ship.Vel.Y = -100
    Ship.Fuel = 100000
    Ship.Angle = -_Pi / 2
End Sub
Sub InitializePlanets
    For I = LBound(Planets) To UBound(Planets)
        Planets(I).Pos.X = (Rnd - 0.5) * 100000
        Planets(I).Pos.Y = (Rnd - 0.5) * 100000
        Planets(I).Mass = 10 ^ Int(Rnd * 3 + 2)
        Planets(I).Color = _RGB32(Rnd * 255, Rnd * 255, Rnd * 255)
    Next I
End Sub
Sub OperateShip (Velocity)
    Dim As Vec2 ShipVelocity, ShipVelocity_dTime
    If Abs(Velocity) > 0 Then
        Ship.Accel.X = Velocity * Cos(Ship.Angle) / FPS
        Ship.Accel.Y = Velocity * Sin(Ship.Angle) / FPS
        If VectorLength(Ship.Accel) > ShipMaxAccel Then VectorMul Ship.Accel, ShipMaxAccel / VectorLength(Ship.Accel)
        If GAMETICK = 240 Then Ship.Fuel = Ship.Fuel - 1
    Else
        Ship.Accel.X = 0
        Ship.Accel.Y = 0
    End If
    ShipVelocity = Ship.Vel
    VectorAdd ShipVelocity, Ship.Accel
    If VectorLength(ShipVelocity) <= ShipMaxVelocity Then Ship.Vel = ShipVelocity
    VectorNew ShipVelocity_dTime, Ship.Vel.X / FPS, Ship.Vel.Y / FPS
    VectorAdd Ship.Pos, ShipVelocity_dTime
    DrawThrust (Abs(Velocity) > 0)
End Sub
Sub DrawShip
    Static SHIP_QIMG
    If SHIP_QIMG = 0 Then SHIP_QIMG = QIMG_LoadSpriteFromFile&("res\ship.qimg", 240)
    QIMG_PutRotatedSprite SHIP_QIMG, 0, hW, hH, _R2D(Ship.Angle) + 90, 16
    Exit Sub
    Dim As Vec2 P1, P2, P3
    P1.X = 0: P1.Y = -8
    P2.X = -5: P2.Y = 8
    P3.X = 5: P3.Y = 8
    VectorRotate P1, _R2D(Ship.Angle) + 90
    VectorRotate P2, _R2D(Ship.Angle) + 90
    VectorRotate P3, _R2D(Ship.Angle) + 90
    CPX = Camera.X - Ship.Pos.X
    CPY = Camera.Y - Ship.Pos.Y
    Line (-CPX + P1.X + hW, -CPY + P1.Y + hH)-(-CPX + P2.X + hW, -CPY + P2.Y + hH), -1
    Line (-CPX + P1.X + hW, -CPY + P1.Y + hH)-(-CPX + P3.X + hW, -CPY + P3.Y + hH), -1
End Sub
Sub DrawThrust (accel) 'BETA
    Exit Sub
    Static Thrusts(1 To FPS) As Vec2
    Static ThrustsIntensity(1 To FPS) As _Byte
    Static ThrustI
    Dim As Vec2 RandomVector
    If accel Then
        ThrustI = ThrustI + 1
        Thrusts(ThrustI) = Ship.Pos
        ThrustsIntensity(ThrustI) = 255
        LastThrusterTick = 0
        If ThrustI = FPS Then ThrustI = 0
    End If
    For I = 1 To FPS
        If ThrustsIntensity(I) = 0 Then _Continue
        Circle (Thrusts(I).X - Camera.X + hW, Thrusts(I).Y - Camera.Y + hH), 2, _RGB32(255, 255, 0, ThrustsIntensity(I))
        ThrustsIntensity(I) = ThrustsIntensity(I) - 1
    Next I
End Sub
Sub DrawStar
    Static As Vec2 P1, P2
    Static __T As Integer
    If CircleTouchBox(StarPos.X, StarPos.Y, 2000, -hW, -hH, hW, hH) Then _PutImage (StarPos.X - Camera.X + hW - 2000, StarPos.Y - Camera.Y + hH - 2000), StarIMG&
    If CircleTouchBox(StarPos.X, StarPos.Y, 2025, -hW, -hH, hW, hH) = 0 Then Exit Sub
    __T = __T - (GAMETICK Mod 2 = 0)
    If __T = 2160 Then __T = 0
    For T = 0 To 360 Step _R2D(_Atan2(1, 2030))
        R = 2025 + 10 * Cos(60 * _D2R(T - __T / 2))
        VectorNew P1, R * Cos(_D2R(T)), R * Sin(_D2R(T))
        VectorNew P2, (R + 4) * Cos(_D2R(T)), (R + 4) * Sin(_D2R(T))
        Line (P1.X - Camera.X + hW, P1.Y - Camera.Y + hH)-(P2.X - Camera.X + hW, P2.Y - Camera.Y + hH), StarColor
    Next T
    For T = 0 To 360 Step _R2D(_Atan2(1, 2030))
        R = 2025 + 10 * Cos(60 * _D2R(T + __T / 2))
        VectorNew P1, R * Cos(_D2R(T)), R * Sin(_D2R(T))
        VectorNew P2, (R + 4) * Cos(_D2R(T)), (R + 4) * Sin(_D2R(T))
        Line (P1.X - Camera.X + hW, P1.Y - Camera.Y + hH)-(P2.X - Camera.X + hW, P2.Y - Camera.Y + hH), StarColor
    Next T
End Sub
Sub DrawPlanets
    For I = LBound(Planets) To UBound(Planets)
        DrawCircle Planets(I).Pos.X, Planets(I).Pos.Y, 100 * Log(Planets(I).Mass), Planets(I).Color
    Next I
End Sub
Sub OperateCamera
    Camera = Ship.Pos
End Sub
Sub OperatePhysics
    ApplyForce Ship.Vel, Ship.Pos, StarPos, ShipMass, StarMass
    For I = LBound(Planets) To UBound(Planets)
        ApplyForce Ship.Vel, Ship.Pos, Planets(I).Pos, ShipMass, Planets(I).Mass
        ApplyForce Planets(I).Vel, Planets(I).Pos, StarPos, Planets(I).Mass, StarMass
        For J = LBound(Planets) To UBound(Planets)
            If I <> J Then ApplyForce Planets(I).Vel, Planets(I).Pos, Planets(J).Pos, Planets(I).Mass, Planets(J).Mass
        Next J
        VectorRotate Planets(I).Vel, -90
        Planets(I).Pos.X = Planets(I).Pos.X + Planets(I).Vel.X / FPS
        Planets(I).Pos.Y = Planets(I).Pos.Y + Planets(I).Vel.Y / FPS
    Next I
End Sub
Sub ApplyForce (Vel As Vec2, P1 As Vec2, P2 As Vec2, M1, M2)
    F = CalculateForce(P1.X, P1.Y, M1, P2.X, P2.Y, M2)
    T = _Atan2(P2.Y - P1.Y, P2.X - P1.X)
    Vel.X = Vel.X + F * Cos(T)
    Vel.Y = Vel.Y + F * Sin(T)
End Sub
Function CalculateForce (X1, Y1, M1, X2, Y2, M2)
    CalculateForce = GravitationalConstant * M1 * M2 / DistanceSquared(X1, Y1, X2, Y2)
End Function
Sub DrawCircle (X, Y, R, C&)
    If CircleTouchBox(X - Camera.X, Y - Camera.Y, R, -hW, -hH, hW, hH) Then Circle (X - Camera.X + hW, Y - Camera.Y + hH), R, C&
End Sub
Sub DrawFilledCircle (X, Y, R, C&)
    If CircleTouchBox(X - Camera.X, Y - Camera.Y, R, -hW, -hH, hW, hH) = 0 Then Exit Sub
    For I = 0 To R
        Circle (X - Camera.X + hW, Y - Camera.Y + hH), I, C&
    Next I
End Sub
Sub DashedCircle (X, Y, R, C&)
    For I = 0 To 360
        If I Mod 36 > 18 Then _Continue
        T = _D2R(I)
        PSet (X + R * Cos(T), Y + R * Sin(T)), C&
    Next I
End Sub
Function inRange (A, B, C)
    If A <= B And B <= C Then inRange = -1
End Function
Function inBox (x, y, x1, y1, x2, y2)
    If inRange(x1, x, x2) And inRange(y1, y, y2) Then inBox = -1
End Function
Function CircleTouchBox (CX, CY, R, X1, Y1, X2, Y2)
    CircleTouchBox = inBox(CX, CY, X1 - R, Y1 - R, X2 + R, Y2 + R)
End Function
Function DistanceSquared (X1, Y1, X2, Y2)
    DistanceSquared = (X1 - X2) ^ 2 + (Y1 - Y2) ^ 2
End Function
Function Distance (X1, Y1, X2, Y2)
    Distance = Sqr(DistanceSquared(X1, Y1, X2, Y2))
End Function
Function Max! (A!, B!)
    If A! > B! Then Max! = A! Else Max! = B!
End Function
Function Min! (A!, B!)
    If A! > B! Then Min! = B! Else Min! = A!
End Function
'$Include:'libVector.bm'
'$Include:'QIMG.bm'
