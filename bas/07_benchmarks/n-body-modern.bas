'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' n-body, SedaiBasic MODERN dialect.
'' Ported from the Python version (Kevin Carson / Tupteq / Fredrik Johansson / Daniel Nanz /
'' Maciej Fijalkowski).
''
'' SEQUENTIAL, like the Python original: n-body advances one timestep at a time and every step
'' depends on the one before it, so there is nothing to spread across workers. Python does not
'' parallelise it either - same weapons.

Const NB = 5                      '' sun, jupiter, saturn, uranus, neptune
Const PI = 3.14159265358979323
Const SOLAR_MASS = 4 * PI * PI
Const DAYS_PER_YEAR = 365.24

Dim As Double x(0 To NB-1), y(0 To NB-1), z(0 To NB-1)
Dim As Double vx(0 To NB-1), vy(0 To NB-1), vz(0 To NB-1)
Dim As Double mass(0 To NB-1)

'' sun
x(0) = 0 : y(0) = 0 : z(0) = 0
vx(0) = 0 : vy(0) = 0 : vz(0) = 0
mass(0) = SOLAR_MASS
'' jupiter
x(1) =  4.84143144246472090
y(1) = -1.16032004402742839
z(1) = -0.103622044471123109
vx(1) =  1.66007664274403694e-03 * DAYS_PER_YEAR
vy(1) =  7.69901118419740425e-03 * DAYS_PER_YEAR
vz(1) = -6.90460016972063023e-05 * DAYS_PER_YEAR
mass(1) = 9.54791938424326609e-04 * SOLAR_MASS
'' saturn
x(2) =  8.34336671824457987
y(2) =  4.12479856412430479
z(2) = -0.403523417114321381
vx(2) = -2.76742510726862411e-03 * DAYS_PER_YEAR
vy(2) =  4.99852801234917238e-03 * DAYS_PER_YEAR
vz(2) =  2.30417297573763929e-05 * DAYS_PER_YEAR
mass(2) = 2.85885980666130812e-04 * SOLAR_MASS
'' uranus
x(3) =  12.8943695621391310
y(3) = -15.1111514016986312
z(3) = -0.223307578892655734
vx(3) =  2.96460137564761618e-03 * DAYS_PER_YEAR
vy(3) =  2.37847173959480950e-03 * DAYS_PER_YEAR
vz(3) = -2.96589568540237556e-05 * DAYS_PER_YEAR
mass(3) = 4.36624404335156298e-05 * SOLAR_MASS
'' neptune
x(4) =  15.3796971148509165
y(4) = -25.9193146099879641
z(4) =  0.179258772950371181
vx(4) =  2.68067772490389322e-03 * DAYS_PER_YEAR
vy(4) =  1.62824170038242295e-03 * DAYS_PER_YEAR
vz(4) = -9.51592254519715870e-05 * DAYS_PER_YEAR
mass(4) = 5.15138902046611451e-05 * SOLAR_MASS

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 1000
If Len(Command(1)) > 0 Then N = CInt(Command(1))

'' offset_momentum: the sun absorbs the total momentum so the system's centre of mass stays put
Dim As Double px = 0, py = 0, pz = 0
For i As Integer = 0 To NB-1
  px -= vx(i) * mass(i)
  py -= vy(i) * mass(i)
  pz -= vz(i) * mass(i)
Next i
vx(0) = px / SOLAR_MASS
vy(0) = py / SOLAR_MASS
vz(0) = pz / SOLAR_MASS

Sub reportEnergy( m() As Double, xx() As Double, yy() As Double, zz() As Double, _
                  vvx() As Double, vvy() As Double, vvz() As Double )
  Dim As Double e = 0
  For i As Integer = 0 To NB-1
    e += 0.5 * m(i) * (vvx(i)*vvx(i) + vvy(i)*vvy(i) + vvz(i)*vvz(i))
    For j As Integer = i+1 To NB-1
      Dim As Double dx = xx(i) - xx(j)
      Dim As Double dy = yy(i) - yy(j)
      Dim As Double dz = zz(i) - zz(j)
      e -= (m(i) * m(j)) / Sqr(dx*dx + dy*dy + dz*dz)
    Next j
  Next i
  Print Using "##.#########"; e
End Sub

reportEnergy( mass(), x(), y(), z(), vx(), vy(), vz() )

Const DT = 0.01
For tick As Integer = 1 To N
  For i As Integer = 0 To NB-1
    For j As Integer = i+1 To NB-1
      Dim As Double dx = x(i) - x(j)
      Dim As Double dy = y(i) - y(j)
      Dim As Double dz = z(i) - z(j)
      Dim As Double d2 = dx*dx + dy*dy + dz*dz
      Dim As Double mag = DT / (d2 * Sqr(d2))
      Dim As Double m1 = mass(i) * mag
      Dim As Double m2 = mass(j) * mag
      vx(i) -= dx * m2 : vy(i) -= dy * m2 : vz(i) -= dz * m2
      vx(j) += dx * m1 : vy(j) += dy * m1 : vz(j) += dz * m1
    Next j
  Next i
  For i As Integer = 0 To NB-1
    x(i) += DT * vx(i)
    y(i) += DT * vy(i)
    z(i) += DT * vz(i)
  Next i
Next tick

reportEnergy( mass(), x(), y(), z(), vx(), vy(), vz() )
