sets
s Markets /1*70/
i(s) market candidate /1*25/
o(s) Competitors /26*70/
h(o) Alias de O /26*70/
j customer node    /1*271/
t set of days /1*7/
tWeekend(t) /6,7/
tWeek(t)    /1*5/   
;
Option decimals = 4
Scalar
betaP Weight factor price  /-2.0/
betaD Weight factor distance /-0.5/
F Maximum Street Markets /10/
;

Parameter
W(i) Fuzzy AHP Weight for candidates
/
1        0.045
2        0.047
3        0.050
4        0.052
5        0.058
6        0.053
7        0.048
8        0.044
9        0.041
10        0.041
11        0.038
12        0.034
13        0.035
14        0.039
15        0.028
16        0.043
17        0.040
18        0.030
19        0.040
20        0.034
21        0.030
22        0.036
23        0.030
24        0.042
25        0.023
/
;

Parameter
Prc(s) Price Location S
/
1        0.080962801
2        0.080962801
3        0.080962801
4        0.080962801
5        0.080962801
6        0.080962801
7        0.080962801
8        0.080962801
9        0.080962801
10        0.080962801
11        0.080962801
12        0.080962801
13        0.080962801
14        0.080962801
15        0.080962801
16        0.080962801
17        0.080962801
18        0.080962801
19        0.080962801
20        0.080962801
21        0.080962801
22        0.080962801
23        0.080962801
24        0.080962801
25        0.080962801
26        0.399343545
27        0.057986871
28        0.424507659
29        0.297592998
30        0.520787746
31        0.464989059
32        0.345733042
33        0.062363239
34        0.355579869
35        0.214442013
36        0.426695842
37        0.190371991
38        0.319474836
39        0.668490153
40        0.164113786
41        0.221006565
42        0.097374179
43        0.519693654
44        0.386214442
45        0.263676149
46        0.254923414
47        0.452954048
48        1
49        1
50        0.671772429
51        0.671772429
52        0.671772429
53        0.452954048
54        0.671772429
55        1
56        0.671772429
57        0.671772429
58        0.452954048
59        0.671772429
60        0.671772429
61        0.452954048
62        0.452954048
63        0.452954048
64        0.671772429
65        1
66        0.671772429
67        0.671772429
68        0.671772429
69        0.671772429
70        3
/
;

Parameter
DistanciaA(s,j) Distance from s to j   ;
$call gdxxrw.exe Datos_Bosa.xlsx par=DistanciaA rng=Distancia_grb!A1:JL71 dim=2 Cdim=1 Rdim=1
$gdxin Datos_Bosa.gdx
$load DistanciaA
$gdxin
;

scalar
min_val /0/,
max_val /0/;

min_val = smin((s,j), DistanciaA(s,j));
max_val = smax((s,j), DistanciaA(s,j));

Parameter
Dis(s,j) Distance Normalized  ;
Dis(s,j) = (DistanciaA(s,j) - min_val)/(max_val-min_val)

*Parameter
*Dis(s,j) Distance Normalized - Distance from s to j   ;
*$call gdxxrw.exe distancia.xlsx par=Dis rng=distancia!A1:SG297 dim=2 Cdim=1 Rdim=1
*$gdxin distancia.gdx
*$load Dis
*$gdxin
;
scalar
min_valD /0/,
max_valD /0/;

min_valD = smin((s,j), Dis(s,j));
max_valD = smax((s,j), Dis(s,j));

Parameter
V(s,j) Utility;
V(s,j)= betaD * Dis(s,j)+betaP * Prc(s);


Parameter
A(s,j) One if Dis is less or equal than 0.5 km;
A(s,j) = DistanciaA(s,j) <= 0.75;

Parameter
PHI(i,j) Auxiliary Parameter;
PHI(i,j)= ((exp(V(i,j))*A(i,j))/(sum(o,exp(V(o,j))*A(o,j))));


Parameter
D(j,t) Demand constant ;
$call gdxxrw.exe Datos_Bosa_P.xlsx par=D rng=D_grb_P5050!A1:H272 dim=2 Cdim=1 Rdim=1
$gdxin Datos_Bosa_P.gdx
$load D
$gdxin
;

free variable
Z
*AUX
;
Positive Variable
Y(i,j,t) The probability of household j of selecting street market i at period t
Yb(j,t)  Accumulates the household probability of selecting sellers at period t and using the auxiliary parameter PHI(ij)
Binary Variable
X(i,t)   1 if the location of street market i is selected in period t - 0 otherwise

;

Equations
Fo         The objective of function aims to maximize the amount of fresh food demanded by households that is satisfied by the street markets considering the qualitative factors that were found with the Fuzzy AHP method.
*R1(i,t)    R1 places a cap on the quantity of F&Vs that can be provided to customers based on the capacity of the street markets.
R2(i,t)    To maintain a balanced distribution constraints R2 and R3 ensure that no street market can operate for three consecutive days and prevent the same street market from opening on the last day of one week and the first day of the following week
R3(i,t)    Para no abrir domingo y lunes
R4         R4 sets an upper limit on the number of street markets that can operate during the week. Finally

R6(j,t)    R6 computes the Yb(jt) as the accumulated probability of households opting for sellers other than the street markets.
R7(i,j,t)  R7 and R8 establish connections between the decision variables and an auxiliary parameter known as PHI(ij)
R8(i,j,t)
*R9(t)      R9 ensures that at least one market is open on each weekday guaranteeing daily access to market services for the community.
G9(i,t)
R10(t)   "On each weekend day, no more than 30% of F can open"
R11      "During weekdays, at least 40% of F"
R12(t)   "Cada día entre semana: máximo 20% de F"
;

Fo..             Z=E=sum((i,j,t),W(i)*D(j,t)*Y(i,j,t));
*R1(i,t)..        sum(j,D(j,t)*Y(i,j,t))=L=Q(i)*X(i,t);
R2(i,t)$(card(t)-2)..        X(i,t)+X(i,t+1)+X(i,t+2)=L=1;

R3(i,t)..        X(i,"1")+X(i,"2")+X(i,"7")=L=1;
R4..             sum((i,t),X(i,t))=E=F;
R6(j,t)..        Yb(j,t)+sum(i,Y(i,j,t))=L=1;
R7(i,j,t)..      Y(i,j,t)-(PHI(i,j)/(1+PHI(i,j)))*X(i,t)=L=0;
R8(i,j,t)..      Y(i,j,t)-PHI(i,j)*Yb(j,t)=L=0;
*R9(t)$(ord(t)<=5)..          sum(i,X(i,t)) =G= 0.1*F;
G9(i,t)..        X(i,"1")+X(i,"2")+X(i,"3")=E=0;
R10(t)$(tWeekend(t))..   sum(i, X(i,t)) =L= 0.3*F;
R11..                    sum((i,t)$(tWeek(t)), X(i,t)) =G= 0.4*F;
R12(t)$(tWeek(t))..      sum(i, X(i,t)) =L= 0.2*F;

Model FLQC /all/;
OPTION OPTCR = 0.0, OPTCA = 0.0;
*Option OPTCR Removes the gap
Solve FLQC using MIP Max Z;
Display PHI,X.l,Y.l,Yb.l,V,A,Dis,DistanciaA,min_val,max_val,min_valD,max_valD;

parameter auxY(i,j,t);
auxY(i,j,t)=Y.l(i,j,t);

parameter AUX;
AUX = sum((i,j,t),D(j,t)*auxY(i,j,t));

parameter AUX2(i);
AUX2(i) = sum((j,t),D(j,t)*auxY(i,j,t));

parameter auxX(i,t);
auxX(i,t)=X.l(i,t);

parameter Ycom(o,j,t);
Ycom(o,j,t) = ((exp(V(o,j))*A(o,j))/(sum(h,exp(V(h,j))*A(h,j)) + sum(i,exp(V(i,j))*A(i,j)*auxX(i,t))));

parameter DemCapCom;
DemCapCom = sum((o,j,t),D(j,t)*Ycom(o,j,t));

parameter DemCapCom2(o);
DemCapCom2(o) = sum((j,t),D(j,t)*Ycom(o,j,t));

Display AUX,AUX2,Ycom,DemCapCom,DemCapCom2;

