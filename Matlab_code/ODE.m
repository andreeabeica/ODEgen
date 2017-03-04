function dydt= ODE(t, y, parameters)
m1 = parameters(1);
m2 = parameters(2);
fa0 = parameters(3);
fb0 = parameters(4);
sinkZf = parameters(5);
sinkCf = parameters(6);
ii = parameters(7);
sourceS0f = parameters(8);
k = parameters(9);
sourceXf = parameters(10);
hh = parameters(11);
sinkYf = parameters(12);
gg = parameters(13);
f1 = parameters(14);


A = y(1);
B = y(2);
S0 = y(3);
S1 = y(4);
C = y(5);
X = y(6);
Y = y(7);
Z = y(8);
E = y(9);
F = y(10);
G = y(11);
M1 = y(12);
M2 = y(13);


ff = 2*hh; 


dydt(size(y,1),1)= 0;
dydt(1)= -k*A  +fa0*S0  +fa0*S0  -f1*A*B ;
dydt(2)= +k*A  +fb0*S1  -f1*A*B ;
dydt(3)= +sourceS0f  -fa0*S0  -fa0*S0 ;
dydt(4)= -fb0*S1 ;
dydt(5)= +f1*A*B  -sinkCf*C ;
dydt(6)= +sourceXf ;
dydt(7)= -sinkYf*Y ;
dydt(8)= -5. * sinkZf*Z^5 ;
dydt(9)= -ff*E  -gg*E ;
dydt(10)=  + 0.4 * ff*E  -hh*F  +ii*G ;
dydt(11)= +gg*E  +hh*F  -ii*G ;
dydt(12)= -m1*M1  +m2*M2 ;
dydt(13)= +m1*M1  -m2*M2 ;
end