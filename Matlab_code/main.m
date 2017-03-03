%parameters 
 params = parameters ();
%define initial conditions 
 init = initial ();
%call solver routine
 t0 = 0; 
 tf = 1e5; 
 [t,y] = ode15s(@(t,y) ODE(t, y, parameters), [t0,tf], init);
%assign species names
 si = y(:, 1);
 a = y(:, 2);
 mr = y(:, 3);
 mt = y(:, 4);
 mm = y(:, 5);
 mq = y(:, 6);
 mA = y(:, 7);
 mI = y(:, 8);
 r = y(:, 9);
 cr = y(:, 10);
 ct = y(:, 11);
 cm = y(:, 12);
 cq = y(:, 13);
 cA = y(:, 14);
 cI = y(:, 15);
 et = y(:, 16);
 em = y(:, 17);
 q = y(:, 18);
 A = y(:, 19);
 I = y(:, 20);
