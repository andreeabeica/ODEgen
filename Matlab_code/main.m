%parameters 
 params = parameters ();
%define initial conditions 
 init = initial ();
%call solver routine
 t0 = 0; 
 tf = 1e5; 
 [t,y] = ode15s(@(t,y) ODE(t, y, parameters), [t0,tf], init);
%assign species names
 A = y(:, 1);
 B = y(:, 2);
 S0 = y(:, 3);
 S1 = y(:, 4);
 C = y(:, 5);
 X = y(:, 6);
 Y = y(:, 7);
 Z = y(:, 8);
 E = y(:, 9);
 F = y(:, 10);
 G = y(:, 11);
 M1 = y(:, 12);
 M2 = y(:, 13);
