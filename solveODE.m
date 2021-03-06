function solveODE(eqation_handler, outputdir)
options = odeset('AbsTol',1e-9);
X10 = 1;
X20 = 1;
[t, y] = ode45(eqation_handler, [0, 1000000], [X10, X20], options);
s = size(y);
dlmwrite(outputdir, y(s(1), 1:end));