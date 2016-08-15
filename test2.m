clear; %% clear exisiting workspace
options = odeset('AbsTol',1e-9);
[t, y] = ode45(@derivs, [0, 1e6], 1, options);
plot(t,y);
