%% Euler Solver
%% Place this code in a file called eulersolver.m
function [t,data] = eulersolver(y,dt,t_final,derivs_Handle)
time = 0;
Nsteps = round(t_final/dt);
t = zeros(Nsteps,1); %%initialize data array
data = zeros(Nsteps,1); %%initialize data array
t(1) = time; %% store intial condition
data(1,:) = y;
for i =1:Nsteps
    dy = feval(derivs_Handle,time,y);
    y = y + dy*dt;
    time = time+dt;
    t(i+1) = time;
    data(i+1) = y;
end