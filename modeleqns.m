%% Euler Solver
function [t,data] = euler1storder(y,dt,t_final)
time = 0;
Nsteps = round(t_final/dt); %% number of steps to take
t = zeros(Nsteps,1); %% initialize space for return array
data = zeros(Nsteps,1); %% initialize space for return array
for i =1:Nsteps
    dy = derivs(time,y); %% compute the derivatives
    y = y + dy*dt; %% extrapolate one time step
    time = time+dt; %% increment time
    t(i) = time; %% store data for return
    data(i) = y;
end

%% derivative functions
function dy = derivs(time,y)
dy =-y; %% govering equation
 