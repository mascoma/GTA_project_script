t = linspace(0,2,20);
y = exp(-t);
plot(t,y);
hold on;
dydt = diff(y)./diff(t);
plot(t(1:end-1),dydt,'r--');
plot(t,-y);

clear; %% clear exisiting workspace
y = 1; %% initial condition
dt = 0.5; %% set the time step interval 
time = 0; %% set the start time=0
t_final = 2; %% end time of the simulation 
Nsteps = t_final/dt; %% number of time steps to take, integer
plot(time,y,'*'); %% plot initial conditions
hold on; %% accumulate contents of the figure
for i = 1:Nsteps %% number of time steps to take
    y = y - dt*y; %% Equation 1.9
    time = time + dt %% Increment time
    plot(time,y,'*'); %% Plot the current point
end
t = linspace(0,t_final,100); %% plot analytical solution
y = exp(-t);
plot(t,y,'r')
xlabel('time'); %% add plot labels
ylabel('y');

clear; %% clear exisiting workspace
dt = [0.0001 0.0005 0.001 0.005 0.01 0.05 0.1 ];
for j = 1:length(dt)
y = 1; %% initial condition
time = 0; %% set the time=0
t_final = 1.; %% final time
Nsteps = round(t_final/dt(j)); %% number of steps to take
for i = 1:Nsteps
y = y - dt(j)*y; %% extrapolate one time step
time = time + dt(j); %% increment time
end
X(j,2) = exp(-t_final) - y; %% compute the error and store
X(j,1) = dt(j); %% store the time step
end
loglog(X(:,1),X(:,2)) %% display on log-log plot
