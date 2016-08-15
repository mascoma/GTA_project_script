%% derivative function
%% place in file derivs.m
function dy = derivs(time,y)
dy =0.0001*(1-y/1e9)*y; %% govering equation