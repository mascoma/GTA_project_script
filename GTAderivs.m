function dy = GTA(time, y)
dy = zeros(2,1);
X1 = 1; %% population X+ 
X2 = 2; %% population X-

r = 0.1; %% growth rate

K = $i; %% carry capacity
c = ck(1); %% cost

N = 1.1e-8; %% N = b*i*ro*mu b: burst size; i: infection; ro: integration; mu: freq of large head
dy(X1) = r*(1-c)*(1-(y(X1)+y(X2))/K)*y(X1) - c*y(X1) + c*N*y(X1)*y(X2);
dy(X2) = r*(1-(y(X1)+y(X2))/K)*y(X2) - c*N*y(X1)*y(X2);
