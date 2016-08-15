cp = linspace (0, 1, 100);

for a = 1:100
   cp(a) = round(cp(a)*100)/100;
end 

r = linspace (0, 1, 100); % growth rate 
for b = 1:100
    r(b) = round(r(b)*100)/100;
end 
[x,y] = meshgrid(cp, r);

K = 10^5; % carrying capacity
 
x = vpa(x); % cp
y = vpa(y); % r
m = x ./ y; 
 
n = 1 - m - x
 
z =  zeros(size(n));
z =  double(z);
for i = 1:numel(n)
    if n(i) > 0  
       z(i) = -log10(K) - log10(n(i)); 
    elseif n(i) < 0 
       z(i) = -8;
    end
    S=sprintf('z[%d]= %d\n', i, z(i));
    disp(S);
end 

isnumeric(z);
imagesc(cp, r,z);
colormap(jet);
colorbar;
title('N = function(cp, r)');
xlabel('cp');
ylabel('r');
zlabel('LogN');
   
   