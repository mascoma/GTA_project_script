c = linspace (0, 0.25, 250);
for a = 1:250
    c(a) = round(c(a)*1000)/1000;
end   
  
p = 1;
 
K = 10^9; % carrying capacity 
r = 0.1; % growth rate 
b = linspace (1, 1000, 1000)
[x,y] = meshgrid(c, b);
n = zeros(size(x));
for i = 1:numel(x)
    if (r - x(i)*p - x(i)*r*p) >0
        n(i)= log10(r) - log10(r - x(i)*p - x(i)*r*p) - log10(K)-log10(y(i));
    elseif (r - x(i)*p - x(i)*r*p) ==0 || (r - x(i)*p - x(i)*r*p) <0
       n(i) = -15;
    end   
S=sprintf('n[%d]= %d\n', i, n(i));
disp(S);
end  


isnumeric(n);
imagesc(c,b,n);
colormap(heat);
colorbar;
title('n = function(c, b)');
xlabel('c');
ylabel('b');
zlabel('n');
   
   