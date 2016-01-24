c = linspace (0, 0.25, 250);
for a = 1:250
    c(a) = round(c(a)*1000)/1000;
end    
p = linspace (0, 1, 100);
for b = 1:100
    p(b) = round(p(b)*100)/100;
end 
K = 10^9; % carrying capacity
r = 0.1; % growth rate 
[x,y] = meshgrid(c, p);
old=digits(20);
x = vpa(x);
y = vpa(y);
m = x .* y;
digits(old);
n = 1 ./(r-m-r .* m);
z =  zeros(size(n));
z =  double(z);
for i = 1:numel(n)
    if n(i) > 0  
       z(i) = log10(n(i))-10; 
    elseif n(i) ==0
       print('error');
    elseif n(i) < 0 
       z(i) = -12;
    end
    S=sprintf('z[%d]= %d\n', i, z(i));
    disp(S);
end  


%for s = 1:250
%    for t = 1:250
        %if n(s, t) > 0
%            z(s, t) = n(s, t);
        %elseif n(s, t) <0 
        %    z(s, t) = -10;
        %else
        %    fprintf('error');
        %end
%    end
%end    



%z = log(r)-log(K)-log(r- m - r .* m);
%z = sym(z); 
%z(91, 100) = r / K * (r - m(91, 100) - r * m(91, 100));
%z(100, 91) = r / K * (r - m(91, 100) - r * m(91, 100));
%z(92, 99) = r / K * (r - m(92, 99) - r * m(92, 99));
%z(99, 92) = r / K * (r - m(92, 99) - r * m(92, 99));
isnumeric(z);
imagesc(c,p,z);
colormap(heat);
colorbar;
title('N = function(c, p)');
xlabel('c');
ylabel('p');
zlabel('N');
   
   