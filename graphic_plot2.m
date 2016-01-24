c = 0.001;
  
p = 1;
 
K = linspace (10^4, 10^9, 10000); % carrying capacity 
r = 0.1; % growth rate 
for i = 1:numel(K)
N(i)= log10(r) - log10(r - c*p - c*r*p) - log10(K(i)); 
S=sprintf('z[%d]= %d\n', i, N(i));
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
plot(K, N, 'LineWidth',5, 'Color',[0,0.7,0.9]);
title('N = function(K)');
xlabel('K');
ylabel('N');
   
   