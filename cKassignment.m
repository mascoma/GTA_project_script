c = linspace (0, 1, 100);
for i = 1:100
   c(i) = round(c(i)*100)/100;
end 

K = linspace (1e5, 1e9, 100);
 
[x,y] = meshgrid(c, K);

fid1 = fopen('/Users/Xin/Desktop/projects/GTA_project/output/20160627/clist.txt', 'w');
fid2 = fopen('/Users/Xin/Desktop/projects/GTA_project/output/20160627/Klist.txt', 'w');
s = size(x);
for a = 1:s(1)
    for b = 1:s(2)
        fprintf(fid1, '%4.8f\n', x(a, b));
        fprintf(fid2, '%d\n', y(a, b));
    end
end
 