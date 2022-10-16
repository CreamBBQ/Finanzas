function [gos,x] = cpoces2(x,W0,rf,ret,gamma)
% Solución de las CPO de una función CES

what = (W0-sum(x))*(1+rf) + (1+ret)*x;

U1 = what.^(-gamma);

gos = zeros(22,1);

for i=1:22;
    gos(i,1) = mean(U1.*(ret(:,i)-rf));
    i=i+1;
end
end

