function [gos,x] = cpoquad2(x,W0,rf,ret,beta1)
% Solución de las CPO de una función cuadrática

what = (W0-sum(x))*(1+rf) + (1+ret)*x;

U1 = 1 - 2*beta1*what; 

gos = zeros(22,1);

for i=1:22;
    gos(i,1) = mean(U1.*(ret(:,i)-rf));
    i=i+1;
end

end

