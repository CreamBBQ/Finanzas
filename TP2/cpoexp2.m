function [gos,x] = cpoexp2(x,W0,rf,ret,delta)
% Solución de las CPO de una función exponcial

what = (W0-sum(x))*(1+rf) + (1+ret)*x;

U1 = delta*exp((-delta)*(what)); 
%Nota: ese +100 adentro de la exponencial es una transformacion lineal
%Por lo que no cambia nada. Esta puesto porque sino los numeros son muy
%chicos y entonces el algoritmo no converge, o converge mal. Con ese +100,
%reescalamos todo para que sea grande y el algoritmo converja bien

gos = zeros(22,1);

for i=1:22;
    gos(i,1) = mean(U1.*(ret(:,i)-rf));
    i=i+1;
end
end