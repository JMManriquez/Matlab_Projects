% clc, clear

format LONGG
%%Ch 3 Formulas

PV = @(FV, i, n) FV./((1+i./100).^n);
%Calculates the Present Value from a Future Value

FV = @(PV, i, n) PV.*((1+i./100).^n);
%Calculates the Future Value from a present value

EIR = @(r, m) (((1 + r./(100*m)).^m) - 1).*100;
%EIR is calculating effective interest rate
%r = nominal annual interest rate
%m = number of compounding periods per years

Im = @(EIR, m) (((1 + EIR./100).^(1./m)) - 1).*100;
%Find compound interest given Effective Interest Rate

%%Ch4 Formulas

%Uniform Series Compound Amount Factor (FV)
USFA = @(A, i, n) A.*(((1+(i./100)).^n - 1)./(i./100));

%Uniform Series Sinking Fund Factor (FV)
USAF = @(F, i, n) F.*((i./100)./((1+(i./100)).^n - 1));

%Uniform Series Capital Recovery Factor (PV)
USAP = @(P, i, n) P.*((i./100).*(1+(i./100)).^n)/((1+(i./100)).^n - 1);

%Uniform Series Present Worth Factor (PV)
USPA = @(A, i, n) A.*(((1+(i./100)).^n)-1)./((i./100).*(1+(i./100)).^n);

%Solve for n value, given P, A, and i

USNPA = @(P, A, i) log(1/(1 - P.*(i./100)./A))./log(1 + (i./100));

%Solve for Present Value/Worth given Gradiant, i, n
PG = @(G, i, n) G.*(1 - (1 + n.*(i./100)).*(1 + (i./100)).^(-n))./((i./100).^2);

%Solve for Uniform Series Given Gradient, i, n
AG = @(G, i, n) G.*((1 + (i./100)).^n - (1 + n.*(i./100)))./((i./100).*((1 + (i./100)).^n - 1));

%Solve for Future Value/Worth given Gradiant, i, n
FG = @(G, i, n) G.*((1 + (i./100)).^n - (1 + n.*(i./100)))./((i./100).^2);

%Solve for Present Worth/Value for Uniform Series A, g (geometric gradient), n, i
PAgG = @(A, g, i, n) A.*(1 - ((1 + (g./100)).^n).*(1 + (i./100)).^(-n))./((i./100) - (g./100));


i = [0:0.00001:100];
zeroed = zeros(1, 100e5);
irr = zeros(1, 100e5);
count = 0;
compare = 0;
tolerance = 0.1;
for j = 1:10e6
    zeroed(j) = -550000 + PV(117189, i(j), 1) + PV(150119.60, i(j), 2) + PV(144708.16, i(j), 3) + PV(147212.50, i(j), 4) + PV(161590.50, i(j), 5) + PV(167063.25, i(j), 6);
    if(zeroed(j) <= (compare+tolerance) && zeroed(j) >= (compare-tolerance))
        count = count + 1;
        fprintf('The i values that are within %.5f and %.5f are %f\n', compare+tolerance, compare-tolerance, i(j))
        irr(count) = i(j);
        if(count > 1)
            iavg = sum(irr)/(count)
        end
    end
end

% i = [0:0.1:100];
% A = zeros(1, 1000);
% B = zeros(1, 1000);
% C = zeros(1, 1000);
% D = zeros(1, 1000);
% 
% for j = 1:1001
%     A(j) = -8000 + USPA(1750, i(j), 10);
%     B(j) = -6000 + USPA(1300, i(j), 10);
%     C(j) = -6000 + USPA(1425, i(j), 10);
%     D(j) = -9500 + USPA(1900, i(j), 10);
%     if(A(j) > B(j) && A(j) > C(j) && A(j) >  D(j) && A(j) >= 0)
%         fprintf('At %3.2f A is best\n', i(j))
%     elseif(B(j) > A(j) && B(j) > C(j) && B(j) >  D(j) && B(j) >= 0)
%         fprintf('At %3.2f B is best\n', i(j))
%     elseif(C(j) > A(j) && C(j) > B(j) && C(j) >  D(j) && C(j) >= 0)
%         fprintf('At %3.2f C is best\n', i(j))
%     elseif(D(j) > A(j) && D(j) > B(j) && D(j) >  C(j) && D(j) >= 0)
%         fprintf('At %3.2f D is best\n', i(j))
%     else
%         fprintf('At %3.2f Do Nothing is best\n', i(j));
%     end
% end
% plot(i, A)
% hold
% plot(i, B)
% plot(i, C)
% plot(i, D)
% title('A vs. B vs. C vs. D');
% xlabel('Interest Rate (i)');
% ylabel('A, B, C, D ($)');
% legend;

% n = [0:0.00001:100];
% zeroed = zeros(1, 800e3);
% nfind = zeros(1, 800e3);
% compare = 140e3;
% tolerance = 0.1;
% count = 0;
% for j = 1:100e5
%     zeroed(j) = 100E3 + PV(120e3, 8, n(j));
%     if(zeroed(j) <= (compare+tolerance) && zeroed(j) >= (compare-tolerance))
%         count = count + 1;
%         fprintf('The n values that are within %.6f and %.6f are %.6f\n', compare+tolerance, compare-tolerance, n(j))
%         nfind(count) = n(j);
%         if(count > 1)
%             navg = sum(nfind)/(count)
%         end
%     end
% end
