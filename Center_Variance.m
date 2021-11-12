format longg
clc;

prompt = 'If you are working with Samples of Population, input (s), if not press any key: ';
str = input(prompt, 's');

if(str == 's')
        z = [ 21.4 40.4 16.4 73.8 36.6 109.8 30.0 4.4 33.1 66.7 81.5];

        temp = 0; 
        n = length(z) 

        %%Sort our Array of Numbers using Bubble Sort
        for k = 1:length(z) - 1
            for j = 1:length(z) - 1
                if(z(j) > z(j+1))
                    temp = z(j);
                    z(j) = z(j+1);
                    z(j+1) = temp;
                end
            end
        end

        disp(z')
        mean = (sum(z))./(length(z))

        if(mod(length(z),2) ~= 0) %%Check if String Length Odd`
            even = 0; %Prepare a flag for later code
            median = z(round(length(z)/2)) %Median Calculation
        else %If the length is even it falls here
            even = 1;
            median = (z(length(z)/2)+z(round((length(z)+1)/2)))/2
        end

        summation = 0;
        summands = zeros(1, 100);
        for i = 1:length(z)
            summands(i) = (z(i) - mean)^2;
            summation = summation + summands(i);
        end
        sampvariance = summation/(length(z) - 1) %Calculate Variance
        sampstddev = sqrt(sampvariance) %Calculate Standard Deviation

        for q = 1:(length(z)/2)
                a(q) = z(q); % Build the First Half of Number Array
                b(q) = z(q+(round(length(z)/2)));
                % Build Second Second Half of Array
        end

        %If the String length is odd, we must append the median to both halves
        if(~even) 
            a = [a median];
            b = [median b];
        end

        disp(a') %Transpose and display my data
        disp(b')

        %amedian is the 1st Quartile
        %bmedian is the 2nd Quartile
        %Check if the first half has Odd or Even Length
        if(mod(length(a),2) ~= 0) %Odd we simply divide and round
            amedian = a(round(length(a)/2)) %%Chooses right at center
        else
            amedian = (a(length(a)/2)+a(round((length(a)+1)/2)))/2
            %Takes the average between the two middle numbers
        end

        %The same previos code is done for the second half
        if(mod(length(b),2) ~= 0)
            bmedian = b(round(length(b)/2))
        else
            bmedian = (b(length(b)/2)+b(round((length(b)+1)/2)))/2
        end

        %Get Statistical Analysis information
        fourthspread = bmedian - amedian

        %Display the Ranges for Outliers, Helps to Visibly Debug Code
        %[Lower Bound, Upper Bound]
        mildoutlier = [amedian-1.5*fourthspread, bmedian+1.5*fourthspread]
        xtremeoutlier = [amedian-3*fourthspread, bmedian+3*fourthspread]

        %Declare the beginning of mild and extreme outliers array
        moutlier = [0];
        xoutlier = [0];

        %Prepare counters for each
        mcount = 0;
        xcount = 0;

        for c = 1:length(z)
            %Check if we have a Mild Lower Bound Outlier
            if(z(c) < mildoutlier(1) && z(c) > xtremeoutlier(1))
                mcount = mcount +1;
                %Build Number Array using mcount since arrays here start from 1
                moutlier(mcount) = z(c);
            %Check if we have Mild Upper Bound outlier   
            elseif(z(c) > mildoutlier(2) && z(c) < xtremeoutlier(2))
                mcount = mcount +1;
                moutlier(mcount) = z(c);
            %Check if we have Lower Bound Extreme Outlier
            elseif(z(c) < xtremeoutlier(1))
                xcount = xcount +1;
                xoutlier(xcount) = z(c);
            %Check if we have Upper Bound Extreme Outlier
            elseif(z(c) > xtremeoutlier(2))
                xcount = xcount +1;
                xoutlier(xcount) = z(c);
            end
        end

        %Display our results
        disp('Mild outliers in data: ')
        disp(moutlier')
        disp('Extreme outliers in data: ')
        disp(xoutlier')
        boxplot(z)
end

%Anonymous Function for Permutation Calculations
%%Permutations
perm = @(k, n) (factorial(n))./(factorial(n-k));

%%Population Mean Calculations:
PEx = @(x, px) sum(x.*px);
%It works and it can take arrays

%Population Variance Method 1
PVx = @(x, Ex, px) sum(((x-Ex).^2).*px);
%It works and it can take in arrays

%Population Standard Deviation
PStDv = @(x, Ex, px) sqrt(sum(((x-Ex).^2).*px));
%It works and it can take arrays

%Binomial Probability Distribution
Bpmf = @(n, x, p) (factorial(n)./(factorial(x).*factorial(n - x))).*(p.^x).*((1-p).^(n-x));
%Works and can take arrays

%Binomial Probability Distribution
%NOTE x IS AN ARRAY OF VALUES
Bcdf = @(n, x, p) sum((factorial(n)./(factorial(x).*factorial(n - x))).*(p.^x).*((1-p).^(n-x)));
%Works and can take arrays

%%Binomial Random Variable Population Calculations
%Expected Value, or Mean
BEx = @(n, p) n.*p;

%Variance
BVx = @(n, p) n.*p.*(1-p);

%Standard Deviation
BStDv = @(n, p) sqrt(n.*p.*(1-p));

%nchoosek equation that takes arrays
NCHOOSEK = @(n, k) (factorial(n)./(factorial(k).*(factorial(n-k))));

%HyperGeometric Distribution PMF (Probability Mass Function)
HGpmf = @(x, n, M, N) (NCHOOSEK(M, x).*NCHOOSEK(N-M, n-x))./(NCHOOSEK(N, n));

%HyperGeometric Distribution Expected Value
HGEx = @(n, M, N) n.*(M./N);

%HyperGemotric Distrubition Variance
HGVx = @(n, M, N) ((N-n)./(N-1)).*n.*(M./N).*(1-(M./N));

%HyperGemotric Distrubition Standard Deviation
HGStdDv = @(n, M, N) sqrt(HGVx(n, M, N));

%Negative Binomial Distribution pmf
NBpmf = @(x, r, p) (NCHOOSEK(x + r - 1, r -1).*(p.^r).*((1-p).^x));

%Negative Binomial Distribution Mean/Expected Value
NBEx = @(r, p) (r.*(1-p))./p;

%Negative Binomial Variance
NBVx = @(r, p) (r.*(1-p))./(p.^2);

%Negative Binomial Standard Deviation
NBStdDv = @(r, p) sqrt(NBVx(r, p));

%Geometric Distribution pmf
GEOpmf = @(x, p) p.*((1-p).^x);

%Geometric Distribution Expected Value/Mean
GEOEx = @(p) (1-p)./p;

%Geometric Distribution Variance
GEOVx = @(p) (1-p)./(p.^2);

%Geometric Distribution Standard Deviation
GEOStdDv = @(p) sqrt(GEOVx(p));

%Poisson Distribution Probability Mass Function
POISpmf = @(x, mu) (exp(-mu).*(mu.^x))./factorial(x);

%Poisson Distribution Expected Value/Mean
POISEx = @(mu) mu;

%Poisson Distribution Variance
POISVx = @(mu) mu;

%Poisson Distribution StandardDeviation
POISStdDv = @(mu) sqrt(POISVx(mu));

%%Continuous Random Variable Calculations

%Uniform Distribution Probability Density Function
UDpdf = @(A, B) (1./(B - A));

%Uniform Distribution cdf, ONLY FOR A <= x < B!!!!!!!
% x < A = 0 and x >= B = 1
UDcdf = @(x, A, B) (x-A)./(B-A);

%Uniform Distribution Expected Value
UDEx = @(A, B) (A + B)./2;

%Uniform Distribution Variance
UDVx = @(A, B) ((B - A).^2)./(12);

%Uniform Distribution Standard Deviation
UDStdDv = @(A, B) sqrt(UDVx(A ,B));

%Exponentially Distributed Prob/ Density Function
EXPpdf = @(lambda, x) lambda.*(exp(-lambda.*x));

%Exponentially Distributed Cumalitve Mass Function
%IF X < 0 = 0
EXPcdf = @(lambda, x) (1 - exp(-lambda.*x));

%Exponentially Distributed Expected Value
EXPEx = @(lambda) 1./lambda;

%Exponentially Distributed Variance
EXPVx = @(lambda) 1./(lambda.^2);

%Exponentially Distributed Standard Deviation
EXPStdDv = @(lambda) sqrt(EXPVx(lambda));

%Standard Normal Distribution Probability Density Function
STDNpdf = @(x) (1/(sqrt(2*pi))).*(exp(-0.5.*(x.^2)));

%Standard Normal Distribution Cumalitive Density Function
STDNcdf = @(b) integral(STDNpdf, -inf, b);

%Standard Normal Distribution Probability; Find z given STDNpdf(z)
%Includes Nonstandard Normal x value given a percentile or STDNpdf(z)
phi_z = left(1); %Make sure to change phi(z)
tolerance = 0.00001;
% mew = 30; %Make sure to change mean
% variance = 0.08^2; % Make sure to change variance
% stdDv = 5; % If given stdDv, uncomment this line and the 3rd inside the if statement
count = 0;
zval = zeros(1,100);
% % xval = zeros(1,100);
% for j = 1:3
j = 1;
    for i = 1:68000
        if(phi_zt00001(i) <= (phi_z(j) + tolerance) && phi_zt00001(i) >= (phi_z(j) - tolerance))
            count = count + 1;
            zval(count) = zt00001(i);
% %         xval(count) = stdDv*zval(count) + mew;
            fprintf('The z value for phi(z) = %.4f with tolerance %.6f is %.5f\n', phi_z(j), tolerance, zval(count));
% %         fprintf('Nonstandard x value for phi(z) is %.3f\n', xval(count));
        end
    end
    if(count > 1)
% %     xavg = sum(xval)/(count)
        zavg = sum(zval)/(count)
    end
% end

%Standard Normal Distribution Cumalitive Mass Function
STDNcdfab= @(a, b) integral(STDNpdf, a, b);

% Nonstandard Normal Distributions; NSTDN Probability Density Function
NSTDNpdf = @(x, mean, variance) (1/sqrt(2.*pi.*variance)).*(exp((-(x - mean).^2)./(2.*variance)));

%Nonstandard Normal Distribution; Cumulative Density Function
NSTDNcdf = @(a, mean, standardDeviation) STDNcdf((a - mean)./standardDeviation);

%Nontandard Normal Distribution Cumalitive Mass Function
NSTDNcdfab = @(a, b, mean, standardDeviation) NSTDNcdf(b, mean,standardDeviation) - NSTDNcdf(a, mean,standardDeviation); 

%Find CONFIDENCE for NORMAL Distribution KNOWN STANDARD DEVIATION
NCL = @(Xbar, z, s, n) Xbar + [-z z].*(s./sqrt(n));

%Find n for a given Confidence Level, interval width, sigma, and z value
%KNOWN
n_NCL = @(z, stdDv, width) ceil((2*[-z z].*(stdDv./width)).^2);

% % Construct Confidence Level WITH STD DV KNOWN POPULATION MEW unknown!! FOR SAMPLES
% % %NORMAL DISTRIBUTION
% %Standard Normal Distribution Probability; Find z given STDNpdf(z)
% % Includes Nonstandard Normal x value given a percentile or STDNpdf(z)
% phi_z = 1 - 0.05/2; %Make sure to change phi(z)
% tolerance = 0.00001;
% % mew = 2;
% % Xbar = 8486;
% stdDv = 0.6; 
% n = 25;
% count = 0;
% zval = zeros(1, 1e2);
% % xval = zeros(1, 1e2);
% % CL = zeros(1, 2);
% width = 0.2;
% for i = 1:68000
%     if(phi_zt00001(i) <= (phi_z + tolerance) && phi_zt00001(i) >= (phi_z - tolerance))
%         count = count + 1;
%         zval(count) = zt00001(i);
% %         xval(count) = stdDv*zval(count) + mew;
%         fprintf('\n\nThe z value for phi(z) = %.4f with tolerance %.6f is %.5f\n', phi_z, tolerance, zval(count));
% %         fprintf('Nonstandard x value for phi(z) = %.4f is %.3f\n',phi_z, xval(count));
%         if(count > 1)
% %             xavg = sum(xval)/(count);
%             zavg = sum(zval)/(count);
% %             CL(1) = Xbar - zavg*(stdDv/sqrt(n));
% %             CL(2) = Xbar + zavg*(stdDv/sqrt(n));
%         else
% %             CL(1) = Xbar - zval(1)*(stdDv/sqrt(n));
% %             CL(2) = Xbar + zval(1)*(stdDv/sqrt(n));
%         end
%     end
% end
% if(count > 1)
%    fprintf('\n\nThe avg z value for phi(z) = %.3f is %.5f\n',phi_z, zavg);
% %    fprintf('The avg x value for phi(z) = %.3f is %.5f\n',phi_z, xavg);
% %    fprintf('The confidence levels for the above are (%.4f , %.4f)\n', CL(1), CL(2));
%    n = n_NCL(zavg, stdDv, width)
% else
% %    fprintf('The confidence levels for the above z are (%.4f, %.4f)\n', CL(1), CL(2));
%    n = n_NCL(zval(1), stdDv, width)
% end

%Find p_hat for Samples Independent w/ Binomial Distribution 
p_hat = @(X, n) X./n;

%Find Confidence for Samples Independent w/ Binomial Distribution
CL_SBD = @(z, p_hat, n) p_hat + [-z z].*sqrt((p_hat.*(1 - p_hat))./n);

%Find n for CL s/t Samples Independent w/ Binomial Distribution 
% (WITH PRIOR GUESS)
n_BCLpg = @(z, pguess, width) ceil((2*[-z z].*(sqrt(pguess.*(1-pguess))./width)).^2);

%Find n for CL s/t Samples Independent w/ Binomial Distribution 
% (CONSERVATIVE ESTIMATE)
n_BCLce = @(z, width) ceil(([-z z]./width).^2);

% %Find confidence level for Sample Binomial Distribution
% %Standard Normal Distribution Probability; Find z given STDNpdf(z)
% phi_z = 1 - 0.01/2; %Make sure to change phi(z)
% % success = 167;
% sampleSize = 2344;
% phat = 0.57;
% tolerance1 = 0.000001;
% tolerance2 = 0.00001;
% count1 = 0;
% count2 = 0;
% zval1 = zeros(1,100);
% zval2 = zeros(1,100);
% CL1 = zeros(1, 2);
% CL2 = zeros(1, 2);
% for i = 1:68000
%     if(phi_zt00001(i) <= (phi_z + tolerance1) && phi_zt00001(i) >= (phi_z - tolerance1))
%         count1 = count1 + 1;
%         zval1(count1) = zt00001(i);
%         fprintf('\n\nThe z value for phi(z) = %.4f with tolerance %.6f is %.5f\n', phi_z, tolerance1, zval1(count1));
%         if(count1 > 1)
%             zavg1 = sum(zval1)/(count1);
%             CL1(1) = CL_SBD(-zavg1, phat, sampleSize);
%             CL1(2) = CL_SBD(zavg1, phat, sampleSize);
%         else
%             CL1(1) = CL_SBD(-zval1(1), phat, sampleSize);
%             CL1(2) = CL_SBD(zval1(1), phat, sampleSize);
%         end
%      elseif((i<6801) && (phi_zt0001(i)<=(phi_z + tolerance2)) && (phi_zt0001(i)>=(phi_z - tolerance2)))
%         count2 = count2 + 1;
%         zval2(count2) = zt0001(i);
%         fprintf('\n\nThe z value for phi(z) = %.4f with tolerance %.5f is %.5f\n', phi_z, tolerance2, zval2(count2));
%         if(count2 > 1)
%             zavg2 = sum(zval2)/(count2);
%             CL1(1) = CL_SBD(-zavg2, phat, sampleSize);
%             CL1(2) = CL_SBD(zavg2, phat, sampleSize);
%         else
%             CL2(1) = CL_SBD(-zval2(1), phat, sampleSize);
%             CL2(2) = CL_SBD(zval2(1), phat, sampleSize);
%         end    
%     end
% end
% if(count1 > 1)
%    fprintf('\n\nThe avg z value for phi(z) = %.3f with tolerance %.10f is %.5f\n',phi_z, tolerance1, zavg1);
% end
% if(count2 > 1)
%    fprintf('\n\nThe avg z value for phi(z) = %.3f with tolerance %.10f is %.5f\n',phi_z, tolerance2, zavg2);
% end
% fprintf('The confidence levels with tolerance %.6f for the above z are (%.5f, %.5f)\n\n', tolerance1, CL1(1), CL1(2));
% fprintf('The confidence levels with tolerance %.5f for the above z are (%.5f, %.5f)\n\n', tolerance2, CL2(1), CL2(2));

%t-Distribution Confidence Intervals with SAMPLE standard deviation and
%mean
tCL = @(xbar, t, s, n) xbar + [-t t].*(s./sqrt(n));

prompt = 'If you are working with Samples solving for Mean, Variance, and Standard Deviation, for t-dist Confidence Interval input (y), if not press any key: ';
str = input(prompt, 's');

if(str == 'y')
    
        z = [105.9 107.8 109.2 108.4 106.5 109.7 107.4];
        temp = 0; 
        n = length(z) 

        %%Sort our Array of Numbers using Bubble Sort
        for k = 1:length(z) - 1
            for j = 1:length(z) - 1
                if(z(j) > z(j+1))
                    temp = z(j);
                    z(j) = z(j+1);
                    z(j+1) = temp;
                end
            end
        end

        disp(z')
        mean = (sum(z))./(length(z))

        if(mod(length(z),2) ~= 0) %%Check if String Length Odd
            even = 0; %Prepare a flag for later code
            median = z(round(length(z)/2)) %Median Calculation
        else %If the length is even it falls here
            even = 1;
            median = (z(length(z)/2)+z(round((length(z)+1)/2)))/2
        end

        summation = 0;
        summands = zeros(1, 100);
        for i = 1:length(z)
            summands(i) = (z(i) - mean)^2;
            summation = summation + summands(i);
        end
        sampvariance = summation/(length(z) - 1) %Calculate Variance
        sampstddev = sqrt(sampvariance) %Calculate Standard Deviation
        t = tinv(1-0.05/2, 7-1)
        tconfidence = tCL(mean, t, sampstddev, length(z));
        fprintf('Confidence Interval for %.5f t and is (%.5f, %.5f)\n\n', t, tconfidence(1), tconfidence(2)) 
end


% %%%%%Listing All phi(z) Normal Values Here For a 0.00001 Tolerance%%%%%%%%%%%%%%%%%%%%%%%%
% zt00001 = [-3.4:0.0001:3.4];                   
% phi_zt00001 = zeros(1, 68000);
% for i = 1:68000
%     phi_zt00001(i) = STDNcdf(zt00001(i));
% end
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% %%%%%Listing All phi(z) Normal Values HereFor a 0.0001 Tolerance%%%%%%%%%%%%%%%%%%%%%%%%
% zt0001 = [-3.4:0.001:3.4];
% phi_zt0001 = zeros(1, 6800);
% for i = 1:6800
%     phi_zt0001(i) = STDNcdf(zt0001(i));
% end
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%Identifying z value for a given Phi(z) = Phi_Z%%%%%%%%%%%%%%%%%%%%
% phi_z = 1 - 0.01/2;
% tolerance1 = 0.000001;
% tolerance2 = 0.00001;
% count1 = 0;
% count2 = 0;
% zval1 = zeros(1, 100);
% zval2 = zeros(1, 100);
% for i = 1:68000
%     if(phi_zt00001(i) <= (phi_z + tolerance1) && phi_zt00001(i) >= (phi_z - tolerance1))
%         count1 = count1 + 1;
%         zval1(count1) = zt00001(i);
%         fprintf('The z value for phi(z) = %.4f with tolerance %.6f is %.5f\n', phi_z, tolerance1, zval1(count1));
%         if(count1 > 1)
%             zavg1 = sum(zval1)/(count1);
%         end
%     elseif((i<6801) && (phi_zt0001(i)<=(phi_z + tolerance2)) && (phi_zt0001(i)>=(phi_z - tolerance2)))
%         count2 = count2 + 1;
%         zval2(count2) = zt0001(i);
%         fprintf('The z value for phi(z) = %.4f with tolerance %.5f is %.5f\n', phi_z, tolerance2, zval2(count2));
%          if(count2 > 1)
%              zavg2 = sum(zval2)/(count2);
%          end
%      end
%             
% end
% 
% if(count1 > 1)
%    fprintf('\n\nThe avg z value for phi(z) = %.3f with tolerance %.10f is %.5f\n',phi_z, tolerance1, zavg1);
% end
% 
% if(count2 > 1)
%    fprintf('\n\nThe avg z value for phi(z) = %.3f with tolerance %.10f is %.5f\n',phi_z, tolerance2, zavg2);
% end
% %%%%%%%%%Identifying z value for a given Phi(z) = Phi_Z%%%%%%%%%%%%%%%%%%%%

%%Central Limit Theoreom for phat to find its STANDARD DEVIATION
phatStdDv = @(p, n) sqrt((p.*(1-p))./n);

%%Test Hypothesis Using Test Statistic for Proportions WITH KNOWN SIGMA
zptestS = @ (phat, p, phatstddv) (phat - p)./phatstddv;

%%Test Hypothesis Using test Statistic FOR NORMAL DISTRIBUTIONS WITH KNOWN SIGMA
ztestS = @(xbar, mew0, stddv, n) (xbar - mew0)./(stddv./sqrt(n));

%%Test Hypothesis Using Test Statistic for Proportion WITH UNKNOWN SIGMA
zptest = @(phat, p0, n) (phat - p0)./sqrt((p0.*(1-p0))./n);

%%Test Hypothesis Using Test Statistic for t-Normal WITH UNKNOWN SIGMA
ttest = @(xbar, mew0, s, n) (xbar - mew0)./(s./sqrt(n));

%%Test Hypothesis Using P-Value for t-Normal WITH UNKNOWN SIGMA T != t
tpval2 = @(tTest, df) 2*(1 - (tcdf(abs(tTest), df)));

%%Test Hypothesis Using P-Value for t-Normal WITH UNKNOWN SIGMA T < t
tpvalLeft = @(tTest, df) tcdf(tTest, df);

%%Test Hypothesis Using P-Value for t-Normal WITH UNKNOWN SIGMA T > t
tpvalRight = @(tTest, df) 1 - tcdf(tTest, df);

%%Find Variance of a Sample given Variance of the Distribution
VarianceBar = @(Variance, n) Variance./n;

%%Find Standard Deviation of a Sample given Std Dev of the distribution
StdDevBar = @(StdDv, n) sqrt(VarianceBar(StdDv.^2, n));