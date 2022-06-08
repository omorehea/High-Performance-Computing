%----------- AM250 HW 6 -------------
%------- Author: Owen Morehead ------
%-------- Date: May 25, 2022 --------

close all; clear all; clc

%-- Plot communication latency time vs. length of real valued (double) message --

%times = importdata('latency_times_and_L_1.dat');
times_more = importdata('latency_times_and_L_moreL.dat');
times_less = importdata('latency_times_and_L_lessL.dat');


Ls = 0:100000;

%-- linear regression on data -> T_msg = t_startup + t_cost/word(L) --

coefs_less = polyfit(times_less(:,1),times_less(:,2),1); %best fit y-int and slope values

fit_less = polyval(coefs_less,Ls); %evaluate along L domain

%-------

coefs_more = polyfit(times_more(50:end,1),times_more(50:end,2),1);

fit_more = polyval(coefs_more,Ls);

%--------------------------------------------------------------------------
figure(1)

plot(times_more(1:end,1),times_more(1:end,2),'b','linewidth',1.3); grid on; hold on; %plot data 
plot(Ls,fit_more,'r--','linewidth',1.2) %best fit

plot(times_less(1:end,1),times_less(1:end,2),'b','linewidth',1.3,'handlevisibility','off'); grid on; hold on; %plot data 
plot(Ls,fit_less,'r--','linewidth',1.2,'handlevisibility','off') %plot best fit for the less data 

xlabel('Message Length','fontsize',18,'interpreter','latex');
ylabel('Message Time (sec)','fontsize',18,'interpreter','latex');
xlim([0,100000]);
legend('Data','Linear Best Fit','fontsize',18,'interpreter','latex');
