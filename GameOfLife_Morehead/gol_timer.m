%-- Game Of Life Timer Program Results --
%-------- Author: Owen Morehead ---------
%--------- Date: June 5, 2022 -----------

close all; clear all; clc

gol_times = importdata('gol_colwise_times_and_N.dat');

Ns = 1:1000;

%analytical equation T_GOL
t_c = 1.55*10^(-8);

t_s = 7.94*10^(-6);

t_w = 1.70*10^(-9);

Proc = 4;

T_GOL = (t_c*Ns.*Ns)/Proc + 2*(t_s + t_w*(Ns));

figure()

plot(gol_times(:,1),gol_times(:,2), 'linewidth',2); grid on; hold on;
plot(Ns, T_GOL,'linewidth',2);

xlabel('Square Grid Dimension: $N$', 'fontsize',18,'interpreter','latex');
ylabel('Time (s)', 'fontsize',18,'interpreter','latex');
title('Game Of Life Program Runtimes' ,'fontsize',19,'interpreter','latex');

legend('Program Data','Model','fontsize',18,'interpreter','latex');

