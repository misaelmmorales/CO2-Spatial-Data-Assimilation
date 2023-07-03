function [W] = gen_wells(G, rock)
%GEN_WELLS Summary of this function goes here
%   Detailed explanation goes here

%R_inj = 0.1*sum(poreVolume(G,rock))/inj_time;
%R_inj = 500 * stb/day;
%R_inj = (1/num_wells) * 556.2 * 1000 * meter^3 / year; %1 MT/yr
R_inj = 0.5 * 556.2 * 1000 * meter^3 / year; %0.5 MT/yr


W = [];
W = verticalWell(W, G, rock, 32, 32, [], ...
                    'Type',          'rate', ...
                    'Val',           R_inj, ...
                    'InnerProduct', 'ip_tpf', ...
                    'Radius',        0.05, ...
                    'Comp_i',        [0 1], ...
                    'name',          'Injector');




end