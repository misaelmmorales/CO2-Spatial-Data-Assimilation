function[schedule, dT1] = gen_schedule(W, bc, dT1)
%GEN_SCHEDULE Summary of this function goes here
%   Detailed explanation goes here

%
% Control Schedule (injection period only)
schedule.control = struct('W', W, 'bc', bc);
schedule.step.val = dT1;
schedule.step.control = ones(numel(dT1),1);
%

end

