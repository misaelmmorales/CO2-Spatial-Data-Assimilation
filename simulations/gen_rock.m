function [rock] = gen_rock(G, logperm, realization)
%GEN_ROCK Summary of this function goes here
%   Detailed explanation goes here

permeability = logperm(:,realization);

poro = 10.^((permeability-7)/10);
permx = 10.^permeability*milli*darcy;
permy = permx;
permz = 0.1*permx;
perm = [permx, permy, permz];

rock = makeRock(G, perm, poro);

end

