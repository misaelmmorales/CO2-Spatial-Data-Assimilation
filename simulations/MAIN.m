clear;clc;close all
%% Main variables
set(0,'DefaultFigureWindowStyle','docked')
%set(0,'DefaultFigureWindowStyle','docked')

proj_dir = 'C:/Users/381792/Documents/Spatial-Data-Assimilation/simulations';
mdir = 'C:/Users/381792/Documents/mrst-2023a';
chdir(mdir);
startup;
chdir(proj_dir);

% Import MRST module
mrstModule add spe10 co2lab
mrstModule add ad-core ad-props ad-blackoil mrst-gui

% Define global variables
dims = 64;

%% Make Grid
nx=dims;       ny=dims;       nz=1;
dx=1200*meter; dy=1200*meter; dz=100*meter;

% Make cartesian grid
G = cartGrid([nx ny nz], [dx dy dz]);
G = computeGeometry(G);

%% Make Initial State
gravity on;  g = gravity;
rhow = 1000; % density of brine corresponding to 94 degrees C and 300 bar
%initState.pressure = rhow * g(3) * G.cells.centroids(:,3);
%initState.pressure = G.cells.centroids(:,3) * 240 * psia;
P0 = 4000*psia;
initState.pressure = repmat(P0, dims*dims, 1);
initState.s = repmat([1, 0], G.cells.num, 1);
initState.sGmax = initState.s(:,2);

%% Make Fluid
co2     = CO2props();             % load sampled tables of co2 fluid properties
p_ref   = 30 * mega * Pascal;     % choose reference pressure
t_ref   = 94 + 273.15;            % choose reference temperature, in Kelvin
rhoc    = co2.rho(p_ref, t_ref);  % co2 density at ref. press/temp
cf_co2  = co2.rhoDP(p_ref, t_ref) / rhoc; % co2 compressibility
cf_wat  = 0;                      % brine compressibility (zero)
cf_rock = 4.35e-5 / barsa;        % rock compressibility
muw     = 8e-4 * Pascal * second; % brine viscosity
muco2   = co2.mu(p_ref, t_ref) * Pascal * second; % co2 viscosity

% Use function 'initSimpleADIFluid' to make a simple fluid object
fluid = initSimpleADIFluid('phases', 'WG'           , ...
                           'mu'  , [muw, muco2]     , ...
                           'rho' , [rhow, rhoc]     , ...
                           'pRef', p_ref            , ...
                           'c'   , [cf_wat, cf_co2] , ...
                           'cR'  , cf_rock          , ...
                           'n'   , [2 2]);

% Modify relative permeability curves
srw = 0.27;
src = 0.20;
fluid.krW = @(s) fluid.krW(max((s-srw)./(1-srw), 0));
fluid.krG = @(s) fluid.krG(max((s-src)./(1-src), 0));

% Add capillary pressure
pe = 5 * kilo * Pascal;
pcWG = @(sw) pe * sw.^(-1/2);
fluid.pcWG = @(sg) pcWG(max((1-sg-srw)./(1-srw), 1e-5));

%% Make Boundary Conditions
bc = [];
vface_ind = (G.faces.normals(:,3) == 0);
bface_ind = (prod(G.faces.neighbors, 2) == 0);
bc_face_ix = find(vface_ind & bface_ind);
bc_cell_ix = sum(G.faces.neighbors(bc_face_ix, :), 2);
p_face_pressure = initState.pressure(bc_cell_ix);
bc = addBC(bc, bc_face_ix, 'pressure', p_face_pressure, 'sat', [1,0]);

%% Define Timesteps
timestep1  = rampupTimesteps(5*year, year/4, 10);
total_time = timestep1;
inj_time = sum(timestep1)/year;

%% Generate Models & Run Simulation
N = 5;
M = size(total_time,1);

logperm = readmatrix('perm_64_32168.csv');

for i=1:N
    rock                     = gen_rock(G, logperm, i);
    W                        = gen_wells(G, rock);
    [schedule, dT1]          = gen_schedule(W, bc, timestep1);
    [model, wellSol, states] = gen_simulation(G, rock, fluid, initState, schedule);

    %wname = sprintf('wells/wells%d', i);   parsave(wname, well_loc)
    %rname = sprintf('states/states%d', i); parsave(rname, states)
    rname = sprintf('states/states%d.mat', i); save(rname, 'states', '-v7');

    fprintf('Simulation %i done\n', i)
end

%% END