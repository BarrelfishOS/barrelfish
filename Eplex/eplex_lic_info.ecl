%
% This file tells lib(eplex) what optimizer version to expect
% on a particular machine. Add lines of the form:
%
%	licence(Hostname, Solver, Version, LicStr, LicNum).
% E.g.
%	licence('breeze',  xpress, '1326icp', default, 0). % OEM XPRESS-MP Version 13.26
%	licence('cow.ic.ac.uk',  cplex, '80', '', 0).	% CPLEX Version 8.0
%
% Hostname must match the result of get_flag(hostname,H),
% converted to an atom. On some machines, this is just a name,
% on others it is the complete internet domain name.
%
% Solver is one of: cplex, xpress, osi, gurobi.
%
% Version is a solver-specific atom, usually the concatenation
% of the major and minor version numbers.  For osi, it indicates
% the actual COIN solver used.
%
% The meaning of LicStr and LicNum depends on the optimizer:
%
% CPLEX:
%	LicStr:	environment settings for runtime licences, e.g.
%		'CPLEXLICENSE=/usr/local/cplexlic.ptr', otherwise ''
%	LicNum:	serial number for runtime licences
%
% XPRESS-MP:
%	LicStr:	atom default if OEM version used.
%               Otherwise: directory where the licence (.pwd) files are located
%			   (overrides value of XPRESS environment variable)
%	LicNum:	unused
%
% COIN:
%	LicStr:	unused, set to ''
%	LicNum:	unused
%
% GUROBI:
%	LicStr:	unused, set to ''
%	LicNum:	unused
%
% The order of entries below is important:  If a machine has multiple
% optimizers (or optimizer versions) installed, and no solver is
% explicitly requested (i.e. lib(eplex) is called rather than, for
% instance, lib(eplex_cplex)), then the first matching will get loaded.
% Hostname and Version may be left uninstantiated.
%

% Examples
licence('breeze.icparc.ic.ac.uk', cplex, '90', '', 0).
licence('morden.icparc.ic.ac.uk', xpress, '1427', default, 0).

% Insert your hosts here

% By default, use COIN/OR OSI solvers
licence(_, osi, clpcbc, '', 0).
licence(_, osi, symclp, '', 0).
licence(_, osi, glpk, '', 0).

% Defaults for other solvers
licence(_Default, gurobi, '50', '', 0).
licence(_Default, cplex, '121', '', 0).
licence(_Default, xpress, '2000', '/opt/xpressmp/bin', 0).
licence(_Default, xpress, '1427icp', default, 0).
licence(_Default, xpress, '1326icp', default, 0).
