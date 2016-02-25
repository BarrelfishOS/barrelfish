% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: scheduler.pl,v 1.1 2006/09/23 01:54:05 snovello Exp $
% ----------------------------------------------------------------------

% $Id: scheduler.pl,v 1.1 2006/09/23 01:54:05 snovello Exp $

:- module_interface(scheduler).

:- lib(fd).
:- lib(structures).
:- lib(forall).

:- export
    schedule/3,
    interval_activity/5,
    discrete_resource/3,
    alt_res_set/3,
    starts_after_end/2,
    set_capacity_max/4,
    close_resource/1,
    isc/2,
    requires/2, requires/3,
    set_not_possible/2,
    consumes/2, consumes/3.

:- op(750, xfx, isc).
:- op(700, xfx, starts_after_end).
:- op(700, xfx, set_not_possible).
:- op(700, xfx, consumes).
:- op(700, xfx, requires).
:- op(700, xfx, --->).


:- export format_scheduler/2.
:- define_macro(activity/4, format_scheduler/2, [write]).
:- define_macro(schedule/5, format_scheduler/2, [write]).

:- export tr_isc/2.
:- define_macro(isc/2, tr_isc/2, [goal]).

:- begin_module(scheduler).

:- import setarg/3 from sepia_kernel.

:- define_struct(activity(handle, start, end, duration)).
:- define_struct(schedule(handle, timemin, timemax, activities, resources)).

format_scheduler(activity(_H, Start, End, Duration), Start-Duration--->End).
format_scheduler(schedule with [timemin:Min, timemax:Max], schedule(Min..Max)).

:- external(c_schedule/3, c_schedule),
   external(c_interval_activity/5, c_interval_activity),
   external(c_discrete_resource/3, c_discrete_resource),
   external(c_schedule_add/3, c_schedule_add),
   external(c_alt_res_set/3, c_alt_res_set),
   external(c_schedule_set/2, c_schedule_set).

% Type checking
code_type(ilc('DiscreteResource'), resource(cap(discrete(_)))).
code_type(ilc('CapResource'), resource(cap(_))).
code_type(ilc('Resource'), resource(_)).
code_type(ilc('AltResSet'), alt_res_set).
code_type(ilc('AltResConstraint'), constraint(alr_res)).
code_type(ilc('ResourceConstraint'), constraint(resource)).

:- define_macro(ilc/1, code_type/2, []).



% Initialization of a schedule ---------------------------------------------
schedule(Min, Max, schedule(S, Min, Max, [], [])) :-
  integer(Min), integer(Max), !,
  c_schedule(Min, Max, S).
schedule(Min, Max, S) :-
  error(4, schedule(Min, Max, S)).

% Backtrackable statements---------------------------------------------------
schedule_add(C, Constraint) :-
  getval(ilog_handle, H),
  c_schedule_add(H, C, Constraint).

schedule_set(C) :-
  getval(ilog_handle, H),
  c_schedule_set(H, C).

% interval_activity(+S, +Start, +End, +Duration, -A)
interval_activity(Schedule, Start, End, Duration, Activity) :-
  Schedule = schedule(S, Min, Max, Activities, _Resources),
  [Start, End, Duration] :: Min..Max,
  dvar_domain(Start, StartI),
  dvar_domain(End, EndI),
  dvar_domain(Duration, DurationI),
  c_interval_activity(S, StartI, EndI, DurationI, A),
  Activity = activity(A, Start, End, Duration),
  setarg(activities of schedule, Schedule, [Activity | Activities]).


discrete_resource(Schedule, Capacity, Resource) :-
  integer(Capacity),
  Schedule = schedule with [handle:S, resources:Rs],
  c_discrete_resource(S, Capacity, R),
  Resource = resource(R, ilc('DiscreteResource')),
  setarg(resources of schedule, Schedule, [Resource | Rs]).

alt_res_set(schedule with handle:S, Resources, Resource) :-
  ( foreach(resource(R, Type), Resources), foreach(R, Rs)
  do
    check_resource_type(Type)
  ),
  c_alt_res_set(S, Rs, ARS),
  Resource = resource(ARS, ilc('AltResSet')).

check_resource_type(ilc('Resource')) :- -?-> true.

% Constraints ---------------------------------------------------------------
tr_isc(X isc Y, Goal) :-
  Y =.. [P | As], append(As, [X], Args), Goal =.. [P | Args].

A1  starts_after_end A2 :-
  starts_after_end(A1, A2, _).

starts_after_end(activity with handle:A1, activity with handle:A2, Constraint) :-
  !,
  schedule_add(starts_after_end(A1, A2, 0), Cstr),
  Constraint = constraint(Cstr, ilc('PrecedenceConstraint')).
starts_after_end(activity with handle:A1, (activity with handle:A2)+Delay, Constraint) :-
  integer(Delay),
  schedule_add(starts_after_end(A1, A2, Delay), Cstr),
  Constraint = constraint(Cstr, ilc('PrecedenceConstraint')).


set_capacity_max(resource(Handle, ilc('DiscreteResource')), TimeMin, TimeMax, Capacity) :-
  -?->
  integer(TimeMax), integer(TimeMax), integer(Capacity),
  schedule_set(set_capacity_max(Handle, TimeMin, TimeMax, Capacity)).


A1 consumes C :- consumes(A1, C, _).

consumes(activity with handle:A, Capacity of resource(Resource, ilc('CapResource')), Constraint) :-
  -?->
  integer(Capacity),
  !,
  schedule_add(consumes(A, Resource, Capacity, cap), Cstr),
  Constraint = constraint(Cstr, ilc('ResourceConstraint')).

consumes(activity with handle:A, Capacity of resource(Resource, ilc('AltResSet')), Constraint) :-
  -?->
  integer(Capacity),
  schedule_add(consumes(A, Resource, Capacity, ars), Cstr),
  Constraint = constraint(Cstr, ilc('AltResConstraint')).



A1 requires C :- requires(A1, C, _).

requires(activity with handle:A, Capacity of resource(Resource, ResourceType), Constraint) :-
  -?->
  integer(Capacity),
  !,
  do_requires(A, Resource, Capacity, Constraint, ResourceType).
requires(activity with handle:A, resource(Resource, ResourceType), Constraint) :-
  do_requires(A, Resource, 1, Constraint, ResourceType).

do_requires(A, Resource, Capacity, Constraint, ilc('CapResource')) :-
  -?->
  schedule_add(requires(A, Resource, Capacity, cap), Cstr),
  Constraint = constraint(Cstr, ilc('ResourceConstraint')).
do_requires(A, Resource, Capacity, Constraint, ilc('AltResSet')) :-
  -?->
  schedule_add(requires(A, Resource, Capacity, ars), Cstr),
  Constraint = constraint(Cstr, ilc('AltResConstraint')).

  

close_resource(resource(R, ilc('Resource'))) :-
  -?->
  schedule_set(close_resource(R)).

constraint(C, ilc('AltResConstraint')) set_not_possible resource(R, ilc('Resource')) :-
  -?->
  schedule_set(set_not_possible(C, R)).
