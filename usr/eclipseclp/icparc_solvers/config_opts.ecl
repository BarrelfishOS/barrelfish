%
% Module for managing configuration options for other modules.
%
% $Id: config_opts.ecl,v 1.2 2009/07/16 09:11:25 jschimpf Exp $
%

%
% Copyright (C) 2004  The SBDS Group
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%

%
% TODO
%
% - XXX Should we insist that notification predicates not fail?  Or not
%   guarantee that they are actually called (e.g. if another one fails
%   first)?  Or should we guarantee that they are all called (modulo errors)
%   but remember whether any failed so that we can fail at the end if
%   necessary?  Or just make it explicitly undefined what happens if such a
%   call fails?
%
% - XXX How should a library/application/etc. document the options that it
%   supports?
%
% - Allow de-registration of options?
%
% - Allow removal of notification predicates?
%
% - Allow multiple occurrences of a set of options for a module through the
%   use of handles.
%
% - Support "protected" options?  i.e. only the modules that registered the
%   options are allowed to change their values.
%
% - Support re-compilation of option-dependent code when the option changes,
%   for efficiency.  (Intended for code that is littered with tests of
%   option values - e.g. when logging/debugging is turned off, we would like
%   to avoid having too much overhead skipping the logging/debugging
%   statements.)
%   XXX - note that this appears to clash with the use of handles: different
%   handles will want the code compiled in different ways; probably some
%   kind of dynamic dispatch will be required.
%

:- module(config_opts).

%
% Interface:
%
%   register_option	Module defines an option
%   set_option		Change the value of an option
%   get_option		Retrieve the value of an option
%   get_all_options	Retrieve the current values of all options
%   request_notification Ask to be notified when a given option changes
%

:- comment(categories, ["Data Structures","Programming Utilities"]).
:- comment(summary, "Library for managing configuration options for libraries and applications").
:- comment(desc, html("\
   This library provides a centralised mechanism for managing configuration
   options for other libraries and applications.  Such options range from
   turning logging on and off and specifying what statistics to collect and
   print, to which redundant constraints and search strategies to use.<P>

   A software component registers the fact that it supports an option by
   calling register_option/3.  An option's value can then be set and
   retrieved through set_option/2 and get_option/2, respectively.  And any
   party wishing to be notified when an option's value changes can achieve
   this by calling request_notification/2.<P>

   The values and associated parameters of configuration options are
   persistent across backtracking.  Values are copied when set and
   retrieved, which means that any non-ground values will contain fresh
   copies of variables when retrieved rather than the original ones.<P>

   Note that at the moment configuration option settings are global, which
   makes them unsuitable for some purposes (e.g. any library that might be
   used by more than one component of a software system if those components
   may wish to use different settings for that library).  We hope to address
   this issue in a future version of this library, probably by allowing the
   use of handles to manage multiple sets of options.<P>

   This library is still experimental and is expected to evolve.  Feedback,
   suggestions, problem reports, etc. welcome.<P>
")).
:- comment(author, "Warwick Harvey").
:- comment(status, "evolving").


:- export
	register_option/3,
	set_option/2,
	get_option/2,
	get_all_options/1,
	request_notification/2.

:- tool(register_option/3, register_option_body/4).
:- tool(request_notification/2, request_notification_body/3).


:- local store(option_store).

    % Information to store for each option.
:- local struct(option(
	    name,	% Redundant?
	    value,	% Current value
	    validate,	% Value validation predicate
	    notify,	% Change notification predicate
	    module	% Defining module
	)).


    % -----------------
    % register_option/3
    % -----------------

:- comment(register_option/3, [
	summary: "Register an option",
	args: [
	    "Name": "Name of the option (any ground term)",
	    "InitialValue": "Initial value to be given to option",
	    "Options": "List of options related to this option"
	],
	amode: register_option(++, ?, +),
	see_also: [set_option/2, get_option/2, get_all_options/1, request_notification/2],
	desc: html("\
   This predicate sets up an option with name Name and initial value
   InitialValue.  Name must be ground; InitialValue can be any term but
   should normally be ground, since it will be copied when stored and
   retrieved, which means that any retrieved non-ground value will contain
   fresh copies of variables rather than the original ones.<P>

   Options must be a list.  The following terms may appear in this list:
   <DL>
   <DT>validation_pred(Pred/Arity)</DT>
       <DD>Pred is used to validate any value to be assigned to the option.
       This can be used to ensure that only legal values are assigned to the
       option.  Arity must be either 1 or 2, corresponding to the arity of
       the predicate to be called: if Arity is 1 then Pred(NewValue) is
       called, if Arity is 2 then Pred(Name, NewValue) is called; Name is
       the name of the option and NewValue is the value to be validated.  If
       the call to Pred succeeds then the value is assigned to the option;
       if it fails the value is not assigned.  Any choice points left by the
       call to Pred are pruned upon success.<P>

       Only one validation predicate may be defined for any option; if more
       are specified then each replaces the one before it.</DD>

   <DT>notify_pred(Pred)</DT>
       <DD>Specifies that Pred(Name, OldValue, NewValue) should be called
       whenever the value of the option Name is changed.  OldValue is the
       old value of the option that has just been replaced and NewValue is
       the new value that has just been assigned.  Pred should not modify
       OldValue or NewValue in any way, as this may interfere with other
       notification predicates.  Note that any choice points left by the
       call to Pred are pruned upon success.<P>

       See also request_notification/2.</DD>
   </DL>")
    ]).

register_option_body(Name, InitialValue, Options, Module) :-
	( nonground(Name) ->
	    printf(error, "Option name must be ground in %q.%n",
		    [register_option(Name, InitialValue, Options)]),
	    abort
	;
	    ( store_get(option_store, Name, Option0) ->
		Option0 = option with [module:Module0],
		( Module0 = Module ->
		    % Ignore re-definition by same module: probably just a
		    % re-compilation of the module, and we want to keep old
		    % user-set value.
		    true
		;
		    printf(error, "Option %q already defined by module %q:%ncannot also define it in module %q.%n",
			    [Name, Module0, Module]),
		    abort
		)
	    ;
		Option0 = option with [
			name:Name,
			value:InitialValue,
			validate:(dummy_validate/1@config_opts),
			notify:[],
			module:Module
		    ],
		process_options(Options, Module, Option0, Option),
		validate_option_value(Option, Name, InitialValue),
		store_set(option_store, Name, Option)
	    )
	).

process_options([], _Module, Option0, Option) ?-
	!,
	Option = Option0.
process_options([Opt | Options], Module, Option0, Option) ?-
	!,
	process_option(Opt, Module, Option0, Option1),
	process_options(Options, Module, Option1, Option).
process_options(Options, _Module, _Option0, _Option) :-
	printf(error, "Options must be a list in %q.%n", [Options]),
	abort.

process_option(validation_pred(PredSpec), Module, Option0, Option) ?-
	!,
	( nonvar(PredSpec), PredSpec = Pred/Arity ->
	    ( ( Arity == 1 ; Arity == 2 ) ->
		( call(current_predicate(Pred/Arity))@Module ->
		    update_struct(option, [validate:(Pred/Arity@Module)],
			    Option0, Option)
		;
		    printf(error,
			    "Validation predicate %q/%q "
			    "not visible in module %q.%n",
			    [Pred, Arity, Module]),
		    abort
		)
	    ;
		printf(error,
			"Validation predicate must have arity 1 or 2 in %q.%n",
			[PredSpec]),
		abort
	    )
	;
	    printf(error,
		    "Validation predicate must be of form Pred/Arity in %q.%n",
		    [PredSpec]),
	    abort
	).
process_option(notify_pred(Pred), Module, Option0, Option) ?-
	!,
	( call(current_predicate(Pred/3))@Module ->
	    Option0 = option with [notify:NotifyList],
	    update_struct(option, [notify:[Pred@Module | NotifyList]],
		    Option0, Option)
	;
	    printf(error,
		    "Notification predicate %q/3 not visible in module %q.%n",
		    [Pred, Module]),
	    abort
	).
process_option(Opt, _Module, _Option0, _Option) :-
	printf(error, "Unrecognised option %q.%n", [Opt]),
	abort.


    % Default validation predicate - just succeed.
dummy_validate(_).


    % ------------
    % set_option/2
    % ------------

:- comment(set_option/2, [
	summary: "Gives a new value to an option",
	args: [
	    "Name": "Name of the option",
	    "Value": "Value to be given to the option"
	],
	amode: set_option(++, ?),
	see_also: [register_option/3, get_option/2, request_notification/2],
	desc: html("\
   This predicate gives a new value Value to the option with name Name.  The
   option must have already been registered with register_option/3.  If the
   option has an associated validation predicate (see register_option/3),
   then this is called before the option is given the new value, and if the
   validation predicate fails the value is not assigned and set_option/2
   fails.<P>

   If the new value is successfully set, then any notification predicates
   associated with the option (see request_notification/2 and
   register_option/3) are called.<P>

   The value set is persistent across backtracking.  Value is copied during
   setting, which means that if it is non-ground then it will contain fresh
   copies of variables rather than the original ones when retrieved later.
")
    ]).

set_option(Name, Value) :-
	( nonground(Name) ->
	    printf(error, "Option name must be ground in %q.%n",
		    [set_option(Name, Value)]),
	    abort
	; store_get(option_store, Name, Option0) ->
	    validate_option_value(Option0, Name, Value),
	    Option0 = option with [value:OldValue],
	    update_struct(option, [value:Value], Option0, Option),
	    store_set(option_store, Name, Option),
	    notify_option_change(Option, Name, OldValue, Value)
	;
	    printf(error, "Trying to set a non-existent option in %q.%n",
		    [set_option(Name, Value)]),
	    abort
	).

validate_option_value(Option, Name, Value) :-
	Option = option with [validate:(Pred/Arity@Module)],
	( Arity == 1 ->
	    Call =.. [Pred, Value]
	; % Arity == 2
	    Call =.. [Pred, Name, Value]
	),
	once(Call)@Module.

    % XXX - should we trap failures in notification predicates?
notify_option_change(Option, Name, OldValue, NewValue) :-
	Option = option with [notify:NotifyList],
	(
	    foreach(Pred@Module, NotifyList),
	    param(Name, OldValue, NewValue)
	do
	    Call =.. [Pred, Name, OldValue, NewValue],
	    once(Call)@Module
	).


    % ------------
    % get_option/2
    % ------------

:- comment(get_option/2, [
	summary: "Retrieves the current value of an option",
	args: [
	    "Name": "Name of the option",
	    "Value": "Value to be given to the option"
	],
	amode: get_option(++, ?),
	see_also: [register_option/3, set_option/2],
	desc: html("\
   This predicate retrieves the current value Value of the option with name
   Name.  The option must have already been registered with
   register_option/3.<P>

   Value is copied during retrieval, which means that if it is non-ground
   then it will contain fresh copies of variables rather than the original
   ones (or sharing with other retrieved copies).
")
    ]).

get_option(Name, Value) :-
	( nonground(Name) ->
	    printf(error, "Option name must be ground in %q.%n",
		    [get_option(Name, Value)]),
	    abort
	; store_get(option_store, Name, Option0) ->
	    Option0 = option with [value:Value]
	;
	    printf(error, "Trying to get a non-existent option in %q.%n",
		    [get_option(Name, Value)]),
	    abort
	).


    % -----------------
    % get_all_options/1
    % -----------------

:- comment(get_all_options/1, [
	summary: "Retrieves the current values of all options",
	args: [
	    "OptionList": "List of (Module:Name)-Value terms"
	],
	amode: get_all_options(?),
	see_also: [register_option/3, set_option/2, get_option/2],
	desc: html("\
   This predicate retrieves the current values of all options, returning a
   list of terms of the form (Module:Name)-Value, where Name is the name of
   the option, Module is the module that defined (registered) it, and Value
   is the current value of the option.<P>

   The values are copied during retrieval, which means that if they are
   non-ground then they will contain fresh copies of variables rather than
   the original ones (or sharing with other retrieved copies).
")
    ]).

get_all_options(List) :-
	stored_keys_and_values(option_store, KeysOptions),
	(
	    foreach(Name - Option, KeysOptions),
	    foreach((Module:Name) - Value, List)
	do
	    Option = option with [value:Value, module:Module]
	).


    % ----------------------
    % request_notification/2
    % ----------------------

:- comment(request_notification/2, [
	summary: "Requests that a predicate be called when an option changes",
	args: [
	    "Name": "Name of the option",
	    "Pred": "Predicate to call"
	],
	amode: request_notification(++, ++),
	see_also: [register_option/3, set_option/2],
	desc: html("\
   Specifies that Pred(Name, OldValue, NewValue) should be called whenever
   the value of the option Name is changed.  OldValue is the old value of
   the option that has just been replaced and NewValue is the new value that
   has just been assigned.  Pred should not modify OldValue or NewValue in
   any way, as this may interfere with other notification predicates.  Note
   also that any choice points left by the call to Pred are pruned upon
   success.
")
    ]).

request_notification_body(Name, Pred, Module) :-
	( nonground(Name) ->
	    printf(error, "Option name must be ground in %q.%n",
		    [request_notification(Name, Pred)]),
	    abort
	; store_get(option_store, Name, Option0) ->
	    process_option(notify_pred(Pred), Module, Option0, Option),
	    store_set(option_store, Name, Option)
	;
	    printf(error, "Trying to request notification for a non-existent option in %q.%n",
		    [request_notification(Name, Pred)]),
	    abort
	).

