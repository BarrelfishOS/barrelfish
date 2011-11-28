% eclipse -e "lib(test_util), [objects], test(objecttests)."
% TODO tests for add, edit, del handler calls

% Test lookup
get_object(doesNotExist, X, [], _) should_fail.
get_object(men, [], [], X) should_give X=object(men, [ ako :: [val person], hair::[val black] ]).

% Single attribute lookup
get_object(men, [hair::X], [], _) should_give X=black.

% Single attribute lookup using inheritance
get_object(john, hair::X, [], _) should_give X=black.

% object lookup based on attribute value
get_object(X, age::28, [], _) should_give X=arnold. % and needs to check that X=anna
get_object(X, [age::28, hair::blonde], [], _) should_give X=anna.

% object lookup based on defined attributes
get_object(X, status::_, [], _) should_give X=anna.
get_object(X, does_not_exist::_, [], _) should_fail.


% Edit and update objects using add_object
add_object(bob, [])
should_give
get_object(bob, X, [], _), X=[].

add_object(bob, [age::12, ako::john]) 
should_give
get_object(bob, [age::X, ako::Y], [], _), X = 12, Y = john.

add_object(bob, age::13)
should_give
get_object(bob, [age::X, ako::Y], [], _), X = 13, Y = john.

add_object(bob, [tt::1]) 
should_give
get_object(bob, tt::X, [], Y), X = 1.


% Delete/remove objects
del_object(bob, tt::X) should_give (X=[1,2,3], get_object(bob,tt::_, [], _) should_fail).
del_object(men, [hair::X]) should_give X=black.

del_object(arnold, [weight::_, age::_]) 
should_give 
(get_object(arnold, weight::X, [], _) should_fail),
(get_object(arnold, age::X, [], _) should_fail).
