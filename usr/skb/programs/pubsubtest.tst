% eclipse -e "lib(test_util), [pubsub], test(pubsubtest)."

subscribed(object(msg, [hair::[val blonde]]), _, X) should_give X=me.
subscribed(object(msg, [hair::[val blabla], attr::[val bla] ]), _, X) should_give X=me.
subscribed(object(na,  [face::[val test]]), _, X) should_give X=me.

find_subscribers(object(name, [face::[ val red ], age::[ val 22 ]]), X) should_give X=subs.
find_subscribers(object(wrongname, [face::[ val red ], age::[ val 22 ]]), X) should_fail.
find_subscribers(object(wrongname, [face::[ val red ], age::[ val 9 ]]), X) should_fail.
find_subscribers(object(xx, [age::[ val 9 ]]), X) should_give X=subs2.
find_subscribers(object(xx, [age::[ val 13 ]]), X) should_give X=subs3.
find_subscribers(object(xx, [age::[ val 14 ], other::[val 'name'] ]), X) should_give X=subs4.


% Using constraints
%subscribed(object(_, [weight::20]), other).
%suspend: (Y #>= 10), subscribed(object(_,  [weight::Y]), X) should_give X=other.
%suspend: (Y #>= 10), subscribed(object(_, [weight::Y]), other).
%subscribed(object(_,  [weight::22]), X) should_give X=other.


%subscribed(object(msg, [hair::blonde]), [], me).
%subscribed(object(msg, [hair::_, attr::bla]), [], me).
%subscribed(object(_, [face::test]), [], me).

