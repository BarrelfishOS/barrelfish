% eclipse -e "lib(test_util), [objects2], test(objecttests)."

get_object(_, [val(a,0)], [], X) should_give X=object(o0, [val(a,0)]).
