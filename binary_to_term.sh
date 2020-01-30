#!/usr/bin/env sh

TMPFILE=`mktemp`

cat - > $TMPFILE

erl -noinput -eval "\
	{ok, Bin} = file:read_file(\"$TMPFILE\"), \
	try \
		io:format(\"~p~n\", [binary_to_term(Bin)]), \
		halt(0) \
	catch _:_ -> \
		halt(1) \
	end."

RES=$?

rm $TMPFILE

exit $RES
