-type id() :: binary(). % 32 alphanumeric characters, where the first 4 characters indicate the type
-type id_type() :: (formula | challenge | response). % possible id types

-type now() :: {integer(), integer(), integer()}.

% an image of a formula
-record(formula, {
	  id :: id(), % random alphanumeric id (tag is form) eg form0123456789abcdefeeeeeeeeeeee
	  source :: binary(), % where the formula originates eg <<"springer">>
	  source_id :: binary(), % the id by which the formula is known to the source eg <<"doi=10.1007/s11276-008-0131-4,eqnId=14">>
	  url :: binary(), % url at which the formula image can be found eg <<"/images/f0040123456789abcdefeeeeeeeeeeee">>
	  latex :: binary() % either 'none' or the formula's latex source code eg <<"$$ 1+1=2 $$">>
	 }).
-type formula() :: #formula{}.

% a challenge submitted to a user
-record(challenge, {
	  id :: id(), % random alphanumeric id (tag is chal) eg chal0123456789abcdefeeeeeeeeeeee
	  generated :: now(), % time when the challenge was generated eg {1303,806421,919499}
	  source :: term(), % where the challenge is to be presented eg 'latexpert' or {'retex', <<"http://example.com/">>}
	  formulas :: list(id())% a list of formula ids for which the user should provide latex source code eg [f0040123456789abcdefeeeeeeeeeeee]
	 }).
-type challenge() :: #challenge{}.

% a users response to a challenge
-record(response, {
	  id :: id(), % random alphanumeric id (tag is resp) eg resp0123456789abcdefeeeeeeeeeeee
	  generated :: now(), % time when the challenge was generated eg {1303,806421,919499}
	  submitted :: now(), % time when the response was submitted eg {1303,806802,868890}
	  source :: term(), % where the challenge was presented eg 'latexpert' or {'retex', <<"http://example.com/">>}
	  user_id :: binary(), % user id provided by the source eg <<"jamie@scattered-thoughts.net">>
	  formulas :: list(id()), % a list of formula ids for which the user provided latex source code eg [f0040123456789abcdefeeeeeeeeeeee]
	  latexs :: list(binary()), % the list of latex strings provided by the user eg [<<"$$ 1+1=2 $$">>]
	  hashes :: list(binary()) % list of hashes of the latex strings as produced by latex:hash
	 }).
-type response() :: #response{}.
