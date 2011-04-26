% an image of a formula
-record(formula, {
	  id, % random alphanumeric id (tag is form) eg f0040123456789abcdefeeeeeeeeeeee
	  source, % where the formula originates eg <<"springer">>
	  source_id, % the id by which the formula is known to the source eg <<"doi=10.1007/s11276-008-0131-4,eqnId=14">>
	  url, % url at which the formula image can be found eg <<"/images/f0040123456789abcdefeeeeeeeeeeee">>
	  latex % either 'none' or the formula's latex source code eg <<"$$ 1+1=2 $$">>
	 }).

% a challenge submitted to a user
-record(challenge, {
	  id, % random alphanumeric id (tag is chal) eg c4110123456789abcdefeeeeeeeeeeee
	  generated, % now() when the challenge was generated eg {1303,806421,919499}
	  source, % where the challenge is to be presented eg 'latexpert' or {'retex', <<"http://example.com/">>}
	  formulae % a list of formula ids for which the user should provide latex source code eg [f0040123456789abcdefeeeeeeeeeeee]
	 }).

% a users response to a challenge
-record(response, {
	  id, % random alphanumeric id (tag is resp) eg 6e5b0123456789abcdefeeeeeeeeeeee
	  generated, % now() when the challenge was generated eg {1303,806421,919499}
	  submitted, % now() when the response was submitted eg {1303,806802,868890}
	  source, % where the challenge was presented eg 'latexpert'
	  user_id, % user id provided by the source eg <<"jamie@scattered-thoughts.net">>
	  formulae, % a list of formula ids for which the user provided latex source code eg [f0040123456789abcdefeeeeeeeeeeee]
	  latexs, % the list of latex strings provided by the user eg [<<"$$ 1+1=2 $$">>]
	  hashes % list of hashes of the latex strings as produced by latex:hash
	 }).
