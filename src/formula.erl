-module(formula).

-include("types.hrl").
-include("util.hrl").
-include("log.hrl").

-export([start/0, by_id/1, new/4, new/5, random/0, to_json/1, from_image/1, from_images/1]).

-spec start() -> ok.
start() ->
    ok = 
	db:ensure_table(
	  formula,
	  [ 
	    {attributes, record_info(fields, formula)},
	    {disc_copies, [node()]} 
	  ]
	 ),
    % this table maps autoincrementing ids to formula ids, so we can pick formulas at random
    ok = autoinc:start(formula_all).

-spec by_id(id()) -> {ok, #formula{}} | {error, not_found}.
by_id(Id) ->
    case ?TRANS(mnesia:read({formula, Id})) of
	{atomic, []} -> {error, not_found};
	{atomic, [Formula]} -> {ok, Formula}
    end.

-spec new(binary(), binary(), binary(), binary()) -> #formula{}.
new(Source, Source_id, Url, Latex) ->
    Id = id:new(formula),
    new(Id, Source, Source_id, Url, Latex).

-spec new(id(), binary(), binary(), binary(), binary()) -> #formula{}.
new(Id, Source, Source_id, Url, Latex) ->
    ?INFO([formula, new, ?VAR(Id), ?VAR(Source), ?VAR(Source_id), ?VAR(Url), ?VAR(Latex)]),
    Formula = #formula{id=Id, source=Source, source_id=Source_id, url=Url, latex=Latex},
    {atomic, _} = 
	% use sync_transaction to slow writes, otherwise large imports overload mnesia
	mnesia:sync_transaction(
	  fun () ->
		  ok = mnesia:write(Formula),
		  autoinc:put(formula_all, Id)
	  end,
	  ?RETRIES
	 ),
    Formula.

-spec random() -> #formula{}.
random() ->
    {atomic, Formula} =
	mnesia:transaction(
	  fun () ->
		  {ok, Id} = autoinc:random(formula_all),
		  % this might fail if we have deleted the formula, but will get a different choice on retry
		  {ok, Formula} = by_id(Id), 
		  Formula
	  end,
	  ?RETRIES
	 ),
    Formula.

-spec to_json(#formula{}) -> json:json_obj().
to_json(#formula{id=Id, url=Url, latex=Latex}) ->
    {struct,
     [
      {<<"id">>, Id},
      {<<"url">>, Url},
      {<<"latex">>, Latex}
     ]
    }.

-spec from_image(string()) -> #formula{}.
from_image(Filename) ->
    ?INFO([from_image, ?VAR(Filename)]),
    Id = id:new(formula),
    Source = <<"springer">>,
    Source_id = list_to_binary(Filename), % !!! would prefer a proper id
    Extension = filename:extension(Filename),
    Bin_extension = list_to_binary(Extension),
    Url = << "image/", Id/binary, Bin_extension/binary >>,
    Latex = none,
    ok = file:rename(Filename, "./priv/www/image/" ++ binary_to_list(Id) ++ Extension),
    new(Id, Source, Source_id, Url, Latex).

-spec from_images(string()) -> list(#formula{}).
from_images(Folder) ->
    ?INFO([from_images, ?VAR(Folder)]), 
    filelib:fold_files(
      Folder,
      ".*",
      true,
      fun (Filename, Formulas) ->
	      [from_image(Filename) | Formulas]
      end,
      []
     ).
