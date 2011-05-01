% default webmachine callbacks shared by all retex resources

content_types_accepted(ReqData, Context) ->
   {[{"application/json", from_json}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

-type arg_spec() :: {arg_spec, atom(), list(arg_spec_key())}.

-type arg_spec_key() :: atom() | {atom(), fun((json:json()) -> term())}.

% we expect Context to be a record whose fields may be arg_specs
% we replace any arg_specs with the corresponding values from the json body
from_json(ReqData, {arg_spec, Record_name, Args}) ->
    try mochijson2:decode(wrq:req_body(ReqData)) of
	Body ->
	    Values = 
		lists:map(
		  fun ({Key, Decoder}) ->
			  {ok, Value} = json:get(Key, Body),
			  Decoder(Value);
		      (Key) ->
			  {ok, Value} = json:get(Key, Body),
			  Value
		  end,
		  Args
		 ),
	    Context = list_to_tuple([Record_name | Values]),
	    {true, ReqData, Context}
    catch
	_ ->
	    {halt, 400} % bad request
    end.
