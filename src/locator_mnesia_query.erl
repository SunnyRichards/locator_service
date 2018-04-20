%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 8:49 PM
%%%-------------------------------------------------------------------
-module(locator_mnesia_query).
-author("Aiden_Richie").

%% API

-record(cb_reference, {token,node_name}).
-record(chb_reference, {token,node_name}).

-define(success,success).
-define(response,response).

-export([save_content_box/2,get_content_box/1,get_content_boxes/1,remove_content_box/1,update_content_box/2,
         save_channel_box/2,get_channel_box/1,get_channel_boxes/1,remove_channel_box/1,update_channel_box/2,
         save_node/1,remove_node/1,get_node_content_boxes/1,get_node_channel_boxes/1]).

%%%%%===================================================================
%%%%% Save Participant Tokens
%%%%%===================================================================

save_content_box(Token,Node) ->
  case mnesia_utilities:check_n_write(cb_reference,#cb_reference{token = Token, node_name = Node},write,Token) of
    {atomic,ok} -> {?success,box_saved};
    Any -> {error,Any}
  end.

save_channel_box(Token,Node) ->
  case mnesia_utilities:check_n_write(chb_reference,#chb_reference{token = Token, node_name = Node},write,Token) of
    {atomic,ok} -> {?success,box_saved};
    Any -> {error,Any}
  end.

%%%===================================================================
%%% Get Participant Tokens
%%%===================================================================

get_content_box(Token) ->
  case mnesia_utilities:search(cb_reference,Token) of
    {atomic,[Val]} -> {?response,Val};
    Any -> {error,Any}
  end.

get_content_boxes(TokenList) ->
  case mnesia_utilities:multiple_read(cb_reference,TokenList) of
    {atomic,[Val]} -> {?response,Val};
    Any -> {error,Any}
  end.

get_channel_box(Token) ->
  case mnesia_utilities:search(chb_reference,Token) of
    {atomic,[Val]} -> {?response,Val};
    Any -> {error,Any}
  end.

get_channel_boxes(TokenList) ->
  case mnesia_utilities:multiple_read(chb_reference,TokenList) of
    {atomic,[Val]} -> {?response,Val};
    Any -> {error,Any}
  end.

get_node_content_boxes(NodeName) ->
  case mnesia_utilities:index_search(cb_reference,NodeName,node_name) of
    {atomic,[Val]} -> {?response,Val};
    Any -> {error,Any}
  end.

get_node_channel_boxes(NodeName) ->
  case mnesia_utilities:index_search(chb_reference,NodeName,node_name) of
    {atomic,[Val]} -> {?response,Val};
    Any -> {error,Any}
  end.

%%%===================================================================
%%% Delete Participant Tokens
%%%===================================================================

remove_content_box(Token) ->
  case mnesia_utilities:delete(cb_reference,Token) of
    {atomic,ok} -> {?success,box_removed};
    Any-> {error,Any}
  end.

remove_channel_box(Token) ->
  case mnesia_utilities:delete(chb_reference,Token) of
    {atomic,ok} -> {?success,box_removed};
    Any-> {error,Any}
  end.

%%%===================================================================
%%% Update Participant Tokens
%%%===================================================================

update_content_box(New_NodeName,Token) ->
  case mnesia_utilities:search(cb_reference,Token) of
    {atomic,[Record = #cb_reference{}]} ->
      case mnesia_utilities:update(cb_reference,Token,Record#cb_reference{node_name = New_NodeName}) of
        {atomic,ok} -> {?success,box_updated};
        Error-> {error,Error}
      end;
    {atomic,{Token,not_found}} -> {error,not_found}
  end.

update_channel_box(New_NodeName,Token) ->
  case mnesia_utilities:search(chb_reference,Token) of
    {atomic,[Record = #chb_reference{}]} ->
      case mnesia_utilities:update(chb_reference,Token,Record#chb_reference{node_name = New_NodeName}) of
        {atomic,ok} -> {?success,box_updated};
        Error -> {error,Error}
      end;
    {atomic,{Token,not_found}} -> {error,not_found}
  end.

%%%===================================================================
%%% Save a Node
%%%===================================================================

% create the node
% ping that node (optional)
% start the mnesia in that node


save_node(NodeName) ->
   mnesia:change_config(extra_db_nodes,[NodeName]),
   mnesia:change_table_copy_type(schema, NodeName,disc_copies).

%%  mnesia:add_table_copy(cb_reference, NodeName,disc_copies),
%%  mnesia:add_table_copy(chb_reference, NodeName,disc_copies).

%%%===================================================================
%%% Remove a Node
%%%===================================================================

remove_node(NodeName) ->
  mnesia:del_table_copy(schema,NodeName).

