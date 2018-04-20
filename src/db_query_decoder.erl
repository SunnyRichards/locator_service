%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2018 8:40 PM
%%%-------------------------------------------------------------------
-module(db_query_decoder).
-author("Aiden_Richie").

%% API
-export([convert/1]).

%%%%%===================================================================
%%%%% Save Participant Tokens & Node
%%%%%===================================================================

convert({box_created,content_box,Node,Token}) ->
  {save_content_box,[Node,Token]};

convert({box_created,channel_box,Node,Token}) ->
  {save_channel_box,[Node,Token]};

%%%===================================================================
%%% Get Participant Tokens
%%%===================================================================

convert({find_box,content_box,Token}) ->
  {get_content_box,[Token]};

convert({find_boxes,content_box,TokenList}) ->
  {get_content_boxes,[TokenList]};

convert({find_box,channel_box,Token}) ->
  {get_channel_box,[Token]};

convert({find_boxes,content_box,TokenList}) ->
  {get_content_boxes,[TokenList]};

convert({find_node_boxes,content_box,Node}) ->
  {get_node_content_boxes,[Node]};

convert({find_node_boxes,channel_box,Node}) ->
  {get_node_channel_boxes,[Node]};

%%%===================================================================
%%% Delete Participant Tokens
%%%===================================================================

convert({box_deleted,content_box,Token}) ->
  {remove_content_box,[Token]};

convert({box_deleted,channel_box,Token}) ->
  {remove_channel_box,[Token]};

%%%===================================================================
%%% Update Participant Tokens
%%%===================================================================

convert({box_updated,content_box,NewNode,Token}) ->
  {update_content_box,[NewNode,Token]};

convert({box_updated,channel_box,NewNode,Token}) ->
  {update_channel_box,[NewNode,Token]}.

