%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2018 10:29 AM
%%%-------------------------------------------------------------------
-module(mnesia_utilities).
-author("Aiden_Richie").

%% API
-export([check_n_write/4,check_n_write_index/5,multiple_write/2,search/2,delete/2,update/3,index_search/3,all_keys/1,replace_key/4]).
-export([check_n_write_multiple_tab/5,write/3,delete_n_return/2,multiple_read/2]).


write(Tab,Record,Lock)->
  Fun= fun()-> mnesia:write(Tab,Record,Lock) end,
  mnesia:transaction(Fun).

check_n_write(Tab,Record,Lock,PrimaryKey)->
  Fun= fun()->
    case mnesia:read(Tab,PrimaryKey,read) of
      [] ->mnesia:write(Tab,Record,Lock);
      _-> <<"Duplicate already exist">>
    end
       end,
  mnesia:transaction(Fun).

check_n_write_index(Tab,Record,Lock,PrimaryKey,Pos)->
  Fun= fun()->
    case mnesia:index_read(Tab,PrimaryKey,Pos) of
      [] ->mnesia:write(Tab,Record,Lock);
      _-> <<"Duplicate already exist">>
    end
       end,
  mnesia:transaction(Fun).

multiple_write(Tab,RecList) ->
  [mnesia:dirty_write(Tab,Rec) || Rec <- RecList],
  ok.

multiple_read(Tab,Keys) -> multiple_read(Tab,Keys,[]).

multiple_read(_Tab,[],Acc) -> Acc;
multiple_read(Tab,[Key|Rest],Acc) ->
  [UserRec] = mnesia:dirty_read(Tab,Key),
  multiple_read(Tab,Rest,[UserRec|Acc]).

update(Tab,Key,Record)->
  Fun =fun()->
    [_R] = mnesia:wread({Tab,Key}),
    mnesia:write(Record)
       end,
  mnesia:transaction(Fun).

search(Tab,Key)->
  Fun = fun()->
    Rec = mnesia:read(Tab,Key,read),
    case Rec of
      []->{Key,not_found};
      _->Rec
    end
        end,
  mnesia:transaction(Fun).

index_search(Tab,Key,Pos)->
  Fun = fun()-> mnesia:index_read(Tab,Key,Pos) end,
  mnesia:transaction(Fun).

delete(Tab,Key)->
  Fun = fun()->
    case mnesia:read(Tab,Key,read) of
      [] -> {Key,not_existing};
      _-> mnesia:delete(Tab,Key,write)
    end
        end,
  mnesia:transaction(Fun).

delete_n_return(Tab,Key)->
  Fun = fun()->
    case mnesia:read(Tab,Key,read) of
      [] -> {Key,not_existing};
      Rec -> mnesia:delete(Tab,Key,write),
        Rec
    end
        end,
  mnesia:transaction(Fun).


all_keys(Tab) ->
  Fun = fun()-> mnesia:all_keys(Tab) end,
  mnesia:transaction(Fun).

replace_key(Tab,OldKey,Lock,Record) ->
  Fun = fun()->
    case mnesia:read(Tab,OldKey,read) of
      [] ->
        {error,not_existing};
      _->
        mnesia:delete(Tab,OldKey,Lock),
        mnesia:write(Tab,Record,Lock)
    end
        end,
  mnesia:transaction(Fun).

check_n_write_multiple_tab(Tab1,Tab2,Key,Lock,Record) ->
  Fun = fun()->
    case mnesia:read(Tab1,Key,read) of
      [] -> {error,not_existing};
      _  -> case mnesia:read(Tab2,Key,read) of
              [] -> mnesia:write(Tab2,Record,Lock);
              _  -> <<"Duplicate already exist">>
            end
    end
        end,
  mnesia:transaction(Fun).
