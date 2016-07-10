%%%------------------------------------------------------------------------
%%% File:      debut.erl
%%% @author    Eric Pailleau <debut@crownedgrouse.com>
%%% @copyright 2015 Eric Pailleau 
%%% @doc  
%%% "debut ?... Un bon début."
%%% DEBian Utility Tool
%%% @end  
%%% The MIT License (MIT):
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-------------------------------------------------------------------------

-module(debut).
-author("Eric Pailleau <debut@crownedgrouse.com>").
-vsn("0.1.0").

-mode(compile).

-export([main/1]).

-include_lib("kernel/include/file.hrl").

%%*** Macros *******************************************************************
-define(DEPS, [debbie, debut, edgar, geas, getopt, swab]).

-define(TEST_HELP(O, A), case getopt:parse(O, Args) of
                            {ok, {Options, NonOptArgs}} -> 
                                % Verbosity
                                put(verbosity, proplists:get_value(verbose, Options, 0)),
                                % Help ?
                                case proplists:is_defined(help, Options) of
                                    true  -> throw({help, A});
                                    false -> NewOptions =
                                             case proplists:is_defined(file, Options) of
                                                true -> % Replace "-f -" by filename from stdin, if needed
                                                        F0    = proplists:get_value(file, Options, ""),
                                                        File = case F0 of
                                                                "-"  -> string:strip(get_stdin(),right,$\n) ;
                                                                F0   -> F0
                                                               end,
                                                        NoFileList = proplists:delete(file, Options),
                                                        NoFileList ++ [{file, File}] ;
                                                false -> Options
                                             end,                                             
                                             throw({A, NewOptions, NonOptArgs})
                                end;
                            {error, {_, _}} -> skip
                        end).

-define(PRINT(S, L), case (get(verbosity) >= L) of 
                          true -> display(S, L) ;
                          false -> ok end ).

-define(FATAL(S), display(S, 1), halt(1)).

display(S, L) when (L < 0) -> case ( - get(verbosity) == L ) of
                                    true -> io:format(standard_error,format(S),[fold(S)]) ; 
                                    false -> skip 
                              end;
display(S, L) when (L==0)  -> io:format(standard_io,format(S),[fold(S)]) ; 
display(S, _)              -> io:format(standard_error,format(S),[fold(S)]).

fold(S) -> case io_lib:printable_unicode_list(S) of
                true -> {ok, X} = swab:sync([{fold, 80}],S), X ;
                false -> S
           end.

format(S) -> case io_lib:printable_unicode_list(S) of
                    true  -> "~ts~n" ;
                    false -> "~p.~n" 
             end.

comment(C, Level) -> ?PRINT("% " ++ C, Level) .

%%*** Options definitions ******************************************************
-define(OptHelpList,
        [
          {version, $V,     "version",    undefined     , "Version"}
         ,{admin,   $A,       "admin",    undefined     , "Administration"}
         ,{build,   $B,       "build",    undefined     , "Build Debian package"}
         ,{info,    $I,        "info",    undefined     , "Get info from Debian package"}
         ,{manage,  $M,      "manage",    undefined     , "Manage Debian repository"}
         ,{help,    $h,        "help",    undefined     , "Help on command"}
        ]).

% -V --version : version (verbosity change output : with -v all dependancies versions, with -vv credits) 
-define(OptVersionList,
        [
          {version, $V,        "version", undefined     , "Version"}
         ,{verbose, $v,        "verbose",    integer    , "Verbosity"}
         ,{help,    $h,        "help",    undefined     , "This help"}
        ]).

% -B --build  : build Debian package
% -d --directory : directory
-define(OptBuildList,
        [
          {build,   $B,       "build",  undefined     , "Build Debian package"}
         ,{dir,     $d,   "directory",     string     , "Directory"}
         ,{user,    $u,        "user",     string     , "(uid:name) User to set on data"}
         ,{group,   $g,       "group",     string     , "(gid:name) Group to set on data"}
         ,{verbose, $v,     "verbose",    integer     , "Verbosity"}
         ,{help,    $h,        "help",  undefined     , "This help"}
        ]).
% -I --info   : Get info from Debian package (mainly control file)
% -f --file   : Debian file
% If extra argument : show only this field if exists
-define(OptInfoList,
        [
          {info,    $I,        "info",    undefined     , "Get info from Debian package"}
         ,{file,    $f,        "file",    string        , "Debian package file"}
         ,{verbose, $v,     "verbose",    integer       , "Verbosity"}
         ,{help,    $h,        "help",    undefined     , "This help"}
         ,{field,   undefined, undefined, string        , "Display particular field(s) only on output (otherwise any)"}
        ]).

% -M --manage      : manage Debian repository
-define(OptRepoList,
        [
          {manage,     $M,      "manage",    undefined     , "Manage Debian repository"}
         ,{file,       $f,        "file",    string        , "Debian file"}
         ,{package,    $p,     "package",    string        , "Debian package"}
         ,{repository, $r,  "repository",    string        , "Target repository"}
         ,{suite,      $s,       "suite",    string        , "Target suite"}
         ,{compo,      $c,   "component",    string        , "Target component"}
         ,{verbose,    $v,     "verbose",    integer       , "Verbosity"}
         ,{help,       $h,        "help",    undefined     , "This help"}
        ]).

% -A --admin  : Administration
% enable  : allow access to repository
% disable : disable access to repository
% admin   : administration
% write   : allowed
% read    : read

-define(OptAdminList,
        [
          {admin,      $A,       "admin",    undefined      , "Administration commands"}
		 ,{info,       $i,        "info",    undefined      , "Show informations"}
         ,{'node',     $n,        "node",     string        , "Debris node, if not on local machine"}
         ,{cookie,     $k,      "cookie",     string        , "Erlang cookie to connect Debris node, if different from local"}
		 ,{user,       $u,        "user",     string        , "User name"}
		 ,{enable,     $e,      "enable",     undefined     , "Enable access"}
		 ,{disable,    $d,     "disable",     undefined     , "Disable access"}
		 ,{level,      $l,       "level",     string        , "Access right level [a,w,r]"}
         ,{repository, $r,  "repository",     string        , "Target repository"}
         ,{verbose,    $v,     "verbose",    integer        , "Verbosity"}
         ,{help,       $h,        "help",    undefined      , "This help"}
        ]).
		


%%*** Main *********************************************************************
-spec main(_) -> ok | no_return().

main([]) ->
    getopt:usage(?OptHelpList, escript:script_name());
main(Args) ->
    try 
        help_cmd(Args),
        version_cmd(Args),
        build_cmd(Args),
        info_cmd(Args),
        manage_cmd(Args),
        admin_cmd(Args),
        throw({help, cmd})
    catch
	    throw:Cmd -> case Cmd of
                        {help, W} when (W =:= cmd )     -> getopt:usage(?OptHelpList, escript:script_name()),
                                                        halt(1);
                        {help, W} when (W =:= version ) -> getopt:usage(?OptVersionList, escript:script_name()),
                                                        halt(1);
                        {help, W} when (W =:= build )   -> getopt:usage(?OptBuildList, escript:script_name()),
                                                        halt(1);
                        {help, W} when (W =:= info )    -> getopt:usage(?OptInfoList, escript:script_name()),
                                                        halt(1);
                        {help, W} when (W =:= manage )  -> getopt:usage(?OptRepoList, escript:script_name()),
                                                        halt(1);
                        {help, W} when (W =:= admin )   -> getopt:usage(?OptAdminList, escript:script_name()),
                                                        halt(1);
                        {version, Opt, _}   -> version(Opt) ;
                        {build, Opt, _}     -> build(Opt) ;
                        {info, Opt, NOpt}   -> info(Opt, NOpt) ;
                        {manage, Opt, _}    -> manage(Opt) ;
                        {admin, Opt, _}     -> admin(Opt) ;
                        Z                   -> io:format("Unexpected error. ~p~n",[Z]), halt(2)
                     end;
        _:Reason -> io:format("Error. ~p~n",[Reason]), halt(3)
    after
        halt(0)
    end.

%%******************************************************************************

help_cmd(Args) -> case getopt:parse([{help, $h, "help", undefined, ""}], Args) of
                        {ok, {_Options, _NonOptArgs}} -> 
                            throw({help, cmd});
                        {error, {_, _}} -> skip
                  end.

version_cmd(Args) -> ?TEST_HELP(?OptVersionList, version).

build_cmd(Args)   -> ?TEST_HELP(?OptBuildList, build).

info_cmd(Args)    -> ?TEST_HELP(?OptInfoList, info).

manage_cmd(Args)  -> ?TEST_HELP(?OptRepoList, manage). 

admin_cmd(Args)  -> ?TEST_HELP(?OptAdminList, admin). 



%%******************************************************************************
%%***  Infos  ******************************************************************
%%******************************************************************************
%%------------------------------------------------------------------------------
%% @doc Print information in package (control or fields in control)
%%      If -f - : get filename from stdin
%% Example :
%% find /var/cache/apt/archives/ | xargs -n1 -I{} ./debut -I -f {} Package Version | xargs -n2 echo
%% @end
%%------------------------------------------------------------------------------

info(Args, NOpt) -> % Extract control file content
                    File = proplists:get_value(file, Args, ""),
                    Control = binary_to_list(extract_control_(File)),
                    case proplists:is_defined(field, Args) of
                         true  -> F = proplists:get_value(field, Args),
                                  search_fields(Control, [F] ++ NOpt) ;
                         false -> io:format("~ts",[Control])
                    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
extract_control_(File) ->  try extract_control(File) of
                                X -> X
                           catch 
                                X -> ?FATAL(X)
                           end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
search_fields(Control, List) -> lists:map(fun(Field) -> ?PRINT(search_field(Control, Field),0) end, List).



%%******************************************************************************
%%*** Repository ***************************************************************
%%******************************************************************************
manage(_) -> ok.

%%******************************************************************************
%%*** Admin ********************************************************************
%%******************************************************************************
admin(Options) -> % Get admin password TODO hide password input
				  User  = proplists:get_value(user, Options, '_'),
				  Repo  = proplists:get_value(repository, Options, '_'),
                  Debris =  list_to_atom(proplists:get_value('node', Options, "debris@" ++ net_adm:localhost())),
				  case proplists:get_value(cookie, Options) of
						[]        -> ok ;
						undefined -> ok ;
						C  -> ?PRINT("Setting cookie for Debris node access",2),
							  erlang:set_cookie(Debris, list_to_atom(C)),
							  erlang:set_cookie(node(), list_to_atom(C))
				  end,
				  % Verify that debris node exists and debris_srv reachable
				  case net_adm:ping(Debris) of
					   pong -> global:sync(), % Need to wait global registration table update
							   case global:whereis_name(debris_srv)  of
									undefined ->  ?PRINT("Debris server not found.",0), halt(1);
									_ -> ok
							   end;
					   _    -> ?PRINT("Cannot ping Debris node.",0), halt(1) 
				  end,
				  Passwd = string:strip(io:get_line("Admin password: "),right, $\n),
				  Seed  = erlang:phash2(rand:uniform()),
				  Cred  = sha256_string(sha256_string(Passwd) ++ integer_to_list(Seed)),
				  % Prepare command
				  Cmd = case proplists:get_value(info, Options, false) of
							% Set permissions
							false -> case proplists:get_value(enable, Options, false) of
										  false -> case proplists:get_value(disable, Options, false) of
														% Change access level and keep enable/disable status
														false -> {permit, Cred, Seed, User, Repo, [], proplists:get_value(level, Options, [])};
										                % Disable user
														_     -> {permit, Cred, Seed, User, Repo, false, proplists:get_value(level, Options, [])}
												   end ;
										  % Enable user
										  _     -> {permit, Cred, Seed, User, Repo, true, proplists:get_value(level, Options, [])}
									 end;
							% Get infos
							true  -> {info, Cred, Seed, User, Repo}	 
				        end,
				  case catch gen_server:call({global, debris_srv}, Cmd) of
									      {'EXIT', _} -> ?PRINT("Cannot reach debris server",1), halt(1);
										  []          -> ?PRINT("Empty debris server response",1), halt(1) ;
										  X           -> io:format("~p~n", [X])
				  end.

%%******************************************************************************
%%*** Version ******************************************************************
%%******************************************************************************
%%------------------------------------------------------------------------------
%% @doc Print version and credits depending verbosity
%% @end
%%------------------------------------------------------------------------------
-spec version(_) -> ok.

version(_Opt) -> % v=0 debut version only on stdout
                ?PRINT(get_version(?MODULE),0), 
                % Hash of versions (debut id)
                Vs = get_all_versions(),
                comment("Uniq id : " ++ integer_to_list(erlang:phash2(Vs)), -1),
                % v=1 all modules versions
                comment("Versions", -1),
                lists:foreach(fun({M, V}) -> ?PRINT({M, V}, -1) end, Vs),
                % v=2 all modules credits
                comment("Credits", -2),
                lists:foreach(fun({M, V}) -> ?PRINT({M, V}, -2) end, get_all_credits()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
get_version(Module) -> List = Module:module_info(attributes), 
                       proplists:get_value(vsn, List, "unknown").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
get_credit(Module) ->  List = Module:module_info(attributes),
                       proplists:get_value(author, List, "unknown").

%%------------------------------------------------------------------------------
%% @doc Get all modules used by debut
%% @end
%%------------------------------------------------------------------------------
get_all_modules()  ->  
        S = filename:basename(escript:script_name()),
        lists:foreach(fun(D) -> code:ensure_loaded(D) end, ?DEPS),
        L = code:all_loaded(),
        % Take only module in escript (dirname = debut)
        lists:filter(fun({_Mod, Path}) -> 
                        case Path of 
                          Path when is_atom(Path) -> false ;
                          Path when is_list(Path) -> 
                                case filename:basename(filename:dirname(Path)) of
                                     S -> true ;
                                     _ -> false
                                end
                        end
                     end, L).

%%------------------------------------------------------------------------------
%% @doc Get all versions of modules
%% @end
%%------------------------------------------------------------------------------
-spec get_all_versions() -> list().

get_all_versions() -> lists:map(fun({M, _P}) -> {M, get_version(M)} end, 
                                              lists:sort(get_all_modules())).

%%------------------------------------------------------------------------------
%% @doc Get all credits of modules
%% @end
%%------------------------------------------------------------------------------
-spec get_all_credits() -> list().

get_all_credits() ->  lists:map(fun({M, _P}) -> {M, get_credit(M)} end, 
                                              lists:sort(get_all_modules())).

%%******************************************************************************
%%*** Build ********************************************************************
%%******************************************************************************
%%------------------------------------------------------------------------------
%% @doc 
%% @end
%%------------------------------------------------------------------------------


build(Options) -> RootPath = proplists:get_value(dir, Options),
                  build(detect(RootPath), RootPath, Options).

%%------------------------------------------------------------------------------
%% @doc 
%% @end
%%------------------------------------------------------------------------------
build(external,RootPath, Options) -> 
                    U = case proplists:get_value(user, Options, []) of
                             [] -> [] ;
                             Su -> case string:tokens(Su, ":") of
                                        [Nu] -> % only a name, take current uid
                                               {ok, Iu} = file:read_file_info(RootPath),
                                               Uid = Iu#file_info.uid,
                                               {user, {Uid, Nu}} ;
                                        [Uid, Nu] -> {user, {list_to_integer(Uid), Nu}};
                                        X   -> ?FATAL(X)
                                        
                                   end                                    
                        end,
                    G = case proplists:get_value(group, Options, []) of
                             [] -> [] ;
                             Sg -> case string:tokens(Sg, ":") of
                                        [Ng] -> % only a name, take current uid
                                               {ok, Ig} = file:read_file_info(RootPath),
                                               Gid = Ig#file_info.gid,
                                               {group, {Gid, Ng}} ;
                                        [Gid, Ng] -> {group, {list_to_integer(Gid), Ng}};
                                        Y   -> ?FATAL(Y)
                                        
                                   end                                    
                        end,
                    case debbie:fy([{root_path, RootPath}] ++ [U] ++ [G]) of
                        {error, Reason} -> ?FATAL(Reason), 1 ;
                        ok -> % Rename .deb to debian format
                              ?PRINT(rename_deb(filename:join(RootPath,"debian.deb")),0),
                              0 
                    end.

%%------------------------------------------------------------------------------
%% @doc 
%% @end
%%------------------------------------------------------------------------------
detect(_) -> external.

%%------------------------------------------------------------------------------
%% @doc 
%% @end
%%------------------------------------------------------------------------------
rename_deb(F) -> Target = filename:join(filename:dirname(F), guess_deb_name(F)),
                 case file:rename(F, Target) of
                      {error, Reason} -> ?FATAL(Reason) ;
                      ok              -> Target
                 end.




%%******************************************************************************
%%------------------------------------------------------------------------------
%% @doc Get stdin
%% @end
%%------------------------------------------------------------------------------
  
get_stdin()  -> get_stdin([]).

get_stdin(F) ->  case io:get_chars('', 8192) of
	                 eof -> F ;
	                 T   -> get_stdin(F ++ T)
	             end.

%% TODO merge in a common lib ? 

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------


extract_control(File) ->  C = case edgar:extract(File,[memory]) of
                            {ok, [{"debian-binary   ", _},
                                  {"control.tar.gz  ", C0},
                                  {_, _}]}                   -> C0 ;
                            {ok, [{"debian-binary", _},
                                  {"control.tar.gz", C1},
                                  {_, _}]}                   -> C1 
                              end,
                          {ok, L} = erl_tar:extract({binary,C}, [memory,compressed]),
                          case proplists:is_defined("./control", L) of
                               true  -> proplists:get_value("./control", L, "") ;
                               false -> case proplists:is_defined("control", L) of
                                            true  -> proplists:get_value("control", L, "") ;
                                            false -> throw("control file not found") 
                                        end
                          end.

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------


search_field(Control, Field) -> {ok, F} = swab:sync([{grab, escape_re(Field) ++ ":(.*)"}, {trim,both}], Control),
                                F. 



% Escape '-' character for regexp
escape_re(S) -> S. % TODO

%%------------------------------------------------------------------------------
%% @doc Return debian package name from control file
%% @end
%%------------------------------------------------------------------------------
guess_deb_name(F) -> % Extract name, version , architecture from control file
                     Control = binary_to_list(extract_control(F)),
                     Name    = search_field(Control, "Package"),
                     Version = search_field(Control, "Version"),
                     Archi   = search_field(Control, "Architecture"),
                     lists:flatten(Name ++ "_" ++ Version ++ "_" ++ Archi ++ ".deb").

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------
sha256_string(S) -> hash_string(crypto:hash(sha256,S)).

%%-------------------------------------------------------------------------
%% @doc 
%% @end
%%-------------------------------------------------------------------------


hash_string(X) -> lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(X)]).

