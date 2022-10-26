-module(test).
-import(string, [substr/3]).
-import(cpu_sup, [start/0, stop/0, util/0]).
% -export([main/0, master/0, start/0, for/1, start_test/1, loop/3, loop_server/1]).
-export([start_master/2,start_worker/1,for/3,for_worker/2,master/2,worker/2,loop_string/2,start/3,wunit/3,last/0
]).
%---------------------------------------Spawnners--------------------------------------------------------
for(0, _, _) ->
    ok;
for(Num, M_Node, Input) ->  
    V = "Master", % Label of Master.
    spawn(test, start, [M_Node, Input, V]),
    for(Num - 1, M_Node, Input). 

for_worker(0, _) -> 
    ok;
for_worker(Num, M_Node) -> % Spawning workers "Num" times. 
    spawn(test, worker, [1, M_Node]),
    for_worker(Num - 1, M_Node).
%----------------------------------------MASTER CODE-----------------------------------------------------
start_master(M_Node, Input) -> % starts master process. 
    register(mid, spawn(test, master, [M_Node, Input])),
    for(10, M_Node, Input).

master(M_Node, Input) -> % This is the main master function that will supervise the actors. 
    % mid ! {self(), startwork},
    io:fwrite("Check"),
    receive
        {Ch, Ninp, Id, Var} -> %Displaying output received from workers. 
            io:fwrite("~p Found Coin ~p\t ~p\t ~p\n", [Var, Ninp, Ch, Id]);
        {_Pid, Msg} ->
            if
                Msg == requestwork ->
                    Worksize = 1,
                    _Pid ! {self(), startwork, Input,Worksize}; %assigns work to the workers and sends the size of working unit.  
                true ->
                    ok
            end
    end,
    master(M_Node, Input).


%----------------------------------------------------------------------------------------------------------------------------
% We have implemented 2 approaches of how the master himself works to find coins. 
% 1. Master works himself to find coins. 
% 2. Master spawns his own slaves to work for him and in this the master just supervises his slaves and other workers. 
% 
%  We found the seconf method to be more efficient. 

% The code for the 1st approach is given below. 

%----------------------------------------------------------------------------------------------------------------------------


% master(N, M_Node, Input) ->
%     % mid ! {self(), startwork},
%     start(M_Node, Input),
%     receive
%         {Ch, Ninp, Id} ->
%             io:fwrite("Master Found Coin ~p" "~p" "~p\n", [Ch, Ninp, Id])
%     end,
%     io:fwrite("\n"),
%     master(N - 1, M_Node, Input).


%---------------------------------------------WORKER CODE---------------------------------------------

start_worker(M_Node) -> % starts workers. 
    statistics(runtime),
    statistics(wall_clock),
    persistent_term:put(t, 10), % keeps a count of number of processes alive. 
    for_worker(10, M_Node).

last() ->
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000,
    U2 = Time2 * 1000,
    io:format(
        "Code time=~p (~p) microseconds~n",
        [U1, U2]
    ).

wunit(0, _, _) ->
    stopped;
wunit(N, M_Node, NumZ) ->
    V = "Worker",
    start(M_Node, NumZ, V),
    wunit(N - 1, M_Node, NumZ).

worker(0, _) ->
    ok;
worker(N, M_Node) -> % The function that every worker execeutes to maintain its messege box. 
    {mid, M_Node} ! {self(), requestwork}, % Just after initializing, the worker requests the master for work. 

    receive
        {_From, Msg, Input,Worksize} ->
            if
                Msg == startwork -> % worker gets a messege from master to start work. 
                    io:fwrite("Starting work"),
                    
                    wunit(Worksize, M_Node, Input),
                    io:fwrite("."),
                    Val = persistent_term:get(t),
                    persistent_term:put(t, Val - 1); %After Each worker finishes its task the count of active workers is decreased
                % From ! {self(), donework};
                true ->
                    ok
            end;
        stop ->
            ok
    end,
    G = persistent_term:get(t),
    if
        G == 0 -> % Checking if active workers are 0. 
            last(); % goes to an endpoint after the last process ends to compute the REAL AND CPU time
        true ->
            worker(N - 1, M_Node) 
    end.

% for(30).

%---------------------------------------CODE FOR GENERATING BITCOIN----------------------------------

loop_string(0, Str) ->
    Str;
loop_string(N, Str) ->
    S = Str ++ "0",
    loop_string(N - 1, S).

start(M_Node, NumZ, Var) ->
    Inp = binary_to_list(base64:encode(crypto:strong_rand_bytes(12))),
    Ninp = "hathavale" ++ Inp,
    % io:fwrite("~p\n",[Ninp]),
    Ch = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Ninp))]),
    S = loop_string(NumZ, ""),

    Bool = string:find(Ch, S) =:= Ch,

    if
        Bool == true ->
            % io:fwrite("~p\n",[substr(Ch,NumZ+1,1)]),
            K = substr(Ch, NumZ + 1, 1),
            if
                K /= "0" ->
                    {mid, M_Node} ! {Ch, Ninp, self(), Var}; % sends the master, the hashed string, input string and the Pid
                true ->
                    start(M_Node, NumZ, Var)
            end;
        true ->
            start(M_Node, NumZ, Var)
    end.