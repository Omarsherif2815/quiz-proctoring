assign_quiz(quiz(_,Day,Slot,Count),[day(Day,L)|T],AssignedTAs):-
	helperFree(L,Slot,TAs_list),
	helperCount(Count,TAs_list,[],0,AssignedTA),
	length(AssignedTA,Count),
	permutation(AssignedTA,AssignedTAs).
	
assign_quiz(quiz(_,Day,Slot,Count),[day(D,L)|T],AssignedTAs):-
		D \= Day,
		assign_quiz(quiz(_,Day,Slot,Count),T,AssignedTAs).

helperFree(List,Slot,TAs_list):-
	nth1(Slot,List,TAs_list).
	
helperCount(Count,_,Acc,Count,Acc):-!.
helperCount(Count,[],Acc,Curr,AssignedTAs):-
	Curr\=Count,
	fail.
	
helperCount(Count,[H|T],Acc,Curr,AssignedTAs):-
	append(Acc,[H],Acc1),
	Curr1 is Curr+1,
	helperCount(Count,T,Acc1,Curr1,AssignedTAs).
	
helperCount(Count,[_|T],Acc,Curr,AssignedTAs):-
	helperCount(Count,T,Acc,Curr,AssignedTAs).
	
assign_quizzes(L,FreeSchedule,Proctoring):-
	hassign_quizzes(L,FreeSchedule,Proctoring),
	check_All(Proctoring).
	
check_All([_]).	
check_All([H1,H2|T]):-
	\+check_Two(H1,H2),
	check_All([H2|T]),
	check_All([H1|T]).

check_Two(proctors(quiz(_,Day,Slot,_),N),proctors(quiz(_,Day,Slot,_),N2)):-
	check(N,N2).

check([], _List2) :- fail.
check([X|_], List2) :- member(X, List2),!.
check([_X|Xs], List2) :- check(Xs, List2).

	
hassign_quizzes([],_,[]).
hassign_quizzes([H|T],FreeSchedule,[H1|T1]):-
	assign_quiz(H,FreeSchedule,L),
	H1 =proctors(H,L),	
	hassign_quizzes(T,FreeSchedule,T1).
	
	

free_schedule(AllTAs, TeachingSchedule, FreeSchedule) :-
    free_schedule_helper(AllTAs, TeachingSchedule,FreeSchedule).

free_schedule_helper(_, [],[]).
free_schedule_helper(AllTAs, [day(Day, DayList)|T],[H|T1]) :-
    free_day_slots(AllTAs, Day, DayList, FreeSlots),
	H = day(Day,FreeSlots),
    free_schedule_helper(AllTAs, T, T1).

free_day_slots(AllTAs, Day, DayList, FreeSlots) :-
    free_day_slots_helper(AllTAs, Day, DayList, [], FreeSlots).

free_day_slots_helper(_, _, [], Acc, Acc).
free_day_slots_helper(AllTAs, Day, [Slot|Slots], Acc, FreeSlots) :-
    findall(TA, (member(ta(TA, Day_off), AllTAs),
                 \+ member(TA, Slot),
                 Day_off \= Day), FreeTAs),
	permutation(FreeTAs,PFreeTAs),			 
    append(Acc, [PFreeTAs], Acc1),
    free_day_slots_helper(AllTAs, Day, Slots, Acc1, FreeSlots).

	
free_schedulewp(AllTAs, TeachingSchedule, FreeSchedule) :-
    free_schedule_helperwp(AllTAs, TeachingSchedule,FreeSchedule).

free_schedule_helperwp(_, [],[]).
free_schedule_helperwp(AllTAs, [day(Day, DayList)|T],[H|T1]) :-
    free_day_slotswp(AllTAs, Day, DayList, FreeSlots),
	H = day(Day,FreeSlots),
    free_schedule_helperwp(AllTAs, T, T1).

free_day_slotswp(AllTAs, Day, DayList, FreeSlots) :-
    free_day_slots_helperwp(AllTAs, Day, DayList, [], FreeSlots).

free_day_slots_helperwp(_, _, [], Acc, Acc).
free_day_slots_helperwp(AllTAs, Day, [Slot|Slots], Acc, FreeSlots) :-
    findall(TA, (member(ta(TA, Day_off), AllTAs),
                 \+ member(TA, Slot),
                 Day_off \= Day), FreeTAs),		 
    append(Acc, [FreeTAs], Acc1),
    free_day_slots_helperwp(AllTAs, Day, Slots, Acc1, FreeSlots).
	
assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule):-
	free_schedulewp(AllTAs,TeachingSchedule,FreeSchedule),
	assign_quizzes(Quizzes,FreeSchedule,ProctoringSchedule).



	
	
	
	
	