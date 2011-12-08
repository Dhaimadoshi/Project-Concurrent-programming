functor
import
   Application % Allows to terminate the application
   System 
   QTk at 'x-oz://system/wp/QTk.ozf'
   OS
   Browser
define
   
   % Default Arguments
   NUMBER_OF_TEAMS = 2
   TEAMS = {MakeTuple homes NUMBER_OF_TEAMS}              % tuple qui contient les Ids des Teams
   HEIGHT   = 16
   WIDTH    = HEIGHT
   ADJUST = 80
   ACTION_DELAY = 1000
   
   SquareBoard = {MakeTuple squareBoard HEIGHT} for I in 1..HEIGHT do SquareBoard.I = {MakeTuple line WIDTH} end
   
   CD = {OS.getCWD}
   PlayerImage = {QTk.newImage photo(file:CD#'/bomb.gif')}
   
   /**
    % Function that creat a port without states.
    %
    % @pre    : Proc , a procedure that takes one argument.
    % @post   : /
    % @return : A new Port that applies the Proc procedure to all the messages it recieves.
    %*/
   
   /** Grid displayer
   *
   *
   *
   */
   
   % Grid handler
   Canvas
   
   Grid=canvas(handle:Canvas
	       width: WIDTH*40 + ADJUST
	       height: HEIGHT*40 + ADJUST
	       glue:nswe
	       bg:white)
   
   % ScoreDisplayer handler
   SDHandel
   
   ScoreBoard=canvas(handle:SDHandel
		     width:100
		     height:80
		     glue:nwe
		     bg:white)
   
fun {NewPortObject Proc}
   Sin in
   thread for Msg in Sin do {Proc Msg} end end
   {NewPort Sin}
end

/**
    % Function that creat a port and remember states.
    %
    % @pre    : Fun , a function that takes two arguments (the state and the message).
    %          Fun has to return a state to be used for next iteration.
    %					Init is the initial state.			
    % @post   : /
    % @return : A new Port that applies the Proc procedure to all the messages it recieves.
    %*/


fun {NewStatePortObject Init Fun}
   proc {Loop S1 State}
      case S1 of Msg|S2 then
	 {Loop S2 {Fun State Msg}}
      [] nil then skip
      end
   end
   Sin
in
   thread {Loop Sin Init} end
   {NewPort Sin}
end

/**
     % Function that implements a delay
     %
     % @pre : /
     % @post : /
     % @return : A new Port that applies a timer
     %*/

fun {Timer}
   {NewPortObject
    proc {$ Msg}
       case Msg of starttimer(T Pid) then
	  thread {Delay T} {Send Pid stoptimer} end
       end
    end}
end

/**
     %
     %
     % @pre :
     % @post :
     % @return :    
     %*/


fun{AddInTuple Tuple Position Label}      % position = tuple.position % TODO : modify if only us by bag
   NewTuple = {MakeTuple Label {Width Tuple}}
in
   for I in 1..{Width Tuple} do
      if I == Position then NewTuple.I = Tuple.I + 1
      else NewTuple.I = Tuple.I
      end
   end
   NewTuple
end



proc{NewPlayer Bid GameMaster Team ?Pid}

   fun{ComputeNextPosition Destination}
      Destination
   end
   
   fun{Parity X Y}
      if(X > Y) then 1
      elseif(X == Y) then 0
      else ~1
      end
   end
  % Bid = {NewBrain Pid}

   Bid = {NewBrain Pid} 
   EMPTY_BAG = bag(0 0 0 0)
   STRENGHT_DEFAULT = 1            % 1 = no weapon, 3 = weapon equiped
   Tid = {Timer}
   ITag = {Canvas newTag($)}
   HomePosition {Send TEAMS.Team whichPosition(HomePosition)}
   GraphicDisplayer = {NewGraphicDisplayer ITag} {Send GraphicDisplayer refresh(HomePosition)}  
in
   {Delay ACTION_DELAY} 
   {Send Bid nextOrder}
   
   Pid = {NewStatePortObject state(waiting HomePosition EMPTY_BAG Team STRENGHT_DEFAULT)
	  fun{$ State Msg}
	     case State of state(S Position Destination Bag Team Strenght) then
		case Msg
		of goto(Destination) andthen S == wait orelse S == move then
		   if(Position == Destination) then
		      {Send Bid nextOrder}
		      state(wait Position Destination Bag Team Strenght)
		   else
		      NextPosition = {ComputeNextPosition Destination} in
		      {Send SquareBoard.(Position.x).(Position.y) leave(Pid Team)}  % /!\ ACK à vérifier
		      {Send SquareBoard.(NextPosition.x).(NextPosition.y) entering(Pid Team)}
		      {Send Tid starttimer(1000 Pid)}
		      state(move NextPosition Destination Bag Team Strenght)
		   end
		[] die then state(dead)
		[] exploit andthen S == wait then
		   {Send SquareBoard.(Position.x).(Position.y) exploit(Pid Team)}
		   state(exploit Position Destination Bag Team Strenght)
		[] getRessource(RessourceType) andthen S == exploit then
		   NewBag = {AddInTuple Bag RessourceType bag}
		   {Send Bid nextOrder}
		   state(wait Position Destination NewBag Team Strenght)
		[] cancel andthen S == exploit then
		   state(wait Position Destination Bag Team Strenght)
	%	[] builTower andthen S == wait then          % delay de la tour sera geré dans la tour
		[] emptyBag andthen S == wait then
		   {Send TEAMS.Team addToPool(Bag)}
		   state(wait Position Destination EMPTY_BAG Team Strenght)		   
		[] stoptimer andthen S == wait orelse S == move then
		   {Send Pid goto(Destination)}
		   State
		[] whichPosition(?Pos) then Pos = Position State
		[] whichDestination(?Dest) then Dest = Destination State
		[] whichBag(?B) then B = Bag State
		[] whichTeam(?T) then T = Team State
		[] whichStrenght(?S) then S = Strenght State
		else {Browser.browse "error on player State"} {Delay 5000} State
		end
	     else {Browser.browse "error on player State"} {Delay 5000} State
	     end
	  end
	 }
   {Send SquareBoard.(HomePosition.x).(HomePosition.y) entering(Pid)}
   
% {Send Bid nextOrder}
end

proc{NewSquare RessourceTypeInitialiaser ?Sid}    % à l'issu combat : sent winner to list, list send loser to gameMaster, GameMaster kill them and sent remove from square to list
   
   DefaultPlayerIdsList = nil
   DefaultTowerList = nil
   FarmerList = nil
   DefaultState = state(FarmerList RessourceTypeInitialiaser DefaultPlayerIdsList DefaultTowerList)
   Tid = {Timer}

   proc{SendToAllList List Msg}
      case List of H|T then {Send H Msg}
      else skip
      end
   end
   
   fun{Fight PlayerIds}
      WinnerTeam Loosers Winners CurrentTeam = 1 BestTean = 1
      TeamStrenght = {ComputeStrenght PlayerIds}
      fun{GetBest TeamStrenght CurrentTeam BestTeam Best}
	 case TeamStrenght of H|T then
	    if(H > Best) then {GetBest T CurrentTeam+1 CurrentTeam H}
	    elseif (H == Best) then {GetBest T CurrentTeam+1 0 Best}
	    else {GetBest T CurrentTeam+1 BestTeam Best}
	    end
	 else BestTeam
	 end
      end
   in
      WinnerTeam = {GetBest {Record.toList TeamStrenght} CurrentTeamPosition BestTeamPosition 0}
      if(WinnerTeam == 0) then                                            % if WinnerTeam == 0 the all lose fight and die
	 {SendToAllList PlayerIds die}
	 nil
      else
	 Loosers = {ListFilter PlayerIds fun{$ Player} T in {Send Player whichTeam(T)} T == WinnerTeam end}
	 {SendToAllList Loosers die}
	 Winners = {ListFilter PlayerIds fun{$ Player} T in {Send Player whichTeam(T)} T \= WinnerTeam end}
	 Winners
   end
   
   fun{AddList List Element}
      case List of H|T then
	 H|{AddList T Element}
      else Element|nil
      end
   end
      
   proc {KillAll Loosers}                   % strenght = 0 pas besoin travail !
      case Loosers
      of H|T then
	 {Send H die}
	 {KillAll T}
      end	
   end
   
   fun{ListFilter List Filter}
      case List of H|T then
	 if {Filter H} then {ListFilter T Filter}
	 else H|{ListFilter T Filter}
	 end
      else nil
      end
   end

   fun{ComputeStrenght PlayerList}
      TeamStrenght = {MakeTuple teamStrenght NUMBER_OF_TEAMS}
      fun{Compute PlayerList TeamStrenght}
	 case PlayerList of H|T then
	    Team {Send H whichTeam(Team)}
	    Strenght {Send H wichStrenght(Strenght)}
	    NewTeamStrenght 
	 in
	    for I in 1..NUMBER_OF_TEAMS do
	       if Team \= I then NewTeamStrenght.I = TeamStrenght.I
	       else NewTeamStrenght.I = TeamStrenght.I + Strenght end
	    end
	    {Compute T NewTeamStrenght}
	 else TeamStrenght
	 end
      end
   in
      {Compute PlayerList TeamStrenght}
   end
in 
  % {ListFilter L fun{? Pid} T in {Send Pid getTeam(T)} T == 1 end}
   
   Sid = {NewStatePortObject
	  DefaultState
	  fun{$ State Msg}
	     FarmList RessourceType PlayerIdsList TowerList
	  in
	     state(FarmList RessourceType PlayerIdsList TowerList) = State
	     case Msg
	     of entering(Pid Team) then
		   if(TowerList == nil) then
		      state(FarmerList RessourceType Pid|PlayerIdsList TowerList)
		   else {Browser.browse 'tower'} State                                         % tour !!!
		   end
	     [] leave(Pid Team) then
		NewPlayerIdsList = {ListFilter PlayerIdsList fun{$ Player} Player == Pid end} in
		state(FarmerList RessourceType NewPlayerIdsList TowerList)
	     [] exploit(Pid Team) then
		fun{StateWithNewFarmer}
		   Fl in
		   {Send Tid starttimer(1000 Sid)}          %team dans exploit
		   Fl = {AddList FarmerList Pid}
		   state(Fl RessourceType PlayerIdsList TowerList)
		end
	     in
		if FarmerList == nil then
		   {StateWithNewFarmer} 
		else
		   FarmerTeam {Send FarmerList.1 whichTeam(FarmerTeam)} in
		   if(Team == FarmerTeam) then
		      {StateWithNewFarmer} 
		   else
		      Winners = {Fight PlayerIdsList}
		   in
		      if(Team == WinnerTeam) then
			 state(nil RessourceType Winners TowerList}
		      else
			 {SendToAllList FarmerList cancel}
			 state(nil RessourceType Winners TowerList)
		      end
		   end
		end
	     [] stoptimer then
		{Send FarmerList.1 getRessource(RessourceType)}
		state(FarmerList.2 RessourceType PlayerIdsList TowerList)
	     [] isRessource(IsRessource) then
		if(RessourceType \= 0) then
		   IsRessource = true
		else
		   IsRessource = false
		end
		State
	     else {Browser.browse "error msg square"}
	     end
	  end     
	 }
end

proc {NewTeam Location ?Tid}
   Default_State = state(0 0 0 0)         % state(food wood stone steel)
in
   {SetBoxColor Location yellow}
   
   Tid = {NewStatePortObject Default_State
	  fun{$ State Msg}
	     Food Wood Stone Steel
	  in
	     state(Food Wood Stone Steel) = State
	     case Msg
	     of whichPosition(?Position) then Position = Location State
	     [] addToPool(Bag) then
		state(Food + Bag.1 Wood + bag.2 Stone + Bag.3 Steel + Bag.4)
	     else {Browser.browse "newTeam Msg error"} {Delay 5000} State
	     end
	  end
	 }
end

/*********
** Brain functions
*
**/

proc{NewBrain Pid ?Bid}
  
   fun{IsBagFull Bag}
      Bag.1 + Bag.2 + Bag.3 + Bag.4 == 10
   end

   fun{FindRessource Position}
      position(x:5 y:5)
   end
   
   Bid = {NewPortObject
	  proc{$ Msg} 
	     case Msg
	     of nextOrder then
		Bag Position in
		{Send Pid whichPosition(Position)}
		{Send Pid whichBag(Bag)}
		if {IsBagFull Bag} then
		   Team HomePosition in
		   {Send Pid whichTeam(Team)}
		   {Send TEAMS.Team whichPosition(HomePosition)} 
		   if(Position == HomePosition)
		   then {Send Pid emptyBag}
		   else {Send Pid goto(HomePosition)}
		   end
		else
		   IsRessource
		in
		   {Send SquareBoard.(Position.x).(Position.y) isRessource(IsRessource)}
		   if IsRessource == true then
		      {Send Pid exploit}
		   else
		      NearestRessource = {FindRessource Position}
		   in
		      {Send Pid goTo(NearestRessource)} 
		   end
		end
	     end
	  end
	 }
end

/*******
** Interface
*
*/

proc{NewGraphicDisplayer ITag ?GDid}
   GDid = {NewPortObject
	   proc{$ Msg}
	      case Msg
	      of refresh(Position) then
		 {ITag delete}
		 {SetBoxImage PlayerImage Position ITag}
	      [] refreshWithFlash(Position) then
		 {ITag delete}
		 {Delay 250}
		 {SetBoxImage PlayerImage Position ITag}
		 
	      else {Browser.browse 'GraphicDisplayer'}
	      end
	   end
	  }
end

proc {DrawSquare X Y}
   {Canvas tk(create rectangle X*40 Y*40 X*40+40 Y*40+40 fill:white outline:black)}
end

proc {DrawSquareGrid Size}
   for X in 1..Size do
      for Y in 1..Size do	 
	 {DrawSquare X Y}
      end
   end
end


proc {SetBoxImage I Position ITag}
   {Canvas tk(create image (Position.x)*40+12 (Position.y)*40+12 image:I tags:ITag)}
end
proc {ITagDeleter ITag}
   {ITag delete}
end

proc {SetBoxColor Position Color}
   {Canvas tk(create rectangle Position.x*40 Position.y*40 Position.x*40+40 Position.y*40+40 fill:Color outline:black)}
end

% game menu
GameMenu=menu(command(
		 text:"Quit"
		 accelerator:ctrl(q)
		 action:proc{$} {Application.exit 0} end))

   % Toolbar containing the menu
Toolbar=lr(	glue:nwe
		menubutton(text:"Game" menu:GameMenu glue:w width:10))

   % App window
Window={QTk.build td(Toolbar Grid ScoreBoard)}
							
{DrawSquareGrid 16}							
   % Display the window

{Window show}

end
