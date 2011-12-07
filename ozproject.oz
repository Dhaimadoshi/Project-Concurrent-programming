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
                Fun has to return a state to be used for next iteration.
		Init is the initial state.			
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

fun{CombineTuples T1 T2 Type}
   Combined
in
   case Type
   of bag then Combined = {MakeTuple bag 4}
   else  Combined = {MakeTuple data 4}
   end
   for I in 1..4 do                         % attention à la taille des tuples, notement pour les tuples de team
      Combined.I = T1.I + T2.I
   end
   Combined
end

fun{AddInTuple Tuple Data Position Label}      % position = tuple.position
   NewTuple = {MakeTuple Label {Width Tuple}}
in
   for I in 1..{Width Tuple} do
      if I == Position then NewTuple.I = Tuple.I + Data
      else NewTuple.I = Tuple.I
      end
   end
      NewTuple
end

proc{NewPlayer Bid GameMaster Team ?Pid}
   EMPTY_BAG = bag(0 0 0 0)
   STRENGHT_DEFAULT = 1            % 1 = no weapon, 3 = weapon equiped
   Tid = {Timer}
   ITag = {Canvas newTag($)}
   HomePosition
   GraphicDisplayer = {NewGraphicDisplayer ITag}

  % Bid = {NewBrain Pid}
   
   fun{BagIsFull Bag}
      Boolean = 10 == Bag.1 + Bag.2 + Bag.3 + Bag.4
   in
      Boolean
   end

   fun{Parity X Y}
      if(X > Y) then 1
      elseif(X == Y) then 0
      else ~1
      end
   end
in
   {Send TEAMS.Team whichPosition(HomePosition)}
   {Send GraphicDisplayer refresh(HomePosition)}
%   {SetBoxImage PlayerImage HomePosition ITag}
   {Delay ACTION_DELAY}

   {Send SquareBoard.(HomePosition.x).(HomePosition.y) entering(Pid Team STRENGHT_DEFAULT)}
   
   Pid = {NewStatePortObject state(waiting
				   HomePosition
				   EMPTY_BAG
				   Team
				   STRENGHT_DEFAULT)
	  
	  fun{$ State Msg}
	     case State
	     of state(waiting Position Bag Team Strenght) then
		case Msg
		of move(Destination) then  Res in
		   {Send GameMaster movePlayer(Position Destination Pid Team Strenght Result)}
		   
		   {Send Tid starttimer(ACTION_DELAY Pid)}
		   {Send GraphicDisplayer refresh(Destination)}
		   state(busy Destination Bag Team Strenght)
		[] goTo(Destination) then
	%	   if(Destination == Position) then       % if true then nothing to do but wait next instruction (so need to go through busy state)
	%	      {Send Tid starttimer(0 Pid)}
	%	      state(busy Position Bag Team Strenght)
	%	   else
		   DirX DirY NewDestination in
		   DirX = {Parity Destination.x Position.x}
		   DirY = {Parity Destination.y Position.y}
		   NewDestination = position(x:Position.x+DirX y:Position.y+DirY)
		   {Send Pid move(NewDestination)}
		   State
	%	   {Delay ACTION_DELAY}
	%	   {Send Pid goTo(Destination)}
	%	   state(waiting NewPosition Bag Team Strenght)
	%	   end
		[] exploit(RessourceType) then     % ressource = tuple (0 0 0 0)
		   {Send GraphicDisplayer refreshWithFlash(Position)}
		   if({BagIsFull Bag}) then
		      {Send Tid starttimer(ACTION_DELAY Pid)}
		      state(busy Position Bag Team Strenght)              % si exploite alors que sac full > perte de temps, c'est à brain de figure it out
		   else
		      Amount = 1           % exploit = always 1 by 1, not hard coded
		      NewBag = {AddInTuple Bag Amount RessourceType bag}
		   in
		      {Send SquareBoard.(Position.x).(Position.y) exploit(Team)}    % changer square en etat exploited
		      {Send Tid starttimer(ACTION_DELAY Pid)}
		      state(busy Position NewBag Team Strenght)
		   end
	%	[] buildTower then
		[] die then state(dead)
	%	[] buyWeapon then
		[] isBagFull(?Bool) then
		   if {BagIsFull Bag}
		   then Bool = true
		   else Bool = false end
		   State
		[] whichTeam(?T) then T = Team State
		[] whichPosition(?Pos) then Pos = Position State
		[] emptyBag then {Browser.browse Bag}
		   {Send Tid starttimer(ACTION_DELAY Pid)}
		   state(busy Position EMPTY_BAG Team Strenght)
		else {Browser.browse "Player State waiting message error"} State
		end
	     [] state(dead) then State           % risque de faire des calcul pour rien
		    % revive
	     [] state(busy Position Bag Team Strenght) then
		case Msg
		of stoptimer then
		   {Send Bid nextOrder}
		   state(waiting Position Bag Team Strenght)
		[] die then state(dead)
		else {Browser.browse "Error on player States"}
		   State
		end 
	     end
	  end
	 }
  % {Send Bid nextOrder}
end

proc{NewGameMaster ?GMid}
   GMid = {NewPortObject
	   proc{$ Msg}
	      case Msg
	      of makePlayer(Team Pid) then
		 Bid = {NewBrain Pid} in 
		 {NewPlayer Bid GMid Team Pid}                    % TODO : Add to brain making players
	    %  [] MakeTower then
	      [] movePlayer(From Dest Pid Team Strenght) then
		 {Send SquareBoard.(From.x).(From.y) leave(Pid Team Strenght)}
		 {Send SquareBoard.(Dest.x).(Dest.y) entering(Pid Team Strenght)}
% envoyer message au gérant des graphics 
	      [] battle(TeamStrenght Sid) then
		 %calcul qui gagne
		 {Send Sid battleResult(winner)}                                              %TODO
	      else {Browser.browse 'new Game Master msg error :'} {Browser.browse Msg} {Delay 10000}
	      end
	   end
	  }
end

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

proc{NewSquare RessourceTypeInitialiaser GameMaster ?Sid}    % à l'issu combat : sent winner to list, list send loser to gameMaster, GameMaster kill them and sent remove from square to list
   proc{NewPlayerList ?PLid ?DefaultTeamIds}
      State = {MakeTuple state NUMBER_OF_TEAMS}
   in
      for I in 1..NUMBER_OF_TEAMS do State.I = nil end     % initialise le State à nil car aucun joueur n'est présent sur la case.
      
      PLid = {NewStatePortObject State                       % State = ListsOfTeamIdentifiers
	      fun{$ State Msg}
		 case Msg
		 of remove(Pid Team TeamIdentifiers) then
		    NewState = {MakeTuple state NUMBER_OF_TEAMS}
		 in    
		    for I in 1..NUMBER_OF_TEAMS do
		       if I == Team then
			  NewLeft = {MakeTuple team {Width State.I}-1}
			  RemovingPosition
		       in
			  for J in 1..{Width State.I} do
			     if State.I.J == Pid then RemovingPosition = J end
			  end
			  for J in 1..{Width NewLeft} do
			     if J<RemovingPosition then NewLeft.J = State.I.J
			     else NewLeft.J-1 = State.I.J
			     end
			  end
			  NewState.I = NewLeft
		       else NewState.I = State.I end
		    end
		    TeamIdentifiers = NewState
		    NewState
		 [] add(Pid Team TeamIdentifiers) then     
		    NewState = {MakeTuple state NUMBER_OF_TEAMS}
		 in    
		    for I in 1..NUMBER_OF_TEAMS do
		       if I == Team then
			  if State.I == nil then NewState.I = team(Pid)
			  else
			     NewState.I = {AdjoinAt State.I {Width State.I}+1 Pid}
			  end
		       else NewState.I = State.I
		       end
		    end
		    TeamIdentifiers = NewState
		    NewState
		 else
		   {Browser.browse 'error on newPlayerList Msg'} {Delay 2000} State
		 end
	      end
	     }
      DefaultTeamIds = State
   end

   PlayerList
   DefaultTeamIds
   {NewPlayerList PlayerList DefaultTeamIds}
   DefaultTeamStrenght = {MakeTuple teamStrenght NUMBER_OF_TEAMS} for I in 1..NUMBER_OF_TEAMS do DefaultTeamStrenght.I = 0 end 
in   
   Sid = {NewStatePortObject state(free RessourceTypeInitialiaser DefaultTeamStrenght DefaultTeamIds)  % TeamStrenght = tuple représentant la force de l'équipe sur cette case % team ids = tuple avec les cons
	  fun{$ State Msg}                                                                 % TeamIds = tuple contenant 1 tuple par équipe contenant les Ids des players sur la case
	     case State
	     of state(free RessourceType TeamStrenght TeamIds) then
		case Msg
		of entering(Pid Team Strenght) then
		   TeamIdentifiers
		in
		   {Send PlayerList add(Pid Team TeamIdentifiers)}
		   {Wait TeamIdentifiers}
		   state(free RessourceType {AddInTuple TeamStrenght Strenght Team teamStrenght} TeamIdentifiers)
		[] exploit(Team) then                              % passe de free à plus free + envoyer tuple
		    state(exploited RessourceType Team TeamStrenght TeamIds)
		[] leave(Pid Team Strenght) then
		   TeamIdentifiers
		in
		   {Send PlayerList remove(Pid Team TeamIdentifiers)}
		 %  {Wait TeamIdentifiers}
		   state(free RessourceType {AddInTuple TeamStrenght (Strenght*~1) Team teamStrenght} TeamIdentifiers)   % multiplify by -1 because we want to decrease the strenght
		[] whichRessource(?WhichRessource) then   % tuple voir brain
		   case RessourceType
		   of nil then WhichRessource.1 = 0 
		   [] food then WhichRessource.1 = 1
		   [] wood then WhichRessource.1 = 2
		   [] stone then WhichRessource.1 = 3
		   [] steel then WhichRessource.1 = 4
		   else {Browser.browse 'ressource type error error'}
		   end
		  
		   State
		else {Browser.browse 'error on Msg free square'} {Delay 2000} State
		end
	     [] state(exploited RessourceType ExploiteByTeam TeamStrenght TeamIds) then
		case Msg
		of release() then State                                                  % TODO
		[]  entering(Pid Team Strenght) then
		   TeamIdentifiers
		in
		   {Send PlayerList add(Pid Team TeamIdentifiers)}
		   {Wait TeamIdentifiers}
		    state(free RessourceType {AddInTuple TeamStrenght Strenght Team teamStrenght} TeamIdentifiers)
		[] leave(Pid Team Strenght) then
		   TeamIdentifiers
		in
		   {Send PlayerList remove(Pid Team TeamIdentifiers)}
		   {Wait TeamIdentifiers}
		   state(free RessourceType {AddInTuple TeamStrenght (Strenght*~1) Team teamStrenght} TeamIdentifiers)   % multiplify by -1 because we want to decrease the strenght
		[] exploit(Team) then
		   if(ExploiteByTeam == Team) then
		       State
		   else
		      State
	%	      WinnerAck                                                   % TODO : combat
	%	   in
	%	      {Send GameMaster battle(TeamStrenght Sid WinnerAck)}
	%	      {Wait WinnerAck}
	%	      %{Send GameMaster TeamsToBeKilled}
		   end
		[] whichRessource(WhichRessource) then       % tuple voir brain
		   case RessourceType
		   of nil then WhichRessource.1 = 0 
		   [] food then WhichRessource.1 = 1
		   [] wood then WhichRessource.1 = 2
		   [] stone then WhichRessource.1 = 3
		   else WhichRessource.1 = 4
		   end
		   State
		else
		  {Browser.browse 'error on Msg exploited square'} {Delay 2000} State
		end
	     else
		{Browser.browse 'new square state error'}{Delay 2000}
		nil
	     end
	  end
	 }
end

proc {NewTeam Location ?Tid}
   Default_State = state(0 0 0 0 )         % state(food wood stone steel)
in
   {SetBoxColor Location yellow}
   
   Tid = {NewStatePortObject Default_State
	  fun{$ State Msg}
	     case Msg
	     of whichPosition(?Position) then Position = Location State
	     [] addToPool(Bag) then 
	     end
	  end
	 }
end

/*********
** Brain functions
*
**/

proc{NewBrain Pid ?Bid}
   
   Bid = {NewPortObject
	  proc{$ Msg} 
	     case Msg
	     of nextOrder then
		IsBagFull Position in
		{Send Pid whichPosition(Position)}
		{Send Pid isBagFull(IsBagFull)}
		if IsBagFull then
		   Team HomePosition in
		   {Send Pid whichTeam(Team)}
		   {Send TEAMS.Team whichPosition(HomePosition)} 
		   if(Position == HomePosition)
		   then {Send Pid emptyBag}
		   else {Send Pid goTo(HomePosition)}
		   end
		else
		   WhichRessource = {MakeTuple whichRessource 2} in              % WichRessource est un tuple qui contient dans son premier champ le type de la case et dans son deuxieme champ la case vers laquelle se trouve la ressource la plus proche dans le cas ou il n'y a pas de ressource sur la case présente
		   {Send SquareBoard.(Position.x).(Position.y) whichRessource(WhichRessource)}
		   if WhichRessource.1 == 0 then
		      WhichRessource.2 = position(x:5 y:5)
		      {Send Pid goTo(WhichRessource.2)}
		   else
		      {Send Pid exploit(WhichRessource.1)}	   
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


GMid
thread {NewGameMaster GMid} end
thread {NewTeam position(x:1 y:1) TEAMS.1} end
thread {NewTeam position(x:4 y:4) TEAMS.2} end

for X in 1..HEIGHT do
   for Y in 1..WIDTH do
      if((Y == 5) andthen (X == 5))
      then
	 SquareBoard.X.Y = {NewSquare food GMid}
	 {SetBoxColor position(x:5 y:5) blue}
      else
      SquareBoard.X.Y = {NewSquare nil GMid}
      end
   end
end


P Br in
thread {NewBrain P Br} end
thread {NewPlayer Br GMid 1 P} end
{Send Br nextOrder}
/*
thread {NewPlayer GMid 1 X2} end

thread {NewPlayer GMid 1 X3} end


thread
    for I in 1..5 do
       {Send X2 move(position(x:0+I y:0))}
       {Delay ACTION_DELAY}
    end
end
thread {Send X3 move(position(x:3 y:0))}
       {Delay ACTION_DELAY}
end
*/
end
