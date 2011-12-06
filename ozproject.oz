functor
import
   Application % Allows to terminate the application
   System 
   QTk at 'x-oz://system/wp/QTk.ozf'
   OS
define

   % Default Arguments
   SquareBoard
   NUMBER_OF_TEAMS = 2
   HOME_LOCATION = homes(home1(x:0 y:0))
   HEIGHT   = 16*40+1
   WIDTH    = HEIGHT
   
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
	       width: WIDTH
	       height: HEIGHT
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

fun{AddInTuple Tuple Data Position Label}
   NewTuple = {MakeTuple Label {Width Tuple}}
in
   for I in 1..{Width Tuple} do
      if I == Position then NewTuple.I = Tuple.I + Data
      else NewTuple.I = Tuple.I
      end
   end
      NewTuple
end

proc{NewPlayer GameMaster Team ?Pid}
   EMPTY_BAG = bag(0 0 0 0)
   STRENGHT_DEFAULT = 1            % 1 = no weapon, 3 = weapon equiped
   Tid = {Timer}
   ITag = {Canvas newTag($)}

   Bid = {NewBrain Pid}
   
   fun{BagIsFull Bag}
      Boolean
   in
      Boolean = 10 == Bag.1 + Bag.2 + Bag.3 + Bag.4
      Boolean
   end
in
   {SetBoxImage PlayerImage HOME_LOCATION.Team ITag}
   {Delay 1000}
   
   Pid = {NewStatePortObject state(waiting
				   HOME_LOCATION.Team
				   EMPTY_BAG
				   Team
				   STRENGHT_DEFAULT)
	  fun{$ State Msg}
	     case State
	     of state(waiting Position Bag Team Strenght) then
		case Msg
		of move(Destination) then
		   {Send GameMaster movePlayer(Position Destination Pid Team Strenght)}
		   {Send Tid starttimer(1000 Pid)}
		   {ITagDeleter ITag}
		   {SetBoxImage PlayerImage Destination ITag}
		   state(busy Destination Bag Team Strenght)
		[] exploit(Ressource) then     % ressource = tuple (0 0 0 0)
		   if({BagIsFull Bag}) then
		      {Send Tid starttimer(1000 Pid)}
		      state(busy Position Bag Team Strenght)              % si exploite alors que sac full > perte de temps, c'est à brain de figure it out
		   else
		      Amount = 1                                         % exploit = always 1 by 1, not hard coded
		   in
		      {Send SquareBoard.(Position.x).(Position.y) exploit(Team)}    % changer square en etat exploited
		      {Send Tid starttimer(1000 Pid)}
		      state(busy Position {AddInTuple Bag Amount Ressource bag} Strenght)
		   end
	%	[] buildTower then
		[] die then state(dead)
	%	[] buyWeapon then
		[] isBagFull(Bool) then
		   if Bag.1 + Bag.2 + Bag.3 + Bag.4 == 10
		   then Bool = true
		   else Bool = false end
		   State
		[] whichTeam(T) then T = Team State
		else State
		end
	     [] state(dead) then State           % risque de faire des calcul pour rien
		    % revive
	     [] state(busy Position Bag Team Strenght) then
		case Msg
		of stoptimer then
		   {Send Bid nextOrder}
		   state(waiting Position Bag Team Strenght)
		[] die then state(dead)
		else State
		end 
	     end
	  end
	 }
   {Send Bid nextOrder}
end

proc{NewGameMaster ?GMid}
   GMid = {NewPortObject
	   proc{$ Msg}
	      case Msg
	      of makePlayer(Team Pid) then {NewPlayer GMid Team Pid}                    % TODO : Add brain
	    %  [] MakeTower then
	      [] movePlayer(From Dest Pid Team Strenght) then
		 {Send SquareBoard.(From.x).(From.y) leave(Pid Team Strenght)}
		 {Send SquareBoard.(Dest.x).(Dest.y) entering(Pid Team Strenght)}
		 % envoyer message au gérant des graphics 
	      [] battle(TeamStrenght Sid) then
		 %calcul qui gagne
		 {Send Sid battleResult(winner)}                                              %TODO
	      else skip% {Browse 'new Game Master msg error'}
	      end
	   end
	  }
end

proc{NewSquare RessourceType GameMaster ?Sid}    % à l'issu combat : sent winner to list, list send loser to gameMaster, GameMaster kill them and sent remove from square to list
   proc{NewPlayerList ?PLid ?DefaultTeamIds}
      State = {MakeTuple state NUMBER_OF_TEAMS}
   in
      for I in 1..NUMBER_OF_TEAMS do State.I = nil end     % initialise le State à nil car aucun joueur n'est présent sur la case.
      
      PLid = {NewStatePortObject State
	      fun{$ State Msg}
		 case Msg
		 of remove(Pid Team TeamIdentifys) then
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
		    TeamIdentifys = NewState
		    NewState
		 [] add(Pid Team TeamIdentifys) then     
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
		    TeamIdentifys = NewState
		    NewState
		 else
		  State % {Browse 'error on newSquare add'}
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
   Sid = {NewStatePortObject state(free RessourceType DefaultTeamStrenght DefaultTeamIds)  % TeamStrenght = tuple représentant la force de l'équipe sur cette case % team ids = tuple avec les cons
	  fun{$ State Msg}                                            % TeamIds = tuple contenant 1 tuple par équipe contenant les Ids des players sur la case
	     case State
	     of state(free RessourceType TeamStrenght TeamIds) then
		case Msg
		of entering(Pid Team Strenght) then
		   TeamIdentifys
		in
		   {Send PlayerList add(Pid Team TeamIdentifys)}
		   {Wait TeamIdentifys}
		   state(free RessourceType {AddInTuple TeamStrenght Strenght Team teamStrenght} TeamIdentifys)
		[] exploit(Team) then                              % passe de free à plus free + envoyer tuple
		    state(exploited RessourceType Team TeamStrenght TeamIds)
		[] leave(Pid Team Strenght) then
		   TeamIdentifys
		in
		   {Send PlayerList remove(Pid Team TeamIdentifys)}
		   {Wait TeamIdentifys}
		   state(free RessourceType {AddInTuple TeamStrenght (Strenght*~1) Team teamStrenght} TeamIdentifys)   % multiplify by -1 because we want to decrease the strenght
		[] whichRessource(WhichRessource) then
		   case RessourceType
		   of nil then WhichRessource = 0
		   [] food then WhichRessource = 1
		   [] wood then WhichRessource = 2
		   [] stone then WhichRessource = 3
		   else WhichRessource = 4
		   end
		   State
		else State
		end
	     [] state(exploited RessourceType ExploiteByTeam TeamStrenght TeamIds) then
		case Msg
		of release() then State                                                  % TODO
		[]  entering(Pid Team Strenght) then
		   TeamIdentifys
		in
		   {Send PlayerList add(Pid Team TeamIdentifys)}
		   {Wait TeamIdentifys}
		    state(free RessourceType {AddInTuple TeamStrenght Strenght Team teamStrenght} TeamIdentifys)
		[] leave(Pid Team Strenght) then
		   TeamIdentifys
		in
		   {Send PlayerList remove(Pid Team TeamIdentifys)}
		   {Wait TeamIdentifys}
		   state(free RessourceType {AddInTuple TeamStrenght (Strenght*~1) Team teamStrenght} TeamIdentifys)   % multiplify by -1 because we want to decrease the strenght
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
		[] whichRessource(WhichRessource) then
		   case RessourceType
		   of nil then WhichRessource = 0
		   [] food then WhichRessource = 1
		   [] wood then WhichRessource = 2
		   [] stone then WhichRessource = 3
		   else WhichRessource = 4
		   end
		   State
		else
		 %  {Browse 'error newsquare msg'}
		   State
		end
	     else
		% {Browse 'new square state error'}
		nil
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
		IsBagFull in
		{Send Pid isBagFull(IsBagFull)}
		if IsBagFull then
		   Team in
		   {Send Pid whichTeam(Team)}
		   {Send Pid move(HOME_LOCATION.Team)}
		else
		   WhichRessource Position in
		   {Send Pid whichPosition(Position)}
		   {Send SquareBoard.(Position.x).(Position.y) whichRessource(WhichRessource)}
		   if WhichRessource == nil then
		      skip% move vers ressource
		   else {Send Pid exploit(WhichRessource)}
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
   {Canvas tk(create rectangle X+1 Y+1 X+41 Y+41 fill:white outline:black)}
end

proc {DrawSquareGrid Size}
   for X in 0..Size-1 do
      for Y in 0..Size-1 do	 
	 {DrawSquare X*40 Y*40}
      end
   end
end


proc {SetBoxImage I Position ITag}
   {Canvas tk(create image (Position.x)*40+12 (Position.y)*40+12 image:I tags:ITag)}
end
proc {ITagDeleter ITag}
   {ITag delete}
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
X1 X2 X3
thread {NewPlayer GMid 1 X1} end
/*
thread {NewPlayer GMid 1 X2} end

thread {NewPlayer GMid 1 X3} end


thread
    for I in 1..5 do
       {Send X2 move(position(x:0+I y:0))}
       {Delay 1000}
    end
end
thread {Send X3 move(position(x:3 y:0))}
       {Delay 1000}
end
*/
end
