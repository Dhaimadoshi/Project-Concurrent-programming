/*-------------------------------------------------------------------------
 %
 % Project 2011 INGE1131
 %
 % This project uses the programming concepts studied in the INGI1131 course.
 % Some part of the code comes from the INGI1131 course.
 %
 % Authors: - Gerard Nicolas
 %          - Couniot Antoine
 % 
 % Version: 8 december 2008
 % 
 % Compile with:
 %		 ozc -x ozproject.oz
 %
 % Example of execution:
 %		./CaptureTheFlag --timeout 666 -f 20
 %
 %-------------------------------------------------------------------------
 %*/


%/!\ Attention aux coordonné corriger X en y et vise versa !

functor
import
   Application % Allows to terminate the application
   System 
   QTk at 'x-oz://system/wp/QTk.ozf'
   OS
   Browser
define
   
   % Default Arguments
   NUMBER_OF_TEAMS = 6
   TEAMS = {MakeTuple homes NUMBER_OF_TEAMS}              % tuple qui contient les Ids des Teams
   HEIGHT = 16
   WIDTH = HEIGHT
   SQUARE_HEIGHT = 40
   SQUARE_WIDTH = SQUARE_HEIGHT
   ADJUST = 80
   ACTION_DELAY = 1000
   GOAL = goal(200 200 200 200)
   RESSOURCE_SPOTS                        % contient ressourceSpot(Food Wood Stone Steel) >> Food > [Position Position ...] .... >> Position = position(x:X y:Y)

   Args = {Application.getArgs
	   record(
	      numberOfTeam(single char:&nbt type:int default:NUMBER_OF_TEAMS))
	  }

   
   SquareBoard = {MakeTuple squareBoard HEIGHT} for I in 1..HEIGHT do SquareBoard.I = {MakeTuple line WIDTH} end
   
   CD = {OS.getCWD}
   PlayerImage = {QTk.newImage photo(file:CD#'/mario.gif')}
   WoodImage = {QTk.newImage photo(file:CD#'/wood.gif')}
   
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

/**
%
% post : list of square in range
**/

fun{GetManathanDist Position}
   fun{Manathan X Y Res}
      if (X > 2) then Res
      elseif (Y > 2) then
	 {Manathan X+1 ~2 Res}
      else
	 Square
	 CondX = if X < 0 then X*~1 else X end
	 CondY = if Y < 0 then Y*~1 else Y end
      
	 Bool = (CondX + CondY) < 3 in
	 if Bool then
	    Square = SquareBoard.((Position.x)+X).((Position.y)+Y)
	    {Manathan X Y+1 Square|Res}
	 else
	    {Manathan X Y+1 Res}
	 end
      end
   end
in
   {Manathan ~2 ~2 nil}
end

proc{SendToAllList List Msg}
   case List of H|T then
      {Send H Msg} {SendToAllList T Msg}
      {Browser.browse msgSent#Msg}
   else skip  
   end
end

proc{NewPlayer Team}

   fun{ComputeNextPosition Position Destination}
      DirX = {Parity Position.x Destination.x}
      DirY = {Parity Position.y Destination.y}
   in
      position(x:Position.x + DirX y: Position.y + DirY)
   end
   
   fun{Parity X Y}
      if(X > Y) then ~1
      elseif(X == Y) then 0
      else 1
      end
   end

   Pid
   Bid = {NewBrain Pid} 
   EMPTY_BAG = bag(0 0 0 0)
   STRENGHT_DEFAULT = 10            % 1 = no weapon, 3 = weapon equiped
   WEAPON = 2
   Tid = {Timer}
   ITag = {Canvas newTag($)}
   HomePosition 
   GraphicDisplayer = {NewGraphicDisplayer ITag}
   DefaultState = state(wait HomePosition HomePosition EMPTY_BAG Team STRENGHT_DEFAULT)
in
   {Send TEAMS.Team whichPosition(HomePosition)}
   {Send GraphicDisplayer refresh(HomePosition)}
  % {Delay ACTION_DELAY}
   {Send Bid nextOrder}
   {Send SquareBoard.(HomePosition.x).(HomePosition.y) entering(Pid Team)}
  
   
   Pid = {NewStatePortObject DefaultState
	  fun{$ State Msg} %  {Browser.browse player} {Browser.browse State} {Browser.browse Msg} {Delay 1000}
	     S Position Destination Bag Team Strenght
	  in
	     state(S Position Destination Bag Team Strenght) = State
	     case Msg
	     of goto(Destination) andthen S == wait orelse S == move then
		if(Position == Destination) then
		   {Send Bid nextOrder}
		   state(wait Position Destination Bag Team Strenght)
		else
		   Ack
		   NextPosition = {ComputeNextPosition Position Destination}
		in
		   {Send SquareBoard.(Position.x).(Position.y) leave(Pid Team Ack)}  % /!\ ACK à vérifier
		   {Wait Ack}
		   {Send SquareBoard.(NextPosition.x).(NextPosition.y) entering(Pid Team)}
		   {Browser.browse move#Team}
		   {Send Tid starttimer(ACTION_DELAY Pid)}
		   state(move NextPosition Destination Bag Team Strenght)
		end
	     [] die then
		{Delay ACTION_DELAY}
		{Send GraphicDisplayer refresh(Position)}
		{Send GraphicDisplayer kill}
		{Send TEAMS.Team deletePlayer}
		{Browser.browse dead#Team#Position}
		state(dead Position Destination Bag Team Strenght)
	     [] exploit andthen S == wait then
		{Browser.browse sentexploit#Team#S}
		{Send SquareBoard.(Position.x).(Position.y) exploit(Pid Team)}
		state(exploit Position Destination Bag Team Strenght)
	     [] getRessource(RessourceType) andthen S == exploit then
		NewBag = {AddInTuple Bag RessourceType bag}
	     in
		{Send GraphicDisplayer refreshWithFlash(Position 250)}
		{Send Bid nextOrder}
		{Browser.browse receiveRes#Team}
		state(wait Position Destination NewBag Team Strenght)
	     [] cancel andthen S == exploit then
		{Browser.browse canceldid#Team}
		{Send Bid nextOrder}
		state(wait Position Destination Bag Team Strenght)
	     [] builTower andthen S == wait then          % delay de la tour sera geré dans la case
		{NewTower Team Position}
		State
	     [] buyWeapon andthen S == wait then state(wait Position Destination Bag Team (Strenght + WEAPON))
	     [] emptyBag andthen S == wait then
		{Send TEAMS.Team addToPool(Bag)}
		{Send Bid nextOrder}
		state(wait Position Destination EMPTY_BAG Team Strenght)		   
	     [] stoptimer andthen S == wait orelse S == move then
		{Send GraphicDisplayer refresh(Position)}
		{Browser.browse Position#Team#graph}
		{Send Pid goto(Destination)}
		State
	     [] whichPosition(?PosisitionResult) then PosisitionResult = Position State
	     [] whichDestination(?DestinationResult) then DestinationResult = Destination State
	     [] whichBag(?BagResult)  then BagResult = Bag State
	     [] whichTeam(?TeamResult)  then TeamResult = Team State
	     [] whichStrenght(?StrenghtResult)  then StrenghtResult = Strenght State
	     [] whichState(?StateResult) then StateResult = S State
	     else {Browser.browse msgSentedTo#S#Msg} State
	     end
	  end
	 }
end

proc{NewSquare RessourceTypeInitialiaser ?Sid}    % à l'issu combat : sent winner to list, list send loser to gameMaster, GameMaster kill them and sent remove from square to list
   
   fun{Fight PlayerIds}
      WinnerTeam Loosers Winners CurrentTeam = 1 BestTeam = 1
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
      WinnerTeam = {GetBest {Record.toList TeamStrenght} CurrentTeam BestTeam 0}
      if(WinnerTeam == 0) then % if WinnerTeam == 0 the all lose fight and die
	 {SendToAllList PlayerIds die}
	 nil#0
      else
	 Loosers = {ListFilter PlayerIds fun{$ Player} T in {Send Player whichTeam(T)} T == WinnerTeam end}
	 Winners = {ListFilter PlayerIds fun{$ Player} T in {Send Player whichTeam(T)} T \= WinnerTeam end}
	 {SendToAllList Loosers die}
	 Winners
      end
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

   fun {CheckTowerKill TowerList Pid PlayerTeam}
      case TowerList of H|T then
	 Ack
      in
	 {Send H playerInRange(Pid PlayerTeam Ack)}
	 if(Ack == playerKilled) then
	    true
	 else {CheckTowerKill T Pid PlayerTeam}
	 end
      else false
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
      TeamStrenght = {MakeTuple teamStrenght Args.numberOfTeam}
      fun{Compute PlayerList TeamStrenght}
	 case PlayerList of H|T then
	    Team {Send H whichTeam(Team)}
	    Strenght {Send H whichStrenght(Strenght)}
	    NewTeamStrenght = {MakeTuple teamStrenght Args.numberOfTeam} 
	 in
	    for I in 1..Args.numberOfTeam do
	       if Team \= I then NewTeamStrenght.I = TeamStrenght.I
	       else NewTeamStrenght.I = TeamStrenght.I + Strenght end
	    end
	    {Compute T NewTeamStrenght}
	 else TeamStrenght
	 end
      end
   in
      for I in 1..Args.numberOfTeam do TeamStrenght.I = 0 end
      {Compute PlayerList TeamStrenght}
   end

   fun{Head FarmerList}
      case FarmerList of H|T then H
      else nil
      end
   end
   
   fun{Tail FarmerList}
      case FarmerList of H|T then T
      else nil
      end
   end
      
   DefaultPlayerIdsList = nil
   DefaultTowerList = nil
   FarmerList = nil
   DefaultState = state(FarmerList RessourceTypeInitialiaser DefaultPlayerIdsList DefaultTowerList)
   Tid = {Timer}
   
in
  % {ListFilter L fun{? Pid} T in {Send Pid getTeam(T)} T == 1 end}
   
   Sid = {NewStatePortObject
	  DefaultState
	  fun{$ State Msg}                                       %  {Browser.browse square} {Browser.browse State} {Browser.browse Msg} {Delay 1000}
	     FarmerList RessourceType PlayerIdsList TowerList           % TowerList = tower(TowerId)
	  in
	     state(FarmerList RessourceType PlayerIdsList TowerList) = State
	     case Msg
	     of entering(Pid Team) then
		if(TowerList == nil) then
		   state(FarmerList RessourceType Pid|PlayerIdsList TowerList)
		else
		   if {CheckTowerKill TowerList Pid Team} then State
		   else state(FarmerList RessourceType Pid|PlayerIdsList TowerList)
		   end
		end
	     [] leave(Pid Team ?Ack) then
		NewPlayerIdsList = {ListFilter PlayerIdsList fun{$ Player} Player == Pid end}
	     in
		Ack = ok
		state(FarmerList RessourceType NewPlayerIdsList TowerList)
	     [] addTower(TowerId) then
		state(FarmerList RessourceType PlayerIdsList TowerId|TowerList)
	     [] removeTower(TowerId) then
		NewTowerList = {ListFilter TowerList fun{$ H} H == TowerId end}
	     in
		state(FarmerList RessourceType PlayerIdsList NewTowerList)
	     [] exploit(Pid Team) then
		fun{StateWithNewFarmer}
		   Fl in
		   {Send Tid starttimer(ACTION_DELAY Sid)}          %team dans exploit
		   {Browser.browse waitRes#Team}
		   Fl = {AddList FarmerList Pid}
		   state(Fl RessourceType PlayerIdsList TowerList)
		end
		fun{IsHomeDefended}
		   if (RessourceType \= 5) then false
		   else false
		% which home
		% isthereplayer?
		   end
		end
		
		{Browser.browse entrer#Team}
		PlayersStatus
	     in
		{Send Pid whichState(PlayersStatus)}
		if PlayersStatus == dead then {Browser.browse playerStatusInSquareIsUsefull#PlayersStatus} State
		else
		   if {IsHomeDefended} then
		      Winners
		   in
		      Winners = {Fight PlayerIdsList}
		   elseif FarmerList == nil then
		      {StateWithNewFarmer}
		   else
		      FarmerTeam
		      FirstFarmer = {Head FarmerList}
		   in
		      {Send FirstFarmer whichTeam(FarmerTeam)}
		      if(Team == FarmerTeam) then
			 {StateWithNewFarmer} 
		      else
			 Winners
		      in
			 Winners = {Fight PlayerIdsList}
			 
			 {Browser.browse battleend#PlayerIdsList}

			 {SendToAllList Winners cancel}
			 state(nil RessourceType Winners TowerList)
		      end
		   end
		end
	     [] stoptimer then
		if(FarmerList \= nil) then
		   T = {Head FarmerList} in
		   {Send T getRessource(RessourceType)}
		   {Browser.browse gerRess#T }
		   state( {Tail FarmerList} RessourceType PlayerIdsList TowerList)
		else State
		end
	     [] isRessource(IsRessource) then
		if(RessourceType \= 0) then
		   IsRessource = true
		else
		   IsRessource = false
		end
		State
	     else {Browser.browse errorMsgSquare} State
	     end
	  end     
	 }
end

proc {NewTeam Location ?Tid}

   fun{IsGoal Pool}
      if (GOAL.1 < Pool.1) andthen
	 (GOAL.2 < Pool.2) andthen
	 (GOAL.3 < Pool.3) andthen
	 (GOAL.4 < Pool.4) then true
      else
	 false
      end
   end
   
   Default_Number_Players = 5
   Default_State = state(0 0 0 0 Default_Number_Players)         % state(food wood stone steel)
in
   {SetBoxColor Location yellow}
   
   Tid = {NewStatePortObject Default_State
	  fun{$ State Msg}                                          % {Browser.browse team} {Browser.browse State} {Browser.browse Msg} {Delay 1000}
	     Food Wood Stone Steel NbPlayers
	  in
	     state(Food Wood Stone Steel NbPlayers) = State
	     case Msg
	     of addToPool(Bag) then
		state((Food + Bag.1) (Wood + Bag.2) (Stone + Bag.3) (Steel + Bag.4) NbPlayers)
	     [] creatPlayer(Team) then
		{NewPlayer Team}
		state(Food-10 Wood Stone Steel (NbPlayers + 1))
	     [] deletePlayer then
		state(Food Wood Stone Steel (NbPlayers - 1))
	     [] isGoalComplete(?Bool) then
		Bool = {IsGoal State}
		State
	     [] whichPosition(?Position) then Position = Location State
	     [] whichRessource(?TeamRessource) then TeamRessource = pool(Food Wood Stone Steel) State
	     else {Browser.browse newTeamMsgerror} {Delay 5000} State
	     end
	  end
	 }
end

proc{NewTower Team Position}
   TowerId
   Life = 20
   Range = {GetManathanDist Position}
   DefaultState = state(Life Team Range)
   ITag = {Canvas newTag($)}
   GraphicDisplayer = {NewGraphicDisplayer ITag}
in
   {SendToAllList Range addTower(TowerId)}
   {SetBoxColor Position red}
   TowerId = {NewStatePortObject DefaultState
	      fun{$ State Msg}
		 case State of state(Life Team Range) then
		    case Msg
		    of playerInRange(Pid PlayerTeam ?Ack) then
		       if(Life < 0) then Ack = nothingHappend State
		       else
			  if(PlayerTeam == Team) then Ack = nothingHappend State
			  else
			     Strenght {Send Pid whichStrenght(Strenght)}
			     NewLife = Life - Strenght
			  in
			     {Send Pid die}
			     Ack = playerKilled
			     if(NewLife < 0) then
				{SendToAllList Range removeTower(TowerId)}
				{Send GraphicDisplayer destroy(Position)}
			     end
			     state(NewLife Team Range)
			  end
		       end
		    end
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

   fun{FindRessource Position Team}
      position(x:5 y:5)                   % doit choisir quel ressource prendre et renvoyer l'endroit de la plus proche source
   end

   proc{GetRessourceOrder}
      Bag Position TeamRessource Team
   in
      {Send Pid whichTeam(Team)}
      {Send Pid whichPosition(Position)}
      {Send Pid whichBag(Bag)}
      if {IsBagFull Bag} then
	 HomePosition in
	 	 {Browser.browse Team}
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
	    NearestRessource = {FindRessource Position Team}           % tu as acces aux ressource de l'équipe via {Send TEAMS.Team whichRessource(unbound Var)}
	 in
	    {Send Pid goto(NearestRessource)} 
	 end
      end
   end
in
   Bid = {NewPortObject
	  proc{$ Msg} 
	     case Msg
	     of nextOrder then
		% est ce que ej vais faire une tour ? si oui ou ?
		% est qu'on va crée un nouveau joueur ?
		% est ce que vais acheter une arme?
		if(false) skip
		else
		{GetRessourceOrder}
		end
	     end
	  end
	 % end
	 }
end

proc{GameLauncher}

% Creat SquareBoard
%
% Creat Teams
%     -> Creat Default nbr of players
%               -> Creat their Brain
%



 %  {Browser.browse fiveSecToOption} {Delay 5000}
   X Y Z A F H in
   for X in 1..HEIGHT do
      for Y in 1..WIDTH do
	 if(X == 5 andthen Y == 5) then
	    thread SquareBoard.X.Y = {NewSquare 2} end
	    {SetBoxImage WoodImage position(x:X y:Y) {Canvas newTag($)}}
	 else
	    thread SquareBoard.X.Y = {NewSquare 0} end
	 end
      end
   end

   {NewTower 1 position(x: 6 y: 7)}
  
   TEAMS.1 = thread {NewTeam position(x:1 y:1)} end
   thread {NewPlayer 1} end
   TEAMS.2 = thread {NewTeam position(x:10 y:1)} end
   thread {NewPlayer 2} end
   if Args.numberOfTeam >= 3 then
      TEAMS.3 = thread {NewTeam position(x:10 y:10)} end
      thread {NewPlayer 3} end
   end	
   if Args.numberOfTeam >= 4 then
      TEAMS.4 = thread {NewTeam position(x:1 y:10)} end
      thread {NewPlayer 4} end
   end
   if Args.numberOfTeam >= 5 then
      TEAMS.5 = thread {NewTeam position(x:3 y:5)} end
      thread {NewPlayer 5} end
   end
   if Args.numberOfTeam >= 6 then   
      TEAMS.6 = thread {NewTeam position(x:12 y:12)} end
      thread {NewPlayer 6} end	
   end
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
	      [] refreshWithFlash(Position Time) then
		 {ITag delete}
		 {Delay Time}
		 {SetBoxImage PlayerImage Position ITag}
	      [] destroy(Position) then
		 {Delay ACTION_DELAY}
		 {SetBoxColor Position white}
	      [] kill then
		 {Delay ACTION_DELAY div 2}
		 {ITag delete}
	      else {Browser.browse graphicDisplayer}
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
   AdjustmentX = 20
   AdjustmentY = 20
   PixelsPositionX PixelsPositionY
in
   PixelsPositionX = (Position.x) * SQUARE_WIDTH + AdjustmentX
   PixelsPositionY = (Position.y) * SQUARE_HEIGHT + AdjustmentY
   {Canvas tk(create image PixelsPositionX PixelsPositionY image:I tags:ITag)}
end
proc {ITagDeleter ITag}
   {ITag delete}
end

proc {SetBoxColor Position Color}
   {Canvas tk(create rectangle Position.x*40 Position.y*40 Position.x*40+40 Position.y*40+40 fill:Color outline:black)}
end

% game menu
GameMenu=menu(command(
		    text:"New game"
		    accelerator:ctrl(n)
		    action:proc{$} {Browser.browse notImplemented} end)
	      separator
	      command(
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

{GameLauncher}

end
