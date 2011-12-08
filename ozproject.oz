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
   HEIGHT   = 16
   WIDTH    = HEIGHT
   SQUARE_HEIGHT = 40
   SQUARE_WIDTH = SQUARE_HEIGHT
   ADJUST = 80
   ACTION_DELAY = 300
   GOAL = goal(200 200 200 200)
   RESSOURCE_SPOTS
   
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



proc{NewPlayer Team ?Pid}

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
  % Bid = {NewBrain Pid}

   Bid = {NewBrain Pid} 
   EMPTY_BAG = bag(0 0 0 0)
   STRENGHT_DEFAULT = 1            % 1 = no weapon, 3 = weapon equiped
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
		{Send GraphicDisplayer kill}
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
	%	[] builTower andthen S == wait then          % delay de la tour sera geré dans la tour
	     [] buyWeapon andthen S == wait then state(wait Position Destination Bag Team (Strenght + WEAPON))
	     [] emptyBag andthen S == wait then
		{Send TEAMS.Team addToPool(Bag)}
		{Send Bid nextOrder}
		state(wait Position Destination EMPTY_BAG Team Strenght)		   
	     [] stoptimer andthen S == wait orelse S == move then
		{Send Pid goto(Destination)}
		{Send GraphicDisplayer refresh(Position)}
		State
	     [] whichPosition(?PosisitionResult) then PosisitionResult = Position State
	     [] whichDestination(?DestinationResult) then DestinationResult = Destination State
	     [] whichBag(?BagResult)  then BagResult = Bag State
	     [] whichTeam(?TeamResult Who)  then TeamResult = Team State
	     [] whichStrenght(?StrenghtResult)  then StrenghtResult = Strenght State
	     [] whichState(?StateResult) then StateResult = S State
	     else {Browser.browse errorOnPlayerState#State#Msg} {Delay ACTION_DELAY} State
	     end
	  end
	 }
end

proc{NewSquare RessourceTypeInitialiaser ?Sid}    % à l'issu combat : sent winner to list, list send loser to gameMaster, GameMaster kill them and sent remove from square to list
   proc{SendToAllList List Msg}
      case List of H|T then
	 {Send H Msg} {SendToAllList T Msg}
	 {Browser.browse msgSent#Msg}
      else skip
	 			 
      end
   end
   
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
	 Loosers = {ListFilter PlayerIds fun{$ Player} T in {Send Player whichTeam(T loosersList)} T == WinnerTeam end}
	 Winners = {ListFilter PlayerIds fun{$ Player} T in {Send Player whichTeam(T winnerList)} T \= WinnerTeam end}
	 {SendToAllList Loosers die}
	 Winners#WinnerTeam
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
	    Team {Send H whichTeam(Team computeStrenght)}
	    Strenght {Send H whichStrenght(Strenght)}
	    NewTeamStrenght = {MakeTuple teamStrenght NUMBER_OF_TEAMS} 
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
      for I in 1..NUMBER_OF_TEAMS do TeamStrenght.I = 0 end
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
	     FarmerList RessourceType PlayerIdsList TowerList
	  in
	     state(FarmerList RessourceType PlayerIdsList TowerList) = State
	     case Msg
	     of entering(Pid Team) then
		if(TowerList == nil) then
		   state(FarmerList RessourceType Pid|PlayerIdsList TowerList)
		else {Browser.browse towerToDO} State                                         % tour !!!
		end
	     [] leave(Pid Team ?Ack) then
		NewPlayerIdsList = {ListFilter PlayerIdsList fun{$ Player} Player == Pid end}
	     in
		Ack = ok
		state(FarmerList RessourceType NewPlayerIdsList TowerList)
	     [] exploit(Pid Team) then
		
		fun{StateWithNewFarmer}
		   Fl in
		   {Send Tid starttimer(ACTION_DELAY Sid)}          %team dans exploit
		   {Browser.browse waitRes#Team}
		   Fl = {AddList FarmerList Pid}
		   state(Fl RessourceType PlayerIdsList TowerList)
		end
		
		{Browser.browse entrer#Team}
		PlayersStatus
	     in
		{Send Pid whichState(PlayersStatus)}
		if PlayersStatus == dead then {Browser.browse playerStatusInSquareIsUsefull#PlayersStatus} State
		else
		   if FarmerList == nil then
		      {StateWithNewFarmer}
		   else
		      FarmerTeam
		      FirstFarmer = {Head FarmerList}
		   in
		      {Send FirstFarmer whichTeam(FarmerTeam exploit1FarmerTeam)}
		      if(Team == FarmerTeam) then
			 {StateWithNewFarmer} 
		      else
			 Winners#WinnerTeam = {Fight PlayerIdsList}
			 {Browser.browse battleend#PlayerIdsList#WinnerTeam#Team}
		      in
			 if(Team == WinnerTeam) then
			    {Browser.browse winnerTeam#Team}
			    {Send Pid cancel}
			    state(nil RessourceType Winners TowerList)
			 else
			    {SendToAllList FarmerList cancel}           % si le lanceur de battle perd alors cancel autres exploit sinon ils sont mort et ok
			    state(nil RessourceType Winners TowerList)
			 end
		      end
		   end
		end
	     [] stoptimer then
		if(FarmerList \= nil) then
		   T = {Tail FarmerList} in
		   {Send {Head FarmerList} getRessource(RessourceType)}
		   {Browser.browse FarmerList#T}
		   state(T RessourceType PlayerIdsList TowerList)
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
   
   Default_State = state(0 0 0 0)         % state(food wood stone steel)
in
   {SetBoxColor Location yellow}
   
   Tid = {NewStatePortObject Default_State
	  fun{$ State Msg}                                          % {Browser.browse team} {Browser.browse State} {Browser.browse Msg} {Delay 1000}
	     Food Wood Stone Steel
	  in
	     state(Food Wood Stone Steel) = State
	     case Msg
	     of whichPosition(?Position) then Position = Location State
	     [] addToPool(Bag) then
		state((Food + Bag.1) (Wood + Bag.2) (Stone + Bag.3) (Steel + Bag.4))
	     [] isGoalComplete(?Bool) then
		Bool = {IsGoal State}
		State
	     [] whichRessource(?TeamRessource) then TeamRessource = pool(Food Wood Stone Steel) State
	     else {Browser.browse newTeamMsgerror} {Delay 5000} State
	     end
	  end
	 }
end

proc{NewTower ?TowerId}
   TowerId = 1
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
in
   Bid = {NewPortObject
	  proc{$ Msg} 
	     case Msg
	     of nextOrder then
		Bag Position TeamRessource Team
		FoodT WoodT StoneT SteelT
	     in
		{Send Pid whichTeam(Team brain)}
		{Send TEAMS.Team whichRessource(TeamRessource)}
		pool(FoodT WoodT StoneT SteelT) = TeamRessource
	%	if(FoodT > 10 andthen ) then
	%	elseif
	%	   elseif
	%	else
		   {Send Pid whichPosition(Position)}
		   {Send Pid whichBag(Bag)}
		   if {IsBagFull Bag} then
		      HomePosition in
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
			 {Send Pid goto(NearestRessource)} 
		      end
		   end
		end
	     end
	 % end
	 }
end

proc{GameLauncher}
   {Browser.browse fiveSecToOption} {Delay 5000}
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
  
   TEAMS.1 = thread {NewTeam position(x:1 y:1)} end
   TEAMS.2 = thread {NewTeam position(x:10 y:1)} end
   TEAMS.3 = thread {NewTeam position(x:10 y:10)} end
   TEAMS.4 = thread {NewTeam position(x:1 y:10)} end
   TEAMS.5 = thread {NewTeam position(x:3 y:5)} end
   TEAMS.6 = thread {NewTeam position(x:12 y:12)} end
   
   
   X = thread {NewPlayer 1} end
   Y = thread {NewPlayer 2} end
   Z = thread {NewPlayer 3} end
   A = thread {NewPlayer 4} end
   F = thread {NewPlayer 5} end
   H = thread {NewPlayer 6} end
   {Send H buyWeapon}
   {Send Z buyWeapon}
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
	      [] kill then {ITag delete}
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
