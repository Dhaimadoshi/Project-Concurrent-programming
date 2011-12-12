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
   HEIGHT = 8
   WIDTH = HEIGHT
   SQUARE_HEIGHT = 96
   SQUARE_WIDTH = SQUARE_HEIGHT
   ADJUST = SQUARE_HEIGHT*2
   ACTION_DELAY = 1000
   GOAL = goal(200 200 200 200)
   RESSOURCE_SPOTS                     % contient ressourceSpot(Food Wood Stone Steel) >> Food > [Position Position ...] .... >> Position = position(x:X y:Y)

   Args = {Application.getArgs
	   record(
	      numberOfTeam(single char:&nbt type:int default:NUMBER_OF_TEAMS))
	  }

   
   SquareBoard = {MakeTuple squareBoard HEIGHT} for I in 1..HEIGHT do SquareBoard.I = {MakeTuple line WIDTH} end
   
   CD = {OS.getCWD}
   PlayerImage = {MakeTuple playerImage 8}
   PlayerImage.1 = {QTk.newImage photo(file:CD#'/K1.gif')}
   PlayerImage.2 = {QTk.newImage photo(file:CD#'/K2.gif')}
   PlayerImage.3 = {QTk.newImage photo(file:CD#'/mario.gif')}
   PlayerImage.4 = {QTk.newImage photo(file:CD#'/mario.gif')}
   PlayerImage.5 = {QTk.newImage photo(file:CD#'/mario.gif')}
   PlayerImage.6 = {QTk.newImage photo(file:CD#'/mario.gif')}
   PlayerImage.7 = {QTk.newImage photo(file:CD#'/mario.gif')}
   PlayerImage.8 = {QTk.newImage photo(file:CD#'/mario.gif')}
   
   BastonImage = {QTk.newImage photo(file:CD#'/baston.gif')}
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
	       width: WIDTH*SQUARE_HEIGHT + ADJUST
	       height: HEIGHT*SQUARE_WIDTH + ADJUST
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
      
	 IfManaThanRange = (CondX + CondY) < 3
	 PosX = (Position.x)+X PosY = (Position.y)+Y
	 InBoard = PosX > 0 andthen PosY > 0 andthen PosX < 9 andthen PosY < 9
      in
	 if IfManaThanRange andthen InBoard then
	    Square = SquareBoard.PosX.PosY
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

   fun{ChooseInEnnemyPool EnemyHome}
      fun{MuchNeeded EnemyPool AllyPool Amount Type}
	 case AllyPool of H|RType|T then
	    if H < Amount andthen EnemyPool.RType > 0 then {MuchNeeded EnemyPool T H RType}
	    else {MuchNeeded EnemyPool T Amount Type}
	    end
	 else Type
	 end
      end
      
      EnemyPool AllyPool FormatAllyPool
   in
      {Send TEAMS.EnemyHome whichRessource(EnemyPool)}
      {Send TEAMS.Team whichRessource(AllyPool)}
      FormatAllyPool = [AllyPool.2 2 AllyPool.3 3 AllyPool.4 4]
      {MuchNeeded EnemyPool FormatAllyPool AllyPool.1 1}
   end

   Pid
   Bid = thread {NewBrain Pid} end
   EMPTY_BAG = bag(0 0 0 0)
   STRENGHT_DEFAULT = 10            % 1 = no weapon, 3 = weapon equiped
   WEAPON = 2
   Tid = {Timer}
   ITag = {Canvas newTag($)}
   HomePosition 
   GraphicDisplayer = thread {NewGraphicDisplayer ITag} end
   DefaultState = state(wait HomePosition HomePosition EMPTY_BAG Team STRENGHT_DEFAULT)
in
   {Send TEAMS.Team whichPosition(HomePosition)}
   {Send GraphicDisplayer refresh(HomePosition Team)}
  % {Delay ACTION_DELAY} 
   {Send Bid nextOrder}
   {Send SquareBoard.(HomePosition.x).(HomePosition.y) entering(Pid Team)}
  
   
   Pid = {NewStatePortObject DefaultState
	  fun{$ State Msg}
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
		   {Send Tid starttimer(ACTION_DELAY Pid)}
		   state(move NextPosition Destination Bag Team Strenght)
		end
	     [] die then
		{Delay ACTION_DELAY}
		{Send GraphicDisplayer refresh(Position Team)}
		{Send GraphicDisplayer kill(Position)}
		{Send TEAMS.Team deletePlayer}
		state(dead Position Destination Bag Team Strenght)
	     [] exploit andthen S == wait then
		{Send SquareBoard.(Position.x).(Position.y) exploit(Pid Team)}
		state(exploit Position Destination Bag Team Strenght)
	     [] getRessource(RessourceType Home) andthen S == exploit then
		RessourceGot NewBag
	     in
		if RessourceType == 5 then
		   RessourceGot = {ChooseInEnnemyPool Home}
		else RessourceGot = RessourceType
		end
		NewBag = {AddInTuple Bag RessourceGot bag}
		{Send GraphicDisplayer refreshWithFlash(Position 250 Team)}
		{Send Bid nextOrder}
		state(wait Position Destination NewBag Team Strenght)
	     [] cancel andthen S == exploit then
		{Send Bid nextOrder}
		state(wait Position Destination Bag Team Strenght)
	     [] builTower andthen S == wait then          % delay de la tour sera geré dans la case
		thread {NewTower Team Position} end
		State
	     [] buyWeapon andthen S == wait then state(wait Position Destination Bag Team (Strenght + WEAPON))
	     [] emptyBag andthen S == wait then
		{Send TEAMS.Team addToPool(Bag)}
		{Send Bid nextOrder}
		state(wait Position Destination EMPTY_BAG Team Strenght)		   
	     [] stoptimer andthen S == wait orelse S == move then
		{Send GraphicDisplayer refresh(Position Team)}
		{Send Pid goto(Destination)}
		State
	     [] whichPosition(?PosisitionResult) then PosisitionResult = Position State
	     [] whichDestination(?DestinationResult) then DestinationResult = Destination State
	     [] whichBag(?BagResult)  then BagResult = Bag State
	     [] whichTeam(?TeamResult)  then TeamResult = Team State
	     [] whichStrenght(?StrenghtResult)  then StrenghtResult = Strenght State
	     [] whichState(?StateResult) then StateResult = S State
	     else State
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
   DefaultHome = 0
   DefaultState = state(FarmerList RessourceTypeInitialiaser DefaultPlayerIdsList DefaultTowerList DefaultHome)
   ITag = {Canvas newTag($)}
    GraphicDisplayer = thread {NewGraphicDisplayer ITag} end
   Tid = {Timer}
   
in
  % {ListFilter L fun{? Pid} T in {Send Pid getTeam(T)} T == 1 end}
   
   Sid = {NewStatePortObject
	  DefaultState
	  fun{$ State Msg}
	     FarmerList RessourceType PlayerIdsList TowerList Home           % TowerList = tower(TowerId)
	  in
	     state(FarmerList RessourceType PlayerIdsList TowerList Home) = State
	     case Msg
	     of entering(Pid Team) then
		if(TowerList == nil) then
		   state(FarmerList RessourceType Pid|PlayerIdsList TowerList Home)
		else
		   if {CheckTowerKill TowerList Pid Team} then State
		   else state(FarmerList RessourceType Pid|PlayerIdsList TowerList Home)
		   end
		end
	     [] leave(Pid Team ?Ack) then
		NewPlayerIdsList = {ListFilter PlayerIdsList fun{$ Player} Player == Pid end}
	     in
		Ack = ok
		state(FarmerList RessourceType NewPlayerIdsList TowerList Home)
	     [] addTower(TowerId) then
		state(FarmerList RessourceType PlayerIdsList TowerId|TowerList Home)
	     [] removeTower(TowerId) then
		NewTowerList = {ListFilter TowerList fun{$ H} H == TowerId end}
	     in
		state(FarmerList RessourceType PlayerIdsList NewTowerList Home)
	     [] setHome(Team) then state(FarmerList 5 PlayerIdsList TowerList Team)
	     [] exploit(Pid Team) then
		fun{StateWithNewFarmer}
		   Fl in
		   {Send Tid starttimer(ACTION_DELAY Sid)}          %team dans exploit
		   Fl = {AddList FarmerList Pid}
		   state(Fl RessourceType PlayerIdsList TowerList Home)
		end
		
		fun{IsHomeDefended}
		   if (RessourceType \= 5) then false
		   else
		      Defence
		   in
		      Defence = {ListFilter PlayerIdsList fun{$ H} T in {Send H whichTeam(T)} T \= Home end}  % defence contient la liste
		      if Defence == nil then false                                                            % des gens présents sur la home et qui appartiennent à la même faction
		      else true
		      end
		   end
		end
		
		PlayersStatus
	     in
		{Send Pid whichState(PlayersStatus)}
		if PlayersStatus == dead then State
		else
		   if {IsHomeDefended} then
		      Winners Position
		   in
		      {Send Pid whichPosition(Position)}
		      {Send GraphicDisplayer baston(Position)}
		      Winners = {Fight PlayerIdsList}
		      {SendToAllList Winners cancel}
		      state(nil RessourceType Winners TowerList Home)
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
			 {SendToAllList Winners cancel}
			 state(nil RessourceType Winners TowerList Home)
		      end
		   end
		end
	     [] stoptimer then
		if(FarmerList \= nil) then
		   T = {Head FarmerList} in
		   {Send T getRessource(RessourceType Home)}
		   state( {Tail FarmerList} RessourceType PlayerIdsList TowerList Home)
		else State
		end
	     [] isRessource(?IsRessource ?WhichHome) then
		WhichHome = Home
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

proc {NewTeam Location Team ?Tid}

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
   LocationX LocationY
in
   position(x:LocationX y:LocationY) = Location
   {SetBoxColor Location yellow}
   {Send SquareBoard.(Location.x).(Location.y) setHome(Team)}
   thread {NewTower Team position(x: LocationX+1 y: LocationY-1)} end
   thread {NewTower Team position(x: LocationX-1 y: LocationY+1)} end

   for I in 1..Default_Number_Players do
      thread {NewPlayer Team} end
   end
   
   Tid = {NewStatePortObject Default_State
	  fun{$ State Msg}                                        
	     Food Wood Stone Steel NbPlayers
	  in
	     state(Food Wood Stone Steel NbPlayers) = State
	     case Msg
	     of addToPool(Bag) then state((Food + Bag.1) (Wood + Bag.2) (Stone + Bag.3) (Steel + Bag.4) NbPlayers)
	     [] creatPlayer(Team) then
		thread {NewPlayer Team} end
		state(Food-10 Wood Stone Steel (NbPlayers + 1))
	     [] deletePlayer then
		state(Food Wood Stone Steel (NbPlayers - 1))
	     [] isGoalComplete(?Bool) then
		Bool = {IsGoal State}
		State
	     [] whichPosition(?Position) then Position = Location State
	     [] whichRessource(?TeamRessource) then TeamRessource = pool(Food Wood Stone Steel) State
	     [] whichNumberPlayers(?NbrePlayers) then NbrePlayers = NbPlayers
	     [] whichTeam(?T) then T = Team
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
   GraphicDisplayer = thread {NewGraphicDisplayer ITag} end
in
   {Browser.browse tower#Position#Range}
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

  
%Position est la position actuelle du Player
%DestinationPlace est la coordonée se trouvant le plus proche du joueur en question et qui fournit la ressource recherchée (elle est initialisée à [0 0] lors du premier appel
%Acc est un accumulateur pour le calcul de la plus proche distance
%Res n'est qu'une variable intermédiaire
   
   fun{ShortCutCalculation Position RessourcePlace DestinationPlace Acc} %RessourePlace = liste de tout les lieux(les coordonées des lieux) ou l'on trouve un type de ressource identique
      DestX DestY
   in
      [DestX DestY] = DestinationPlace
      case RessourcePlace of (P1|P2) then             %(le type de la ressource en question dépend de la fonction qui appelle celle-ci)
	 case Position of position(x:X y:Y) then
	    case P1 of P12|P13 then
	       Res CoordX = P1.1 CoordY = P1.2.1 in
	       if(CoordX>=X) andthen(CoordY>=Y)then Res=(CoordX-X)+(CoordY-Y)
	       elseif (CoordX>=X)andthen (CoordY<Y)then Res = (CoordX-X)+(Y-CoordY)
	       elseif (CoordX<X)andthen (CoordY>=Y)then Res = (X-CoordX)+(CoordY-Y)
	       else Res = (X-CoordX)+(Y-CoordY)
	       end
	       if (Acc > Res)then {ShortCutCalculation Position P2 [CoordX CoordY] Res} 
	       else {ShortCutCalculation Position P2 DestinationPlace Acc} 
	       end
	    else
	       position(x: DestX y: DestY)
	    end
	 else position(x: DestX y: DestY)
	 end
      else position(x: DestX y: DestY)
      end
   end
   
%SquareListRessource est la liste de toute les cases correspondant à toute les ressource possible  . elle s'écrit sous la forme -> ressource(food : [[1 2][4 4][3 6]] wood : [[1 5][6 2] [1 1]] stone : [[1 5][6 2] [1 1]] steel : [[1 5][6 2] [1 1]])
%Position est la position du Player
%Team est le numéro de la team en question
%AssignementPlayerList est un tuple composé de 4 champs qui représente le nombre de personne de la team qui sont assigné à chaque ressource (dans l'ordre : food wood stone steel)
%RessourceTeam est un tuple qui dit combien d'unité de chaque bien posséde la team 
%DestinationPlace est initialisé à [0 0] avant d'être modifié par la fonction ShortCutCalcutation et va contenir l'endroit le plus proche de la ressource désirée
%WinnerLevelUnit est le niveau d'unité nécessaire de chaque ressource pour remporter la partie
%MinPeopleAssign est le nombre minimun de personne qui doive être assigné à l'exploitation d'une ressource 
   
   
   
   fun{FindNearestRessourceNeeded Team Position AssignementPlayerList SquareListRessource}
   %Test permet de savoir à quel ressource on regarde et Iter permet de voir si on a déjà regarder à toute les ressources (si il vaut 4) et donc indique que l'on doit forcer l'exploitation
      fun{FindRessource Team Position AssignementPlayerList SquareListRessource Test Iter} %Count 
   %pour avoir RessourceTeam il faut envoyer {Send TEAMS.team whichRessource(RessourceTeam)}
	 RessourceTeam = pool(150 20 45 10) NbreRess = 4 Acc=200 DestinationPlace=[0 0] SquareListRessoure Choice WinnerLevelUnit = 300 MinPeopleAssign = 5
      in
	 case RessourceTeam of pool(Food Wood Stone Steel)
	 then
	    case SquareListRessource of ressourceSpots(F W STO STE)then
	       case AssignementPlayerList of player(N1 N2 N3 N4)then
		  if Test==0 then
		     if (Iter == NbreRess)then                                            %Si on a parcouru toute les ressources
			{ShortCutCalculation Position F DestinationPlace Acc}
		     elseif (Food > WinnerLevelUnit)orelse (AssignementPlayerList.1)>= MinPeopleAssign then
			{FindRessource Team Position AssignementPlayerList SquareListRessource Test+1 Iter+1}
		     else
			{ShortCutCalculation Position F DestinationPlace Acc}
		     end
		  elseif Test==1 then
		     if (Iter == NbreRess)then
			{ShortCutCalculation Position W DestinationPlace Acc}
		     elseif (Wood > WinnerLevelUnit) orelse (AssignementPlayerList.2)>= MinPeopleAssign then
			{FindRessource Team Position AssignementPlayerList SquareListRessource Test+1 Iter+1}
		     else {ShortCutCalculation Position W DestinationPlace Acc}
		     end
		  elseif Test==2 then
		     if (Iter == NbreRess)then
			{ShortCutCalculation Position STO DestinationPlace Acc}
		     elseif (Stone > WinnerLevelUnit) orelse  AssignementPlayerList.3>= MinPeopleAssign then
			{FindRessource Team Position AssignementPlayerList SquareListRessource Test+1 Iter+1}
		     else
			{ShortCutCalculation Position STO DestinationPlace Acc}
		     end
		  elseif Test==3 then
		     if (Iter == NbreRess)then
			{ShortCutCalculation Position STE DestinationPlace Acc}
		     elseif (Steel> WinnerLevelUnit) orelse  AssignementPlayerList.4>= MinPeopleAssign then
			{FindRessource Team Position AssignementPlayerList SquareListRessource Test+1 Iter+1}
		     else
			{ShortCutCalculation Position STE DestinationPlace Acc}
		     end
		  else {Browser.browse erreurfunctionFindRessource1}{Delay 10000} nil
		  end
	       else {Browser.browse erreurfunctionFindRessource2}{Delay 10000} nil
	       end
	    else {Browser.browse erreurfunctionFindRessource3}{Delay 10000} nil
	    end
	 else {Browser.browse erreurfunctionFindRessource4}{Delay 10000} nil
	 end
      end
   in
      {FindRessource Team Position AssignementPlayerList SquareListRessource ({OS.rand} mod 4) 0}
   end
  
   proc{GetRessourceOrder}
      Bag Position TeamRessource Team
   in
      {Send Pid whichTeam(Team)}
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
	 IsRessource WhichHome
      in
	 {Send SquareBoard.(Position.x).(Position.y) isRessource(IsRessource WhichHome)}
	 if IsRessource \= 0 andthen WhichHome \= Team then
	    {Send Pid exploit}
	 else
	    
	    NearestRessource = {FindNearestRessourceNeeded Team Position player(0 0 0 0) RESSOURCE_SPOTS}           % tu as acces aux ressource de l'équipe via {Send TEAMS.Team whichRessource(unbound Var)}
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
		{GetRessourceOrder}
	     end
	  end
	 % end
	 }
end

proc{GameLauncher}
   {Browser.browse seting}{Delay 4000}
   {DrawSquareGrid HEIGHT WIDTH}
   {MapInitialiser}
% Creat SquareBoard
%
% Creat Teams
%     -> Creat Default nbr of players
%               -> Creat their Brain
%


/**
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
  
   TEAMS.1 = thread {NewTeam position(x:1 y:1) 1} end
   thread {NewPlayer 1} end
   TEAMS.2 = thread {NewTeam position(x:10 y:1) 2} end
   thread {NewPlayer 2} end
   if Args.numberOfTeam >= 3 then
      TEAMS.3 = thread {NewTeam position(x:10 y:10) 3} end
      thread {NewPlayer 3} end
   end	
   if Args.numberOfTeam >= 4 then
      TEAMS.4 = thread {NewTeam position(x:1 y:10) 4} end
      thread {NewPlayer 4} end
   end
   if Args.numberOfTeam >= 5 then
      TEAMS.5 = thread {NewTeam position(x:3 y:5) 5} end
      thread {NewPlayer 5} end
   end
   if Args.numberOfTeam >= 6 then   
      TEAMS.6 = thread {NewTeam position(x:12 y:12) 6} end
      thread {NewPlayer 6} end	
   end
   */
end

/*******
** Interface
*
*/

proc{NewGraphicDisplayer ITag ?GDid}
   GDid = {NewPortObject
	   proc{$ Msg}
	      case Msg
	      of refresh(Position Team) then
		 {ITag delete}
		 {SetBoxImage PlayerImage.Team Position ITag}
	      [] refreshWithFlash(Position Time Team) then
		 {ITag delete}
		 {Delay Time}
		 {SetBoxImage PlayerImage.Team Position ITag}
	      [] destroy(Position) then
		 {Delay ACTION_DELAY}
		 {SetBoxColor Position white}
	      [] kill(Position) then
		 {SetBoxImage BastonImage Position ITag}
		 {Delay ACTION_DELAY div 2}
		 {ITag delete}
	      else {Browser.browse graphicDisplayerError}
	      end
	   end
	  }
end

proc {DrawSquare X Y}
   {Canvas tk(create rectangle X*SQUARE_HEIGHT Y*SQUARE_HEIGHT X*SQUARE_HEIGHT+SQUARE_HEIGHT Y*SQUARE_HEIGHT+SQUARE_HEIGHT fill:white outline:black)}
end

proc {DrawSquareGrid Height Width}
   for X in 1..Height do
      for Y in 1..Width do	 
	 {DrawSquare X Y}
      end
   end
end


proc {SetBoxImage I Position ITag}
   AdjustmentX = SQUARE_HEIGHT div 2
   AdjustmentY = SQUARE_HEIGHT div 2
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
   {Canvas tk(create rectangle Position.x*SQUARE_HEIGHT Position.y*SQUARE_HEIGHT Position.x*SQUARE_HEIGHT+SQUARE_HEIGHT Position.y*SQUARE_HEIGHT+SQUARE_HEIGHT fill:Color outline:black)}
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












proc{MapInitialiser}
   proc{CreatTeams Teams}
      case Teams of team(T Position)|Tail then
	 thread TEAMS.T = {NewTeam Position T} end
	 {CreatTeams Tail}
      else skip
      end
   end
   
   fun{InitialiseSquare Type X Y RessSpots TeamPositions TP}
      NewRessSpots = {MakeTuple ressourceSpots 4}
   in
      case Type
      of team(T) then
	 thread SquareBoard.X.Y = {NewSquare 0} end
	 TP = team(T position(x:X y:Y))|TeamPositions
	 RessSpots
      [] ressource(R) then
	 thread SquareBoard.X.Y = {NewSquare R} end
	 for I in 1..4 do
	    if I == R then
	       NewRessSpots.I = [X Y]|RessSpots.I
	    else NewRessSpots.I = RessSpots.I
	    end
	 end
	 TP = TeamPositions
	 NewRessSpots
      else
	 thread SquareBoard.X.Y = {NewSquare 0} end
	 TP = TeamPositions
	 RessSpots
      end
   end
   
   fun{Initialise X Y RessSpots TeamPositions}
      NewRessSpots TP
   in
      
      if HEIGHT >= X then
	 if WIDTH >= Y then
	    NewRessSpots = {InitialiseSquare Map.X.Y X Y RessSpots TeamPositions TP}
	    {Initialise X+1 Y NewRessSpots TP}
	 else
	    RESSOURCE_SPOTS = RessSpots
	    TeamPositions
	 end
      else {Initialise 1 Y+1 RessSpots TeamPositions}
      end
   end
   
   A = team(1) B = team(2)
   F = ressource(1) W = ressource(2) S = ressource(3)  I = ressource(4)
   RessSpots = ressourceSpots(nil nil nil nil)
   TeamsInfo
   Map = map(
	    line(S 0 0 0 0 0 0 0)
	    line(0 A 0 0 0 0 0 0)
	    line(0 0 0 0 0 W 0 0)
	    line(0 0 0 0 0 0 0 0)
	    line(0 0 0 0 0 0 0 0)
	    line(0 0 F 0 0 0 0 0)
	    line(0 0 0 0 0 0 B 0)
	    line(0 0 0 0 0 0 0 I)    
	    )
in
   TeamsInfo = {Initialise 1 1 RessSpots nil}
   {CreatTeams TeamsInfo}
end



















   % App window
Window={QTk.build td(Toolbar Grid ScoreBoard)}
							
							
   % Display the window

{Window show}

{GameLauncher}

end
