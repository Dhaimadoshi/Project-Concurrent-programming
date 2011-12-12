
%Position est la position actuelle du Player
%DestinationPlace est la coordonÅÈe se trouvant le plus proche du joueur en question et qui fournit la ressource recherchÅÈe (elle est initialisÅÈe Å‡ [0 0] lors du premier appel
%Acc est un accumulateur pour le calcul de la plus proche distance
%Res n'est qu'une variable intermÅÈdiaire
declare

fun{ShortCutCalculation Position RessourcePlace DestinationPlace Acc} %RessourePlace = liste de tout les lieux(les coordonÅÈes des lieux) ou l'on trouve un type de ressource identique
   case RessourcePlace of (P1|P2) then             %(le type de la ressource en question dÅÈpend de la fonction qui appelle celle-ci)
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
	    DestinationPlace
	 end
      else DestinationPlace
      end
   else DestinationPlace 
   end
end

%SquareListRessource est la liste de toute les cases correspondant Å‡ toute les ressource possible  . elle s'ÅÈcrit sous la forme -> ressource(food : [[1 2][4 4][3 6]] wood : [[1 5][6 2] [1 1]] stone : [[1 5][6 2] [1 1]] steel : [[1 5][6 2] [1 1]])
%Position est la position du Player
%Team est le numÅÈro de la team en question
%AssignementPlayerList est un tuple composÅÈ de 4 champs qui reprÅÈsente le nombre de personne de la team qui sont assignÅÈ Å‡ chaque ressource (dans l'ordre : food wood stone steel)
%RessourceTeam est un tuple qui dit combien d'unitÅÈ de chaque bien possÅÈde la team 
%DestinationPlace est initialisÅÈ Å‡ [0 0] avant d'ÅÍtre modifiÅÈ par la fonction ShortCutCalcutation et va contenir l'endroit le plus proche de la ressource dÅÈsirÅÈe
%WinnerLevelUnit est le niveau d'unitÅÈ nÅÈcessaire de chaque ressource pour remporter la partie
%MinPeopleAssign est le nombre minimun de personne qui doive ÅÍtre assignÅÈ Å‡ l'exploitation d'une ressource 



fun{FindNearestRessourceNeeded Team Position AssignementPlayerList SquareListRessource}
   %Test permet de savoir Å‡ quel ressource on regarde et Iter permet de voir si on a dÅÈjÅ‡ regarder Å‡ toute les ressources (si il vaut 4) et donc indique que l'on doit forcer l'exploitation
   fun{FindRessource Team Position AssignementPlayerList SquareListRessource Test Iter} %Count 
   %pour avoir RessourceTeam il faut envoyer {Send TEAMS.team whichRessource(RessourceTeam)}
      RessourceTeam = pool(150 20 45 10) NbreRess = 4 Acc=200 DestinationPlace=[0 0] SquareListRessoure Choice WinnerLevelUnit = 300 MinPeopleAssign = 5
   in
      case RessourceTeam of pool(Food Wood Stone Steel)
      then
	 case SquareListRessource of ressource(food:F wood:W stone:STO steel:STE)then
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
	       else {Browser.browse erreurfunctionFindRessource} nil
	       end
	    else {Browser.browse erreurfunctionFindRessource}nil
	    end
	 else {Browser.browse erreurfunctionFindRessource}nil
	 end
      else {Browser.browse erreurfunctionFindRessource}nil
      end
   end
in
   {FindRessource Team Position AssignementPlayerList SquareListRessource ({OS.rand} mod 4) 0}
end

   


Team = 1
PositionRessource = ressource(food : [[1 2][4 4][3 6]] wood : [[1 5][6 8][4 1]] stone : [[7 5][6 2][9 1]] steel : [[1 5][6 6][1 1]])
Liste = {FindNearestRessourceNeeded Team position(x:10 y:10) player(1 1 1 0) PositionRessource}%il faut un Acc trÅËs grand pour que ÅÁa marche
{Browse Liste}
 