type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])


setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
                    Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
                    P ('h',2),P ('g',2),P ('f',2),P ('e',2),
                    P ('d',2),P ('c',2),P ('b',2),P ('a',2)],
				   [R ('h',8),N ('g',8),B ('f',8),K ('e',8),
                    Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
                    P ('h',7),P ('g',7),P ('f',7),P ('e',7),
                    P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
					
horoof []=""
horoof (h:t)=' ':' ':h:' ':horoof t

visualizeBoard b= putStrLn $ horoof ['a'..'h']++"\n"++show 8++"|"++helpVis b 8 ['a'..'h']

helpVis (x,white,black) 0 _="\nTurn: "++show x++"\n"
helpVis (x,white,black) n []|n==1="\n"++helpVis (x,white,black) (n-1) ['a'..'h']
                            |otherwise="\n"++show (n-1)++"|"++helpVis (x,white,black) (n-1) ['a'..'h']
helpVis (x,white,black) n (h:t)|elem (h,n) (pieceToLoc white) = (makeStr (getHarf (h,n) white))++"W |"++helpVis (x,white,black) n t
                               |elem (h,n) (pieceToLoc black) = (makeStr (getHarf (h,n) black))++"B |"++helpVis (x,white,black) n t
							   |otherwise="   |"++helpVis (x,white,black) n t

					
		
getLoc::Piece->Location--betakhod piece we taraga3ly el location beta3o bas		
getLoc (P (c,i))= (c,i)
getLoc (N (c,i)) = (c,i)
getLoc (K (c,i)) = (c,i)
getLoc (Q (c,i)) = (c,i)
getLoc (R (c,i)) = (c,i)
getLoc (B (c,i)) = (c,i)
getLoc _ = error "Invalid piece"

getP::Piece->Char--betakhod piece we taraga3ly el type beta3ha ka character		
getP (P _) = 'P'
getP (N _) = 'N'
getP (K _) = 'K'
getP (Q _) = 'Q'
getP (R _) = 'R'
getP (B _) = 'B'
getP _ = error "Invalid piece"
		
pieceToLoc :: [Piece] -> [Location]--betakhod list pieces we teraga3ly list el locations beto3hom bas		
pieceToLoc[]=[]		
pieceToLoc (pieces:t)=(getLoc pieces) : pieceToLoc t





isLegal::Piece->Board->Location->Bool	

-------------------------------------------------------------------------------------------------------------------------------------------
isLegal (R loc) (x,white,black) (newc,newi)|newi<1 || newi>8 || newc<'a' || newc>'h'=False--out of bounds
										   |(isStraightLine loc (newc,newi))==False=False--rook trying to move but not in a straight line
                                           |(elem loc (pieceToLoc white))==True && (elem (newc,newi) (pieceToLoc white))==True=False--trying to move a white piece on a white piece
										   |(elem loc (pieceToLoc black))==True && (elem (newc,newi) (pieceToLoc black))==True=False--trying to move a black piece on a black piece
                                           |otherwise=lineEmpty loc (newc,newi) (x,white,black)
										   
isLegal (B loc) (x,white,black) (newc,newi)|newi<1 || newi>8 || newc<'a' || newc>'h'=False--out of bounds
										   |(elem loc (pieceToLoc white))==True && (elem (newc,newi) (pieceToLoc white))==True=False--trying to move a white piece on a white piece
										   |(elem loc (pieceToLoc black))==True && (elem (newc,newi) (pieceToLoc black))==True=False
										   |(isDiag loc (newc,newi))==False=False
										   |otherwise=diagEmpty loc (newc,newi) (x,white,black)								   
											
isLegal (Q loc) (x,white,black) (newc,newi)|newi<1 || newi>8 || newc<'a' || newc>'h'=False--out of bounds
										   |(isStraightLine loc (newc,newi))==False && (isDiag loc (newc,newi))==False=False--queen trying to move but not in a straight line more in a diagonal
										   |(elem loc (pieceToLoc white))==True && (elem (newc,newi) (pieceToLoc white))==True=False--trying to move a white piece on a white piece
										   |(elem loc (pieceToLoc black))==True && (elem (newc,newi) (pieceToLoc black))==True=False--trying to move a black piece on a black piece
                                           |isDiag loc (newc,newi)==True && diagEmpty loc (newc,newi) (x,white,black)==True=True
										   |isStraightLine loc (newc,newi)==True && lineEmpty loc (newc,newi) (x,white,black)==True=True
										   |otherwise=False
										   
isLegal (N (oldc,oldi)) (x,white,black) (newc,newi)|oldc==newc=False
												   |newi<1 || newi>8 || newc<'a' || newc>'h'=False
                                                   |(elem (oldc,oldi) (pieceToLoc white))==True && (elem (newc,newi) (pieceToLoc white))==True=False
												   |(elem (oldc,oldi) (pieceToLoc black))==True && (elem (newc,newi) (pieceToLoc black))==True=False
												   |(abs (newi-oldi)>2)=False 
                                                   |(abs (newi-oldi)==0)=False	   
												   |((newc>(nextC(nextC oldc))) && oldc>'g') || ((newc<(prevC(prevC oldc))) && oldc<'b')=False
												   |(newc==nextC oldc) && ((abs (newi - oldi)) ==2 ) = True
												   |(newc==prevC oldc) && ((abs(newi-oldi)) == 2) = True
												   |(newc==(prevC(prevC oldc))) && ((abs(newi-oldi))==1) = True
												   |(newc==(nextC(nextC oldc))) && ((abs(newi-oldi))==1) = True
											 	   |otherwise= False
												   
isLegal (K (oldc,oldi)) (x,white,black) (newc,newi)|newi<1 || newi>8 || newc<'a' || newc>'h'=False
												   |(elem (oldc,oldi) (pieceToLoc white))==True && (elem (newc,newi) (pieceToLoc white))==True=False
												   |(elem (oldc,oldi) (pieceToLoc black))==True && (elem (newc,newi) (pieceToLoc black))==True=False
												   |(abs (newi-oldi)>1)=False 
												   |oldc==newc && (abs (newi-oldi)==1)=True
												   |oldi==newi && newc==(nextC oldc)=True
												   |oldi==newi && newc==(prevC oldc)=True
												   |newc==(nextC oldc) && (abs (newi-oldi)==1)=True
												   |newc==(prevC oldc) && (abs (newi-oldi)==1)=True
												   |otherwise=False
												   
isLegal (P (oldc,oldi)) (x,white,black) (newc,newi)|(elem (oldc,oldi) (pieceToLoc white)) && newi<=oldi = False
                                                  |(elem (oldc,oldi) (pieceToLoc black)) && newi>=oldi = False
												  |newi<1 || newi>8 || newc<'a' || newc>'h'=False
												  |(elem (oldc,oldi) (pieceToLoc white))==True && (elem (newc,newi) (pieceToLoc white))==True=False
											      |(elem (oldc,oldi) (pieceToLoc black))==True && (elem (newc,newi) (pieceToLoc black))==True=False
												  |(elem (oldc,oldi) (pieceToLoc white)) && oldc==newc && newi==(oldi+1) && (elem (newc,newi) (pieceToLoc white))==False && (elem (newc,newi) (pieceToLoc black))==False=True
												  |(elem (oldc,oldi) (pieceToLoc white)) && oldc==newc && oldi==2 && newi==(oldi+2) && (elem (newc,(oldi+1)) (pieceToLoc white))==False && (elem (newc,(oldi+1)) (pieceToLoc black))==False=True
												  |(elem (oldc,oldi) (pieceToLoc white)) && (newc == (prevC oldc)) && (newi - oldi ==1) && (elem ((newc,newi)) (pieceToLoc black)) = True
												  |(elem (oldc,oldi) (pieceToLoc white)) && (newc == (nextC oldc)) && (newi - oldi ==1) && (elem ((newc,newi)) (pieceToLoc black)) = True
												  |(elem (oldc,oldi) (pieceToLoc black)) && oldc==newc && newi==(oldi-1) && (elem (newc,newi) (pieceToLoc white))==False && (elem (newc,newi) (pieceToLoc black))==False=True
												  |(elem (oldc,oldi) (pieceToLoc black)) && oldc==newc && oldi==7 && newi==(oldi-2) && (elem (newc,(oldi-1)) (pieceToLoc white))==False && (elem (newc,(oldi-1)) (pieceToLoc black))==False=True
												  |(elem (oldc,oldi) (pieceToLoc black)) && (newc == (prevC oldc)) && ((abs(newi - oldi)) ==1) && (elem ((newc,newi)) (pieceToLoc white))= True
												  |(elem (oldc,oldi) (pieceToLoc black)) && (newc == (nextC oldc)) && ((abs(newi - oldi)) ==1) && (elem ((newc,newi)) (pieceToLoc white))= True
												  |otherwise=False
										   
--diagonals---------------------------------------------------------------------------------------------------------------------------------------------------------------											   
										
diagEmpty (cold,nold) (cnew,nnew) (x,white,black)|nnew>nold && cnew>cold=checkDiagIncR ((nextC cold),(nold+1)) ((prevC cnew),(nnew-1)) (x,white,black)--up right doesnt check your current location or the last location
                                                 |nnew>nold && cnew<cold=checkDiagIncL ((prevC cold),(nold+1)) ((nextC cnew),(nnew-1)) (x,white,black)--upLeft
												 |nnew<nold && cnew>cold=checkDiagDecR ((nextC cold),(nold-1)) ((prevC cnew),(nnew+1)) (x,white,black)--down right
                                                 |otherwise=checkDiagDecL ((prevC cold),(nold-1)) ((nextC cnew),(nnew+1)) (x,white,black)--downleft
                                               
checkDiagIncR (cold,nold) (cnew,nnew) (x,white,black)|(prevC cold)==cnew && (nold-1)==nnew=True
                                                     |elem (cold,nold) (pieceToLoc white)=False
                                                     |elem (cold,nold) (pieceToLoc black)=False
													 |otherwise=checkDiagIncR ((nextC cold),(nold+1)) (cnew,nnew) (x,white,black)
													
checkDiagIncL (cold,nold) (cnew,nnew) (x,white,black)|(nextC cold)==cnew && (nold-1)==nnew=True
                                                     |elem (cold,nold) (pieceToLoc white)=False
                                                     |elem (cold,nold) (pieceToLoc black)=False
													 |otherwise=checkDiagIncL ((prevC cold),(nold+1)) (cnew,nnew) (x,white,black)
													
checkDiagDecR (cold,nold) (cnew,nnew) (x,white,black)|(prevC cold)==cnew && (nold+1)==nnew=True
													 |elem (cold,nold) (pieceToLoc white)=False
                                                     |elem (cold,nold) (pieceToLoc black)=False
													 |otherwise=checkDiagDecR ((nextC cold),(nold-1)) (cnew,nnew) (x,white,black)		

checkDiagDecL (cold,nold) (cnew,nnew) (x,white,black)|(nextC cold)==cnew && (nold+1)==nnew=True
													 |elem (cold,nold) (pieceToLoc white)=False
                                                     |elem (cold,nold) (pieceToLoc black)=False
													 |otherwise=checkDiagDecL ((prevC cold),(nold-1)) (cnew,nnew) (x,white,black)													 

isDiag::Location->Location->Bool
isDiag (cold,nold) (cnew,nnew)|nnew>nold=isDiagInc (cold,nold) (cnew,nnew)--up left or right
                              |otherwise=isDiagDec (cold,nold) (cnew,nnew)--down left or right 

isDiagInc::Location->Location->Bool
isDiagInc (cold,nold) (cnew,nnew)|cnew>cold=upR (cold,nold) (cnew,nnew)--up right
                                 |otherwise=upL (cold,nold) (cnew,nnew)--up Left
								 
upR (cold,nold) (cnew,nnew)|cold>'h'=False
                           |nold>8=False
	                       |cold==cnew && nold==nnew=True
	                       |otherwise= upR ((nextC cold),(nold+1)) (cnew,nnew)
						   
upL (cold,nold) (cnew,nnew)|cold=='z'=False
                           |nold>8=False
	                       |cold==cnew && nold==nnew=True
	                       |otherwise= upL ((prevC cold),(nold+1)) (cnew,nnew)
						   


isDiagDec::Location->Location->Bool		
isDiagDec (cold,nold) (cnew,nnew)|cnew>cold=downR (cold,nold) (cnew,nnew)--down right
                                 |otherwise=downL (cold,nold) (cnew,nnew)--down eft
								 
downR (cold,nold) (cnew,nnew)|cold>'h'=False
                             |nold<1=False
	                         |cold==cnew && nold==nnew=True
	                         |otherwise= downR ((nextC cold),(nold-1)) (cnew,nnew)
						   
downL (cold,nold) (cnew,nnew)|cold=='z'=False
                             |nold<1=False
	                         |cold==cnew && nold==nnew=True
	                         |otherwise= downL ((prevC cold),(nold-1)) (cnew,nnew)						 

										
										
										
nextC 'a'='b'
nextC 'b'='c'
nextC 'c'='d'
nextC 'd'='e'
nextC 'e'='f'
nextC 'f'='g'
nextC 'g'='h'
nextC 'h'='i'
nextC 'i'='i'


prevC 'h'='g'
prevC 'g'='f'
prevC 'f'='e'
prevC 'e'='d'
prevC 'd'='c'
prevC 'c'='b'
prevC 'b'='a'
prevC 'a'='z'
prevC 'z'='z'
																
--straightLines-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------											
isStraightLine (cold,nold) (cnew,nnew)|cold/=cnew && nold/=nnew=False
                                      |otherwise=True     			
			
lineEmpty (cold,nold) (cnew,nnew) (x,white,black)|nold==nnew && cold>cnew=checkLetters(cold,nold) (cnew,nnew) ([(nextC cnew)..(prevC cold)]) (x,white,black)--left (men gheir el loc wel newLoc)
											     |nold==nnew && cold<cnew=checkLetters(cold,nold) (cnew,nnew) ([(nextC cold)..(prevC cnew)]) (x,white,black)--right
                                                 |cold==cnew && nold>nnew=checkNums(cold,nold) (cnew,nnew) ([(nnew+1)..(nold-1)]) (x,white,black)--down
											 	 |otherwise=checkNums(cold,nold) (cnew,nnew) ([(nold+1)..(nnew-1)]) (x,white,black)--up
								                

checkLetters::Location->Location->[Char]->Board->Bool
checkLetters _ _ [] _ = True
checkLetters (cold,nold) (cnew,nnew) (h:t) (x,white,black)|elem (h,nold) (pieceToLoc white)=False--ba check law fee ayy haga fe nos el taree2
                                                          |elem (h,nold) (pieceToLoc black)=False               
                                                          |otherwise=checkLetters (cold,nold) (cnew,nnew) t (x,white,black)
														   							   
checkNums::Location->Location->[Int]->Board->Bool
checkNums _ _ [] _ = True		  
checkNums (cold,nold) (cnew,nnew) (h:t) (x,white,black)|elem (cold,h) (pieceToLoc white)=False--ba check law fee ayy haga fe nos el taree2
                                                       |elem (cold,h) (pieceToLoc black)=False               
                                                       |otherwise=checkNums (cold,nold) (cnew,nnew) t (x,white,black)
													
-------------------------------------------------------------------------------------------------------------------------------------------													  
													  					  
fullBoard = [R ('a',1),R ('a',2),R ('a',3),R ('a',4),R ('a',5),R ('a',6),R ('a',7),R ('a',8),
 R ('b',1),R ('b',2),R ('b',3),R ('b',4),R ('b',5),R ('b',6),R ('b',7),R ('b',8),
 R ('c',1),R ('c',2),R ('c',3),R ('c',4),R ('c',5),R ('c',6),R ('c',7),R ('c',8),
 R ('d',1),R ('d',2),R ('d',3),R ('d',4),R ('d',5),R ('d',6),R ('d',7),R ('d',8),
 R ('e',1),R ('e',2),R ('e',3),R ('e',4),R ('e',5),R ('e',6),R ('e',7),R ('e',8),
 R ('f',1),R ('f',2),R ('f',3),R ('f',4),R ('f',5),R ('f',6),R ('f',7),R ('f',8),
 R ('g',1),R ('g',2),R ('g',3),R ('g',4),R ('g',5),R ('g',6),R ('g',7),R ('g',8),
 R ('h',1),R ('h',2),R ('h',3),R ('h',4),R ('h',5),R ('h',6),R ('h',7),R ('h',8)]
 
suggestMove p b = helpSugg p b fullBoard

helpSugg p b []=[]
helpSugg p b (h:t)|isLegal p b (getLoc h) = (getLoc h):helpSugg p b t 
                  |otherwise=helpSugg p b t

-------------------------------------------------------------------------------------------------------------------------------------------------------

move p newLoc (x,white,black) | (isLegal p (x,white,black) newLoc)==False = error ("Illegal move for piece "++show p)
							  | x==White && (elem (getLoc p) (pieceToLoc black)) = error "This is White player's turn, Black can't move."
							  | x==Black && (elem (getLoc p) (pieceToLoc white)) = error "This is Black player's turn, White can't move."                     
						      | x==White && (elem newLoc (pieceToLoc black))=(Black,(createPiece (getP p) newLoc):(updateB p newLoc white),(delB p newLoc black)) --white eating black
							  |	x==Black && (elem newLoc (pieceToLoc white))=(White,(delB p newLoc white),(createPiece (getP p) newLoc):(updateB p newLoc black))  --black eating white		
                              | x==White && (elem newLoc (pieceToLoc black))==False = (Black,(createPiece (getP p) newLoc):(updateB p newLoc white),black)         --white moving to free loc
							  |	x==Black && (elem newLoc (pieceToLoc white))==False=  (White,white,(createPiece (getP p) newLoc):(updateB p newLoc black))     	--black moving to free loc		
                              |otherwise=error "khalas"							  
	
updateB p newLoc []=[]--bamsah el old point mel white 	
updateB p newLoc (h:t)|p/=h = h:updateB p newLoc t
                      |otherwise=updateB p newLoc t
		
delB p newLoc []=[]--bamsah el etakel mel black 		
delB p newLoc (h:t)| newLoc/=(getLoc h) = h:delB p newLoc t
                   |otherwise= delB p newLoc t



makeStr 'P'="P"
makeStr 'N'="N"
makeStr 'K'="K"
makeStr 'Q'="Q"
makeStr 'R'="R"
makeStr 'B'="B"

getHarf loc []=error "ghalattt"
getHarf loc (h:t)|(getLoc h)==loc = (getP h)
                 |otherwise= getHarf loc t


createPiece :: Char -> Location -> Piece				   
createPiece 'P' loc = P loc
createPiece 'N' loc = N loc
createPiece 'K' loc = K loc
createPiece 'Q' loc = Q loc
createPiece 'R' loc = R loc
createPiece 'B' loc = B loc
createPiece _ _ = error "Invalid piece"

