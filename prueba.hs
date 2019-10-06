bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]]





(New "1" (-1) (bind a9 
	(New "" (-1) (bind b10 
		(New "" (-1) (bind b11 
			(Par "" 
				[Send "" 1@0 
					(ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0]),
					Send "" 2@0 
						(ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),
					Send "" 0@0 
						(ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0]),
					ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],
					Buffer 0@0 (True,0,0),
					Buffer 1@0 (True,0,0),
					Buffer 2@0 (True,0,0)]))))))))




					
EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a9 (New "" (-1) (bind b10 (New "" (-1) (bind b11 (Par "" [IChoice "" 3@1) [2@0,1@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [2@0,1@0]),Send "" 2@0 (ChanInst (TVar "" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0]),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))



EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a9 (New "" (-1) (bind b10 (New "" (-1) (bind b11 (Par "" [Send "" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0]),Send "" 2@0 (ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),IChoice "" (Send "9" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [1@0,0@0]),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))




EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a9 (New "" (-1) (bind b10 (New "" (-1) (bind b11 (Par "" [Recv "" 2@0 (IChoice "" (Send "9" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [2@0,1@0])),Send "" 2@0 (ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),Send "" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0]),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))




EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a10 (New "" (-1) (bind b11 (New "" (-1) (bind b12 (Par "" [IChoice "" (Send "9" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [2@0,1@0]),Send "" 2@0 (ChanInst (TVar "" (Send "9" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [1@0,0@0]),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))



EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a11 (New "" (-1) (bind b12 (New "" (-1) (bind b13 (Par "" [Recv "" 2@0 (IChoice "" (Send "9" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [2@0,1@0])),Send "" 2@0 (ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),IChoice "" (Send "9" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [1@0,0@0]),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))




EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a9 (New "" (-1) (bind b10 (New "" (-1) (bind b11 (Par "" [Send "" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0]),Send "" 2@0 (ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),Recv "" 1@0 (IChoice "" (Send "9" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [1@0,0@0])),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))




EqnSys (bind [[(r2,{ChanAbst (bind [x] (Recv "4" 0@0 (New "5" 0 (bind b (Par "6" [ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],Par "7" [ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],Null]])))))}),(f1,{ChanAbst (bind [x,y] (Recv "8" 0@0 (IChoice "" (Send "9" 0@1 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0,0@1],Null])) (Seq "11" [ChanInst (TVar "CALL on Line 11" 1@1) [0@0,0@1],Null]))))}),(g,{ChanAbst (bind [x] (Send "13" 0@0 (Seq "14" [ChanInst (TVar "CALL on Line 14" 1@2) [0@0],Null])))})]] (New "1" (-1) (bind a10 (New "" (-1) (bind b11 (New "" (-1) (bind b12 (Par "" [IChoice "" (Send "9" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [2@0,1@0]),Send "" 2@0 (ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),Recv "" 1@0 (IChoice "" (Send "9" 0@0 (ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0])) (ChanInst (TVar "CALL on Line 11" 3@1) [1@0,0@0])),ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],Buffer 0@0 (True,0,0),Buffer 1@0 (True,0,0),Buffer 2@0 (True,0,0)]))))))))




(New "1" (-1) (bind a12 
	(New "" (-1) (bind b13 
		(New "" (-1) (bind b14 
			(Par "" 
				[Recv "" 2@0 
					(IChoice "" 
						(Send "9" 1@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0,1@0])) 
						(ChanInst (TVar "CALL on Line 11" 3@1) [2@0,1@0])),
					Send "" 2@0 
						(ChanInst (TVar "CALL on Line 14" 3@2) [2@0]),
					Recv "" 1@0 
						(IChoice "" 
							(Send "9" 0@0 
								(ChanInst (TVar "CALL on Line 10" 3@1) [1@0,0@0])) 
							(ChanInst (TVar "CALL on Line 11" 3@1) [1@0,0@0])),
					ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],
					Buffer 0@0 (True,0,0),
					Buffer 1@0 (True,0,0),
					Buffer 2@0 (True,0,0)]))))))))







(New "1" (-1) (bind a7 
	(New "" (-1) (bind b8 
		(Par "" 
			[Send "" 1@0 
				(ChanInst (TVar "CALL on Line 14" 2@2) [1@0]),
			Send "" 0@0 
				(ChanInst (TVar "CALL on Line 10" 2@1) [1@0,0@0]),
			Recv "" 0@0 
				(New "5" (-1) (bind b9 
					(Par "" 
						[ChanInst (TVar "SPAWN on Line 6" 3@1) [1@0,0@0],
						ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],
						Buffer 0@0 (True,0,0)]))),
			Buffer 0@0 (True,0,0),
			Buffer 1@0 (True,0,0)]))))))






(New "1" (-1) (bind a8 
	(New "" (-1) (bind b9 
		(Par "" 
			[Send "" 1@0 (ChanInst (TVar "CALL on Line 14" 2@2) [1@0]),
			IChoice "" 
				(Send "9" 0@0 
					(ChanInst (TVar "CALL on Line 10" 2@1) [1@0,0@0])) 
				(ChanInst (TVar "CALL on Line 11" 2@1) [1@0,0@0]),
			Recv "" 0@0 
				(New "5" (-1) (bind b10 
					(Par "" 
						[ChanInst (TVar "SPAWN on Line 6" 3@1) [1@0,0@0],
						ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],
						Buffer 0@0 (True,0,0)]))),
			Buffer 0@0 (True,0,0),
			Buffer 1@0 (True,0,0)]))))))






 (New "1" (-1) (bind a10 
	(New "" (-1) (bind b11 
		(Par "" 
			[Send "" 1@0 
				(ChanInst (TVar "CALL on Line 14" 2@2) [1@0]),
			Recv "" 1@0 
				(IChoice "" 
					(Send "9" 0@0 
						(ChanInst (TVar "CALL on Line 10" 2@1) [1@0,0@0])) 
					(ChanInst (TVar "CALL on Line 11" 2@1) [1@0,0@0])),
			Recv "" 0@0 
				(New "5" (-1) (bind b12 
					(Par "" 
						[ChanInst (TVar "SPAWN on Line 6" 3@1) [1@0,0@0],
						ChanInst (TVar "SPAWN on Line 7" 3@0) [0@0],
						Buffer 0@0 (True,0,0)]))),
			Buffer 0@0 (True,0,0),
			Buffer 1@0 (True,0,0)]))))))





			
			
 (New "1" (-1) (bind a6 
	(Par "" 
		[Send "13\nSPAWN on Line 2\n" 0@0 
			(ChanInst (TVar "CALL on Line 14" 1@2) [0@0]),
		Recv "4\nSPAWN on Line 3\n" 0@0 
			(New "5" (-1) (bind b7 
				(Par "" 
					[ChanInst (TVar "SPAWN on Line 6" 2@1) [1@0,0@0],
					ChanInst (TVar "SPAWN on Line 7" 2@0) [0@0],
					Buffer 0@0 (True,0,0)]))),
		Buffer 0@0 (True,0,0)]))))

		
		
		
		
		
-------------------------FINITE STATE-------------------------		
		
		
EqnSys (bind [[(r1,{ChanAbst (bind [x,y] (New "5" 0 (bind c (Recv "6" 1@0 (Seq "7" [ChanInst (TVar "CALL on Line 7" 2@0) [1@0,0@0],Null])))))}),(w,{ChanAbst (bind [x] (Send "8" 0@0 (Send "9" 0@0 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0],Null]))))})]] 
(New "1" (-1) (bind a11 
	(New "" (-1) (bind c12 
		(Par "" 
			[Send "8\nCALL on Line 10\nSEND on line 9\n\tRECV on line 6\n\tCALL on Line 7\n\tRECV on line 6\n\t\tSEND on line 8\n\t\tSPAWN on Line 3\n\t\t\n\tSPAWN on Line 4\n\t\n\t\nSEND on line 8\n\tRECV on line 6\n\tSPAWN on Line 4\n\t\nSPAWN on Line 3\n\n" 1@0 
				(Send "9" 1@0 
					(ChanInst (TVar "CALL on Line 10" 2@1) [1@0])),
			Recv "6\nCALL on Line 7\nRECV on line 6\n\tSEND on line 9\n\tSEND on line 8\n\t\tRECV on line 6\n\t\tSPAWN on Line 4\n\t\t\n\tSPAWN on Line 3\n\t\nCALL on Line 7\nRECV on line 6\n\tSEND on line 8\n\tSPAWN on Line 3\n\t\nSPAWN on Line 4\n\n\n" 1@0 
				(ChanInst (TVar "CALL on Line 7" 2@0) [1@0,0@0]),
			Buffer 0@0 (True,0,0),
			Buffer 1@0 (True,0,0)]))))))


EqnSys (bind [[(r1,{ChanAbst (bind [x,y](New "5" 0 (bind c (Recv "6" 1@0 (Seq "7" [ChanInst (TVar "CALL on Line 7" 2@0) [1@0,0@0],Null])))))}),(w,{ChanAbst (bind [x] (Send "8" 0@0 (Send "9" 0@0 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0],Null]))))})]] 
(New "1" (-1) (bind a13 
	(New "" (-1) (bind c14 
		(Par "" 
			[Send "9\nSEND on line 8\n\tRECV on line 6\n\tSPAWN on Line 4\n\t\nSPAWN on Line 3\n" 1@0 
				(ChanInst (TVar "CALL on Line 10" 2@1) [1@0]),
			Recv "6\nCALL on Line 7\nRECV on line 6\n\tSEND on line 8\n\tSPAWN on Line 3\n\t\nSPAWN on Line 4\n\n" 1@0 
				(ChanInst (TVar "CALL on Line 7" 2@0) [1@0,0@0]),
			Buffer 0@0 (True,0,0),
			Buffer 1@0 (True,0,0)]))))))


EqnSys (bind [[(r1,{ChanAbst (bind [x,y] (New "5" 0 (bind c (Recv "6" 1@0 (Seq "7" [ChanInst (TVar"CALL on Line 7" 2@0) [1@0,0@0],Null])))))}),(w,{ChanAbst (bind [x] (Send "8" 0@0 (Send "9" 0@0 (Seq "10" [ChanInst (TVar "CALL on Line 10" 1@1) [0@0],Null]))))})]] 
(New "1" (-1) (bind a10 
	(New "2" (-1) (bind b11 
		(New "" (-1) (bind c12 
			(Par "" 
				[Send "8\nSPAWN on Line 3\n" 2@0 
					(Send "9" 2@0 (ChanInst (TVar "CALL on Line 10" 3@1) [2@0])),
					Recv"6\nSPAWN on Line 4\n" 2@0 
						(ChanInst (TVar "CALL on Line7" 3@0) [2@0,0@0]),
					Buffer 0@0 (True,0,0),
					Buffer 1@0 (True,0,0),
					Buffer 2@0 (True,0,0)]))))))))
 
 
 
 
 
 
 
 
 
 