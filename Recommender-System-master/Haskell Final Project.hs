--This is the final project 
--Edited by : Hoda Amr Elhemaly
--Concepts of Programming Languages
-- Recommender System using HASKELL
--In this project, We build a recommender system that uses the users’ purchases history to:
--    Recommend an item to the user based on their previous purchases if the current cart is empty
--    If there are items already added to the users cart, recommend an item to the user based on their previous purchases and the current items in the cart
--    Another option is to recommend an item to the user based on the intersection between the items they previously purchased and other users’ purchases


import System.Random
import System.IO.Unsafe

users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory =  [
                        ("user1", [["item1", "item2", "item3"], ["item1", "item2", "item4"]]),
                        ("user2", [["item2", "item5"], ["item4", "item5"]]),
                        ("user3", [["item3", "item2"]]),
                        ("user4", [])
                    ]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

--createEmptyFreqList ::(String) -> [(String,[])]
createEmptyFreqList []=[]
createEmptyFreqList (x:xs)= (x,[]):createEmptyFreqList xs

---------------------------------------------basic helpers --------------------------------------------

flatten []=[]
flatten [[a]]=[a]
flatten (x:xs) = x++ flatten xs

occursIn x []= False
occursIn x (x1:xs)= if x==x1 then True
else occursIn x xs
removeduplicates []=[]
removeduplicates (x1:x2)= if occursIn x1 x2 then removeduplicates x2 else x1:removeduplicates x2 	
finalList []=[]
finalList (x1:x2)= removeduplicates (flatten (x1:x2)) 
	

-- getting list of purchases that user bought	:				
getList x []= [] 									
getList x ((xs,x1):x2)= if  x==xs then x1
else getList x x2

-- counting how many items were bought with that item  : 
countsamelist []=0
countsamelist (x1:x2)= (length x1 -1)	
counthelper [] item number= number
counthelper (x1:x2) item number= if occursIn item x1 then countsamelist (x1:x2) + counthelper x2 item number else counthelper x2 item number		
count [] []=[]		
count (xs:xz) []=[] 	
count (xs:xz) (x1:x2)  = (x1, counthelper (xs:xz) x1 0 ): count (xs:xz) x2 

-- frequency list items: 
freqListItems "" = error "enter a user"
freqListItems user=if occursIn user users then count (getList user purchasesHistory) (finalList (getList user purchasesHistory)) 
else error "enter a valid user"
									
-- get One User Stat :

--Take all purchases of user and items and get a list with all the items with each item baught with other item how many times
getOneUserStat user= getOneUserStatHelper user (createEmptyFreqList items)
getOneUserStatHelper user []=[]

getOneUserStatHelper user ((xs,xy):xz)= (xs, (allhowmanytogether xs items (finalList(getList user purchasesHistory)) user  )):getOneUserStatHelper user xz 

-- get how many together item with all the items 
allhowmanytogether item _ [] user=[]
allhowmanytogether item  [] (xs:xy) user =[]

allhowmanytogether item (x1:x2) (xs:xy) user =if occursIn x1 (xs:xy) then (if item==x1 then allhowmanytogether item x2 (finalList(getList user purchasesHistory)) user else( if howmanytogether item x1 (getList user purchasesHistory) 0 ==(x1,0)then (allhowmanytogether item x2 (finalList(getList user purchasesHistory)) user)else (howmanytogether item x1 (getList user purchasesHistory) 0):allhowmanytogether item x2 (finalList(getList user purchasesHistory)) user) )else allhowmanytogether item x2 (finalList(getList user purchasesHistory)) user
 
-- how many times the item is together with another item
howmanytogether item1 item2 [] n= (item2,n)						
howmanytogether item1 item2 (x1:x2) n =if occursIn item1 x1 then (if occursIn item2 x1 then howmanytogether item1 item2 x2 (n+1) else howmanytogether item1 item2 x2 n) else howmanytogether item1 item2 x2 n

-- get the frequency list of the user from the getAllUsersStats
getCart user= getCartHelper user (getAllUsersStats user)
getCartHelper user ((x1,x2):x3) = if x1==user then x2 else getCartHelper user x3 

--get list of all the items from the entered 
--toaskforitems user= toaskforitemshelper user (getCart user)
freqListCart user []=[]
freqListCart user (x1:x2)= toaskforitem x1 user ++ freqListCart user x2

-- get the list of one item from the list that is entered 
toaskforitem item user= askforitem item (getCart user) 
askforitem item []=[]
askforitem item ((x1,x2):x3)=if item == x1 then x2 else askforitem item x3 

--get All Users Stat :
getAllUsersStats l= getAllUsersStatsHelper users 
getAllUsersStatsHelper [] = [] 
getAllUsersStatsHelper (x1:x2) = (x1,getOneUserStat x1): getAllUsersStatsHelper x2
 
