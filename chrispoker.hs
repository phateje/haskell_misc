module Main
	where
import IO
import Random
import Char
import List
money = 1000
cardsLeft = 52
cards = [("club", 2), ("club", 3), ("club", 4), ("club", 5), ("club", 6), ("club", 7), ("club", 8), ("club", 9), ("club", 10), ("club", 11), ("club", 12), ("club", 13), ("club", 14), 
		 ("diamond", 2), ("diamond", 3), ("diamond", 4), ("diamond", 5), ("diamond", 6), ("diamond", 7), ("diamond", 8), ("diamond", 9), ("diamond", 10), ("diamond", 11), ("diamond", 12), ("diamond", 13), ("diamond", 14),
		 ("heart", 2), ("heart", 3), ("heart", 4), ("heart", 5), ("heart", 6), ("heart", 7), ("heart", 8), ("heart", 9), ("heart", 10), ("heart", 11), ("heart", 12), ("heart", 13), ("heart", 14), 
		 ("spade", 2), ("spade", 3), ("spade", 4), ("spade", 5), ("spade", 6), ("spade", 7), ("spade", 8), ("spade", 9), ("spade", 10), ("spade", 11), ("spade", 12), ("spade", 13), ("spade", 14)]
main = do
	putStrLn "Welcome to Straight Poker Casino. Have fun!"
	putStrLn ("MONEY: " ++ (show money))
	get_random 52 cards [] 10 -> tenRandomCards
	set_player_cards tenRandomCards [] -> myCards
	let myCardsSorted = sortCards myCards
	putStrLn ("MY CARDS: \n" ++ (show myCardsSorted) ++ "\n")
	bet <- get_bet
	putStrLn ("Your bet is: " ++ (show bet))
	set_dealer_cards tenRandomCards [] -> dealersCards
	let dealersCardsSorted = sortCards dealersCards
	putStrLn ("DEALERS CARDS: \n" ++ (show dealersCardsSorted) ++ "\n")
	-- this is a boolean value, shows if the player won
	winner <- check_hand myCardsSorted dealersCardsSorted  
	putStrLn ("Did i win? " ++ (show winner))

-- **GETS 10 RANDOM CARDS FROM THE DECK, PUTS THEM IN A LIST OF 10**
get_random numberOfList list taken count = do
                  if count == 0 then return taken
                     else do
                        rand <- randomRIO (0, numberOfList-1)
                        let chosenCard = getCardAt list rand
                        let tempList = delete chosenCard list
                        get_random (numberOfList-1) tempList (chosenCard : taken) (count-1)

                        
-- ***** FUNCTIONS TO GET CARDS FROM THE RANDOM GENERATED LIST OF 10, AND PUT THEM INTO TWO HANDS ******
set_player_cards list myCards = do
	let myCards2 = ((getCardAt list 0) : (getCardAt list 1) : (getCardAt list 2) : (getCardAt list 3) : (getCardAt list 4) : myCards)		  
	return myCards2
set_dealer_cards list myCards = do
	let myCards2 = ((getCardAt list 5) : (getCardAt list 6) : (getCardAt list 7) : (getCardAt list 8) : (getCardAt list 9) : myCards)		  
	return myCards2
check_highCard player dealer = if (snd (head player)) == (snd (head dealer))
						then (check_highCard (tail player) (tail dealer))
						else (snd (head player)) > (snd (head dealer))
check_pair hand = if (snd (head hand)) == (snd (head (tail hand)))
				then (snd (head hand))
				else
					if length(hand) <= 2
						then (0)
						else
							(check_pair (tail hand))
check_hand myCardsSorted dealersCardsSorted = do
		let highCardBool = check_highCard myCardsSorted dealersCardsSorted
		putStrLn ("Did I have the highest car???  " ++ (show highCardBool))
		let myPair = check_pair myCardsSorted
		putStrLn ("MY BEST PAIR: " ++ (show myPair))
		let dealersPair = check_pair dealersCardsSorted
		putStrLn ("DEALERS BEST PAIR: " ++ (show dealersPair))
		let myPairWins = ((myPair > dealersPair) || (myPair == dealersPair && highCard))
		let result = (myPairWins : highCardBool : [])
		return result
-- ** UHHHHH**
getCardAt xs x = xs!!x
get_bet = do
	hSetBuffering stdin LineBuffering
	putStrLn "Please enter your bet: ?"
	bet <- getLine
	return (read bet :: Int)
sortCards [] = []
sortCards (x:xs) = sortCards [y | y <-xs, (snd y) > (snd x)]
		++ [x]
		++ sortCards [y | y <-xs, (snd y) <= (snd x)]