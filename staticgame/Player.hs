-- | Write a report describing your design and strategy here.
module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards
import Prelude



notASuit :: Suit -> Card -> Bool
notASuit suit (Card cSuit _)  = suit /= cSuit

aSuit :: Suit -> Card -> Bool
aSuit suit (Card cSuit _)  = suit == cSuit

getSuitToPlay :: (Card, PlayerId) -> Suit
getSuitToPlay (Card suit _, _) = suit

playCard :: PlayFunc
playCard _ cards [] _ 
    | filter (== Card Club Two) cards == [Card Club Two] = (Card Club Two, "bullshit")
    | otherwise = 
        let possible = filter (notASuit Heart) cards
        in  if not (null possible)
                then (head possible, "bullshit")
                else (head cards, "bullshit")

playCard _ cards currP _ =
    let possible = filter (aSuit (getSuitToPlay (last currP))) cards 
    in if not (null possible)
        then (head possible, "bullshit")
        else 
            let poss = filter (notASuit Heart) cards
            in if not (null poss)
                then (head poss, "bullshit")
                else (head cards, "bullshit")

        
    


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
