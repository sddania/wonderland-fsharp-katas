// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

type Round =
    | Won
    | Lost  // the round cannot match because the hands cannot be never the same. 

type Game =
    | Win
    | Lose
    | NotFinished

let playRound (card1: Card, card2: Card) : Round =
    let (suit1, rank1) = card1
    let (suit2, rank2) = card2

    if (suit1 > suit2 || (suit1 = suit2 && rank1 > rank2)) then
        Won
    else
        Lost

let rec playGame (hand1: Card list, hand2: Card list) : Game =
    if hand1.Length = 0 then
        Lose
    elif hand2.Length = 0 then
        Win
    else
        match (playRound (hand1.Head, hand2.Head)) with
        | Won -> playGame ((hand1 @ [ hand2.Head ]), hand2.Tail)
        | Lost -> playGame (hand1.Tail, (hand2 @ [ hand1.Head ]))


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // playRound
    printfn "the highest rank wins the cards in the round"
    test <@ playRound ((Heart, Ace), (Diamond, Ace)) = Won @>
    test <@ playRound ((Heart, Ace), (Heart, Value(3))) = Won @>

    printfn "queens are higher rank than jacks"
    test <@ playRound ((Heart, Queen), (Heart, Jack)) = Won @>
    test <@ playRound ((Heart, Jack), (Heart, Queen)) = Lost @>

    printfn "kings are higher rank than queens"
    test <@ playRound ((Heart, King), (Heart, Queen)) = Won @>
    test <@ playRound ((Heart, Queen), (Heart, King)) = Lost @>

    printfn "aces are higher rank than kings"
    test <@ playRound ((Heart, Ace), (Heart, King)) = Won @>
    test <@ playRound ((Heart, King), (Heart, Ace)) = Lost @>

    printfn "if the ranks are equal, clubs beat spades"
    test <@ playRound ((Club, Ace), (Spade, Ace)) = Won @>

    printfn "if the ranks are equal, diamonds beat clubs"
    test <@ playRound ((Diamond, Ace), (Club, Ace)) = Won @>

    printfn "if the ranks are equal, hearts beat diamonds"
    test <@ playRound ((Heart, Ace), (Diamond, Ace)) = Won @>

    // playGame
    printfn "the player loses when they run out of cards"
    test <@ playGame ([], []) = Lose @>
    test <@ playGame ([ (Heart, Ace) ], []) = Win @>


// run the tests
tests ()
