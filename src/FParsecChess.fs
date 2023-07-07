module FParsecChess

type Tag = {
    Name : string
    Value : string
}
    
type Piece =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn

type File = 
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H

type Rank = 
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight

type Square = File * Rank

type Move = {
    Piece  : Piece
    Square : Square
}

type Round = {
    RoundNumber : int
    WhiteMove   : Move
    BlackMove   : Move
}

type Game = {
    Tags : Tag list
    Rounds : Round list
}

[<RequireQualifiedAccess>]
module Parse =
    open FParsec

    let doubleQuote = pchar '"'

    let tag : Parser<Tag, unit> =
        pchar '[' >>. many1CharsTill anyChar spaces1 .>> doubleQuote .>>. manyCharsTill anyChar doubleQuote .>> pchar ']'
        >>= (fun (a, b) -> preturn { Name = a; Value = b })

    let roundNumber = pint32 .>> skipChar '.'

    let file = 
        choice [
            pchar 'a' >>% A
            pchar 'b' >>% B
            pchar 'c' >>% C
            pchar 'd' >>% D
            pchar 'e' >>% E
            pchar 'f' >>% F
            pchar 'g' >>% G
            pchar 'h' >>% H
        ]

    let rank =
        choice [
            pchar '1' >>% One
            pchar '2' >>% Two
            pchar '3' >>% Three
            pchar '4' >>% Four
            pchar '5' >>% Five
            pchar '6' >>% Six
            pchar '7' >>% Seven
            pchar '8' >>% Eight
        ]
    
    let square = file .>>. rank

    let piece =
        choice [
            pchar 'N' >>% Knight
            pchar 'B' >>% Bishop
            pchar 'R' >>% Rook
            pchar 'Q' >>% Queen
            pchar 'K' >>% King
        ] <|>% Pawn

    let move = piece .>>. square >>= (fun (piece, square) -> preturn { Piece = piece; Square = square })

    let round =
        tuple3 (roundNumber .>> spaces1) (move .>> spaces1) move
        >>= (fun (round, move1, move2) -> preturn { RoundNumber = round; WhiteMove = move1; BlackMove = move2 })

    let rounds : Parser<Round list, unit> = sepBy round spaces1

    let game =
        sepEndBy tag newline .>> many1 newline .>>. rounds
        >>= (fun (tags, rounds) -> preturn { Tags = tags; Rounds = rounds })

