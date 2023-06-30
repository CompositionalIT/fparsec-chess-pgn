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

    let space = skipChar ' '
    let doubleQuote = pchar '"'

    let tag : Parser<Tag, unit> =
        pchar '[' >>. many1CharsTill anyChar space .>> doubleQuote .>>. manyCharsTill anyChar doubleQuote .>> pchar ']'
        >>= (fun (a, b) -> { Name = a; Value = b } |> preturn)

    let period = skipChar '.'

    let roundNumber = pint32 .>> period

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
        tuple3 (roundNumber .>> space) (move .>> space) move
        >>= (fun (round, move1, move2) -> preturn { RoundNumber = round; WhiteMove = move1; BlackMove = move2 })

    let rounds : Parser<Round list, unit> = sepBy round space

    let game =
        sepEndBy tag newline .>> many newline .>>. rounds
        >>= (fun (tags, rounds) -> preturn { Tags = tags; Rounds = rounds })

