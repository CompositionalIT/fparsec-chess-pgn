module Tests

open FParsec
open Expecto
open FParsecChess

let tryParse parser text  =
    match run parser text with
    | Success (result, _, _) -> Some result
    | Failure (_, _, _) -> None

[<Tests>]
let tests =
    testList "Parser" [
        testCase "Tag name" (fun _ ->
            let expectedTag = {
                Name = "Event"
                Value = "F/S Return Match"
            }
            Expect.equal (tryParse Parse.tag "[Event \"F/S Return Match\"]") (Some expectedTag) ""
        )
        testCase "Multi digit round number" (fun _ ->
            Expect.equal (tryParse Parse.roundNumber "12. e4") (Some 12) ""
        )
        testCase "A file" (fun _ ->
            Expect.equal (tryParse Parse.file "a") (Some A) ""
        )
        testCase "A rank" (fun _ ->
            Expect.equal (tryParse Parse.rank "4") (Some Four) ""
        )
        testCase "Invalid rank" (fun _ ->
            Expect.equal (tryParse Parse.rank "asdf") None ""
        )
        testCase "A square" (fun _ ->
            Expect.equal (tryParse Parse.square "b7") (Some (B, Seven)) ""
        )
        testCase "Rook piece" (fun _ ->
            Expect.equal (tryParse Parse.piece "R") (Some Rook) ""
        )
        testCase "Pawn piece" (fun _ ->
            Expect.equal (tryParse Parse.piece "") (Some Pawn) ""
        )
        testCase "Move" (fun _ ->
            Expect.equal (tryParse Parse.move "Rd8") (Some { Piece = Rook; Square = D, Eight }) ""
        )
        testCase "Pawn move" (fun _ ->
            Expect.equal (tryParse Parse.move "d8") (Some { Piece = Pawn; Square = D, Eight }) ""
        )
        testCase "Round" (fun _ ->
            let expectedRound = {
                RoundNumber = 3
                WhiteMove = { Piece = Pawn; Square = D, Eight }
                BlackMove = { Piece = Rook; Square = F, One }
            }
            Expect.equal (tryParse Parse.round "3. d8 Rf1") (Some expectedRound) ""
        )
        testCase "Rounds" (fun _ ->
            let expectedRounds = [
                {
                    RoundNumber = 9
                    WhiteMove = { Piece = Pawn; Square = D, Eight }
                    BlackMove = { Piece = Rook; Square = F, One }
                }
                {
                    RoundNumber = 10
                    WhiteMove = { Piece = Pawn; Square = D, Four }
                    BlackMove = { Piece = Knight; Square = D, Seven }
                }
                {
                    RoundNumber = 11
                    WhiteMove = { Piece = Pawn; Square = C, Four }
                    BlackMove = { Piece = Queen; Square = C, Six }
                }
            ]
            Expect.equal (tryParse Parse.rounds "9. d8 Rf1 10. d4 Nd7 11. c4 Qc6") (Some expectedRounds) ""
        )
        testCase "Game" (fun _ ->
            let expected = {
                Tags = [
                    { Name = "Tag1"; Value = "Value 1" }
                    { Name = "Tag2"; Value = "Value 2" }
                ]
                Rounds = [
                    {
                        RoundNumber = 9
                        WhiteMove = { Piece = Pawn; Square = D, Eight }
                        BlackMove = { Piece = Rook; Square = F, One }
                    }
                    {
                        RoundNumber = 10
                        WhiteMove = { Piece = Pawn; Square = D, Four }
                        BlackMove = { Piece = Knight; Square = D, Seven }
                    }
                ]
            }
            let text = """
[Tag1 "Value 1"]
[Tag2 "Value 2"]

9. d8 Rf1 10. d4 Nd7
            """
            Expect.equal (tryParse Parse.game (text.Trim())) (Some expected) ""
        )

    ]