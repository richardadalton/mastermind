// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

exception CanNotBeSolved

type Colour = Red|Orange|Yellow|Green|Blue
type Answer = {Black: int; White: int}

// Store the colours in a list so we can iterate over them to generate guesses
let ColourList = [Red; Orange; Yellow; Green; Blue]

// E.g.
// code     = Red, Orange, Yellow, Blue
// guess    = Yellow, Yellow, Green, Blue
// expected = {Black = 1 (Blue); White = 1 (Yellow)}
let check code guess =
    let IsMatch t = fst t = snd t

    // right = [(Blue, Blue)]
    // wrong = [(Red, Yellow); (Orange, Yellow); (Yellow, Green)]
    let right, wrong =
        List.zip code guess
        |> List.partition IsMatch
 
    // Number of Black Pegs
    // 1 (Blue, Blue)
    let rightColourRightPosition =
        List.length right

    // Number of White Pegs
    // wrongCode  = [Red; Orange; Yellow]
    // wrongGuess = [Yellow; Yellow; Green]
    let wrongCode, wrongGuess = List.unzip wrong 

    // E.g. when colour = Yellow, result = 2
    let howManyOfThisColourOutOfPlace colour =
        wrongGuess
        |> List.filter(fun c -> c = colour)
        |> List.length

    // Number of White Pegs
    // 1 (Yellow) Although Yellow is guessed twice, there is only one Yellow in the code, so result is 1
    let rightColourWrongPosition =
        wrongCode                                                                          // [Red; Orange; Yellow]
        |> Seq.countBy(id)                                                                 // seq [(Red, 1); (Orange, 1); (Yellow, 1)]
        |> Seq.map (fun group -> (snd group, howManyOfThisColourOutOfPlace (fst group)))   // seq [(1, 1); (1, 0); (1, 2)] (fst is occurences in code, snd is occurences in guess)
        |> Seq.sumBy Math.Min                                                              // For each colour, sum the lesser of occurences in code and in guess

    {Black = rightColourRightPosition; White = rightColourWrongPosition}

// Yields every possible combination of 4 colours from the 5 available
let possibleCodes =
    seq {
            for i in ColourList do
                for j in ColourList do
                    for k in ColourList do
                        for l in ColourList do
                            yield [i;j;k;l]
    }
    |> List.ofSeq

// The solve function is passed a checkFunction, which is a clojure over the secret code.
// Guesses can be tried againts the checkFunction, and the feedback used to filter guesses.    
let solve checkFunction =
    
    let filterPossibilities possibilities guess =
        let answer = checkFunction guess
        possibilities
        |> List.filter (fun potential -> (check guess potential) = answer)

    let rec solve_iter possible =
        match possible with
        | head::[] -> head
        | head::_ -> solve_iter (filterPossibilities possible head)
        | _ -> raise CanNotBeSolved

    solve_iter possibleCodes
    
    
[<EntryPoint>]
let main argv =
    // Capture the secret code in the clojure solveFunction
    let solveFunction = check [Red; Orange; Yellow; Green]

    // Pass solveFunction to solve, it will uses it to try guesses
    let result = solve solveFunction
    
    printfn "%A" result
    0 // return an integer exit code