// Define Cuisine
type CuisineType =
    | KoreanCuisine
    | TurkishCuisine

// Define MovieType
type CinemaExperience =
    | Standard
    | IMAX
    | DBOX
    | StandardWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

// Define Activity
type LeisureActivity =
    | BoardGameActivity
    | Relax
    | MovieExperience of CinemaExperience
    | DiningExperience of CuisineType
    | RoadTrip of int * float

// Calculate the Budget
let calculateActivityCost activity =
    match activity with
    | BoardGameActivity -> 0.0
    | Relax -> 0.0
    | MovieExperience experience ->
        match experience with
        | Standard -> 12.0 * 2.0
        | IMAX -> 17.0 * 2.0
        | DBOX -> 20.0 * 2.0
        | StandardWithSnacks -> (12.0 + 5.0) * 2.0
        | IMAXWithSnacks -> (17.0 + 5.0) * 2.0
        | DBOXWithSnacks -> (20.0 + 5.0) * 2.0
    | DiningExperience cuisine ->
        match cuisine with
        | KoreanCuisine -> 70.0
        | TurkishCuisine -> 65.0
    | RoadTrip (distance, fuelRate) -> float distance * fuelRate

// Define a list of activities
let plannedActivities = [
    BoardGameActivity
    Relax
    MovieExperience Standard
    MovieExperience IMAXWithSnacks
    DiningExperience KoreanCuisine
    RoadTrip (100, 0.15)
]

// Calculate the cost for each activity
let activityCosts = plannedActivities |> List.map (fun activity ->
    let cost = calculateActivityCost activity
    printfn "Cost for %A: %.2f CAD" activity cost
    cost
)

// Calculate the total budget
let totalCost = List.sum activityCosts

printfn "Total Budget: %.2f CAD" totalCost
