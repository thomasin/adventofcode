module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (div)
import Http
import String.Extra exposing (leftOf, rightOf)
import Dict.Extra
import Dict exposing (Dict)
import List.Extra
import Time exposing (Time)

--


getActions : Cmd Msg
getActions =
  Http.get
    { url = "../src/04.txt"
    , expect = Http.expectString GotInput
    }


--


initialModel =
  { shifts = []
  }


type alias Model =
  { shifts : Shifts
  }


--type Shift = Shift Time String ( List (Time, String) )
type Shift = Shift Time String Sleeps
type Sleep = Sleep Int Int

type alias Shifts = List Shift
type alias Sleeps = List Sleep

type Msg = GotInput (Result Http.Error String)

main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getActions )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInput ( Err _ ) -> ( model, Cmd.none )
        GotInput ( Ok input ) ->
            String.lines input
            |> List.map turnIntoAction
            |> List.filterMap identity
            |> List.sortWith sortAction
            |> groupByShifts []
            |> List.filterMap ( turnIntoShift [] )
            |> (\s -> ( { model | shifts = s }, Cmd.none ))


groupByShifts : List { shift : ( Time, String ), times: List ( Time, String ) } -> List ( Time, String ) -> List { shift : ( Time, String ), times: List ( Time, String ) }
groupByShifts shifts actions =
  case actions of
    [] -> shifts
    ((date, action)::xs) ->
      if String.contains "Guard" action then
        groupByShifts ({shift = (date, action), times = []}::shifts) xs
      else
        case shifts of
          [] -> groupByShifts shifts xs
          (shift::ys) -> groupByShifts ({shift | times = (date, action)::shift.times } ::ys) xs


turnIntoShift : ( List Sleep ) -> { shift : ( Time, String ), times: List ( Time, String ) } -> Maybe Shift
turnIntoShift sleeps { shift, times } =
  case List.sortWith sortAction times of
    [] -> Just <| Shift (Tuple.first shift) (getGuardId <| Tuple.second shift) sleeps
    x::[] -> Nothing
    x::y::xs ->
      case sleepFromParts (sleepPartFromString x) (sleepPartFromString y) of
        Just sleep -> turnIntoShift (sleep::sleeps) { shift = shift, times = xs }
        Nothing -> turnIntoShift sleeps { shift = shift, times = xs }


guardWhoSleptMost shifts =
  totalSleptByGuard shifts
  |> Dict.toList
  |> List.Extra.maximumBy Tuple.second
  |> Maybe.map Tuple.first
  |> Maybe.andThen ( minuteMostSleptByGuard shifts )

totalSleptByGuard : Shifts -> Dict String Int
totalSleptByGuard shifts =
  List.map (\(Shift _ guardId sleeps) -> (guardId, sumSleeps sleeps)) shifts
  |> Dict.Extra.fromListDedupe (\a b -> a + b)


minuteMostSleptByGuard : Shifts -> String -> Maybe String
minuteMostSleptByGuard shifts guardId =
  List.filter ((==) guardId << guardIdFromShift ) shifts
  |> getAllSleeps
  |> List.concatMap getMinutesAsleep
  |> List.Extra.gatherEquals
  |> List.Extra.maximumBy ( List.length << Tuple.second )
  |> Maybe.map Tuple.second
  |> Maybe.andThen List.head
  |> Maybe.map ( \m -> "guardId: " ++ guardId ++ ", minute: " ++ (String.fromInt m) )

guardIdFromShift (Shift _ guardId _) = guardId

getAllSleeps shifts =
  List.concatMap getSleeps shifts

getMinutesAsleep (Sleep sleptAt duration) = List.range sleptAt ( sleptAt + duration - 1 )

sumSleeps sleeps =
  List.map (\(Sleep _ duration) -> duration) sleeps
  |> List.sum

getSleeps (Shift _ _ sleeps) = sleeps

getGuardId =
  leftOf " " << rightOf "#"


--addAction ( Shift d id actions ) action =
--  let
--    sleep = case actions of
--      [] -> 
--  Shift d id ( List.sortWith sortAction actions )


sleepFromParts : SleepPart -> SleepPart -> Maybe Sleep
sleepFromParts part1 part2 =
  case part1 of
    FallsAsleep asleepAt ->
      case part2 of
        WakesUp wakesUpAt -> Just <| Sleep ( Time.minute asleepAt ) ( ( Time.minute wakesUpAt ) - ( Time.minute asleepAt ) ) 
        FallsAsleep _ -> Nothing
    WakesUp wakesUpAt ->
      case part2 of
        FallsAsleep asleepAt -> Just <| Sleep ( Time.minute asleepAt ) ( ( Time.minute wakesUpAt ) - ( Time.minute asleepAt ) ) 
        WakesUp _ -> Nothing


sleepPartFromString : ( Time, String ) -> SleepPart
sleepPartFromString ( time, string ) =
  if String.contains "falls asleep" string then
    FallsAsleep time
  else
    WakesUp time

type SleepPart = FallsAsleep Time | WakesUp Time


sortAction : ( Time, String ) -> ( Time, String ) -> Order
sortAction ( d1, _ ) ( d2, _ ) = Time.sort d1 d2


turnIntoAction : String -> Maybe ( Time, String )
turnIntoAction str =
    Maybe.map (\d -> ( d, rightOf "]" str ) ) ( Time.fromString str )


view : Model -> Document Msg
view model =
    Document
        "..."
        [ Html.div [] [ Html.text ( "biggest snoozer: " ++ ( Maybe.withDefault "none" <| guardWhoSleptMost model.shifts ) ) ]
        , Html.div [] ( List.map displayShift model.shifts )
        ]


displayShift (Shift date guardId sleeps) =
  Html.div []
    ( (++)
      [ Html.span [] [ Html.text ( "date: " ++ ( Time.toString date ) ) ]
      , Html.span [] [ Html.text ( " guard: " ++ guardId ) ]
      ] ( List.map displaySleep sleeps ) )


displaySleep (Sleep sleptAt sleptFor) =
  Html.div []
    [ Html.text ( "slept at: " ++ ( String.fromInt sleptAt ) )
    , Html.text ( "slept for: " ++ ( String.fromInt sleptFor ) )
    ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
