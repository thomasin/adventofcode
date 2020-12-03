module Time exposing (Time, sort, fromString, toString, minute)

import String.Extra exposing (leftOf, rightOf)

--


sort t1 t2 = unwrap2 sortTime t1 t2


type Time = Time Parts


type alias Parts =
  { year : Int
  , month : Int
  , day : Int
  , hour : Int
  , minute : Int
  }


unwrap2 f ( Time a ) ( Time b ) = f a b

sortTime : Parts -> Parts -> Order
sortTime d1 d2 =
  compare ( .minute d1 ) ( .minute d2 )
  |> compareField .hour d1 d2
  |> compareField .day d1 d2
  |> compareField .month d1 d2


compareField : ( Parts -> Int ) -> Parts -> Parts -> Order -> Order
compareField key d1 d2 with =
  case compare ( key d1 ) ( key d2 ) of
    LT -> LT
    GT -> GT
    EQ -> with


toString : Time -> String
toString ( Time parts ) =
  ( String.fromInt ( .month parts ) )
  ++ "-" ++ ( String.fromInt ( .day parts ) )
  ++ " " ++ ( String.fromInt ( .hour parts ) )
  ++ ":" ++ ( String.fromInt ( .minute parts ) )


minute ( Time parts ) = .minute parts


fromString : String -> Maybe Time
fromString str =
  Maybe.map5 Parts
    ( timePortion "[" "-" str )
    ( timePortion "-" "-" str )
    ( timePortionSecond "-" " " str )
    ( timePortion " " ":" str )
    ( timePortion ":" "]" str )
  |> Maybe.map Time


timePortion str1 str2 =
   String.toInt << middleOf str1 str2

timePortionSecond str1 str2 =
  String.toInt << rightOf str1 << middleOf str1 str2

middleOf str1 str2 = leftOf str2 << rightOf str1
