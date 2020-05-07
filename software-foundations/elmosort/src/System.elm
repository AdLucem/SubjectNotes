module System exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Browser
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import List exposing (filter, take, drop, head, tail, map)
import Tuple exposing (first, second)

{- define the data structures -}

type Selections
    = NoneSelected
    | OneSelected Int
    | BothSelected Int Int

type Msg = ArrayItemClicked Int | Partition | Next 

type alias Model =
    { array : List Int
    , stack : List (Int, Int)
    , selections : Selections
    }


-- functions defined on the data structure

getIndex : List Int -> Int -> Int -> Int
getIndex ls a i = 
  let
    top = case (head ls) of
            Nothing -> -1
            Just x -> x
    rest = case (tail ls) of
            Nothing -> []
            Just xs -> xs
  in
    if (top == -1) then -1
    else if (top == a) then i
    else (getIndex rest a (i+1)) 
      
slice : List Int -> Int -> Int -> List Int
slice ls i j = drop i (take (j+1) ls)

separate : List Int -> Int -> List Int
separate ls pivot = 
  let
    before = filter ((>) pivot) ls
    after = filter ((<) pivot) ls
  in
    before ++ [pivot] ++ after

partition : List Int -> Int -> Int -> Int -> List Int
partition ls start end pivot = 
  if (ls == []) then [] else
  let
    pre = take start ls
    post = drop (end+1) ls
    ls_ = slice ls start end
  in
    pre ++ (separate ls_ pivot) ++ post


condPre : Int -> Int -> Int -> List (Int, Int)
condPre i start end = 
  if ((i-1) >= start) 
  then [(start, i-1)]
  else []
condPost : Int -> Int -> Int -> List (Int, Int)
condPost i start end =
  if ((i+1) <= end)
  then [(i+1, end)]
  else []


appendStack : List (Int, Int) 
           -> List Int 
           -> Int 
           -> Int
           -> Int
           -> List (Int, Int)
appendStack stack ls start end pivot = 
  let
    i = getIndex ls pivot 0
  in
   (condPre i start end) ++ (condPost i start end) ++ stack
    
    

next : List Int -> List (Int, Int) -> (List Int, List (Int, Int))
next ls stack = 
  let 
    top = case (head stack) of
            Nothing -> (-1, -1)
            Just x -> x
    rest = case (tail stack) of
            Nothing -> []
            Just xs -> xs
    s = first top
    e = second top
    pivot = case (head ls)  of
              Nothing -> -10000
              Just y -> y 
    ls_ = partition ls s e pivot
    stack_ = appendStack rest ls_ s e pivot
  in
    if (top == (-1, -1)) then (ls, []) else
    if (pivot == -10000) then ([], []) else
    (ls_, stack_)

partHelper : Model -> Selections -> Model
partHelper model selec =
    let {array, stack, selections} = model
    in
        case selec of
            NoneSelected -> model
            OneSelected i -> model
            BothSelected i j ->
                let
                    pivot = case (head array) of
                                Nothing -> -1
                                Just x -> x
                    array_ = partition array i j pivot
                in
                    { model | array = array_ }


select : Int -> Selections -> Selections
select i selections =
    case selections of
        NoneSelected ->
            OneSelected i
        OneSelected j ->
            BothSelected j i
        BothSelected j k ->
            BothSelected j k


deselect : Int -> Selections -> Selections
deselect i selections =
    case selections of
        NoneSelected ->
            NoneSelected
        OneSelected j ->
            if (i == j) then 
                NoneSelected
            else
                OneSelected j
        BothSelected j k ->
            if (i == j) then
                OneSelected k
            else
                if (i == k) then
                    OneSelected j
                else (BothSelected j k)

isSelected : Int -> Selections -> Bool
isSelected index selections =
    case selections of
        NoneSelected ->
            False
        OneSelected j ->
            if j == index then True else False
        BothSelected j k ->
            if (j == index || k == index) then True else False

{- initialize the data structures -}
init_b1 =
    { array = [30, 24, 56, 5, 0]
    , stack = [(0, 4)]
    , selections = NoneSelected
    }

update: Msg -> Model -> Model
update msg model =
    let {array, stack, selections} = model
    in
        case msg of
            ArrayItemClicked idx ->
                if (isSelected idx selections) then
                    { model | selections = deselect idx selections }
                else
                    { model | selections = select idx selections }
            Partition ->
                partHelper model selections
            Next -> 
                let new = next array stack
                in
                    { model | array = first new, stack = second new }

        
view: Model -> Html Msg
view model =
    let {array, stack, selections} = model
        fstStack = map (\t -> first t) stack
        sndStack = map (\t -> second t) stack
    in
        Html.div
            [ HA.style "border" "1px solid black"
            , HA.style "margin" "auto"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "width" "max-content"
            ]
            [
             Svg.svg
                 [ SA.width "1000px"
                 , SA.height "500px"
                 , SA.viewBox "0 0 500 500"
                 ]
                 [ viewArray array selections
                 ]
            , viewControls selections
            , text (Debug.toString stack)                 
            ]

            
viewArray: List Int -> Selections -> Svg Msg
viewArray array selections =
    let arrayYPos = 250.0
        arrayXPos = 50.0
        itemRadius = 30.0                     
    in
        Svg.g
            []
            (List.indexedMap (\i a ->
                                  Svg.g
                                  [SE.onClick (ArrayItemClicked i)]
                                  [ Svg.circle
                                        [ SA.cx (String.fromFloat (arrayXPos + (toFloat i) * 2.5 * itemRadius))
                                        , SA.cy (String.fromFloat arrayYPos)
                                        , SA.r (String.fromFloat itemRadius)
                                        , SA.fill (if (isSelected i selections) then "rgb(200, 200, 200)" else "rgb(100, 100, 100)")
                                        ]
                                        []
                                  , Svg.text_
                                      [ SA.x (String.fromFloat (arrayXPos + (toFloat i) * 2.5 * itemRadius))
                                      , SA.y (String.fromFloat arrayYPos)
                                      , SA.fontSize "30"
                                      , SA.fontWeight "bold"
                                      , SA.fill "rgb(0,0,0)"
                                      , SA.textAnchor "middle"
                                      , SA.dominantBaseline "middle"
                                      ]
                                        [a |> String.fromInt |> Svg.text]
                                  ]
                             ) array
            )


viewControls : Selections -> Html Msg
viewControls selections =
    case selections of
        NoneSelected ->
            div [] 
            [ button [HE.onClick Next] [text "next"]]
        OneSelected i ->
            div [] 
            [ button [HE.onClick Next] [text "next"]]
        BothSelected i j ->
            div []
            [ button [HE.onClick Partition] [text "partition"],
              button [HE.onClick Next] [text "next"] ]
            

main = Browser.sandbox
       { init = init_b1
       , update = update
       , view = view
       }