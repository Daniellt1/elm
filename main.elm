module Main exposing (..)

import Html exposing (..)
import RemoteData
import Http
import Json.Decode as Decode
import Dict


-- MODEL --


type alias Model =
    { providers : Maybe (Dict.Dict String Provider)
    , hotels : RemoteData.WebData (List Hotel)
    }


type alias ProviderUrl =
    ( String, String )


type alias ProviderData =
    { hotels : List Hotel }


type alias Provider =
    { name : String
    , url : String
    , data : RemoteData.WebData ProviderData
    }


type alias Hotel =
    { name : String }


init : ( Model, Cmd Msg )
init =
    ( Model (Just Dict.empty) RemoteData.Loading, fetchProvidersUrls )



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Providers: "
            ]
        , div [] [ viewHotels model.hotels ]
        ]


viewHotels : RemoteData.WebData (List Hotel) -> Html Msg
viewHotels hotels =
    case hotels of
        RemoteData.NotAsked ->
            text "Initialising."

        RemoteData.Loading ->
            text "Loading."

        RemoteData.Failure err ->
            text ("Error: " ++ toString err)

        RemoteData.Success hotelsItems ->
            div [] (List.map viewHotel hotelsItems)


viewHotel : Hotel -> Html Msg
viewHotel hotel =
    div []
        [ text ("Hotel name: " ++ hotel.name) ]



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE --


type Msg
    = ProvidersUrlsFetched (RemoteData.WebData (List ProviderUrl))
    | ProviderDataFetched String (RemoteData.WebData ProviderData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProvidersUrlsFetched response ->
            let
                cmds : List (Cmd Msg)
                cmds =
                    case response of
                        RemoteData.Success providersUrls ->
                            List.map (\p -> Tuple.second p) providersUrls
                                |> List.map fetchProviderData

                        _ ->
                            []

                providers : Maybe (Dict.Dict String Provider)
                providers =
                    case response of
                        RemoteData.Success providersUrls ->
                            Just <|
                                Dict.fromList <|
                                    List.map createProviderTuple providersUrls

                        _ ->
                            Nothing
            in
                { model | providers = providers }
                    ! cmds

        ProviderDataFetched url response ->
            ({ model
                | hotels = updateHotels model.hotels (hotelsFromProviderDataResponse response)
             }
            )
                ! []


createProviderTuple : ProviderUrl -> ( String, Provider )
createProviderTuple ( name, url ) =
    ( name, Provider name url RemoteData.Loading )


createProviderUrl : String -> String -> ProviderUrl
createProviderUrl name url =
    ( name, url )


hotelsFromProviderDataResponse : RemoteData.WebData ProviderData -> RemoteData.WebData (List Hotel)
hotelsFromProviderDataResponse providerDataResponse =
    case providerDataResponse of
        RemoteData.Success data ->
            RemoteData.succeed data.hotels

        RemoteData.Failure err ->
            RemoteData.Failure err

        _ ->
            RemoteData.succeed []


updateHotels : RemoteData.WebData (List Hotel) -> RemoteData.WebData (List Hotel) -> RemoteData.WebData (List Hotel)
updateHotels old new =
    let
        mappedOld =
            case old of
                RemoteData.Loading ->
                    RemoteData.succeed []

                _ ->
                    old
    in
        RemoteData.map2 mergeHotels mappedOld new


mergeHotels : List Hotel -> List Hotel -> List Hotel
mergeHotels old new =
    List.append old new


fetchProvidersUrls : Cmd Msg
fetchProvidersUrls =
    Http.get "http://localhost:3000/providersUrls" decodeProvidersUrls
        |> RemoteData.sendRequest
        |> Cmd.map ProvidersUrlsFetched


decodeProviderUrl : Decode.Decoder ProviderUrl
decodeProviderUrl =
    Decode.map2 createProviderUrl (Decode.field "name" Decode.string) (Decode.field "url" Decode.string)


decodeProvidersUrls : Decode.Decoder (List ProviderUrl)
decodeProvidersUrls =
    Decode.list decodeProviderUrl


fetchProviderData : String -> Cmd Msg
fetchProviderData url =
    Http.get url decodeProviderData
        |> RemoteData.sendRequest
        |> Cmd.map (ProviderDataFetched url)


decodeProviderData : Decode.Decoder ProviderData
decodeProviderData =
    Decode.map ProviderData (Decode.field "hotels" decodeHotels)


decodeHotel : Decode.Decoder Hotel
decodeHotel =
    Decode.map Hotel (Decode.field "name" Decode.string)


decodeHotels : Decode.Decoder (List Hotel)
decodeHotels =
    Decode.list decodeHotel



-- MAIN --


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
