module View exposing (..)

import Dict
import Html exposing (..)
import RemoteData
import Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Providers: "
            , viewStatusBar model.providers
            ]
        , div [] [ viewPages model.hotels model.hotelsPages ]
        ]


viewStatusBar : Maybe (Dict.Dict String Provider) -> Html Msg
viewStatusBar providers =
    let
        providersSize : Dict.Dict String Provider -> Int
        providersSize providers =
            Dict.size providers

        filterFetchedProviders : String -> Provider -> Bool
        filterFetchedProviders _ provider =
            case provider.data of
                RemoteData.Success _ ->
                    True

                _ ->
                    False

        fetchedProvidersSize : Dict.Dict String Provider -> Int
        fetchedProvidersSize providers =
            Dict.size <|
                Dict.filter filterFetchedProviders providers
    in
    case providers of
        Just providers_ ->
            if providersSize providers_ == 0 then
                text "Loading"
            else if providersSize providers_ == fetchedProvidersSize providers_ then
                text "Loaded"
            else
                text (toString (fetchedProvidersSize providers_) ++ " of " ++ toString (providersSize providers_))

        Nothing ->
            text ""


viewPages : Hotels -> HotelsPages -> Html Msg
viewPages hotels pages =
    Dict.values pages
        |> List.map (viewPage hotels)
        |> div []


viewPage : Hotels -> HotelPage -> Html Msg
viewPage hotels page =
    let
        getHotelById : HotelId -> Maybe Hotel
        getHotelById id_ =
            Dict.get id_ hotels
    in
    case page of
        RemoteData.NotAsked ->
            text "Initialising."

        RemoteData.Loading ->
            text "Loading."

        RemoteData.Failure err ->
            text ("Error: " ++ toString err)

        RemoteData.Success hotelsIds ->
            div []
                (List.map getHotelById hotelsIds
                    |> List.map viewHotel
                )


viewHotel : Maybe Hotel -> Html Msg
viewHotel hotel =
    case hotel of
        Just hotelItem ->
            div []
                [ text ("Hotel name: " ++ hotelItem.accommodationName) ]

        Nothing ->
            text ""
