module Main exposing (..)

import Dict
import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import RemoteData


-- MODEL --


type alias Model =
    { providers : Maybe (Dict.Dict String Provider)
    , hotels : RemoteData.WebData (List Hotel)
    }


type alias ProviderUrl =
    { name : String
    , url : String
    }


type alias ProviderData =
    { hotels : List Hotel
    }


type alias Provider =
    { name : String
    , url : String
    , data : RemoteData.WebData ProviderData
    }


type alias Hotel =
    { accomodationId : String
    , accomodationName : String
    , deals : List Deal
    }


type alias Deal =
    { hotel : String
    , offerId : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Just Dict.empty) RemoteData.Loading, fetchProvidersUrls )



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Providers: "
            , viewStatusBar model.providers
            ]
        , div [] [ viewHotels model.hotels ]
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
        [ text ("Hotel name: " ++ hotel.accomodationName) ]



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE --


type Msg
    = ProvidersUrlsFetched (RemoteData.WebData (List ProviderUrl))
    | ProviderDataFetched String String (RemoteData.WebData ProviderData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProvidersUrlsFetched response ->
            let
                cmds : List (Cmd Msg)
                cmds =
                    case response of
                        RemoteData.Success providersUrls ->
                            List.map fetchProviderData providersUrls

                        _ ->
                            []

                providers : Maybe (Dict.Dict String Provider)
                providers =
                    case response of
                        RemoteData.Success providersUrls ->
                            Just <|
                                Dict.fromList <|
                                    List.map nameProviderTuple providersUrls

                        _ ->
                            Nothing

                hotels : RemoteData.WebData (List Hotel)
                hotels =
                    case response of
                        RemoteData.Failure e ->
                            RemoteData.Failure e

                        _ ->
                            RemoteData.Loading
            in
            { model
                | providers = providers
                , hotels = hotels
            }
                ! cmds

        ProviderDataFetched name url response ->
            { model
                | hotels = updateHotels model.hotels (hotelsFromProviderDataResponse response)
                , providers = updateProvider name (Provider name url response) model.providers
            }
                ! []


updateProvider : String -> Provider -> Maybe (Dict.Dict String Provider) -> Maybe (Dict.Dict String Provider)
updateProvider name provider providers =
    case providers of
        Just providers_ ->
            Just (Dict.update name (\_ -> Just provider) providers_)

        Nothing ->
            providers


nameProviderTuple : ProviderUrl -> ( String, Provider )
nameProviderTuple { name, url } =
    ( name, Provider name url RemoteData.Loading )


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
    Http.get "http://localhost:8080/providersUrls" decodeProvidersUrls
        |> RemoteData.sendRequest
        |> Cmd.map ProvidersUrlsFetched


decodeProviderUrl : Decode.Decoder ProviderUrl
decodeProviderUrl =
    Pipeline.decode ProviderUrl
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "url" Decode.string


decodeProvidersUrls : Decode.Decoder (List ProviderUrl)
decodeProvidersUrls =
    Decode.list decodeProviderUrl


fetchProviderData : ProviderUrl -> Cmd Msg
fetchProviderData { name, url } =
    Http.get url decodeProviderData
        |> RemoteData.sendRequest
        |> Cmd.map (ProviderDataFetched name url)


decodeProviderData : Decode.Decoder ProviderData
decodeProviderData =
    Decode.map ProviderData <|
        (Decode.field "deals" decodeDeals
            |> Decode.andThen
                (\deals ->
                    Decode.field "hotels" (decodeHotels deals)
                )
        )


decodeHotel : List Deal -> Decode.Decoder Hotel
decodeHotel deals =
    Pipeline.decode Hotel
        |> Pipeline.required "accomodationId" Decode.string
        |> Pipeline.required "accomodationName" Decode.string
        |> Pipeline.hardcoded deals


decodeHotels : List Deal -> Decode.Decoder (List Hotel)
decodeHotels deals =
    Decode.list (decodeHotel deals)


decodeDeals : Decode.Decoder (List Deal)
decodeDeals =
    Decode.list decodeDeal


decodeDeal : Decode.Decoder Deal
decodeDeal =
    Pipeline.decode Deal
        |> Pipeline.required "hotel" Decode.string
        |> Pipeline.required "offerId" Decode.string



-- MAIN --


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
