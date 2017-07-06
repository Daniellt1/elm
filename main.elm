module Main exposing (..)

import Dict
import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import List.Extra
import RemoteData


-- MODEL --


type alias Model =
    { currentPage : Int
    , pages : HotelsPages
    , providers : Maybe (Dict.Dict String Provider)
    }


type alias HotelsPages =
    Dict.Dict Int HotelPage


type alias HotelPage =
    RemoteData.WebData (List HotelId)


type alias ProviderUrl =
    { name : String
    , url : String
    }


type alias ProviderData =
    { hotels : Hotels
    , deals : Deals
    }


type alias Provider =
    { name : String
    , url : String
    , data : RemoteData.WebData ProviderData
    }


type alias HotelsState =
    RemoteData.WebData Hotels


type alias Hotels =
    Dict.Dict HotelId Hotel


type alias Hotel =
    { accomodationId : HotelId
    , accomodationName : String
    , deals : Deals
    }


type alias HotelId =
    String


type alias DealsPages =
    Dict.Dict Int DealPage


type alias DealPage =
    List DealId


type alias Deals =
    Dict.Dict String Deal


type alias Deal =
    { hotel : String
    , offerId : String
    }


type alias DealId =
    String


init : ( Model, Cmd Msg )
init =
    ( Model 1 (Dict.singleton 1 RemoteData.NotAsked) (Just Dict.empty), fetchProvidersUrls )



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Providers: "
            , viewStatusBar model.providers
            ]
        , div [] [ viewPages model.pages ]
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


viewPages : HotelsPages -> Html Msg
viewPages pages =
    Dict.values pages
        |> List.map viewPage
        |> div []


viewPage : HotelPage -> Html Msg
viewPage page =
    case page of
        RemoteData.NotAsked ->
            text "Initialising."

        RemoteData.Loading ->
            text "Loading."

        RemoteData.Failure err ->
            text ("Error: " ++ toString err)

        RemoteData.Success hotelsItems ->
            text (toString page)


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

                currentPage : HotelPage
                currentPage =
                    case response of
                        RemoteData.Failure e ->
                            RemoteData.Failure e

                        _ ->
                            RemoteData.Loading
            in
            { model
                | providers = providers
                , pages = Dict.update model.currentPage (\_ -> Just currentPage) model.pages
            }
                ! cmds

        ProviderDataFetched name url response ->
            { model
                | providers = updateProvider name (Provider name url response) model.providers
                , pages = updatePage model.currentPage (pageFromProviderDataResponse response) model.pages
            }
                ! []


updatePage : Int -> HotelPage -> HotelsPages -> HotelsPages
updatePage index page pages =
    let
        mergePages : HotelPage -> HotelPage -> HotelPage
        mergePages old new =
            RemoteData.map2 (\old new -> List.append old new |> List.Extra.unique) old new

        updatePageValue newPage maybeOldPage =
            case maybeOldPage of
                Just oldPage ->
                    case oldPage of
                        RemoteData.Loading ->
                            Just (mergePages (RemoteData.succeed []) newPage)

                        _ ->
                            Just (mergePages oldPage newPage)

                Nothing ->
                    Just newPage
    in
    Dict.update index (updatePageValue page) pages


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


pageFromProviderDataResponse : RemoteData.WebData ProviderData -> HotelPage
pageFromProviderDataResponse providerDataResponse =
    case providerDataResponse of
        RemoteData.Success data ->
            RemoteData.succeed (Dict.keys data.hotels)

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
    let
        dealsDecoder : Decode.Decoder Deals
        dealsDecoder =
            Decode.field "deals" decodeDeals

        hotelsDecoder : Decode.Decoder Hotels
        hotelsDecoder =
            dealsDecoder
                |> Decode.andThen
                    (\deals ->
                        Decode.field "hotels" (decodeHotels deals)
                    )
    in
    Pipeline.decode ProviderData
        |> Pipeline.custom hotelsDecoder
        |> Pipeline.custom dealsDecoder


decodeHotel : Deals -> Decode.Decoder Hotel
decodeHotel deals =
    Pipeline.decode Hotel
        |> Pipeline.required "accomodationId" Decode.string
        |> Pipeline.required "accomodationName" Decode.string
        |> Pipeline.hardcoded deals


decodeIdHotelTuple : Deals -> Decode.Decoder ( HotelId, Hotel )
decodeIdHotelTuple deals =
    Decode.map (\hotel -> ( hotel.accomodationId, hotel )) (decodeHotel deals)


decodeHotels : Deals -> Decode.Decoder (Dict.Dict String Hotel)
decodeHotels deals =
    Decode.map (\l -> Dict.fromList l) (Decode.list (decodeIdHotelTuple deals))


decodeDeals : Decode.Decoder Deals
decodeDeals =
    Decode.map (\l -> Dict.fromList l) (Decode.list decodeIdDealTuple)


decodeIdDealTuple : Decode.Decoder ( DealId, Deal )
decodeIdDealTuple =
    Decode.map (\deal -> ( deal.offerId, deal )) decodeDeal


decodeDeal : Decode.Decoder Deal
decodeDeal =
    Pipeline.decode Deal
        |> Pipeline.required "hotel" Decode.string
        |> Pipeline.required "offerId" Decode.string



-- MAIN --


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
