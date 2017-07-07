module Helpers exposing (..)

import Dict
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import List.Extra
import RemoteData
import Types exposing (..)

-- if Loading -- Reload page
-- if Success with size -- do Nothing
-- if Success without items -- No results

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
                            -- if newPage failed - leave Loading
                            -- if newPage Success - leave Success
                            

                        RemoteData.Success _ ->
                            -- if newPage failed - leave Success
                            -- if newPage Success - merge
                            if RemoteData.isSuccess newPage then
                                Just (mergePages oldPage newPage)
                            else
                                Just oldPage

                        _ ->
                            -- TODO: merge errors for failure
                            Just newPage

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
nameProviderTuple ( name, url ) =
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


hotelsFromProviderDataResponse : RemoteData.WebData ProviderData -> Hotels
hotelsFromProviderDataResponse providerDataResponse =
    case providerDataResponse of
        RemoteData.Success data ->
            data.hotels

        _ ->
            Dict.empty


dealsFromProviderDataResponse : RemoteData.WebData ProviderData -> Deals
dealsFromProviderDataResponse providerDataResponse =
    case providerDataResponse of
        RemoteData.Success data ->
            data.deals

        _ ->
            Dict.empty


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
    Http.get "http://localhost:3000/json/holiday-search/broker/main/offers?sourceID=15&itemsPerPage=20&dealsPerHotel=4&minDate=2017-02-18&searchType=sun&maxPrice=100&sgRegion=38366&page=1&maxDate=2017-02-24&minPrice=10&rating=4%2C2%2C3" decodeProvidersUrls
        |> RemoteData.sendRequest
        |> Cmd.map ProvidersUrlsFetched


decodeProvidersUrls : Decode.Decoder ProvidersUrls
decodeProvidersUrls =
    Decode.field "urls" <| Decode.keyValuePairs Decode.string


fetchProviderData : ProviderUrl -> Cmd Msg
fetchProviderData ( name, url ) =
    -- TODO: hardcoded part of the url should be removed
    Http.get ("http://localhost:3000/offer-broker-prefix" ++ url) decodeProviderData
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
    let
        accommodationIdDecoder : Decode.Decoder HotelId
        accommodationIdDecoder =
            Decode.field "accommodationId" Decode.string

        dealsDecoder : Decode.Decoder Deals
        dealsDecoder =
            accommodationIdDecoder
                |> Decode.andThen
                    (\accommodationId ->
                        Decode.succeed (Dict.filter (\_ deal -> deal.hotel == accommodationId) deals)
                    )
    in
    Pipeline.decode Hotel
        |> Pipeline.custom accommodationIdDecoder
        |> Pipeline.required "accommodationName" Decode.string
        |> Pipeline.custom dealsDecoder


decodeIdHotelTuple : Deals -> Decode.Decoder ( HotelId, Hotel )
decodeIdHotelTuple deals =
    Decode.map (\hotel -> ( hotel.accommodationId, hotel )) (decodeHotel deals)


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
