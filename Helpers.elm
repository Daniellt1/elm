module Helpers exposing (..)

import Dict
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import List.Extra
import RemoteData
import Types exposing (..)


{-
   mergeFirstPage:
      Because of the multiple providers - first page merging proccess has
      specific implementation:
      1. If old page is loading:
          1.1 If new page is succeeded - return new one.
          1.2 If new page is not succeeded - return old one.
      2 If old page is succeeded:
          1.1 If new page is succeeded - return merged old and new.
          1.2 If new page is not succeeded - return old one.

      That logic guarantees than in the view layer - page is always in loading or
      succeeded state. All occured errror going to be handled by update function
      as soon as all providers loaded.
-}


mergeFirstPage : HotelPage -> HotelPage -> HotelPage
mergeFirstPage oldPage newPage =
    let
        mergePages : HotelPage -> HotelPage -> HotelPage
        mergePages old new =
            RemoteData.map2 (\old new -> List.append old new |> List.Extra.unique) old new
    in
        case oldPage of
            RemoteData.Loading ->
                if RemoteData.isSuccess newPage then
                    newPage
                else
                    oldPage

            RemoteData.Success _ ->
                if RemoteData.isSuccess newPage then
                    mergePages oldPage newPage
                else
                    oldPage

            _ ->
                newPage


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


hotelPageFromProviderDataResponse : RemoteData.WebData ProviderData -> HotelPage
hotelPageFromProviderDataResponse providerDataResponse =
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



-- REST starts


fetchInitData : Cmd Msg
fetchInitData =
    Http.get "http://localhost:3000/json/holiday-search/broker/main/offers?sourceID=15&itemsPerPage=20&dealsPerHotel=4&minDate=2017-02-18&searchType=sun&maxPrice=100&sgRegion=38366&page=1&maxDate=2017-02-24&minPrice=10&rating=4%2C2%2C3" decodeInitData
        |> RemoteData.sendRequest
        |> Cmd.map InitDataFetched


fetchProvidersData : ProvidersUrls -> List (Cmd Msg)
fetchProvidersData providersUrls =
    List.map fetchProviderData providersUrls


fetchProviderData : ProviderUrl -> Cmd Msg
fetchProviderData ( name, url ) =
    -- TODO: hardcoded part of the url should be removed
    Http.get ("http://localhost:3000/offer-broker-prefix" ++ url) decodeProviderData
        |> RemoteData.sendRequest
        |> Cmd.map (ProviderDataFetched name url)



-- REST ends
-- Decoders start
-- Init data decoder


decodeInitData : Decode.Decoder InitData
decodeInitData =
    Pipeline.decode InitData
        |> Pipeline.custom (Decode.maybe decodeProvidersUrls)
        |> Pipeline.custom (Decode.maybe decodeOffers)



-- Provider url decoder


decodeProvidersUrls : Decode.Decoder ProvidersUrls
decodeProvidersUrls =
    Decode.keyValuePairs Decode.string
        |> Decode.field "urls"



-- Offer decoder


decodeOffers : Decode.Decoder Offers
decodeOffers =
    (Pipeline.decode Offers
        |> Pipeline.custom decodeHotels
        |> Pipeline.custom decodeDeals
    )
        |> Decode.field "offers"



-- HotelPage helpers


overideHotelPage : Int -> HotelPage -> HotelsPages -> HotelsPages
overideHotelPage index page pages =
    Dict.update index (\_ -> Just page) pages


hotelPageFromHotels : Hotels -> HotelPage
hotelPageFromHotels hotels =
    RemoteData.Success (Dict.keys hotels)



-- Provider data decoder


decodeProviderData : Decode.Decoder ProviderData
decodeProviderData =
    Pipeline.decode ProviderData
        |> Pipeline.custom decodeHotels
        |> Pipeline.custom decodeDeals


mapProvidersUrlsToProviders : ProvidersUrls -> Providers
mapProvidersUrlsToProviders urls =
    List.map nameProviderTuple urls
        |> Dict.fromList
        |> Just



-- Hotels related decoders


decodeHotels : Decode.Decoder Hotels
decodeHotels =
    -- Decode deals first
    decodeDeals
        |> Decode.andThen
            (\deals ->
                -- Decode hotels as tuple ( HoteId, Hotel ) and transform to Dict
                Decode.map (\l -> Dict.fromList l) (Decode.list (decodeIdHotelTuple deals))
                    |> Decode.field "hotels"
            )


decodeIdHotelTuple : Deals -> Decode.Decoder ( HotelId, Hotel )
decodeIdHotelTuple deals =
    Decode.map (\hotel -> ( hotel.accommodationId, hotel )) (decodeHotel deals)


decodeHotel : Deals -> Decode.Decoder Hotel
decodeHotel deals =
    let
        accommodationIdDecoder : Decode.Decoder HotelId
        accommodationIdDecoder =
            Decode.field "accommodationId" Decode.string

        dealsDecoder : Decode.Decoder DealPage
        dealsDecoder =
            accommodationIdDecoder
                |> Decode.andThen
                    (\accommodationId ->
                        Dict.filter (\_ deal -> deal.hotel == accommodationId) deals
                            |> Dict.keys
                            |> Decode.succeed
                    )

        dealsPagesDecoder : Decode.Decoder DealsPages
        dealsPagesDecoder =
            Decode.map (\deals -> Dict.singleton 1 deals) dealsDecoder
    in
        Pipeline.decode Hotel
            |> Pipeline.custom accommodationIdDecoder
            |> Pipeline.required "accommodationName" Decode.string
            |> Pipeline.custom dealsPagesDecoder



-- Deals related decoders


decodeDeals : Decode.Decoder Deals
decodeDeals =
    -- Decode deals as tuple ( DealId, Deal ) and transform to Dict
    Decode.map (\l -> Dict.fromList l) (Decode.list (decodeIdDealTuple))
        |> Decode.field "deals"


decodeIdDealTuple : Decode.Decoder ( DealId, Deal )
decodeIdDealTuple =
    Decode.map (\deal -> ( deal.offerId, deal )) decodeDeal


decodeDeal : Decode.Decoder Deal
decodeDeal =
    Pipeline.decode Deal
        |> Pipeline.required "hotel" Decode.string
        |> Pipeline.required "offerId" Decode.string



-- Decoders end
