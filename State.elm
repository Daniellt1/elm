module State exposing (..)

import Dict
import Helpers exposing (..)
import RemoteData
import Types exposing (..)
import Http


init : ( Model, Cmd Msg )
init =
    ( { currentPage = 1
      , deals = Dict.empty
      , hotels = Dict.empty
      , hotelsPages = Dict.singleton 1 RemoteData.Loading
      , providers = Just Dict.empty
      }
    , fetchInitData
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        {-
           Ideally InitData endpoint should be separated into different ones for
           urls and cache, but as per current website implementation - init data
           return either providers urls or cached data.
        -}
        InitDataFetched response ->
            let
                errorCase : Http.Error -> ( Model, Cmd Msg )
                errorCase error =
                    { model
                        | providers = Nothing
                        , hotelsPages = overideHotelPage 1 (RemoteData.Failure error) model.hotelsPages
                    }
                        ! []
            in
                case response of
                    RemoteData.Failure e ->
                        errorCase e

                    RemoteData.Success { urls, offers } ->
                        case offers of
                            Just offersData ->
                                {- The priority is give to offers case:
                                   is offers exists - load cached data only
                                -}
                                { model
                                    | deals = offersData.deals
                                    , hotels = offersData.hotels
                                    , hotelsPages = overideHotelPage 1 (hotelPageFromHotels offersData.hotels) model.hotelsPages
                                    , providers = Nothing
                                }
                                    ! []

                            Nothing ->
                                {- If offers does not exist and urls exits:
                                   load data from provider
                                -}
                                case urls of
                                    Just urls ->
                                        { model
                                            | providers = mapProvidersUrlsToProviders urls
                                            , hotelsPages = overideHotelPage 1 RemoteData.Loading model.hotelsPages
                                        }
                                            ! fetchProvidersData urls

                                    -- Neither offers or urls exist
                                    Nothing ->
                                        errorCase Http.Timeout

                    _ ->
                        model ! []

        ProviderDataFetched name url response ->
            let
                updatedProviders : Providers
                updatedProviders =
                    updateProvider name (Provider name url response) model.providers

                hasAllProvidersBeenLoaded : Bool
                hasAllProvidersBeenLoaded =
                    Maybe.withDefault Dict.empty updatedProviders
                        |> Dict.filter (\_ provider -> RemoteData.isLoading provider.data)
                        |> Dict.isEmpty

                newPage : HotelPage
                newPage =
                    (hotelPageFromProviderDataResponse response)

                updatePage : Maybe HotelPage -> Maybe HotelPage
                updatePage maybeOldPage =
                    case maybeOldPage of
                        Just oldPage ->
                            if hasAllProvidersBeenLoaded then
                                case oldPage of
                                    {-
                                       If page state is loading on the stage when all providers
                                       have been loaded - return NetworkError
                                    -}
                                    RemoteData.Loading ->
                                        Just (RemoteData.Failure Http.NetworkError)

                                    _ ->
                                        Just oldPage
                            else
                                Just (mergeFirstPage oldPage newPage)

                        Nothing ->
                            Nothing
            in
                { model
                    | deals = Dict.union (dealsFromProviderDataResponse response) model.deals
                    , hotels = Dict.union (hotelsFromProviderDataResponse response) model.hotels
                    , hotelsPages = Dict.update 1 updatePage model.hotelsPages
                    , providers = updatedProviders
                }
                    ! []
