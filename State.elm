module State exposing (..)

import Dict
import Helpers exposing (..)
import RemoteData
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { currentPage = 1
      , deals = Dict.empty
      , hotels = Dict.empty
      , hotelsPages = Dict.singleton 1 RemoteData.NotAsked
      , providers = Just Dict.empty
      }
    , fetchProvidersUrls
    )


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
                , hotelsPages = Dict.update model.currentPage (\_ -> Just currentPage) model.hotelsPages
            }
                ! cmds

        ProviderDataFetched name url response ->
            { model
                | deals = Dict.union (dealsFromProviderDataResponse response) model.deals
                , hotels = Dict.union (hotelsFromProviderDataResponse response) model.hotels
                , hotelsPages = updatePage model.currentPage (pageFromProviderDataResponse response) model.hotelsPages
                , providers = updateProvider name (Provider name url response) model.providers
            }
                ! []
