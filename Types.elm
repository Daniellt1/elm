module Types exposing (..)

import Dict
import RemoteData


type Msg
    = ProvidersUrlsFetched (RemoteData.WebData (List ProviderUrl))
    | ProviderDataFetched String String (RemoteData.WebData ProviderData)


type alias Model =
    { currentPage : Int
    , deals : Deals
    , hotels : Hotels
    , hotelsPages : HotelsPages
    , providers : Maybe (Dict.Dict String Provider)
    }


type alias HotelsPages =
    Dict.Dict Int HotelPage


type alias HotelPage =
    RemoteData.WebData (List HotelId)


type alias ProviderUrl =
    ( String, String )


type alias ProvidersUrls =
    List ProviderUrl


type alias ProviderData =
    { hotels : Hotels
    , deals : Deals
    }


type alias Provider =
    { name : String
    , url : String
    , data : RemoteData.WebData ProviderData
    }


type alias Hotels =
    Dict.Dict HotelId Hotel


type alias Hotel =
    { accommodationId : HotelId
    , accommodationName : String
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
    { hotel : HotelId
    , offerId : String
    }


type alias DealId =
    String
