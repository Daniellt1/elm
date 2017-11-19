module Types exposing (..)

import Dict
import RemoteData


-- Msg union type


type Msg
    = InitDataFetched (RemoteData.WebData InitData)
    | ProviderDataFetched String String (RemoteData.WebData ProviderData)



-- Model type


type alias Model =
    { currentPage : Int
    , deals : Deals
    , hotels : Hotels
    , hotelsPages : HotelsPages
    , providers : Providers
    }



-- Init data related types


type alias InitData =
    { urls : Maybe ProvidersUrls
    , offers : Maybe Offers
    }



-- Offers related types


type alias Offers =
    { hotels : Hotels
    , deals : Deals
    }



-- Provider urls related types and constructors


type alias ProvidersUrls =
    List ProviderUrl


type alias ProviderUrl =
    ( String, String )


createProviderUrl : String -> String -> ProviderUrl
createProviderUrl name url =
    ( name, url )



-- Provider related types


type alias Providers =
    Maybe (Dict.Dict String Provider)


type alias Provider =
    { name : String
    , url : String
    , data : RemoteData.WebData ProviderData
    }


type alias ProviderData =
    { hotels : Hotels
    , deals : Deals
    }



-- Hotel pages related types


type alias HotelsPages =
    Dict.Dict Int HotelPage


type alias HotelPage =
    RemoteData.WebData (List HotelId)



-- Hotels related types


type alias Hotels =
    Dict.Dict HotelId Hotel


type alias Hotel =
    { accommodationId : HotelId
    , accommodationName : String
    , dealsPages : DealsPages
    }


type alias HotelId =
    String



-- Deals pages related types


type alias DealsPages =
    Dict.Dict Int DealPage


type alias DealPage =
    RemoteData.WebData (List DealId)



-- Deals related types


type alias D =
    Dict.Dict String Deal


type alias Deal =
    { hotel : HotelId
    , offerId : String
    }


type alias DealId =
    String
