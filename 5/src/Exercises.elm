module Exercises exposing (..)

import Theme exposing (ThemeConfig(..))
import Lists exposing (filter, map)

type alias UserDetails = 
  { firstName: String
  , lastName: String
  , phoneNumber: Maybe String
  }
type alias User = {id: String, email: String, details: UserDetails}


makeUser id email firstName lastName phoneNumber = 
  User id email (UserDetails firstName lastName phoneNumber)

usersWithPhoneNumbers : List User -> List String
usersWithPhoneNumbers users =
  users
    |> filter (.details >> .phoneNumber >> (\x -> not (x == Nothing)))
    |> map (\x -> x.email)

type alias AccountConfiguration = 
  { preferredTheme: ThemeConfig
  , subscribedToNewsletter: Bool
  , twoFactorAuthOn: Bool
  }


changePreferenceToDarkTheme : List AccountConfiguration -> List AccountConfiguration
changePreferenceToDarkTheme accounts = 
  accounts
    |> filter (\x -> x.preferredTheme == Dark)

