module Network.TimeKeeper.Protocol where
import Data.Text

data Path = Root
          | Path :/: Text
          deriving (Eq, Ord, Show, Read)

data Action = Put         { path :: Path, putNewValue :: Maybe Text }
            | Get         { path :: Path }
            | List        { path :: Path }
            | Subscribe   { path :: Path }
            | Unsubscribe { path :: Path }
            deriving (Eq, Ord, Show, Read)

data Event = Updated     { eventPath :: Path, oldValue :: Maybe Text, newValue :: Maybe Text }
           | ValueIs     { eventPath :: Path,                         newValue :: Maybe Text }
           deriving (Eq, Ord, Show, Read)
