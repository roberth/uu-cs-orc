module Network.TimeKeeper.Protocol where
import Data.Text

-- | Definition of a path.
--
-- A path consists of zero or more node names.
-- It identifies a node, which may or may not have a value.
--
-- Children of a node are defined as all nodes that have a value
-- and whose paths satisfy
--
-- > childPath = parentPath ++ [childNodeName]
data Path = Path [NodeName]
          deriving (Eq, Ord, Show, Read)
type NodeName = Text

-- | The set of commands emitted by clients.
data Action = Put         { path :: Path, putNewValue :: Maybe Text }
            | Get         { path :: Path }
            | GetChildren { path :: Path }
            | Subscribe   { path :: Path }
            | Unsubscribe { path :: Path }
            deriving (Eq, Ord, Show, Read)

-- | The set of events emitted by servers.
data Event = Updated     { eventPath :: Path, oldValue :: Maybe Text, newValue :: Maybe Text }
           | ValueIs     { eventPath :: Path,                         newValue :: Maybe Text }
           | ChildrenAre { eventPath :: Path, children :: [Text] }
           deriving (Eq, Ord, Show, Read)
