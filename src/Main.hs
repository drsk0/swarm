{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth.Common
import Auth.Google
import Control.Exception (bracket)
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Crypto.Hash
import Data.Acid
import Data.Aeson
import qualified Data.Binary as B
import Data.Binary.Instances.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.List.Extra
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Morpheus as M
import Data.Morpheus.Subscriptions
import Data.Morpheus.Types
  ( GQLType(..)
  , ResolverM
  , ResolverQ
  , ResolverS
  , RootResolver(..)
  , SubscriptionField
  , Undefined(..)
  , lift
  , publish
  , subscribe
  )
import Data.Ord
import Data.SafeCopy
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding
import Data.Time.Clock
import Data.X509
import qualified Firebase.CloudMessaging as FCM
import GHC.Generics
import qualified Network.HTTP.Simple as Http
import Network.HTTP.Types.Status
import Network.WebSockets (ServerApp)
import Network.WebSockets.Snap
import Snap.Core
import qualified Snap.Test as Snap (evalHandler, get)
import Snap.Http.Server
import qualified Snap.Util.CORS as CORS
import qualified System.Console.GetOpt as Opt
import Snap.Internal.Core

-- Firebase
-----------

firebaseProjectName :: String
firebaseProjectName = "swarm-90059"

-- Utils
--------

errorOnLeft :: String -> Either String a -> IO a
errorOnLeft msg =
  \case
    Left err -> error $ msg <> ": " <> err
    Right x -> pure x

interleave :: [a] -> [a] -> [a]
interleave (e:es) (o:os) = e : o : interleave es os
interleave es [] = es
interleave [] os = os

-- TODO replace with Down
flipOrder :: Ordering -> Ordering
flipOrder EQ = EQ
flipOrder LT = GT
flipOrder GT = LT

----------------------------------------------------------------------------------------------------
-- Data model
class ReadAccess a where
  canRead :: UserId -> a -> Bool

class WriteAccess a where
  canWrite :: UserId -> a -> Bool


data State = State
  { stSwarms :: M.Map SwarmRef Swarm
  , stUserSwarms :: M.Map UserId [WithRef T.Text]
  , stNextRefCounter :: Int
  , stInvites :: M.Map InviteToken SwarmRef
  , stFish :: M.Map UserId Fish
  }

publicRef :: SwarmRef
publicRef = "public"

publicSwarmName :: WithRef T.Text
publicSwarmName = WithRef publicRef "public"

emptyState :: State
emptyState =
  State
    { stSwarms = M.singleton publicRef $ emptySwarm "god" "public"
    , stUserSwarms = M.empty
    , stNextRefCounter = 0
    , stInvites = M.empty
    , stFish = M.empty
    }
data Swarm = Swarm
  { sBallots :: M.Map BallotId Ballot
  , sFish :: S.Set UserId
  , sCreator :: UserId
  , sName :: T.Text
  } deriving (Generic)

instance B.Binary Swarm

emptySwarm :: UserId -> T.Text -> Swarm
emptySwarm uId n =
  Swarm {sBallots = M.empty, sFish = S.empty, sCreator = uId, sName = n}

newSwarm :: UserId -> T.Text -> Swarm
newSwarm uId n =
  Swarm {sBallots = M.empty, sFish = S.singleton uId, sCreator = uId, sName = n}

data WithId a = WithId
  { wiId :: T.Text
  , wiValue :: a
  } deriving (Show, Generic, GQLType)
instance ReadAccess a => ReadAccess (WithId a) where
  canRead uId x = canRead uId (wiValue x)

data WithRef a = WithRef
  { wrRef :: T.Text
  , wrValue :: a
  } deriving (Generic, Ord, Eq, GQLType)

instance ReadAccess a => ReadAccess (WithRef a) where
  canRead uId x = canRead uId (wrValue x)

data Fish = Fish
  { fUserId :: UserId
  , fName :: T.Text
  , fProfile :: T.Text
  , fFcmToken :: Maybe T.Text
  } deriving (Generic, Eq, Ord, GQLType)
instance B.Binary Fish

newtype Invite = Invite
  { ivToken :: InviteToken
  } deriving (Eq, Ord, Generic)
instance B.Binary Invite

newtype UpVote = UpVote
  { uvUserId :: UserId
  } deriving (Generic, GQLType, Show, Eq, Ord)
instance B.Binary UpVote

-- | Sort by descending upvotes
sortByUpVotes :: [WithId Argument] -> [WithId Argument]
sortByUpVotes = sortOn (Down . S.size . aUpVotes . wiValue)

data Ballot = Ballot
  { bTitle :: T.Text
  , bDescription :: T.Text
  , bCreator :: UserId
  , bCreationDate :: T.Text
  , bDelegations :: S.Set Delegation
  , bOptions :: M.Map OptionId Option
  } deriving (Show, Eq, Generic, GQLType)
instance B.Binary Ballot
instance ReadAccess Ballot where
  canRead _uId Ballot {} = True

data Option = Option
  { oTitle :: T.Text
  , oDescription :: T.Text
  , oUserId :: T.Text
  , oArguments :: M.Map ArgumentId Argument
  , oVotes :: M.Map UserId Vote
  } deriving (Show, Eq, Generic, GQLType)
instance B.Binary Option
instance ReadAccess Option where
  canRead _uId Option {} = True

data NrOfStars
  = ZeroStars
  | OneStar
  | TwoStars
  | ThreeStars
  | FourStars
  | FiveStars
  deriving (Show, Eq, Ord, Generic, GQLType, Enum)
instance B.Binary NrOfStars

data Vote = Vote
  { vUserId :: UserId
  , vStars :: NrOfStars
  } deriving (Show, Eq, Ord, Generic, GQLType)
instance B.Binary Vote
instance ReadAccess Vote where
  canRead uId Vote {..} = vUserId == uId

data Delegation = Delegation
  { dUserId :: UserId
  , dDelegatee :: UserId
  } deriving (Show, Eq, Ord, Generic, GQLType)
instance B.Binary Delegation
instance ReadAccess Delegation where
  canRead uId Delegation {..} = dUserId == uId || dDelegatee == uId

data Argument = Argument
  { aUserId :: UserId
  , aProContra :: ProContra
  , aUpVotes :: S.Set UpVote
  , aText :: T.Text
  } deriving (Show, Eq, Generic, GQLType)
instance B.Binary Argument
instance ReadAccess Argument where
  canRead _uId Argument {} = True

data ProContra = Pro | Contra deriving (Show, Eq, Generic, GQLType)
instance B.Binary ProContra

data IProContra = IPro | IContra deriving (Show, Eq, Generic, GQLType)
instance B.Binary IProContra

data BallotResult = BallotResult
  { brVotes :: BallotResultMap
  , brTurnout :: Int
  , brOptions :: [WithId Option]
  } deriving (Generic, GQLType)
emptyResult = BallotResult M.empty 0 []

type ArgumentId = T.Text
type BallotId = T.Text
type BallotResultMap = M.Map OptionId Int
type DelegationId = T.Text
type OptionId = T.Text
type SwarmRef = T.Text
type UpVoteId = T.Text
type UserId = T.Text
type UserName = T.Text
type VoteId = T.Text
type InviteToken = T.Text

mkId :: B.Binary a => a -> T.Text
mkId x = T.pack $ show (hash $ BSL.toStrict $ B.encode x :: Digest SHA256)

mkRef :: B.Binary a => a -> Update State T.Text
mkRef x = do
  s <- get
  put $ s {stNextRefCounter = stNextRefCounter s + 1}
  pure $
    T.pack $
    show
      (hash $ BSL.toStrict (B.encode x <> B.encode (stNextRefCounter s)) :: Digest SHA256)

updateSwarm :: SwarmRef -> (Swarm -> Swarm) -> Update State ()
updateSwarm sRef f =
  modify $ \st -> st {stSwarms = M.adjust f sRef (stSwarms st)}

updateBallot :: SwarmRef -> BallotId -> (Ballot -> Ballot) -> Update State ()
updateBallot sRef bId f =
  updateSwarm sRef $ \s -> s {sBallots = M.adjust f bId (sBallots s)}

updateOption :: SwarmRef -> BallotId -> OptionId -> (Option -> Option) -> Update State ()
updateOption sRef bId oId f =
  updateBallot sRef bId $ \b -> b {bOptions = M.adjust f oId (bOptions b)}

updateArgument :: SwarmRef -> BallotId -> OptionId -> ArgumentId -> (Argument -> Argument) -> Update State ()
updateArgument sRef bId oId aId f = updateOption sRef bId oId $ \o -> o {oArguments = M.adjust f aId $ oArguments o}

askSwarm :: SwarmRef -> (Swarm -> a) -> Query State a
askSwarm sRef p = do
  ss <- asks stSwarms
  let s = fromMaybe (error "unknow swarm ref: ") $ M.lookup sRef ss
  pure $ p s

askBallot :: String -> SwarmRef -> BallotId -> UserId -> Query State (Maybe Ballot)
askBallot err sRef bId uId = do
  askSwarm
    sRef
    (fmap (projectBallot uId) .
     M.lookup bId . sBallots)

askBallot0 :: SwarmRef -> BallotId -> (Ballot -> a) -> Query State (Maybe a)
askBallot0 sRef bId p = askSwarm sRef (fmap p . M.lookup bId . sBallots)

data CreateBallotArgs = CreateBallotArgs
  { cbaTitle :: T.Text
  , cbaDescription :: T.Text
  } deriving (Generic, GQLType)

createBallot :: UserId -> SwarmRef -> UTCTime -> CreateBallotArgs -> Update State (WithId Ballot)
createBallot uId sRef now CreateBallotArgs {..} = do
  updateSwarm sRef $ \s -> s {sBallots = M.insert bId b (sBallots s)}
  pure $ WithId {wiId = bId, wiValue = b}
  where
    b =
      Ballot { bTitle = cbaTitle, bDescription = cbaDescription
             , bCreator = uId
             , bCreationDate = T.pack $ show now
             , bDelegations = S.empty
             , bOptions = M.empty}
    bId = mkId b

newtype DeleteBallotArgs = DeleteBallotArgs {
  dbaBallotId :: BallotId
} deriving (Generic, GQLType)

deleteBallot :: UserId -> SwarmRef -> DeleteBallotArgs -> Update State Bool
deleteBallot uId sRef DeleteBallotArgs {dbaBallotId} = do
  bs <- liftQuery $ askSwarm sRef sBallots
  let ballotM = M.lookup dbaBallotId bs
  if fmap bCreator ballotM == Just uId && fmap totalVotes ballotM == Just 0
    then do
      updateSwarm sRef $ \s -> s {sBallots = M.delete dbaBallotId $ sBallots s}
      pure True
    else pure False
  where
    totalVotes Ballot {bOptions} =
      sum
        [ fromEnum vStars
        | Option {oVotes} <- M.elems bOptions
        , Vote {vStars} <- M.elems oVotes
        ]

data CreateVoteArgs = CreateVoteArgs
  { cvaBallotId :: BallotId
  , cvaOptionId :: OptionId
  , cvaStars :: NrOfStars
  } deriving (Generic, GQLType)

createVote :: UserId -> SwarmRef -> CreateVoteArgs -> Update State Bool
createVote uId sRef CreateVoteArgs {..} = do
  updateBallot sRef cvaBallotId $
    \b -> b {bDelegations = S.filter (\d -> dUserId d /= uId) $ bDelegations b}
  updateOption sRef cvaBallotId cvaOptionId $
    \o -> o {oVotes = M.insert uId v $ oVotes o}
  pure True
  where
    v =
      Vote
        { vUserId = uId
        , vStars = cvaStars
        }

data CreateDelegationArgs = CreateDelegationArgs
  { dmaDelegateeId :: UserId
  , dmaBallotId :: BallotId
  } deriving (Generic, GQLType)

createDelegation :: UserId -> SwarmRef -> CreateDelegationArgs -> Update State Bool
createDelegation uId sRef CreateDelegationArgs {..} = do
  updateBallot sRef dmaBallotId $
    \b -> b { bDelegations = S.insert d $ bDelegations b
            , bOptions = M.map (\o -> o {oVotes = M.delete uId $ oVotes o}) $ bOptions b
            }
  pure True
  where
    d = Delegation {dUserId = uId, dDelegatee = dmaDelegateeId}

data CreateOptionArgs = CreateOptionArgs
  { coaTitle :: T.Text
  , coaDescription :: T.Text
  , coaBallotId :: BallotId
  } deriving (Generic, GQLType)

createOption :: UserId -> SwarmRef -> CreateOptionArgs -> Update State OptionId
createOption uId sRef CreateOptionArgs {..} = do
  updateBallot sRef coaBallotId $ \b -> b {bOptions = M.insert oId o $ bOptions b}
  pure oId
  where
    o =
      Option
        { oTitle = coaTitle
        , oDescription = coaDescription
        , oUserId = uId
        , oArguments = M.empty
        , oVotes = M.empty
        }
    oId = mkId o

data DeleteOptionArgs =
  DeleteOptionArgs
    { doaBallotId :: BallotId
    , doaOptionId :: OptionId
    }
  deriving (Generic, GQLType, Show)

deleteOption :: UserId -> SwarmRef -> DeleteOptionArgs -> Update State Bool
deleteOption uId sRef args@DeleteOptionArgs {doaBallotId, doaOptionId} = do
  bM <- liftQuery $ askBallot ("deleteOption: " <> show args) sRef doaBallotId uId
  let optM = bM >>= \b -> M.lookup doaOptionId (bOptions b)
  if fmap oUserId optM == Just uId && fmap totalVotes optM == Just 0
    then do
      updateBallot sRef doaBallotId $ \b ->
        b {bOptions = M.delete doaOptionId $ bOptions b}
      pure True
    else pure False
  where
    totalVotes :: Option -> Int
    totalVotes Option {oVotes} =
      sum [fromEnum vStars | Vote {vStars} <- M.elems oVotes]


data CreateArgumentArgs = CreateArgumentArgs
  { caaBallotId :: BallotId
  , caaOptionId :: OptionId
  , caaText :: T.Text
  , caaProContra :: IProContra
  , caaUserId :: UserId
  } deriving (Show, Eq, Generic, GQLType)

createArgument ::
     UserId -> SwarmRef -> CreateArgumentArgs -> Update State Bool
createArgument _uId sRef CreateArgumentArgs {..} = do
  updateOption sRef caaBallotId caaOptionId $ \o -> o {oArguments = M.insert aId a $ oArguments o}
  pure True
  where
    a =
      Argument
        { aUserId = caaUserId
        , aText = caaText
        , aProContra =
            if caaProContra == IPro
              then Pro
              else Contra
        , aUpVotes = S.empty
        }
    aId = mkId a

data CreateUpVoteArgs = CreateUpVoteArgs
  { cuaBallotId :: BallotId
  , cuaOptionId :: OptionId
  , cuaArgumentId :: ArgumentId
  } deriving (Show, Eq, Generic, GQLType)

createUpVote :: UserId -> SwarmRef -> CreateUpVoteArgs -> Update State Bool
createUpVote uId sRef CreateUpVoteArgs {..} = do
  updateArgument sRef cuaBallotId cuaOptionId cuaArgumentId $ \a -> a {aUpVotes = S.insert uv $ aUpVotes a}
  pure True
  where
    uv = UpVote {uvUserId = uId}

data DeleteUpVoteArgs = DeleteUpVoteArgs
  { duaBallotId :: BallotId
  , duaOptionId :: OptionId
  , duaArgumentId :: ArgumentId
  } deriving (Show, Eq, Generic, GQLType)

deleteUpVote :: UserId -> SwarmRef -> DeleteUpVoteArgs -> Update State Bool
deleteUpVote uId sRef DeleteUpVoteArgs {..} = do
  updateArgument sRef duaBallotId duaOptionId duaArgumentId $ \a -> a {aUpVotes = S.filter (\uv -> uvUserId uv /= uId) $ aUpVotes a}
  pure True

newtype CreateSwarmArgs =
  CreateSwarmArgs
    { csaName :: T.Text
    }
  deriving (Generic, GQLType)

(+++) :: Ord a => [a] -> [a] -> [a]
(+++) xs ys = nubSort $ xs ++ ys

createSwarm :: UserId -> UserName -> CreateSwarmArgs -> Update State SwarmRef
createSwarm uId uName CreateSwarmArgs {..} = do
  sRef <- mkRef sw
  modify
    (\s ->
       s
         { stSwarms = M.insert sRef sw $ stSwarms s
         , stUserSwarms =
             M.insertWith (+++) uId [WithRef sRef csaName] $ stUserSwarms s
         })
  createFish uId uName
  pure sRef
  where
    sw = newSwarm uId csaName

createFish :: UserId -> UserName -> Update State ()
createFish uId uName = do
  fish <- gets stFish
  unless (M.member uId fish) $
    modify $ \st ->
      st
        { stFish =
            M.insert
              uId
              (Fish
                 { fUserId = uId
                 , fName = uName
                 , fProfile = "Write your profile with markdown here!"
                 , fFcmToken = Nothing
                 }) $
            stFish st
        , stUserSwarms = M.insert uId [publicSwarmName] $ stUserSwarms st
        , stSwarms =
            M.adjust (\s -> s {sFish = S.insert uId $ sFish s}) publicRef $
            stSwarms st
        }


newtype CreateInviteArgs =
  CreateInviteArgs
    { ciaToken :: T.Text
    }
  deriving (Generic, GQLType)

createInvite :: SwarmRef -> CreateInviteArgs -> Update State Bool
createInvite sRef CreateInviteArgs {..} = do
  modify $ \s -> s {stInvites = M.insert ciaToken sRef $ stInvites s}
  return True

newtype TakeInviteArgs =
  TakeInviteArgs
    { tiaToken :: T.Text
    }
  deriving (Generic, GQLType)

takeInvite ::
     UserId -> UserName -> TakeInviteArgs -> Update State (Maybe SwarmRef)
takeInvite uId uName TakeInviteArgs {..} = do
  ivs <- gets stInvites
  ss <- gets stSwarms
  case M.lookup tiaToken ivs of
    Nothing -> pure Nothing
    Just sRef -> do
      case M.lookup sRef ss of
        Just swarm -> do
          modify $ \s ->
            s
              { stInvites = M.delete tiaToken $ stInvites s
              , stUserSwarms =
                  M.insertWith (+++) uId [WithRef sRef (sName swarm)] $
                  stUserSwarms s
              , stSwarms =
                  M.insert sRef swarm {sFish = S.insert uId $ sFish swarm} $
                  stSwarms s
              }
          createFish uId uName
          pure $ Just sRef
        Nothing -> pure Nothing


newtype UpdateProfileArgs =
  UpdateProfileArgs
    { upaBody :: T.Text
    }
  deriving (Generic, GQLType)

updateProfile :: UserId -> UserName -> UpdateProfileArgs -> Update State Bool
updateProfile uId uName UpdateProfileArgs {..} = do
  modify $ \st ->
    st {stFish = M.adjust (\f -> f {fProfile = upaBody}) uId $ stFish st}
  pure True

newtype UpdateFcmTokenArgs =
  UpdateFcmTokenArgs
    { uftToken :: T.Text
    }
  deriving (Generic, GQLType)

updateFcmToken :: UserId -> UpdateFcmTokenArgs -> Update State Bool
updateFcmToken uId UpdateFcmTokenArgs {uftToken} = do
  modify $ \st ->
    st {stFish = M.adjust (\f -> f {fFcmToken = Just uftToken}) uId $ stFish st}
  pure True

computeResult ::
     [Delegation] -> M.Map OptionId Option -> Int -> BallotResult
computeResult dels opts nrOfFish =
  BallotResult
    { brVotes =
        M.fromList
          [ (optId, total)
          | (optId, opt) <- options
          , let total =
                  foldl
                    (\acc v ->
                       acc +
                       fromEnum (vStars v) *
                       (1 +
                        length
                          [ d
                          | d@Delegation {..} <- dels
                          , dDelegatee == vUserId v
                          ]))
                    0
                    (oVotes opt)
          ]
    , brTurnout =
        round $ 1_000.0 * fromIntegral totalVotes / fromIntegral nrOfFish
    , brOptions = [WithId oId o | (oId, o) <- options]
    }
  where
    options = M.toList opts
    allVotes = concatMap M.elems [oVotes o | (_oId, o) <- options]
    delegationsWithVotes =
      [d | d <- dels, dDelegatee d `S.member` S.fromList (map vUserId allVotes)]
    totalVotes =
      length (nubSort $ concatMap M.keys [oVotes o | (_oId, o) <- options]) +
      length delegationsWithVotes

-- | Query for unique ballot
newtype QueryBallotByIdArgs = QueryBallotByIdArgs
  { qbiBId :: BallotId
  } deriving (Generic, GQLType)


projectBallot :: UserId -> Ballot -> Ballot
projectBallot uId b =
  b
    { bDelegations = S.filter (\d -> dUserId d == uId) $ bDelegations b
    , bOptions =
        M.map
          (\o ->
             o
               { oVotes = M.restrictKeys (oVotes o) $ S.insert uId ds
               , oArguments =
                   M.map
                     (\a ->
                        a
                          { aUpVotes =
                              S.filter (\uv -> uvUserId uv == uId) $ aUpVotes a
                          }) $
                   oArguments o
               }) $
        bOptions b
    }
  where
    ds = S.map dDelegatee $ bDelegations b


queryUserFcmTokens :: SwarmRef -> Query State (T.Text, [T.Text])
queryUserFcmTokens sRef = do
  ss <- asks stSwarms
  fs <- asks stFish
  let s =
        fromMaybe (error $ "Failed to lookup swarm " ++ show sRef) $
        M.lookup sRef ss
  pure
    ( sName s
    , [t | f <- M.elems $ M.restrictKeys fs (sFish s), Just t <- [fFcmToken f]])

queryBallotById ::
     UserId -> SwarmRef -> QueryBallotByIdArgs -> Query State (Maybe Ballot)
queryBallotById uId sRef QueryBallotByIdArgs {..} = do
  bs <- askSwarm sRef sBallots
  pure $ projectBallot uId <$> M.lookup qbiBId bs

queryAllBallots :: UserId -> SwarmRef -> Query State [BallotId]
queryAllBallots uId sRef = do
  bs <- askSwarm sRef sBallots
  pure $
    map fst $
    sortBy
      (\b1 b2 ->
         flipOrder
           (comparing
              ((read :: String -> UTCTime) . T.unpack . bCreationDate . snd)
              b1
              b2)) $
    [(bId, projectBallot uId b) | (bId, b) <- M.toList bs]

newtype QueryBallotResultArgs =
  QueryBallotResultArgs
    { qbrBId :: BallotId
    }
  deriving (Generic, GQLType)

queryBallotResult ::
     UserId -> SwarmRef -> QueryBallotResultArgs -> Query State BallotResult
queryBallotResult _uId sRef QueryBallotResultArgs {..} = do
  fish <- askSwarm sRef sFish
  delsM <- askBallot0 sRef qbrBId bDelegations
  optionsM <- askBallot0 sRef qbrBId bOptions
  pure $ computeResult (maybe [] S.toList delsM) (fromMaybe M.empty optionsM) (S.size fish)

newtype QueryNrOptionsArgs =
  QueryNrOptionsArgs
    { qaoBId :: BallotId
    }
  deriving (Generic, GQLType, Show)

queryAllOptions :: UserId -> SwarmRef -> QueryNrOptionsArgs -> Query State [OptionId]
queryAllOptions uId sRef arg@QueryNrOptionsArgs {qaoBId} = do
  bM <- askBallot ("queryAllOptions: " <> show arg) sRef qaoBId uId
  pure $  maybe [] (M.keys . bOptions) bM

data QueryOptionArgs =
  QueryOptionArgs
    { qobBId :: BallotId
    , qobOId :: OptionId
    }
  deriving (Generic, GQLType, Show)

queryOption ::
     UserId
  -> SwarmRef
  -> QueryOptionArgs
  -> Query State (Maybe Option)
queryOption uId sRef arg@QueryOptionArgs {..} = do
  bM <- askBallot ("queryOption" <> show arg) sRef qobBId uId
  pure $ M.lookup qobOId . bOptions =<< bM

data QueryAllArgumentsArgs =
  QueryAllArgumentsArgs
    { qaaBId :: BallotId
    , qaaOId :: OptionId
    , qaaProContra :: IProContra
    }
  deriving (Generic, GQLType, Show)

queryAllArguments ::
     UserId
  -> SwarmRef
  -> QueryAllArgumentsArgs
  -> Query State [ArgumentId]
queryAllArguments uId sRef arg@QueryAllArgumentsArgs {..} = do
  bM <- askBallot ("queryAllArguments" <> show arg) sRef qaaBId uId
  let allArgs =
        map (uncurry WithId) $
        M.toList $ maybe M.empty oArguments $ M.lookup qaaOId . bOptions =<< bM
  let h = \case
        IPro -> Pro
        IContra -> Contra
  let proContracArgs proContra =
        map wiId $
        sortByUpVotes $
        filter (\x -> aProContra (wiValue x) == h proContra) allArgs
  pure (proContracArgs qaaProContra)

data QueryArgumentByIdArgs =
  QueryArgumentByIdArgs
    { qabBId :: BallotId
    , qabOId :: OptionId
    , qabAId :: ArgumentId
    }
  deriving (Generic, GQLType, Show)

queryArgumentById ::
     UserId -> SwarmRef -> QueryArgumentByIdArgs -> Query State (Maybe Argument)
queryArgumentById uId sRef arg@QueryArgumentByIdArgs {..} = do
  bM <- askBallot ("queryArgumentById:" <> show arg) sRef qabBId uId
  pure $ M.lookup qabAId . oArguments =<< M.lookup qabOId . bOptions =<< bM

queryUserSwarms :: UserId -> Query State [WithRef T.Text]
queryUserSwarms uId = do
  ss <- asks stUserSwarms
  pure $ fromMaybe [] (M.lookup uId ss)

queryFish :: UserId -> SwarmRef -> Query State [Fish]
queryFish _uId sRef = do
  ss <- asks stSwarms
  fish <- asks stFish
  let uIds = maybe S.empty sFish $ M.lookup sRef ss
  pure $ M.elems $ M.restrictKeys fish uIds

newtype QueryFishByUserIdArgs =
  QueryFishByUserIdArgs
    { qfUserId :: T.Text
    }
  deriving (Generic, GQLType)

queryFishByUserId :: UserId -> QueryFishByUserIdArgs -> Query State (Maybe Fish)
queryFishByUserId _uId QueryFishByUserIdArgs {..} = do
  ps <- asks stFish
  pure $ M.lookup qfUserId ps

----------------------------------------------------------------------------------------------------
-- acid


$(deriveSafeCopy 0 'base ''WithRef)
$(deriveSafeCopy 0 'base ''WithId)
$(deriveSafeCopy 0 'base ''State)
$(deriveSafeCopy 0 'base ''Ballot)
$(deriveSafeCopy 0 'base ''BallotResult)
$(deriveSafeCopy 0 'base ''NrOfStars)
$(deriveSafeCopy 0 'base ''Vote)
$(deriveSafeCopy 0 'base ''Invite)
$(deriveSafeCopy 0 'base ''Argument)
$(deriveSafeCopy 0 'base ''ProContra)
$(deriveSafeCopy 0 'base ''IProContra)
$(deriveSafeCopy 0 'base ''Option)
$(deriveSafeCopy 0 'base ''Delegation)
$(deriveSafeCopy 0 'base ''Fish)
$(deriveSafeCopy 0 'base ''Swarm)
$(deriveSafeCopy 0 'base ''QueryBallotByIdArgs)
$(deriveSafeCopy 0 'base ''QueryBallotResultArgs)
$(deriveSafeCopy 0 'base ''QueryAllArgumentsArgs)
$(deriveSafeCopy 0 'base ''QueryArgumentByIdArgs)
$(deriveSafeCopy 0 'base ''QueryFishByUserIdArgs)
$(deriveSafeCopy 0 'base ''CreateBallotArgs)
$(deriveSafeCopy 0 'base ''DeleteBallotArgs)
$(deriveSafeCopy 0 'base ''CreateUpVoteArgs)
$(deriveSafeCopy 0 'base ''DeleteUpVoteArgs)
$(deriveSafeCopy 0 'base ''CreateOptionArgs)
$(deriveSafeCopy 0 'base ''DeleteOptionArgs)
$(deriveSafeCopy 0 'base ''CreateSwarmArgs)
$(deriveSafeCopy 0 'base ''CreateInviteArgs)
$(deriveSafeCopy 0 'base ''TakeInviteArgs)
$(deriveSafeCopy 0 'base ''UpdateProfileArgs)
$(deriveSafeCopy 0 'base ''QueryNrOptionsArgs)
$(deriveSafeCopy 0 'base ''QueryOptionArgs)
$(deriveSafeCopy 0 'base ''CreateVoteArgs)
$(deriveSafeCopy 0 'base ''CreateArgumentArgs)
$(deriveSafeCopy 0 'base ''CreateDelegationArgs)
$(deriveSafeCopy 0 'base ''UpVote)
$(deriveSafeCopy 0 'base ''UpdateFcmTokenArgs)
$(makeAcidic
    ''State
    [ 'createBallot
    , 'createFish
    , 'deleteBallot
    , 'createVote
    , 'createDelegation
    , 'createArgument
    , 'createOption
    , 'deleteOption
    , 'createUpVote
    , 'deleteUpVote
    , 'createSwarm
    , 'createInvite
    , 'takeInvite
    , 'updateProfile
    , 'queryBallotById
    , 'queryBallotResult
    , 'queryAllOptions
    , 'queryOption
    , 'queryAllArguments
    , 'queryArgumentById
    , 'queryUserSwarms
    , 'queryFish
    , 'queryFishByUserId
    , 'queryAllBallots
    , 'queryUserFcmTokens
    , 'updateFcmToken
    ])
$(makeAcidic ''Option [])

----------------------------------------------------------------------------------------------------
-- IO

main :: IO ()
main = do
  (conf :: Config Snap a) <-
    extendedCommandLineConfig
      (serviceAccountFileLocationArg : dbLocationArg : optDescrs defaultConfig)
      (<>)
      defaultConfig
  bracket
    (initialize $ getOther conf)
    shutDown
    (httpServe conf . CORS.applyCORS CORS.defaultOptions . site)

data Resources = Resources
  { rState :: AcidState State
  , rServerApp :: ServerApp
  , rHttpApp :: BSL.ByteString -> Snap BSL.ByteString
  }

data AcidConf =
  AcidConf
    { acStateFile :: Maybe FilePath
    , acServiceAccountFile :: Maybe FilePath
    }
data AcidConf0 =
  AcidConf0
    { acStateFile0 :: FilePath
    , acServiceAccountFile0 :: FilePath
    }

instance Semigroup AcidConf where
  c1 <> c2 =
    AcidConf
      { acStateFile = acStateFile c1 `overwriteConf` acStateFile c2
      , acServiceAccountFile = acServiceAccountFile c1 `overwriteConf` acServiceAccountFile c2
      }
instance Monoid AcidConf where
  mempty = AcidConf Nothing Nothing

overwriteConf :: Maybe a -> Maybe a -> Maybe a
overwriteConf aM bM = case (aM, bM) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just b) -> Just b
  (Just a, Nothing) -> Just a
  (Just a, Just b) -> Just b

defaultAcidConf =
  AcidConf
    { acStateFile = Just "db"
    , acServiceAccountFile = Just "service-account.json"
    }

completeAcidConf :: AcidConf -> AcidConf0
completeAcidConf c =
  AcidConf0
    { acStateFile0 =
        fromMaybe (error "internal error: completeAcidConf") acStateFile
    , acServiceAccountFile0 =
        fromMaybe
          (error "internal error: completeAcidConf")
          acServiceAccountFile
    }
  where
    AcidConf {..} = defaultAcidConf <> c

dbLocationArg :: Opt.OptDescr (Maybe (Config Snap AcidConf))
dbLocationArg =
  Opt.Option
    []
    ["db"]
    (Opt.OptArg
       (\fpM -> Just $ setOther mempty {acStateFile = fpM} emptyConfig)
       "PATH")
    "The location of the database state file."

serviceAccountFileLocationArg :: Opt.OptDescr (Maybe (Config Snap AcidConf))
serviceAccountFileLocationArg =
  Opt.Option
    []
    ["service-account-file"]
    (Opt.OptArg
       (\fpM -> Just $ setOther mempty {acServiceAccountFile = fpM} emptyConfig)
       "PATH")
    "The location of the google service account .json file."

initialize :: Maybe AcidConf -> IO Resources
initialize acidConfM = do
  let AcidConf0 {..} = completeAcidConf $ fromMaybe defaultAcidConf acidConfM
  rState <- openLocalStateFrom acStateFile0 emptyState
  rCerts <- startCertificateUpdateThread
  rToken <- FCM.startServiceAuthUpdateThread acServiceAccountFile0
  let app = M.deriveApp $ rootResolver rState rToken rCerts
  (rServerApp, rHttpApp) <- Snap.evalHandler (Snap.get "/" mempty) $ do
              (rServerApp, publish) <- webSocketsApp app
              let rHttpApp = httpPubApp [publish] app
              pure (rServerApp, rHttpApp)
  pure $ Resources {..}

shutDown :: Resources -> IO ()
shutDown Resources{rState} = closeAcidState rState

site :: Resources -> Snap ()
site Resources {rState, rServerApp, rHttpApp} = do
  rq <- getRequest
  liftIO $ putStrLn "new request"
  liftIO $ print rq
  route
    [ ("/api/query", gqlQueryHandler rHttpApp)
    , ("/api/subscribe", runWebSocketsSnap rServerApp)
    ]

gqlQueryHandler :: (BSL.ByteString -> Snap BSL.ByteString) -> Snap ()
gqlQueryHandler app = do
  rqBody <- readRequestBody 10000
  logError "request: "
  logError $ BSL.toStrict rqBody
  r <- app rqBody
  writeLBS r
  logError "response: "
  logError $ BSL.toStrict r
  logError "done with request"

----------------------------------------------------------------------------------------------------
-- Snap utilities

getUserId0 ::
     AcidState State
  -> IORef Certificates
  -> Snap (UserId, UserName, SwarmRef)
getUserId0 _ _ = pure ("drsk", "drsk", "public")

-- Get the user id from the authorization context.
-- Note: This function needs to check that the user id is member of the given swarm!
getUserId ::
     AcidState State
  -> IORef Certificates
  -> Snap (UserId, UserName, SwarmRef)
getUserId st certsRef = do
  token <- getMandatoryHeader "authorization"
  certs <- liftIO $ readIORef certsRef
  jwsOrErr <- liftIO $ validateToken certs token
  (_jwsHeader, jwsContent) <- unauthorizedIfErr jwsOrErr
  now <- liftIO getCurrentTime
  jws <-
    unauthorizedIfErr $ do
      jws0 <- eitherDecode $ BSL.fromStrict jwsContent
      case validateJws jws0 now of
        Nothing -> Right jws0
        Just errs -> Left $ unlines errs
  swarmRefBs <- getMandatoryHeader "swarm"
  let uId = sub jws
  let uName = fromMaybe uId $ name jws
  liftIO $ update st $ CreateFish uId uName
  let sRef = decodeUtf8 swarmRefBs
  ss <- liftIO $ query st $ QueryUserSwarms uId
  if sRef `S.member` S.fromList (map wrRef ss)
    then pure (uId, uName, sRef)
    else do
      liftIO $
        putStrLn $
        "user id " <>
        show uId <> "is not member of the given swarm " <> show swarmRefBs
      unauthorized

unauthorizedIfErr :: Either String a -> Snap a
unauthorizedIfErr =
  \case
    Left err -> do
      liftIO $ putStrLn "Unauthorized"
      liftIO $ print err
      unauthorized
    Right a -> pure a

unauthorized :: Snap a
unauthorized = do
  liftIO $ putStrLn "Unauthorized"
  modifyResponse $ setResponseStatus 401 "Unauthorized"
  getResponse >>= finishWith
  pass

getMandatoryHeader :: BS.ByteString -> Snap BS.ByteString
getMandatoryHeader name = do
  mbHeader <- getOptionalHeader name
  mbMissingHeaderParam name mbHeader

getOptionalHeader :: BS.ByteString -> Snap (Maybe BS.ByteString)
getOptionalHeader name = do
  headers <- getsRequest headers
  pure $ getHeader (CI.mk name) headers

mbMissingHeaderParam :: BS.ByteString -> Maybe BS.ByteString -> Snap BS.ByteString
mbMissingHeaderParam paramName =
  \case
    Nothing -> do
      liftIO $ putStrLn $ "missing header " <> show paramName
      modifyResponse $
        setResponseStatus 400 ("Missing mandatory header/parameter: " <> paramName)
      getResponse >>= finishWith
      pass
    Just a -> pure a

internalError :: String -> Snap ()
internalError err = do
  liftIO $ putStrLn $ "internal error: " <> err
  modifyResponse $ setResponseStatus 500 "Internal Server Error"
  writeBS "500 error"
  r <- getResponse
  finishWith r

----------------------------------------------------------------------------------------------------
-- Graphql

data GQLQuery m = GQLQuery
  { qAllBallots :: m [BallotId]
  , qBallotById :: QueryBallotByIdArgs -> m (Maybe Ballot)
  , qBallotResult :: QueryBallotResultArgs -> m BallotResult
  , qAllOptions :: QueryNrOptionsArgs -> m [OptionId]
  , qOption :: QueryOptionArgs -> m (Maybe Option)
  , qAllArguments :: QueryAllArgumentsArgs -> m [ArgumentId]
  , qArgumentById :: QueryArgumentByIdArgs -> m (Maybe Argument)
  , qUserSwarms :: m [WithRef T.Text]
  , qFish :: m [Fish]
  , qFishByUserId:: QueryFishByUserIdArgs -> m (Maybe Fish)
  } deriving (Generic, GQLType)

newtype GQLQuery0 m =
  GQLQuery0
    { qUserSwarms0 :: m [WithRef T.Text]
    }
  deriving (Generic, GQLType)

data GQLMutation m = GQLMutation
  { mCreateBallot :: CreateBallotArgs -> m BallotId
  , mDeleteBallot :: DeleteBallotArgs -> m Bool
  , mCreateVote :: CreateVoteArgs -> m Bool
  , mCreateDelegation :: CreateDelegationArgs -> m Bool
  , mCreateArgument :: CreateArgumentArgs -> m Bool
  , mCreateUpVote :: CreateUpVoteArgs -> m Bool
  , mDeleteUpVote :: DeleteUpVoteArgs -> m Bool
  , mCreateOption :: CreateOptionArgs -> m OptionId
  , mDeleteOption :: DeleteOptionArgs -> m Bool
  , mCreateSwarm :: CreateSwarmArgs -> m SwarmRef
  , mCreateInvite :: CreateInviteArgs -> m Bool
  , mTakeInvite :: TakeInviteArgs -> m (Maybe SwarmRef)
  , mUpdateProfile :: UpdateProfileArgs -> m Bool
  , mUpdateFcmToken :: UpdateFcmTokenArgs -> m Bool
  } deriving (Generic, GQLType)


data Channel
  =  ChannelArguments
  | ChannelBallots
  | ChannelResults
  | ChannelOptions
  deriving (Generic, Eq, Show, Hashable)



data Content
  = CCreateVote
  | CCreateSwarm
  | CCreateBallot
  | CDeleteBallot
  | CCreateUpVote
  | CDeleteUpVote
  | CCreateOption
  | CDeleteOption
  | CCreateArgument
  | CPropagateDelegation
  | CCreateDelegation
  deriving Show

type APIEvent = Event Channel Content

data GQLSubscription (m :: * -> *) =
  GQLSubscription
    { sAllBallots :: SubscriptionField (ResolverS APIEvent Snap [BallotId])
    , sBallotResult :: QueryBallotResultArgs -> SubscriptionField (ResolverS APIEvent Snap BallotResult)
    , sBallotById :: QueryBallotByIdArgs -> SubscriptionField (ResolverS APIEvent Snap (Maybe Ballot))
    , sAllOptions :: QueryNrOptionsArgs -> SubscriptionField (ResolverS APIEvent Snap [OptionId])
    , sAllArguments :: QueryAllArgumentsArgs -> SubscriptionField (ResolverS APIEvent Snap [ArgumentId])
    , sArgumentById :: QueryArgumentByIdArgs -> SubscriptionField (ResolverS APIEvent Snap (Maybe Argument))
    , sOption :: QueryOptionArgs -> SubscriptionField (ResolverS APIEvent Snap (Maybe Option))
    }
  deriving (Generic, GQLType)

-- | API served when user id and swarm ref are given.
rootResolver ::
     AcidState State
  -> IORef FCM.Token
  -> IORef Certificates
  -> RootResolver Snap APIEvent GQLQuery GQLMutation GQLSubscription
rootResolver st tokRef certRef =
  RootResolver
    { queryResolver =
        GQLQuery
          { qAllBallots = resolveQAllBallots st certRef
          , qAllOptions = resolveQAllOptions st certRef
          , qBallotById = resolveQBallotById st certRef
          , qBallotResult = resolveQBallotResult st certRef
          , qOption = resolveQOption st certRef
          , qAllArguments = resolveQAllArguments st certRef
          , qArgumentById = resolveQArgumentById st certRef
          , qUserSwarms = resolveQUserSwarms st certRef
          , qFish = resolveQFish st certRef
          , qFishByUserId = resolveQFishByUserId st certRef
          }
    , mutationResolver =
        GQLMutation
          { mCreateBallot = resolveMCreateBallot st tokRef certRef
          , mDeleteBallot = resolveMDeleteBallot st certRef
          , mCreateVote = resolveMCreateVote st certRef
          , mCreateDelegation = resolveMCreateDelegation st certRef
          , mCreateArgument = resolveMCreateArgument st tokRef certRef
          , mCreateUpVote = resolveMCreateUpVote st certRef
          , mDeleteUpVote = resolveMDeleteUpVote st certRef
          , mCreateOption = resolveMCreateOption st tokRef certRef
          , mDeleteOption = resolveMDeleteOption st certRef
          , mCreateSwarm = resolveMCreateSwarm st certRef
          , mCreateInvite = resolveMCreateInvite st certRef
          , mTakeInvite = resolveMTakeInvite st certRef
          , mUpdateProfile = resolveMUpdateProfile st certRef
          , mUpdateFcmToken = resolveMUpdateFcmToken st certRef
          }
    , subscriptionResolver =
        GQLSubscription
          { sAllBallots = resolveSAllBallots st certRef
          , sAllArguments =
              resolveSAllArguments st certRef
          , sArgumentById = resolveSArgumentById st certRef
          , sBallotResult = resolveSBallotResult st certRef
          , sBallotById = resolveSBallotById st certRef
          , sAllOptions = resolveSAllOptions st certRef
          , sOption = resolveSOption st certRef
          }
    }

resolveQAllBallots ::
     AcidState State
  -> IORef Certificates
  -> ResolverQ APIEvent Snap [BallotId]
resolveQAllBallots st certRef = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryAllBallots uId sRef

resolveQBallotById ::
     AcidState State
  -> IORef Certificates
  -> QueryBallotByIdArgs
  -> ResolverQ APIEvent Snap (Maybe Ballot)
resolveQBallotById st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryBallotById uId sRef args

resolveQBallotResult ::
     AcidState State
  -> IORef Certificates
  -> QueryBallotResultArgs
  -> ResolverQ APIEvent Snap BallotResult
resolveQBallotResult st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryBallotResult uId sRef args

resolveQOption ::
     AcidState State
  -> IORef Certificates
  -> QueryOptionArgs
  -> ResolverQ APIEvent Snap (Maybe Option)
resolveQOption st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryOption uId sRef args

resolveQAllOptions ::
     AcidState State
  -> IORef Certificates
  -> QueryNrOptionsArgs
  -> ResolverQ APIEvent Snap [OptionId]
resolveQAllOptions st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryAllOptions uId sRef args

resolveQAllArguments ::
     AcidState State
  -> IORef Certificates
  -> QueryAllArgumentsArgs
  -> ResolverQ APIEvent Snap [ArgumentId]
resolveQAllArguments st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryAllArguments uId sRef args

resolveQArgumentById ::
     AcidState State
  -> IORef Certificates
  -> QueryArgumentByIdArgs
  -> ResolverQ APIEvent Snap (Maybe Argument)
resolveQArgumentById st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryArgumentById uId sRef args

resolveQUserSwarms ::
     AcidState State
  -> IORef Certificates
  -> ResolverQ APIEvent Snap [WithRef T.Text]
resolveQUserSwarms st certRef = do
  (uId, _uName, _sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryUserSwarms uId

resolveQFish ::
     AcidState State -> IORef Certificates -> ResolverQ APIEvent Snap [Fish]
resolveQFish st certRef = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryFish uId sRef

resolveQFishByUserId ::
     AcidState State
  -> IORef Certificates
  -> QueryFishByUserIdArgs
  -> ResolverQ APIEvent Snap (Maybe Fish)
resolveQFishByUserId st certRef args = do
  (uId, _uName, _sRef) <- lift $ getUserId st certRef
  liftIO $ query st $ QueryFishByUserId uId args

resolveMCreateBallot ::
     AcidState State
  -> IORef FCM.Token
  -> IORef Certificates
  -> CreateBallotArgs
  -> ResolverM APIEvent Snap BallotId
resolveMCreateBallot st tokRef certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  b <-
    liftIO $ do
      now <- getCurrentTime
      b <- update st $ CreateBallot uId sRef now args
      notifySwarm st tokRef sRef $ \swarmName token ->
        FCM.Message
          { FCM.name = "NewBallotNotification"
          , FCM.notification =
              FCM.Notification
                { FCM.title = "New ballot for " <> swarmName
                , FCM.body = cbaTitle args
                }
          , FCM.token = token
          }
      pure b
  liftIO $ putStrLn "Creating CreateBallot event"
  publish [Event [ChannelBallots] CCreateBallot]
  liftIO $ putStrLn "published CCreateBallot"
  pure $ wiId b

notifySwarm ::
     AcidState State
  -> IORef FCM.Token
  -> SwarmRef
  -> (T.Text -> FCM.Token -> FCM.Message)
  -> IO ()
notifySwarm st tokRef sRef msg = do
  (swarmName, ts) <- query st $ QueryUserFcmTokens sRef
  req <-
    Http.parseRequest $
    "POST https://fcm.googleapis.com/v1/projects/" ++
    firebaseProjectName ++ "/messages:send"
  auth <- readIORef tokRef
  let req1 =
        Http.setRequestHeader
          "authorization"
          ["Bearer " <> T.encodeUtf8 auth]
          req
  forM_ ts $ \token -> do
    let req2 =
          Http.setRequestBodyJSON
            (toJSON $
             FCM.FcmMessage {FCM.validate_only = False, FCM.message = msg swarmName token})
            req1
    res <- Http.httpBS req2
    unless (Http.getResponseStatus res == ok200) $
      putStrLn $ "Failed to notify swarm " <> T.unpack sRef <> " : " <> show res

resolveMDeleteBallot ::
     AcidState State
  -> IORef Certificates
  -> DeleteBallotArgs
  -> ResolverM APIEvent Snap Bool
resolveMDeleteBallot st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  now <- liftIO getCurrentTime
  b <- liftIO $ update st $ DeleteBallot uId sRef args
  publish [Event [ChannelBallots] CDeleteBallot]
  liftIO $ putStrLn "published CDeleteBallot"
  pure b

resolveMCreateVote ::
     AcidState State
  -> IORef Certificates
  -> CreateVoteArgs
  -> ResolverM APIEvent Snap Bool
resolveMCreateVote st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  b <- liftIO $ update st $ CreateVote uId sRef args
  publish [Event [ChannelResults] CCreateVote]
  liftIO $ putStrLn "published CCreateVote"
  pure b

-- TODO (drsk) write a function `then` to publish after database update
resolveMCreateDelegation ::
     AcidState State
  -> IORef Certificates
  -> CreateDelegationArgs
  -> ResolverM APIEvent Snap Bool
resolveMCreateDelegation st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  b <- liftIO $ update st $ CreateDelegation uId sRef args
  publish [Event [ChannelResults, ChannelBallots] CCreateDelegation]
  liftIO $ putStrLn "published CCreateDelegation"
  pure b

resolveMCreateArgument ::
     AcidState State
  -> IORef FCM.Token
  -> IORef Certificates
  -> CreateArgumentArgs
  -> ResolverM APIEvent Snap Bool
resolveMCreateArgument st tokRef certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  r <-
    liftIO $ do
      r <- update st $ CreateArgument uId sRef args
      notifySwarm st tokRef sRef $ \swarmName token ->
        FCM.Message
          { FCM.name = "NewArgumentNotification"
          , FCM.notification =
              FCM.Notification
                { FCM.title = "New argument for swarm " <> sRef
                , FCM.body = headDef "no title" $ T.lines $ caaText args
                }
          , FCM.token = token
          }
      pure r
  publish [Event [ChannelArguments, ChannelOptions] CCreateArgument]
  liftIO $ putStrLn "published CCreateArgument"
  pure r

resolveMCreateUpVote ::
     AcidState State
  -> IORef Certificates
  -> CreateUpVoteArgs
  -> ResolverM APIEvent Snap Bool
resolveMCreateUpVote st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  r <- liftIO $ update st $ CreateUpVote uId sRef args
  publish [Event [ChannelArguments] CCreateUpVote]
  liftIO $ putStrLn "published CCreateUpVote"
  pure r

resolveMDeleteUpVote ::
     AcidState State
  -> IORef Certificates
  -> DeleteUpVoteArgs
  -> ResolverM APIEvent Snap Bool
resolveMDeleteUpVote st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  b <- liftIO $ update st $ DeleteUpVote uId sRef args
  publish [Event [ChannelArguments] CDeleteUpVote]
  pure b

resolveMCreateOption ::
     AcidState State
  -> IORef FCM.Token
  -> IORef Certificates
  -> CreateOptionArgs
  -> ResolverM APIEvent Snap OptionId
resolveMCreateOption st tokRef certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  oId <- liftIO $ update st $ CreateOption uId sRef args
  publish [Event [ChannelOptions] CCreateOption]
  liftIO $ putStrLn "published CCreateOption"
  liftIO $ notifySwarm st tokRef sRef $ \swarmName token ->
    FCM.Message
      { FCM.name = "NewOptionNotification"
      , FCM.notification =
          FCM.Notification
            { FCM.title = "New option for swarm " <> sRef
            , FCM.body = coaTitle args
            }
      , FCM.token = token
      }
  pure oId

resolveMDeleteOption ::
     AcidState State
  -> IORef Certificates
  -> DeleteOptionArgs
  -> ResolverM APIEvent Snap Bool
resolveMDeleteOption st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  r <- liftIO $ update st $ DeleteOption uId sRef args
  publish [Event [ChannelOptions] CDeleteOption]
  liftIO $ putStrLn "published CDeleteOption"
  pure r


resolveMCreateSwarm ::
     AcidState State
  -> IORef Certificates
  -> CreateSwarmArgs
  -> ResolverM APIEvent Snap SwarmRef
resolveMCreateSwarm st certRef args = do
  (uId, uName, _sRef) <- lift $ getUserId st certRef
  liftIO $ update st $ CreateSwarm uId uName args

resolveMCreateInvite ::
     AcidState State
  -> IORef Certificates
  -> CreateInviteArgs
  -> ResolverM APIEvent Snap Bool
resolveMCreateInvite st certRef args = do
  (uId, _uName, sRef) <- lift $ getUserId st certRef
  liftIO $ update st $ CreateInvite sRef args

resolveMTakeInvite ::
     AcidState State
  -> IORef Certificates
  -> TakeInviteArgs
  -> ResolverM APIEvent Snap (Maybe SwarmRef)
resolveMTakeInvite st certRef args = do
  (uId, uName, sRef) <- lift $ getUserId st certRef
  liftIO $ update st $ TakeInvite uId uName args

resolveMUpdateProfile ::
     AcidState State
  -> IORef Certificates
  -> UpdateProfileArgs
  -> ResolverM APIEvent Snap Bool
resolveMUpdateProfile st certRef args = do
  (uId, uName, sRef) <- lift $ getUserId st certRef
  liftIO $ update st $ UpdateProfile uId uName args

resolveMUpdateFcmToken ::
     AcidState State
  -> IORef Certificates
  -> UpdateFcmTokenArgs
  -> ResolverM APIEvent Snap Bool
resolveMUpdateFcmToken st certRef args = do
  (uId, uName, sRef) <- lift $ getUserId st certRef
  liftIO $ update st $ UpdateFcmToken uId args

resolveSAllBallots ::
     AcidState State
  -> IORef Certificates
  -> SubscriptionField (ResolverS APIEvent Snap [BallotId])
resolveSAllBallots st certRef = do
  subscribe ChannelBallots $ do
    pure $ \(Event _ _c) -> queryBallots
      -- case c of
        -- CDeleteBallot -> do
          -- liftIO $ putStrLn "debug: CDeleteBallot"
          -- queryBallots
        -- CCreateBallot -> do
          -- liftIO $ putStrLn "debug: CCreateBallot"
          -- queryBallots
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sAllBallots " <> show other) >>
          -- pure []
  where
    queryBallots = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryAllBallots uId sRef

resolveSAllArguments ::
     AcidState State
  -> IORef Certificates
  -> QueryAllArgumentsArgs
  -> SubscriptionField (ResolverS APIEvent Snap [ArgumentId])
resolveSAllArguments st certRef args = do
  subscribe ChannelArguments $ do
    pure $ \(Event _ _c) -> queryArguments
      -- case c of
        -- CCreateArgument -> queryArguments
        -- CCreateUpVote -> queryArguments
        -- CDeleteUpVote -> queryArguments
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sAllArguments " <> show other) >>
          -- pure []
  where
    queryArguments = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryAllArguments uId sRef args

resolveSArgumentById ::
     AcidState State
  -> IORef Certificates
  -> QueryArgumentByIdArgs
  -> SubscriptionField (ResolverS APIEvent Snap (Maybe Argument))
resolveSArgumentById st certRef args =
  subscribe ChannelArguments $ do
    pure $ \(Event _ _c) -> queryArguments
      -- case c of
        -- CCreateArgument -> queryArguments
        -- CCreateUpVote -> queryArguments
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sArgumentById " <> show other) >>
          -- pure Nothing
  where
    queryArguments = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryArgumentById uId sRef args

resolveSBallotResult ::
     AcidState State
  -> IORef Certificates
  -> QueryBallotResultArgs
  -> SubscriptionField (ResolverS APIEvent Snap BallotResult)
resolveSBallotResult st certRef args = do
  subscribe ChannelResults $ do
    pure $ \(Event _ _c) -> queryResult
      -- case c of
        -- CCreateVote -> queryResult
        -- CCreateDelegation -> queryResult
        -- CCreateOption -> queryResult
        -- CDeleteOption -> queryResult
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sBallotResult " <> show other) >>
          -- pure emptyResult
  where
    queryResult = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryBallotResult uId sRef args

resolveSBallotById ::
     AcidState State
  -> IORef Certificates
  -> QueryBallotByIdArgs
  -> SubscriptionField (ResolverS APIEvent Snap (Maybe Ballot))
resolveSBallotById st certRef args = do
  subscribe ChannelBallots $ do
    pure $ \(Event _ _c) -> queryBallot
      -- case c of
        -- CCreateDelegation -> queryBallot
        -- CCreateBallot -> queryBallot
        -- CDeleteBallot -> queryBallot
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sBallotById " <> show other) >>
          -- pure Nothing
  where
    queryBallot = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryBallotById uId sRef args

resolveSOption ::
     AcidState State
  -> IORef Certificates
  -> QueryOptionArgs
  -> SubscriptionField (ResolverS APIEvent Snap (Maybe Option))
resolveSOption st certRef args = do
  subscribe ChannelOptions $ do
    pure $ \(Event _ _c) -> queryOption
      -- case c of
        -- CCreateArgument -> queryOption
        -- CCreateOption -> queryOption
        -- CDeleteOption -> queryOption
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sOption " <> show other) >>
          -- pure Nothing
  where
    queryOption = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryOption uId sRef args

resolveSAllOptions ::
     AcidState State
  -> IORef Certificates
  -> QueryNrOptionsArgs
  -> SubscriptionField (ResolverS APIEvent Snap [OptionId])
resolveSAllOptions st certRef args = do
  subscribe ChannelOptions $ do
    pure $ \(Event _ _c) -> queryOptions
      -- case c of
        -- CCreateOption -> queryOptions
        -- CDeleteOption -> queryOptions
        -- CCreateArgument -> queryOptions
        -- other ->
          -- lift $
          -- internalError ("unexpected event for sAllOptions " <> show other) >>
          -- pure []
  where
    queryOptions = do
      (uId, _uName, sRef) <- lift $ getUserId st certRef
      liftIO $ query st $ QueryAllOptions uId sRef args
