{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Monadic builder for polyhedral programs.
--
-- Usage:
--
-- @
-- jacobi :: Program '["N", "T"]
-- jacobi = 'program' $ do
--   old <- 'array' "old" ['range' 0 ('param' \@"N" + 1)]
--   new <- 'array' "new" ['range' 0 ('param' \@"N" + 1)]
--   'rescheduled' ('skew' \@"t" \@"i" 1 >> 'parallel' \@"t") $
--     'for_' \@"t" ('range' 1 ('param' \@"T")) $ \\t ->
--       'for_' \@"i" ('range' 1 ('param' \@"N")) $ \\i ->
--         new'!['ix' i] '<==' (old'!['ix' i-1] + old'!['ix' i] + old'!['ix' i+1]) / 3
-- @
module Isl.DSL.Builder
  ( -- * Program construction
    ProgM
  , program
    -- * Array declaration
  , array
  , withPolicy
    -- * Loop nests
  , for_
    -- * Statements
  , (<==)
  , (!)
    -- * Affine constructors
  , param
  , lit
    -- * Schedule transforms (SchedM)
  , SchedM
  , rescheduled
  , skew
  , tile
  , interchange
  , parallel
  , unroll
  , shift
  , fuse
  , mapBand
    -- * Memory mappings
  , mmap
  , doubleBuffer
    -- * Dimension lookup
  , dim
  ) where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import Isl.DSL.Types


-- ---------------------------------------------------------------------------
-- Builder state
-- ---------------------------------------------------------------------------

data BuildEnv = BuildEnv
  { beNextId     :: !Int                   -- next StmtId counter
  , beArrays     :: !(Map String ArrayDecl)
  , beTransforms :: [Transform]            -- from rescheduled (reversed)
  , beMemMaps    :: !(Map String MemMapDef)
  , beParDims    :: !(Set.Set String)
  , beNodes      :: [LoopNode]             -- nodes at current nesting level (reversed)
  , beDimStack   :: [String]               -- current loop variable names (innermost first)
  }

initBuildEnv :: BuildEnv
initBuildEnv = BuildEnv 0 Map.empty [] Map.empty Set.empty [] []

-- | The program builder monad. @ps@ tracks the parameter names at the type level.
newtype ProgM (ps :: [Symbol]) a = ProgM { unProgM :: State BuildEnv a }
  deriving (Functor, Applicative, Monad)

freshStmtId :: ProgM ps StmtId
freshStmtId = ProgM $ do
  n <- gets beNextId
  modify' $ \s -> s { beNextId = n + 1 }
  pure (StmtId n)

emitNode :: LoopNode -> ProgM ps ()
emitNode node = ProgM $ modify' $ \s -> s { beNodes = node : beNodes s }


-- ---------------------------------------------------------------------------
-- Program construction
-- ---------------------------------------------------------------------------

-- | Build a polyhedral program from the builder monad.
program :: ProgM ps a -> Program ps
program (ProgM m) =
  let env = execState m initBuildEnv
  in Program
    { pLoops      = reverse (beNodes env)
    , pArrays     = beArrays env
    , pTransforms = beTransforms env
    , pMemMaps    = beMemMaps env
    , pParDims    = beParDims env
    }


-- ---------------------------------------------------------------------------
-- Array declaration
-- ---------------------------------------------------------------------------

-- | Declare an array with per-dimension bounds.
--
-- @
-- old <- array "old" [range 0 (param \@"N" + 1)]
-- @
array :: String -> [Range] -> ProgM ps ArrayRef
array name bounds = do
  let decl = ArrayDecl name bounds AnyWrite
  ProgM $ modify' $ \s -> s { beArrays = Map.insert name decl (beArrays s) }
  pure (ArrayRef name)

-- | Set the write policy for an array.
--
-- @
-- new <- array "new" [range 0 (param \@"N" + 1)] \`withPolicy\` ExactWrite
-- @
withPolicy :: ProgM ps ArrayRef -> WritePolicy -> ProgM ps ArrayRef
withPolicy mref policy = do
  ref@(ArrayRef name) <- mref
  ProgM $ modify' $ \s ->
    s { beArrays = Map.adjust (\d -> d { adPolicy = policy }) name (beArrays s) }
  pure ref


-- ---------------------------------------------------------------------------
-- Loop nests
-- ---------------------------------------------------------------------------

-- | Introduce a loop dimension. The callback receives a typed index @Ix name@.
--
-- @
-- for_ \@"i" (range 1 (param \@"N")) $ \\i ->
--   a'!['ix' i] '<==' ...
-- @
for_ :: forall (name :: Symbol) ps a. KnownSymbol name
     => Range -> (Ix name -> ProgM ps a) -> ProgM ps a
for_ (Range lo hi) body = do
  let dimName = symbolVal (Proxy @name)
  -- Save current nodes, start collecting body nodes
  savedNodes <- ProgM $ gets beNodes
  ProgM $ modify' $ \s -> s
    { beNodes = []
    , beDimStack = dimName : beDimStack s
    }
  -- Run body
  result <- body (Ix (AVar dimName))
  -- Collect body nodes and restore
  bodyNodes <- ProgM $ gets beNodes
  ProgM $ modify' $ \s -> s
    { beNodes = ForLoop dimName lo hi (reverse bodyNodes) : savedNodes
    , beDimStack = drop 1 (beDimStack s)
    }
  pure result


-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

-- | Array subscript operator. Produces a 'ValExpr' (array read).
--
-- @
-- old'!['ix' i - 1]   -- :: ValExpr
-- @
(!) :: ArrayRef -> [AExpr] -> ValExpr
(!) (ArrayRef name) subs = VRead name subs

infixl 9 !

-- | Assignment: @array![subs] \<== expr@. Returns a 'StmtRef'.
--
-- @
-- new'!['ix' i] '<==' (old'!['ix' i-1] + old'!['ix' i] + old'!['ix' i+1]) / 3
-- @
(<==) :: ValExpr -> ValExpr -> ProgM ps StmtRef
(<==) (VRead name subs) rhs = do
  sid <- freshStmtId
  emitNode (Assign sid name subs rhs)
  pure (StmtRef sid)
(<==) _ _ = error "<==: LHS must be an array access (arr![subs])"

infix 1 <==


-- ---------------------------------------------------------------------------
-- Affine constructors
-- ---------------------------------------------------------------------------

-- | Reference a program parameter by name.
--
-- @
-- param \@"N" :: AExpr
-- @
param :: forall (name :: Symbol). KnownSymbol name => AExpr
param = AParam (symbolVal (Proxy @name))

-- | Integer literal as 'AExpr'.
lit :: Integer -> AExpr
lit = ALit


-- ---------------------------------------------------------------------------
-- Schedule transforms (SchedM)
-- ---------------------------------------------------------------------------

-- | Schedule transform monad. Composes via @(>>)@.
newtype SchedM a = SchedM { unSchedM :: State [Transform] a }
  deriving (Functor, Applicative, Monad)

emitTransform :: Transform -> SchedM ()
emitTransform t = SchedM $ modify' (t :)

-- | Decorator: apply schedule transforms to a loop nest.
--
-- @
-- 'rescheduled' ('skew' \@"t" \@"i" 1 >> 'parallel' \@"t") $
--   'for_' \@"t" ... $ \\t -> ...
-- @
rescheduled :: SchedM () -> ProgM ps a -> ProgM ps a
rescheduled (SchedM sm) body = do
  result <- body
  let transforms = reverse (execState sm [])
  ProgM $ modify' $ \s ->
    s { beTransforms = beTransforms s ++ transforms }
  pure result

-- | Skew dimension @d0@ by @d1@ with the given factor.
skew :: forall (d0 :: Symbol) (d1 :: Symbol). (KnownSymbol d0, KnownSymbol d1)
     => Integer -> SchedM ()
skew factor = emitTransform $
  TSkew (symbolVal (Proxy @d0)) (symbolVal (Proxy @d1)) factor

-- | Tile a dimension by the given size.
tile :: forall (name :: Symbol). KnownSymbol name => Integer -> SchedM ()
tile size = emitTransform $ TTile (symbolVal (Proxy @name)) size

-- | Interchange two dimensions.
interchange :: forall (d0 :: Symbol) (d1 :: Symbol). (KnownSymbol d0, KnownSymbol d1)
            => SchedM ()
interchange = emitTransform $
  TInterchange (symbolVal (Proxy @d0)) (symbolVal (Proxy @d1))

-- | Mark a dimension as parallel. Verified at compile time (ompVerify).
parallel :: forall (name :: Symbol). KnownSymbol name => SchedM ()
parallel = do
  let dimName = symbolVal (Proxy @name)
  emitTransform $ TParallel dimName

-- | Unroll a dimension by the given factor.
unroll :: forall (name :: Symbol). KnownSymbol name => Integer -> SchedM ()
unroll factor = emitTransform $ TUnroll (symbolVal (Proxy @name)) factor

-- | Shift a dimension by a constant offset.
shift :: forall (name :: Symbol). KnownSymbol name => Integer -> SchedM ()
shift offset = emitTransform $ TShift (symbolVal (Proxy @name)) offset

-- | Fuse two sibling bands.
fuse :: forall (d0 :: Symbol) (d1 :: Symbol). (KnownSymbol d0, KnownSymbol d1)
     => SchedM ()
fuse = emitTransform $
  TFuse (symbolVal (Proxy @d0)) (symbolVal (Proxy @d1))

-- | Generic: apply an arbitrary quasi-affine map to a named band.
-- This is the fully expressive primitive; named transforms are sugar over this.
mapBand :: forall (name :: Symbol). KnownSymbol name
        => (SchedCtx -> [(String, AExpr)]) -> SchedM ()
mapBand f = emitTransform $ TMapBand (symbolVal (Proxy @name)) f


-- ---------------------------------------------------------------------------
-- Memory mappings
-- ---------------------------------------------------------------------------

-- | Apply a memory mapping to an array.
-- The function receives the iteration context and the logical subscripts,
-- and returns the physical subscripts.
--
-- @
-- mmap old $ \\ctx subs -> [dim \@"t" ctx \`mod_\` 2] ++ subs
-- @
mmap :: ArrayRef -> (SchedCtx -> [AExpr] -> [AExpr]) -> ProgM ps ()
mmap (ArrayRef name) f =
  ProgM $ modify' $ \s -> s { beMemMaps = Map.insert name f (beMemMaps s) }

-- | Sugar: double-buffer an array along a loop dimension.
--
-- @
-- doubleBuffer \@"t" old 2
-- @
doubleBuffer :: forall (loopVar :: Symbol) ps. KnownSymbol loopVar
             => ArrayRef -> Integer -> ProgM ps ()
doubleBuffer ref modulus = mmap ref $ \ctx subs ->
  dim @loopVar ctx `mod_` modulus : subs

-- | Look up a dimension in a schedule/iteration context.
--
-- @
-- dim \@"t" ctx :: AExpr
-- @
dim :: forall (name :: Symbol). KnownSymbol name => SchedCtx -> AExpr
dim (SchedCtx m) =
  let dimName = symbolVal (Proxy @name)
  in case Map.lookup dimName m of
    Just e  -> e
    Nothing -> error $ "dim: unknown dimension " ++ show dimName
