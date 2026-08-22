{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Declarations produced by the DSL parser, and the compiler that turns a
--   list of declarations into a fully interpreted "Pattern.Tables" value.
module Decl where

import           Control.Lens
import           Control.Monad

import qualified Data.List  as L
import qualified Data.Map   as M
import           Data.Ratio ((%))
import qualified Data.Text  as T

import           Pattern
import           Species

import           System.Random (StdGen)

-- ---------------------------------------------------------------------------
-- Declarations (name level, directly parseable)

-- | Action specification for a species: a kill percentage or the special
--   white behavior (~смешанные~ — kill iff cross-color reproducers exist).
data ActionSpec = ActionPercent Int | ActionCross | ActionCrossPercent Int deriving Show

-- | Partner preference: preferred set, fallback set, optional condition
--   gating the fallback.
data PartnerSpec = PartnerSpec [PatSp T.Text] [PatSp T.Text] (Maybe Cond)
  deriving Show

-- | Kill rule: target tokens and excluded species names.
data KillSpec = KillSpec [PatSp T.Text] [T.Text] deriving Show

-- | Sympathy rule: actor pattern and clauses (partner pattern, percent,
--   optional condition).
data SympathySpec = SympathySpec (Pat T.Text) [(Pat T.Text, Int, Maybe Cond)]
  deriving Show

-- | Creation rule: actor pattern, partner pattern and result clauses.
data CreationSpec = CreationSpec (Pat T.Text) (Pat T.Text) [(Pat T.Text, Int)]
  deriving Show

-- | Offspring distribution: (number of offspring, percent) pairs.
type OffspringSpec = [(Int, Int)]

data Decl =
      DeclBase [T.Text]
    | DeclSynonym (T.Text, T.Text, T.Text)
    | DeclColor (T.Text, T.Text)
    | DeclParams [(T.Text, T.Text)]
    | DeclActions [(T.Text, ActionSpec)]
    | DeclPartners [(T.Text, PartnerSpec)]
    | DeclKills [(T.Text, KillSpec)]
    | DeclSympathies [SympathySpec]
    | DeclCreations [CreationSpec]
    | DeclMortalities [(T.Text, Int)]               -- Смертность: percent
    | DeclScales [(T.Text, Int)]                    -- Долголетие: integer
    | DeclKillSuccess [(T.Text, Int)]               -- Успех убийства: percent
    | DeclOffspring [(T.Text, OffspringSpec)]       -- Потомство: pairs

instance Show Decl where
    show (DeclBase xs)         = "Базовые виды:" ++ concatMap ((" " ++) . T.unpack) xs
    show (DeclSynonym (x, y, z)) = "Синоним: " ++ T.unpack x ++ " x " ++ T.unpack y ++ " ~ " ++ T.unpack z
    show (DeclColor (n, c))    = "Цвет: " ++ T.unpack n ++ " " ++ T.unpack c
    show (DeclParams ps)       = "Параметры: " ++ show ps
    show (DeclActions as)      = "Действия: " ++ show as
    show (DeclPartners ps)     = "Партнёры: " ++ show ps
    show (DeclKills ks)        = "Убийство: " ++ show ks
    show (DeclSympathies ss)   = "Симпатии: " ++ show ss
    show (DeclCreations cs)    = "Рождение: " ++ show cs
    show (DeclMortalities ms)  = "Смертность: " ++ show ms
    show (DeclScales ls)       = "Долголетие: " ++ show ls
    show (DeclKillSuccess ks)  = "Успех убийства: " ++ show ks
    show (DeclOffspring os)    = "Потомство: " ++ show os

-- ---------------------------------------------------------------------------
-- Errors

data ErrorKind =
      ErrorResolution T.Text        -- name cannot be resolved
    | ErrorValidation T.Text        -- a rule is inconsistent

instance Show ErrorKind where
    show (ErrorResolution n) = "Не удалось разрешить имя: " ++ T.unpack n
    show (ErrorValidation m) = "Ошибка валидации: " ++ T.unpack m

-- ---------------------------------------------------------------------------
-- Compiler

-- | Intermediate compile state.
data CS = CS {
      _csBase  :: [T.Text],
      _csSyn   :: [(T.Text, T.Text, T.Text)],
      _csCol   :: [(T.Text, T.Text)],
      _csEnv   :: Env,
      _csAct   :: [(T.Text, ActionSpec)],
      _csPar   :: [(T.Text, PartnerSpec)],
      _csKill  :: [(T.Text, KillSpec)],
      _csSym   :: [SympathySpec],
      _csCre   :: [CreationSpec],
      _csMort  :: [(T.Text, Int)],
      _csScale :: [(T.Text, Int)],
      _csKSuc  :: [(T.Text, Int)],
      _csOff   :: [(T.Text, OffspringSpec)]
    }

emptyCS :: CS
emptyCS = CS [] [] [] defaultEnv [] [] [] [] [] [] [] [] []

applyDecl :: CS -> Decl -> Either ErrorKind CS
applyDecl cs (DeclBase xs)     = Right cs { _csBase = xs }
applyDecl cs (DeclSynonym s)   = Right cs { _csSyn = _csSyn cs ++ [s] }
applyDecl cs (DeclColor p)     = Right cs { _csCol = _csCol cs ++ [p] }
applyDecl cs (DeclParams ps)   = foldM applyParam cs ps
applyDecl cs (DeclActions as)  = Right cs { _csAct = _csAct cs ++ as }
applyDecl cs (DeclPartners ps) = Right cs { _csPar = _csPar cs ++ ps }
applyDecl cs (DeclKills ks)    = Right cs { _csKill = _csKill cs ++ ks }
applyDecl cs (DeclSympathies ss) = Right cs { _csSym = _csSym cs ++ ss }
applyDecl cs (DeclCreations cr) = Right cs { _csCre = _csCre cs ++ cr }
applyDecl cs (DeclMortalities ms) = Right cs { _csMort = _csMort cs ++ ms }
applyDecl cs (DeclScales ls)    = Right cs { _csScale = _csScale cs ++ ls }
applyDecl cs (DeclKillSuccess ks) = Right cs { _csKSuc = _csKSuc cs ++ ks }
applyDecl cs (DeclOffspring os) = Right cs { _csOff = _csOff cs ++ os }

applyParam :: CS -> (T.Text, T.Text) -> Either ErrorKind CS
applyParam cs (name, value) =
    case name of
      "Поле"               -> setInt envSpaceSize
      "Начало"             -> setInt envInitial
      "Победа"             -> setInt envWin
      "Максимум шагов"     -> setInt envMaxSteps
      "Изнасилование"      -> setRat envRape
      "Награда за убийство"   -> setRat envKillReward
      "Штраф за размножение"  -> setRat envReproPenalty
      _ -> Left (ErrorValidation ("Неизвестный параметр: " <> name))
  where
    setInt :: Lens' Env Int -> Either ErrorKind CS
    setInt l =
        case reads (T.unpack value) of
          [(n, "")] -> Right cs { _csEnv = set l n (_csEnv cs) }
          _ -> Left (ErrorValidation ("Некорректное значение параметра " <> name <> ": " <> value))
    setRat :: Lens' Env Rational -> Either ErrorKind CS
    setRat l =
        case T.stripSuffix "%" value of
          Just pct ->
              case (reads (T.unpack pct) :: [(Int, String)]) of
                [(n, "")] -> Right cs { _csEnv = set l (fromIntegral n % 100) (_csEnv cs) }
                _ -> Left (ErrorValidation ("Некорректный процент параметра " <> name <> ": " <> value))
          Nothing -> Left (ErrorValidation ("Параметр " <> name <> " должен быть задан в процентах"))

-- | Resolve a species name (base or hybrid).
resolveSpecies :: Ord b => M.Map T.Text (Species b) -> T.Text -> Either ErrorKind (Species b)
resolveSpecies resolver n =
    case M.lookup n resolver of
      Just s  -> Right s
      Nothing -> Left (ErrorResolution n)

-- | Compile a list of declarations into a full rule table.
compileDecls :: [Decl] -> StdGen -> Either ErrorKind (Tables T.Text)
compileDecls decls seed = do
    cs <- foldM applyDecl emptyCS decls
    let bases = _csBase cs
    if null bases
    then Left (ErrorValidation "Не заданы базовые виды")
    else do
      let baseSp = map Pure bases
          baseMap = M.fromList (zip bases baseSp)
      synMap <- foldM (addSynonym baseMap) M.empty (_csSyn cs)
      let resolver = M.union synMap baseMap
          allSp = genAll bases
          spNameAll = M.fromList [ (s, M.findWithDefault (autoName s) s inv) | s <- allSp ]
            where
              autoName (Pure x)  = x
              autoName (Mix x y) = x <> "+" <> y
              -- inverse of the resolver: species -> its declared name
              inv = M.fromList [ (s, n) | (n, s) <- M.toList resolver ]
          zeros = M.fromList [(s, 0) | s <- allSp]
          emptyLives = M.fromList [(s, []) | s <- allSp]
          emptyHist = M.fromList [(s, M.empty) | s <- allSp]
          emptySp = Space 0 emptyLives zeros zeros emptyHist
      colMap <- foldM (addColor resolver) M.empty (_csCol cs)
      actMap <- compileActions resolver allSp (_csAct cs)
      parMap <- compilePartners resolver allSp (_csPar cs)
      killMap <- compileKills resolver allSp (_csKill cs)
      symRules <- compileSympathy resolver (_csSym cs)
      creRules <- compileCreation resolver (_csCre cs)
      mortMap <- compilePercentMap resolver allSp (1 % 100) (_csMort cs)
      scaleMap <- compileScaleMap resolver allSp 60 (_csScale cs)
      ksucMap <- compilePercentMap resolver allSp 1 (_csKSuc cs)
      offMap <- compileOffspring resolver allSp (_csOff cs)
      return (Tables seed bases allSp (_csEnv cs) emptySp resolver spNameAll colMap
                      actMap parMap killMap symRules creRules
                      mortMap scaleMap ksucMap offMap)
  where
    addSynonym :: M.Map T.Text (Species T.Text) -> M.Map T.Text (Species T.Text)
               -> (T.Text, T.Text, T.Text) -> Either ErrorKind (M.Map T.Text (Species T.Text))
    addSynonym resolver m (x, y, z) = do
        sx <- resolveSpecies resolver x
        sy <- resolveSpecies resolver y
        case (sx, sy) of
          (Pure u, Pure v) | u /= v ->
              if M.member z m || M.member z resolver
              then Left (ErrorValidation ("Имя уже используется: " <> z))
              else Right (M.insert z (mix u v) m)
          _ -> Left (ErrorValidation ("Гибрид можно получить только из двух разных исходных видов: " <> x <> " x " <> y))

    addColor :: M.Map T.Text (Species T.Text) -> M.Map (Species T.Text) T.Text
             -> (T.Text, T.Text) -> Either ErrorKind (M.Map (Species T.Text) T.Text)
    addColor resolver m (n, c) = do
        s <- resolveSpecies resolver n
        return (M.insert s c m)

    compileActions :: M.Map T.Text (Species T.Text) -> [Species T.Text]
                   -> [(T.Text, ActionSpec)] -> Either ErrorKind (M.Map (Species T.Text) (ActionRule T.Text))
    compileActions resolver allSp specs = do
        explicit <- foldM go M.empty specs
        case lookup "*" specs of
          Nothing -> return explicit
          Just d -> do
              dRule <- specRule d
              return (M.fromList [ (s, M.findWithDefault dRule s explicit) | s <- allSp ])
      where
        go m (name, _)  | name == "*" = Right m
        go m (name, spec) = do
            s <- resolveSpecies resolver name
            r <- specRule spec
            return (M.insert s r m)
        specRule (ActionPercent k)
            | k < 0 || k > 100 = Left (ErrorValidation "Процент убийства вне диапазона 0..100")
            | otherwise = Right (ActionKillProb (fromIntegral k % 100))
        specRule ActionCross = Right ActionKillCross
        specRule (ActionCrossPercent k)
            | k < 0 || k > 100 = Left (ErrorValidation "Процент убийства вне диапазона 0..100")
            | otherwise = Right (ActionKillCrossProb (fromIntegral k % 100))

    compilePartners :: M.Map T.Text (Species T.Text) -> [Species T.Text]
                    -> [(T.Text, PartnerSpec)] -> Either ErrorKind (M.Map (Species T.Text) (PartnerRule T.Text))
    compilePartners resolver allSp specs = do
        explicit <- foldM go M.empty specs
        case lookup "*" specs of
          Nothing -> return explicit
          Just d -> do
              dRule <- specRule d
              return (M.fromList [ (s, M.findWithDefault dRule s explicit) | s <- allSp ])
      where
        go m (name, _)  | name == "*" = Right m
        go m (name, spec) = do
            s <- resolveSpecies resolver name
            r <- specRule spec
            return (M.insert s r m)
        specRule (PartnerSpec pref fb cond) = Right (PartnerRule pref fb cond)

    compileKills :: M.Map T.Text (Species T.Text) -> [Species T.Text]
                 -> [(T.Text, KillSpec)] -> Either ErrorKind (M.Map (Species T.Text) (KillRule T.Text))
    compileKills resolver allSp specs = do
        explicit <- foldM go M.empty specs
        case lookup "*" specs of
          Nothing -> return explicit
          Just d -> do
              dRule <- specRule d
              return (M.fromList [ (s, M.findWithDefault dRule s explicit) | s <- allSp ])
      where
        go m (name, _)  | name == "*" = Right m
        go m (name, spec) = do
            s <- resolveSpecies resolver name
            r <- specRule spec
            return (M.insert s r m)
        specRule (KillSpec targets excl) = do
            ex <- mapM (resolveSpecies resolver) excl
            return (KillRule targets ex)

    compileSympathy :: M.Map T.Text (Species T.Text) -> [SympathySpec]
                    -> Either ErrorKind [SympathyRule T.Text]
    compileSympathy resolver specs = do
        rules <- mapM go specs
        return (L.sortOn (negate . concreteness . _srActor) rules)
      where
        go (SympathySpec apat clauses) = do
            checkNames resolver (patternNames apat)
            cs <- mapM checkClause clauses
            return (SympathyRule apat cs)
        -- percentages are absolute probabilities
        checkClause (ppat, p, mcond) = do
            checkPercent p
            checkNames resolver (patternNames ppat)
            return (ppat, fromIntegral p % 100, mcond)

    -- | Compile a per-species percent-valued section (Смертность,
    --   Успех убийства) with a "*" default rule.
    compilePercentMap :: M.Map T.Text (Species T.Text) -> [Species T.Text]
                      -> Rational  -- ^ default value (species without an entry)
                      -> [(T.Text, Int)]
                      -> Either ErrorKind (M.Map (Species T.Text) Rational)
    compilePercentMap resolver allSp def specs = do
        explicit <- foldM go M.empty specs
        case lookup "*" specs of
          Nothing -> return (M.fromList [ (s, M.findWithDefault def s explicit) | s <- allSp ])
          Just d -> do
              dVal <- checkPct d
              return (M.fromList [ (s, M.findWithDefault dVal s explicit) | s <- allSp ])
      where
        go m (name, _) | name == "*" = Right m
        go m (name, k) = do
            s <- resolveSpecies resolver name
            v <- checkPct k
            return (M.insert s v m)
        checkPct k
            | k < 0 || k > 100 = Left (ErrorValidation "Процент вне диапазона 0..100")
            | otherwise = Right (fromIntegral k % 100)

    -- | Compile the per-species lifespan scale (Долголетие) with a "*" rule.
    compileScaleMap :: M.Map T.Text (Species T.Text) -> [Species T.Text]
                    -> Int  -- ^ default scale (species without an entry)
                    -> [(T.Text, Int)]
                    -> Either ErrorKind (M.Map (Species T.Text) Int)
    compileScaleMap resolver allSp def specs = do
        explicit <- foldM go M.empty specs
        case lookup "*" specs of
          Nothing -> return (M.fromList [ (s, M.findWithDefault def s explicit) | s <- allSp ])
          Just d -> do
              dVal <- checkScale d
              return (M.fromList [ (s, M.findWithDefault dVal s explicit) | s <- allSp ])
      where
        go m (name, _) | name == "*" = Right m
        go m (name, k) = do
            s <- resolveSpecies resolver name
            v <- checkScale k
            return (M.insert s v m)
        checkScale k
            | k < 1 = Left (ErrorValidation "Долголетие должно быть не меньше 1")
            | otherwise = Right k

    -- | Compile the per-species offspring distribution (Потомство) with a
    --   "*" rule. Percentages must sum to exactly 100%.
    compileOffspring :: M.Map T.Text (Species T.Text) -> [Species T.Text]
                     -> [(T.Text, OffspringSpec)]
                     -> Either ErrorKind (M.Map (Species T.Text) [(Int, Rational)])
    compileOffspring resolver allSp specs = do
        explicit <- foldM go M.empty specs
        case lookup "*" specs of
          Nothing -> return (M.fromList [ (s, M.findWithDefault [(1, 1)] s explicit) | s <- allSp ])
          Just d -> do
              dVal <- checkDist d
              return (M.fromList [ (s, M.findWithDefault dVal s explicit) | s <- allSp ])
      where
        go m (name, _) | name == "*" = Right m
        go m (name, pairs) = do
            s <- resolveSpecies resolver name
            v <- checkDist pairs
            return (M.insert s v m)
        checkDist pairs = do
            mapM_ (\k -> if k < 0
                         then Left (ErrorValidation "Число потомков не может быть отрицательным")
                         else Right ()) (map fst pairs)
            mapM_ (\p -> if p < 0 || p > 100
                         then Left (ErrorValidation "Процент потомства вне диапазона 0..100")
                         else Right ()) (map snd pairs)
            if sum (map snd pairs) /= 100
            then Left (ErrorValidation "Сумма вероятностей потомства должна быть 100%")
            else Right [ (k, fromIntegral p % 100) | (k, p) <- pairs ]

    compileCreation :: M.Map T.Text (Species T.Text) -> [CreationSpec]
                    -> Either ErrorKind [CreationRule T.Text]
    compileCreation resolver specs = do
        rules <- mapM go specs
        return (L.sortOn (negate . (\(CreationRule a p _) -> concreteness a + concreteness p)) rules)
      where
        go (CreationSpec apat ppat results) = do
            checkNames resolver (patternNames apat)
            checkNames resolver (patternNames ppat)
            rs <- mapM checkResult results
            return (CreationRule apat ppat rs)
        checkResult (rpat, p) = do
            checkPercent p
            checkNames resolver (patternNames rpat)
            return (rpat, fromIntegral p % 100)

    checkNames resolver ns = mapM_ (\n -> void (resolveSpecies resolver n)) ns
    checkPercent p | p < 0 || p > 100 = Left (ErrorValidation "Процент вне диапазона 0..100")
                   | otherwise = Right ()
