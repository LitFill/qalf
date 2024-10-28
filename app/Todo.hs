{-# LANGUAGE OverloadedStrings #-}

module Todo where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Flow
import Text.Printf (printf)

type Isi = Text
type Prioritas = Float
data Skala = S | M | L | XL
  deriving (Show, Eq, Ord)

data Todo where
  Todo ::
    { isi :: Isi
    , prioritas :: Prioritas
    , skala :: Skala
    } ->
    Todo

instance Eq Todo where
  todo1 == todo2 = isi todo1 == isi todo2

instance Ord Todo where
  compare todo1 todo2 =
    prioritas todo1 `compare` prioritas todo2
      <> skala todo1 `compare` skala todo2

instance Show Todo where
  show todo =
    let isi' = isi todo
        prio = prioritas todo
        skal = show $ skala todo
     in printf "%s <%f> [%s]" isi' prio skal

testTodo :: Todo
testTodo = mkTodo "Belajar record" 1.5 M

mkTodo :: Isi -> Prioritas -> Skala -> Todo
mkTodo isi prioritas skala = Todo{isi, prioritas, skala}

type Todos = [Todo]

normalize :: Todos -> Todos
normalize = sort .> zip [1 ..] .> map updatePrioritas
 where
  updatePrioritas (prioritasBaru, todo) =
    todo{prioritas = prioritasBaru}

-- TODO: add padding
pprintTodos :: Todos -> Text
pprintTodos = normalize .> reverse .> zip [1 ..] .> aux
 where
  aux :: [(Integer, Todo)] -> Text
  aux todos =
    let x = unzip todos |> snd |> head
        padNomor = prioritas x |> show |> length
        padIsi = unzip todos |> snd |> map (isi .> T.unpack .> length) |> maximum
     in map (aux2 padNomor padIsi) todos |> T.concat

  aux2 :: Int -> Int -> (Integer, Todo) -> Text
  aux2 padNomor padIsi (nomor, todo) =
    let nomor' = show nomor |> T.pack |> T.justifyRight padNomor ' '
        isi' = isi todo |> T.justifyLeft padIsi ' '
        todo' = todo{isi = isi'}
     in printf "%s. %s\n" nomor' (show todo') |> T.pack

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c

exampleTodos :: Todos
exampleTodos =
  map
    (uncurry3 mkTodo)
    [ ("Ngupload", 1, M)
    , ("Laporan PPL", 2, L)
    , ("Skripsi", 1.5, XL)
    , ("Nglondri", 3, M)
    , ("Pit", 1.7, L)
    , ("Duit", 4, L)
    , ("PPT", 1.6, L)
    , ("Ziaroh", 0.5, M)
    ]
