{-# LANGUAGE RecordWildCards #-}

module Lecture10.Reader where

import Data.Maybe
import Data.List
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = ask >>= return . find ((pId==) . id)

processSingle :: Person -> String
processSingle Person{..} =
  "Уважаем" ++ intercalate " " [ending, name, surname] ++ "!\nРазрешите предложить Вам наши услуги."
  where
    ending = case sex of
      Male -> "ый"
      Female -> "ая"

processPair :: Person -> Person -> String
processPair Person{name=hName, surname=hSurname} Person{name=wName, surname=wSurname} =
  intercalate " " ["Уважаемые", hName, hSurname, "и", wName, wSurname] ++ "!\nРазрешите предложить вам наши услуги."

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
  person <- findById pId
  let sId = join $ marriedBy <$> person
  spouse <- case sId of
    (Just sId) -> findById sId
    _ -> return Nothing
  return $ case (person, spouse) of
    (Nothing, _) -> Nothing
    (person, Nothing) -> liftM processSingle person
    (person, spouse) -> liftM2 processPair person spouse


processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = do
  pId <- personIds
  return . runReader (processPerson pId) $ persons

-- </Задачи для самостоятельного решения>
