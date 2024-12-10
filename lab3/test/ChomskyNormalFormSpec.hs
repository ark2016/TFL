-- test/ChomskyNormalFormSpec.hs
module ChomskyNormalFormSpec where

import Test.Hspec
import Grammar (Grammar(..), Symbol(..))
import ChomskyNormalForm (toChomskyNormalForm)

spec :: Spec
spec = do
  describe "ChomskyNormalForm.toChomskyNormalForm" $ do

    it "обрабатывает простую грамматику без epsilon-, цепных и длинных правил, где уже всё в CNF" $ do
      -- Грамматика уже в CNF:
      -- S → A B
      -- A → a
      -- B → b
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b'])
            ]
      let expected = grammar
      toChomskyNormalForm grammar `shouldBe` expected

    it "удаляет длинные правила" $ do
      -- Исходная грамматика:
      -- S → A B C
      -- A → a
      -- B → b
      -- C → c
      --
      -- После удаления длинных правил и приведения к CNF, должно быть что-то вроде:
      -- S → S1 C
      -- S1 → A B
      -- A → a
      -- B → b
      -- C → c
      --
      -- Затем проверим, что другие шаги не изменят грамматику, так как нет ε-, цепных и бесполезных правил.
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "B", NonTerminal "C"]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b']),
              (NonTerminal "C", [Terminal 'c'])
            ]
      -- Ожидаемый результат: заменяем S → A B C на:
      -- S → S1 C
      -- S1 → A B
      let expected = Grammar [
              (NonTerminal "S1", [NonTerminal "A", NonTerminal "B"]),
              (NonTerminal "S", [NonTerminal "S1", NonTerminal "C"]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b']),
              (NonTerminal "C", [Terminal 'c'])
            ]

      toChomskyNormalForm grammar `shouldBe` expected

    it "удаляет epsilon-правила" $ do
      -- Грамматика с ε-правилом:
      -- S → A B | ε
      -- A → a
      -- B → b
      --
      -- После удаления ε-правил:
      -- S может вывести a b или просто ε, но в CNF правило S → ε возможно, только если S стартовый символ.
      -- В CNF:
      -- S → A B | ε
      -- A → a
      -- B → b
      -- Поскольку S стартовый, правило S→ε сохранится.
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
              (NonTerminal "S", [Epsilon]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b'])
            ]

      let expected = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
              (NonTerminal "S", [Epsilon]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b'])
            ]

      toChomskyNormalForm grammar `shouldBe` expected

    it "удаляет цепные правила" $ do
      -- Исходная грамматика:
      -- S → A
      -- A → B
      -- B → b
      --
      -- После удаления цепных правил и приведения к CNF:
      -- S → b
      -- A → b
      -- B → b
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "A"]),
              (NonTerminal "A", [NonTerminal "B"]),
              (NonTerminal "B", [Terminal 'b'])
            ]
      let expected = Grammar [
              (NonTerminal "S", [Terminal 'b']),
              (NonTerminal "A", [Terminal 'b']),
              (NonTerminal "B", [Terminal 'b'])
            ]

      toChomskyNormalForm grammar `shouldBe` expected

    it "удаляет бесполезные символы" $ do
      -- Исходная грамматика:
      -- S → A B
      -- A → a
      -- B → b
      -- C → D  (бесполезные символы)
      -- D → c
      --
      -- После удаления бесполезных символов (C,D) и приведения к CNF остаются:
      -- S → A B
      -- A → a
      -- B → b
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b']),
              (NonTerminal "C", [NonTerminal "D"]),
              (NonTerminal "D", [Terminal 'c'])
            ]
      let expected = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "B"]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "B", [Terminal 'b'])
            ]

      toChomskyNormalForm grammar `shouldBe` expected

    it "заменяет терминалы в двусимвольных правилах" $ do
      -- Исходная грамматика:
      -- S → A a
      -- A → a
      --
      -- Приведём к CNF:
      -- Для S → A a, необходимо заменить a на новый нетерминал X_a, где X_a → a
      -- Результат:
      -- S → A X_a
      -- A → a
      -- X_a → a
      let grammar = Grammar [
              (NonTerminal "S", [NonTerminal "A", Terminal 'a']),
              (NonTerminal "A", [Terminal 'a'])
            ]
      let expected = Grammar [
              (NonTerminal "S", [NonTerminal "A", NonTerminal "X_a"]),
              (NonTerminal "A", [Terminal 'a']),
              (NonTerminal "X_a", [Terminal 'a'])
            ]

      toChomskyNormalForm grammar `shouldBe` expected
