-- module Spec where

import MyParser
import MyElaborator
import MyCodeGen
import qualified Sprockell

import Test.Hspec
import Test.QuickCheck

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Data.Either
import Data.List
import GHC.IO.Handle
import System.IO
import System.Directory
import System.Timeout
import Control.Exception (try, SomeException)


main :: IO ()
main = hspec $ do

  ---------------------------------------------------------------------------------------------------------------
  --                                                  PARSING                                                  --
  ---------------------------------------------------------------------------------------------------------------

  -- `shouldSatisfy` isLeft -> means an error was returned
  describe "Parsing" $ do
    -- it "should parse numbers" $ do
    --     property $ \n -> (parseMyLang $ show (getPositive n)) `shouldBe` (Right (getPositive n) :: Either String Integer)
    it "parses a constant" $ do
      parse parseConst "" "42" `shouldBe` Right (Const 42)
      parse parseConst "" "1234567890" `shouldBe` Right (Const 1234567890)
      parse parseConst "" "abc" `shouldSatisfy` isLeft

    it "parses a variable" $ do
      parse var "" "x" `shouldBe` Right (Var "x")
      parse var "" "x3" `shouldBe` Right (Var "x3")
      parse var "" "variable" `shouldBe` Right (Var "variable")
      parse var "" "ValidName" `shouldBe` Right (Var "ValidName")
      parse var "" "0invalid" `shouldSatisfy` isLeft

    it "parses true/false" $ do
      parse boolean "" "true" `shouldBe` Right (Boolean True)
      parse boolean "" "false" `shouldBe` Right (Boolean False)
      parse boolean "" "abc" `shouldSatisfy` isLeft
      parse boolean "" "123" `shouldSatisfy` isLeft

    it "parses an addition expression" $ parse expr "" "1 + 2" `shouldBe` Right (Add (Const 1) (Const 2))

    it "parses a subtraction expression" $ parse expr "" "3 - 1" `shouldBe` Right (Sub (Const 3) (Const 1))

    it "parses a multiplication expression" $ parse expr "" "2 * 3" `shouldBe` Right (Mult (Const 2) (Const 3))

    it "parses a division expression" $ parse expr "" "6 / 2" `shouldBe` Right (Div (Const 6) (Const 2))

    it "parses a declaration" $ parse declaration "" "int x = 5;" `shouldBe` Right (Primitive Local TInt "x" (Just (Const 5)))

    it "parses an assignment" $ parse assignment "" "x = 10" `shouldBe` Right (Absolute "x" (Const 10))

    it "parses a condition" $ do
      parse condition "" "true && false" `shouldBe` Right (And (Expr (Condition (Boolean True))) (Expr (Condition (Boolean False))))
      parse condition "" "true || false" `shouldBe` Right (Or (Expr (Condition (Boolean True))) (Expr (Condition (Boolean False))))
      parse condition "" "2 <= 3" `shouldBe` Right (Le (Expr (Const 2)) (Expr (Const 3)))
      parse condition "" "x >= 5" `shouldBe` Right (Ge (Expr (Var "x")) (Expr (Const 5)))
      parse condition "" "34 < y" `shouldBe` Right (Lt (Expr (Const 34)) (Expr (Var "y")))
      parse condition "" "2 > 5" `shouldBe` Right (Gt (Expr (Const 2)) (Expr (Const 5)))
      parse condition "" "x == 2" `shouldBe` Right (Eq (Expr (Var "x")) (Expr (Const 2)))
      parse condition "" "3 != 5" `shouldBe` Right (Neq (Expr (Const 3)) (Expr (Const 5)))

    it "parses an array with index" $ parse arrayIndex "" "x[2]" `shouldBe` Right (ArrayIndex "x" (Const 2))

    it "parses an array literal" $ parse arrayLiteral "" "[2, 3, a+b, s]" `shouldBe` Right (ArrayLiteral [Const 2, Const 3, Add (Var "a") (Var "b"), Var "s"])

    it "parses a string literal" $ parse parseString "" "\"Hello\"" `shouldBe` Right (StringLiteral "Hello")

    it "parses a character" $ parse parseChar "" "'a'" `shouldBe` Right (Char 'a')

    it "parses an expression" $ do
      parse expr "" "((1 + 2) * (3 - 4)) / ((5 * 6) / 7)" `shouldBe`
        Right (Div (Mult (Add (Const 1) (Const 2)) (Sub (Const 3) (Const 4))) (Div (Mult (Const 5) (Const 6)) (Const 7)))

      parse expr "" "3 * (x + 4) / (2 - y)" `shouldBe`
        Right (Div (Mult (Const 3) (Add (Var "x") (Const 4))) (Sub (Const 2) (Var "y")))

      parse expr "" "((x > 2 || y < 5) && z == 10)" `shouldBe`
        Right (Condition (And (Expr (Condition (Or (Gt (Expr (Var "x")) (Expr (Const 2))) (Lt (Expr (Var "y")) (Expr (Const 5)))))) (Eq (Expr (Var "z")) (Expr (Const 10)))))

    it "parses an assignment" $ do
      parse assignment "" "x[1] = b + 2 * 4" `shouldBe` Right (Partial "x" (Const 1) (Add (Var "b") (Mult (Const 2) (Const 4))))
      parse assignment "" "y = a - c + b" `shouldBe` Right (Absolute "y" (Add (Sub (Var "a") (Var "c")) (Var "b")))

    it "parses a declaration" $ do
      parse declaration "" "int x" `shouldBe` Right (Primitive Local TInt "x" Nothing)
      parse declaration "" "int x = 10" `shouldBe` Right (Primitive Local TInt "x" (Just (Const 10)))
      parse declaration "" "int x = 3+4" `shouldBe` Right (Primitive Local TInt "x" (Just (Add (Const 3) (Const 4))))
      parse declaration "" "global bool x" `shouldBe` Right (Primitive Global TBool "x" Nothing)
      parse declaration "" "char y" `shouldBe` Right (Primitive Local TChar "y" Nothing)
      parse declaration "" "int x [2]" `shouldBe` Right (Array TInt "x" 2 Nothing)
      parse declaration "" "char c [3] = ['a', 'c', 'd']" `shouldBe` Right (Array TChar "c" 3 (Just (ArrayLiteral [Char 'a',Char 'c',Char 'd'])))
      parse declaration "" "Lock z" `shouldBe` Right (TLock "z")
      parse declaration "" "String s = \"Hello, World!\"" `shouldBe` Right (String "s" (StringLiteral "Hello, World!"))


    it "parses a thread block" $ do
      parse statement "" "thread { print(42); x = 10; }" `shouldBe` Right (Thread [Print (Const 42), Assignment (Absolute "x" (Const 10))])
      parse statement "" "thread { }" `shouldBe` Right (Thread [])
      parse statement "" "thread { { print(42); } }" `shouldBe` Right (Thread [Block [Print (Const 42)]])

    it "parses a conditional if statement without else" $ parse statement "" "if (x == 0) { print(x); }" `shouldBe` Right (If (Eq (Expr (Var "x")) (Expr (Const 0))) [Print (Var "x")] Nothing)

    it "parses a conditional if statement with else" $ do
      parse statement "" "if (x > 0) { print(x); } else { print(x); }" `shouldBe` Right (If (Gt (Expr (Var "x")) (Expr (Const 0))) [Print (Var "x")] (Just [Print (Var "x")]))
      parse statement "" "if ((x == 0) && (y > 10)) { print(x); } else { print(y); }" `shouldBe` Right (If (And (Expr (Condition (Eq (Expr (Var "x")) (Expr (Const 0))))) (Expr (Condition (Gt (Expr (Var "y")) (Expr (Const 10)))))) [Print (Var "x")] (Just [Print (Var "y")]))
      parse statement "" "if (x == 0) { if (y == 1) {print(1);} else {print(2);} } else { print(3); }" `shouldBe` Right (If (Eq (Expr (Var "x")) (Expr (Const 0))) [If (Eq (Expr (Var "y")) (Expr (Const 1))) [Print (Const 1)] (Just [Print (Const 2)])] (Just [Print (Const 3)]))


    it "parses a while loop" $ do
      parse statement "" "while (x < 10) { x = x + 1; }" `shouldBe` Right (While (Lt (Expr (Var "x")) (Expr (Const 10))) [Assignment (Absolute "x" (Add (Var "x") (Const 1)))])
      parse statement "" "while ((x > 0) || (y < 10)) { y = y - 1; }" `shouldBe` Right (While (Or (Expr (Condition (Gt (Expr (Var "x")) (Expr (Const 0))))) (Expr (Condition (Lt (Expr (Var "y")) (Expr (Const 10)))))) [Assignment (Absolute "y" (Sub (Var "y") (Const 1)))])
      parse statement "" "while (x < 10) { { print(x); } x = x + 1; }" `shouldBe` Right (While (Lt (Expr (Var "x")) (Expr (Const 10))) [Block [Print (Var "x")], Assignment (Absolute "x" (Add (Var "x") (Const 1)))])


    it "parses a print statement" $ do
      parse statement "" "print(\"Hello, world!\");" `shouldBe` Right (Print (StringLiteral "Hello, world!"))
      parse statement "" "print(3 * (x + 4) / (2 - y));" `shouldBe` Right (Print (Div (Mult (Const 3) (Add (Var "x") (Const 4))) (Sub (Const 2) (Var "y"))))

    it "parses a block" $ parse block "" "{ int x = 5; x = 10; }" `shouldBe` Right [Block [ Declaration (Primitive Local TInt "x" (Just (Const 5))), Assignment (Absolute "x" (Const 10)) ]]

    it "parses a block statement" $ parse statement "" "{ int x = 5; x = x + 1; }" `shouldBe` Right (Block [ Declaration (Primitive Local TInt "x" (Just (Const 5))), Assignment (Absolute "x" (Add (Var "x") (Const 1)))])

    it "parses a lock statement" $ parse statement "" "someLock.lock;" `shouldBe` Right (Lock "someLock")

    it "parses an unlock statement" $ parse statement "" "someLock.unlock;" `shouldBe` Right (Unlock "someLock")

    it "fails to parse an incomplete statement" $ parse statement "" "x = " `shouldSatisfy` isLeft

    it "parses a program" $ do
      parse program "" "int x = 5; x = 10; " `shouldBe` Right (Program [ Declaration (Primitive Local TInt "x" (Just (Const 5))), Assignment (Absolute "x" (Const 10)) ])
      parse program "" "int x = 5; thread { x = x + 1; { int y = 10; y = y + 2; } }" `shouldBe`
        Right (Program
          [ Declaration (Primitive Local TInt "x" (Just (Const 5)))
          , Thread
              [ Assignment (Absolute "x" (Add (Var "x") (Const 1)))
              , Block
                  [ Declaration (Primitive Local TInt "y" (Just (Const 10)))
                  , Assignment (Absolute "y" (Add (Var "y") (Const 2)))
                  ]
              ]
          ])
      parse program "" "int x = 0; while (x < 10) { if (x == 0) { print(x); } else { print(x + 1); } x = x + 1; }" `shouldBe`
        Right (Program
          [ Declaration (Primitive Local TInt "x" (Just (Const 0)))
          , While (Lt (Expr (Var "x")) (Expr (Const 10)))
              [ If (Eq (Expr (Var "x")) (Expr (Const 0)))
                  [Print (Var "x")]
                  (Just [Print (Add (Var "x") (Const 1))])
              , Assignment (Absolute "x" (Add (Var "x") (Const 1)))
              ]
          ])
      parse program "" "Lock myLock; thread { myLock.lock; x = x + 1; myLock.unlock; } thread { myLock.lock; x = x + 2; myLock.unlock; }" `shouldBe`
        Right (Program
          [ Declaration (TLock "myLock")
          , Thread
              [ Lock "myLock"
              , Assignment (Absolute "x" (Add (Var "x") (Const 1)))
              , Unlock "myLock"
              ]
          , Thread
              [ Lock "myLock"
              , Assignment (Absolute "x" (Add (Var "x") (Const 2)))
              , Unlock "myLock"
              ]
          ])
      parse program "" "int x = 5; thread { if (x > 0) { if (x < 10) { print(x); } else { print(10); } } else { print(0); } }" `shouldBe`
        Right (Program
          [ Declaration (Primitive Local TInt "x" (Just (Const 5)))
          , Thread
              [ If (Gt (Expr (Var "x")) (Expr (Const 0)))
                  [ If (Lt (Expr (Var "x")) (Expr (Const 10)))
                      [Print (Var "x")]
                      (Just [Print (Const 10)])
                  ]
                  (Just [Print (Const 0)])
              ]
          ])
      parse program "" "int x = 5; int y = 10; x = x + y; y = x - y; print(x); print(y);" `shouldBe`
        Right (Program
          [ Declaration (Primitive Local TInt "x" (Just (Const 5)))
          , Declaration (Primitive Local TInt "y" (Just (Const 10)))
          , Assignment (Absolute "x" (Add (Var "x") (Var "y")))
          , Assignment (Absolute "y" (Sub (Var "x") (Var "y")))
          , Print (Var "x")
          , Print (Var "y")
          ])
    it "parses a complex program" $ do
      let input = unlines
            [ "int x;"
            , "if (x > 2) {"
            , "  int x = 12;"
            , "  while (x < 24) {"
            , "    char x = 'c';"
            , "  }"
            , "}"
            , "else"
            , "{"
            ,   "y = x + 1;"
            , "}"
            , "while (x < 5) {"
            , "  x = x + 1;"
            , "  print(x);"
            , "}"
            , "bool y = (x - 2 * 16 == 3 * x);"
            , "thread {"
            , "  int x = 0;"
            , "}"
            , "print((x && y));"
            , "{"
            , "  y = 24 + x;"
            , "}"
            ]
      let expected = Program
            [ Declaration (Primitive Local TInt "x" Nothing)
              , If (Gt (Expr (Var "x")) (Expr (Const 2)))
                  [ Declaration (Primitive Local TInt "x" (Just (Const 12)))
                  , While (Lt (Expr (Var "x")) (Expr (Const 24)))
                      [ Declaration (Primitive Local TChar "x" (Just (Char 'c')))
                      ]
                  ]
                  ( Just
                    [ Assignment (Absolute "y" (Add (Var "x") (Const 1)))
                    ]
                  )
              , While (Lt (Expr (Var "x")) (Expr (Const 5)))
                  [ Assignment (Absolute "x" (Add (Var "x") (Const 1)))
                  , Print (Var "x")
                  ]
              , Declaration (Primitive Local TBool "y"
                  (Just (Condition (Eq (Expr (Sub (Var "x") (Mult (Const 2) (Const 16))))
                                    (Expr (Mult (Const 3) (Var "x")))))))
              , Thread
                  [ Declaration (Primitive Local TInt "x" (Just (Const 0)))
                  ]
              , Print (Condition (And (Expr (Var "x")) (Expr (Var "y"))))
              , Block
                  [ Assignment (Absolute "y" (Add (Const 24) (Var "x")))
                  ]
              ]
      parse program "" input `shouldBe` Right expected


  -------------------------------------------------------------------------------------------------------------------
  --                                                  ELABORATION                                                  --
  -------------------------------------------------------------------------------------------------------------------


  describe "Elaboration" $ do
    it "renames a variable in multiple types of statements" $ do
      let input =
            [ Declaration (Primitive Local TInt "x" Nothing)
              , If (Gt (Expr (Var "x")) (Expr (Const 2)))
                  [ Declaration (Primitive Local TInt "x" (Just (Const 12)))
                  , While (Lt (Expr (Var "x")) (Expr (Const 24)))
                      [ Declaration (Primitive Local TChar "x" (Just (Char 'c')))
                      ]
                  ]
                  ( Just
                    [ Assignment (Absolute "y" (Add (Var "x") (Const 1)))
                    ]
                  )
              , While (Lt (Expr (Var "x")) (Expr (Const 5)))
                  [ Assignment (Absolute "x" (Add (Var "x") (Const 1)))
                  , Print (Var "x")
                  ]
              , Declaration (Primitive Local TBool "y"
                  (Just (Condition (Eq (Expr (Sub (Var "x") (Mult (Const 2) (Const 16))))
                                    (Expr (Mult (Const 3) (Var "x")))))))
              , Thread
                  [ Declaration (Primitive Local TInt "x" (Just (Const 0)))
                  ]
              , Print (Condition (And (Expr (Var "x")) (Expr (Var "y"))))
              , Block
                  [ Assignment (Absolute "y" (Add (Const 24) (Var "x")))
                  ]
              ]
      let expected =
            [ Declaration (Primitive Local TInt "y" Nothing)
              , If (Gt (Expr (Var "y")) (Expr (Const 2)))
                  [ Declaration (Primitive Local TInt "y" (Just (Const 12)))
                  , While (Lt (Expr (Var "y")) (Expr (Const 24)))
                      [ Declaration (Primitive Local TChar "y" (Just (Char 'c')))
                      ]
                  ]
                  ( Just
                    [ Assignment (Absolute "y" (Add (Var "y") (Const 1)))
                    ]
                  )
              , While (Lt (Expr (Var "y")) (Expr (Const 5)))
                  [ Assignment (Absolute "y" (Add (Var "y") (Const 1)))
                  , Print (Var "y")
                  ]
              , Declaration (Primitive Local TBool "y"
                  (Just (Condition (Eq (Expr (Sub (Var "y") (Mult (Const 2) (Const 16))))
                                    (Expr (Mult (Const 3) (Var "y")))))))
              , Thread
                  [ Declaration (Primitive Local TInt "y" (Just (Const 0)))
                  ]
              , Print (Condition (And (Expr (Var "y")) (Expr (Var "y"))))
              , Block
                  [ Assignment (Absolute "y" (Add (Const 24) (Var "y")))
                  ]
              ]
      renameVar "x" "y" input `shouldBe` expected

    it "type checks an expression" $ typeCheckingExpr (Div (Mult (Const 3) (Add (Var "x") (Const 4))) (Sub (Const 2) (Var "y"))) [HInt] (Scope [(HInt, "x"), (HInt, "y")] [])
      `shouldBe` Right HInt

    it "type checks a condition" $ typeCheckingCond (And (Expr (Condition (Or (Gt (Expr (Var "x")) (Expr (Const 2))) (Lt (Expr (Var "y")) (Expr (Const 5)))))) (Eq (Expr (Var "z")) (Expr (Const 10)))) [HBool] (Scope [(HInt, "x"), (HInt, "y"), (HInt, "z")] [])
      `shouldBe` Right HBool

    it "type checks an invalid expression" $ typeCheckingExpr (Div (Mult (Const 3) (Add (Var "x") (Const 4))) (Sub (Const 2) (Var "y"))) [HInt] (Scope [(HInt, "x"), (HChar, "y")] [])
      `shouldSatisfy` isLeft

    it "type checks an invalid condition" $ typeCheckingCond (And (Expr (Condition (Or (Gt (Expr (Var "x")) (Expr (Condition (Boolean True)))) (Lt (Expr (Var "y")) (Expr (Const 5)))))) (Eq (Expr (Var "z")) (Expr (Const 10)))) [HBool] (Scope [(HInt, "x"), (HChar, "y"), (HBool, "z")] [])
      `shouldSatisfy` isLeft

    it "type checks an expression with different data types" $ typeCheckingExpr (Condition (Eq (Eq (Expr (Var "x")) (Expr (Const 3))) (Expr (Var "y")))) [HBool] (Scope [(HInt, "x"), (HBool, "y")] [])
      `shouldBe` Right HBool

    it "type checks declaration" $ do
      typeCheckingBlock [Declaration (Primitive Local TInt "y" Nothing)] (Scope [] []) []
        `shouldBe` Right [Declaration (Primitive Local TInt "y" Nothing)]
      typeCheckingBlock [Declaration (Primitive Global TBool "x" (Just (Condition (Eq (Eq (Expr (Var "x")) (Expr (Const 3))) (Expr (Var "y"))))))] (Scope [(HInt, "x"), (HBool, "y")] [Scope [] []]) []
        `shouldBe` Right [Declaration (Primitive Global TBool "x_1" (Just (Condition (Eq (Eq (Expr (Var "x")) (Expr (Const 3))) (Expr (Var "y"))))))]

    it "type checks an assignment" $ do
      typeCheckingBlock [Assignment (Absolute "y" (Add (Var "y") (Const 1)))] (Scope [(HInt, "y")] []) []
        `shouldBe` Right [Assignment (Absolute "y" (Add (Var "y") (Const 1)))]
      typeCheckingBlock [Assignment (Absolute "x" (Condition (Eq (Expr (Var "x")) (Expr (Condition (Boolean True))))))] (Scope [(HBool, "x")] []) []
        `shouldBe` Right [Assignment (Absolute "x" (Condition (Eq (Expr (Var "x")) (Expr (Condition (Boolean True))))))]
      -- should give error, because, tries to compare integer with boolean
      typeCheckingBlock [Assignment (Absolute "x" (Condition (Eq (Expr (Var "x")) (Expr (Condition (Boolean True))))))] (Scope [(HInt, "x")] []) []
        `shouldSatisfy` isLeft

    it "checks for shadowing" $ do
      checkShadowing (Scope [(HInt, "x"), (HInt, "y")] [Scope [(HChar, "c")] [Scope [] []]]) "x" `shouldBe` True
      checkShadowing (Scope [(HInt, "x"), (HInt, "y")] [Scope [(HChar, "c")] [Scope [] []]]) "c" `shouldBe` True
      checkShadowing (Scope [(HInt, "x"), (HInt, "y")] [Scope [(HChar, "c")] [Scope [] []]]) "f" `shouldBe` False

    it "type checks a if statement" $ do
      typeCheckingBlock [If (Boolean True) [Declaration (Primitive Local TInt "y" Nothing)] Nothing] (Scope [] []) [] `shouldBe` Right [If (Boolean True) [Declaration (Primitive Local TInt "y" Nothing)] Nothing]
      typeCheckingBlock [If (Boolean False) [Declaration (Primitive Local TInt "y" Nothing)] (Just [Print (Const 1)])] (Scope [] []) [] `shouldBe` Right [If (Boolean False) [Declaration (Primitive Local TInt "y" Nothing)] (Just [Print (Const 1)])]
      -- should give error, because, the block tries to assign Int to Bool
      typeCheckingBlock [If (Boolean True) [Declaration (Primitive Local TBool "y" (Just (Var "x")))] Nothing] (Scope [(HInt, "x")] []) [] `shouldSatisfy` isLeft

    it "type checks a thread block" $ do
      typeCheckingBlock [Thread [Declaration (Primitive Local TInt "y" Nothing), Print (Var "y")]] (Scope [] []) [] `shouldBe` Right [Thread [Declaration (Primitive Local TInt "y" Nothing), Print (Var "y")]]
      -- should give error, because, inside of thread is not possible to declare Locks inside threads
      typeCheckingBlock [Thread [Declaration (TLock "x"), Print (Var "y")]] (Scope [] []) [] `shouldSatisfy` isLeft
      -- should give error, because, inside of thread is not possible to declare global variables inside threads
      typeCheckingBlock [Thread [Declaration (Primitive Global TInt "x" Nothing), Print (Var "y")]] (Scope [] []) [] `shouldSatisfy` isLeft

    it "type checks a while block" $ do
      typeCheckingBlock [While (Boolean True) [Assignment (Absolute "y" (Add (Var "y") (Const 1))), Print (Var "y")]] (Scope [(HInt, "y")] []) [] `shouldBe` Right [While (Boolean True) [Assignment (Absolute "y" (Add (Var "y") (Const 1))), Print (Var "y")]]
      -- should give error, because, inside of thread is not possible to declare Locks inside while blocks
      typeCheckingBlock [While (Boolean True) [Declaration (TLock "x"), Print (Var "y")]] (Scope [(HInt, "y")] []) [] `shouldSatisfy` isLeft
      -- should give error, because, inside of thread is not possible to declare global variables inside while blocks
      typeCheckingBlock [While (Boolean True) [Declaration (Primitive Global TChar "c" Nothing), Print (Var "y")]] (Scope [(HInt, "y")] []) [] `shouldSatisfy` isLeft
      -- should give error, because, inside of thread is not possible to declare threads inside while blocks
      typeCheckingBlock [While (Boolean True) [Thread [Print (Var "y")], Print (Var "y")]] (Scope [(HInt, "y")] []) [] `shouldSatisfy` isLeft


    it "checks redeclaration of a variable in the same scope" $ do
      checkReDeclaration [] [] `shouldBe` Right True
      checkReDeclaration [Declaration (Primitive Local TInt "x" Nothing), Declaration (TLock "x")] [] `shouldSatisfy` isLeft
      checkReDeclaration [Declaration (Primitive Local TInt "x" Nothing), Block [Declaration (TLock "x")]] [] `shouldBe` Right True
      checkReDeclaration [Declaration (Primitive Local TInt "x" Nothing), Block [Declaration (TLock "y"), Declaration (Primitive Local TChar "y" Nothing)]] [] `shouldSatisfy` isLeft

    it "type checks a program" $ do
      let prog = Program
            [ Declaration (Primitive Local TInt "y" Nothing)
              , If (Gt (Expr (Var "y")) (Expr (Const 2)))
                  [ Declaration (Primitive Local TInt "y" (Just (Const 12)))
                  , While (Lt (Expr (Var "y")) (Expr (Const 24)))
                      [ Declaration (Primitive Local TChar "y" (Just (Char 'c')))
                      ]
                  ]
                  ( Just
                    [ Assignment (Absolute "y" (Add (Var "y") (Const 1)))
                    ]
                  )
              , While (Lt (Expr (Var "y")) (Expr (Const 5)))
                  [ Assignment (Absolute "y" (Add (Var "y") (Const 1)))
                  , Print (Var "y")
                  ]
              , Declaration (Primitive Local TBool "b"
                  (Just (Condition (Eq (Expr (Sub (Var "y") (Mult (Const 2) (Const 16))))
                                    (Expr (Mult (Const 3) (Var "y")))))))
              , Thread
                  [ Declaration (Primitive Local TInt "y" (Just (Const 0)))
                  ]
              , Print (Condition (And (Expr (Var "b")) (Expr (Var "b"))))
              , Block
                  [ Assignment (Absolute "y" (Add (Const 24) (Var "y")))
                  ]
              ]
      let expected = Program
            [ Declaration (Primitive Local TInt "y" Nothing)
              , If (Gt (Expr (Var "y")) (Expr (Const 2)))
                  [ Declaration (Primitive Local TInt "y_1" (Just (Const 12)))
                  , While (Lt (Expr (Var "y_1")) (Expr (Const 24)))
                      [ Declaration (Primitive Local TChar "y_2" (Just (Char 'c')))
                      ]
                  ]
                  ( Just
                    [ Assignment (Absolute "y" (Add (Var "y") (Const 1)))
                    ]
                  )
              , While (Lt (Expr (Var "y")) (Expr (Const 5)))
                  [ Assignment (Absolute "y" (Add (Var "y") (Const 1)))
                  , Print (Var "y")
                  ]
              , Declaration (Primitive Local TBool "b"
                  (Just (Condition (Eq (Expr (Sub (Var "y") (Mult (Const 2) (Const 16))))
                                    (Expr (Mult (Const 3) (Var "y")))))))
              , Thread
                  [ Declaration (Primitive Local TInt "y_1" (Just (Const 0)))
                  ]
              , Print (Condition (And (Expr (Var "b")) (Expr (Var "b"))))
              , Block
                  [ Assignment (Absolute "y" (Add (Const 24) (Var "y")))
                  ]
              ]
      typeCheckProgram prog `shouldBe` Right expected


  -------------------------------------------------------------------------------------------------------------------
  --                                                  COMPILATION                                                  --
  -------------------------------------------------------------------------------------------------------------------

  describe "Compilation" $ do
    it "prints a number" $ do
      let fileName = "printConst"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      output `shouldBe` "Sprockell 0 says 101\n"

    it "prints a character" $ do
      let fileName = "printChar"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      output `shouldBe` "Sprockell 0 says a\n"

    it "prints a boolean" $ do
      let expectedLines = [ "Sprockell 0 says true"
                          , "Sprockell 0 says false"
                          ]
      let fileName = "printBool"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite
      lines output `shouldBe` expectedLines

    it "prints Hello World!" $ do
      let expectedLines = [ "Sprockell 0 says Hello World!"
                          ]
      let fileName = "printHelloWorld"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "prints nested variables" $ do
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 0 says W"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says true"
                          ]
      let fileName = "printVars"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "prints sum of two numbers" $ do
      let expectedLines = [ "Sprockell 0 says 3"
                          , "Sprockell 0 says 3"
                          ]
      let fileName = "addTwoNums"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "prints squared area of a triangle" $ do
      let expectedLines = [ "Sprockell 0 says 216" ]
      let fileName = "areaTriangle"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "swaps two variables" $ do
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 0 says 5"
                          ]
      let fileName = "swapVars"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests variable delcaration (int, bool, char)" $ do
      let expectedLines = [ "Sprockell 0 says 0"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says  "  -- 2 spaces here
                          , "Sprockell 0 says K"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says true"
                          ]
      let fileName = "testVars"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests assignment of expressions" $ do
      let expectedLines = [ "Sprockell 0 says 0"
                          , "Sprockell 0 says 12"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 47"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says false"
                          ]
      let fileName = "testExpr"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests boolean operations" $ do
      let expectedLines = [ "Sprockell 0 says false"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says 202"
                          , "Sprockell 0 says 404"
                          , "Sprockell 0 says 505"
                          , "Sprockell 0 says 606"
                          , "Sprockell 0 says 999"
                          ]
      let fileName = "logicalOps"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests conditionals" $ do
      let expectedLines = [ "Sprockell 0 says 1"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 8"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 12"
                          , "Sprockell 0 says 14"
                          , "Sprockell 0 says 17"
                          , "Sprockell 0 says 18"
                          , "Sprockell 0 says 20"
                          , "Sprockell 0 says 24"
                          , "Sprockell 0 says 25"
                          , "Sprockell 0 says 28"
                          , "Sprockell 0 says 30"
                          , "Sprockell 0 says 32"
                          , "Sprockell 0 says 33"
                          ]
      let fileName = "testConditionals"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests blocks/scopes" $ do
      let expectedLines = [ "Sprockell 0 says c"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 8"
                          , "Sprockell 0 says 9"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 20"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says 19"
                          , "Sprockell 0 says 18"
                          , "Sprockell 0 says 17"
                          , "Sprockell 0 says 16"
                          , "Sprockell 0 says 15"
                          , "Sprockell 0 says 999"
                          ]
      let fileName = "blocks"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests if and while" $ do
      let expectedLines = [ "Sprockell 0 says 1"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says 5"
                          , "Sprockell 0 says 5"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 999"
                          ]
      let fileName = "checkIfWhile"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests signed division (a/b; -a/b; a/-b; -a/-b; 0/b; a/b)" $ do
      let expectedLines = [ "Sprockell 0 says Integer division of 10 by 3:"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says Integer division of -19 by 4:"
                          , "Sprockell 0 says -4"
                          , "Sprockell 0 says Integer division of 21 by -4:"
                          , "Sprockell 0 says -5"
                          , "Sprockell 0 says Integer division of -23 by -5:"
                          , "Sprockell 0 says 4"
                          , "Sprockell 0 says Division of 8 by zero:"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says Division of zero by 5:"
                          , "Sprockell 0 says 0"
                          ]
      let fileName = "testDivision"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    -- this test takes more than others (around 1.5 sec) because of division of big numbers (1000000 / 3)
    it "tests arithmetic operations (extensive)" $ do
      let expectedLines = [ "Sprockell 0 says 3"
                          , "Sprockell 0 says 2"
                          , "Sprockell 0 says 6"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says 7"
                          , "Sprockell 0 says 9"
                          , "Sprockell 0 says 1"
                          , "Sprockell 0 says 65"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says 10"
                          , "Sprockell 0 says -3"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 2000000"
                          , "Sprockell 0 says 1000000"
                          , "Sprockell 0 says 333333"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 3"
                          , "Sprockell 0 says -2"
                          , "Sprockell 0 says 105"
                          , "Sprockell 0 says 68"
                          , "Sprockell 0 says 0"
                          ]
      let fileName = "testArithmetic"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests variable shadowing" $ do
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 1 says false"
                          , "Sprockell 2 says 20"
                          , "Sprockell 2 says a"
                          , "Sprockell 2 says 30"
                          , "Sprockell 2 says possible"
                          , "Sprockell 2 says z"
                          , "Sprockell 3 says 50"
                          , "Sprockell 3 says W"
                          ]
      let fileName = "testVarShadowing"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests fib(15) and fib(30)" $ do
      let expectedLines = [ "Sprockell 0 says 610"
                          , "Sprockell 0 says 832040"
                          ]
      let fileName = "fib"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests threads and locks" $ do
      let expectedLines = [ "Sprockell 1 says Thread 1 x"
                          , "Sprockell 1 says 1"
                          , "Sprockell 1 says Thread 1 y"
                          , "Sprockell 1 says 7"
                          , "Sprockell 2 says Thread 2 x"
                          , "Sprockell 2 says 0"
                          , "Sprockell 3 says Thread 3 x"
                          , "Sprockell 3 says 0"
                          , "Sprockell 3 says Thread 3 y"
                          , "Sprockell 3 says 21"
                          , "Sprockell 4 says Thread 4 x"
                          , "Sprockell 4 says 2"
                          , "Sprockell 4 says Thread 4 y"
                          , "Sprockell 4 says 23"
                          , "Sprockell 0 says Final x"
                          , "Sprockell 0 says 2"
                          , "Sprockell 0 says Final y"
                          , "Sprockell 0 says 23"
                          ]
      let fileName = "testThreads"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests multiple (4) locks" $ do
      let fileName = "locks"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      let lastValue = take 2 $ drop (length output - 3) output
      lastValue `shouldBe` "25"

    it "tests concurrency with Peterson's algorithm (for 2 threads)" $ do
      let fileName = "peterson"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      -- evaluate the final value of the critical variable
      let secondLastChar = output !! (length output - 2)
      secondLastChar `shouldBe` '0'

    it "tests concurrency with a banking system (main thread + 4 threads)" $ do
      let expectedLines = [ "Sprockell 0 says Final balances:"  -- careful with space here
                          , "Sprockell 0 says -5050"
                          , "Sprockell 0 says 7050"
                          ]
      let fileName = "banking"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName
      let ls = lines output
      let last3 = drop (length ls - 3) ls

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      last3 `shouldBe` expectedLines

    it "tests operations (+, -, *, /, &&, ||) when stack is full" $ do
      -- we use stack.push which overwrites the last values of the stack (if it is full)
      let expectedLines = [ "Sprockell 0 says 10"
                          , "Sprockell 0 says 0"
                          , "Sprockell 0 says 24"
                          , "Sprockell 0 says 2"
                          , "Sprockell 0 says false"
                          , "Sprockell 0 says true"
                          , "Sprockell 0 says 2"
                          , "Sprockell 0 says 0"
                          ]
      let fileName = "testFullStack"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runFile fileName fileName

      -- append output to log file
      let toWrite = outGen ++ output
      appendFile (logsDir ++ fileName ++ logExt) toWrite

      lines output `shouldBe` expectedLines

    it "tests infinite loops (while)" $ do
      let fileName = "testInfiniteWhile"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- timeout (1 * 10^6) $ compile (progsDir ++ fileName ++ fileExt) fileName
      output `shouldBe` Nothing
      -- this test contains no output

    it "tests infinite loops (locks)" $ do
      let fileName = "testInfLock"

      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcHead ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- timeout (1 * 10^6) $ compile (progsDir ++ fileName ++ fileExt) fileName
      output `shouldBe` Nothing
      -- this test contains no output

    it "fails to declare a variable with incorrect type" $ do
      let program = "ints x;"

      -- write src code to log file
      let fileName = "failDecl"
      let toWrite = srcHead ++ program
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runStr program "failDecl"
      let res = "ParseError" `isPrefixOf` output
      res `shouldBe` True
      -- this test contains no output and no generated code

    it "fails to reference an undeclared variable in assignment" $ do
      let program = "int x = y + 1;"

      -- write src code to log file
      let fileName = "failRef"
      let toWrite = srcHead ++ program
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runStr program "failRef"
      let res = "TypeError" `isPrefixOf` output
      res `shouldBe` True
      -- this test contains no output and no generated code

    it "fails to use correct operator" $ do
      let program = "int x = 5 ** 2;"

      -- write src code to log file
      let fileName = "failOp"
      let toWrite = srcHead ++ program
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runStr program "failOp"
      let res = "ParseError" `isPrefixOf` output
      res `shouldBe` True
      -- this test contains no output and no generated code

    it "fails to close braces properly" $ do
      let program = "int x = { 5 + 2;"

      -- write src code to log file
      let fileName = "failBrace"
      let toWrite = srcHead ++ program
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runStr program "failBrace"
      let res = "ParseError" `isPrefixOf` output
      res `shouldBe` True
      -- this test contains no output and no generated code

    it "fails to match types in assignment" $ do
      let program = "char x = 99;"

      -- write src code to log file
      let fileName = "failAssign"
      let toWrite = srcHead ++ program
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      output <- runStr program fileName
      let res = "TypeError" `isPrefixOf` output
      res `shouldBe` True
      -- this test contains no output and no generated code

    it "fails to allocate more than 8 shared variables" $ do
      let program =    "global int a1;"
                    ++ "global int a2;"
                    ++ "global int a3;"
                    ++ "global int a4;"
                    ++ "global int a5;"
                    ++ "global int a6;"
                    ++ "global int a7;"
                    ++ "global int a8;"
                    ++ "global int a9;"
                    ++ "print(a9);"

      -- write src code to log file and no generated code
      let fileName = "failShMem"
      let toWrite = srcHead ++ program
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      res <- try (evaluate program fileName) :: IO (Either SomeException ())
      isLeft res `shouldBe` True
      -- this test contains no output

    it "fails to allocate more than 32 local variables" $ do
      let fileName = "failLocalMem"
      -- write src code to log file
      srcLine <- readFile (progsDir ++ fileName ++ fileExt)
      let toWrite = srcLine ++ srcLine
      writeFile (logsDir ++ fileName ++ logExt) toWrite

      res <- try (compile fileName fileName) :: IO (Either SomeException ())
      isLeft res `shouldBe` True
      -- this test contains no output


----------------------------------------------------------------------
--                         HELPER FUNCTIONS                         --
----------------------------------------------------------------------

-- headers for sections in log files
srcHead :: String
srcHead = "--------------- SOURCE CODE ---------------\n"

genHead :: String
genHead = "\n\n------------- GENERATED CODE -------------\n"

outGen :: String
outGen = "\n\n----------------- OUTPUT -----------------\n"

-- path to programs directory
progsDir :: FilePath
progsDir = "./test/programs/"

-- path to logs directory
logsDir :: FilePath
logsDir = "./test/logs/"

-- extenstion for log files
logExt :: String
logExt = ".log"

-- string to hold the file extension
fileExt :: String
fileExt = ".txt"


-- runFile - compile file and catch output
runFile :: FilePath -> String -> IO String
runFile path logName = redirectOutput $ compile (progsDir ++ path ++ fileExt) logName


-- runStr - compile string and catch output
runStr :: String -> String -> IO String
runStr str logName = redirectOutput $ evaluate str logName


-- function to compile files
compile :: FilePath -> String -> IO ()
compile filePath logName = do
  input <- readFile filePath
  evaluate input logName


-- function to evaluate input (parse, elaborate, gen code)
evaluate :: String -> String -> IO ()
evaluate input logName = do
  let output = createAST input
  case output of
      -- error
      Left  err -> print err

      -- compile ast
      Right ast -> do
          let env = compileProgram ast
          let threads = mainCode env : threadsCode env
          -- putStrLn $ "Main Code: " ++ show (mainCode env)
          -- putStrLn $ "Threads code: " ++ show (threadsCode env)
          let toAppend = genHead ++ formatThreads threads
          -- append generated code to log file
          appendFile (logsDir ++ logName ++ logExt) toAppend
          Sprockell.run threads


-- helper function to format threads
formatThreads :: [[Sprockell.Instruction]] -> String
formatThreads threads = "[[" ++ intercalate "],\n [" (map showThread threads) ++ "\n]]"


-- helper function to show each thread's instructions
showThread :: [Sprockell.Instruction] -> String
showThread instructions = intercalate ",\n  " (map show instructions)


-- redirect the output of an IO action that writes to "stdout"
redirectOutput :: IO () -> IO String
redirectOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  f
  hFlush stdout
  hDuplicateTo stdout_dup stdout
  hClose stdout_dup
  -- str <- readFile tmpf
  -- removeFile tmpf      -- when removing the file an exception is thrown because it is still being read (idk how to fix this)
  -- return str
  readFile tmpf