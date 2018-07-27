import           Data.Sequence (Seq (..), adjust, empty, fromList, index,
                                replicate, sort, update, (><), (|>))
import           Lib
import           Test.Hspec    (describe, hspec, it, shouldBe, xdescribe)

aToZ :: Alphabet
aToZ = ['a' .. 'z']


main :: IO ()
main = hspec $ do
  let aToE = "abcde"
  describe "span" $
    it "aaabb" $ span (== 'a') "aaabb" `shouldBe` ("aaa", "bb")
  describe "normalize" $ do
    it "empty" $ normalize aToZ "" `shouldBe` ""
    it "aaa" $ normalize aToZ "aaa" `shouldBe` "aaa"
    it "AAA" $ normalize aToZ "AAA" `shouldBe` "aaa"
    it "AbCdE" $ normalize aToZ "AbCdE" `shouldBe` "abcde"
    it "Spe$ doin_klE" $ normalize aToZ "Spe$ doin_klE" `shouldBe` "spedoinkle"
  describe "mkHisto" $ do
    let
      test alphabet input output =
        it ("test case " ++ input) $ mkHisto alphabet input `shouldBe` output
    test aToE "abbcddddeee" [1,2,1,4,3]
    test aToE "eeeeeddddcccbba" [1,2,3,4,5]
    test aToE "Bananarama!" [5,1,0,0,0]
    test aToZ "Floccinaucinihilipilification"
      -- a b c d e f g h i j k l m n o p q r s t u v w x y z
        [2,0,4,0,0,2,0,1,9,0,0,3,0,3,2,1,0,0,0,1,1,0,0,0,0,0]
  describe "upsert" $ do
    it "updates in-place" $
      upsert 'z' 1 (const 'c') (fromList "bbb") `shouldBe` fromList "bcb"
    it "grows a seq where necessary" $
      upsert 'z' 4 (const 'c') (fromList "bb") `shouldBe` fromList "bbzzc"
  describe "addWord" $ do
    let
      test alphabet word tree result =
        it word $ addWord alphabet word tree `shouldBe` result
    test aToE "abcde" (Node empty)
      (Node $ fromList [
        Node empty,
        Node $ fromList [ -- 1 a
          Node empty,
          Node $ fromList [ -- 1 b
            Node empty,
            Node $ fromList [ --  1 c
              Node empty,
              Node $ fromList [ --  1 d
                Node empty,
                Leaf $ fromList ["abcde"] -- 1 e
                ]
              ]
            ]
          ]
        ])
    test aToE "aaabbc" (Node empty)
      (Node $ fromList [
        Node empty,
        Node empty,
        Node empty,
        Node $ fromList [ -- 3 a's
          Node empty,
          Node empty,
          Node $ fromList [ -- 2 b's
            Node empty,
            Node $ fromList [ --  1 c
              Node $ fromList [ --  0 d's
                Leaf $ fromList ["aaabbc"] -- 0 e's
                ]
              ]
            ]
          ]
        ])
    test aToE "aaabbc" (addWord aToE "abcde" (Node empty))
      (Node $ fromList [
        Node empty,
        Node $ fromList [ -- 1 a
          Node empty,
          Node $ fromList [ -- 1 b
            Node empty,
            Node $ fromList [ --  1 c
              Node empty,
              Node $ fromList [ --  1 d
                Node empty,
                Leaf $ fromList ["abcde"] -- 1 e
                ]
              ]
            ]
          ],
        Node empty,
        Node $ fromList [ -- 3 a's
          Node empty,
          Node empty,
          Node $ fromList [ -- 2 b's
            Node empty,
            Node $ fromList [ --  1 c
              Node $ fromList [ --  0 d's
                Leaf $ fromList ["aaabbc"] -- 0 e's
                ]
              ]
            ]
          ]
        ])
    test aToE "aaabbc" (addWord aToE "cababa" (Node empty))
      (Node $ fromList [
        Node empty,
        Node empty,
        Node empty,
        Node $ fromList [ -- 3 a's
          Node empty,
          Node empty,
          Node $ fromList [ -- 2 b's
            Node empty,
            Node $ fromList [ --  1 c
              Node $ fromList [ --  0 d's
                Leaf $ fromList ["cababa", "aaabbc"] -- 0 e's
                ]
              ]
            ]
          ]
        ])
  describe "mkTree" $
    it "adds all words" $
      mkTree aToE ["abcde", "aaabbc", "cababa"] `shouldBe`
        (Node $ fromList [
          Node empty,
          Node $ fromList [ -- 1 a
            Node empty,
            Node $ fromList [ -- 1 b
              Node empty,
              Node $ fromList [ --  1 c
                Node empty,
                Node $ fromList [ --  1 d
                  Node empty,
                  Leaf $ fromList ["abcde"] -- 1 e
                  ]
                ]
              ]
            ],
          Node empty,
          Node $ fromList [ -- 3 a's
            Node empty,
            Node empty,
            Node $ fromList [ -- 2 b's
              Node empty,
              Node $ fromList [ --  1 c
                Node $ fromList [ --  0 d's
                  Leaf $ fromList ["cababa", "aaabbc"] -- 0 e's
                  ]
                ]
              ]
            ]
          ])
  describe "getSubgrams" $ do
    let
      words = ["abc", "cba", "abcde", "bcd", "dbc", "de", "dee", "deed"]
      tree = mkTree aToE words
    it "finds all words" $
      sort (getSubgrams aToE "abcdede" tree) `shouldBe` sort (fromList words)
    it "finds anagrams" $
      sort (getSubgrams aToE "abc" tree) `shouldBe` sort (fromList ["abc", "cba"])
    it "finds subgrams" $
      sort (getSubgrams aToE "abcd" tree) `shouldBe` sort (fromList ["abc", "cba", "bcd", "dbc"])
  describe "getAnagrams" $ do
    let
      words = ["abc", "cba", "abcde", "bcd", "dbc", "de", "dee", "deed"]
      tree = mkTree aToE words
    it "finds exact one-word anagrams" $
      sort (getAnagrams aToE "abc" tree) `shouldBe` sort (fromList ["abc", "cba"])
    it "finds multi-word anagrams" $
      sort (getAnagrams aToE "ab cb cd" tree) `shouldBe`
        sort (fromList [
          "abc bcd",
          "abc dbc",
          "bcd abc",
          "bcd cba",
          "cba bcd",
          "cba dbc",
          "dbc abc",
          "dbc cba"
          ])
  describe "getAnagramsOptimized" $ do
    let
      words = ["abc", "cba", "abcde", "bcd", "dbc", "de", "dee", "deed"]
      tree = mkTree aToE words
    it "finds exact one-word anagrams" $
      sort (getAnagramsOptimized aToE "abc" tree) `shouldBe` sort (fromList ["abc", "cba"])
    it "finds multi-word anagrams" $
      sort (getAnagramsOptimized aToE "ab cb cd" tree) `shouldBe`
        sort (fromList ["bcd abc","bcd cba","dbc abc","dbc cba"])
