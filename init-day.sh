# init-day.sh <day>

mkdir "./app/Day$1"

cat << EOF > "./app/Day$1/Day$101.hs"
module Main where

main = print "Hello Day $101"

EOF

cat << EOF > "./app/Day$1/Day$102.hs"
module Main where

main = print "Hello Day $102"

EOF

touch "./app/Day$1/Input.txt"

cat << EOF >> "./AoC2022.cabal"

executable Day$101
  main-is:          Day$1/Day$101.hs
  build-depends:
    , AoC2022
    , base
    , ilist
    , parsec
    , split
    , containers
    , vector
    , transformers
    , search-algorithms
  hs-source-dirs:   app
  default-language: Haskell2010
  ghc-options: -Wall -Werror

executable Day$102
  main-is:          Day$1/Day$102.hs
  build-depends:
    , AoC2022
    , base
    , ilist
    , parsec
    , split
    , containers
    , vector
    , transformers
    , search-algorithms
  hs-source-dirs:   app
  default-language: Haskell2010
  ghc-options: -Wall -Werror

EOF

cabal run Day$101
cabal run Day$102