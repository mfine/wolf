name:                wolf
version:             VERSION
synopsis:            Amazon Simple Workflow Service Wrapper.
homepage:            https://github.com/swift-nav/wolf
license:             MIT
license-file:        LICENSE
author:              Swift Navigation Inc.
maintainer:          Mark Fine <dev@swiftnav.com>
copyright:           Copyright (C) 2015 Swift Navigation, Inc.
category:            Network, AWS, Cloud, Distributed Computing
build-type:          Simple
extra-source-files:  README.md
cabal-version:       >= 1.10

description:
  Wolf is a wrapper around Amazon Simple Workflow Service.

source-repository head
  type:              git
  location:          git@github.com:swift-nav/wolf.git

library
  exposed-modules:     Network.AWS.Wolf
  other-modules:       Network.AWS.Wolf.Act
                     , Network.AWS.Wolf.Ctx
                     , Network.AWS.Wolf.Decide
                     , Network.AWS.Wolf.File
                     , Network.AWS.Wolf.Prelude
                     , Network.AWS.Wolf.S3
                     , Network.AWS.Wolf.SWF
                     , Network.AWS.Wolf.Types
                     , Network.AWS.Wolf.Types.Ctx
                     , Network.AWS.Wolf.Types.Product
                     , Network.AWS.Wolf.Types.Sum
  default-language:    Haskell2010
  hs-source-dirs:      src
  ghc-options:         -Wall
  build-depends:       aeson
                     , amazonka
                     , amazonka-core
                     , amazonka-s3
                     , amazonka-swf
                     , base >= 4.7 && < 5
                     , bytestring
                     , conduit
                     , conduit-combinators
                     , conduit-extra
                     , directory
                     , exceptions
                     , filemanip
                     , lifted-async
                     , monad-control
                     , optparse-applicative
                     , preamble
                     , process
                     , resourcet
                     , text
                     , time
                     , yaml

executable wolf-actor
  hs-source-dirs:      main
  main-is:             actor.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall -O2
  build-depends:       base
                     , wolf
                     , optparse-generic
  default-language:    Haskell2010

executable wolf-decider
  hs-source-dirs:      main
  main-is:             decider.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall -O2
  build-depends:       base
                     , wolf
                     , optparse-generic
  default-language:    Haskell2010

executable shake-wolf
  main-is:             Shakefile.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall -O2
  build-depends:       base >= 4.7 && < 5
                     , basic-prelude
                     , directory
                     , shake
  default-language:    Haskell2010

